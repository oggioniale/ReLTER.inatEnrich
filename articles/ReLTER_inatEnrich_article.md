# ReLTER.inatEnrich — Technical Reference

## Overview

This article is a technical complement to the [Getting Started
vignette](https://oggioniale.github.io/ReLTER.inatEnrich/articles/ReLTER_inatEnrich.md).
It documents the internal data structures produced at each step of the
enrichment pipeline, the API endpoints queried, and the recommended
patterns for accessing nested list-columns in downstream analyses.

------------------------------------------------------------------------

## Package architecture

`ReLTER.inatEnrich` is structured around four public functions that
operate sequentially on an `sf` tibble of iNaturalist occurrences:

| Function | Data source | Output column(s) |
|----|----|----|
| [`add_iucn_to_occ()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_iucn_to_occ.md) | iNaturalist API `/v1/taxa/{id}` | `status_IUCN` (list-column) |
| [`add_nativeness_to_occ()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_nativeness_to_occ.md) | iNaturalist API `/v1/taxa/{id}` | `establishmentMeans` (list-column) |
| [`add_eunis_legal_to_occ()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_eunis_legal_to_occ.md) | BISE REST API (`biodiversity.europa.eu`) | `directive`, `annex` (flat columns) |
| [`create_leaflet_occ_map()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/create_leaflet_occ_map.md) | — | Leaflet map object |

All API calls use `httr2` with retry logic (`max_tries = 3`,
`max_seconds = 120`) and return a safe empty tibble on failure, so the
pipeline never interrupts on a single taxon error.

------------------------------------------------------------------------

## Nested list-column structures

Two of the four enrichment functions produce **list-columns** — columns
where each cell contains a tibble rather than a scalar value. This
design allows multiple records per taxon (e.g. one IUCN assessment per
geographic scope) to coexist in a single row of the occurrence tibble
without information loss.

### `establishmentMeans`

Produced by
[`add_nativeness_to_occ()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_nativeness_to_occ.md).
Each cell contains a **1 × 11 tibble** with the nativeness status, the
authority that recorded it (filtered to the specified country), and
EASIN alien species fields.

    # str(occ$establishmentMeans[[1]])
    tibble [1 × 11] (S3: tbl_df/tbl/data.frame)
     $ iNat_nativeness                    : chr "native"
     $ iNat_authority                     : chr "Italy Check List"
     $ iNat_checkList_uri                 : chr "https://www.inaturalist.org/lists/..."
     $ EASIN_url                          : chr NA
     $ EASIN_id                           : chr NA
     $ EASIN_LSID                         : chr NA
     $ EASIN_firstIntroductionsInEU_year  : chr NA
     $ EASIN_firstIntroductions_Country   : chr NA
     $ EASIN_status                       : chr NA
     $ EASIN_hasImpact                    : chr NA
     $ EASIN_IsEUConcern                  : chr NA

**Accessing the values:**

``` r

library(dplyr)
library(purrr)

# Extract nativeness for all rows as a flat character vector
occ |>
  mutate(
    nativeness   = map_chr(establishmentMeans,
                           ~ .x$iNat_nativeness[[1]] %||% NA_character_),
    em_authority = map_chr(establishmentMeans,
                           ~ .x$iNat_authority[[1]]  %||% NA_character_)
  )

# Unnest to a flat tibble (duplicates rows where needed)
occ |>
  tidyr::unnest(establishmentMeans)
```

**When is it `NA`?**

`iNat_nativeness` and `iNat_authority` are both `NA_character_` when:

- the taxon has no listed taxa records in iNaturalist, or
- none of the listed taxa records match the specified `country`, or
- the API call failed after the maximum number of retries.

EASIN fields are `NA` when the taxon is not present in the EASIN
catalogue (i.e. it is not considered an alien species in Europe).

------------------------------------------------------------------------

### `status_IUCN`

Produced by
[`add_iucn_to_occ()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_iucn_to_occ.md).
Each cell contains an **N × 4 tibble**, with one row per geographic
scope for which an IUCN assessment exists. The number of rows varies by
taxon.

    # str(occ$status_IUCN[[1]])
    tibble [3 × 4] (S3: tbl_df/tbl/data.frame)
     $ status            : chr [1:3] "LC" "LC" "LC"
     $ authority         : chr [1:3] "IUCN Red List" "IUCN Red List" "IUCN Red List"
     $ scope_of_assesment: chr [1:3] "Globally" "Europe" "Mediterranean"
     $ url               : chr [1:3] "https://www.iucnredlist.org/..."
                                     "https://www.iucnredlist.org/..."
                                     "https://www.iucnredlist.org/..."

The `scope_of_assesment` field identifies the geographic scope of the
assessment:

- `"Globally"` — global assessment
- `"Europe"`, `"Mediterranean"`, etc. — regional assessments

**Accessing the values:**

``` r

# Extract only the global status (scope_of_assesment is "Globally")
occ |>
  mutate(
    iucn_global = map_chr(status_IUCN, function(tbl) {
      global <- tbl |> filter(scope_of_assesment == "Globally")
      if (nrow(global) == 0) NA_character_ else global$status[[1]]
    })
  )

# Extract all scopes as a flat tibble (one row per taxon × scope)
occ |>
  select(taxon.id, name, status_IUCN) |>
  tidyr::unnest(status_IUCN)

# Get all distinct statuses for a single taxon
occ$status_IUCN[[1]] |>
  select(scope_of_assesment, status, url)
```

**IUCN status codes reference:**

| Code | Meaning               |
|------|-----------------------|
| `EX` | Extinct               |
| `EW` | Extinct in the Wild   |
| `CR` | Critically Endangered |
| `EN` | Endangered            |
| `VU` | Vulnerable            |
| `NT` | Near Threatened       |
| `LC` | Least Concern         |
| `DD` | Data Deficient        |
| `NE` | Not Evaluated         |

------------------------------------------------------------------------

### `directive` and `annex` (flat columns)

Produced by
[`add_eunis_legal_to_occ()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_eunis_legal_to_occ.md).
Unlike the IUCN and nativeness data, EU legal information is stored as
**flat character columns**. When a taxon is covered by more than one
directive or annex, the occurrence tibble contains **multiple rows for
the same observation** — one per directive/annex combination.

    # Example: a taxon covered by both directives
    # A tibble: 2 × (n + 2)
    #   taxon.id  ...  directive                              annex
    #   <int>          <chr>                                  <chr>
    # 1 48484          Birds Directive (2009/147/EC)          I
    # 2 48484          Habitats Directive (92/43/EEC)         IV

**Annex values by directive:**

For the **Habitats Directive**, annex values follow the standard format:
`"II"`, `"II*"` (priority species), `"IV"`, `"V"`, or combinations.

For the **Birds Directive**, annex values reflect the structure of the
directive itself: `"I"` (special protection areas).

These values are parsed from the BISE Protection table, which returns
full English descriptions
(e.g. `"Annex IV: animal and plant species of community interest in need of strict protection."`).
The parser extracts the Roman numeral code (`"IV"`) using a regular
expression.

> **Note:** The BISE API returns EU-wide data only — annex assignments
> are not filtered by country. All annexes applicable to the species
> across the EU are returned. This means that **row count in
> `occ_eLTER_legal` may exceed row count in `occ_eLTER_nativeness`** for
> taxa with multiple directive entries. Keep this in mind when computing
> summaries.

``` r

# Count unique observations (not rows) correctly
n_obs <- occ_eLTER_legal |>
  dplyr::distinct(taxon.id, observed_on, user.login) |>
  nrow()

# Summarise directives per taxon without duplicating observations
directives_per_taxon <- occ_eLTER_legal |>
  dplyr::select(taxon.id, directive, annex) |>
  dplyr::distinct() |>
  dplyr::group_by(taxon.id) |>
  dplyr::summarise(
    n_directives = sum(!is.na(directive)),
    directives   = paste(na.omit(directive), collapse = "; "),
    annexes      = paste(na.omit(annex),     collapse = "; "),
    .groups      = "drop"
  )
```

------------------------------------------------------------------------

## API details

### iNaturalist API

Both
[`add_iucn_to_occ()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_iucn_to_occ.md)
and
[`add_nativeness_to_occ()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_nativeness_to_occ.md)
query the iNaturalist v1 taxa endpoint:

    GET https://api.inaturalist.org/v1/taxa/{taxon_id}

The relevant fields extracted from the response are:

- `results[[1]]$conservation_statuses` → IUCN status, authority, URL
- `results[[1]]$listed_taxa` → establishment means, place name, list
  title

Both functions iterate over **unique taxon IDs only**, so each taxon is
queried exactly once regardless of how many observations exist for it in
the dataset.

**Rate limiting:** the iNaturalist API applies rate limiting to
unauthenticated requests (approximately 60 requests per minute). The
functions include a short delay between requests (`Sys.sleep(0.1)`,
approximately 6 req/sec) to stay well within this limit. The built-in
retry logic (`max_tries = 3`, `max_seconds = 120`) handles any transient
failures gracefully.

### BISE REST API

[`get_eunis_legal_info()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/get_eunis_legal_info.md)
(called by
[`add_eunis_legal_to_occ()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_eunis_legal_to_occ.md))
queries the BISE (Biodiversity Information System for Europe) REST API
at `https://biodiversity.europa.eu` using two sequential POST requests:

**Step 1 — Species name → `id_eunis`:**

    POST https://biodiversity.europa.eu/++api++/data/all-species-search-listing/@connector-data
    Content-Type: application/json

    {
      "data_query": [
        {
          "i": "scientific_name",
          "o": "plone.app.querystring.operation.string.contains",
          "v": "Canis lupus"
        }
      ],
      "form": {"db_version": "latest"}
    }

The response contains parallel arrays under `data.results` (e.g.
`id_eunis`, `scientific_name`). The exact-match row is selected by
comparing `scientific_name` case-insensitively; the first result is used
as a fallback.

**Step 2 — `id_eunis` → legal status (Protection table):**

    POST https://biodiversity.europa.eu/++api++/data/natura-2000-species-legal-status/@connector-data
    Content-Type: application/json

    {
      "data_query": [
        {
          "i": "id_eunis",
          "o": "plone.app.querystring.operation.selection.any",
          "v": ["1367"]
        }
      ],
      "form": {"db_version": "latest"}
    }

The response parallel arrays include `legal_instrument`, `annex`,
`url_link`, and `id_eunis`. Only rows where `legal_instrument` is
`"EU Habitats Directive"` or `"EU Birds Directive"` are retained.

The `annex` field contains full descriptions such as:

    "Annex IV: animal and plant species of community interest in need of strict protection."

These are parsed to short codes (`"IV"`) using the regular expression
`sub("^Annex\\s+(I{1,3}V?|VI?|IV|V?I{0,3}).*", "\\1", annex_str)`.

**Throttling:**
[`add_eunis_legal_to_occ()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_eunis_legal_to_occ.md)
inserts a 200 ms pause between taxa (`Sys.sleep(0.2)`) to avoid
overloading the BISE server.

------------------------------------------------------------------------

## Error handling and NA semantics

All three enrichment functions use the same defensive pattern:

``` r

# Conceptual structure shared by all enrichment functions
result <- tryCatch(
  query_external_api(taxon.id),
  error = function(e) {
    warning("Error for taxon.id: ", taxon.id, " — ", conditionMessage(e))
    return(empty_fallback_tibble)   # never returns NULL
  }
)
```

This guarantees that:

1.  The pipeline always completes even if individual API calls fail
2.  Failed taxa are identifiable by `NA` values in the enrichment
    columns
3.  A [`warning()`](https://rdrr.io/r/base/warning.html) is emitted for
    each failure, visible in the R console

**`annex = NA` semantics:** a taxon may have a valid `directive` entry
but `annex = NA`. This means the taxon is present in the BISE Protection
table as covered by that directive, but no annex code could be parsed
from the BISE description string. This is rare but can occur for entries
with non-standard formatting.

To inspect which taxa failed or have missing data after running the
pipeline:

``` r

# Taxa with no IUCN data
occ |>
  filter(map_lgl(status_IUCN, ~ all(is.na(.x$status))))

# Taxa with no nativeness data
occ |>
  filter(map_lgl(establishmentMeans, ~ is.na(.x$iNat_nativeness[[1]])))

# Taxa with no EU legal data
occ |>
  filter(is.na(directive)) |>
  distinct(taxon.id, name)

# Taxa with directive but no annex resolved
occ |>
  filter(!is.na(directive), is.na(annex)) |>
  distinct(taxon.id, name, directive)
```

------------------------------------------------------------------------

## Performance considerations

For large occurrence datasets (\> 1 000 unique taxa), the enrichment
functions can take several minutes due to the sequential nature of the
API calls and the retry logic. A few practical tips:

- **Cache intermediate results** between steps using
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) /
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html) to avoid
  re-querying on re-runs:

``` r

# Save after the slowest step
saveRDS(occ_eLTER_IUCN,      "cache/occ_eLTER_IUCN.rds")
saveRDS(occ_eLTER_nativeness, "cache/occ_eLTER_nativeness.rds")
saveRDS(occ_eLTER_legal,      "cache/occ_eLTER_legal.rds")

# Reload
occ_eLTER_legal <- readRDS("cache/occ_eLTER_legal.rds")
```

- **Rate limits**: the iNaturalist API does not require authentication
  for read-only taxon queries, but it does apply rate limiting. The
  built-in per-request delay and retry logic handle this automatically;
  no manual throttling is needed in user code. For BISE, the 200 ms
  inter-taxon delay in
  [`add_eunis_legal_to_occ()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_eunis_legal_to_occ.md)
  is sufficient for typical dataset sizes.
