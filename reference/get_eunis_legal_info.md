# Get EUNIS Legal Information for a Species Using iNaturalist Taxon ID

**\[experimental\]** This function takes a `taxon.id` from iNaturalist,
retrieves the corresponding scientific name from the iNaturalist API,
searches the EUNIS database, and extracts the legal information related
to the EU Habitats Directive (92/43/EEC) and Birds Directive
(2009/147/EC), as delivered by data retrieved from the
[EUNIS](https://eunis.eea.europa.eu/) species database.

## Usage

``` r
get_eunis_legal_info(taxon.id)
```

## Arguments

- taxon.id:

  Integer. The taxon ID of the species in iNaturalist.

## Value

A tibble with the following columns:

- taxon.id:

  iNaturalist taxon ID provided as input

- scientific_name:

  Scientific name of the species retrieved from iNaturalist

- `Legal text`:

  Legal directive text from EUNIS (92/43/EEC or 2009/147/EC)

- Annex:

  Annex information from EUNIS table

## Author

Alessandro Oggioni, PhD (2023) <alessandro.oggioni@cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
## Not run:
get_eunis_legal_info(taxon.id = 61749)
} # }
## End (Not run)
```
