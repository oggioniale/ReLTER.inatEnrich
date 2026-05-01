# Visualization functions

## Overview

This article illustrates the visualization functions available in
`ReLTER.inatEnrich` using a small example dataset.

## Taxonomic composition

[`iconic_taxa()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/iconic_taxa.md)
shows the number of observations and species per iNaturalist iconic
taxon group.

``` r

iconic_taxa(df)
```

![](ReLTER_inatEnrich_charts_files/figure-html/iconic-taxa-1.png)

## Top observed species

[`top_n_species()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/top_n_species.md)
displays the most observed species coloured by iconic taxon group.

``` r

top_n_species(df, n = 10)
```

![](ReLTER_inatEnrich_charts_files/figure-html/top-species-1.png)

## Temporal distribution

### By year

``` r

obs_per_year(df)
```

![](ReLTER_inatEnrich_charts_files/figure-html/obs-year-1.png)

### By month

``` r

obs_per_month(df)
```

![](ReLTER_inatEnrich_charts_files/figure-html/obs-month-1.png)

### By hour of the day

``` r

obs_per_hour(df)
```

![](ReLTER_inatEnrich_charts_files/figure-html/obs-hour-1.png)

## Conservation category

[`obs_pie_chart()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/obs_pie_chart.md)
summarises species by conservation category following a fixed priority
order: Alien (IAS) \> Habitats Directive \> Birds Directive \> Other.

``` r

obs_pie_chart(df)
```

![](ReLTER_inatEnrich_charts_files/figure-html/pie-chart-1.png)

## Interactive maps

The interactive map outputs (species richness grid and occurrence map)
are best explored directly:

- [Species richness
  map](https://oggioniale.github.io/ReLTER.inatEnrich/figures/map_example.html)
