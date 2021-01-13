Investigating disparities in supermarket access in the City of Bristol
================

Hello\! This document contains the code and steps used to investigate
the above topic.

To study accessibility, this project uses network analysis to calculate
the mean number of supermarkets within a 1km walk from residential
areas.

*Data Preparation*

Firstly, load libraries.

``` r
library(sf)
```

    ## Linking to GEOS 3.8.1, GDAL 3.1.1, PROJ 6.3.1

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(magrittr)
```

    ## 
    ## Attaching package: 'magrittr'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     set_names

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
library(dplyr)
library(dodgr)
library(expss)
```

    ## 
    ## Attaching package: 'expss'

    ## The following objects are masked from 'package:magrittr':
    ## 
    ##     and, equals, not, or

    ## The following objects are masked from 'package:stringr':
    ## 
    ##     fixed, regex

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, compute, contains, first, last, na_if, recode, vars

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     keep, modify, modify_if, transpose, when

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     contains, nest

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     vars

``` r
library(osmdata)
```

    ## Data (c) OpenStreetMap contributors, ODbL 1.0. https://www.openstreetmap.org/copyright

Next, load and reproject data.

The ‘Bristol LSOA’ shapefile has been uploaded to this repository for
convenience [(Source: Open Data
Bristol, 2020)](https://opendata.bristol.gov.uk/explore/dataset/lsoa110/information/)

``` r
Bristol_LSOA <- st_read('lsoa110/lsoa110.shp')
```

    ## Reading layer `lsoa110' from data source `/Users/emma/Desktop/Masters/PSA/Assessment/CW1/Coursework/lsoa110/lsoa110.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 263 features and 9 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: -2.721657 ymin: 51.39737 xmax: -2.51042 ymax: 51.54437
    ## geographic CRS: WGS 84

``` r
Bristol_LSOA_BNG <- st_transform(Bristol_LSOA, 27700)

Bristol_bbox <- c(-2.703944,51.393971,-2.489433,51.534516)

Bristol_Network <- opq(bbox = Bristol_bbox) %>%
  add_osm_feature(key = 'highway', value = c('primary', 'secondary', 'tertiary', 'residential', 'path', 'footway', 'unclassified', 'living_street', 'pedestrian')) %>%
  osmdata_sf()

Bristol_Supermarkets <- opq(bbox = Bristol_bbox) %>%
  add_osm_feature(key = 'shop', value = 'supermarket') %>%
  osmdata_sf()

Bristol_Homes <- opq(bbox = Bristol_bbox) %>%
  add_osm_feature(key = 'landuse', value = 'residential') %>%
  osmdata_sf()
```

The OpenStreetMap data is formatted, and the data is ready to be
analysed.

``` r
Bristol_Roads_Nodes <- Bristol_Network$osm_points [, 'osm_id']
Bristol_Roads_Edges <- Bristol_Network$osm_lines [, c('osm_id', 'name', 'highway', 'oneway', 'maxspeed')]
Bristol_Supermarkets_Points <- Bristol_Supermarkets$osm_points [, c('osm_id', 'name')]
Bristol_Homes_Points <- Bristol_Homes$osm_points [, c('osm_id', 'name')]

Bristol_Roads_Edges <- Bristol_Roads_Edges[Bristol_LSOA,]
```

    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar

``` r
Bristol_Roads_Nodes <- Bristol_Roads_Nodes[Bristol_LSOA,]
```

    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar
    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar

``` r
Bristol_Supermarkets_Points <- Bristol_Supermarkets_Points[Bristol_LSOA,]
```

    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar
    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar

``` r
Bristol_Homes_Points <- Bristol_Homes_Points[Bristol_LSOA,]
```

    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar
    ## although coordinates are longitude/latitude, st_intersects assumes that they are planar

*Network Analysis*

The first step of the network analysis is the construction of the
network graph

``` r
graph <- weight_streetnet(Bristol_Roads_Edges, wt_profile = 'foot')
```

    ## The following highway types are present in data yet lack corresponding weight_profile values: NA,

Next, the network is analysed and number of supermarkets within 1km is
assigned to each point

``` r
Homes_Supermarkets_calc <- dodgr_distances(graph, from = st_coordinates(Bristol_Homes_Points), to = st_coordinates(Bristol_Supermarkets_Points), shortest = TRUE, pairwise = FALSE, quiet = FALSE)
```

    ## Calculating shortest paths ... done.

``` r
Bristol_Homes_Points$Supermarkets_within_1km <- count_row_if(lt(1001), Homes_Supermarkets_calc)
```

*Creating the map*

The points are then grouped by LSOA and the mean calculated, before
being joined back to the LSOA shapefile.

``` r
Bristol_Homes_BNG <- st_transform(Bristol_Homes_Points, 27700)
Bristol_Homes_LSOA <- st_intersection(Bristol_Homes_BNG, Bristol_LSOA_BNG)
```

    ## Warning: attribute variables are assumed to be spatially constant throughout all
    ## geometries

``` r
LSOA_Homes_no_geom <- st_drop_geometry(Bristol_Homes_LSOA)

Bristol_Homes_LSOA_mean <- LSOA_Homes_no_geom %>%
  group_by(lsoa11cd) %>%
  summarise(mean = mean(Supermarkets_within_1km), n=n())
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
Bristol_Homes_Supermarkets_join <- Bristol_LSOA %>%
  left_join(Bristol_Homes_LSOA_mean, by = 'lsoa11cd')
```

As the output maps were created in QGIS, the results of the network
analysis are exported.

``` r
write_csv(Bristol_Homes_Supermarkets_join, file = 'Supermarket-Network-Analysis.csv')
```
