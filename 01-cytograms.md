Obtaining flow cytometry data
================
Compiled at 2021-07-23 19:02:02 UTC

``` r
knitr::opts_chunk$set(fig.width=14, fig.height=8, echo=TRUE, eval=TRUE, cache=FALSE,
                      warning=FALSE, message=FALSE,
                      cache.lazy = FALSE)

## Load packages
library(tidyverse)
library(knitr)
library(cmap4r)
library(here)

## Setup locations
base = "01-cytograms"
here::i_am("01-cytograms.Rmd")
```

    ## here() starts at /home/sangwonh/repos/flowmixapp

``` r
knitr::opts_chunk$set(fig.path = here::here("data", base, 'figures/'))

## Setup some plotting details
coul <- colorRampPalette(RColorBrewer::brewer.pal(8, "RdYlBu"))(25)
source("helpers.R")
```

``` r
## Common project name
## outputdir = file.path("data", base)
outputdir = here::here("data", base)
if(!dir.exists(outputdir)) dir.create(outputdir)
```

<!-- Compared to obtaining environmental covariates (00-cytograms.Rmd), this will be short! -->

The link of cleaned data is originally from here:
<https://drive.google.com/drive/folders/1nK4cBdbXFDODRrLmqo-GoZS-f_yXeiGF>

First, we downloaded the content (as of July 23, 2021) into a folder.

Then, we visualize some data from the cruise KOK1609.

``` r
## dat = read.csv(here::here("data", base, "SCOPE_19", "SCOPE_19.hourly.csv.gz")) %>% as_tibble()
dat = read.csv(file.path("data", base, "SCOPE_19", "SCOPE_19.hourly.csv.gz")) %>% as_tibble()

for(itime in c(1,10,20,30)){

  one_hour_dat = dat %>% group_by(date) %>% group_split() %>% .[[itime]]
  coordnames = c("fsc_small_coord", "chl_small_coord", "pe_coord")
  onetime = one_hour_dat %>% pull(date) %>% unique()
  
  ## We'll make three plots, two dimensions at a time.
  plist = list()
  for(ii in 1:3){
  
    ## Two coordinates' names
    inds = list(c(1,2), c(2,3), c(3,1))[[ii]]
    coordname1 = coordnames[inds[1]]
    coordname2 = coordnames[inds[2]]

    ## Make 2d plot
    one_hour_dat %>%
      select(fsc_small_coord, pe_coord, chl_small_coord, count = n_per_ul) %>%
      group_by(x=!!sym(coordname1), y=!!sym(coordname2)) %>% 
      dplyr::summarise(count=sum(count), .groups = 'drop') %>% 
      ggplot() +
      geom_raster(aes(x=x, y=y, fill=count)) +
      coord_fixed(ratio = 1)+
      theme_set(theme_bw()) -> p

    ## Save to list
    plist[[ii]] = p
  }

  ## Make 1 x 3 image
  do.call(gridExtra::grid.arrange, c(plist, ncol=3, top = onetime))
}
```

![](/home/sangwonh/repos/flowmixapp/data/01-cytograms/figures/unnamed-chunk-1-1.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/01-cytograms/figures/unnamed-chunk-1-2.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/01-cytograms/figures/unnamed-chunk-1-3.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/01-cytograms/figures/unnamed-chunk-1-4.png)<!-- -->

``` r
## dat_parquet <- arrow::read_parquet(here::here("data", base, "SCOPE_19", "SCOPE_19.hourly.parquet"))
dat_parquet <- arrow::read_parquet(file.path("data", base, "SCOPE_19", "SCOPE_19.hourly.parquet"))
stopifnot(nrow(dat_parquet) == nrow(dat))
```
