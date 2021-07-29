Obtaining environmental covariates
================
Compiled at 2021-07-29 22:32:04 UTC

``` r
knitr::opts_chunk$set(fig.width=14, fig.height=8, echo=TRUE, eval=TRUE, cache=FALSE,
                      warning=FALSE, message=FALSE,
                      cache.lazy = FALSE)

## ## Setup the cache and file directory
## origCache <- knitr::opts_chunk$get("cache.path")
## base <- sub("_cache/.*$", "", origCache)
## knitr::opts_chunk$set(fig.path = file.path(getwd(), "data", base, 'figures/'))

## Load packages
library(tidyverse)
library(knitr)
library(cmap4r)
library(here)

## Setup some plotting details
coul <- colorRampPalette(RColorBrewer::brewer.pal(8, "RdYlBu"))(25)
source("helpers.R")
```

``` r
## ## Common project name
## outputdir = file.path("data", base)
## if(!dir.exists(outputdir)) dir.create(outputdir)


base = "00-covariates"
here::i_am("00-covariates.Rmd")
knitr::opts_chunk$set(fig.path = here::here("data", base, 'figures/'))
outputdir = here::here("data", base)
if(!dir.exists(outputdir)) dir.create(outputdir)
```

# Decide on cruises and environmental variables

There are 105 cruises in total (the meta information is also in
<https://docs.google.com/spreadsheets/d/1Tsi7OWIZWfCQJqLDpId2aG_i-8Cp-p63PYjjvDkOtH4/edit#gid=0>).

Out of these cruises, we choose:

  - Cruises that exist in CMAP, and
  - Cruises for which a common set of environmental covariates can be
    colocalized.

Based on these criteria, we isolate our attention to **32** cruises
between 2013 and 2018 – the ones between the following two cruises
(excluding the cruises for which PAR, salinity and temperature are too
sparse to use):

  - TN291 (2013, January), and
  - KOK1803 (HOT302, 2018 May)

<!-- end list -->

``` r
## Load all the seaflow cruise IDs
meta_data = read.csv(file.path(outputdir, "seaflow-instrument-log-metadata.csv")) %>%
  as_tibble() %>%
  select(cruise, Cruise.ID, Location, Year, Month) %>%
  arrange(Year, Month)
ids_seaflow = meta_data %>% select(Cruise.ID, Year, Month)

## (Commented out) Ask CMAP which cruises exist there.
## ids_in_cmap = get_cruises() %>% pull(Name)
## saveRDS(ids_in_cmap, file = "ids_in_cmap.RDS")
ids_in_cmap = readRDS(file.path(outputdir, "ids-in-cmap.RDS"))

## Out of all seaflow cruises, focus on those between 2013 and 2019
ids_seaflow = ids_seaflow %>% filter(Cruise.ID %in% ids_in_cmap) %>% arrange(Year, Month)
begin = which(ids_seaflow$Cruise.ID == "KM1314")
end = which(ids_seaflow$Cruise.ID == "KOK1803")
ids_final = ids_seaflow[begin:end,]  %>% pull(Cruise.ID)

## PAR is missing or erroneous for these cruises:
ids_final = ids_final[-which(ids_final %in% c("TN291", "TN292", "CN13ID",
                                              "KOK1805", "SKQ201615S", "MV1405"))] 
## There's too little data in these cruises.
ids_final = ids_final[-which(ids_final %in% c("KOK1512", "KM1603", "KM1823"))]  

## Save the final cruise list.
meta_data %>% filter(Cruise.ID %in% ids_final) %>% kable() %>% print()
```

| cruise         | Cruise.ID  | Location          | Year | Month     |
| :------------- | :--------- | :---------------- | ---: | :-------- |
| KiloMoana\_1   | KM1314     | Seattle - Hawaii  | 2013 | August    |
| DeepDOM        | KN210-04   | South Atlantic    | 2013 | March     |
| SCOPE\_1       | KM1427     | Aloha             | 2014 | December  |
| SCOPE\_5       | KM1512     | Aloha             | 2015 | July      |
| SCOPE\_6       | KM1513     | Aloha             | 2015 | July      |
| SCOPE\_4       | KM1510     | Aloha             | 2015 | June      |
| SCOPE\_2       | KM1502     | Portland - Hawaii | 2015 | March     |
| SCOPE\_3       | KM1508     | Aloha             | 2015 | May       |
| SCOPE\_11      | KM1518     | Aloha             | 2015 | November  |
| SCOPE\_10      | KOK1515    | Aloha             | 2015 | October   |
| SCOPE\_15      | KOK1604    | Aloha             | 2016 | April     |
| SCOPE\_16      | KOK1606    | Subtropical front | 2016 | April     |
| SCOPE\_19      | KOK1609    | Aloha             | 2016 | August    |
| SCOPE\_13      | KM1602     | Aloha             | 2016 | February  |
| SCOPE\_12      | KM1601     | Aloha             | 2016 | January   |
| SCOPE\_18      | KOK1608    | Aloha             | 2016 | July      |
| SCOPE\_17      | KOK1607    | Aloha             | 2016 | May       |
| KM1712         | KM1712     | Hawaii - Alaska   | 2017 | August    |
| MESO\_SCOPE    | KM1709     | Aloha             | 2017 | July      |
| HOT-294        | KM1708     | Aloha             | 2017 | June      |
| MGL1704        | MGL1704    | Subtropical front | 2017 | May       |
| HOT297         | KM1717     | Aloha             | 2017 | November  |
| KM1713         | KM1713     | Alaska - Hawaii   | 2017 | September |
| HOT301         | KOK1801    | Aloha             | 2018 | April     |
| HOT300         | KM1805     | Aloha             | 2018 | February  |
| HOT299         | KM1802     | Aloha             | 2018 | January   |
| KOK1806        | KOK1806    | Hawaii            | 2018 | July      |
| HOT304         | KOK1807    | Aloha             | 2018 | July      |
| HOT303         | KOK1804    | Aloha             | 2018 | June      |
| SCOPE\_Falkor1 | FK180310-1 | Hawaii            | 2018 | March     |
| SCOPE\_Falkor2 | FK180310-2 | Hawaii            | 2018 | March     |
| HOT302         | KOK1803    | Aloha             | 2018 | May       |

Also, here’s the list of CMAP variables we’ll use:

``` r
vars = read.csv(file = file.path("/home/sangwonh/repos/cruisedat", "CMAP_vars_all_cruises.csv")) %>% as_tibble()
vars %>% kable() %>% print()
```

| Variable                             | Table\_Name            |
| :----------------------------------- | :--------------------- |
| sss                                  | tblSSS\_NRT            |
| sst                                  | tblSST\_AVHRR\_OI\_NRT |
| Fe                                   | tblPisces\_NRT         |
| PP                                   | tblPisces\_NRT         |
| Si                                   | tblPisces\_NRT         |
| NO3                                  | tblPisces\_NRT         |
| CHL                                  | tblPisces\_NRT         |
| PHYC                                 | tblPisces\_NRT         |
| PO4                                  | tblPisces\_NRT         |
| O2                                   | tblPisces\_NRT         |
| vgosa                                | tblAltimetry\_REP      |
| vgos                                 | tblAltimetry\_REP      |
| sla                                  | tblAltimetry\_REP      |
| ugosa                                | tblAltimetry\_REP      |
| ugos                                 | tblAltimetry\_REP      |
| wind\_stress                         | tblWind\_NRT           |
| eastward\_wind                       | tblWind\_NRT           |
| surface\_downward\_eastward\_stress  | tblWind\_NRT           |
| wind\_speed                          | tblWind\_NRT           |
| surface\_downward\_northward\_stress | tblWind\_NRT           |
| northward\_wind                      | tblWind\_NRT           |
| ftle\_bw\_sla                        | tblLCS\_REP            |
| disp\_bw\_sla                        | tblLCS\_REP            |
| AOU\_WOA\_clim                       | tblWOA\_Climatology    |
| density\_WOA\_clim                   | tblWOA\_Climatology    |
| o2sat\_WOA\_clim                     | tblWOA\_Climatology    |
| oxygen\_WOA\_clim                    | tblWOA\_Climatology    |
| salinity\_WOA\_clim                  | tblWOA\_Climatology    |
| conductivity\_WOA\_clim              | tblWOA\_Climatology    |
| nitrate\_WOA\_clim                   | tblWOA\_Climatology    |
| phosphate\_WOA\_clim                 | tblWOA\_Climatology    |
| silicate\_WOA\_clim                  | tblWOA\_Climatology    |

# Goal

Our goals are:

  - Load covariates from cruise: salinity, temperature, and sunlight
    (PAR).

  - Obtain colocalized covariates from Simons CMAP.

  - Combine the two sources to get a single table for each cruise.

  - Clean and complete (impute) environmental covariates whenever
    possible, to have more complete data.

# Prepare on-board data

## Clean on-board sunlight (PAR)

Load PAR data first.

``` r
## Getting PAR data from the SFL directory 
## sfl_dir = "/home/sangwonh/Dropbox/research/usc/flow-cytometry/data/colocalize/seaflow-sfl-master/curated"
sfl_dir = file.path(outputdir, "cruise-data")
if(!dir.exists(sfl_dir)) dir.create(sfl_dir)
all_sfl_files = list.files(sfl_dir)
all_sfl_files = all_sfl_files[-which(all_sfl_files  == "README.md")]
conversion_table = readRDS(file.path(outputdir, "conversion_table.RDS"))

## Read in SFL tables
datlist_par = list()
for(id in ids_final){
  cruise_sfl_filename = conversion_table %>% filter(Cruise.ID == id) %>% pull(filename)
  par_table = tryCatch({
    read.table(file.path(sfl_dir, cruise_sfl_filename), header = TRUE, sep = "") %>% as_tibble() %>%
      mutate(time = lubridate::as_datetime(DATE)) %>%
      select(time, par = PAR) %>% mutate(par = as.numeric(par)) %>% add_column(id = id)
  }, error = function(e){ return(NULL) })
  if(is.null(par_table)) next
  datlist_par[[id]] = par_table
}
```

PAR data is cleaned (mainly) using `fill_in_par()`.

``` r
## Manual cleaning of a single cruise:
par_new = datlist_par[["KM1709"]]$par %>% median_smooth() %>% median_smooth()
datlist_par[["KM1709"]]$par = par_new

## Manual data removal of a single cruise:
too_early = which(datlist_par[["KM1712"]]$time < lubridate::as_datetime("2017-08-02"))
datlist_par[["KM1712"]][too_early, "par"] = NA

## Aggregate to hourly level.
datlist_par_hourly = datlist_par %>% purrr::map(.%>% coarsen_to_hourly())

## Make the conversion, using our "fill_in_par()" function
datlist_par_hourly_new = Map(fill_in_par, datlist_par_hourly, names(datlist_par_hourly))
plotlist_par_hourly_new = Map(fill_in_par, datlist_par_hourly, names(datlist_par_hourly), makeplot=TRUE) 

## A couple of manual adjustments
datlist_par_hourly_new[["KOK1803"]] = fill_in_par(datlist_par_hourly[["KOK1803"]], "KOK1803", fill_all=TRUE)##, makeplot=TRUE)
plotlist_par_hourly_new[["KOK1803"]] = fill_in_par(datlist_par_hourly[["KOK1803"]], "KOK1803", fill_all=TRUE, makeplot=TRUE)
```

``` r
## Showing the interpolated PAR in green
do.call(gridExtra::grid.arrange, c(plotlist_par_hourly_new, ncol=2)) 
```

![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/plot-cleaned-par-1.png)<!-- -->

## Cleaning on-board salinity and temperature

Temperature and salinity data is fixed and interpolated whenever there’s
a gap shorter than 12 hours.

``` r
datlist_sss_sst_hourly = read.csv(file.path(outputdir, "clean_sfl.csv")) %>% as_tibble() %>%
  rename(id = cruise, time = DATE, sss_cruise = SALINITY, sst_cruise = TEMP) %>%
  filter(id %in% ids_final) %>% 
  mutate(id = as.factor(id)) %>%
  named_group_split(id) %>% purrr::map(.%>% 
                                       mutate(sss_cruise = as.numeric(scale(sss_cruise)),
                                              sst_cruise = as.numeric(scale(sst_cruise)), id = id) %>% 
                                       mutate(time = lubridate::as_datetime(time)) %>%
                                       arrange(time) %>% 
                                       padr::pad() %>% 
                                       fill(id))

## Manual outlier fix for salinity (NOT sure about this)
ii = which(datlist_sss_sst_hourly[["FK180310-2"]]$sss_cruise <= -6)
stopifnot(length(ii) == 1)
datlist_sss_sst_hourly[["FK180310-2"]]$sss_cruise[ii] = mean(datlist_sss_sst_hourly[["FK180310-2"]]$sss_cruise[ii + ((-3):(-1))], na.rm=TRUE)
datlist_sss_sst_hourly[["FK180310-2"]]$sss_cruise = scale(datlist_sss_sst_hourly[["FK180310-2"]]$sss_cruise)

## Fill in sss and sst using spline interpolation
datlist_sss_sst_hourly_new <-
  datlist_sss_sst_hourly %>% purrr::map( .%>% arrange(time) %>% 
    mutate(sss_cruise = zoo::na.spline(sss_cruise, maxgap = 12)) %>% 
    mutate(sst_cruise = zoo::na.spline(sst_cruise, maxgap = 12)) %>%
  fill(id))

## Basic check: the two data have the same cruises?
stopifnot(setdiff(names(datlist_sss_sst_hourly), names(datlist_par_hourly)) %>% length() == 0)
stopifnot(setdiff(names(datlist_par_hourly), names(datlist_sss_sst_hourly)) %>% length() == 0)
```

## Combine (par) with (temperature, salinity)

Now, we’ll combine all the on-board data.

``` r
datlist_combined = list()
for(id in ids_final){
  datlist_combined[[id]] = full_join(datlist_sss_sst_hourly_new[[id]], datlist_par_hourly_new[[id]],
                                     by = c("id", "time")) %>% 
    mutate_at(vars(-time, -id, -contains("par")), ~as.numeric(scale(.x))) %>% 
    mutate_at(vars(par), ~as.numeric(scale(.x, center=FALSE)))
}
```

We’ll additionally add four lagged PAR variables.

``` r
datlist_combined_lagged = datlist_combined %>% purrr::map(. %>% add_all_lagged_par())
```

Here are the finalized on-board covariates:

``` r
## Make a plot of all data
for(ii in 1:8){
  datlist_combined_lagged %>% .[4*(ii-1)+(1:4)] %>% ## .["MGL1704"] %>% ##.["KN210-04"] %>%
    bind_rows() %>% 
    group_by(id) %>% 
    arrange(time) %>% 
    pivot_longer(cols = c("par", "sss_cruise", "sst_cruise")) %>%
    ## pivot_longer(cols = c(contains("par"), "sss_cruise", "sst_cruise")) %>%
    ggplot() + 
    facet_wrap(~id, scales = "free", ncol = 2) +
    geom_point(aes(x = time, y = value, col = name), cex=.5) +
    geom_line(aes(x = time, y = value, col = name)) +
    scale_x_datetime(date_labels = "%b%d\n%Y") +
    theme(strip.text.x = element_text(size = rel(1), face = "bold"))  -> p
  print(p)
}
```

![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/plot-cruise-dat-1.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/plot-cruise-dat-2.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/plot-cruise-dat-3.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/plot-cruise-dat-4.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/plot-cruise-dat-5.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/plot-cruise-dat-6.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/plot-cruise-dat-7.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/plot-cruise-dat-8.png)<!-- -->

``` r
datlist_combined = datlist_combined_lagged   ##.["KN210-04"] %>% 
```

# Combine with CMAP variables

The CMAP variables can be downloaded using this script (not run now):

``` r
datlist = mclapply(ids_final, function(id){
  reslist = lapply(1:nvar, function(ivar){
    tryCatch({

      varname = vars %>% pull(Variable) %>% .[ivar]
      tabname = vars %>% pull(Table_Name) %>% .[ivar]

      if(tabname %in% c("tblWOA_Climatology", "tblPisces_NRT", "tblCHL_REP", "tblModis_PAR")){
        param = list_of_params[[tabname]]
      } else {
        param = list_of_params[["other"]]
      }
      for(isize in 1:20){

        ## Increase the radius
        if(isize > 1) param$latTolerance = param$latTolerance + 0.1
        if(isize > 1) param$lonTolerance = param$lonTolerance + 0.1
        dt = along_track(id,
                         targetTables = tabname,
                         targetVars = varname,
                         temporalTolerance = param$temporalTolerance,
                         latTolerance = param$latTolerance,
                         lonTolerance = param$lonTolerance,
                         depthTolerance = param$depthTolerance,
                         depth1 = param$depth1,
                         depth2 = param$depth2) %>% as_tibble() 
        dt_small = dt %>% dplyr::select(!contains("_std"))
        dt_small_missing_prop = dt_small %>% summarize(naprop = mean(is.na(!!sym(varname)))) %>% unlist()
        print(dt_small_missing_prop)
        if(dt_small_missing_prop == 0) break
      }
      return(dt_small)
    }, error = function(e){
      return(NULL)
    })
  })
  lens = reslist %>% purrr::map(. %>% length()) %>% unlist()
  if(any(lens == 0)){ reslist = reslist[-which(lens == 0)] }
  if(length(reslist) > 0){
    fullres = reslist %>% purrr::reduce(full_join, by = c("time", "lon", "lat"))
    fullres = fullres %>% dplyr::select(!contains("_std"))
    saveRDS(fullres, file = file.path(outputdir, paste0(id, "-adaptive.RDS")))
    return(NULL)
  }
  return(NULL)
}, mc.cores = length(ids_final), mc.preschedule = FALSE)
```

(Assuming the above code has been run, let’s proceed to load and process
it.)

Running this code produces `datlist`, accessed last in 2021-07-20. We’ll
save this to a file, and summarize using a heatmap of data availability,
for each cruise and variable.

``` r
## Read in all the data points
## filename0 = file.path(outputdir, paste0(id, "-adaptive", ".RDS"))
filename0 = here::here("data", base, "cmap-data", paste0("MGL1704", "-adaptive", ".RDS"))
fullres0 = readRDS(file = filename0) %>% add_column(id = "MGL1704") %>% .[c(),]
datlist_cmap = sapply(ids_final, function(id){
  filename = here::here("data", base, "cmap-data", paste0(id, "-adaptive", ".RDS"))
  ## filename = file.path(outputdir, paste0(id, "-adaptive", ".RDS"))
  if(file.exists(filename)){
    fullres = readRDS(file = filename) %>% add_column(id = id)
  } else { return(fullres0) }
}, simplify = FALSE, USE.NAMES = TRUE)
## saveRDS(datlist, file = file.path(outputdir, "datlist.RDS"))

## Make a plot of CMAP data availability
plot_frequency(datlist_cmap)
```

![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/load-cmap-1.png)<!-- -->

The pipeline is:

1.  Complete (interpolate) PAR & salinity & temperature. \<– This is b/c
    the completeness of PAR is super important.

2.  Left-combine this with colocalized variables. \<— This ensures that
    PAR completeness is preserved.

3.  Complete once more. \<– This ensures that all other variables are
    connected.

4.  Scale all variables (EXCEPT par, which has already been scaled).
    Save to file.

Step 2 is here:

``` r
## Ready to combine (cruise data) + (colocalized data)
noncruisedat = datlist_cmap %>% purrr::map(.%>% coarsen_to_hourly())
cruisedat = datlist_combined 

## Basic Check
stopifnot(all(names(noncruisedat) == names(cruisedat)))

## Combine the two (inner join because PAR and lagged PARs are complete now)
combined_dat = Map(function(x,y){ inner_join(x,y, by = c("id", "time")) },
                   noncruisedat, cruisedat)
```

Step 3 is here:

``` r
## Complete the non-cruise data once more.
combined_dat_complete = combined_dat %>%
  purrr::map(. %>%  mutate_at(vars(-time, -lat, -lon, -contains("par"), -sss_cruise, -sst_cruise, -id),
                              ~ zoo::na.approx(.x, maxgap = 12)))
```

By the way, data completion at this step is minimal; here is an example:

``` r
id = ids_final[21]
list(combined_dat%>% .[[id]] %>% add_column(type = "incomplete"),
     combined_dat_complete %>% .[[id]] %>% add_column(type = "complete")) %>%
  data.table::rbindlist(fill = TRUE) %>% 
  select(-id) %>% 
  ## mutate_at(vars(-time, -lat, -lon), scale) %>%
  pivot_longer(cols = -one_of(c("time", "lat", "lon", "type"))) %>%
  ggplot() + facet_wrap(~name, scales = "free_y", ncol=2) +
  geom_line(aes(x=time, y=value), . %>% filter(type=="complete"), col = rgb(0,0,0,0.3))+
  geom_point(aes(x=time, y=value), . %>% filter(type=="complete"), col='green', cex=.5) +
  geom_point(aes(x=time, y=value), . %>% filter(type=="incomplete"),cex=.5) +
  scale_x_datetime(breaks = scales::date_breaks("1 day"), date_labels = "%b %d %Y")  +
  theme(axis.text.x = element_text(angle = 90)) -> p
print(p)
```

![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/fill-plot-1.png)<!-- -->

Remove all the rows with any NAs; the summarizing heatmap should have
all 1’s or NA’s (grey) now.

``` r
## Remove all rows with any NAs 
combined_dat_complete = combined_dat_complete %>% lapply(., function(a){
  if(nrow(a) == 0) return(a)
  a %>% select(where(~!all(is.na(.x)))) %>% na.omit() 
})

## Make plot of frequency; this should be all 1's wherever data exists.
plot_frequency(combined_dat_complete)
```

![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/remove-na-1.png)<!-- -->

# Save data

Lastly, scale all variables EXCEPT par, which has already been scaled.
Then, save to RDS files.

``` r
## Temporary
bigdat = combined_dat_complete %>% data.table::rbindlist(use.names = TRUE, fill = TRUE, idcol=NULL) %>% as_tibble()

for(id in ids_final){
  if(nrow(combined_dat[[id]]) == 0) next

  ## Unscaled version
  onedat = combined_dat_complete %>% .[[id]] 
  filename = paste0(id, "-covariates-unscaled.RDS")
  ## saveRDS(onedat, file = file.path(outputdir, filename))
  saveRDS(onedat, file = here::here("data", base, filename))

  ## Scaled version
  onedat_scaled = onedat %>% 
    mutate_at(vars(-time, -id, -contains("par")), ~as.numeric(scale(.x)))
  filename = paste0(id, "-covariates-scaled.RDS")
  ## saveRDS(onedat_scaled, file = file.path(outputdir, filename))
  saveRDS(onedat, file = here::here("data", base, filename))

  ## Common scaled version
  onedat = combined_dat_complete %>% .[[id]] 
  onedat_scaled = onedat %>% 
    mutate_at(vars(-time, -id, -contains("par")), ~as.numeric(scale(.x)))
  filename = paste0(id, "-covariates-scaled.RDS")
  ## saveRDS(onedat_scaled, file = file.path(outputdir, filename))
  saveRDS(onedat, file = here::here("data", base, filename))

  ## Plot scaled data
  p = onedat_scaled %>%
    pivot_longer(-one_of("time", "lat", "lon", "id"))  %>% 
    ggplot() + facet_wrap(~name, scales = "free_y", ncol=2) +
    geom_line(aes(x=time, y=value), lwd=.8, col=rgb(0,0,0,0.3)) + ##, col=name))
    geom_point(aes(x=time, y=value), cex=.3) +
    ggtitle(id)  
  print(p)
  ## ggsave(file = paste0(id, "-covariates.png"), width=15, height=10)
}
```

![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-1.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-2.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-3.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-4.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-5.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-6.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-7.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-8.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-9.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-10.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-11.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-12.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-13.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-14.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-15.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-16.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-17.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-18.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-19.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-20.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-21.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-22.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-23.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-24.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-25.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-26.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-27.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-28.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-29.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-30.png)<!-- -->![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/scale-and-save-31.png)<!-- -->

Some odd things 1. PP seems to shoot up in one or two cruise. 2. Fe also
shoots up, but to a lesser extent. 3. WOA variables for the same cruise
as PP’s culprit ()–CHL also shoots up. 4. The first cruise FK180310-1
has very high values of PHYC, O2, CHL

# Visualize all data together

We visualize each covariate in a separate panel:

``` r
bigdat = combined_dat_complete %>% data.table::rbindlist(use.names = TRUE, fill = TRUE, idcol=NULL) %>% as_tibble()
## bigdat = bigdat %>% group_by(id) %>% mutate_at(vars(-time, -id, -contains("par")), ~as.numeric(scale(.x))) %>% ungroup()
bigdat_long = bigdat %>% pivot_longer(cols = -one_of(c("time", "lat", "lon", "id")))
vals = bigdat_long$name %>% unique()
bigdat_long$time = as.factor(bigdat_long$time)
chunks = split(1:27, ceiling(seq_along(1:27)/3))
for(chunk in chunks){
  bigdat_long %>% filter(name %in% vals[chunk]) %>% 
    ggplot() +
    theme_bw() +
    facet_grid(rows = vars(name), scales = "free_y") +
    geom_point(aes(x=time, y=value, col = id), cex=.5) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    guides(colour = guide_legend(nrow = 2)) +
    theme(legend.position="bottom") -> p
  ## filename = paste0(vals[chunk] %>% substr(0,3), collapse="-") %>% paste0("-scaled.png")
  filename = paste0(vals[chunk] %>% substr(0,3), collapse="-") %>% paste0(".png")
  ggsave(file = here::here("data", base, filename), width = 20, height=10)
}

## Better (but larger) plots
bigdat = combined_dat_complete %>% data.table::rbindlist(use.names = TRUE, fill = TRUE, idcol=NULL) %>% as_tibble()
bigdat_long = bigdat %>% pivot_longer(cols = -one_of(c("time", "lat", "lon", "id")))
plist = list()
for(val in vals){
  print(val)
  bigdat_long %>% filter(name %in% c(val)) %>% 
  ggplot() +
  theme_bw() +
  facet_grid(.~id, scales = "free",  space='free_x') +
  geom_line(aes(x=time, y=value), lwd=.5, col=rgb(0,0,0,0.3)) +
  geom_point(aes(x=time, y=value), cex=.3, col='red') +
  guides(colour = guide_legend(nrow = 2)) +
  xlab("") +
    ylab("") +
    ggtitle(val) +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(strip.text.x = element_text(size = rel(.5), face="bold")) -> p
  plist[[val]] = p
}
```

    ## [1] "Si"
    ## [1] "NO3"
    ## [1] "CHL"
    ## [1] "PHYC"
    ## [1] "PO4"
    ## [1] "O2"
    ## [1] "AOU_WOA_clim"
    ## [1] "density_WOA_clim"
    ## [1] "o2sat_WOA_clim"
    ## [1] "oxygen_WOA_clim"
    ## [1] "salinity_WOA_clim"
    ## [1] "conductivity_WOA_clim"
    ## [1] "nitrate_WOA_clim"
    ## [1] "phosphate_WOA_clim"
    ## [1] "silicate_WOA_clim"
    ## [1] "sss_cruise"
    ## [1] "sst_cruise"
    ## [1] "par"
    ## [1] "par_3"
    ## [1] "par_6"
    ## [1] "par_9"
    ## [1] "par_12"
    ## [1] "sst"
    ## [1] "Fe"
    ## [1] "PP"
    ## [1] "vgosa"
    ## [1] "vgos"
    ## [1] "sla"
    ## [1] "ugosa"
    ## [1] "ugos"
    ## [1] "sss"

``` r
## Save these to files
library(gridExtra)
chunks = split(1:27, ceiling(seq_along(1:27)/3))
for(chunk in chunks){
  print(chunk)
  pp = do.call("grid.arrange", c(plist[chunk], ncol=1))
  filename = paste0(vals[chunk] %>% substr(0,3), collapse="-") %>% paste0("-new.png")
  ggsave(plot = pp, file = here::here("data", base, filename), width = 40, height=20)
}
```

    ## [1] 1 2 3

![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/unnamed-chunk-3-1.png)<!-- -->

    ## [1] 4 5 6

![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/unnamed-chunk-3-2.png)<!-- -->

    ## [1] 7 8 9

![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/unnamed-chunk-3-3.png)<!-- -->

    ## [1] 10 11 12

![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/unnamed-chunk-3-4.png)<!-- -->

    ## [1] 13 14 15

![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/unnamed-chunk-3-5.png)<!-- -->

    ## [1] 16 17 18

![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/unnamed-chunk-3-6.png)<!-- -->

    ## [1] 19 20 21

![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/unnamed-chunk-3-7.png)<!-- -->

    ## [1] 22 23 24

![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/unnamed-chunk-3-8.png)<!-- -->

    ## [1] 25 26 27

![](/home/sangwonh/repos/flowmixapp/data/00-covariates/figures/unnamed-chunk-3-9.png)<!-- -->
