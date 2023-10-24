# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: E.O. Lewis
# Date: 09/23/2023
# Purpose: POST EHD-ETC-indexbuilds - explore and analyze sensitivity re: urban/rural correlations
# PRE-code: "/PhD Work/WSDOT Equity/Analysis/transpoequity_indices/EHD-ETC-indexbuilds.R"
# source("/PhD Work/WSDOT Equity/Analysis/transpoequity_indices/EHD-ETC-indextests.R", echo=T)
#
# NOTES:
# partially derivative of code developed by j.f. found at https://github.com/jfrostad/ehd_mapsense/tree/main/code
# forest plot code derivative of code developed by K. Hoffman found at https://www.khstats.com/blog/forest-plots/ 
#***********************************************************************************************************************

##----CONFIG------------------------------------------------------------------------------------------------------------
# clear memory
rm(list=ls())

# set locations & packages
user <- Sys.info()['user']
main.dir <- file.path('C:/Users', user, 'OneDrive/Documents/PhD Work/WSDOT Equity/Analysis/')
my_repo <- file.path(main.dir, 'transpoequity_indices')
setwd(my_repo)

###BASE IN###
data.in <- file.path(main.dir, 'Data')

###OUTS###
data.out <- file.path(my_repo, 'data')
plots.out <- file.path(my_repo, 'plots')

#load necessary packages
library(pacman)
pacman::p_load(readxl, here, snakecase, janitor, data.table, dplyr, naniar, stringr, magrittr, scales, Hmisc,
               ggtern, ggplot2, ggpubr, ggridges, ggrepel, ggdist, grid, gridExtra, RColorBrewer, #viz pkgs
               sf, viridis, farver, reldist, ggnewscale, ggallin, biscale, cowplot, patchwork, gridarrange,
               tigris, tidycensus, ggcorrplot, forcats,
               broom.mixed, ggstance, jtools, factoextra, scam,
               epitools, 
               stargazer, zoo)

#***********************************************************************************************************************

##---TEST URBAN/RURAL DISADVANTAGE REPRESENTATION-----------------------------------------------------------------------

#read in data
ehd_ur <- read_sf(here(data.out, 'ehd_scores.shp')) %>%
  rename(
    is_urban = is_urbn   #for some reason this keeps getting shortened when writing sf
  )
etc_ur <- read_sf(here(data.out, 'etc_scores.shp'))

#remove geometry for calculations and ensure only complete cases (some water-only tracts remain)
ehd_ur <- st_drop_geometry(ehd_ur[complete.cases(ehd_ur$is_urban),])
etc_ur <- st_drop_geometry(etc_ur[complete.cases(etc_ur$is_urban),])



# create vectors of iterations
#suffixes <- c("_z_h", "_mm_h", "_d_h", "_z_nh", "_mm_nh", "_d_nh")
                #NOTE: sometimes the 'n' gets dropped from "nh" depending on how the st wrote out
suffixes <- c("_z_h", "_mm_h", "_d_h", "_z_n", "_mm_n", "_d_n")

# EHD -------
# create vectors of variable thresholds unique to EHD for looping
#thresholds <- c('fsi9', 'fsi7')
thresholds <- c('fs9', 'fs7')
                #NOTE: sometimes the 'i' gets dropped/added depending on how the st wrote out

# create a list for raw calcs and a df of key outputs for analysis
ehd_OR_list <- list()
ehd_OR <- data.frame(
  it = character(),     # Create empty columns with the desired names
  estimate = numeric(),
  conf.low = numeric(),
  conf.up = numeric(),
  midp = numeric(),
  fisher = numeric(),
  chisq = numeric()
)

# loop through odds ratio calculations and save outputs to df
for (thresh in thresholds) {
  for (suffix in suffixes) {
    ct <- table(ehd_ur$is_urban,
                ehd_ur[[paste0(thresh, suffix)]] )
    
    rownames(ct) <- c("rural", "urban")
    colnames(ct) <- c("no", "yes")
    
    or <- epitools::oddsratio(ct)
    
    ehd_OR_list[[paste0(thresh, suffix)]] <- or
    
    row <- data.frame(
      it = paste0(thresh, suffix), 
      estimate = or$measure['urban', 'estimate'],
      conf.low = or$measure['urban', 'lower'],
      conf.up = or$measure['urban', 'upper'],
      midp = or$p.value['urban', 'midp.exact'],
      fisher = or$p.value['urban', 'fisher.exact'],
      chisq = or$p.value['urban', 'chi.square']
    )
    
    ehd_OR <- rbind(ehd_OR, row)
  }
}

ehd_OR <- ehd_OR %>%
  mutate(
    thresh = case_when(
      grepl("7", it) ~ "top 40%",
      grepl("9", it) ~ "top 20%"
      ),
    index ='EHD'
  )

rm(ct, or, row, suffix, thresh, thresholds)  #clean-up

# ETC -------

# create vectors of iterations
suffixes <- c("_z_h", "_mm_h", "_d_h", "_z_nh", "_mm_nh", "_d_nh")

# create a list for raw calcs and a df of key outputs for analysis
etc_OR_list <- list()
etc_OR <- data.frame(
  it = character(),     # Create empty columns with the desired names
  estimate = numeric(),
  conf.low = numeric(),
  conf.up = numeric(),
  midp = numeric(),
  fisher = numeric(),
  chisq = numeric()
)

# loop through odds ratio calculations and save outputs to df
for (suffix in suffixes) {
  ct <- table(etc_ur$is_urban,
              etc_ur[[paste0('fri', suffix)]] )
  
  rownames(ct) <- c("rural", "urban")
  colnames(ct) <- c("no", "yes")
  
  or <- epitools::oddsratio(ct)
  
  etc_OR_list[[paste0('fri', suffix)]] <- or
  
  row <- data.frame(
    it = paste0('fri', suffix), 
    estimate = or$measure['urban', 'estimate'],
    conf.low = or$measure['urban', 'lower'],
    conf.up = or$measure['urban', 'upper'],
    midp = or$p.value['urban', 'midp.exact'],
    fisher = or$p.value['urban', 'fisher.exact'],
    chisq = or$p.value['urban', 'chi.square']
  )
  
  etc_OR <- rbind(etc_OR, row)
}

etc_OR$thresh <- 'top 35%'
etc_OR$index <- 'ETC'

rm(ct, or, row, suffix)  #clean-up


# bind BOTH & prep for analysis --------------

#bind
both_OR <- rbind(ehd_OR, etc_OR)

both_OR <- both_OR %>%
  mutate(
    rescale = case_when(
      grepl("z", it) ~ "z-scale",
      grepl("mm", it) ~ "min-max",
      grepl("d", it) ~ "deciles"
    ),
    hier = case_when(
      grepl("_h", it) ~ "hierarchical",
      grepl("nh", it) ~ "non-hierarchical"
    ),
    p.value = case_when(
      chisq < .001 ~ "***",
      chisq < .01 ~ "**",
      chisq < .05 ~ "*",
      TRUE ~ str_pad( # everything >= 0.05, present as-is rounded to 3 decimal places
        as.character(round(chisq, 3)),
        width = 4,
        pad = "0",
        side = "right"
      )
    ),
    across(
      c(estimate, conf.low, conf.up),
      ~ round(as.numeric(.x), 2)
    ),
    version = 
      paste(index, rescale, hier, thresh, sep = ', '),
    estimate_lab = 
      paste0(estimate, " (", conf.low, "-", conf.up, ")")
  ) 

write.csv(both_OR, file.path(data.out, 'oddsratios_both.csv'), row.names = FALSE)

#***********************************************************************************************************************


##---SCRAP -------------------------------------------------------------------------------------------------------------

# my scraps

ehd_ur <- st_drop_geometry(ehd_ur[complete.cases(ehd_ur$is_urban),
                                  c(grep('is_urban', names(ehd_ur)),
                                    grep('fsi', names(ehd_ur)))])



# code originally from frey

# clear memory
rm(list=ls())

#set opts
set.seed(98118)
options(scipen=999) #readability
#use cairo to render instead of quartz (quartz causes big slowdowns with geom_sf)
if(!identical(getOption("bitmapType"), "cairo") && isTRUE(capabilities()[["cairo"]])){
  options(bitmapType = "cairo")
}

#set control flow params
reload <- F #set true if you want to reprep all the data
remap <- F #set true if you want to redraw all the data maps
rerun_gsa <- F #set true if you are rerunning the global sensitivity analysis
gsa_cores <- 1

#version
gsa_version <- 1 #first draft with params=
gsa_version <- 2 #reduced noise
gsa_version <- 3 #switch to MARC instead of just avg. rerunning with fixed PCA sign

## Set core_repo location
user            <- Sys.info()['user']
main.dir         <- ifelse(Sys.info()["sysname"] == "Linux",
                           file.path('/homes', user, '_code/ehd_mapsense/'),
                           file.path('C:/Users', user, 'Documents/ehd_mapsense/'))
my_repo <- file.path(main.dir, 'repo')
setwd(my_repo)

#load packages
#TODO only relevant to running in linux on shared cluster
package_lib    <- ifelse(.Platform$GUI=='RStudio',
                         '/mnt/share/homes/jfrostad/_code/_lib/pkg_R_ehd',
                         '/mnt/share/homes/jfrostad/_code/_lib/pkg_R_int')
## Load libraries and  MBG project functions.
.libPaths(package_lib)

#TODO cleanup old packages
pacman::p_load(readxl, snakecase, janitor, data.table, naniar, stringr, magrittr, scales, Hmisc,
               ggtern, ggplot2, ggpubr, ggridges, ggrepel, ggdist, grid, gridExtra, RColorBrewer, #viz pkgs
               sf, viridis, farver, reldist, ggnewscale, ggallin, biscale, cowplot,
               tigris, tidycensus, ggcorrplot,
               broom.mixed, ggstance, jtools, factoextra, scam,
               COINr, randtoolbox, sensobol, #sens packages
               stargazer,
               parallel, pbmcapply,
               cluster, ggdendro, #HCA packages
               #caret, mlbench, randomForest, pls,
               zoo)


#also bring in the census tracts shapefile in order to do some cartography
#can be downloaded from the census website using tigris
tracts_20 <- tracts('WA', year=2020, cb=T) %>% 
  st_transform(32148) %>% 
  erase_water(area_threshold = 0.9) %>% #intersect with water overlay and remove
  mutate('GEOID'=substring(GEOID, 10)) #remove the excess first 9 chr and rename GEOID


#use the water shapefile as an overlay
counties_list <- counties('WA', cb=T)
water_sf <- area_water('WA', counties_list$COUNTYFP %>% unique) %>% 
  st_simplify(preserveTopology = TRUE, dTolerance = 100)

#also overlay roads
road_sf <- primary_secondary_roads(state='WA') %>% 
  st_simplify(preserveTopology = TRUE, dTolerance = 100)

#also overlay places
places_sf <- places(state = 'WA', cb = T) 
# #***********************************************************************************************************************
