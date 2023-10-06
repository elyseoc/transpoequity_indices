# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: E.O. Lewis
# Date: 09/23/2023
# Purpose: POST EHD-ETC-indexbuilds - explore and analyze sensitivity re: urban/rural correlations
# PRE-code: "/PhD Work/WSDOT Equity/Analysis/transpoequity_indices/EHD-ETC-indexbuilds.R"
# source("/PhD Work/WSDOT Equity/Analysis/transpoequity_indices/EHD-ETC-indextests.R", echo=T)
#
# NOTES:
# partially derivative of code developed by j.f. found at https://github.com/jfrostad/ehd_mapsense/tree/main/code
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
               sf, viridis, farver, reldist, ggnewscale, ggallin, biscale, cowplot,
               tigris, tidycensus, ggcorrplot,
               broom.mixed, ggstance, jtools, factoextra, scam,
               epitools, 
               stargazer, zoo)

#***********************************************************************************************************************

#read in data
ehd_ur <- read_sf(here(data.out, 'ehd_scores_urban-rural-des.shp'))
etc_ur <- read_sf(here(data.out, 'etc_scores_urban-rural-des.shp'))


# look at differences btw urban/rural scoring
ehd_counts <- st_drop_geometry(ehd_ur[complete.cases(ehd_ur$is_urban),
                                      c(grep('is_urban', names(ehd_ur)),
                                        grep('fsi', names(ehd_ur)))])



thresholds <- c('fsi9', 'fsi7')
suffixes <- c("_z_h", "_mm_h", "_d_h", "_z_nh", "_mm_nh", "_d_nh")

# Loop through the suffixes and create binary output variables
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

for (thresh in thresholds) {
  for (suffix in suffixes) {
    ct <- table(ehd_counts$is_urban,
          ehd_counts[[paste0(thresh, suffix)]] )
    
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

rm(ct, or, row, suffix, thresh)  #clean-up


ehd_OR <- ehd_OR %>%
  mutate(
    rescale = case_when(
    grepl("z", it) ~ "z-scale",
    grepl("mm", it) ~ "min-max",
    grepl("d", it) ~ "deciles"
    ),
    thresh = case_when(
      grepl("7", it) ~ "7-10",
      grepl("9", it) ~ "9-10"
    ),
    hier = case_when(
      grepl("_h", it) ~ "hierarchical",
      grepl("nh", it) ~ "non-hierarchical"
    )
  )










## NEEDS WORK -----------------

## for some reason, the geometry is not being recognized 
tracts_10 <- tracts_10 #%>%
#erase_water(area_threshold = 0.9) %>% #intersect with water overlay and remove
left_join(ehd_out)

tracts_20 <- tracts_20 #%>%
#erase_water(area_threshold = 0.9) %>% #intersect with water overlay and remove
left_join(etc_out)


# write out shps with added scoring data
write_sf(tracts_10, file.path(data.out, 'ehd_scores_urban-rural-des1.shp'))
st_write(tracts_20, file.path(data.out, 'etc_scores_urban-rural-des.shp'))



variables_to_plot <- c(
  "pctnvh", "avgcmm", "trnfrq", "jb45dr", "drvpoi",
  "wlkpoi", "avghht", "ftltsp"
)

# Create a data frame with the selected variables and their scaled versions
selected_data <- etc_calcs[, c(
  variables_to_plot,
  paste0(variables_to_plot, "_z"),
  paste0(variables_to_plot, "_mm"),
  paste0(variables_to_plot, "_d")
)]

# Melt the data frame for plotting
melted_data <- reshape2::melt(selected_data)

variable_categories <- rep("Raw", length(variables_to_plot))
variable_categories <- c(variable_categories, rep("z-scaled", length(variables_to_plot)))
variable_categories <- c(variable_categories, rep("Min-Max", length(variables_to_plot)))
variable_categories <- c(variable_categories, rep("Deciles", length(variables_to_plot)))

# Add the new 'Category' variable to 'melted_data'
melted_data$Category <- factor(variable_categories, levels = c("Raw", "z-scaled", "Min-Max", "Deciles"))


# Plot the density plots
ggplot(data = melted_data, aes(x = value, fill = Category)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~variable, scales = "free_y") +
  scale_fill_manual(values = c("blue", "green", "red", "purple")) +
  labs(title = "Density Plots of Original and Scaled Variables") +
  theme_minimal()




##---SCRAP -------------------------------------------------------------------------------------------------------------
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
