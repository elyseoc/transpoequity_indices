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
               COINr, randtoolbox, sensobol, #sens packages
               stargazer,
               parallel, pbmcapply,
               cluster, ggdendro, #HCA packages
               #caret, mlbench, randomForest, pls,
               zoo)

#***********************************************************************************************************************

##----URBAN/RURAL DESIGNATIONS------------------------------------------------------------------------------------------------------------


# use sf package to bring in tracts 
tracts_20 <- tracts('WA', year=2020, cb=T) %>% 
  st_transform(32148) %>% 
  mutate(GEOID = as.numeric(GEOID))   #set variable as.numeric for later joining

tracts_10 <- tracts('WA', year=2010, cb=T) %>% 
  st_transform(32148) %>%
  rename(AFFGEOID = GEO_ID) %>%   #note that the variable GEO_ID in 2010 tracts has this AFFGEOID equivalent in 2020 - set to this for consistency
  mutate(GEOID = as.numeric(paste0(STATE, COUNTY, TRACT)))   #set variable as.numeric for later joining


#read in WSDOT urban areas layer
urban_a <- read_sf(here(file.path(
  data.in, '00_Boundaries/WSDOT_-_Highway_Urban_and_Urbanized_Areas'),
  'WSDOT_-_Highway_Urban_and_Urbanized_Areas.shp')) %>% 
  st_transform(32148)

# Perform a spatial join and calculate the overlap area for both sets of TIGER tract lines

df_names <- c("tracts_10", "tracts_20")
df_list <- list(tracts_10, tracts_20)

for (i in seq_along(df_list)) {
  df <- df_list[[i]]
  
  overlap <- st_intersection(df, urban_a)
  overlap$overlap_area <- as.numeric(st_area(overlap))
  overlap <- overlap %>% 
    select(GEOID, overlap_area) %>% 
    st_drop_geometry()
  
  df$area <- as.numeric(st_area(df))
  
  tracts_calcs <- df %>% 
    select(GEOID, area) %>% 
    st_drop_geometry()
  
  tracts_calcs <- left_join(tracts_calcs, overlap)
  
  tracts_calcs <- tracts_calcs %>%
    group_by(GEOID) %>%
    summarise(area = first(area), overlap_area = sum(overlap_area))
  
  # Calculate the overlap ratio
  tracts_calcs$overlap_ratio <- tracts_calcs$overlap_area / tracts_calcs$area
  tracts_calcs[is.na(tracts_calcs)] <- 0 # NAs exist where there is no overlap so coerce to 0
  
  # Assign urban/not for tracts w/ >= 50% urban area
  df$is_urban <- ifelse(tracts_calcs$overlap_ratio >= 0.5, 1, 0)
  
  # Assign the modified data frame back to the list
  df_list[[i]] <- df
  
  assign(df_names[i], df_list[[i]])
}

rm(df, df_list, df_names, i, overlap, tracts_calcs, urban_a)    #clean-up


#bring in ETC & EHD processed scoring data
ehd_out <- read.csv(file.path(data.out, 'ehd_scores'))
etc_out <- read.csv(file.path(data.out, 'etc_scores'))


# join the index scores to the related shp of tracts
ehd_out <- ehd_out %>%
  left_join(select(tracts_10, GEOID, is_urban))


# write out shps with added scoring data
write_sf(ehd_out, file.path(data.out, 'ehd_scores_urban-rural-des1.shp'))
st_write(tracts_20, file.path(data.out, 'etc_scores_urban-rural-des.shp'))


#***********************************************************************************************************************










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
