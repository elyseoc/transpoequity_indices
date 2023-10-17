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
pacman::p_load(readxl, here, data.table, dplyr, tidyr, units,
               sf, tidycensus)

#***********************************************************************************************************************

##----FUNCTIONS------------------------------------------------------------------------------------------------------------
decile_scale <- function(x) {
  x_jittered <- x + runif(length(x), -1e-10, 1e-10)  # Add small jitter
  breaks <- quantile(x_jittered, probs = seq(0, 1, 0.1), na.rm = TRUE)
  cutted <- cut(x_jittered, breaks = breaks, labels = FALSE, include.lowest = TRUE)
  return(cutted)
}

min_max_scale <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

#***********************************************************************************************************************

##----ETC CALCS------------------------------------------------------------------------------------------------------------
etc <- read_sf(here(file.path(
  data.in, 'USDOT_ETC/DOT_Index_State_5_3'),'DOT_Index_State_5_3.shp')) %>%
  subset(stbbr=='WA') %>% 
  #rename tracts FIPS code column name to GEOID to match other dfs
  dplyr::rename(GEOID = trctfp)


# Select the base variables needed for calcs + retain the GEOID
etc_all_vars <- c(
  "pctnvh", "avgcmm", "trnfrq", "jb45dr", "drvpoi",
  "wlkpoi", "avghht", "ftltsp", "ozn", "pm25",
  "dslpm", "cncrtx", "hzrdst", "txcrls", "trtdsp",
  "rskmns", "clmns", "ldmns", "ppr80h", "hghvlr",
  "rlwys", "arprts", "prts", "imprdw", "asthm",
  "cncr", "bldprs", "dbts", "mntlhl", "ppvrty",
  "pndplm", "pnmply", "phstnr", "phbrd7", "pnnsrd",
  "pnntrn", "endnql", "p65ldr", "p17yng", "pdsb",
  "plmng", "pmblhm", "annlls", "extht", "extrmp",
  "drghtd", "pctnnd", "mnmp")

etc <- st_drop_geometry(etc) #drop geometry to allow for numerical calcs
#reverse the distributions for "Frequency of Transit Services per Sq Mi" and "Jobs within a 45-min Drive" so they add appropriately to the disadvantage estimation
etc$trnfrq <- max(etc$trnfrq) - etc$trnfrq + min(etc$trnfrq)
etc$jb45dr <- max(etc$jb45dr) - etc$jb45dr + min(etc$jb45dr)


# Calculate z-scores for the selected variables
z_scores <- scale(etc[, etc_all_vars])


# Shift all z-scores to be positive
z_scores[is.na(z_scores)] <- 0  #NOTE: there are no lead mines in WA state, which yield "NA" values
min_z_score <- min(z_scores)
shifted_z_scores <- z_scores - min_z_score

# Add the shifted z-scored variables to the 'etc' data frame
colnames(shifted_z_scores) <- paste0(colnames(z_scores), "_z")
etc <- cbind(etc[c(1)], etc[, etc_all_vars], shifted_z_scores)


# generate min-max scaled variables
min_max_scaled_vars <- as.data.frame(apply(etc[, etc_all_vars], 2, min_max_scale))
min_max_scaled_vars[is.na(min_max_scaled_vars)] <- 0 # again, no lead mines, need 0 coerced

# Add the min-max scaled variables to the 'etc' data frame
colnames(min_max_scaled_vars) <- paste0(etc_all_vars, "_mm")
etc <- cbind(etc, min_max_scaled_vars)


# Perform decile scaling for the selected variables
decile_scaled_vars <- as.data.frame(apply(etc[, etc_all_vars], 2, decile_scale))

# Add the decile scaled variables to the 'etc' data frame
colnames(decile_scaled_vars) <- paste0(etc_all_vars, "_d")
etc <- cbind(etc, decile_scaled_vars)


#clean-up
rm(decile_scaled_vars, min_max_scaled_vars, shifted_z_scores, 
   z_scores, min_z_score)
gc()


# calculate revised indices

# Create a new variable for the subindex of transportation access for each standardization type
# then scale each accordingly
etc$trnsac_z <- rowSums(etc[, c(
  "pctnvh_z", "avgcmm_z", "trnfrq_z", "jb45dr_z", "drvpoi_z", "wlkpoi_z"
)])
etc$trnsac_z <- scale(etc$trnsac_z, center = TRUE, scale = TRUE)

etc$trnsac_mm <- rowSums(etc[, c(
  "pctnvh_mm", "avgcmm_mm", "trnfrq_mm", "jb45dr_mm", "drvpoi_mm", "wlkpoi_mm"
)])
etc$trnsac_mm <- min_max_scale(etc$trnsac_mm)

etc$trnsac_d <- rowSums(etc[, c(
  "pctnvh_d", "avgcmm_d", "trnfrq_d", "jb45dr_d", "drvpoi_d", "wlkpoi_d"
)])
etc$trnsac_d <- decile_scale(etc$trnsac_d)


# do the same for the Future Climate Risk variable
etc$ftclrk_z <- rowSums(etc[, c(
  "extht_z", "extrmp_z", "drghtd_z", "pctnnd_z"
)])
etc$ftclrk_z <- scale(etc$ftclrk_z, center = TRUE, scale = TRUE)

etc$ftclrk_mm <- rowSums(etc[, c(
  "extht_mm", "extrmp_mm", "drghtd_mm", "pctnnd_mm"
)])
etc$ftclrk_mm <- min_max_scale(etc$ftclrk_mm)

etc$ftclrk_d <- rowSums(etc[, c(
  "extht_d", "extrmp_d", "drghtd_d", "pctnnd_d"
)])
etc$ftclrk_d <- decile_scale(etc$ftclrk_d)


#calculate each component index percentile rank score

# transportation insecurity
etc$trncmpr_z <- rowSums(etc[, c("avghht_z", "ftltsp_z", "trnsac_z")])
etc$trncmpr_mm <- rowSums(etc[, c("avghht_mm", "ftltsp_mm", "trnsac_mm")])
etc$trncmpr_d <- rowSums(etc[, c("avghht_d", "ftltsp_d", "trnsac_d")])

# Compute the percentile rank of the sums for each re-scaling technique
etc <- etc %>%
  mutate(
    trncmpr_z = percent_rank(trncmpr_z),
    trncmpr_mm = percent_rank(trncmpr_mm),
    trncmpr_d = percent_rank(trncmpr_d)
  )

# environmental burden
etc$evncmpr_z <- rowSums(etc[, c(
  "ozn_z", "pm25_z", "dslpm_z", "cncrtx_z", "hzrdst_z",
  "txcrls_z", "trtdsp_z", "rskmns_z", "clmns_z", "ldmns_z",
  "ppr80h_z", "hghvlr_z", "rlwys_z", "arprts_z", "prts_z", "imprdw_z"
)])
etc$evncmpr_mm <- rowSums(etc[, c(
  "ozn_mm", "pm25_mm", "dslpm_mm", "cncrtx_mm", "hzrdst_mm",
  "txcrls_mm", "trtdsp_mm", "rskmns_mm", "clmns_mm", "ldmns_mm",
  "ppr80h_mm", "hghvlr_mm", "rlwys_mm", "arprts_mm", "prts_mm", "imprdw_mm"
)])
etc$evncmpr_d <- rowSums(etc[, c(
  "ozn_d", "pm25_d", "dslpm_d", "cncrtx_d", "hzrdst_d",
  "txcrls_d", "trtdsp_d", "rskmns_d", "clmns_d", "ldmns_d",
  "ppr80h_d", "hghvlr_d", "rlwys_d", "arprts_d", "prts_d", "imprdw_d"
)])
etc <- etc %>%
  mutate(
    evncmpr_z = percent_rank(evncmpr_z),
    evncmpr_mm = percent_rank(evncmpr_mm),
    evncmpr_d = percent_rank(evncmpr_d)
  )


# health vulnerability
etc$hltcmpr_z <- rowSums(etc[, c(
  "asthm_z", "cncr_z", "bldprs_z", "dbts_z", "mntlhl_z"
)])
etc$hltcmpr_mm <- rowSums(etc[, c(
  "asthm_mm", "cncr_mm", "bldprs_mm", "dbts_mm", "mntlhl_mm"
)])
etc$hltcmpr_d <- rowSums(etc[, c(
  "asthm_d", "cncr_d", "bldprs_d", "dbts_d", "mntlhl_d"
)])
etc <- etc %>%
  mutate(
    hltcmpr_z = percent_rank(hltcmpr_z),
    hltcmpr_mm = percent_rank(hltcmpr_mm),
    hltcmpr_d = percent_rank(hltcmpr_d)
  )


# social vulnerability
etc$sclcmpr_z <- rowSums(etc[, c(
  "ppvrty_z", "pndplm_z", "pnmply_z", "phstnr_z", "phbrd7_z",
  "pnnsrd_z", "pnntrn_z", "endnql_z", "p65ldr_z", "p17yng_z",
  "pdsb_z", "plmng_z", "pmblhm_z"
)])
etc$sclcmpr_mm <- rowSums(etc[, c(
  "ppvrty_mm", "pndplm_mm", "pnmply_mm", "phstnr_mm", "phbrd7_mm",
  "pnnsrd_mm", "pnntrn_mm", "endnql_mm", "p65ldr_mm", "p17yng_mm",
  "pdsb_mm", "plmng_mm", "pmblhm_mm"
)])
etc$sclcmpr_d <- rowSums(etc[, c(
  "ppvrty_d", "pndplm_d", "pnmply_d", "phstnr_d", "phbrd7_d",
  "pnnsrd_d", "pnntrn_d", "endnql_d", "p65ldr_d", "p17yng_d",
  "pdsb_d", "plmng_d", "pmblhm_d"
)])
etc <- etc %>%
  mutate(
    sclcmpr_z = percent_rank(sclcmpr_z),
    sclcmpr_mm = percent_rank(sclcmpr_mm),
    sclcmpr_d = percent_rank(sclcmpr_d)
  )


#finally, climate & disaster risk burden
etc$clmcmpr_z <- rowSums(etc[, c("annlls_z", "mnmp_z", "ftclrk_z")])
etc$clmcmpr_mm <- rowSums(etc[, c("annlls_mm", "mnmp_mm", "ftclrk_mm")])
etc$clmcmpr_d <- rowSums(etc[, c("annlls_d", "mnmp_d", "ftclrk_d")])
etc <- etc %>%
  mutate(
    clmcmpr_z = percent_rank(clmcmpr_z),
    clmcmpr_mm = percent_rank(clmcmpr_mm),
    clmcmpr_d = percent_rank(clmcmpr_d)
  )



# NOW FINAL index scores - HEIRARCHICAL
etc$fr_z_h <- rowSums(etc[, c("trncmpr_z", "hltcmpr_z", "evncmpr_z", "sclcmpr_z", "clmcmpr_z")])
etc$fr_mm_h <- rowSums(etc[, c("trncmpr_mm", "hltcmpr_mm", "evncmpr_mm", "sclcmpr_mm", "clmcmpr_mm")])
etc$fr_d_h <- rowSums(etc[, c("trncmpr_d", "hltcmpr_d", "evncmpr_d", "sclcmpr_d", "clmcmpr_d")])


# ALSO calcuate final scores given a non-hierarchical calc
#separate out transpo variables to DOUBLE them per ETC weighting criteria
trans_insec_vars <- c(
  "pctnvh", "avgcmm", "trnfrq", "jb45dr", "drvpoi", "wlkpoi",
  "avghht", "ftltsp"
)

all_other_vars <- c(
  "ozn", "pm25", "dslpm", "cncrtx",
  "hzrdst", "txcrls", "trtdsp", "rskmns", "clmns", "ldmns",
  "ppr80h", "hghvlr", "rlwys", "arprts", "prts", "imprdw",
  "asthm", "cncr", "bldprs", "dbts", "mntlhl", "ppvrty",
  "pndplm", "pnmply", "phstnr", "phbrd7", "pnnsrd", "pnntrn",
  "endnql", "p65ldr", "p17yng", "pdsb", "plmng", "pmblhm",
  "annlls", "extht", "extrmp", "drghtd", "pctnnd", "mnmp"
)


# DOUBLE the transpo insecurity variable score, add all other values normally
etc$fr_z_nh <- rowSums(etc[, c(paste0(trans_insec_vars, "_z"))]) * 2 +
  rowSums(etc[, c(paste0(all_other_vars, "_z"))])

etc$fr_mm_nh <- rowSums(etc[, c(paste0(trans_insec_vars, "_mm"))]) * 2 +
  rowSums(etc[, c(paste0(all_other_vars, "_mm"))])

etc$fr_d_nh <- rowSums(etc[, c(paste0(trans_insec_vars, "_d"))]) * 2 +
  rowSums(etc[, c(paste0(all_other_vars, "_d"))])



# calculate binary disadvantaged/not variable
# specify all suffixes of the 3X2 types of scores calculated
suffixes <- c("_z_h", "_mm_h", "_d_h", "_z_nh", "_mm_nh", "_d_nh")

# Loop through the suffixes and create binary output variables
for (suffix in suffixes) {
  etc[[paste0("frp", suffix)]] <- percent_rank(
    etc[[paste0("fr", suffix)]])
  etc[[paste0("fri", suffix)]] <- ifelse(
    etc[[paste0("frp", suffix)]] < 0.65, 0, 1)
}


#write out full calc set
write.csv(etc, file.path(data.out, 'etc_allcalcs'), row.names = FALSE)


# subset to just final scores
etc_out <- etc[, !grepl('trn', names(etc)) & grepl('fr', names(etc))] #have to specify the 'trn' drop to remove transit frequency (trnfr) variables


# add GEOID variable on
etc_out <- cbind(etc[c(1)], etc_out)

rm(etc_all_vars, trans_insec_vars, all_other_vars, suffix, suffixes, 
   etc)  #clean-up



# calculate measures of disadvantage variability

# calculate range of percentile rank values
etc_out <- etc_out %>%
  rowwise() %>%
  mutate(frp_range = 
              max(c_across(starts_with("frp"))) - min(c_across(starts_with("frp")))
  ) %>%
  ungroup()


# sum of instances of disadvantage assignment
# create vectors of iterations to be considered
suffixes <- c("_z_h", "_mm_h", "_d_h", "_z_nh", "_mm_nh", "_d_nh")

#sum into a single count
etc_out$fri_counts <- rowSums(etc_out[paste0("fri", suffixes)])


#write it out
write.csv(etc_out, file.path(data.out, 'etc_scores'), row.names = FALSE)


#***********************************************************************************************************************

##----EHD CALCS------------------------------------------------------------------------------------------------------------

#read data
ehd <- read_xlsx(here(file.path(data.in, 'from_Joey/ehd_data_v3.xlsx')), 
                 sheet = "Measure",
                 col_types=c('numeric', 'numeric', 'numeric',
                             'guess', 'guess', 'guess',
                             'guess', 'guess', 'guess', 
                             'guess')) %>% as.data.table

ehd_vars <- read_xlsx(here(file.path(data.in, 'from_Joey/ehd_data_v3.xlsx')),
                      sheet = "Dictionary",
                      col_types=c('guess', 'guess', 'guess', 'guess', 'guess',
                                  'guess', 'guess', 'guess')) %>% as.data.table


# Join the abbreviated variable names from the Dictionary sheet and rename to GEOID for consistency
ehd <- ehd %>%
  left_join(select(ehd_vars, FFC, var), by = c("ItemName" = "FFC")) %>%
  rename(GEOID = GeoCode)

# Pivot the data from long to wide format
ehd <- ehd %>%
  pivot_wider(
    id_cols = GEOID,            # Unique identifier
    names_from = var,           # Column to spread into new columns
    values_from = RankCalculatedValue  # Values to fill the new columns
  )


# Create a list of vectors with variable names for each type of scaling
var_list <- list(
  envex = c("dslpm_wa", "ozn_wa", "pm25_wa", "txcrls_rsei", "hghvlr_wa"),
  enveff = c("ppr80h", "prox_hzw-tsdfs", "prox_npl", "prox_rskmns", "wwdis"),
  senspop = c("cardiod", "lowbw"),
  soecon = c("pndplm", "phbrd", "plmng", "ppvrty_185", "poc", "trnexp", "pnmply")
)

# Define the scaling types
scaling_types <- c("_z", "_mm", "_d")

# Iterate through the list of variable names and scaling types
for (vars in var_list) {
  for (var in vars) {
    for (type in scaling_types) {
      col_name <- paste0(var, type)
      
      if (type == "_z") {
        ehd[[col_name]] <- scale(ehd[[var]], center = TRUE, scale = TRUE)
        ehd[[col_name]] <- pmax(0, ehd[[col_name]])  # Ensure _z values are positive
      } else if (type == "_mm") {
        ehd[[col_name]] <- (ehd[[var]] - min(ehd[[var]], na.rm = TRUE)) /
          (max(ehd[[var]], na.rm = TRUE) - min(ehd[[var]], na.rm = TRUE))
      } else if (type == "_d") {
        ehd[[col_name]] <- as.numeric(decile_scale(ehd[[var]]))
      }
    }
  }
}


# CALCULATE the different versions of the indices

# first calculate average values for each variable scaling type
avg_calcs <- list()

for (var_group in names(var_list)) {
  for (version in scaling_types) {
    # Calculate the sum for the current version
    avg_calcs[[paste0("avg_", var_group, version)]] <- 
      rowMeans(ehd[, paste0(var_list[[var_group]], version)], na.rm = TRUE)
  }
}

avg_calcs[is.na(avg_calcs)] <- 0

ehd <- cbind(ehd, avg_calcs)


# use EHD equation to calculate deciles of scores i.e. Hierarchical scores by scaling type
for (version in scaling_types) {
  # Calculate the rank for the current version hierarchy
  ehd[[paste0("fs", version, "_h")]] <-
    ((ehd[[paste0("avg_", 'envex', version)]] + 
        0.5*ehd[[paste0("avg_", 'enveff', version)]])/2) *
    ((ehd[[paste0("avg_", 'senspop', version)]] + 
        ehd[[paste0("avg_", 'soecon', version)]])/2)
  # now calculate Non-Hierarchical scores by scaling type, all decile ranked
    # NOTE: retain weighting scheme of 0.5 for enviro effects
  ehd[[paste0("fs", version, "_nh")]] <- 
    rowSums(ehd[, paste0(var_list$envex, version)], na.rm = T) +
      0.5 * rowSums(ehd[, paste0(var_list$enveff, version)], na.rm = T) +
      rowSums(ehd[, paste0(var_list$senspop, version)], na.rm = T) +
      rowSums(ehd[, paste0(var_list$soecon, version)], na.rm = T)
}


# calculate binary disadvantaged/not variable according to two thresholds:
#   9s & 10s most stringent (original top 20%)
#   7s - 10s more recent, less stringent funding threshold (top 40%)

# specify all suffixes of the 3X2 types of scores calculated
suffixes <- c("_z_h", "_mm_h", "_d_h", "_z_nh", "_mm_nh", "_d_nh")

# Loop through the suffixes and create binary output variables
for (suffix in suffixes) {
  ehd[[paste0("fsp", suffix)]]<- percent_rank(
    ehd[[paste0("fs", suffix)]])
  ehd[[paste0("fsi9", suffix)]] <- ifelse(
    ehd[[paste0("fsp", suffix)]] < 0.8, 0, 1)
  ehd[[paste0("fsi7", suffix)]] <- ifelse(
    ehd[[paste0("fsp", suffix)]] < 0.6, 0, 1)
}


#write out full calc set
write.csv(ehd, file.path(data.out, 'ehd_allcalcs'), row.names = FALSE)


# subset to just final scores
ehd_out <- ehd[, !grepl('prox', names(ehd)) & grepl('fs', names(ehd))] #have to specify the 'prox' drop to remove hazardous waste and treatment disposal facilities (prox_...tsdfs) variables

# add GEOID variable on
ehd_out <- cbind(ehd[c(1)], ehd_out)

rm(avg_calcs, scaling_types, suffix, suffixes, var_group, version,
   ehd, ehd_vars, var_list, col_name, type, var, vars)  #clean-up



# calculate measures of disadvantage variability

# calculate range of percentile rank values
ehd_out <- ehd_out %>%
  rowwise() %>%
  mutate(frp_range = 
           max(c_across(starts_with("fsp"))) - min(c_across(starts_with("fsp")))
  ) %>%
  ungroup()


# sum of instances of disadvantage assignment
# create vectors of iterations to be considered
suffixes <- c("_z_h", "_mm_h", "_d_h", "_z_nh", "_mm_nh", "_d_nh")

#sum into a single count
ehd_out$fsi9_counts <- rowSums(ehd_out[paste0("fsi9", suffixes)])
ehd_out$fsi7_counts <- rowSums(ehd_out[paste0("fsi7", suffixes)])


#write it out
write.csv(ehd_out, file.path(data.out, 'ehd_scores'), row.names = FALSE)


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
  tracts_calcs$is_urban <- ifelse(tracts_calcs$overlap_ratio >= 0.5, 1, 0)
  
  # join the new variable to the base df
  df <- left_join(df, select(tracts_calcs, GEOID, is_urban))
  
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
etc_out <- etc_out %>%
  left_join(select(tracts_20, GEOID, is_urban))

# write out shps with added scoring data
write_sf(ehd_out, file.path(data.out, 'ehd_scores.shp'))
write_sf(etc_out, file.path(data.out, 'etc_scores.shp'))

#***********************************************************************************************************************



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


#***********************************************************************************************************************
