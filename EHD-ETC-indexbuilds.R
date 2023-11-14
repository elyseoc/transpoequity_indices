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

z_scale_and_shift <- function(x) {
  z_scaled <- scale(x)                 # Z-scale the input vector
  shifted <- z_scaled - min(z_scaled, na.rm = TRUE)  # Shift all values to be positive
  return(shifted)
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
  dplyr::rename(
    GEOID = trctfp,           #rename variables to match existing naming conventions
    frp_base = fnlrnkp,
    fri_base = fnlrnki
  ) %>%
  mutate(
    frp_base = frp_base/100   #ETC reports in % not decimal - convert to decimal
  )
etc_base <- st_drop_geometry(etc[, c("GEOID", "frp_base", "fri_base")]) %>%
  mutate(GEOID = as.numeric(GEOID))


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

#**reverse the distributions for "Frequency of Transit Services per Sq Mi" and "Jobs within a 45-min Drive" so they add appropriately to the disadvantage estimation
etc$trnfrq <- max(etc$trnfrq) - etc$trnfrq + min(etc$trnfrq)
etc$jb45dr <- max(etc$jb45dr) - etc$jb45dr + min(etc$jb45dr)


# Calculate z-scores for the selected variables
z_scores <- z_scale_and_shift(etc[, etc_all_vars])

# Add the shifted z-scored variables to the 'etc' data frame
colnames(z_scores) <- paste0(colnames(z_scores), "_z")
etc <- cbind(etc[c(1)], etc[, etc_all_vars], z_scores)


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
rm(decile_scaled_vars, min_max_scaled_vars, z_scores)
gc()


# calculate revised indices

# Create a new variable for the subindex of transportation access for each standardization type
# then scale each accordingly
etc$trnsac_z <- rowSums(etc[, c(
  "pctnvh_z", "avgcmm_z", "trnfrq_z", "jb45dr_z", "drvpoi_z", "wlkpoi_z"
)])
etc$trnsac_z <- z_scale_and_shift(etc$trnsac_z)

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
etc$ftclrk_z <- z_scale_and_shift(etc$ftclrk_z)

etc$ftclrk_mm <- rowSums(etc[, c(
  "extht_mm", "extrmp_mm", "drghtd_mm", "pctnnd_mm"
)])
etc$ftclrk_mm <- min_max_scale(etc$ftclrk_mm)

etc$ftclrk_d <- rowSums(etc[, c(
  "extht_d", "extrmp_d", "drghtd_d", "pctnnd_d"
)])
etc$ftclrk_d <- decile_scale(etc$ftclrk_d)


# generate HIERARCHICAL index values ******************************************

#calculate each component index scores & rescaling rank values

# Create a list of vectors with variable names for each type of scaling
var_list <- list(
  trncm = c("avghht", "ftltsp", "trnsac"),                             #transporation insecurity vars
  evncm = c("ozn", "pm25", "dslpm", "cncrtx", "hzrdst", "txcrls",      #enviro burden vars
            "trtdsp", "rskmns", "clmns", "ppr80h",                  # "ldmns", REMOVE lead mines b/c they are all N/A b/c they do not exist in WA state
            "hghvlr", "rlwys", "arprts", "prts", "imprdw"),
  hltcm = c("asthm", "cncr", "bldprs", "dbts", "mntlhl"),              #health sensitivity vars
  sclcm = c("ppvrty", "pndplm", "pnmply", "phstnr", "phbrd7",          #socioecon vuln. vars
            "pnnsrd", "pnntrn", "endnql", "p65ldr", "p17yng",
            "pdsb", "plmng", "pmblhm"),
  clmcm = c("annlls", "mnmp", "ftclrk")                                #climate change risk vars
)

# Define the scaling types
scaling_types <- c("_z", "_mm", "_d")

# Define the version of values combined
hier <- '.rnk'

# Iterate through the list of categorical sets of variables and scaling types
for (type in scaling_types) {
  for (vars_name in names(var_list)) {
    vars <- var_list[[vars_name]]
    
    # Print the current vector name
    #print(paste("Current vector name:", vars_name))       #used to troubleshoot
    #print(paste("Current scaling type:", type))           #used to troubleshoot
    
    vars <- paste(vars, type, sep = "")
    
    s <- rowSums(etc[, vars])               #sum to a score for each category
    
    col_name <- paste0(vars_name, hier, type, sep = "")
    
    if (type == "_z") {                                         #calculate ranks for each category
      etc[[col_name]] <- z_scale_and_shift(s)
    } else if (type == "_mm") {
      etc[[col_name]] <- min_max_scale(s)
    } else if (type == "_d") {
      etc[[col_name]] <- decile_scale(s)
    }   

  }
}

# now a run to calc final scores
hier_vars <- names(etc[,grepl(paste0(hier, '_', sep = ''), names(etc))])
hnh <- '_h'

threat <- c("evncm", "clmcm")
vuln <- c("trncm", "hltcm",  "sclcm")

for (type in scaling_types) {
  
  col_name <- paste0("trncm", hier, type, sep = "")
  etc[[col_name]] <- 2*etc[[col_name]]               #double the transpo variable
  
  col_name <- paste0("frp", type, hnh, ".ss", sep = "") #re-set col_name variable to final score type: Hier., Simple Sum
  
  vars_f <- hier_vars[grepl(type, hier_vars)]
  
  etc[[col_name]] <- percent_rank(rowSums(etc[, vars_f]))   #FINAL percentile-ranked value 
  
  
  t <- paste(threat, hier, type, sep = "")
  v <- paste(vuln, hier, type, sep = "")
  
  col_name <- paste0("frp", type, hnh, ".m", sep = "") #re-set col_name variable to final score type: Hier., Multiplicative
  
  etc[[col_name]] <- percent_rank(rowSums(etc[,t])*rowSums(etc[,v]))    #FINAL percentile-ranked  value 
  
}



# generate NON-HIERARCHICAL index values **************************

#separate out transpo & climate change variables to over-write transpo and climate change 
var_list$trncm <- c(
  "pctnvh", "avgcmm", "trnfrq", "jb45dr", "drvpoi", "wlkpoi",   #transporation insecurity vars NON-hierarchical
  "avghht", "ftltsp"
  )
var_list$clmcm <- c(
  "annlls", "extht", "extrmp", "drghtd", "pctnnd", "mnmp"       #climate change risk vars NON-hierarchical
  )


# Define the scaling types
scaling_types <- c("_z", "_mm", "_d")

# Define the version of values combined
hier <- '.scr'

# Iterate through the list of categorical sets of variables and scaling types
for (type in scaling_types) {
  for (vars_name in names(var_list)) {
    vars <- var_list[[vars_name]]
    
    # Print the current vector name
    #print(paste("Current vector name:", vars_name))       #used to troubleshoot
    
    col_name <- paste0(vars_name, hier, type, sep = "")
    
    #print(paste("Current scaling type:", type))           #used to troubleshoot
    
    vars <- paste(vars, type, sep = "")
    
    etc[[col_name]] <- rowSums(etc[, vars])               #sum to a score for each category
    
  }
}

# now a run to calc final scores
hier_vars <- names(etc[,grepl(paste0(hier, '_', sep = ''), names(etc))])
hnh <- '_nh'

threat <- c("evncm", "clmcm")
vuln <- c("trncm", "hltcm",  "sclcm")

for (type in scaling_types) {
  
  col_name <- paste0("trncm", hier, type, sep = "")
  etc[[col_name]] <- 2*etc[[col_name]]               #double the transpo variable
  
  col_name <- paste0("frp", type, hnh, ".ss", sep = "") #re-set col_name variable to final score type: Hier., Simple Sum
  
  vars_f <- hier_vars[grepl(type, hier_vars)]
  
  etc[[col_name]] <- percent_rank(rowSums(etc[, vars_f]))   #FINAL percentile-ranked value 
  
  
  t <- paste(threat, hier, type, sep = "")
  v <- paste(vuln, hier, type, sep = "")
  
  col_name <- paste0("frp", type, hnh, ".m", sep = "") #re-set col_name variable to final score type: Hier., Multiplicative
  
  etc[[col_name]] <- percent_rank(rowSums(etc[,t])*rowSums(etc[,v]))    #FINAL percentile-ranked  value 
  
}



# calculate binary disadvantaged/not variable
# specify all suffixes of the 3X4 types of scores calculated
suffixes <- c("_z_h.ss", "_mm_h.ss", "_d_h.ss", 
              "_z_h.m", "_mm_h.m", "_d_h.m", 
              "_z_nh.ss", "_mm_nh.ss", "_d_nh.ss",
              "_z_nh.m", "_mm_nh.m", "_d_nh.m")

# Loop through the suffixes and create binary output variables
for (suffix in suffixes) {
  etc[[paste0("fri", suffix)]] <- ifelse(
    etc[[paste0("frp", suffix)]] < 0.65, 0, 1)
}


#write out full calc set
write.csv(etc, file.path(data.out, 'etc_allcalcs.csv'), row.names = FALSE)


# subset to just final scores
etc_out <- etc[, !grepl('trn', names(etc)) & grepl('fr', names(etc))] #have to specify the 'trn' drop to remove transit frequency (trnfr) variables


# add GEOID variable on
etc_out <- cbind(etc[c(1)], etc_out) %>%
  mutate(GEOID = as.numeric(GEOID))

rm(etc_all_vars, var_list, col_name, hier, hier_vars, hnh, s, t, v, threat, vuln, 
   suffix, suffixes, scaling_types, type, vars, vars_f, vars_name, etc)           #clean-up


# calculate measures of disadvantage variability *************************************

#join base ranks to full set of scores
etc_out <- left_join(etc_out, etc_base)


# calculate range of percentile rank values
etc_out <- etc_out %>%
  rowwise() %>%
  mutate(frp_range = 
              max(c_across(starts_with("frp"))) - min(c_across(starts_with("frp")))
  ) %>%
  ungroup()


# sum of instances of disadvantage assignment
# create vectors of iterations to be considered - 3x4 + _base
suffixes <- c("_z_h.ss", "_mm_h.ss", "_d_h.ss", 
              "_z_h.m", "_mm_h.m", "_d_h.m", 
              "_z_nh.ss", "_mm_nh.ss", "_d_nh.ss",
              "_z_nh.m", "_mm_nh.m", "_d_nh.m",
              "_base")

#sum into a single count
etc_out$fri_counts <- rowSums(etc_out[paste0("fri", suffixes)])



#write it out
write.csv(etc_out, file.path(data.out, 'etc_scores.csv'), row.names = FALSE)


rm(etc_base, etc_out)    #clean-up


#***********************************************************************************************************************

##----EHD CALCS------------------------------------------------------------------------------------------------------------

#read data
ehd_base <- read_sf(here(file.path(
  data.in, 'WA_State_All/WA_DOH/EHD_Current'),'EHD.shp')) %>%
  dplyr::rename(
    GEOID = Census_Tra,           #rename variables to match existing naming conventions
    frp_base = EHD_Rank
  )
ehd_base <- st_drop_geometry(ehd_base[, c("GEOID", "frp_base")]) %>%
  mutate(GEOID = as.numeric(GEOID))

ehd_base <- st_drop_geometry(ehd_base[])

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

rm(ehd_vars)   #clean-up



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
        ehd[[col_name]] <- z_scale_and_shift(ehd[[var]])
      } else if (type == "_mm") {
        ehd[[col_name]] <- min_max_scale(ehd[[var]])
      } else if (type == "_d") {
        ehd[[col_name]] <- as.numeric(decile_scale(ehd[[var]]))
      }
    }
  }
}


# generate HIERARCHICAL index values ******************************************

# Define the version of values combined
hier <- '.rnk'

# Iterate through the list of categorical sets of variables and scaling types
for (type in scaling_types) {
  for (vars_name in names(var_list)) {
    vars <- var_list[[vars_name]]
    
    # Print the current vector name
    #print(paste("Current vector name:", vars_name))       #used to troubleshoot
    #print(paste("Current scaling type:", type))           #used to troubleshoot
    
    vars <- paste(vars, type, sep = "")
    
    s <- rowSums(ehd[, vars], na.rm = TRUE)               #sum to a score for each category
    
    col_name <- paste0(vars_name, hier, type, sep = "")
    
    if (type == "_z") {                                         #calculate ranks for each category
      ehd[[col_name]] <- z_scale_and_shift(s)
    } else if (type == "_mm") {
      ehd[[col_name]] <- min_max_scale(s)
    } else if (type == "_d") {
      ehd[[col_name]] <- decile_scale(s)
    }   
    
  }
}

# now a run to calc final scores
hier_vars <- names(ehd[,grepl(paste0(hier, '_', sep = ''), names(ehd))])
hnh <- '_h'

#define threats & vulnerabilities
threat <- c("envex", "enveff")
vuln <- c("senspop", "soecon")

for (type in scaling_types) {
  
  col_name <- paste0("enveff", hier, type, sep = "")
  ehd[[col_name]] <- 0.5*ehd[[col_name]]               #1/2 the Enviro. Effects variable
  
  col_name <- paste0("frp", type, hnh, ".ss", sep = "") #re-set col_name variable to final score type: Hier., Simple Sum
  
  vars_f <- hier_vars[grepl(type, hier_vars)]
  
  ehd[[col_name]] <- percent_rank(rowSums(ehd[, vars_f], na.rm = TRUE))   #FINAL percentile-ranked value 
  
  
  t <- paste(threat, hier, type, sep = "")
  v <- paste(vuln, hier, type, sep = "")
  
  col_name <- paste0("frp", type, hnh, ".m", sep = "") #re-set col_name variable to final score type: Hier., Multiplicative
  
  ehd[[col_name]] <- percent_rank(rowSums(ehd[,t])*rowSums(ehd[,v]))    #FINAL percentile-ranked  value 
  
}


# generate NON-HIERARCHICAL index values **************************

# Define the version of values combined
hier <- '.scr'

# Iterate through the list of categorical sets of variables and scaling types
for (type in scaling_types) {
  for (vars_name in names(var_list)) {
    vars <- var_list[[vars_name]]
    
    # Print the current vector name
    #print(paste("Current vector name:", vars_name))       #used to troubleshoot
    
    col_name <- paste0(vars_name, hier, type, sep = "")
    
    #print(paste("Current scaling type:", type))           #used to troubleshoot
    
    vars <- paste(vars, type, sep = "")
    
    ehd[[col_name]] <- rowSums(ehd[, vars], na.rm = TRUE)               #sum to a score for each category
    
  }
}

# now a run to calc final scores
#pull summary variables aligned w/ non-hier run
hier_vars <- names(ehd[,grepl(paste0(hier, '_', sep = ''), names(ehd))])
hnh <- '_nh'      #specify variable coding as non-hier

#define threats & vulnerabilities
threat <- c("envex", "enveff")
vuln <- c("senspop", "soecon")


for (type in scaling_types) {
  
  col_name <- paste0("enveff", hier, type, sep = "")
  ehd[[col_name]] <- 0.5*ehd[[col_name]]               #1/2 the Enviro. Effects variable
  
  col_name <- paste0("frp", type, hnh, ".ss", sep = "") #re-set col_name variable to final score type: non-Hier., Simple Sum
  
  vars_f <- hier_vars[grepl(type, hier_vars)]
  
  ehd[[col_name]] <- percent_rank(rowSums(ehd[, vars_f], na.rm = TRUE))   #FINAL percentile-ranked value 
  
  
  t <- paste(threat, hier, type, sep = "")
  v <- paste(vuln, hier, type, sep = "")
  
  col_name <- paste0("frp", type, hnh, ".m", sep = "") #re-set col_name variable to final score type: non-Hier., Multiplicative
  
  ehd[[col_name]] <- percent_rank(rowSums(ehd[,t])*rowSums(ehd[,v]))    #FINAL percentile-ranked  value 
  
}



# calculate binary disadvantaged/not variable **********************************

# specify all suffixes of the 3X4 types of scores calculated
suffixes <- c("_z_h.ss", "_mm_h.ss", "_d_h.ss", 
              "_z_h.m", "_mm_h.m", "_d_h.m", 
              "_z_nh.ss", "_mm_nh.ss", "_d_nh.ss",
              "_z_nh.m", "_mm_nh.m", "_d_nh.m")

# Loop through the suffixes and create binary output variables
for (suffix in suffixes) {
  ehd[[paste0("fri7", suffix)]] <- ifelse(
    ehd[[paste0("frp", suffix)]] < 0.60, 0, 1)
  ehd[[paste0("fri9", suffix)]] <- ifelse(
    ehd[[paste0("frp", suffix)]] < 0.80, 0, 1)
}


#write out full calc set
write.csv(ehd, file.path(data.out, 'ehd_allcalcs.csv'), row.names = FALSE)



# subset to just final scores
ehd_out <- ehd[, !grepl('trn', names(ehd)) & grepl('fr', names(ehd))] #have to specify the 'trn' drop to remove transit expense (trnex) variables


# add GEOID variable on
ehd_out <- cbind(ehd[c(1)], ehd_out)

rm(var_list, col_name, hier, hier_vars, hnh, s, t, v, threat, vuln, 
   suffix, suffixes, scaling_types, type, var, vars, vars_f, vars_name, ehd)           #clean-up


#manage & assign disadvantage to base (v.2.0) EHD rank values

ehd_base$frp_base <- ehd_base$frp_base / 10

ehd_base$fri7_base <- ifelse(
  ehd_base$frp_base < 0.60, 0, 1)
ehd_base$fri9_base <- ifelse(
  ehd_base$frp_base < 0.80, 0, 1)


# calculate measures of disadvantage variability *************************************

#join base ranks to full set of scores
ehd_out <- left_join(ehd_out, ehd_base)


# calculate measures of disadvantage variability

# calculate range of percentile rank values
ehd_out <- ehd_out %>%
  rowwise() %>%
  mutate(frp_range = 
           max(c_across(starts_with("frp"))) - min(c_across(starts_with("frp")))
  ) %>%
  ungroup()


# sum of instances of disadvantage assignment
# specify all suffixes of the 3X4 types of scores calculated + _base
suffixes <- c("_z_h.ss", "_mm_h.ss", "_d_h.ss", 
              "_z_h.m", "_mm_h.m", "_d_h.m", 
              "_z_nh.ss", "_mm_nh.ss", "_d_nh.ss",
              "_z_nh.m", "_mm_nh.m", "_d_nh.m",
              "_base")

#sum into a single count
ehd_out$fri9_counts <- rowSums(ehd_out[paste0("fri9", suffixes)])
ehd_out$fri7_counts <- rowSums(ehd_out[paste0("fri7", suffixes)])


#write it out
write.csv(ehd_out, file.path(data.out, 'ehd_scores.csv'), row.names = FALSE)

rm(ehd_base, ehd_out)    #clean-up


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
ehd_out <- read.csv(file.path(data.out, 'ehd_scores.csv'))
etc_out <- read.csv(file.path(data.out, 'etc_scores.csv'))

#reduce full scoring to just summary values & GEOID
ehd_sums <- ehd_out[, grepl('GEOID', names(ehd_out)) | 
                      grepl('range', names(ehd_out)) | 
                      grepl('counts', names(ehd_out))]
etc_sums <- etc_out[, grepl('GEOID', names(etc_out)) | 
                      grepl('range', names(etc_out)) | 
                      grepl('counts', names(etc_out))]

# join the index scores to the related shp of tracts
ehd_sums <- ehd_sums %>%
  left_join(select(tracts_10, GEOID, is_urban))
etc_sums <- etc_sums %>%
  left_join(select(tracts_20, GEOID, is_urban))

# write out shps with added scoring data
write_sf(ehd_sums, file.path(data.out, 'ehd_sumsUR.shp'))
write_sf(etc_sums, file.path(data.out, 'etc_sumsUR.shp'))


# make a version of the full scores CSV w/ the Urban/Rural designation
ehd_ur <- st_drop_geometry(select(tracts_10, GEOID, is_urban))
etc_ur <- st_drop_geometry(select(tracts_20, GEOID, is_urban))

# join the is_urban variable to the calcs then write it out
ehd_out <- left_join(ehd_out, ehd_ur)
etc_out <- left_join(etc_out, etc_ur)

write.csv(ehd_out, file.path(data.out, 'ehd_scores.csv'), row.names = FALSE)
write.csv(etc_out, file.path(data.out, 'etc_scores.csv'), row.names = FALSE)


#***********************************************************************************************************************


#---FIX integreated into main code BUT ETC disadvantage weirdness---------------------------------------------------------------------

etc_scores <- read.csv(file.path(data.out, 'etc_scores.csv'))

#spot check
geo_checks <- c('53025010100', '53025011300', '53047970800', '53047970700')

geo <- etc_scores$GEOID

etc_check <- etc_scores[, grepl("fri", names(etc_scores))]

etc_check <- cbind(geo, etc_check)

etc_check <- etc_check[etc_check$geo %in% geo_checks, ]
etc_check <- etc_scores[etc_scores$GEOID %in% geo_checks, ]


#huh - there is a discrepancy between published disadvantage online and classifications in the data download

min(etc_scores$frp_base)
max(etc_scores$frp_base)

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
