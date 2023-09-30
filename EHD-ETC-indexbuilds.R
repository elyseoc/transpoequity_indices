# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: E.O. Lewis
# Date: 09/23/2023
# Purpose: POST EHD-ETC-indexbuilds - explore and analyze sensitivity re: urban/rural correlations
# PRE-code: "/PhD Work/WSDOT Equity/Analysis/transpoequity_indices/EHD-ETC-indexbuilds.R"
# source("/PhD Work/WSDOT Equity/Analysis/transpoequity_indices/EHD-ETC-indextests.R", echo=T)
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
pacman::p_load(readxl, here, data.table, dplyr, 
               sf, tidycensus)

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


##----ETC CALCS------------------------------------------------------------------------------------------------------------
etc <- read_sf(here(file.path(
  data.in, 'USDOT_ETC/DOT_Index_State_5_3'),'DOT_Index_State_5_3.shp')) %>%
  subset(stbbr=='WA')

#rename tracts FIPS code column name to GEOID to match other dfs
colnames(etc)[colnames(etc) == "trctfp"] <- "GEOID"
#remove extra first 9 characters
etc <- etc %>% mutate('GEOID'=substring(GEOID, 10))


# Select the variables for which you want to calculate z-scores
etc_calcs_base <- etc[, c(
  "pctnvh", "avgcmm", "trnfrq", "jb45dr", "drvpoi",
  "wlkpoi", "avghht", "ftltsp", "ozn", "pm25",
  "dslpm", "cncrtx", "hzrdst", "txcrls", "trtdsp",
  "rskmns", "clmns", "ldmns", "ppr80h", "hghvlr",
  "rlwys", "arprts", "prts", "imprdw", "asthm",
  "cncr", "bldprs", "dbts", "mntlhl", "ppvrty",
  "pndplm", "pnmply", "phstnr", "phbrd7", "pnnsrd",
  "pnntrn", "endnql", "p65ldr", "p17yng", "pdsb",
  "plmng", "pmblhm", "annlls", "extht", "extrmp",
  "drghtd", "pctnnd", "mnmp"
)]
etc_calcs_base <- st_drop_geometry(etc_calcs_base) #drop geometry to allow for numerical calcs
#reverse the distributions for "Frequency of Transit Services per Sq Mi" and "Jobs within a 45-min Drive" so they add appropriately to the disadvantage estimation
etc$trnfrq <- max(etc$trnfrq) - etc$trnfrq + min(etc$trnfrq)
etc$jb45dr <- max(etc$jb45dr) - etc$jb45dr + min(etc$jb45dr)


# Calculate z-scores for the selected variables
z_scores <- scale(etc_calcs_base)


# Shift all z-scores to be positive
z_scores[is.na(z_scores)] <- 0  #NOTE: there are no lead mines in WA state, which yield "NA" values
min_z_score <- min(z_scores)
shifted_z_scores <- z_scores - min_z_score

# Add the shifted z-scored variables to the 'etc' data frame
colnames(shifted_z_scores) <- paste0(colnames(etc_calcs_base), "_z")
etc_calcs <- cbind(etc$GEOID, etc_calcs_base, shifted_z_scores)

# generate min-max scaled variables

min_max_scaled_vars <- as.data.frame(apply(etc_calcs_base, 2, min_max_scale))
min_max_scaled_vars[is.na(min_max_scaled_vars)] <- 0 # again, no lead mines, need 0 coerced

# Add the min-max scaled variables to the 'etc' data frame
colnames(min_max_scaled_vars) <- paste0(colnames(etc_calcs_base), "_mm")
etc_calcs <- cbind(etc_calcs, min_max_scaled_vars)


# Perform decile scaling for the selected variables
decile_scaled_vars <- as.data.frame(apply(etc_calcs_base, 2, decile_scale))

# Add the decile scaled variables to the 'etc' data frame
colnames(decile_scaled_vars) <- paste0(colnames(etc_calcs_base), "_d")
etc_calcs <- cbind(etc_calcs, decile_scaled_vars)


#clean-up
rm(decile_scaled_vars, etc_calcs_base, min_max_scaled_vars, shifted_z_scores, 
   z_scores, min_z_score)
gc()


# calculate revised indices

# Create a new variable for the subindex of transportation access for each standardization type
# then scale each accordingly
etc_calcs$trnsac_z <- rowSums(etc_calcs[, c(
  "pctnvh_z", "avgcmm_z", "trnfrq_z", "jb45dr_z", "drvpoi_z", "wlkpoi_z"
)])
etc_calcs$trnsac_z <- scale(etc_calcs$trnsac_z, center = TRUE, scale = TRUE)

etc_calcs$trnsac_mm <- rowSums(etc_calcs[, c(
  "pctnvh_mm", "avgcmm_mm", "trnfrq_mm", "jb45dr_mm", "drvpoi_mm", "wlkpoi_mm"
)])
etc_calcs$trnsac_mm <- min_max_scale(etc_calcs$trnsac_mm)

etc_calcs$trnsac_d <- rowSums(etc_calcs[, c(
  "pctnvh_d", "avgcmm_d", "trnfrq_d", "jb45dr_d", "drvpoi_d", "wlkpoi_d"
)])
etc_calcs$trnsac_d <- decile_scale(etc_calcs$trnsac_d)


# do the same for the Future Climate Risk variable
etc_calcs$ftclrk_z <- rowSums(etc_calcs[, c(
  "extht_z", "extrmp_z", "drghtd_z", "pctnnd_z"
)])
etc_calcs$ftclrk_z <- scale(etc_calcs$ftclrk_z, center = TRUE, scale = TRUE)

etc_calcs$ftclrk_mm <- rowSums(etc_calcs[, c(
  "extht_mm", "extrmp_mm", "drghtd_mm", "pctnnd_mm"
)])
etc_calcs$ftclrk_mm <- min_max_scale(etc_calcs$ftclrk_mm)

etc_calcs$ftclrk_d <- rowSums(etc_calcs[, c(
  "extht_d", "extrmp_d", "drghtd_d", "pctnnd_d"
)])
etc_calcs$ftclrk_d <- decile_scale(etc_calcs$ftclrk_d)


#calculate each component index percentile rank score

# transportation insecurity
etc_calcs$trncmpr_z <- rowSums(etc_calcs[, c("avghht_z", "ftltsp_z", "trnsac_z")])
etc_calcs$trncmpr_mm <- rowSums(etc_calcs[, c("avghht_mm", "ftltsp_mm", "trnsac_mm")])
etc_calcs$trncmpr_d <- rowSums(etc_calcs[, c("avghht_d", "ftltsp_d", "trnsac_d")])

# Compute the percentile rank of the sums for each re-scaling technique
#DOUBLE THEM HERE for ease later in code
etc_calcs <- etc_calcs %>%
  mutate(
    trncmpr_z = percent_rank(trncmpr_z)*2,
    trncmpr_mm = percent_rank(trncmpr_mm)*2,
    trncmpr_d = percent_rank(trncmpr_d)*2
  )

# environmental burden
etc_calcs$evncmpr_z <- rowSums(etc_calcs[, c(
  "ozn_z", "pm25_z", "dslpm_z", "cncrtx_z", "hzrdst_z",
  "txcrls_z", "trtdsp_z", "rskmns_z", "clmns_z", "ldmns_z",
  "ppr80h_z", "hghvlr_z", "rlwys_z", "arprts_z", "prts_z", "imprdw_z"
)])
etc_calcs$evncmpr_mm <- rowSums(etc_calcs[, c(
  "ozn_mm", "pm25_mm", "dslpm_mm", "cncrtx_mm", "hzrdst_mm",
  "txcrls_mm", "trtdsp_mm", "rskmns_mm", "clmns_mm", "ldmns_mm",
  "ppr80h_mm", "hghvlr_mm", "rlwys_mm", "arprts_mm", "prts_mm", "imprdw_mm"
)])
etc_calcs$evncmpr_d <- rowSums(etc_calcs[, c(
  "ozn_d", "pm25_d", "dslpm_d", "cncrtx_d", "hzrdst_d",
  "txcrls_d", "trtdsp_d", "rskmns_d", "clmns_d", "ldmns_d",
  "ppr80h_d", "hghvlr_d", "rlwys_d", "arprts_d", "prts_d", "imprdw_d"
)])
etc_calcs <- etc_calcs %>%
  mutate(
    evncmpr_z = percent_rank(evncmpr_z),
    evncmpr_mm = percent_rank(evncmpr_mm),
    evncmpr_d = percent_rank(evncmpr_d)
  )


# health vulnerability
etc_calcs$hltcmpr_z <- rowSums(etc_calcs[, c(
  "asthm_z", "cncr_z", "bldprs_z", "dbts_z", "mntlhl_z"
)])
etc_calcs$hltcmpr_mm <- rowSums(etc_calcs[, c(
  "asthm_mm", "cncr_mm", "bldprs_mm", "dbts_mm", "mntlhl_mm"
)])
etc_calcs$hltcmpr_d <- rowSums(etc_calcs[, c(
  "asthm_d", "cncr_d", "bldprs_d", "dbts_d", "mntlhl_d"
)])
etc_calcs <- etc_calcs %>%
  mutate(
    hltcmpr_z = percent_rank(hltcmpr_z),
    hltcmpr_mm = percent_rank(hltcmpr_mm),
    hltcmpr_d = percent_rank(hltcmpr_d)
  )


# social vulnerability
etc_calcs$sclcmpr_z <- rowSums(etc_calcs[, c(
  "ppvrty_z", "pndplm_z", "pnmply_z", "phstnr_z", "phbrd7_z",
  "pnnsrd_z", "pnntrn_z", "endnql_z", "p65ldr_z", "p17yng_z",
  "pdsb_z", "plmng_z", "pmblhm_z"
)])
etc_calcs$sclcmpr_mm <- rowSums(etc_calcs[, c(
  "ppvrty_mm", "pndplm_mm", "pnmply_mm", "phstnr_mm", "phbrd7_mm",
  "pnnsrd_mm", "pnntrn_mm", "endnql_mm", "p65ldr_mm", "p17yng_mm",
  "pdsb_mm", "plmng_mm", "pmblhm_mm"
)])
etc_calcs$sclcmpr_d <- rowSums(etc_calcs[, c(
  "ppvrty_d", "pndplm_d", "pnmply_d", "phstnr_d", "phbrd7_d",
  "pnnsrd_d", "pnntrn_d", "endnql_d", "p65ldr_d", "p17yng_d",
  "pdsb_d", "plmng_d", "pmblhm_d"
)])
etc_calcs <- etc_calcs %>%
  mutate(
    sclcmpr_z = percent_rank(sclcmpr_z),
    sclcmpr_mm = percent_rank(sclcmpr_mm),
    sclcmpr_d = percent_rank(sclcmpr_d)
  )


#finally, climate & disaster risk burden
etc_calcs$clmcmpr_z <- rowSums(etc_calcs[, c("annlls_z", "mnmp_z", "ftclrk_z")])
etc_calcs$clmcmpr_mm <- rowSums(etc_calcs[, c("annlls_mm", "mnmp_mm", "ftclrk_mm")])
etc_calcs$clmcmpr_d <- rowSums(etc_calcs[, c("annlls_d", "mnmp_d", "ftclrk_d")])
etc_calcs <- etc_calcs %>%
  mutate(
    clmcmpr_z = percent_rank(clmcmpr_z),
    clmcmpr_mm = percent_rank(clmcmpr_mm),
    clmcmpr_d = percent_rank(clmcmpr_d)
  )



# NOW FINAL index scores - HEIRARCHICAL
etc_calcs$fnlrnk_z_h <- rowSums(etc_calcs[, c("trncmpr_z", "hltcmpr_z", "evncmpr_z", "sclcmpr_z", "clmcmpr_z")])
etc_calcs$fnlrnk_mm_h <- rowSums(etc_calcs[, c("trncmpr_mm", "hltcmpr_mm", "evncmpr_mm", "sclcmpr_mm", "clmcmpr_mm")])
etc_calcs$fnlrnk_d_h <- rowSums(etc_calcs[, c("trncmpr_d", "hltcmpr_d", "evncmpr_d", "sclcmpr_d", "clmcmpr_d")])

# Compute the percentile rank of the mean percentile ranks for each grouping
etc_calcs <- etc_calcs %>%
  mutate(
    fnlrnk_z_h = percent_rank(fnlrnk_z_h),
    fnlrnk_mm_h = percent_rank(fnlrnk_mm_h),
    fnlrnk_d_h = percent_rank(fnlrnk_d_h)
  )


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
etc_calcs$fnlrnk_z_nh <- rowSums(etc_calcs[, c(paste0(trans_insec_vars, "_z"))]) * 2 +
  rowSums(etc_calcs[, c(paste0(all_other_vars, "_z"))])

etc_calcs$fnlrnk_mm_nh <- rowSums(etc_calcs[, c(paste0(trans_insec_vars, "_mm"))]) * 2 +
  rowSums(etc_calcs[, c(paste0(all_other_vars, "_mm"))])

etc_calcs$fnlrnk_d_nh <- rowSums(etc_calcs[, c(paste0(trans_insec_vars, "_d"))]) * 2 +
  rowSums(etc_calcs[, c(paste0(all_other_vars, "_d"))])

# calculate percentile rank
etc_calcs <- etc_calcs %>%
  mutate(
    fnlrnk_z_nh = percent_rank(fnlrnk_z_nh),
    fnlrnk_mm_nh = percent_rank(fnlrnk_mm_nh),
    fnlrnk_d_nh = percent_rank(fnlrnk_d_nh)
  )

rm(trans_insec_vars, all_other_vars)


# calculate binary disadvantaged/not variable
# specify all suffixes of the 3X2 types of scores calculated
suffixes <- c("_z_h", "_mm_h", "_d_h", "_z_nh", "_mm_nh", "_d_nh")

# Loop through the suffixes and create binary output variables
for (suffix in suffixes) {
  input_var <- paste0("fnlrnk", suffix)
  output_var <- paste0("fnlrnki", suffix)
  etc_calcs[[output_var]] <- ifelse(etc_calcs[[input_var]] < 0.65, 0, 1)
}

#***********************************************************************************************************************

##----EHD CALCS------------------------------------------------------------------------------------------------------------




