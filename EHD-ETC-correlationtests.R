# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: E.O. Lewis
# Date: 09/23/2023
# Purpose: explore and analyze correlations btwn ETC & EHD variables and population density
# source("/PhD Work/WSDOT Equity/Analysis/transpoequity_indices/EHD-ETC-correlationtests.R", echo=T)
#
# NOTES:
#
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

##----ADD POPULATION DENSITY------------------------------------------------------------------------------------------------------------

#read in and manage ETC set
etc <- read_sf(here(file.path(
  data.in, 'USDOT_ETC/DOT_Index_State_5_3'),'DOT_Index_State_5_3.shp')) %>%
  subset(stbbr=='WA') %>% 
  #rename tracts FIPS code column name to GEOID to match other dfs
  dplyr::rename(GEOID = trctfp)

vars_keep <- c(
  "GEOID", "totPop", "povper", "hhminc", "estct", "hhnov", 
  "perpov",                                                 #binary but relevant: Areas of Persistent Poverty Indicator
#  "uzalow", "uzamid", "uzahigh", "uaind", "compua",         #binary but NOT relevant: all urban area related
  "wtae", "wtgs", "wtmf", "wtp", 
  "wtps", "dtae", "dtgs", "dtmf", "dtp", "dtps", "pctnvh", "avgcmm", "trnfrq", 
  "jb45dr", "drvpoi", "wlkpoi", "avghht", "ftltsp", "ozn", "pm25", "dslpm", 
  "cncrtx", "hzrdst", "txcrls", "trtdsp", "rskmns", "clmns", "ldmns", "ppr80h", 
  "hghvlr", "rlwys", "arprts", "prts", "imprdw", "asthm", "cncr", "bldprs", 
  "dbts", "mntlhl", "ppvrty", "pndplm", "pnmply", "phstnr", "phbrd7", "pnnsrd", 
  "pnntrn", "endnql", "p65ldr", "p17yng", "pdsb", "plmng", "pmblhm", "annlls", 
  "extht", "extrmp", "drghtd", "pctnnd", "mnmp"
)

etc <- etc %>%
  select(all_of(vars_keep)) %>% 
  mutate(GEOID = as.numeric(GEOID))


#read in and manage EHD set
ehd <- read_xlsx(here(file.path(data.in, 'from_Joey/ehd_data_v3.xlsx')), 
                 sheet = "Measure",
                 col_types=c('numeric', 'numeric', 'numeric',
                             'guess', 'guess', 'guess',
                             'guess', 'guess', 'guess', 
                             'guess')) %>% as.data.table

ehd_vars <- read_xlsx(here(file.path(data.in, 'from_Joey/ehd_data_v3.xlsx')),
                      sheet = "Dictionary",
                      col_types=c('guess', 'guess', 'guess', 'guess', 'guess',
                                  'guess')) %>% as.data.table

# Join the abbreviated variable names from the Dictionary sheet and rename to GEOID for consistency
ehd <- ehd %>%
  left_join(select(ehd_vars, FFC, FA), by = c("ItemName" = "FFC")) %>%
  rename(GEOID = GeoCode)

# Pivot the data from long to wide format
ehd <- ehd %>%
  pivot_wider(
    id_cols = GEOID,            # Unique identifier
    names_from = FA,           # Column to spread into new columns
    values_from = RankCalculatedValue  # Values to fill the new columns
  )

#add geometry to ehd
tracts_10 <- tracts('WA', year=2010, cb=T) %>% 
  st_transform(32148) %>%
  rename(AFFGEOID = GEO_ID) %>%   #note that the variable GEO_ID in 2010 tracts has this AFFGEOID equivalent in 2020 - set to this for consistency
  mutate(GEOID = as.numeric(paste0(STATE, COUNTY, TRACT)))   #set variable as.numeric for later joining


ehd <- left_join(select(tracts_10, GEOID), ehd)


#read in population data sets
etc_pop <- read_sf(here(file.path(
  data.in, 'USDOT_ETC/DOT_Index_State_5_3'),'DOT_Index_State_5_3.shp')) %>%
  subset(stbbr=='WA') %>% 
  #rename tracts FIPS code column name to GEOID to match other dfs
  dplyr::rename(GEOID = trctfp) %>% 
  mutate(GEOID = as.numeric(GEOID)) %>% 
  select(GEOID, totPop) %>% 
  st_drop_geometry()

ehd_pop <- read.csv(file.path(data.in, 'WA_State_All/WTN_All/Community_Population_2010-tracts/Population_-_Census_Tract_2019.csv')) %>%
  dplyr::rename(GEOID = Census.Tract, totPop = Population) %>% 
  mutate(GEOID = as.numeric(GEOID)) %>% 
  select(GEOID, totPop)


#join population data to ehd and etc
etc <- left_join(etc, etc_pop)
ehd <- left_join(ehd, ehd_pop)


rm(ehd_vars, vars_keep, tracts_10, ehd_pop, etc_pop)   #clean-up


#add population density to both dfs
df_names <- c("ehd", "etc")
df_list <- list(ehd, etc)

for (i in seq_along(df_list)) {
  df <- df_list[[i]]
  
  df <- df %>%
    mutate(
      a_tract = st_area(df),
      pop_dens = totPop/a_tract
    )
  
  # Assign the modified data frame back to the list
  df_list[[i]] <- df
  
  assign(df_names[i], df_list[[i]])
}

rm(df, df_names, df_list, i)    #clean-up

#***********************************************************************************************************************

##---CORRELATION TESTS--------------------------------------------------------------------------------------------------

#write a list of variables by index
var_tests <- list()

var_tests$etc <-  c(
  "povper", "hhminc", "estct", "hhnov", "wtae", "wtgs", "wtmf", "wtp", 
  "wtps", "dtae", "dtgs", "dtmf", "dtp", "dtps", "pctnvh", "avgcmm", "trnfrq", 
  "jb45dr", "drvpoi", "wlkpoi", "avghht", "ftltsp", "ozn", "pm25", "dslpm", 
  "cncrtx", "hzrdst", "txcrls", "trtdsp", "rskmns", "clmns", 
  #"ldmns",     #there are no lead mines in WA so REMOVE
  "ppr80h", 
  "hghvlr", "rlwys", "arprts", "prts", "imprdw", "asthm", "cncr", "bldprs", 
  "dbts", "mntlhl", "ppvrty", "pndplm", "pnmply", "phstnr", "phbrd7", "pnnsrd", 
  "pnntrn", "endnql", "p65ldr", "p17yng", "pdsb", "plmng", "pmblhm", "annlls", 
  "extht", "extrmp", "drghtd", "pctnnd", "mnmp"
)

var_tests$ehd <- c(
  "dslpm_wa", "ozn_wa", "pm25_wa", "txcrls_rsei", "hghvlr_wa", "ppr80h",
  "prox_hzw-tsdfs", "prox_npl", "prox_rskmns", "wwdis", "cardiod", "lowbw",
  "pndplm", "phbrd", "plmng", "ppvrty_185", "poc", "trnexp", "pnmply"
)

# create a df of key outputs for analysis
both_cor <- data.frame(
  var = character(),     # Create empty columns with the desired names
  index = character(), 
  pears.cor = numeric(),
  pears.pval = numeric(),
  pears.conf.low = numeric(),
  pears.conf.up = numeric(),
  spear.rho = numeric(),
  spear.pval = numeric()
)

# create a list and names vector of dfs for analysis
df_list <- list(etc, ehd)
df_names <- c('etc', 'ehd')


# loop through correlation calculations and save outputs to df
for (i in seq_along(df_list)) {
  for (vars in var_tests[[i]]) {
    df <- df_list[[i]]
    
    x <- df[[vars]]
    x_jit <- x + runif(length(x)) * 1e-6
    y <- unclass(df$pop_dens)
    y_jit <- y + runif(length(y)) * 1e-6
    
    p.test <- cor.test(x, y, method = "pearson")
    s.test <- cor.test(x_jit, y_jit, method = "spearman")
    
    row <- data.frame(
      var = vars, 
      index = df_names[i],
      pears.cor = unname(p.test$estimate),
      pears.pval = p.test$p.value,
      pears.conf.low = p.test$conf.int[1],
      pears.conf.up = p.test$conf.int[2],
      spear.rho = unname(s.test$estimate),
      spear.pval = s.test$p.value
    )
    
    both_cor <- rbind(both_cor, row)
  }  
}

rm(df, df_list, df_names, p.test, s.test, vars, var_tests, row,    #clean-up
   i, x, y, x_jit, y_jit)


#add more legible values to df
both_cor <- both_cor %>%
  mutate(
    conf.int.crosses0 =
      ifelse(pears.conf.low < 0 & pears.conf.up > 0, 1, 0),
    p.value.p = case_when(
      pears.pval < .001 ~ "<0.001",
      round(pears.pval, 2) == .05 ~ as.character(round(pears.pval,3)),
      pears.pval < .01 ~ # if less than .01, go one more decimal place
        as.character(round(pears.pval, 3)),
      TRUE ~ # otherwise just round to 2 decimal places 
        as.character(round(pears.pval, 2)) 
    ),
    p.value.s = case_when(
      spear.pval < .001 ~ "<0.001",
      round(spear.pval, 2) == .05 ~ as.character(round(spear.pval,3)),
      spear.pval < .01 ~ # if less than .01, go one more decimal place
        as.character(round(spear.pval, 3)),
      TRUE ~ # otherwise just round to 2 decimal places 
        as.character(round(spear.pval, 2))
    ),
    p.mag.p = case_when(
      pears.pval < .001 ~ "***",
      pears.pval < .01 ~ "**",
      pears.pval < .05 ~ "*",
      pears.pval < .1 ~ "-",
      TRUE ~ ""
    ),
    p.mag.s = case_when(
      spear.pval < .001 ~ "***",
      spear.pval < .01 ~ "**",
      spear.pval < .05 ~ "*",
      spear.pval < .1 ~ "-",
      TRUE ~ ""
    ),
    p.mag.same =
      ifelse(p.mag.p == p.mag.s, 1, 0),
    direction.pears = 
      ifelse(pears.cor < 0, "negative", "positive"),
    direction.spear = 
      ifelse(spear.rho < 0, "negative", "positive"),
    direction.same =
      ifelse(direction.pears == direction.spear, 1, 0),
    across(
      c(pears.cor, pears.conf.low, pears.conf.up, spear.rho),
      ~ round(as.numeric(.x), 2)
    )
  )


#bring in variable names, index-spec. categories, & common/my categories
ehd_vars <- read_xlsx(here(file.path(data.in, 'from_Joey/ehd_data_v3.xlsx')),
                      sheet = "Dictionary",
                      col_types=c('guess', 'guess', 'guess', 'guess', 'guess',
                                  'guess', 'guess', 'guess')) %>% as.data.table

etc_vars <- read_xlsx(here(file.path(data.in, 'USDOT_ETC/DataDictionary.xlsx')),
                      sheet = "Dictionary",
                      col_types=c('guess', 'guess', 'guess', 'guess', 'guess',
                                  'guess', 'guess', 'guess')) %>% 
  as.data.table


# separate out by index type to prep for join
ehd_cor <- both_cor %>%
  filter(index == 'ehd')
etc_cor <- both_cor %>%
  filter(index == 'etc')

# join additional variable info by index
ehd_cor <- left_join(ehd_cor, ehd_vars)
etc_cor <- left_join(etc_cor, etc_vars)

#re-bind and overwrite original correlation df w/ the added variable info
both_cor <- rbind(ehd_cor, etc_cor)

rm(ehd_vars, ehd_cor, etc_vars, etc_cor)    #clean-up

#separate out variables that yield similar results in both tests vs. variables that need a closer look
# variables confidently correlated
both_cor <- both_cor %>%
  mutate(
    high.likelihood.cor = 
      ifelse(
        conf.int.crosses0 == 0 & direction.same == 1 & p.mag.same == 1, 1, 0
      )
    )

#write it out
write.csv(both_cor, file.path(data.out, 'popdense-correlations_both'), row.names = FALSE)

# take a look
table(both_cor[both_cor$high.likelihood.cor == 1, ]$direction.pears, 
      both_cor[both_cor$high.likelihood.cor == 1, ]$index)

#          ehd etc
# negative   2  19
# positive  12  16      as expected BUT importantly: 14/19 EHD variables are significantly correlated !!

# NOTE: looking at both_cor$high.likelihood.cor == 0, it seems these values 
#       are simply too close to 0
#       i.e. do NOT demonstrate statistically significant correlation
#            NOR do they havesignificant magnitudes to impact index outcomes


#***********************************************************************************************************************

##---DATA MISSINGNESS-------------------------------------------------------------------------------------------------------------
#use Margin of Error (MOE) reported by USCB for variables considered in the CDC/ATSDR's SVI to assess data missingness






##---SCRAP -------------------------------------------------------------------------------------------------------------


#***********************************************************************************************************************

