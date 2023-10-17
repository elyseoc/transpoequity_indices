# initial work w/ community consult data

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
pacman::p_load(readxl, here, data.table, dplyr, tidyr, car,
               ggtern, ggplot2, ggpubr, ggridges, ggrepel, ggdist, grid, gridExtra, RColorBrewer, #viz pkgs
               viridis, farver, reldist, ggnewscale, ggallin, biscale, cowplot)

#***********************************************************************************************************************



# read in response data
#NOTE: data prepped in excel prior to reading in
commcon <- read_xlsx(
  here(file.path(data.in, 'survey_responses/comm_consult/Equity in Planning Survey.xlsx')), 
                 sheet = "Responses") %>% as.data.table

commcon_vars <- read_xlsx(
  here(file.path(data.in, 'survey_responses/comm_consult/Equity in Planning Survey.xlsx')), 
  sheet = "Dictionary") %>% as.data.table

colnames(commcon) <- commcon_vars$Q_number


# race/ethnicity variables

#overarching POC
#subset down to just the race/ethnicity variables
commcon_num <- commcon %>%
  select(matches("38|39"))

#coerce 1s to all responses and 99s to everything else to make it numerical and MANAGEABLE
commcon_num <- commcon_num %>%
  mutate_all(~ifelse(!is.na(.), 1, 99)) %>%
  cbind(commcon[c(1)])

commcon_num <- commcon_num %>%
  rowwise() %>%
  mutate(
    race_eth = if (Q38_f == 1 | Q39_z == 1) {   #NOTE: CURRENTLY 39_z is "Spain" - but CHECK future
      0
    } else if (
      Q38_a == 1 | Q38_b == 1 | Q38_c == 1 | Q38_d == 1 | Q38_e == 1 | 
      Q38_g == 1 | Q38_z == 1 | Q39_b == 1 | Q39_c == 1 | Q39_d == 1   #NOTE: CURRENTLY 38_z is latine/mixed race - but CHECK future
    ) {
      1
    } else if (Q38_h == 1) {    #set "Prefer not to answer" to 999 - currently none exist
      999
    } else {
      NA
    }
  ) %>%
  mutate(
    tribes = if (Q38_d == 1) {    #create variable for just Native American or Alaska Native
    1
  } else if (!is.na(race_eth)) {    #set all other race/ethnicities to 0
    0
  } else {
    NA
  }
  ) %>%
  ungroup() %>%
  select(!matches("38|39"))

                  ## race_eth
                      # 1 == any POC
                      # 0 == white
                      # 999 == Prefer not to answer
                  ## tribes
                      # 1 == Native American or Alaska Native
                      # 0 == any other response


# gender
commcon <- commcon %>% 
  rowwise() %>%
  mutate(
    gender = ifelse(
      any(Q42 == "Non-binary", Q42 == "Other (please specify)", 
          Q42 == "Transgender"), 3,
      ifelse(Q42 == "Male", 2,
      ifelse(Q42 == "Female", 1, NA
    )))
  ) %>%
  ungroup()
                      # Female == 1
                      # Male == 2
                      # Gender different from sex assigned at birth == 3

# disability
# by type
commcon <- commcon %>% 
  rowwise() %>%
  mutate(
    disability_type = 
      ifelse(Q43 == "Yes", 1, 
             ifelse(Q44 == "Yes", 2, 
                    ifelse(Q45 == "Yes", 3,
                           ifelse(Q46 == "Yes", 4, 
      ifelse(Q43 == "No" & Q44 == "No" & Q45 == "No" & Q46 == "No", 0, NA
             )))))
  ) %>%
  ungroup() %>% 
  mutate(
    disability_bi = 
      ifelse(disability_type >= 1 & disability_type <= 4, 1, 
             ifelse(disability_type == 0, 0, NA
             )),
    disability_lims =
      ifelse(Q47 == "Yes", 1, 
             ifelse(Q47 == "No", 0, NA
             ))
  )
                  ## disability_type
                      # 1 == deaf/difficulty hearing
                      # 2 == blind/difficulty seeing
                      # 3 == cognitive difficulty
                      # 4 == mobility difficulty (walking or climbing stairs)
                      # 0 == no disability reported

                  ## disability_bi
                      # 1 == any disability reported
                      # 0 == no disability reported

                  ## disability_lims
                      # 1 == disability makes it difficult to do errands alone
                      # 0 == no disability-related difficulty to do errands alone





#***********************************************************************************************************************

#SCRAP ----------

library(car)
D$q1.num <- recode(D$Q1, "'Strongly Agree'=5; 'Agree'=4; 'Neutral'=3;
                           'Disagree'=2; 'Strongly Disagree'=1")

