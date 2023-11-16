# ----HEADER------------------------------------------------------------------------------------------------------------
# Author: E.O. Lewis
# Date: 09/23/2023
# Purpose: POST EHD-ETC-indexbuilds - explore and analyze sensitivity re: urban/rural correlations
# PRE-code: "/PhD Work/WSDOT Equity/Analysis/transpoequity_indices/EHD-ETC-indexbuilds.R"
#         AND "/PhD Work/WSDOT Equity/Analysis/transpoequity_indices/EHD-ETC-correlationtests.R"
# source("/PhD Work/WSDOT Equity/Analysis/transpoequity_indices/EHD-ETC-indextests.R", echo=T)
#
# NOTES:
# partially derivative of code developed by j.f. found at https://github.com/jfrostad/ehd_mapsense/tree/main/code
# forest plot code derivative of code developed by K. Hoffman found at https://www.khstats.com/blog/forest-plots/ 
# color palette info:  https://search.r-project.org/CRAN/refmans/viridisLite/html/viridis.html 
#                      https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
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
               tigris, tidycensus, ggcorrplot, forcats, #tmap,tmaptools, ggspatial,  NOT used, but can handle interactive
               broom.mixed, ggstance, jtools, factoextra, scam,
               epitools, 
               stargazer, zoo)

#***********************************************************************************************************************


##---PLOT VARIABLE DISTRIBUTIONS-----------------------------------------------------------------------

#read in data
ehd_all <- read.csv(file.path(data.out, 'ehd_allcalcs.csv'))
etc_all <- read.csv(file.path(data.out, 'etc_allcalcs.csv'))










#***********************************************************************************************************************


##---PLOT COUNTS TYPE DISADVANTAGE-----------------------------------------------------------------------

#read in data
both_dis <- read_sf(here(data.out, 'both_disadvantage.shp'))


#set up the color palette to match the maps
counts_pal <- c('#A83800', '#477998', '#C490D1', '#FFD37F')
#define labels used in all
legend_lab <- 'Identified as\nDisadvantaged\nin any Calculation\nIteration'
x_lab <- 'Census Tract Type'
y_lab <- "Count"

#both ALL
df <- both_dis
x <- df$is_urban %>% as.factor()
fill <- df$dis %>% as.factor()
plot <- 
  ggplot(df, aes(x=x, fill=fill)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(legend_lab, values=counts_pal) +
  scale_x_discrete(name = x_lab,
                   labels=c("0" = "Rural", "1" = "Urban")) +
  scale_y_continuous(name = y_lab, limits = c(0, 650)) + 
  theme_minimal()


#both z-score
df <- both_dis
x <- df$is_urban %>% as.factor()
fill <- df$dis_z %>% as.factor()
plot <- 
  ggplot(df, aes(x=x, fill=fill)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(legend_lab, values=counts_pal) +
  scale_x_discrete(name = x_lab,
                   labels=c("0" = "Rural", "1" = "Urban")) +
  scale_y_continuous(name = y_lab, limits = c(0, 650)) + 
  theme_minimal()


# run calcs for ALL
calcs <- table(df$is_urban,df$dis)

#calc % total tracts ruled out by all methods considered
sum(calcs[c('0','1'),'Neither'])/sum(calcs)
# now % rural tracts ruled out
calcs['0','Neither']/sum(calcs['0',])
# and % urban tracts ruled out
calcs['1','Neither']/sum(calcs['1',])


# run calcs for z-score
calcs <- table(df$is_urban,df$dis_z)

#calc % total tracts ruled out by all methods considered
sum(calcs[c('0','1'),'Neither'])/sum(calcs)
# now % rural tracts ruled out
calcs['0','Neither']/sum(calcs['0',])
# and % urban tracts ruled out
calcs['1','Neither']/sum(calcs['1',])


#***********************************************************************************************************************


##---PLOT VARIABLE CORRELTATIONS-----------------------------------------------------------------------

#read in data
both_cor <- read.csv(file.path(data.out, 'popdense-correlations_both.csv')) %>%
  mutate(
    index = case_when(
      index == 'ehd' ~ 'EHD',
      index == 'etc' ~ 'ETC'
    )
  )

#read in variable details & definitions
both_details <- read.csv(file.path(data.out, 'variable-details_both.csv'))


#add details to correlation results
both_cor <- left_join(both_cor, both_details)


both_cor <- both_cor %>%
  mutate(
    in_both_indices_text = case_when(
      in_both_indices == 1 ~ 'same',
      in_both_indices == 99 ~ 'similar',
      in_both_indices == 0 ~ 'different'
    ),
    full_info = 
      paste(FFC, in_both_indices_text, sep = ', '),
    estimate = case_when(
      high.likelihood.cor == 0 ~ 0,
      TRUE ~ pears.cor
    ),
    conf.low = case_when(
      high.likelihood.cor == 0 ~ 0,
      TRUE ~ pears.conf.low
    ),
    conf.up = case_when(
      high.likelihood.cor == 0 ~ 0,
      TRUE ~ pears.conf.up
    ),
    UR_cor = case_when(
      estimate == 0 ~ 'none',
      estimate > 0 ~ 'urban',
      estimate < 0 ~ 'rural'
    )
  )

# #filter for EHD focused comparisons
# ehd.sim_cor <- both_cor %>%
#   filter(index == "ehd" | in_both_indices == 1 | in_both_indices == 99)


# set up variables used in plot specification either where values are used more than once or to store text in vectors rather than in the plot specification itself
df <- both_cor[complete.cases(both_cor[, "in_both_indices"]), ]
HEAL.concept <- as.factor(df$HEAL_concept) # NOTE: the variable name is used as the legend header
Index <- df$index
x <- df$estimate
Variable <- as.factor(df$FFC)
xl <- df$conf.low
xu <- df$conf.up
shape <- c(2,4)
yint <- seq(5, 50, by = 5)

# generate forestplot of variable correlation results
p <- 
  df %>%
  ggplot(aes(y = Variable)) + 
  geom_point(aes(x = x, color = HEAL.concept, shape = Index), size=3) +
  geom_linerange(aes(xmin=xl, xmax=xu, color = HEAL.concept), linewidth=1) +
  scale_shape_manual(values = shape) + 
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_hline(yintercept = yint, color = "lightgray", linewidth=0.01) +
  theme_classic()
p


#generate bar plots of variable counts per index by correlation outcome
plots <- list()
for (index_value in unique(Index)) {
  data_subset <- subset(df, Index == index_value)
  
  # Correlation <- data_subset$UR_cor
  # HEAL.concept <- data_subset$HEAL_concept
  # 
  # data_subset <- as.data.frame(table(Correlation,HEAL.concept))
  
  p <- ggplot(data_subset, aes(x = UR_cor, fill = HEAL_concept)) +
    geom_bar() +
    labs(x = "") +
    ylim(0, 25) + 
    theme_minimal()
  
  plots[[index_value]] <- p
}

# Print the two plots
grid.arrange(grobs = plots)




##---PLOT ODDS RATIO RESULTS-----------------------------------------------------------------------

# read in data
both_OR <- read.csv(file.path(data.out, 'oddsratios_both.csv')) 

both_OR <- both_OR %>%
  mutate(
    rescale = case_when(
      is.na(rescale) & index == 'EHD' ~ 'deciles',
      is.na(rescale) & index == 'ETC' ~ 'min-max',
      TRUE ~ rescale
      ),
    hier = case_when(
      is.na(hier) ~ 'hier.',
      TRUE ~ hier
    ),
    agg = case_when(
      is.na(agg) & index == 'EHD' ~ 'mult.',
      is.na(agg) & index == 'ETC' ~ 'sum',
      TRUE ~ agg
    ),
    hier = paste(hier, agg, sep = ', ')
  )

#pull colors for plot pallete
colors1 <- brewer.pal(4,'Set1')
colors2 <- brewer.pal(3,'Set2')
colors <- c(colors2[1],colors1[4],colors2[2])


# set up variables used in plot specification either where values are used more than once or to store text in vectors rather than in the plot specification itself
df <- both_OR[both_OR$index == 'EHD', ]
explain <- "Urban tracts ____ more likely to be classified as disadvantaged"
x_labs <- c("10x", "20x", "30x", "40x", "50x", "likelihood (95% CI)", 'p-value')
x_cuts <- c(10,20,30,40,50)
pal <- colors
shp  <- c(7,0,13,1)
x_max <- 55
x_min <- -18
x_mid <- x_max/2
est_ci <- x_max + 5
pval <- x_max + 15
name <- -0.5
y_labs <- df$version
y_max <- nrow(df) + 3

# generate forestplot of odds ratio results
p <- 
  df %>%
  ggplot(aes(y = fct_rev(version))) + 
  #geom_point(aes(x=estimate, color = rescale), size=3) +
  geom_point(aes(x=estimate, color = rescale, shape=hier), size=3) +
  geom_linerange(aes(xmin=conf.low, xmax=conf.up, color = rescale)) +
  scale_color_manual(values = pal) +
  scale_shape_manual(values = shp) + 
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_vline(xintercept = x_cuts, color="grey") +
  annotate("rect", xmin = 1, xmax = x_max,
           ymin = y_max-2.5, ymax = y_max,
           fill = 'white') +
  annotate('text', x = c(x_cuts,est_ci,pval), y = y_max-2, 
           label = x_labs, vjust = 0) +
  annotate('text', x = x_mid, y = y_max-1, label = explain, fontface = 'bold', 
           vjust = 0) +
  geom_text(
    aes(x = name, y = version, label = y_labs), hjust = 1
  ) +
  geom_text(
    aes(x = est_ci, y = version, label = estimate_lab)#, hjust = 0
  ) +
  geom_text(
    aes(x = pval, y = version, label = p.value), #hjust = 0
  ) +
  coord_cartesian(xlim = c(x_min, pval+2)) +
  # annotate("rect", xmin = x_min-4, xmax = pval+3,
  #          ymin = 0, ymax = 6.5,
  #          alpha = 0.1) +
  theme_void() 
p


# re-run for ETC
df <- both_OR[both_OR$index == 'ETC', ]



#***********************************************************************************************************************


##---PLOT VARIATION RANGES & COUNTS-----------------------------------------------------------------------

#read in data
ehd_ur <- read.csv(file.path(data.out, 'ehd_scores.csv'))
etc_ur <- read.csv(file.path(data.out, 'etc_scores.csv'))

#GAVE UP on map using ggplots - probably isn't that hard, but was just easier to tinker in ArcGIS :(

#prep dfs for figures
ehd_ur <- na.omit(ehd_ur)
etc_ur <- na.omit(etc_ur) 

ehd_ur <- ehd_ur %>%
  mutate(
    fri9_cnt_factors = 
      as.factor(case_when(
        fri9_counts == 0 ~ ' 0',
        fri9_counts > 0 & fri9_counts <= 5 ~ ' 1 - 5',
        fri9_counts > 5 & fri9_counts <= 8 ~ ' 6 - 8',
        fri9_counts > 8 & fri9_counts <= 12 ~ ' 9 - 12',
        fri9_counts == 13 ~ '13'
      )),
    fri7_cnt_factors = 
      as.factor(case_when(
        fri7_counts == 0 ~ ' 0',
        fri7_counts > 0 & fri7_counts <= 5 ~ ' 1 - 5',
        fri7_counts > 5 & fri7_counts <= 8 ~ ' 6 - 8',
        fri7_counts > 8 & fri7_counts <= 12 ~ ' 9 - 12',
        fri7_counts == 13 ~ '13'
      ))
  )

etc_ur <- etc_ur %>%
  mutate(
    fri_counts_factors = 
      as.factor(case_when(
        fri_counts == 0 ~ ' 0',
        fri_counts > 0 & fri_counts <= 5 ~ ' 1 - 5',
        fri_counts > 5 & fri_counts <= 8 ~ ' 6 - 8',
        fri_counts > 8 & fri_counts <= 12 ~ ' 9 - 12',
        fri_counts == 13 ~ '13'
      ))
  )



#plot rank range vs. baseline by counts of disadvantage ---------

#set up the color palette to match the maps
counts_pal <- viridis::inferno(10)[c(10,8,6,4,1)]
#define labels used in all
text <- "Disadvantage\nThreshold"
legend_lab <- '# of times\nidentified as\nDisadvantaged'
y_lab <- 'Rank Variation'

#ehd
df <- ehd_ur
x <- df$frp_base
y <- df$frp_range
fill <- df$fri9_cnt_factors
x_line <- 0.8
y_max <- max(y)
x_lab <- 'Baseline EHD Ranking'
plot <-
  ggplot(df, aes(x=x, y=y, fill=fill)) +
  geom_hex(aes(alpha=log(..count..)), bins=50, position = position_jitter(width = 0.03)) +
  geom_vline(xintercept=x_line, linetype='dashed', color='grey') +
  # annotate("rect", xmin = x_line-0.1, xmax = x_line+0.1, 
  #          ymin = y_max-0.04, ymax = y_max+0.04,
  #          fill = 'white') +
  # annotate("text", x = x_line, y = y_max, label = text, hjust = 0.5) +
  #doesn't change:
  scale_fill_manual(legend_lab, values=counts_pal) +
  scale_x_continuous(x_lab) +
  scale_y_continuous(y_lab) +
  scale_alpha_continuous(guide='none', range=c(.5,1)) +
  theme_minimal()
  
file.path(plots.out, 'ehd_9_rank_variation_hex.png') %>% ggsave(height=6, width=8)

df <- ehd_ur
x <- df$frp_base
y <- df$frp_range
fill <- df$fri7_cnt_factors
x_line <- 0.6
y_max <- max(y)
x_lab <- 'Baseline EHD Ranking'
plot <-
  ggplot(df, aes(x=x, y=y, fill=fill)) +
  geom_hex(aes(alpha=log(..count..)), bins=50, position = position_jitter(width = 0.03)) +
  geom_vline(xintercept=x_line, linetype='dashed', color='grey') +
  # annotate("rect", xmin = x_line-0.1, xmax = x_line+0.1, 
  #          ymin = y_max-0.04, ymax = y_max+0.04,
  #          fill = 'white') +
  # annotate("text", x = x_line, y = y_max, label = text, hjust = 0.5) +
  #doesn't change:
  scale_fill_manual(legend_lab, values=counts_pal) +
  scale_x_continuous(x_lab) +
  scale_y_continuous(y_lab) +
  scale_alpha_continuous(guide='none', range=c(.5,1)) +
  theme_minimal()

file.path(plots.out, 'ehd_7_rank_variation_hex.png') %>% ggsave(height=6, width=8)


#etc

#set up the color palette to match the maps
counts_pal <- viridis::inferno(10)[c(10,8,6,4,1)]
#define labels used in all
text <- "Disadvantage\nThreshold"
legend_lab <- '# of times\nidentified as\nDisadvantaged'
y_lab <- 'Rank Variation'

df <- etc_ur
x <- df$frp_base
y <- df$frp_range
fill <- df$fri_counts_factors
x_line <- 0.65
y_max <- max(y)
x_lab <- 'Baseline ETC Ranking'
plot <-
  ggplot(df, aes(x=x, y=y, fill=fill)) +
  geom_hex(aes(alpha=log(..count..)), bins=50) +
  geom_vline(xintercept=x_line, linetype='dashed', color='grey') +
  # annotate("rect", xmin = x_line-0.1, xmax = x_line+0.1, 
  #          ymin = y_max-0.04, ymax = y_max+0.04,
  #          fill = 'white') +
  # annotate("text", x = x_line, y = y_max, label = text, hjust = 0.5) +
  #doesn't change:
  scale_fill_manual(legend_lab, values=counts_pal) +
  scale_x_continuous(x_lab) +
  scale_y_continuous(y_lab) +
  scale_alpha_continuous(guide='none', range=c(.5,1)) +
  theme_minimal()

file.path(plots.out, 'etc_rank_variation_hex.png') %>% ggsave(height=6, width=8)


#plot barplots of urban/rural splits by counts of disadvantage ------------------------

#set up the color palette to match the maps
counts_pal <- viridis::inferno(10)[c(10,8,6,4,1)]
#define labels used in all
text <- "Disadvantage\nThreshold"
legend_lab <- '# of times\nidentified as\nDisadvantaged'
x_lab <- 'Census Tract Type'
y_lab <- "Count"

#ehd
df <- ehd_ur
x <- df$is_urban %>% as.factor()
fill <- df$fri9_cnt_factors
plot <- 
  ggplot(df, aes(x=x, fill=fill)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(legend_lab, values=counts_pal) +
  scale_x_discrete(name = x_lab,
                   labels=c("0" = "Rural", "1" = "Urban")) +
  scale_y_continuous(y_lab, limits = c(0, 600)) +
  theme_minimal()

file.path(plots.out, 'ehd_9_urban-rural_disadvantage_count_hex.png') %>% ggsave(height=3, width=8)

df <- ehd_ur
x <- df$is_urban %>% as.factor()
fill <- df$fri7_cnt_factors
plot <- 
  ggplot(df, aes(x=x, fill=fill)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(legend_lab, values=counts_pal) +
  scale_x_discrete(name = x_lab,
                   labels=c("0" = "Rural", "1" = "Urban")) +
  scale_y_continuous(y_lab, limits = c(0, 600)) +
  theme_minimal()

file.path(plots.out, 'ehd_7_urban-rural_disadvantage_count_hex.png') %>% ggsave(height=3, width=8)

#etc

#set up the color palette to match the maps
counts_pal <- viridis::inferno(10)[c(10,8,6,4,1)]
#define labels used in all
text <- "Disadvantage\nThreshold"
legend_lab <- '# of times\nidentified as\nDisadvantaged'
x_lab <- 'Census Tract Type'
y_lab <- "Count"

df <- etc_ur
x <- df$is_urban %>% as.factor()
fill <- df$fri_counts_factors
plot <- 
  ggplot(df, aes(x=x, fill=fill)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(legend_lab, values=counts_pal) +
  scale_x_discrete(name = x_lab,
                   labels=c("0" = "Rural", "1" = "Urban")) +
  scale_y_continuous(y_lab, limits = c(0, 600)) +
  theme_minimal()

file.path(plots.out, 'etc_urban-rural_disadvantage_count_hex.png') %>% ggsave(height=3, width=8)

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

#useful tidbits
brewer.pal(5, "YlOrRd")
#https://r-graph-gallery.com/38-rcolorbrewers-palettes.html


# my scraps

tm_shape(na.omit(ehd_ur)) +
  tm_borders() +  # Add borders for better visualization
  tm_fill("frp_rng", style = "cat", title = '',
          breaks = breaks,
          palette = rng_palette, labels = as.character(breaks)) +
  tm_compass(type = "arrow", position = c("right", "top"), size = 1) +  # Add a north arrow
  tm_layout(frame = FALSE, 
            inner.margins = c(0.05, 0.05, 0.05, 0.05),
            legend.title.size = 1.5,
            scale = 1.5)

tm_shape(na.omit(ehd_ur)) +
  tm_borders() +
  tm_fill("frp_rng", style = "pretty", title = 'Frp Range',
          n = 5, palette = rng_palette) +
  tm_compass(type = "arrow", position = c("right", "top"), size = 1) +
  tm_scale_bar(position = c("left", "bottom")) +  # Add a scale bar
  tm_layout(frame = FALSE, 
            inner.margins = c(0.05, 0.05, 0.05, 0.05),
            legend.title.size = 1.5,
            scale = 1.5,
            legend.frame = TRUE)



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
