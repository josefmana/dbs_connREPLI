# This is a script used to analyze the data for the replication study of presumed predictive value of specific
# fMRI connectivity patterns of stimulated parts of STN in STN DBS in PD for motor, cognitive and affective outcomes.

# The goals of this script are:
# (i) calculate bootstrapped Spearman's correlation of R-map predictions and outcomes in the very same way as it was calculated
#     in the original studies

# list packages to use
pkgs <- c( "rstudioapi", # setting working directory via RStudio API
           "tidyverse", "dplyr", # data wrangling
           "ggplot2", "GGally", #"gridExtra", # plotting
           "rcompanion" # calculate Spearman Rho
           )

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# set working directory (works in RStudio only)
setwd( dirname(getSourceEditorContext()$path) )

# create folders "mods", "figs", "tabs" and "sess" to store results and sessions info in
# prints TRUE and creates the folder if it was not present, prints NULL if the folder was already present.
sapply( c("mods", "figs", "tabs", "sess"), function(i) if( !dir.exists(i) ) dir.create(i) )

# read the data
d.outs <- read.table( "data/preds/dbs_connREPLI_observed_outcomes.csv", sep = ",", header = T )
d.prds <- read.table( "data/preds/Predictions_1.csv", sep = ",", header = T )


# ---- data wrangling ----

# keep only patients with VATs and correlations to normative templates calculated (i.e., included in d.prds)
d0 <- d.outs %>% slice( which( id %in% unique(d.prds$id) ) ) %>%
  # pivot to a wide format
  pivot_wider( id_cols = id, names_from = ass, values_from = c(drs,bdi,mds_updrs_iii) )
  
# calculate gain and percentage change scores
for ( i in c("drs","bdi","mds_updrs_iii") ){
  d0[[ paste0(i,"_gain") ]] <- d0[[ paste0(i,"_r1") ]] - d0[[ paste0(i,"_pre") ]]
  d0[[ paste0(i,"_perc") ]] <- ( d0[[ paste0(i,"_gain") ]] / d0[[ paste0(i,"_pre") ]] ) %>% ifelse( . == Inf, NA, . ) # when pre was 0, the percentage goes to Infinity, change these cases to NA
}

# left join the predictions
d0 <- d0 %>% left_join(
  
  # need to re-code the predictions data frame first
  d.prds %>%
    mutate( template = case_when( map == "Cog_Dec_Reich" ~ "drs",
                                  map == "Depression_Siddiqi" ~ "bdi",
                                  map == "PD_Horn_orig" ~ "mds_updrs_iii" )
            ) %>%
    # pivot to a wider format next
    pivot_wider( id_cols = id, names_from = template, values_from = contains("R") ),
  
  # join by id
  by = "id"
    
)


# ---- Spearman's Rho replication ----

# prepare a table
#t1 <- data.frame( outcome = c("drs","bdi","mds_updrs_iii"), N = NA, gain = NA, perc = NA )

# fill-in number of observations
#for ( i in t1$outcome ) t1[ t1$outcome == i, "N" ] <- complete.cases( d0[[paste0(i,"_gain")]] ) %>% sum()

# loop through all outcomes and calculate Spearman' Rhos with bootstapped 95% CIs
#for ( i in t1$outcome ) {
#  for ( j in c("gain","perc") ) {
#    t1[ t1$outcome==i, j ] <- paste0(
#      spearmanRho( as.formula( paste0(i,"_",j,"~R_",i) ), data = d0[ complete.cases( d0[[paste(i,j,sep="_")]] ), ], method = "spearman", ci = T )$rho %>% round(3) %>% sprintf("%.3f",.), " [",
#      spearmanRho( as.formula( paste0(i,"_",j,"~R_",i) ), data = d0[ complete.cases( d0[[paste(i,j,sep="_")]] ), ], method = "spearman", ci = T )$lower.ci %>% round(3) %>% sprintf("%.3f",.), ", ",
#      spearmanRho( as.formula( paste0(i,"_",j,"~R_",i) ), data = d0[ complete.cases( d0[[paste(i,j,sep="_")]] ), ], method = "spearman", ci = T )$upper.ci %>% round(3) %>% sprintf("%.3f",.), "]"
#    )
#  }
#}

# print the table to jpeg
#tableGrob(
#  t1 %>% mutate( outcome = c("DRS-2","BDI-II","MDS-UPDRS III") ) %>% `colnames<-`( c("Outcome","N","Gain","Percentage") ),
#  rows = NULL, theme = ttheme_minimal( padding = unit(c(8,8), "mm") )
#) %>%
#  grid.arrange() %>%
#  ggsave( "tabs/t1_spearman_rho_with_bootstrapped_95_percent_ci.jpg" , width = 13.7/2, height = 7.57/2, plot = . )


# ---- Spearman's Rho correlation matrix ----

# set theme of ggplot2
theme_set( theme_bw( base_size = 18) )

# plot it
ggpairs(
  # list the variables
  d0[ , c("drs_gain", "R_drs", "bdi_gain", "R_bdi", "mds_updrs_iii_gain", "R_mds_updrs_iii") ],
  #d0[ , c( paste0( c("drs","bdi","mds_updrs_iii"), "_gain"), paste0( "R_", c("drs","bdi","mds_updrs_iii") ) ) ],
  # Spearman's Rho above the diagonal, histograms on the diagonal, scatterdot plots below the diagonal
  upper = list( continuous = wrap( "cor", method = "spearman" ) ),
  diag = list( continuous = wrap( "barDiag", bins = 10 ) ),
  lower = list( continuous = "points" )
)

# save it
ggsave( "figs/f1_spearman_cor_matrix.jpg", dpi = 300, width = 13.7, height = 14.4 )

