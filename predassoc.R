# This is a script used to analyze the data for the replication study of presumed predictive value of specific
# MRI connectivity patterns of stimulated parts of STN in STN DBS in PD for motor, cognitive and affective outcomes.

# The goals of this script are:
# (i) calculate bootstrapped Spearman's correlation of prediction values and outcomes

# list packages to use
pkgs <- c( "rstudioapi", # setting working directory via RStudio API
           "readxl", # working with Excel files
           "tidyverse", "dplyr", # data wrangling
           "corrplot" # plotting
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
d.outs <- read.table( "_nogithub/data/observations.csv", sep = ",", header = T )
d.prds <- read_excel( "_nogithub/raw/preds/Prague_predictions.xls", sheet = "Sheet1" ) %>% rename( "id" = "Patient" )

# list the outcomes
outs <- c( "drs", "bdi", "staix1", "staix2", "mds_updrs_iii", "ledd" )

# extract predictions
prds <- names(d.prds)[ !grepl( "Carer", names(d.prds) ) & grepl( "Total_abs", names(d.prds ) ) ]


# ---- data wrangling ----

# keep only patients with VATs and correlations to normative templates calculated (i.e., included in d.prds)
d0 <- d.outs %>%
  slice( which( id %in% unique(d.prds$id) ) ) %>% # keep only patients with predictions
  pivot_wider( id_cols = id, names_from = ass, values_from = -c(1:2) ) # pivot to a wide format
  
# calculate gain and percentage change scores
for ( i in outs ){
  d0[[ paste0(i,"_gain") ]] <- d0[[ paste0(i,"_r1") ]] - d0[[ paste0(i,"_pre") ]]
  d0[[ paste0(i,"_perc") ]] <- ( d0[[ paste0(i,"_gain") ]] / d0[[ paste0(i,"_pre") ]] ) %>% ifelse( . == Inf, NA, . ) # when pre was 0, the percentage goes to Infinity, change these cases to NA
}

# prepare plotting parameters
# if the plots do not show in the next step, you may want to enlarge the "Plots" tab (in RStudio)
par( mfrow = c( length(outs), 2 ) )

# as a sanity check see histograms of all change scores
for ( i in outs ) {
  hist( d0[[paste0(i,"_gain")]], main = paste0(i,"_gain"), ylab = NULL, xlab = NULL, breaks = 20 )
  hist( d0[[paste0(i,"_perc")]], main = paste0(i,"_perc"), ylab = NULL, xlab = NULL, breaks = 20 )
}

# return plotting parameters to default
par( mfrow = c(1,1) )

# left join the predictions
d0 <- d0 %>% left_join( d.prds, by = "id" )


# ---- Spearman's Rho correlation matrix ----

# prepare a (full Spearman) correlation matrix
r <- cor( d0[ , c( paste0( outs, "_gain" ), prds) ], use = "complete.obs", method = "spearman" )

# prepare a color palette
col <- colorRampPalette(colors = c("#f4ff4d", "#c7d123", "#acb515", "#81890b", "#656c06"))

# prepare jpeg device for saving the correlation matrix
jpeg( "figs/corrmat.jpg", units = "in", width = 10.9, height = 10.9, res = 300, quality = 100 )

# print a stylised correlation matrix
corrplot( r, tl.col = "blue", bg = "gray", addCoef.col = "black", col = col(100), # set-up colors
          tl.srt = 35, tl.cex = 1.2, cl.cex = 1.2,number.cex = 1.1, # set-up symbol sizes
          type = "lower", method = "circle" # set-up corrmat type
          ) %>%
  # add a rectangle marking correlations of interest
  # ie, prediction/observation correlation coefficients
  corrRect( namesMat = rbind( c("drs_gain","Apathy_Total_abs","ledd_gain","MMSE_Total_abs"), rep(NA,4) ), lwd = 5, col = "red" )

# save it
dev.off()


# ---- session info ----

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = "sess/pred_assoc.txt" )
