# This is a script used to analyze the data for the replication study of presumed predictive value of specific
# MRI connectivity patterns of stimulated parts of STN in STN DBS in PD for motor, cognitive and affective outcomes.

# The input for this script comprises of data that were pre-processed via '00_import.R' and comprise of:
#
# (i) outcome clinical data including preop. and one year postop. values of
#   (i.i) DRS-2 for cognitive assessment,
#   (i.ii) BDI-II for mood assessment,
#   (i.iii) STAIX1 for state anxiety assessment,
#   (i.iv) STAIX2 for trait anxiety assessment,
#   (i.v) MDS-UPDRS III for motor symptoms assessment,
#
# (ii) sets of
#   (ii.i) fMRI-based predictions (from Clemens, April 2023),
#   (ii.ii) multivariate non-motor model predictions (from Garance, June 2023),
#   (ii.iii) multiple motor symptoms models predictions (from Patricia, October 2023),
#   (ii.iv) symptom-specific  predictins (from Nandith, October 2023)

# The output of comprises of:
#
# (i) a set of correlation matrices (one for each set of predictions, i.e., source),
# (ii) a table summarising patient sample characteristics (only number for now),
# (iii) a table mapping single variables to their labels used in graphing,
# (iv) a complementary data set including patients "ready-to-be-included"


# ENVIRONMENT SETUP ----

# list packages to be used
pkgs <- c("rstudioapi","tidyverse","psych", "corrplot")

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# set working directory (works in RStudio only)
setwd( dirname(getSourceEditorContext()$path) )

# create folders to store results in
# prints TRUE and creates the folder if it was not present, prints NULL if the folder was already present.
sapply( c("_figs","_tabs"), function(i) if( !dir.exists(i) ) dir.create(i) )

# prepare a color palette
col <- colorRampPalette(colors = c("#f4ff4d", "#c7d123", "#acb515", "#81890b", "#656c06") )


