# In the latest run (2022-09-12) I ran in R version 4.2.0 (2022-04-22), on aarch64-apple-darwin20 (64-bit)
# platform under macOS Monterey 12.4. the following versions of packages employed: dplyr_1.0.9, tidyverse_1.3.1,
# and RNifti_1.4.1

# set working directory (works in RStudio only)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# list packages to be used
pkgs <- c("dplyr", # for object manipulations
          "tidyverse", # for more object manipulations
          "RNifti" # R-native NIfTI tools
)

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}