# This is a script used to prepare the data for the replication study of presumed predictive value of specific
# fMRI connectivity patterns of stimulated parts of STN in STN DBS in PD for motor, cognitive and affective outcomes.

# The goals of this script are:
# (i) deface MRIs of patients that were not defaced previously,
# (ii) document quality checks and pre-processing steps in Lead-DBS leading to extraction of volumes of activated tissue (VATs),
# (iii) shuffle the files so that they end up in the right places.

# list packages to be used
pkgs <- c("rstudioapi", # setting working directory via RStudio API
          "dplyr", "tidyverse", # data wrangling
          "RNifti" # R-native NIfTI tools
          )

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# set working directory (works in RStudio only)
setwd( dirname(getSourceEditorContext()$path) )

# write down some important parameters
d.dir <- "data/mri" # Where the MRI data is?

# read patients identificators
d.out <- read.csv( "data/dbs_connREPLI_outcome_data.csv", sep = ";" ) # outcome data
d.def <- read.csv( "data/dbs_connREPLI_pats2deface.csv", header = F )$V1 # patients to deface


# ---- outcome data pre-processing ----

# keep only pre- and r1- assessments which are to be included
d.out <- d.out[ d.out$ass %in% c("pre","r1") , ]

# exclude patients with no pre- or post-test
d.out <- d.out[ !( d.out$id %in% ( which( table(d.out$id) < 2 ) %>% names() ) ) , ]

# prepare sums scores for each outcome
# starting with neuropsychology
for ( i in c("drs","bdi") ) d.out[[i]] <- d.out[ , grepl( paste0("psych.",i), names(d.out) ) ] %>% rowSums()

# continue with motor scores
d.out <- d.out %>% mutate(
  # create a new variable with MDS-UPDRS III (after transformation from the old UPDRS III un some cases)
  mds_updrs_iii =
    case_when( ass == "pre" ~ motor.med_off, ass == "r1" ~ motor.stim_on ) + # raw score
    case_when( motor.ldopa_test == "updrs_iii" ~ 7, motor.ldopa_test == "mds_updrs_iii" | is.na(motor.ldopa_test) ~ 0 ) # add seven for patients with the old UPDRS III
)

# keep only variables of interest
d0 <- d.out[ , c("id","ass","drs","bdi","mds_updrs_iii","ledd") ] %>% mutate( ledd = as.numeric( sub(",",".",ledd) ) ) %>% `rownames<-`( 1:nrow(.) )

# collapse patient's IPN187 pre assessment into a single row
d0 <- d0[-37, ] # delete the first IPN187 pre row
d0[ with(d0, id == "IPN187" & ass == "pre") , c("drs","bdi","mds_updrs_iii","ledd") ] <- c(133,12,43,1596.25)

# save the outcome data frame as .csv for further analyses
write.table( arrange(d0, d0$id), file = "data/preds/dbs_connREPLI_observed_outcomes.csv", row.names = F, sep = ",", quote = F )


# ---- extract patients who will be included ----

# remove MRIs of excluded patients
for( i in list.files( d.dir, recursive = F )[ !( list.files( d.dir, recursive = F ) %in% unique(d.out$id) ) ] ) unlink( paste0( d.dir, "/", i ), recursive = T )


# ---- defacing via spm_deface ----

# write a MatLab script for spm_deface
writeLines( paste0( "spm_deface( {\n",
                    paste( paste0("'", getwd(), "/data/mri/", paste0( d.def, "/anat_t1.nii" ),"'"),
                           collapse = "\n"),
                    "\n} )"
                    ), con = "processing/conduct_spmdeface.m" )

# go to MatLab and run the code there

# remove original (faced) T1s and rename the new (defaced) T1s appropriately
for ( i in d.def ) {
  
  # do not run if the data were already processed via Lead-DBS (i.e., the original file was renamed to "raw_anat_t1.nii")
  if ( !file.exists( paste0( d.dir, "/", i, "/raw_anat_t1.nii" ) ) ) {
    
    file.remove( paste0( d.dir, "/", i, "/anat_t1.nii" ) ) # remove the original
    file.rename( from = paste0(d.dir,"/",i,"/anon_anat_t1.nii"), to = paste0(d.dir,"/",i,"/anat_t1.nii") ) # rename the defaced file
    
  }
}


# ---- coregistration via LeadDBS ----

# run "processing/dbs_connREPLI_leaddbs_coregistration.m"
# because I accidentally overwrote the defaced patients, had to run "processing/dbs_connREPLI_leaddbs_coregistration_repair.m" as well

# read-out the summary of coregistration
d.cor <- read.csv( "data/dbs_connREPLI_coregistration_sum.csv", sep = "," )

# try different algorithms for patients with unsuccessful coregistration and list the results
d.cor <- d.cor %>% mutate( t2_action = case_when( id %in% c("IPN195","IPN263") ~ "fsl_flirt", id == "IPN214" ~ "spm&ants" ),
                           ct_action = case_when( id == "IPN211" ~ "fsl_flirt" )
                           )


# ---- normalization via LeadDBS ----

# run "processing/dbs_connREPLI_leaddbs_normalization.m"
# because I accidentally overwrote the defaced patients, had to run "processing/dbs_connREPLI_leaddbs_normalization_repair.m" as well

# read-out the summary of normalization
d.nor <- read.csv( "data/dbs_connREPLI_normalization_sum.csv", sep = "," )


# ---- electrode localization via LeadDBS ----

# print patients according to the electrodes for Lead pre-reconstruction
with( d.out, id[ grepl( "st.jude|cardion", stim_pars.electrode ) ] ) %>% sort() # St. Jude, N = 35
with( d.out, id[ grepl( "medtronic_3389", stim_pars.electrode ) ] ) %>% sort() # Medtronic 3389, N = 26
with( d.out, id[ grepl( "medtronic_B33005", stim_pars.electrode ) ] ) %>% sort() # Medtronic B33005, N = 1

# this one is done manually via LeadDBS in MatLab

# read-out the summary of localization
d.loc <- read.csv( "data/dbs_connREPLI_localization_sum.csv", sep = "," )


# ---- VATs calculation via LeadDBS ----

# first, trim down to only patients with nice looking data
d.loc[ d.loc$loc_quality %in% c("bad","thrash"), "id" ] # do not calculate VATs for thrash localizations if there are any
d.out[ d.out$ass == "r1" & is.na(d.out$stim_pars.right_neg_cont) , "id" ] # patients with missing stimulation parameters

# read-out the summary of VAT calculation
d.stim <- read.csv( "data/dbs_connREPLI_stimulation_sum.csv", sep = "," )


# ---- MRI pre-processing summary ----

# put summary of all pre-processing steps into a single object
d.sum <- left_join( d.cor, d.nor, by = "id" ) %>% left_join( d.loc, by = "id" ) %>% left_join( d.stim, by = "id" )

# save as .csv
write.table( d.sum, file = "data/dbs_connREPLI_preprocessing_summary.csv", sep = ",", row.names = F, quote = F )


# ---- VATs extraction ----

# list all patients in the MRI data folder
id.mri <- list.files( d.dir, recursive = F ) %>% as.data.frame() %>% filter( grepl("IPN", . ) ) %>% t() %>% as.character()
id.out <- unique( d0$id ) %>% sort()

# check whether the IDs are identical in the outcome file as in the MRI data folder
isTRUE( all.equal( id.mri, id.out ) ) # a-ok

# list paths to VATs for each patient
VAT.path <- lapply( id.mri, function(i)
  
  # first list all paths to all files for each patient
  list.files( paste0( d.dir, "/", i ) , recursive = T ) %>% as.data.frame() %>%
    # keep only paths to VATs in MNI space, niix format, drop the files with gaussian efield
    filter( grepl("MNI", . ) & grepl( "vat", . ) & grepl( "nii", . ) & !grepl( "gauss", . ) ) %>%
    t() %>% as.character() # do some housekeeping
  
) %>% `names<-`( id.mri )

# list summary of all VATs present
VAT.sum <- data.frame( id = id.mri ) %>%
  # add a dummy variable indicating whether there's VAT for each patient (right/left separately)
  mutate( vat.right = sapply(id, function(i) any( grepl( "right", VAT.path[[i]] ) ) ) %>% as.numeric(),
          vat.left = sapply(id, function(i) any( grepl( "left", VAT.path[[i]] ) ) ) %>% as.numeric() )

# extract all VATs and put them in their own folders
# first prepare a folder for all the VATs
if( !dir.exists("data/vat") ) dir.create( "data/vat" )

# next add VATs of each patient
for ( i in names(VAT.path) ) {
  
  # prepare patient folder
  if( !dir.exists( paste0( "data/vat/", i ) ) ) dir.create( paste0( "data/vat/", i ) )
  
  # check whether there's any VAT for the patient i
  if ( length( VAT.path[[i]] != 0 ) ) {
    
    # loop through all files of each patient
    for ( j in 1:length( VAT.path[[i]] ) ) {
      
      # copy VAT from the mri to vat folder
      file.copy( from = paste( d.dir, i, VAT.path[[i]][j], sep = "/" ),
                 to = paste( d.dir, i, sub( ".*/", "", VAT.path[[i]][j] ), sep = "/" )
                 )
      
    }
  }
}


# ---- session info ----

if( !dir.exists("sess") ) dir.create("sess") # prepare a folder for session's info if it does not exist yet
capture.output( sessionInfo(), file = "sess/data_prep.txt" ) # write the sessionInfo() into a .txt file
