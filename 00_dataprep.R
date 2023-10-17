# This is a script used to prepare the data for the replication study of presumed predictive value of specific
# fMRI connectivity patterns of stimulated parts of STN in STN DBS in PD for motor, cognitive and affective outcomes.

# The goals of this script are:
# (i) prepare the outcomes file
# (ii) deface MRIs of patients that were not defaced previously,
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
d.dir <- "_nogithub/data/mri" # Where the MRI data is?

# read data and patients identificators
d.out <- read.csv( "_nogithub/raw/dbs_connREPLI_outcome_data.csv", sep = ";" ) # outcome data
d.def <- read.csv( "_nogithub/raw/dbs_connREPLI_pats2deface.csv", header = F )$V1 # patients to deface
d.its <- read.csv( "_nogithub/raw/ITEMPO_DATA_2023-10-17_1255.csv", sep = "," ) # item-level UPDRS-III
n.its <- read.csv( "_nogithub/raw/mds_updrs_iii_redcap_names.csv", sep = "," ) # MDS-UPDRS-III RedCap names

# prepare a folder for tables, figures, models, and sessions info
sapply( c("tabs","figs","mods","sess","_nogithub","_nogithub/data"), function(i) if( !dir.exists(i) ) dir.create(i) )


# ---- outcome data pre-processing ----

# keep only pre- and r1- assessments which are to be included
d.out <- d.out[ d.out$ass %in% c("pre","r1") , ]

# exclude patients with no pre- or post-test
d.out <- d.out[ !( d.out$id %in% ( which( table(d.out$id) < 2 ) %>% names() ) ) , ]

# prepare sums scores for each outcome
# first re-score STAIX reverse items
for ( i in c(1,2,5,8,10,11,15,16,19,20) ) d.out[ , paste0("psych.staix1_",i) ] <- 5 - d.out[ , paste0("psych.staix1_",i) ]
for ( i in c(1,6,7,10,13,16,19) ) d.out[ , paste0("psych.staix2_",i) ] <- 5 - d.out[ , paste0("psych.staix2_",i) ]

# starting with neuropsychology
for ( i in c("drs","bdi","staix1","staix2") ) d.out[[i]] <- d.out[ , grepl( paste0("psych.",i), names(d.out) ) ] %>% rowSums()

# continue with motor scores
d.out <- d.out %>% mutate(
  # create a new variable with MDS-UPDRS III (after transformation from the old UPDRS III un some cases)
  mds_updrs_iii =
    case_when( ass == "pre" ~ motor.med_off, ass == "r1" ~ motor.stim_on ) + # raw score
    case_when( motor.ldopa_test == "updrs_iii" ~ 7, motor.ldopa_test == "mds_updrs_iii" | is.na(motor.ldopa_test) ~ 0 ) # add seven for patients with the old UPDRS III
)

# keep only variables of interest
d0 <- d.out[ , c("id","ass","drs","bdi","staix1","staix2","mds_updrs_iii","ledd") ] %>% mutate( ledd = as.numeric( sub(",",".",ledd) ) ) %>% `rownames<-`( 1:nrow(.) )

# collapse patient's IPN187 pre assessment into a single row
d0 <- d0[-37, ] # delete the first IPN187 pre row
d0[ with(d0, id == "IPN187" & ass == "pre") , c("drs","bdi","staix1","staix2","mds_updrs_iii","ledd") ] <- c(133,12,34,32,43,1596.25)

# save the outcome data frame as .csv for further analyses
write.table( arrange(d0, d0$id), file = "_nogithub/data/observations.csv", sep = ",", row.names = F, quote = F )


# ---- item-level MDS-UPDRS III data ----

# re-code redcap_event name
d.its <- d.its %>%
  rename( "id" = "study_id" ) %>%
  mutate( event = case_when( redcap_event_name == "screening_arm_1" ~ "pre", redcap_event_name == "nvtva_r1_arm_1" ~ "post" ) )

# extract separate data sets for each pre/post:med_on/med_off:stim_NA/stim_on/stim_off combination (four in our data set)
d1 <- lapply( setNames( names(n.its), names(n.its) ),
              function(i) # loop through conditions/combinations from above
                
                # and select only data from included patients and measures from selected combination
                d.its[ with( d.its, id %in% unique(d0$id) & event == strsplit(i,"_")[[1]][1] ) , c( "id", t(n.its[,i]) ) ] %>%
                # rename the columns such that they are identical across combinations
                `colnames<-`(
                  c(# patient id first
                    colnames(.)[1],
                    # way too sophisticated re-coding for items (because they differ across conditions in RedCap quite enough to make it messy)
                    sub( "_ldopateston", "", sub( "_ldopatest", "", sub( "mdsupdrs_3", "item", sub( "_[^_]*$", "" , colnames(.)[2:ncol(.)] ) ) ) )
                  )
                ) %>%
                
                # add variables for event (pre vs post), medication (on vs off) and stimulation (on vs off vs no)
                mutate( event = strsplit(i,"_")[[1]][1],
                        medic = strsplit(i,"_")[[1]][2],
                        stim = strsplit(i,"_")[[1]][3],
                        .after = id
                        )
              ) %>%
  
  # collapse all the data to a single sexy data file
  do.call( rbind.data.frame, . ) %>%
  arrange( by = id ) # sort by IDs

# save it
write.table( d1, "_nogithub/data/mds_updrs_iii_item_level_observations.csv", sep = ",", row.names = F, quote = F, na = "NA" )
write.table( d1[ d1$event == "pre", ], "_nogithub/data/mds_updrs_iii_item_level_preop.csv", sep = ",", row.names = F, quote = F, na = "NA" ) # pre-surgery only for sharing with colleagues from NetStim


# ---- defacing via spm_deface ----

# write a MatLab script for spm_deface
writeLines( paste0( "spm_deface( {\n",
                    paste( paste0("'", getwd(), d.dir, "/", paste0( d.def, "/anat_t1.nii" ),"'"), collapse = "\n"),
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

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = "sess/data_prep.txt" )
