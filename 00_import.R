# This is a script used to prepare the data for the replication study of presumed predictive value of specific
# fMRI connectivity patterns of stimulated parts of STN in STN DBS in PD for motor, cognitive and affective outcomes.

# The script works directly with outcome variables data (UPDRS-III, DRS-2, BDI-II) from Prague data set as well as with
# overlaps of Prague patients' one-year post-surgery VATs-based connectivity with (f)MRI maps implying better/worse post-DBS
# outcome by NetStim group.

# list packages to be used
pkgs <- c("rstudioapi","tidyverse")

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# set working directory (works in RStudio only)
setwd( dirname(getSourceEditorContext()$path) )

# prepare a data folder for the outcomes
if( !dir.exists("_data") ) dir.create("_data")


# IN-HOUSE FUNCTIONS ---

# prepare an array of observed data
obs_array <- function( d, vars, nms, resp ) {
  
  # prepare dimension sizes and names
  dims <- sapply( setNames(vars,nms), function(i) length( unique( d[[i]] ) ) )
  dimnms <- lapply( setNames(vars,nms), function(i) unique( d[[i]] ) )
  
  # prepare a data set with all combinations of vars levels included
  d <- expand.grid(dimnms) %>% rev() %>% left_join( d, by = vars )
  
  # set-up an array for the data
  out <- array( data = d[[resp]], dim = dims, dimnames = dimnms )
  return(out)

}


# DATA READ ----

# read outcome data
d0 <- read.csv( "_raw/redcap_export_20231102.csv", sep = "," ) # outcome data
its <- read.csv( "_raw/mds_updrs_iii_redcap_names.csv", sep = "," ) # MDS UPDRS-III RedCap names
mot <- read.csv( "_raw/mds_updrs_iii_scoring.csv", sep = ";" ) # scoring of MDS UPDRS-III
psy <- read.csv( "_raw/psycho_scoring.csv", sep = ";" ) # scoring of psychological variables

# read overlaps
d3 <-
  list( fMRI_1 = read.csv( "_raw/overlaps_202304_1.csv", sep = ","), # the first set of overlaps (Clemens, April 2023)
        fMRI_2 = read.csv( "_raw/overlaps_202304_2.csv", sep = ","), # the first set of overlaps (Clemens, April 2023)
        mulvar = read.csv( "_raw/overlaps_202306.csv", sep = ";", dec = "," ), # the second set of overlaps (Garance, June 2023)
        multip = read.csv( "_raw/overlaps_202310.csv", na.strings = "NaN", skip = 1 ) # the third set of overlaps (Patricia & Nanditha, October 2023)
        )


# RESPONSE DATA PRE-PROCESSING ----

# keep only pre- and r1- assessments which are to be included
d0 <-
  d0[ d0$redcap_event_name %in% c("screening_arm_1","nvtva_r1_arm_1") , ] %>%
  rename( "id" = "study_id" ) %>%
  rename( "event" = "redcap_event_name") %>%
  mutate( event = case_when( grepl("screening",event) ~ "pre", grepl("nvtva_r1",event) ~ "r1" ) )

# exclude patients with no pre- or post-test
d0 <- d0[ !( d0$id %in% ( which( table(d0$id) < 2 ) %>% names() ) ) , ]

# ---- extract response data ----

# extract FAQ item scores
for( i in unlist( strsplit( with( psy, item[scale=="faq"] ), "," ) ) ) {
  d0[ , paste0("faq_",i) ] <-
    case_when(
      d0[ , paste0("faq_uvod_",i) ] == 1 ~ d0[ , paste0("faq_vykon_",i) ], # the patient evaluated an activity directly
      d0[ , paste0("faq_uvod_",i) ] == 2 ~ d0[ , paste0("faq_nikdy_",i) ]  # the patient evaluated an activity indirectly
    )
}

# remove all FAQ scores apart from items
d0 <- d0[ , -which( grepl( "faq_fill|faq_uvod|faq_vykon|faq_nikdy|faq_score", names(d0) ) ) ]

# remove DRS-2 total score from the data set (we want single subscores only)
d0 <- d0[ , -which( names(d0) == "drsii_total" ) ]

# tidy-up column names for BDI-II
names(d0)[ grepl("bdi",names(d0)) ] <- names(d0)[ grepl("bdi",names(d0)) ] %>% sub( "_[^_]*$", "", . )

# extract item-level data for all response variables of interest
d1 <-
  lapply( with( psy, setNames( c("mds_updrs_iii",scale), c("mds_updrs_iii",scale) ) ),
          function(i) {
            
            # MDS UPDRS-III needs special treatment because it was measured in medication/stimulation on/off states
            if ( i == "mds_updrs_iii") {
              
              # prepare data set containing MDS UPDRS-III item responses
              d <-
                lapply( setNames( names(its), names(its) ),
                        function(j) # loop through conditions/combinations from above
                          
                          # and select only data from included patients and measures from selected combination
                          d0[ with( d0, event == strsplit(j,"_")[[1]][1] ) , c( "id", t(its[,j]) ) ] %>%
                          # rename the columns such that they are identical across combinations
                          `colnames<-`(
                            c(# patient id first
                              colnames(.)[1],
                              # way too sophisticated re-coding for items (because they differ across conditions in RedCap quite enough to make it messy)
                              sub( "_[^_]*$", "" , colnames(.)[2:ncol(.)] ) %>%
                                sub( "mdsupdrs_3", "item", . ) %>%
                                sub( "_ldopatest", "", . ) %>%
                                sub( "_ldopateston", "", . )
                            )
                          ) %>%
                          
                          # add variables for event (pre vs post), medication (on vs off) and stimulation (on vs off vs no)
                          mutate( event = strsplit(j,"_")[[1]][1],
                                  medic = strsplit(j,"_")[[1]][2],
                                  stim = strsplit(j,"_")[[1]][3],
                                  .after = id
                          )
                ) %>%
                
                # collapse all the data to a single sexy data file
                do.call( rbind.data.frame, . ) %>%
                arrange( by = id ) %>%
                pivot_longer( cols = contains("item"),
                              values_to = "score",
                              names_to = "item",
                              names_transform = function(x) sub( "_", "", sub("item_","",x) )
                              )
              
              # extract data array
              # VARIABLES ORDER IS CRITICAL FOR CORRECT EXTRACTION
              return( obs_array( d = d, vars = c("item","stim","medic","event","id"), nms = c("item","stim","medic","event","id"), resp = "score") )
              
            } else {
              
              # prepare the data to suitable format
              d <-
                pivot_longer( data = d0,
                              cols = contains( paste0(i,"_") ),
                              values_to = "score",
                              names_to = "item",
                              names_transform = function(x) sub( paste0(i,"_"), "", x )
                              ) %>%
                select( id, event, item, score )
              
              # extract data array
              # VARIABLES ORDER IS CRITICAL FOR CORRECT EXTRACTION
              return( obs_array( d = d, vars = c("item","event","id"), nms = c("item","event","id"), resp = "score") )
              
            }
    
          }
        )

# save the resulting array as .rds file
saveRDS( object = d1, file = "_data/response_data.rds" )

