# This is a script used to prepare the data for the replication study of presumed predictive value of specific
# fMRI connectivity patterns of stimulated parts of STN in STN DBS in PD for motor, cognitive and affective outcomes.

# The script works directly with outcome variables data (UPDRS-III, DRS-2, BDI-II) from Prague data set as well as with
# overlaps of Prague patients' one-year post-surgery VATs-based connectivity with (f)MRI maps implying better/worse post-DBS
# outcome by NetStim group.

# list packages to be used
pkgs <- c("rstudioapi","tidyverse","purrr")

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


# RESPONSE DATA EXTRACTION ----

# list variables of interest
v <- c( "mds_updrs_iii", psy$scale )

# keep only pre- and r1- assessments which are to be included
d0 <-
  d0[ d0$redcap_event_name %in% c("screening_arm_1","nvtva_r1_arm_1") , ] %>%
  rename( "id" = "study_id" ) %>%
  rename( "event" = "redcap_event_name") %>%
  mutate( event = case_when( grepl("screening",event) ~ "pre", grepl("nvtva_r1",event) ~ "r1" ) )

# exclude patients with no pre- or post-test
d0 <- d0[ !( d0$id %in% ( which( table(d0$id) < 2 ) %>% names() ) ) , ]

# extract FAQ item scores
for( i in unlist( strsplit( with( psy, item[scale=="faq"] ), "," ) ) ) {
  d0[ , paste0("faq_",i) ] <-
    case_when(
      d0[ , paste0("faq_uvod_",i) ] == 1 ~ d0[ , paste0("faq_vykon_",i) ], # the patient evaluated an activity directly
      d0[ , paste0("faq_uvod_",i) ] == 2 ~ d0[ , paste0("faq_nikdy_",i) ]  # the patient evaluated an activity indirectly
    )
}

# remove all FAQ scores apart from item responses
d0 <- d0[ , -which( grepl( "faq_fill|faq_uvod|faq_vykon|faq_nikdy|faq_score", names(d0) ) ) ]

# remove DRS-2 total score from the data set (we want single subscores only)
d0 <- d0[ , -which( names(d0) == "drsii_total" ) ]

# tidy-up column names for BDI-II
names(d0)[ grepl("bdi",names(d0)) ] <- names(d0)[ grepl("bdi",names(d0)) ] %>% sub( "_[^_]*$", "", . )

# extract item-level data for all response variables of interest
d1 <-
  lapply( setNames(v,v),
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
                              # way too sophisticated re-coding for items
                              # (because they differ across conditions in RedCap quite enough to make it messy)
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


# RESPONSE DATA TRANSFORMATION ----

# in this chunk of code we pre-process (i.e., sum items for the most part) response data
# start by reversing item scores where applicable (psychological variables only, no reverse items in MDS UPDRS-III)
with(
  psy,
  for ( i in scale[complete.cases(rev)] ) for ( j in unlist( strsplit(rev[scale==i],",") ) ) {
    # reverse item scores by subtracting raw score from scale's (min + max)
    d1[[i]][j, , ] <<- # double arrow to ensure the results will go beyond with()
      ( max[scale==i] + min[scale==i] ) - d1[[i]][j, , ]
  }
)

# prepare an array for MDS UPDRS-III subscales
for ( i in mot$scale[-1] ) d1[[i]] <- d1$mds_updrs_iii[ with( mot, unlist( strsplit( item[scale==i], "," ) ) ), , , , ]


# ---- prepare a wide dataframe with outcomes ----

# prepare a dataframe with sum scores of each psychological variable of interest
df <- lapply( with( psy, setNames(scale,scale) ), function(i) sapply( c("pre","r1"), function(j) colSums( d1[[i]][ ,j, ] ) ) )

# add to df MDS UPDRS-III (sub)scales
for ( i in mot$scale ) {
  df[[i]] <-
    lapply(
      # loop through stimulation conditions
      with( dimnames(d1[[i]]), setNames(stim,stim) ),
      function(j)
        # loop through conditions conditions
        lapply(
          with( dimnames(d1[[i]]), setNames(medic,medic) ),
          function(k)
            # loop through events last in order for them to be on the tail of final variable names
            sapply( dimnames(d1[[i]])$event, function(l) colSums( d1[[i]][ ,j,k,l, ] )  )
        )
    ) %>%
    
    # pull all the conditions together
    do.call( cbind.data.frame, . )

}

# pull all the variable data frames into a single (wide) table
df <- do.call( cbind.data.frame, df ) %>%
  
  # in column names use underscores "_" instead of dots "."
  `colnames<-`( gsub( ".", "_", colnames(.), fixed = T ) ) %>%
  
  # add LEDD for both pre and r1 assessments
  mutate(
    ledd_pre = sapply( rownames(.), function(i) with( d0, levodopa_equivalent[ id == i & event == "pre" ] ), USE.NAMES = F ),
    ledd_r1 = sapply( rownames(.), function(i) with( d0, levodopa_equivalent[ id == i & event == "r1" ] ), USE.NAMES = F )
  ) %>%
  
  # drop columns of conditions that were not measured (such as preop on stimulation conditions)
  # NOTE THAT THESE COLUMNS CAN BE IDENTIFIED VIA COMPUTING COLUMN MEANS BECAUSE THEIR MEAN DOES NOT EXIST (RESULTING IN NaN)
  select( all_of( colnames(.)[ !( apply( ., 2, mean, na.rm = T ) %>% cbind() %>% is.na() ) ] ) ) %>%
  
  # put IDs to a column for later merging via full_join()
  rownames_to_column("id")


# OVERLAPS PRE-PROCESSING ----

# in this chunk of the script we will rename and shuffle overlap (prediction) data in order for them to be easily integrable
# with the rest of the data sets

# for fMRI overlaps rename id column, skim redundant variables, tidy-up names and pivot wider
for( i in paste0("fMRI_",1:2) ) {
  d3[[i]] <-
    d3[[i]] %>%
    `colnames<-`( c("id","template","overlap", "cat") ) %>%
    select(-cat) %>%
    mutate( template = sub( ".nii.gz", "", template ) ) %>%
    pivot_wider( values_from = overlap, names_from = template )
}

# for the multivariable model rename id column and skim redundant variable
d3$mulvar <- 
  d3$mulvar %>%
  rename( "id" = "Patient" ) %>%
  select(-X)

# for the multiple predictions model transform id column to canonical form, skim redundant variables and rename columns
d3$multip <-
  d3$multip %>%
  mutate( id = sub( "'","",sub( "sub-","",Patients) ) ) %>%
  select( -Patients, -X ) %>%
  `colnames<-`( gsub( ".", "", colnames(.), fixed = T ) )

# collapse or overlap data into a single data frame
d3 <- reduce( d3, full_join, by = "id" )


# SAVE DATA ----

# merge observed data (df) with overlap estimations for predictions (d3) and save the result data set as .csv
df <- full_join( df, d3, by = "id")
write.table( x = df, file = "_data/combined_obspred.csv", sep = ",", na = "NA", dec = ".", row.names = F, quote = F )


# SESSION INFO ----

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = "import_envir.txt" )
