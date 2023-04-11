# In the latest run (2023-01-10) I ran in R version 4.2.0 (2022-04-22), on aarch64-apple-darwin20 (64-bit)
# platform under macOS 13.1. the following versions of packages employed: dplyr_1.0.9 and tidyverse_1.3.1

# set working directory (works in RStudio only)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# list packages to be used
pkgs <- c( "dplyr", "tidyverse" ) # data wranggling

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# read the outcome data
d0 <- read.csv( "data/dbs_mriCONN_replicate_outcome_data.csv", sep = ";" )

# extract only the included patients
d1 <- d0 %>% filter( ass %in% c("pre","r1") )
d1 <- d1[ !( d1$id %in% ( which( table(d1$id) < 2 ) %>% names() ) ) , ]


# ---- VATs extraction ----

# list all patients in the MRI data folder
id.mri <- list.files( "data/mri", recursive = F ) %>% as.data.frame() %>% filter( grepl("IPN", . ) ) %>% t() %>% as.character()
id.out <- unique( d1$id ) %>% sort()

# check whether the IDs are identical in the outcome file as in the MRI data folder
isTRUE( all.equal( id.mri, id.out ) ) # a-ok

# list paths to VATs for each patient
VAT.path <- lapply( id.mri, function(i)
  
  # first list all paths to all files for each patient
  list.files( paste0( "data/mri/", i ) , recursive = T ) %>% as.data.frame() %>%
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
# first prepare an overall folder for VATs
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
      file.copy( from = paste0( "data/mri/", i, "/", VAT.path[[i]][j] ),
                 to = paste0( "data/vat/", i, "/", sub( ".*/", "", VAT.path[[i]][j] ) ) )
    }
  }
}







# ---- check the new patients ----

# read-out the summary of VAT calculation
d.stim <- read.csv( "data/dbs_mriCONN_replicate_stimulation_sum.csv", sep = "," )

# extract IDs of the new patients
id.new <- VAT.sum[ ( ( VAT.sum[ , 2:3 ] %>% rowSums() ) > 1 ) , "id" ]
id.new <- id.new[ ( !( VAT.sum[ ( ( VAT.sum[ , 2:3 ] %>% rowSums() ) > 1 ) , "id" ] %in% d.stim[ d.stim$stim_orig == 1, "id" ] ) ) ]
id.not <- VAT.sum[ ( ( VAT.sum[ , 2:3 ] %>% rowSums() ) == 0 ) , "id" ]

# prepare sums scores for each outcome
# starting with neuropsychology
for ( i in c("drs","bdi") ) d1[[paste0("psych.",i)]] <- d1[ , grepl( paste0("psych.",i), names(d1) ) ] %>% rowSums()

# continue with motor scores
d1 <- d1 %>% mutate(
  motor.mds_updrs_iii =
    case_when( ass == "pre" ~ motor.med_off, ass == "r1" ~ motor.stim_on ) +
    case_when( motor.ldopa_test == "updrs_iii" ~ 7, motor.ldopa_test == "mds_updrs_iii" | is.na(motor.ldopa_test) ~ 0 )
  )

# extract outcome data of newly localized patients
d.new <- d1[ d1$id %in% id.new, c("id","ass","psych.drs","psych.bdi","motor.mds_updrs_iii") ] %>%
  # change to a wide format
  pivot_wider( id_cols = "id", names_from = "ass", values_from = c("psych.drs","psych.bdi","motor.mds_updrs_iii") ) %>%
  # add medication when postoperative MDS UPDRS III was taken
  mutate( med = sapply( id, function(i) d1[ with( d1, id == i & ass == "r1" ) , "motor.med" ] ) %>% sub( ".*\\." , "" , . ) )

# the same for not yet localized ones
d.not <- d1[ d1$id %in% id.not, c("id","ass","psych.drs","psych.bdi","motor.mds_updrs_iii") ] %>%
  # change to a wide format
  pivot_wider( id_cols = "id", names_from = "ass", values_from = c("psych.drs","psych.bdi","motor.mds_updrs_iii") ) %>%
  # add medication when postoperative MDS UPDRS III was taken
  mutate( med = sapply( id, function(i) d1[ with( d1, id == i & ass == "r1" ) , "motor.med" ] ) %>% sub( ".*\\." , "" , . ) )

# number of new patients for each outcome
data.frame(
  drs = with( d.new, !( is.na(psych.drs_pre) | is.na(psych.drs_r1) ) ) %>% sum(),
  bdi = with( d.new, !( is.na(psych.bdi_pre) | is.na(psych.bdi_r1) ) ) %>% sum(),
  updrs = with( d.new, !( is.na(motor.mds_updrs_iii_pre) | is.na(motor.mds_updrs_iii_r1) | med == "on" ) ) %>% sum(),
  all = ( ( is.na( d.new[,2:7] ) %>% rowSums() ) == 0 & d.new$med == "off" ) %>% sum()
)

# number of not yet localized patients for each outcome
data.frame(
  drs = with( d.not, !( is.na(psych.drs_pre) | is.na(psych.drs_r1) ) ) %>% sum(),
  bdi = with( d.not, !( is.na(psych.bdi_pre) | is.na(psych.bdi_r1) ) ) %>% sum(),
  updrs = with( d.not, !( is.na(motor.mds_updrs_iii_pre) | is.na(motor.mds_updrs_iii_r1) | med == "on" ) ) %>% sum(),
  all = ( ( is.na( d.not[,2:7] ) %>% rowSums() ) == 0 & d.not$med == "off" ) %>% sum()
)
