# Analyse motor outcomes' one year post STN-DBS motor scores using VAT-based connectivity estimates
# and their overlap with previous results

# ENVIRONMENT SETUP ----

# clear environment
rm( list = ls() )

# list packages to be used
pkgs <- c("here","tidyverse","psych","ggplot2")#"corrplot")

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# create folders to store results in
# prints TRUE and creates the folder if it was not present, prints NULL if the folder was already present
sapply( c("figs","tabs"), function(i) if( !dir.exists(i) ) dir.create(i) )

# prepare a color palette
#col <- colorRampPalette(colors = c("#f4ff4d", "#c7d123", "#acb515", "#81890b", "#656c06") )


# IN-HOUSE FUNCTIONS ----

# print rounded number
rprint <- function( x, dec = 2 ) sprintf( paste0("%.",dec,"f"), round( x, dec ) )

# calculate cases used for analysis
case_count <-
  
  function( out, pred, d0 ) {
    
    d <- na.omit( d0[ , c(out,pred) ] ) # extract all outcome values from a data set with predictor present
    return( nrow(d) ) # return number of non-NAs
    
  }


# DATA PROCESSING ----

# read the data
d0 <- read.csv( here("_raw","motor_sum_scores.csv"), sep = "," ) # motor data
ids <- c( read.csv( here("_raw","pats_included.csv"), sep = ",", header = F ) )$V1 # patients included so far
prd <- read.csv( here("_data","combined_respred.csv"), sep = "," ) # predictions
map <- read.csv( here("_data","respred_pairs.csv"), sep = ";" ) # mapping of overlaps to responses

# keep only mapping to UPDRS III
map <- map[ with( map, grepl("updrs_iii",response) | source == "symptom" ), ]

# keep only predictors of UPDRS III
prd <- prd[ prd$id %in% ids, c("id",map$predictor) ]

# extract outcomes of interest
out <-
  
  unique( map$response ) %>%
  strsplit( split = ",", fixed = T ) %>%
  unlist() %>%
  c( "ledd_mg", . )

# keep only outcomes of interest
d1 <-
  
  d0 %>%
  
  filter( id %in% ids ) %>% # included patients
  filter( event %in% c("screening","y1") ) %>% # pre- and one-year post-surgery
  filter( stim %in% c("none","on") ) %>% # stimulation turned ON
  filter( med == "off" ) %>% # medication OFF

  select( -stim, -med ) %>% # don't need stimulation and medication variables from now on
  
  mutate( event = case_when( event == "screening" ~ "pre", event == "y1" ~ "post" ) ) %>%
  
  pivot_wider(
    id_cols = c( id, sex, hy, type_pd, asym_park, edu_years ), # pre-surgery variables
    values_from = c( age_years, stimtime_years, vattime_months, all_of(out) ), # time-varying variables
    names_from = event
  ) %>%
  
  mutate( across( paste0(out,"_post"), .names = "{.col}_gain" ) - across( paste0(out,"_pre") ) ) %>%
  
  left_join( prd, by = "id" )


# SAMPLE DESCRIPTION ----


