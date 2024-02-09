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

# clear environment
rm( list = ls() )

# list packages to be used
pkgs <- c("here","tidyverse","psych","corrplot")

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# create folders to store results in
# prints TRUE and creates the folder if it was not present, prints NULL if the folder was already present.
sapply( c("_figs","_tabs"), function(i) if( !dir.exists(i) ) dir.create(i) )

# prepare a color palette
col <- colorRampPalette(colors = c("#f4ff4d", "#c7d123", "#acb515", "#81890b", "#656c06") )


# IN-HOUSE FUNCTIONS ----

# calculate cases used for analysis
case_count <- function( out, pred, d0 ) {
  
  # extract all outcome values from a data set with predictor present
  d <- na.omit( d0[ , c(out,pred) ] )
  return( nrow(d) ) # return number of non-NAs
  
}

# plot a correlation matrix
mat_plot <- function( r, p, col, lab, rect, num = .8, tl = .8, cl = .8, rwd = 1 ) {
  
  # extract variable names
  nm <- sapply( rownames(r), function(i) lab[ lab$var == i , "lab" ], USE.NAMES = F )
  
  # rename rows and columns
  r <- r %>% `rownames<-`(nm) %>% `colnames<-`(nm)
  p <- p %>% `rownames<-`(nm) %>% `colnames<-`(nm)
  
  # prepare a plot with blank spaces for p < .05 and rectangle showing correlations of interest
  cp <- 
    corrplot( corr = r, p.mat = p, diag = F, insig = "blank", # inputs + significance handling
              tl.col = "blue", bg = "gray", col = col(100), addCoef.col = "black", # text and colours
              tl.srt = 45, tl.cex = tl, number.cex = num, cl.cex = cl, # text sizes and angles
              method = "circle", type = "lower" # corrmat type
    ) %>%
    # add rectangle showing correlations of interest
    corrRect( namesMat = rect, col = "red", lwd = rwd )
  
  # add values to non-significant results
  with( cp$corrPos, text( x, y, round(corr,2), cex = num ) )
  
}


# DATA PROCESSING ----

# read the data
d0 <- read.csv( here("_data","combined_respred.csv"), sep = "," ) # data set
ids <- c( read.csv( here("_raw","pats_included.csv"), sep = ",", header = F ) )$V1 # patients included so far
lab <- read.csv( here("_raw","varlabels.csv"), sep = "," ) # variables' labels for nicer plots
map <- read.csv( here("_data","respred_pairs.csv"), sep = ";" ) # mapping of overlaps to responses
map <- map[ complete.cases(map$response), ]

# extract all responses of interest
resp <- c( "ledd", na.omit( unlist( lapply( map$response, function(i) strsplit( i, ",") ) ) %>% unique() ) )
resp <- resp[ c(1,4,5,8:11,2,3,6,7) ]

# prepare a data set for correlations
# begin by extracting post-minus-pre differences for ledd and psychological variables
d1 <-
  with(
    d0,
    sapply(
      setNames( resp[c(1,8:11)], paste0(resp[c(1,8:11)],"_gain") ),
      function(i)
        sapply( id, function(j) get( paste0(i,"_r1") )[id==j] - get( paste0(i,"_pre") )[id==j] )
    )
  ) %>%
  
  # change to data frame
  as.data.frame()

# add gain scores for MDS-UPDRS III scores
for ( i in resp[2:7] ) {
  d1[ , paste0(i,"_gain") ] <- with( d0, get( paste0(i,"_on_off_r1") ) - get( paste0(i,"_none_off_pre") ) ) # gain scores
  #d1[ , paste0(i,"_eff") ] <- with( d0, get( paste0(i,"_on_off_r1") ) - get( paste0(i,"_off_off_r1") ) ) # stimtest scores
}

# flip DRS-2 gain scores because they are the only one with inverse interpretation
d1$drsii_gain <- -d1$drsii_gain

# add overlaps
with( map, d1[ , predictor ] <<- d0[ , predictor] )

# prepare data sets for each overlap source separately
d2 <-
  with(
    map,
    
    # loop through sources
    lapply(
      setNames( unique(source), unique(source) ),
      function(i) {
        
        # extract variables of interest
        v <- c(
          paste0( unique( unlist( lapply( response[source == i], function(x) strsplit(x,",") ) ) ), "_gain" ),
          predictor[source == i]
        )
        
        # extract relevant variables and patients
        return( d1[ids, v] )
        
      }
    )
  )


# STATISTICAL ANALYSIS ----

# save the variable labels table
write.table(
  lab %>% `colnames<-`( c("Variable_name", "Label") ),
  file = "_tabs/varlabels.csv",
  sep = ",", row.names = T, col.names = T, quote = F
)

# extract and save number of subjects per correlation pair
write.table(
  x = sapply( names(d1), function(i) sapply( names(d1), function(j) case_count(i,j,d1) ) ),
  file = "_tabs/sample_sizes.csv",
  sep = ",", row.names = T, col.names = T, quote = F
)

# extract correlation matrixes
corrs <-
  lapply(
    setNames( names(d2), names(d2) ),
    function(i) {
      
      # compute Spearman's correlation with bootstrapped 95% CIs and p values
      c <- cor.ci( d2[[i]], n.iter = 1e4, p = .05, method = "spearman", use = "pairwise.complete.cases", plot = F )
      
      # prepare a lower diagonal matrix of p values
      p <- c$rho
      diag(p) <- 0
      p[ upper.tri(p, diag = F) ] <- NA
      p[ lower.tri(p, diag = F) ] <- c$ci$p
      
      # return a list with estimates and p values
      return( list( rho = c$rho, p = p ) )
      
    }
  )

# list rectangle corners for correlation plots
corrs$fMRI$rect <- rbind( c("DRS-2","Reich","MDS-UPDRS III","Horn comb"), rep(NA,4) )
corrs$multivariate$rect <- rbind( c("BDI-II","Apathy pred","DRS-2","MMSE pred"), rep(NA,4) )
corrs$multiple$rect <- rbind( c("UPDRS III","Fiber","MDS-UPDRS III","Euc. dist"), rep(NA,4) )
corrs$symptom$rect <- rbind( c("Bradyk.","Brad. pred","MDS-UPDRS III","UPDRS pred"), rep(NA,4) )


# RESULTS PRESENTATION ----

# ---- per source correlation matrices ----

# loop through all sources
for ( i in names(d2) ) {
  
  # prepare jpeg device
  jpeg( paste0("_figs/corrmat_",i,".jpg"), units = "in", width = 10.9, height = 10.9, res = 300, quality = 100 )
  
  # plot it
  with( corrs[[i]], mat_plot( rho, p, col, lab, rect, num = 1.3, tl = 1.3, cl = 1.3, rwd = 5 ) )
  
  # save it
  dev.off()
  
}


# ---- full correlation matrix ----

# prepare jpeg device
jpeg( "_figs/corrmat_full.jpg", units = "in", width = 10.9, height = 10.9, res = 300, quality = 100 )

# plot it
cor(d1, use = "pairwise.complete.obs", method = "spearman") %>%
  `colnames<-`(lab$lab) %>%
  `rownames<-`(lab$lab) %>%
  corrplot( diag = F, method = "square", type = "lower",
            tl.srt = 45, tl.cex = 1, cl.cex = 1,
            tl.col = "black", col = COL2('PuOr', 200)
  )

# save it
dev.off()


# NEW PATIENTS ----

# prepare a data set with new patients that were not yet included
d3 <- d1[ min( which( rownames(d1) %in% ids ) ):nrow(d1) , ] # keep only newly operated patients
d3 <- d3[ !rownames(d3) %in% ids, ] # drop already included patients

# calculate the number of unused subjects with clinical data and save it
apply( d3, 2, function(x) sum( !is.na(x) ) ) %>% write.table( file = "_tabs/unused.csv", sep = ",", col.names = F, quote = F )


# SESSION INFO ----

# write the sessionInfo() into a .txt file
capture.output( sessionInfo(), file = here("scripts","predassoc_envir.txt" ) )
