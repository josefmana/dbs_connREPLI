setwd( dirname(rstudioapi::getSourceEditorContext()$path) ) # set working directory (works only in RStudio)
data.dir <- "data" # where the data is?

# conduct zipping for all the data inside the data folder
lapply( list.files(data.dir), # zip only BIDS data
        function(i)
          zip( zipfile = paste0( data.dir, "/", i, ".zip" ), # output zip file
               files = paste( data.dir, i, list.files( paste0(data.dir,"/",i) ), sep = "/" ) # input raw files
               )
        )
