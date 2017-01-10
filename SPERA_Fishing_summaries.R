####################################################################################################################
# SoG Sponge Reef CSAS paper data extraction
# 
# Objective:  
#
# Background: 
#
# Summary:    
# 
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       June 24, 2015
###################################################################################################################

# start fresh
rm(list=ls())

# Set working directory
getwd()
setwd("F:/GFFOS") 

# Confirm that 32 bit R is being used
cat( "Test which version of R is being used...")
cat( " /64 = 64 bit; /i386 = 32 bit..." )
cat( " 32 bit R needed to open MS Access databases." )
Sys.getenv("R_ARCH")   
# "/i386" 32 bit R --- which is necessary to grab data from MS Access database
# "/64"   64 bit R --- use Tools | Global Options to change R version

# Install missing packages and load required packages (if required)
UsePackages <- function( pkgs, update=FALSE, locn="http://cran.rstudio.com/" ) {
  # Identify missing (i.e., not yet installed) packages
  newPkgs <- pkgs[!(pkgs %in% installed.packages( )[, "Package"])]
  # Install missing packages if required
  if( length(newPkgs) )  install.packages( newPkgs, repos=locn )
  # Loop over all packages
  for( i in 1:length(pkgs) ) {
    # Load required packages using 'library'
    eval( parse(text=paste("library(", pkgs[i], ")", sep="")) )
  }  # End i loop over package names
  # Update packages if requested
  if( update ) update.packages( ask=FALSE )
}  # End UsePackages function

# Make packages available
UsePackages( pkgs=c("ggplot2","RODBC","plyr","data.table", "lattice", "sp", 
                    "rgdal", "maps", "tidyr","Hmisc","reshape2","dplyr") )  ### There may be more packages here than needed ###


########### Functions ##############
# Grab the fishing events
GrabFishingDat <- function( db, data ) {
  # Extract the data base extension
  dbExt <- strsplit( x=basename(db), split=".", fixed=TRUE )[[1]][2]
  # Connect to the data base depending on the extension
  if( dbExt == "mdb" )  dataBase <- odbcConnectAccess( access.file=db )
  if( dbExt == "accdb" )  dataBase <- odbcConnectAccess2007( access.file=db )
  # Grab the logbook data
  fe <- sqlFetch( channel=dataBase, sqtable=data )
  # Grab the survey data
#   hdrs <- sqlFetch( channel=dataBase, sqtable=tbl )
  # Message re logservation data
  cat( "Imported logbook table with", nrow(fe), 
       "rows and", ncol(fe), "columns" )
  # Close the connection
  odbcCloseAll( )
  # Return the data tables
  return( fe )
}  # End function

# Omit records that are not complete based on desired columns
CompleteFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


####################################
# Data Extraction
# 1. Access the data
# 2. 

#########
### 1 ###
#########

# Read in the data
locDB <- "F:/GFFOS/GFFOSSQL.mdb" #### CHANGE WORKING DIRECTORY ####
tbl <- "GF_D_OFFICIAL_FE_CATCH" ### This table is massive....
df <- GrabFishingDat( db=locDB,  data=tbl )
head(df)


df1 <- read.table("F:/SPERA/SoG sponge manuscript/Data/FishingEvents/SoG_FishingEvents.txt", header=TRUE, sep=",")
df1 <- CompleteFun( df1, c("START_LATITUDE","START_LONGITUDE","END_LATITUDE","END_LONGITUDE"))
df1$START_LONGITUDE <- (-1*df1$START_LONGITUDE)
df1$END_LONGITUDE <- (-1*df1$END_LONGITUDE)
write.csv(df1, file="F:/SPERA/SoG sponge manuscript/Data/FishingEvents/SoG_FishingEvents.csv", sep=",")
