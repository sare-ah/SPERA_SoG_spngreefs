####################################################################################################################
# Read MS Access database
# 
# Objective:  Connect to an MS Access database and read a table into R
#
# 
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       April, 2016
###################################################################################################################

# start fresh
rm(list=ls())

getwd()
setwd() 

# Check which version of R is being used and reset if necessary
Sys.getenv("R_ARCH")   
# "/i386" 32 bit R --- which is necessary to grab data from MS Access database
# "/64"   64 bit R

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
UsePackages( pkgs="RODBC" )


########### Functions ##############
# Grab the survey data
GrabSurveyDat <- function( db, data, clean=TRUE ) {
  # Extract the data base extension
  dbExt <- strsplit( x=basename(db), split=".", fixed=TRUE )[[1]][2]
  # Connect to the data base depending on the extension
  if( dbExt == "mdb" )  dataBase <- odbcConnectAccess( access.file=db )
  if( dbExt == "accdb" )  dataBase <- odbcConnectAccess2007( access.file=db )
  # Grab the logbook data
  df <- sqlFetch( channel=dataBase, sqtable=data, nullstring=0 )
  # Message re logservation data
  cat( "Imported data table with", nrow(df), 
       "rows and", ncol(df), "columns\n" )
  # Close the connection
  odbcCloseAll( )
  # Return the data tables
  return( df )
}  # End GrablogsDat function


####################################
# Data Extraction

# Read in the data
locDB <- "M:/Shellfish Habitat.mdb" 
tbl <- "Shellfish_Habitat_1"
df <- GrabSurveyDat( db=locDB,  data=tbl )

head(df)

