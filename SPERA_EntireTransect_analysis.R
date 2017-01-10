############################################################################################
# SPERA - Sponge reef condition
#   Cruise Pac2013-070 - December 2013
#   Cruise Pac2012-068 - September 2012
# 
# Objective:  Anaylysis of all data collected during the ROV dives 
#                      
#
# Summary:    
# 
#  
#
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       March, 2016
############################################################################################

# start fresh
rm(list=ls())

setwd("~/SPERA/SoG sponge manuscript/Data/Data4Analysis/")

Sys.getenv("R_ARCH")   
# "/i386" 32 bit R --- which is necessary to grab data from MS Access database
# "/64"   64 bit R

################ Functions ##################################################################

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

# Remove rows with NA values in specific columns within a dataframe
completeFun <- function(data, desiredCols) {
  # completeFun( dataframe, ("column name") )
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# Make packages available
UsePackages( pkgs=c("RODBC","rgdal","sp","reshape2","ggplot2","plyr","dplyr","tidyr","Hmisc","stringr","data.table") ) 

# 1. Read in data ##############################################
################################################################
cruise <- "Pac2013-070"
#cruise <- "Pac2012-068"

if (cruise == "Pac2013-070") {
  obs3 <- read.csv("Pac2013-070_obs3.csv")
}

# TO DO: Add in code for 2012-068

# Set timestamp to POSIX and Greenwich Mean Time
obs3$DateTime <- as.POSIXct(obs3$DateTime, format="%Y-%m-%d %H:%M:%S", tz="GMT" )

# 2. Identify Orange Heterchone Locations
#############################################
# Determine and plot locations where comments were made regarding orange coloured sponges
# Reviewed comments in Access and need to extract the records where comments included
# the word ORANGE
# Not all comments about orange heterchone lined up with species observations 
# of Heterchone....

# Extract records with ORANGE in Comment field
# grepl() searches for a pattern 'orange' in the field Comment
orng <- filter(obs3, grepl('orange',Comment))
head(orng)

# Build a shapefile of orange observations
orng <- completeFun( orng, c("UTM_x", "UTM_y") )
coordinates(orng) <- c("UTM_x", "UTM_y") 
# UTM zone 10
crs.geo <- CRS("+proj=utm +zone=10 ellps=WGS84") 
# define projection
proj4string(orng) <- crs.geo
filename <- paste(cruise,"orangeHeterchoneObs", sep="_")
writeOGR(orng, dsn="E:/GIS/SPERA/SoG - sponge health/SHP", layer=filename, driver="ESRI Shapefile" )


