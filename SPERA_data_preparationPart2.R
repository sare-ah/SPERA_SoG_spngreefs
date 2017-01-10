############################################################################################
# SPERA - Sponge reef condition
#   Cruise Pac2013-070 - December 2013
#   Cruise Pac2012-068 - September 2012
# 
# Objective:  Create two complete datasets with video observations & navigation data                     
#
# Summary:    Read in video miner obs and make any corrections needed to the dataset
#             Read in reef lut and reef.nav dataset
#             Merge video miner and reef.nav dataset
#             Remove all records not On-The-Reef and match transects to reef complex
#
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       November, 2015
#
############################################################################################

# start fresh
rm(list=ls())

setwd("~/SPERA/SoG sponge manuscript/Data/Data4Prep/")

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
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# Make packages available
UsePackages( pkgs=c("rgdal","sp","reshape2","plyr","dplyr","tidyr","Hmisc","stringr","data.table") ) 

############ Read in data #######################################################

# 1. Video Miner data
################################################################
# Read in video miner data table
# I had problems with the 24hr clock when reading in obs1 from the MS Access db, 
# so I would export to XLS, format date & time, save as .csv

# set cruise
cruise <- "Pac2013-070"
# cruise <- "Pac2012-068"

if (cruise == "Pac2013-070") {
  obs1 <- read.csv("Pac2013-070_data.csv")
}

if (cruise == "Pac2012-068") {
  obs1 <- read.csv("Pac2012-068_data.csv")
}

# Clean-up the data 
obs1$TransectDate <- as.Date(obs1$TransectDate)  # Convert to date format
obs1$CruiseName <- as.factor(cruise)  # Add a cruise field
obs1$SpeciesName <- capitalize( tolower( obs1$SpeciesName ) ) # Standardize format for species names
colnames(obs1)[colnames(obs1)=="TransectName"] <- "Transect"  # Change a column name
# Remove unnecessary rows
obs1 <- select( obs1, -c(Auto,TimeCode,TextTimeDecimal,TimeSource,Side,Range,Length,Width,X,Y,Z,ScreenCaptureName,FileName,ElapsedTime,ReviewedDate, ReviewedTime))

#####  Clean-up errors found in the data #####  
##### Once this analysis is complete this list of known issues will be provided to J.Pegg & L. Barton to update survey dbs  ##### 
# 6BE --- does not exist
# PARALITHODES CAMMTSCHATICA --- correct spelling is PARALITHODES CAMTSCHATICA
#                            --- valid name is Paralithodes camtschaticus
# Pac2013-070 
# Recode Octopus 97A (order) to Octopus 98D (spp)
# Recode Pallid urchin to Strongylocentrotus pallidus
# Recode North pacific spiny dogfish to Squalus suckleyi
# Records in several dives have 'ReviewedDate' input into the 'TransectDate' field
# ****** Need to decide what to do with SpeciesID = 848 (NO FISH IN SAMPLE) ******
# ****** Need to confirm if there is Rockfish = 389 & Rockfish = 388 ******
if (cruise == "Pac2013-070") {
  obs1$SpeciesID <- as.character(obs1$SpeciesID)
  obs1$SpeciesID[obs1$SpeciesID=="97A"] <- "98D"
  obs1$SpeciesID <- as.factor(obs1$SpeciesID)
  obs1$SpeciesName[obs1$SpeciesName=="Pallid urchin"] <- "Strongylocentrotus pallidus" 
  obs1$SpeciesName[obs1$SpeciesName=="North pacific spiny dogfish"] <- "Squalus suckleyi"
  obs1$TransectDate[obs1$Transect=='2'] <- "2013-12-05"
  obs1$TransectDate[obs1$Transect=='3'] <- "2013-12-05"
  obs1$TransectDate[obs1$Transect=='4'] <- "2013-12-05"
  obs1$TransectDate[obs1$Transect=='5' & obs1$TransectDate=="2014-12-05"] <- "2013-12-05"
}

### TO DO ###
# Add code similar to above to handle any edits necessary for Pac2012-068

# Format DateTime field to correct DateTime format and character data type (to allow for the join to work)  
obs1$DateTime <- strptime( paste( obs1$TransectDate, obs1$TextTime ), format="%Y-%m-%d %H:%M:%S", tz="GMT" ) 
obs1$DateTime <- as.character(obs1$DateTime)

head(obs1)


# 2. Reef look-up table
################################################################
# Transect number (both cruises) & reef complex 
reef.lut <- read.csv("Reef_complexes.csv", header=TRUE )
# Subset for cruise of interest
reef.lut <- reef.lut[ which( reef.lut$CruiseName==cruise ),]
summary(reef.lut)

# Determine which reef each transect belongs to
obs1$Reef <- reef.lut[match(obs1$Transect, reef.lut$Transect),"Reef"]

# 3. Nav datasets  for timestamps On-The-Reef
################################################################
# Read in updated times.nav
if (cruise == "Pac2013-070") {
  reef.nav <- read.csv("Pac2013-070_navReef.csv", header=TRUE)
  entire.nav <- read.csv("Pac2013-070_navEntire.csv", header=TRUE)
}

if (cruise == "Pac2012-068") {
  reef.nav <- read.csv("Pac2012-068_navReef.csv", header=TRUE)
  entire.nav <- read.csv("Pac20132-068_navEntire.csv", header=TRUE)
}

# Convert DateTime to POSIX format for Greenwich Mean Time
reef.nav$DateTime <- as.character(reef.nav$DateTime)
entire.nav$DateTime <- as.character(entire.nav$DateTime)

# 4. Merge nav & obs data to create two new datasets --- (1) On-the-Reef, (2) Entire transect
################################################################
# Clean-up the dataset, remove duplicate columns
obs1$ProjectName <- NULL
obs1$TransectDate <- NULL
obs1$CruiseName <- cruise
obs1$DateTime <- as.character(obs1$DateTime)


# 4A. On-the-Reef
# Find Video Miner InPoly (On-The-Reef) observations
# Create a dataframe with all timestamps associated with an observation that are On-the-Reef
obs2 <- left_join( x=reef.nav, y=obs1, by=c("Transect","DateTime") )

# Clean-up the dataset, remove duplicate columns
obs2$X <- NULL
# Reset the DateTime field to a POSIX data type
obs2$DateTime <- as.POSIXct(obs2$DateTime, format="%Y-%m-%d %H:%M:%S", tz="GMT" )
obs2 <- obs2[ order(obs2$Transect, obs2$DateTime), ]  # order by Transect and DateTime

# Dates may be incorrect if the transect crossed the date line, reset date field based on timestamp
obs1$date <- NULL
obs1$Date <- as.Date(obs1$DateTime)

# Remove dives that are not part of the analysis
# For Pac2013-070 remove Dive 1, 30, 37, & 99
# Dive 1 --- Testing/training dive
# Dive 30 --- extra dive in steep area to look for coral. No luck, mostly steep mud, most surfaces covered with sediment
# Dive 37 --- extra dive to take pretty photos and video, cameras tilted up and down a lot. Water quality wasn't that great, but we spent a lot of time taking zoomed in photos of sponges anyway.
# Dive 99 --- Testing/training dive
if (cruise == "Pac2013-070") {
obs2 <- filter( obs2, Transect!='1' & Transect!='30' & Transect!='99' & Transect!='37') 
}

# For Pac2012-068 remove Dive 1, 18, 20 --- I can't remember why these dives were eliminated
if (cruise == "Pac2012-068") {
  obs2 <- filter( obs2, Transect!='1' & Transect!='18')
}

summary(obs2)
obs2 <- completeFun(obs2, "Polyline")
# Determine which reef each transect belongs to
obs2$Reef <- reef.lut[match(obs2$Transect, reef.lut$Transect),"Reef"]
summary(obs2)

filename <- paste("C:/Users/daviessa/Documents/SPERA/SoG sponge manuscript/Data/Data4Analysis/",cruise,"_obs2.csv",sep="")
write.csv(obs2, filename)

# 4B. Entire Transect
# Find Video Miner InPoly (On-The-Reef) observations
# Create a dataframe with all timestamps associated with an observation that are InPoly
obs3 <- full_join( x=entire.nav, y=obs1, by=c("Transect","DateTime") )

# Clean-up the dataset, remove extra columns
obs3$X <- NULL
# Several of the dates were incorrect, reset date field based on timestamp
obs3$date <- NULL
obs3$Date <- as.Date(obs3$DateTime)

# Determine which reef each transect belongs to
obs3$Reef <- reef.lut[match(obs3$Transect, reef.lut$Transect),"Reef"]
summary(obs3)

filename <- paste("C:/Users/daviessa/Documents/SPERA/SoG sponge manuscript/Data/Data4Analysis/",cruise,"_obs3.csv",sep="")
write.csv(obs3, filename)


