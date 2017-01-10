############################################################################################
# SPERA - Sponge reef condition
#   Cruise Pac2013-070 - December 2013
#   Cruise Pac2012-068 - September 2012
# 
# Objective:  Create a navigation dataset and point shp for times On-the-Reef          
#
# Summary:    Create a timeseries of seconds on the reef polygon using the In/Out times; 
#             join this timeseries with the entire.nav data to create reef.nav; 
#             build a point shapefile of nav positions On-The-Reef
#
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       March, 2016
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

# Make packages available
UsePackages( pkgs=c("rgdal","sp","reshape2","plyr","dplyr","tidyr","Hmisc") ) 

############ Read in data ###########################################################

# Set cruise
cruise <- "Pac2013-070"
#cruise <- "Pac2012-068"

# 1. On Reef Transect timeseries (from In/Out dive times)
################################################################
cruise <- "Pac2013-070"
#cruise <- "Pac2012-068"

if (cruise == "Pac2013-070") {
  # For 2013 Elise has determined the In/Out Polygon timestamps based on lights flickering on video
  # Built a table of Transect|In| StartT|EndT from Elise's xls
  times.reef <- read.csv("InOutPolygonTimes.csv", header=TRUE)
}

# TO DO: Add in code for 2012-068

# Remove any columns that are full of NA's --- if necessary
# The following line will keep only columns where the number of rows is greater than 
# the number of rows with NA's, this will eliminate excess columns X.1, X.2, X.3
times.reef <- times.reef[,colSums(is.na(times.reef))<nrow(times.reef)]
head(times.reef)

# Combine and format a date time fields for both Start Time and End Time
times.reef$StartDate <- as.Date(times.reef$StartDate, format="%d/%m/%Y")
times.reef$EndDate <- as.Date(times.reef$EndDate, format="%d/%m/%Y")
times.reef$Start <- strptime(paste(times.reef$StartDate, times.reef$StartTime),format="%Y-%m-%d %H:%M:%S", tz="GMT" ) 
times.reef$End <- strptime(paste(times.reef$EndDate, times.reef$EndTime),format="%Y-%m-%d %H:%M:%S", tz="GMT" )
times.reef$Start <- as.POSIXct(times.reef$Start) 
times.reef$End <- as.POSIXct(times.reef$End) 


# 2. Navigation data (Hypack)
################################################################
# Read in navigation data
#### Change file to correct cruise #### 
entire.nav <- read.csv( "Pac2013-070_nav_data_R_interpolated_Mar4.csv", header=TRUE )
# Remove extra columns & dives not included in analysis (if needed)
entire.nav <- select( entire.nav, c(dive,date,DateTime,x,y,lat_dd,lon_dd,depth) )
# Rename columns --- rename(df, new.name = old.name)
entire.nav <- rename( entire.nav, Transect=dive, UTM_x=x, UTM_y=y )
# Convert to integer, if needed
entire.nav$Transect <- as.integer(entire.nav$Transect)
# Remove date field
entire.nav$date <- NULL
# Convert DateTime from a factor into POSIXct
entire.nav$DateTime <- as.POSIXct(entire.nav$DateTime, format="%Y-%m-%d %H:%M:%S", tz="GMT")
# Reorder by transect and timestamp
entire.nav <- entire.nav[ order(entire.nav$Transect, entire.nav$DateTime), ]

# Pac2013-070 ### Remove dives that are not part of the analysis (Dive 0, 1, 30, & 99)
if (cruise == "Pac2013-070") {
  entire.nav <- filter( entire.nav, Transect!='0' & Transect!='1' & Transect!='15' & Transect!='30' & Transect!='36' & Transect!='37' )
}
head(entire.nav)

# Write out entire navigation data
filename <- paste("C:/Users/daviessa/Documents/SPERA/SoG sponge manuscript/Data/Data4Prep/",cruise,"_navEntire.csv",sep="")
write.csv(entire.nav, filename)

##### Known issues with nav & video miner data match-up for Pac2013-070 ##### 
# Dive 0  - Practice dive at beginning of the cruise
# Dive 1  - Maps to Parksville reef, but there no video miner observations for dive 1
# Dive 15 - Tracking timestamps do not match video miner observations, one is completely off 
# Dive 13 - DiveDate is recorded as Dec 7th in Dive log, however this is not possible becaude the timestamps overlap with Dive 8 - DiveDate changed to Dec 8th in InOutPolygonTimes.csv
# Dive 24 - DiveDate is recorded as Dec 9th in Dive log, however this is not possible because the timestamps overlap with Dive 21 - DiveDate changed to Dec 10th in InOutPolygonTimes.csv
# Dive 30 - Extra dive to collect photos, not a straight transect
# Dive 36 - Tracking crosses the reef complex however video miner observations start at 23:02:40 and end at 23:03:20 - almost immediately 
# Dive 37 - Removed from analysis because of comments in the dive log
# "CommentDive - extra dive to take pretty photos and video, cameras tilted up and down a lot. Water quality wasn't that great, but we spent a lot of time taking zoomed in photos of sponges anyway."



############ Build new nav dataset for On-The-Reef ################################################

# 1. Build timeseries dataframe for InPoly times (times.reef)
################################################################
# This timeseries will consist of only timestamps that are On-The-Reef, based on times.reef.csv
# Frequency = 1 second
# Build an empty dataframe to enter transect, DateTime timestamps, and transect part into
newTimes <- data.frame(Transect=integer(0), DateTime=as.Date(character()), Part=integer())

# Print out the number of rows from times.reef dataframe --- number of times needed to loop thru dataframe
cat( nrow(times.reef), " rows ...\n")

# Loop thru the times.reef df, build a time series, and write to a new dataframe 
for ( i in 1:nrow(times.reef)) {
  # Create variable for Transect name and Transect part 
  tName <- times.reef$Transect[i]
  tPart <- times.reef$Part[i]
  # Determine start and end times
  time.min <- times.reef$Start[i]
  time.max <- times.reef$End[i]
  # Build time series
  timeSeq1sec <- seq( time.min, time.max , by="secs" )
  # Build a new dataframe with variables and time series
  newRows <- data.frame( Transect=tName, DateTime=timeSeq1sec, Part=tPart )
  # Populate the final time series dataframe for study area
  newTimes <- rbind( newTimes, newRows )
  cat( "... row ", i )
}
cat( "\n")
rm(newRows)
tail(newTimes)
 
# Create a polyline field to denote transect name and part
newTimes$Polyline <- paste(newTimes$Transect,times$Part, sep="_")


# 2. Merge new timeseries with entire nav dataset
################################################################
# Merge timeseries and entire nav data, retain all timeseries records, but not all nav data records
# Set DateTime as character before the join
entire.nav$DateTime <- as.character(entire.nav$DateTime)
newTimes$DateTime <- as.character(newTimes$DateTime)
# Join all newTimes records to matching entire.nav records by Transect & DateTime
reef.nav <- left_join( x=newTimes, y=entire.nav, by=c("Transect","DateTime") )
# Ensure longitude values are negative
head(reef.nav)
reef.nav$lon_dd <- (reef.nav$lon_dd*-1)
summary(reef.nav)

# Remove records that have no lat/lon values
# They represent timestamps On-The-Reef with no hypack data available
reef.nav <- na.omit(reef.nav)

# Write out reef navigation data
filename <- paste("C:/Users/daviessa/Documents/SPERA/SoG sponge manuscript/Data/Data4Prep/",cruise,"_navReef.csv",sep="")
write.csv(reef.nav, filename)


# 3. Build a shapefile of reef.nav dataset
################################################################
# set spatial coordinates
coordinates(reef.nav) <- c("UTM_x", "UTM_y") 
# geographical projection, datum NAD83 
crs <- CRS("+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") 
# define projection
proj4string(reef.nav) <- crs
layer <- paste(cruise,"_ReefNav",sep="")
writeOGR(reef.nav, dsn="E:/GIS/SPERA/SoG - sponge health/SHP", layer=layer, driver="ESRI Shapefile" )

cat("...built ",layer," shapefile...")


