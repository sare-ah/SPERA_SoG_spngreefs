####################################################################################################################
# Shellfish Fisheries within the Strait of Georgia Sponge Reef Complexes
# 
# Objective:  Select fishing events that cross over the sponge reef polygon   
#
# Summary:    For each fishery of interest - access logbook data, read fishing events, build shapefile, clip fishing
#             events by the sponge reef complex
# 
# Fisheries:  Crab by trap, Euphausiid, Geoduck & Horseclam by dive, Goose barnacle, Octopus by dive, Octopus by trap, 
#             Opal squid, Prawn & shrimp by trap, Scallop by dive, Scallop by trawl, Shrimp trawl, Tanner crab
#
# Note:       32 bit R needed to read MS Access database
#             Working directory and location of databases are hard coded into the script
#             Script builds SHP, currently R cannot write to GDB
#
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       November, 2016
###################################################################################################################

###################
### Start Fresh ###
###################

rm(list=ls())

getwd()
setwd("~/R/Habitat Mapping/Footprints")

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
UsePackages( pkgs=c("RODBC","rgdal","sp", "dplyr", "sqldf", "maptools", "data.table") ) 

#################
### Functions ###
#################

# Grab the fishing events
GrabFishingDat <- function( db, logs, clean=TRUE ) {
  # Extract the data base extension
  dbExt <- strsplit( x=basename(db), split=".", fixed=TRUE )[[1]][2]
  # Connect to the data base depending on the extension
  if( dbExt == "mdb" )  dataBase <- odbcConnectAccess( access.file=db )
  if( dbExt == "accdb" )  dataBase <- odbcConnectAccess2007( access.file=db )
  # Grab the logbook data
  fe <- sqlFetch( channel=dataBase, sqtable=logs )
  # Message re logservation data
  cat( "Imported logbook table with", nrow(fe), 
       "rows and", ncol(fe), "columns" )
  # Close the connection
  odbcCloseAll( )
  # Return the data tables
  return( fe )
}  # End GrablogsDat function

# Remove rows with NA values in specific columns within a dataframe
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#########################################
#### Build Fisheries with Point data ####
#########################################

# Parameters for each fishery
name <- c( "Crab","Euphausiid","Octopus_dive","Octopus_trap","Opal_squid","Prawn","Scallop_dive","Scallop_trawl","Shrimp_trawl" )
# sf_log_db$ is mapped to the L:/ drive
# sf_bio_db$ is mapped to the K:/ drive
database <- c( "L:/CrabLogs.mdb","L:/EuphausiidLogs.mdb","L:/OctopusDiveLogs.mdb","L:/OctopusTrapLogs.mdb","L:/Squid_ZLogs.mdb",
               "L:/PrawnLogs.mdb","L:/ScallopDiveLogs.mdb","L:/ScallopTrawlLogs.mdb","L:/ShrimpTrawlLogs.mdb" ) 
records <- c( "Logbook","Logbook","Logbook","Logbook",
              "Logbook","Catch","Logbook","Logbook","Logbook" )
degree.lower <- c( TRUE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE )
tdate <- c( FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE ) 
degree.upper <- c( FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE )
degree.long <- c( FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE )

# Create a dataframe to hold the unique set of parameters for each fishery
fisheries <- data.frame( name, database, records, degree.lower, tdate, degree.upper, degree.long )
fisheries$name <- as.character(fisheries$name)
fisheries$database <- as.character(fisheries$database)
fisheries$records <- as.character(fisheries$records)

# Loop through dataframe, read in fishing events, correct formatting, write shapefile
for (i in 1:nrow(fisheries)){
  # Assign parameters
  fishery <- fisheries[i,1]
  locDB <- fisheries[i,2]
  fishingEvents <- fisheries[i,3]
  cat(fishery, " fishery ...")
  # Read in fishing events
  fe <- GrabFishingDat( db=locDB, logs=fishingEvents )
  #head(fe)
  # Correct for difference in lat/long/year formatting
  if ( fisheries$degree.lower[i] == TRUE ){
    fe$lon <- ( (fe$long_deg) + (fe$long_min/60) )
    fe$lon <- ( fe$lon*-1 )
    fe$lat <- ( (fe$lat_deg) + (fe$lat_min/60) )
  }
  if ( fisheries$tdate[i] == TRUE ){
    fe$year <- format(fe$tdate, "%Y")
  }
  if ( fisheries$degree.upper[i] == TRUE ){
    fe$lon <- fe$Lon
    fe$lat <- fe$Lat
    fe$Lon <- NULL
    fe$Lat <- NULL
  }
  if ( fisheries$degree.long[i] == TRUE ){
    fe$lon <- ( (fe$Long_deg) + (fe$Long_min/60) )
    fe$lon <- ( fe$lon*-1 )
    fe$lat <- ( (fe$Lat_deg) + (fe$Lat_min/60) )
  }
  # Remove rows without lat/lon
  fe <- completeFun( fe,c("lon", "lat") )
  cat(" ...")
  # Assign coordinate fields and create Large SpatialPoints DataFrame
  coordinates(fe) <- c( "lon", "lat" )  # set spatial coordinates
  # Define projection
  crs.geo <- CRS( "+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs" ) # geographical, datum NAD83
  proj4string(fe) <- crs.geo  # define projection system of our data
  # Write shapefile using writeOGR function
  # Field names will be abbreviated for ESRI Shapefile driver
  writeOGR( fe, dsn='.', layer=fishery, driver="ESRI Shapefile" )
  rm(fe)
  cat(" Created shapefile ...\n" )
}



####################################
#### Manipulate Groundfish data ####
####################################


grdfsh <- read.csv("F:/SPERA/SoG sponge manuscript/Data/FishingEvents/SoG_FishingEvents.csv")

grdfsh <- completeFun(grdfsh, c("START_LATITUDE","START_LONGITUDE","END_LATITUDE","END_LONGITUDE"))

grdfsh$START_LONGITUDE <- grdfsh$START_LONGITUDE*-1
grdfsh$END_LONGITUDE <- grdfsh$END_LONGITUDE*-1

# Reset the rownames because the subset of data does not match the number of records
rownames(grdfsh) <- NULL

# Extract start & end coordinates
begin.coord <- data.frame(lon=grdfsh$START_LONGITUDE, lat=grdfsh$START_LATITUDE)
end.coord <- data.frame(lon=grdfsh$END_LONGITUDE, lat=grdfsh$END_LATITUDE)

# Remove boogus values, Longitude greater than -180 degrees
grdfsh <- filter( grdfsh, END_LONGITUDE>= -180)
grdfsh <- filter( grdfsh, END_LONGITUDE<= -120)
grdfsh <- filter( grdfsh, START_LONGITUDE<= -120)
summary(grdfsh)

# Build list of coordinate pairs
sl <- vector("list", nrow(grdfsh))
for (i in seq_along(sl)) {
  sl[[i]] <- Lines(list(Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
}

# Build a spatial line object
set.lines <- SpatialLines(sl)

# Build a SpatialLinesDataFrame to attach the attribute data to
Sldf <- SpatialLinesDataFrame( set.lines, data=grdfsh)

# Define projection
crs.geo <- CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" ) 
proj4string(Sldf) <- crs.geo  # define projection system of our data

# Transform to Albers
albers <- paste( "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126", 
                  "+x_0=1000000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs", sep=" " )
Sldf <- spTransform( Sldf, CRS(albers)) 

# Write shapefile using writeOGR function
# Field names will be abbreviated for ESRI Shapefile driver
dsn <- "C:/Users/daviessa/ocuments/ArcGIS/"
writeOGR( Sldf, dsn=dsn, layer="Grdfsh_FE", driver="ESRI Shapefile", overwrite_layer=TRUE )
cat(" ... Created shapefile ...\n" )




write.csv(grdfsh, "F:/SPERA/SoG sponge manuscript/Data/FishingEvents/SoG_FishingEvents_clean.csv", row.names=TRUE )

######################################
#### Clip with Reef SHP in ArcMap ####
######################################


######################################
#### Summarize FE on Sponge Reef  ####
######################################

# Read in data
crab <- readOGR("C:/Users/daviessa/Documents/ArcGIS/SoG_ReefFish", "Crab_clip")
crab <- crab@data
summary(crab)

prawn <- readOGR("C:/Users/daviessa/Documents/ArcGIS/SoG_ReefFish", "Prawn_clip")
prawn <- prawn@data
summary(prawn)
df <- prawn
prawn <- df

# Remove records earlier than 2010
crab <- filter( crab, year>2010 )
prawn$year <- as.character(prawn$year)
prawn$year <- as.integer(prawn$year)
prawn <- filter( prawn, year>2010 )

write.csv(crab, "F:/SPERA/SoG sponge manuscript/Data/FishingEvents/crab_reef.csv")

write.csv(prawn, "F:/SPERA/SoG sponge manuscript/Data/FishingEvents/prawn_reef.csv")

dtc <- data.table(crab, key="year")
dtc[,length(ID),length(cfv), by=key(dt)]

dt <- data.table(prawn, key="year")
dt[,.N,by=key(dt)]


crab %>% 
  group_by(year) %>% 
  mean(crab$soak_tm)


shellfish <- list(crab,prawn)

for i to length(shellfish){
  fish <- as.data.frame(shellfish[i])
  # Select data for years 2011 to 2016
  fish <- filter( fish, year > 2010)
  fish %>% 
    group_by(year) %>% 
    unique(fish$cfv)
    df <- ddply(fish, "year", summarise, 
                          fishEvents = nrow(ID),
                          noVessels = length(unique(cfv)))
                       
  
  yr <- length(unique(fish$year))
  cfv <- length(unique(fish$cfv))
  # count number of rows
  rcrds <- nrow(fish)
  
}

### Number of Trips per year
yr <- length(unique(prawn$year))
yr

### Number of Vessels per year
cfv <- length(unique(crab$cfv))

### Number of Fishing Events per year








