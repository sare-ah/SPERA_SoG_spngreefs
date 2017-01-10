############################################################################################
# SPERA - Sponge reef condition
#   Cruise Pac2013-070 - December 2013
#   Cruise Pac2012-068 - September 2012
# 
# Objective:  Anaylysis of data on the SoG sponge reef complexes 
#             (tracks beyond the reef have been omitted)          
#
# Summary:    
# 
#  
#
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       November, 2015
#
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
  obs2 <- read.csv("Pac2013-070_obs2.csv")
}

# TO DO: Add in code for 2012-068

# Set timestamp to POSIX and Greenwich Mean Time
obs2$DateTime <- as.POSIXct(obs2$DateTime, format="%Y-%m-%d %H:%M:%S", tz="GMT" )


# 2. Look-up tables & species list #############################
################################################################
# Transect number (both cruises) & reef complex 
reef.lut <- read.csv("C:/Users/daviessa/Documents/SPERA/SoG sponge manuscript/Data/Data4Prep/Reef_complexes.csv", header=TRUE )
# Subset for cruise of interest
reef.lut$Reef <- as.character(reef.lut$Reef)
reef.lut <- reef.lut[ which( reef.lut$CruiseName==cruise ),]
# Remove dives where Reef field is blank; these dives were for sightseeing photos, not analysis
reef.lut <- reef.lut[!reef.lut$Reef == " ",]
reef.lut$Reef <- as.factor(reef.lut$Reef)
summary(reef.lut)

# Sponge species table and create a vector of sponge species names 
# Species list determined by querying the video miner db for sponge species present
spongeSpp <- read.csv("SpongeSpecies.csv", header=TRUE )
spongeID <- as.vector(spongeSpp$SpeciesID)
head(spongeSpp)

# Species list for cruise
allSpp <- data.frame( obs2$SpeciesName, obs2$SpeciesID ) # Grab all species
allSpp <- unique(allSpp) # Create a list of unique species
allSpp[allSpp==""] <- NA # Remove blanks from the list
allSpp <- na.omit(allSpp)

# Ordered species list for output tables using data from both cruises
ordered.spp <- read.csv("All_species_both_cruises.csv", header=TRUE )
ordered.spp$OrderBy <- as.numeric(ordered.spp$OrderBy)


# 3. Transect Length, FoV, Area covered, and speed estimates ###############
############################################################################

### LENGTH ###
# Calculate the transect length InPoly
# In ArcMap convert TimesNav.shp to polyline layer - with multiple parts
# Add field called Length_m and calculate length

# Read in shp with ROV tracks on reef
# Polylines built from the Hypack track points
dsn <- "E:/GIS/SPERA/SoG - sponge health/SHP"
if (cruise == "Pac2013-070") {
  layer <- "Pac2013_070_ReefNavPolyline"
}
if (cruise == "Pac2012-068") {
  layer <- " "
}

# Read in shapefile
polyline <- readOGR( dsn=dsn, layer=layer)
# Assign attribute table to a dataframe
tracks <- polyline@data
head(tracks)
# Change column names
colnames(tracks)[colnames(tracks)=="ET_ID"] <- "Polyline"
tracks$Polyline <- as.character(tracks$Polyline)
# Extract Transect number from the Polyline name
tracks$Transect <- as.numeric(ldply(strsplit(tracks$Polyline, split="_"))[[1]])
# Add reef name, reorder data, and export as csv
tracks$Reef <- reef.lut[match(tracks$Transect, reef.lut$Transect),"Reef"]
tracks <- tracks[, c(5,4,2,3)]
#arrange(tracks, Reef) # another way to order a dataframe by a specific column
tracks[ order(tracks$Reef),]
# Write dataframe to file 
filename <- paste("../AnalysisResults/",cruise,"tracks.csv")
write.csv(tracks, filename)


### Field of View ###
# Calculate field of view, remove NULL Field of View values 
# Field of View is in cm, not meters
# Select all records where FoV is not null
df <- obs2[!is.na(obs2$FieldOfView),]
# Select only the fields of interest
df <- select( df, Transect, DateTime, FieldOfView, Reef )
# Remove multiple records for the same timestamp, due to multiple species observations
df <- unique(df)
# There are still duplicate records for timestamps where FoV measurements changed 
# This was probably caused by a video miner glitch with the auto-populate for Field of View measurements
# For each timestamp we need only one FoV measurement
# Looking at the data it appears that the later value (high row name) is a new measured value
df$rname <- rownames(df)
rownames(df) <- NULL
# Create a data table in order to use functions from the package data.table
dt <- data.table(df, key="DateTime")
# Summarise for maximum rowname at each unique DateTime timestamp, 
# i.e. take dt, select rname and calculate the maximum row name value grouped by DateTime
obs2.fov <- dt[,.SD[which.max(rname)],by=DateTime]

# Use ddply to summarise the FoV data by transect
# With obs2.fov calculate the FoV mean, sd, & counts for each Transect
foView_byDive <- ddply( obs2.fov, "Transect", summarise, 
                        meanFoV.cm = mean(FieldOfView),
                        sdFoV.cm = sd(FieldOfView),
                        cnt.FoV = length(FieldOfView))
foView_byDive

# Can I sum up the FoV this way, or should I summarise the foView_byDive??  Check for differences....
foView_byReef <- ddply( obs2.fov, "Reef", summarise, 
                        meanFoV.cm = mean(FieldOfView),
                        sdFoV.cm = sd(FieldOfView) )
foView_byReef

### Area ###
# Calculate estimated area covered per transect
# Determine the length
area_byDive <- ddply( tracks, "Transect", summarise,
                      Total.Length.m = sum(Length_m) )
# Merge with the width (FoV)
area_byDive <- merge( area_byDive, foView_byDive, by="Transect")
# Calculate the estimated area per dive
area_byDive <- transform( area_byDive, Area.Est.m2 = Total.Length.m * (meanFoV.cm/100) )
area_byDive

area_byReef <- ddply( tracks, "Reef", summarise,
                      No.Transects = length(unique(Transect)),
                      Total.Length.m = sum(Length_m) )
# Merge with foView_byReef summary
area_byReef <- merge( area_byReef, foView_byReef, by="Reef")
# Use transform() to calculate a new field called Area.Est.m2
area_byReef <- transform( area_byReef, Area.Est.m2 = Total.Length.m * (meanFoV.cm/100) )
area_byReef

### Speed ###
# Estimate average speed travelled InPoly 
# Calculate length of time for each transect
speed <- select( obs2, Reef, Transect, DateTime ) 
# Remove multiple records for the same timestamp, due to multiple species observations
speed <- unique(speed)
# Summarise the unique timestamps into elapsed seconds & elapsed minutes
speed <- ddply( speed, "Transect", summarise,
                TimeSecs = length(DateTime),
                TimeMins = (length(DateTime)/60))

# Add distance travelled
speed <- full_join( speed, area_byDive, by="Transect")
# Average estimated speed
speed <- transform( speed, Speed.m.sec = Total.Length.m / TimeSecs)
# Add reef name
speed$Reef <- reef.lut[match(speed$Transect, reef.lut$Transect),"Reef"]
# reorder data
speed <- speed[, c(10,1,2,3,4,5,6,7,8,9)]
speed[ order(speed$Reef),]

### Summary tables ###
# Build summary table by dive
measurements_byDive <- speed[ order(speed$Reef),]
head(measurements_byDive)
filename <- paste("../AnalysisResults/",cruise,"measurements_byDive.csv")
write.csv(measurements_byDive, filename)

# Total time for each reef
time_byReef <- select( speed, Reef, TimeMins )
# With time_byReef group rows by reef and sum the time in minutes
time_byReef <- time_byReef %>% 
  group_by(Reef) %>% 
  summarise(TimeMins = sum(TimeMins))
time_byReef

# Build summary table by reef
measurements_byReef <- merge( area_byReef, time_byReef, by="Reef")
# reorder
measurements_byReef <- measurements_byReef[, c(1,2,7,3,4,5,6)]
filename <- paste("../AnalysisResults/",cruise,"measurements_byReef.csv")
write.csv(measurements_byReef, filename)

### Field of View accuracy check ###
# Are the number of FoV records linked to the number of timestamps?
# Or does the number of FoV records match the number of species obs OR
# is there one record for every 10 secs? or some other time interval?
fov.chk <- full_join(measurements_byDive, foView_byDive, by="Transect")
fov.chk <- transform( measurements_byDive, accuracy.chk = (cnt.FoV / TimeSecs)*100 )
fov.chk
# Compare the fields TimeSecs and cnt.FoV.x
# FoV records are very close for most dives
# Need to determine how much variability is 
# acceptable? Within 10%?

################################################################
################################################################

###########################
# 4. Produce summary tables
###########################

# Data exploration --- Sponge species --- which are documented by species counts and which by abundance?
# Create a subset of sponge observations
spngObs <- filter(obs2, SpeciesID %in% spongeSpp$SpeciesID)
# select relevant records
spngObs <- select(spngObs, Transect, DateTime,SpeciesName,SpeciesID,SpeciesCount,Abundance)

write.csv(spngObs, "spongeObs.csv")
# After examining the above spreadsheet I determined observation types for each species...
# Aphrocallistes - Abundance & some Counts
# Demosponge - Abundance & some Counts
# Heterchone - Abundance & some Counts
# Rhabdocalyptus - Abundance & some Counts
# Sponges - Abundance & some Counts
# Staurocalyptus - Compete --- all records have both Cnts or Abundance
# Stylissa - Abundance & some Counts


# A. Count of SPECIES OBSERVATIONS
###################################
# Reef | Transect | Sponge Species Count

# Determine the counts of sponge species per transect 
# Select species observations
cnt.spng <- select( obs2, DateTime, SpeciesName, SpeciesID, SpeciesCount, Transect, Reef, UTM_x,UTM_y, lat_dd, lon_dd )
# Select species that match sponge list 
cnt.spng <- cnt.spng[cnt.spng$SpeciesID %in% spongeID,]
# To remove other species from factor list convert to a character and then back to factor for xtabs
cnt.spng$SpeciesName <- as.character(cnt.spng$SpeciesName)
cnt.spng$SpeciesName <- as.factor(cnt.spng$SpeciesName)
cnt.spng$SpeciesCount[is.na(cnt.spng$SpeciesCount)] <- 1  # This may need to be corrected to deal with abundance measurements instead

# Build a pivot table
#summarise( group_by( cnt.spng, SpeciesName),Count=sum(SpeciesCount))
spng_byTransect <- aggregate( SpeciesCount ~ Reef + Transect + SpeciesName, data=cnt.spng, FUN=sum )
spng_byTransect <- dcast( spng_byTransect, Reef + Transect ~ SpeciesName, value.var="SpeciesCount", FUN=sum )
spng_byTransect[is.na(spng_byTransect)] <- 0 
spng_byTransect
filename <- paste("../AnalysisResults/",cruise,"_sponge_byTransect.csv",sep="")
write.csv(spng_byTransect, filename)


# B. Total Sponge % Cover SPECIES OBSERVATIONS
###############################################
# Reef|Cnt Trans|Live|Dead|L+D Length|Total Length|% Cover
# Calculate new transect length based on InPolygon timeseries
# How to convert substrate observations into a length?
# Can the timeseries data be converted into a line segments?

# Summary of sponge counts by reef
spng_byReef<- ddply( cnt.spng, "Reef", summarise,
                     No.Transects = length(unique(Transect)),
                     Cnt.Sponges = sum(SpeciesCount) )
spng_byReef

# Summary of transect length
tracks_byReef <- ddply( tracks, "Reef", summarise,
                        No.Transects = length(Transect),
                        Total.Length.m = sum(Length_m) )
tracks_byReef

# Put the two together and reorder
spng_byReef <- merge(tracks_byReef, spng_byReef, by="Reef")
spng_byReef <- spng_byReef[, c(1,2,5,3)]

# Need to add a correction for observation to length....
### NOTE: Assumption of length per observation --> 1 sec = 1/4 m
spng_byReef <- transform( spng_byReef, Spng.length = (Cnt.Sponges) / 4 )
spng_byReef <- spng_byReef[, c(1,2,3,5,4)]
spng_byReef <- transform( spng_byReef, Percent.Cover = (Spng.length / Total.Length.m)*100 )
spng_byReef
filename <- paste("../AnalysisResults/",cruise,"_spongePercentCvr_byReef.csv",sep="")
write.csv(spng_byReef, filename)


# C. Sponge Condition
############################
##### TO DO: Need to change counts into a more appropriate unit of measure
health <- select( obs2, Transect,DateTime,Height,Reef )
# Remove rows with no health condition observations
health <- na.omit(health)
# Video miner added health observations to the field Height, rename to Condition
health$Condition <- health$Height
health$Height <- NULL
# Set Condition as a factor
health$Condition <- as.factor(health$Condition)
# Reassign the values in the field condition to names that make sense
health$Condition <- revalue( health$Condition, c("1"="Undamaged","2"="Mild damage","3"="Moderate damage","4"="Severe damage","212"=NA) )
# Add a new field called count and assume that one health observation equals one count
#### This assumption should probably be discussed and verified with all authors ####
health$Cnt <- 1

# By transect
# Sum health observations by Reef, Transect, and Condition level
health_byTransect <- aggregate(Cnt ~ Reef + Transect + Condition, data=health, FUN=sum)
# Reorganize dataframe to create a column for each Condition 
health_byTransect <- dcast(health_byTransect, Reef + Transect ~ Condition, value.var="Cnt", FUN=sum )
health_byTransect[is.na(health_byTransect)] <- 0 
health_byTransect
filename <- paste("../AnalysisResults/",cruise,"_spongeCondition_byTransect.csv",sep="")
write.csv(health_byTransect, filename)

# By reef
# Further summarise the condition level by creating a crosstab query
health_byReef <- xtabs( ~Reef+Condition, data=health, na.action=na.exclude)
health_byReef
filename <- paste("../AnalysisResults/",cruise,"_spongeCondition_byReef.csv",sep="")
write.csv(health_byReef, filename)


# D. Count of Live vs. Dead Sponge BY SUBSTRATE
#################################################
# Reef | Transect | Live (12) | Dead (13)
# Create shp for live and dead sponge??? 
# Need to understand how and when substrate was keypunched

# Determine the counts of dominant substrate
cnt.dom <- select( obs2, DateTime, DominantSubstrate, SubdominantSubstrate, Transect, Reef, UTM_x,UTM_y, lat_dd, lon_dd )
# Filter for rows where dominant substrate equals live sponge or dead sponge
cnt.dom <- filter(cnt.dom, DominantSubstrate==12 | DominantSubstrate==13)
# Remove duplicates created from multiple species observations at one timestamp
cnt.dom <- unique(cnt.dom)
# Assign new columns to count number of observations
cnt.dom$Live1[cnt.dom$DominantSubstrate==12] <- 1
cnt.dom$Dead1[cnt.dom$DominantSubstrate==13] <- 1
cnt.dom[is.na(cnt.dom)] <- 0

# Summary of counts for dominant sponge substrate
summary.dom <- ddply( cnt.dom, "Reef", summarise,
                      Live1 = sum(Live1),
                      Dead1 = sum(Dead1))
summary.dom

# Determine the counts of subdominant substrate (same as above, just using the subdominant field)
cnt.subdom <- select( obs2, DateTime,DominantSubstrate, SubdominantSubstrate, Transect, Reef, UTM_x,UTM_y, lat_dd, lon_dd )
cnt.subdom <- filter(cnt.subdom, SubdominantSubstrate==12 | SubdominantSubstrate==13)  
cnt.subdom$Live2[cnt.subdom$SubdominantSubstrate==12] <- 1
cnt.subdom$Dead2[cnt.subdom$SubdominantSubstrate==13] <- 1
cnt.subdom [is.na(cnt.subdom )] <- 0

# Summary of counts for subdominant sponge substrate
summary.subdom <- ddply( cnt.subdom, "Reef", summarise,
                      Live2 = sum(Live2),
                      Dead2 = sum(Dead2))
summary.subdom

# Put the dominant & subdominant summaries side-by-side
sponge.substrate <- full_join( summary.dom, summary.subdom, by="Reef")
# Convert NAs to zeros
sponge.substrate[is.na(sponge.substrate)] <- 0
# Merge with tracks by reef to illustrate the different level of effort among the reef complexes
sponge.substrate <- merge(tracks_byReef, sponge.substrate)
sponge.substrate
filename <- paste("../AnalysisResults/",cruise,"spngSubstrate_byReef.csv", sep="")
write.csv(sponge.substrate, filename)

# Combine the dominant and subdominant fields to create only live or dead classifications
# Change column names
cnt.dom$Live <- cnt.dom$Live1  # Dominant = live
cnt.dom$Dead <- cnt.dom$Dead1 # Dominant = dead
cnt.dom$Live1 <- NULL
cnt.dom$Dead1 <- NULL
cnt.subdom$Live <- cnt.subdom$Live2 # Subdominant = live
cnt.subdom$Dead <- cnt.subdom$Dead2 # Subdominant = dead
cnt.subdom$Live2 <- NULL
cnt.subdom$Dead2 <- NULL

# Combine to one large dataframe
dom.subdom <- rbind(cnt.dom, cnt.subdom)
# rm(cnt.dom,cnt.subdom)

# Summarise in a data table by Transect
cntLiveDead_byTransect <- data.table(dom.subdom, key=c("Reef","Transect"))
cntLiveDead_byTransect <- cntLiveDead_byTransect[,list(sum(Live),
                                 sum(Dead)),
                           by=list(Reef,Transect)]
colnames(cntLiveDead_byTransect) <- c("Reef","Transect","Live","Dead")
cntLiveDead_byTransect
filename <- paste("../AnalysisResults/",cruise,"cntLiveDead_byTransect.csv")
write.csv(cntLiveDead_byTransect, filename)
# rm(dom.subdom)

# Summarise in a data table by Reef
cntLiveDead_byReef <- data.table(dom.subdom, key=c("Reef"))
cntLiveDead_byReef <- cntLiveDead_byReef[,list(sum(Live),
                                                 sum(Dead)),
                                                 by=list(Reef)]
colnames(cntLiveDead_byReef) <- c("Reef","Live","Dead")
cntLiveDead_byReef


# What other types of substrate are observed along the sponge reef?
# Dominant Substrate summary
dom.sub <- select( obs2, Reef, Transect, DominantSubstrate )
dom.sub <- completeFun( dom.sub, ("DominantSubstrate") )
dom.sub$DominantSubstrate <- as.factor(dom.sub$DominantSubstrate)
dom.sub$DominantSubstrate <- revalue(dom.sub$DominantSubstrate,
                                 c("1"="Bedrock, smooth", "2"="Bedrock with crevices","3"="Boulders",
                                   "4"="Cobble", "5"="Gravel","7"="Sand","9"="Mud",
                                   "12"="Live Sponge","13"="Dead Sponge"))
# Build a crosstab query
sub_byTransect <- xtabs( ~Transect+DominantSubstrate, data=dom.sub, na.action=na.exclude )
# Replace counts with "X"
# Apply to sub_byTransect a function where if the value is > 1 then set value to X
sub_byTransect <- apply(sub_byTransect, 2, function(x) {x[x > 1] <- "X"; x})
# Apply to sub_byTransect a function where if the value is zero then set value to ""
sub_byTransect <- apply(sub_byTransect, 2, function(x) {x[x == 0] <- ""; x})
# Create a dataframe
sub_byTransect <- as.data.frame(sub_byTransect)
# Add transect number, reef name, reorder data, and export as csv
sub_byTransect$Transect <- rownames(sub_byTransect)
rownames(sub_byTransect) <- NULL
sub_byTransect$Reef <- reef.lut[match(sub_byTransect$Transect, reef.lut$Transect),"Reef"]
sub_byTransect <- sub_byTransect[order(sub_byTransect$Reef),]
sub_byTransect <- sub_byTransect[, c(11,10,1,2,3,4,5,6,7,8,9)]
filename <- paste("../AnalysisResults/",cruise,"_substrate_byTransect.csv",sep="")
write.csv(sub_byTransect, filename)

# Build a crosstab query of dominant substrate type by reef
sub_byReef <- xtabs( ~Reef+DominantSubstrate, data=dom.sub, na.action=na.exclude )
# Replace counts with "X"
sub_byReef <- apply(sub_byReef, 2, function(x) {x[x > 1] <- "X"; x})
# Replace 0's with ""
sub_byReef <- apply(sub_byReef, 2, function(x) {x[x == 0] <- ""; x})
filename <- paste("../AnalysisResults/",cruise,"_substrate_byReef.csv", sep="")
write.csv(sub_byReef, filename)
 


# E. Total Sponge % Cover BY SUBSTRATE
#######################################
# Reef|Cnt Trans|Live|Dead|L+D Length|Total Length|% Cover
# Calculate new transect length based on InPolygon timeseries
# How to convert substrate observations into a length?
# Can the timeseries data be converted into a line segments?

# To include both cruises...
# dives_byReef <- subset( reef.lut, select=c( CruiseName, Reef, Transect ) )
# dives_byReef <- data.table( dives_byReef, key=c("CruiseName","Reef"))
# dives_byReef <- dives_byReef[,list(length(Transect)),
#                  by=list(CruiseName,Reef)]

tracks_byReef <- ddply( tracks, "Reef", summarise,
                        No.Transects = length(unique(Transect)),
                        Total.Length.m = sum(Length_m) )
tracks_byReef

# Reef|Cnt Trans|Live|Dead|L+D Length|Total Length|% Cover
sponge.cover <- merge(tracks_byReef, cntLiveDead_byReef, by="Reef")
# Need to add a correction for observation to length....
# Assumption of length per observation --> 1 sec = 1/4 m
sponge.cover <- transform( sponge.cover, LD.length = (Live + Dead) / 4 )
sponge.cover <- sponge.cover[, c(1,2,4,5,6,3)]
sponge.cover <- transform( sponge.cover, Percent.Cover = LD.length / Total.Length.m )
sponge.cover


# F. Total Sponge Density BY SUBSTRATE
########################################
# Reef|Cnt Trans|Live1|Live2|Dead1|Dead2|Area???
# Area estimated on a reef level from transect length and FoV estimates
# How dense or patchy is the sponge cover
##########################
# Summarise 1' and 2' substrate percentage cover
# Provide a mechanism to compare sponge complexes within the SoG
sponge.density <- join_all(list(area_byReef, summary.dom, summary.subdom), by="Reef", type="full")
sponge.density <- sponge.density[, c(1,2,7,8,9,10,6)]
sponge.density

# D. Live Sponge Proportion BY SUBSTRATE
##########################################
# 12(1) / (12(1)+13(1))
# Reef|Cnt Trans|12|12+13|Proportion
sponge.proportion <- sponge.cover
sponge.proportion$Sponge <- sponge.proportion$Live + sponge.proportion$Dead
sponge.proportion <- transform( sponge.proportion, Proportion = Live / Sponge )
sponge.proportion$LD.length <- NULL
sponge.proportion$Total.Length.m <- NULL
sponge.proportion$Percent.Cover <- NULL
sponge.proportion$Dead <- NULL
sponge.proportion 

