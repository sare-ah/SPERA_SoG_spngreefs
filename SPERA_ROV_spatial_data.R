############################################################################################
# SPERA - Sponge reef shapefiles
#   Cruise Pac2013-070 - December 2013
#   Cruise Pac2012-068 - September 2012
# 
# Objective:  Build shapefiles and plots of various sponge observations          
#
# Summary:    Build two shp; (1) for sponge species observations and 
#             (2) for sponge substrate observations, plot shps in pdf format
# 
#  
#
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       February, 2016
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
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# Make packages available
UsePackages( pkgs=c("RODBC","rgdal","sp","reshape2","plyr","dplyr","tidyr","Hmisc","stringr","data.table","geoR") ) 

# 1. Read in data ##############################################
################################################################
cruise <- "Pac2013-070"

if (cruise == "Pac2013-070") {
  obs2 <- read.csv("Pac2013-070_obs2.csv")
}


# 2. Look-up tables ############################################
################################################################
spongeSpp <- read.csv("SpongeSpecies.csv")
spongeID <- unique(spongeSpp$SpeciesID)


# 3. Build species shapefiles ##################################
################################################################
# Grab all species observations, format species names 
speciesObs <- subset( obs2, select=-c(DominantSubstrate, DominantPercent, SubdominantSubstrate, SubdominantPercent) )
speciesObs <- subset( speciesObs, SpeciesName!='NA' & SpeciesName!=' ')

# Select records of dataframe with sponge species observations
spongeObs <- subset( speciesObs, SpeciesID %in% spongeID )
spongeObs$SpeciesName <- as.character(spongeObs$SpeciesName)
sponges <- unique(spongeObs$SpeciesName)
sponges

# Summarise which sponge species were observed on each transect
spng_byTransect <- xtabs( ~Transect+SpeciesName, data=spongeObs )
spng_byTransect

# Summarise which sponge species were observed on each reef
spng_byReef <- xtabs( ~Reef+SpeciesName, data=spongeObs )
spng_byReef 

# Build shapefile for sponge species observation on the reef
spongeObs <- completeFun( spongeObs, c("UTM_x", "UTM_y") )
coordinates(spongeObs) <- c("UTM_x", "UTM_y") 
# UTM zone 10
crs.geo <- CRS("+proj=utm +zone=10 ellps=WGS84") 
# define projection
proj4string(spongeObs) <- crs.geo
filename <- paste(cruise,"spongeObs", sep="_")
writeOGR(spongeObs, dsn="E:/GIS/SPERA/SoG - sponge health/SHP", layer=filename, driver="ESRI Shapefile" )



# 4. Build substrate shapefiles ################################
################################################################
# Build shp for substrate types (Live Sponge, Dead Sponge, Other Substrate(?)) from timestamp points
# TO DO: Need to go back through the data & the protocol and determine if the autofill was used for the habitat
# Look at the datacode field and only map those observations that correspond to an actual key punch.
# ---> Datacode field does not appear to be useful for these two cruises!
# How can we have live sponge species observations amongst the dead sponge substrate type?

live.sponge1 <- subset( obs2, DominantSubstrate=="12" ) 
dead.sponge1 <- subset( obs2, DominantSubstrate=="13" )
live.sponge2 <- subset( obs2, SubdominantSubstrate=="12" ) 
dead.sponge2 <- subset( obs2, SubdominantSubstrate=="13" )
sponge.sets <- list( live.sponge1,dead.sponge1,live.sponge2,dead.sponge2 )

for( i in 1:length(sponge.sets) ){
  object <- as.data.frame(sponge.sets[i])
  # Remove rows without lat/lon
  object <- completeFun( object,c("UTM_x", "UTM_y") )
  # set spatial coordinates
  coordinates(object) <- c("UTM_x", "UTM_y") 
  # geographical projection, datum NAD83
  crs.geo <- CRS("+proj=utm +zone=10 ellps=WGS84") 
  # define projection
  proj4string(object) <- crs.geo
  if (i == 1) {
    filename=paste(cruise,"LiveSpng_dom", sep="_")
  } else if(i ==2) {
    filename=paste(cruise,"DeadSpng_dom", sep="_")
  } else if(i ==3) {
  filename=paste(cruise,"LiveSpng_sub", sep="_")
  } else if(i ==4) {
  filename=paste(cruise,"DeadSpng_sub", sep="_")
  } 
  writeOGR(object, dsn="E:/GIS/SPERA/SoG - sponge health/SHP", layer=filename, driver="ESRI Shapefile" )
}


# 5. Plot shapefiles ################################
#####################################################
# Set working directory to spatial directory
setwd("E:/GIS/SPERA/SoG - sponge health/SHP")

# List all shp files
list.files(pattern = "\\.shp$")

# Read in shp
reefs <- readOGR(".","SpongeReefsGeorgiaBasin")
tracks <- readOGR(".","TimesNavLines_070")
spngs <- readOGR(".","Pac2013-070_spongeObs")
live1 <- readOGR(".","Pac2013-070_LiveSpng_dom")
dead1 <- readOGR(".","Pac2013-070_DeadSpng_dom")

# Check projection files
print(proj4string(reefs))

# Plot
############################################
### Achilles Bank 
############################################

# Achilles zoomed in ... for better resolution
pdf("PDFs/Achilles_substrate1.pdf")
plot(reefs, xlim=c(391900,392400), ylim=c(5487200,5490500), axes=TRUE, border="gray", main="Achilles Bank")
lines(tracks, lty=1)
points(spngs, pch=3, col="chartreuse4",cex=1.5)
# Add dominant live & dead sponge as red & black X's
points(live1, pch=4, col="red")
points(dead1, pch=4, col="dimgray")
# Add legend
legend('left',inset=0.02,legend=c("ROV tracks","Sponge Species Observations", "Live Sponge Substrate",
                                      "Dead Sponge Substrate"), col=c("black","chartreuse4","red","dimgray"), 
       pch=c(95,3,4,4),bg="white")
dev.off()

#  Achilles zoomed in ... just sponge species observations
pdf("PDFs/Achilles_spongespecies.pdf")
plot(reefs, xlim=c(391900,392400), ylim=c(5487200,5488600), axes=TRUE, border="gray", main="Achilles Bank")
# Add sponge species observations as red crosses
lines(tracks, lty=1)
points(spngs, pch=16, col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                            "darkorchid","darkmagenta","burlywood")[as.numeric(spngs$SpecsNm)])
legend('right',inset=0.02,pch=16,col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                                          "darkorchid","rosybrown"),legend=levels(spngs$SpecsNm),bg="white")
dev.off()


############################################
### Active Pass North (Galiano Ridge) 
############################################

# Active Pass North zoomed in ... for better resolution
pdf("PDFs/ActivePassNorth_substrate.pdf")
plot(reefs, xlim=c(475500,477500), ylim=c(5416500,5418000), axes=TRUE, border="gray", main="Active Pass North (Galiano Ridge)")
lines(tracks, lty=1)
points(spngs, pch=3, col="chartreuse4",cex=1.5)
# Add dominant live & dead sponge as red & black X's
points(live1, pch=4, col="red")
points(dead1, pch=4, col="dimgray")
# Add legend
legend('topright',inset=0.02,legend=c("ROV tracks","Sponge Species Observations", "Live Sponge Substrate",
                                      "Dead Sponge Substrate"), col=c("black","chartreuse4","red","dimgray"), 
       pch=c(95,3,4,4),bg="white")
dev.off()


#  Active Pass North zoomed in ... just sponge species observations
pdf("PDFs/ActivePassNorth_spongespecies.pdf")
plot(reefs, xlim=c(476800,477000), ylim=c(5416950,5417100), axes=TRUE, border="gray", main="Active Pass North")
# Add sponge species observations as red crosses
lines(tracks, lty=1)
points(spngs, pch=16, col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                            "darkorchid","darkmagenta","burlywood")[as.numeric(spngs$SpecsNm)])
legend('topright',inset=0.02,pch=16,col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                                          "darkorchid","rosybrown"),legend=levels(spngs$SpecsNm),bg="white")
dev.off()


############################################
### Active Pass South 
############################################

# Active Pass South zoomed in ... for better resolution
pdf("PDFs/ActivePassSouth_substrate.pdf")
plot(reefs, xlim=c(482000,483000), ylim=c(5412500,5413500), axes=TRUE, border="gray", main="Active Pass South")
lines(tracks, lty=1)
points(spngs, pch=3, col="chartreuse4",cex=1.5)
# Add dominant live & dead sponge as red & black X's
points(live1, pch=4, col="red")
points(dead1, pch=4, col="dimgray")
# Add legend
legend('topright',inset=0.02,legend=c("ROV tracks","Sponge Species Observations", "Live Sponge Substrate",
                                      "Dead Sponge Substrate"), col=c("black","chartreuse4","red","dimgray"), 
       pch=c(95,3,4,4),bg="white")

dev.off()


#  Active Pass South zoomed in ... just sponge species observations
pdf("PDFs/ActivePassSouth_spongespecies.pdf")
plot(reefs, xlim=c(482750,482950), ylim=c(5412600,5412800), axes=TRUE, border="gray", main="Active Pass South")
# Add sponge species observations as red crosses
lines(tracks, lty=1)
points(spngs, pch=16, col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                            "darkorchid","darkmagenta","burlywood")[as.numeric(spngs$SpecsNm)])
legend('topright',inset=0.02,pch=16,col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                                         "darkorchid","rosybrown"),legend=levels(spngs$SpecsNm),bg="white")
dev.off()


############################################
### Fraser Ridge 
############################################

# Fraser Ridge zoomed in ... for better resolution
pdf("PDFs/FraserRidge_substrate.pdf")
plot(reefs, xlim=c(471500,472500), ylim=c(5444700,5445300), axes=TRUE, border="gray", main="Fraser Ridge")
lines(tracks, lty=1)
points(spngs, pch=3, col="chartreuse4",cex=1.5)
# Add dominant live & dead sponge as red & black X's
points(live1, pch=4, col="red")
points(dead1, pch=4, col="dimgray")
# Add legend
legend('topright',inset=0.02,legend=c("ROV tracks","Sponge Species Observations", "Live Sponge Substrate",
                                      "Dead Sponge Substrate"), col=c("black","chartreuse4","red","dimgray"), 
       pch=c(95,3,4,4),bg="white")
dev.off()



#  Fraser Ridge zoomed in ... just sponge species observations
pdf("PDFs/FraserRidge_spongespecies.pdf")
plot(reefs, xlim=c(471900,472100), ylim=c(5445000,5445200), axes=TRUE, border="gray", main="Fraser Ridge")
# Add sponge species observations as red crosses
lines(tracks, lty=1)
points(spngs, pch=16, col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                            "darkorchid","darkmagenta","burlywood")[as.numeric(spngs$SpecsNm)])
legend('topleft',inset=0.02,pch=16,col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                                         "darkorchid","rosybrown"),legend=levels(spngs$SpecsNm),bg="white")
dev.off()


############################################
### Howe Sound (Defence Island) 
############################################

# Howe Sound (Defence Island) zoomed in ... for better resolution
pdf("PDFs/HSnd_DefenceIsld_substrate.pdf")
plot(reefs, xlim=c(479500,479900), ylim=c(5489700,5490500), axes=TRUE, border="gray", main="Howe Sound (Defence Island)")
lines(tracks, lty=1)
points(spngs, pch=3, col="chartreuse4",cex=1.5)
# Add dominant live & dead sponge as red & black X's
points(live1, pch=4, col="red")
points(dead1, pch=4, col="dimgray")
# Add legend
legend('topright',inset=0.02,legend=c("ROV tracks","Sponge Species Observations", "Live Sponge Substrate",
                                     "Dead Sponge Substrate"), col=c("black","chartreuse4","red","dimgray"), 
       pch=c(95,3,4,4),bg="white")
dev.off()


#  Howe Sound (Defence Island) zoomed in ... just sponge species observations
pdf("PDFs/HSnd_DefenceIsld_spongespecies.pdf")
plot(reefs, xlim=c(479700,479900), ylim=c(5489700,5490000), axes=TRUE, border="gray", main="Howe Sound (Defence Island)")
# Add sponge species observations as red crosses
lines(tracks, lty=1)
points(spngs, pch=16, col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                            "darkorchid","darkmagenta","burlywood")[as.numeric(spngs$SpecsNm)])
legend('topleft',inset=0.02,pch=16,col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                                          "darkorchid","rosybrown"),legend=levels(spngs$SpecsNm),bg="white")
dev.off()


############################################
### Howe Sound (Passage Island) 
############################################

# Howe Sound (Passage Island) zoomed in ... for better resolution
pdf("PDFs/HSnd_PassageIsld_substrate.pdf")
plot(reefs, xlim=c(476500,478000), ylim=c(5463500,5467200), axes=TRUE, border="gray", main="Howe Sound (Passage Island)")
lines(tracks, lty=1)
points(spngs, pch=3, col="chartreuse4",cex=1.5)
# Add dominant live & dead sponge as red & black X's
points(live1, pch=4, col="red")
points(dead1, pch=4, col="dimgray")
# Add legend
legend('topleft',inset=0.02,legend=c("ROV tracks","Sponge Species Observations", "Live Sponge Substrate",
                                      "Dead Sponge Substrate"), col=c("black","chartreuse4","red","dimgray"), 
       pch=c(95,3,4,4),bg="white")
dev.off()


#  Howe Sound (Passage Island) zoomed in ... just sponge species observations
pdf("PDFs/HSnd_PassageIsld_spongespecies.pdf")
plot(reefs, xlim=c(476500,477000), ylim=c(5464300,5465500), axes=TRUE, border="gray", main="Howe Sound (Passage Island)")
# Add sponge species observations as red crosses
lines(tracks, lty=1)
points(spngs, pch=16, col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                            "darkorchid","darkmagenta","burlywood")[as.numeric(spngs$SpecsNm)])
legend('topright',inset=0.02,pch=16,col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                                          "darkorchid","rosybrown"),legend=levels(spngs$SpecsNm),bg="white")
dev.off()

############################################
### McCall Bank (Sechelt) 
############################################

# McCall Bank (Sechelt) zoomed in ... for better resolution
pdf("PDFs/McCallBank_Sechelt_substrate.pdf")
plot(reefs, xlim=c(451000,451500), ylim=c(5466100,5467100), axes=TRUE, border="gray", main="McCall Bank (Sechelt)")
lines(tracks, lty=1)
points(spngs, pch=3, col="chartreuse4",cex=1.5)
# Add dominant live & dead sponge as red & black X's
points(live1, pch=4, col="red")
points(dead1, pch=4, col="dimgray")
# Add legend
legend('topright',inset=0.02,legend=c("ROV tracks","Sponge Species Observations", "Live Sponge Substrate",
                                     "Dead Sponge Substrate"), col=c("black","chartreuse4","red","dimgray"), 
       pch=c(95,3,4,4),bg="white")

dev.off()


#  McCall Bank Sechelt zoomed in ... just sponge species observations
pdf("PDFs/McCallBank_Sechelt_spongespecies.pdf")
plot(reefs, xlim=c(451000,451400), ylim=c(5466700,5466900), axes=TRUE, border="gray", main="McCall Bank (Sechelt)")
# Add sponge species observations as red crosses
lines(tracks, lty=1)
points(spngs, pch=16, col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                            "darkorchid","darkmagenta","burlywood")[as.numeric(spngs$SpecsNm)])
legend('topright',inset=0.02,pch=16,col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                                         "darkorchid","rosybrown"),legend=levels(spngs$SpecsNm),bg="white")
dev.off()



############################################
### McCall Bank North 
############################################

# McCall Bank North zoomed in ... for better resolution
pdf("PDFs/McCallBank_North_substrate.pdf")
plot(reefs, xlim=c(439000,443500), ylim=c(5473000,5476000), axes=TRUE, border="gray", main="McCall Bank North")
lines(tracks, lty=1)
points(spngs, pch=3, col="chartreuse4",cex=1.5)
# Add dominant live & dead sponge as red & black X's
points(live1, pch=4, col="red")
points(dead1, pch=4, col="dimgray")
# Add legend
legend('topleft',inset=0.02,legend=c("ROV tracks","Sponge Species Observations", "Live Sponge Substrate",
                                      "Dead Sponge Substrate"), col=c("black","chartreuse4","red","dimgray"), pch=c(95,3,4,4))

dev.off()


#  McCall Bank North zoomed in ... just sponge species observations
pdf("PDFs/McCallBank_North_spongespecies.pdf")
plot(reefs, xlim=c(440000,443500), ylim=c(5475000,5475300), axes=TRUE, border="gray", main="McCall Bank North")
# Add sponge species observations as red crosses
lines(tracks, lty=1)
points(spngs, pch=16, col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                            "darkorchid","darkmagenta","burlywood")[as.numeric(spngs$SpecsNm)])
legend('topleft',inset=0.02,pch=16,col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                                          "darkorchid","rosybrown"),legend=levels(spngs$SpecsNm),bg="white")
dev.off()



############################################
### McCall Bank South 
############################################

# McCall Bank South zoomed in ... for better resolution
pdf("PDFs/McCallBank_South_substrate.pdf")
plot(reefs, xlim=c(446050,449000), ylim=c(5467800,5469500), axes=TRUE, border="gray", main="McCall Bank South")
lines(tracks, lty=1)
points(spngs, pch=3, col="chartreuse4",cex=1.5)
# Add dominant live & dead sponge as red & black X's
points(live1, pch=4, col="red")
points(dead1, pch=4, col="dimgray")
# Add legend
legend('topright',inset=0.02,legend=c("ROV tracks","Sponge Species Observations", "Live Sponge Substrate",
                                      "Dead Sponge Substrate"), col=c("black","chartreuse4","red","dimgray"), pch=c(95,3,4,4))
dev.off()


#  McCall Bank South zoomed in ... just sponge species observations
pdf("PDFs/McCallBank_South_spongespecies.pdf")
plot(reefs, xlim=c(448400,448900), ylim=c(5468400,5468700), axes=TRUE, border="gray", main="McCall Bank South")
# Add sponge species observations as red crosses
lines(tracks, lty=1)
points(spngs, pch=16, col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                            "darkorchid","darkmagenta","burlywood")[as.numeric(spngs$SpecsNm)])
legend('topright',inset=0.02,pch=16,col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                                          "darkorchid","rosybrown"),legend=levels(spngs$SpecsNm),bg="white")
dev.off()






############################################
### Nanaimo 
############################################

# Nanaimo zoomed in ... for better resolution
pdf("PDFs/Nanaimo_substrate.pdf")
plot(reefs, xlim=c(441800,442400), ylim=c(5452200,5452900), axes=TRUE, border="gray", main="Nanaimo")
lines(tracks, lty=1)
points(spngs, pch=3, col="chartreuse4",cex=1.5)
# Add dominant live & dead sponge as red & black X's
points(live1, pch=4, col="red")
points(dead1, pch=4, col="dimgray")
# Add legend
legend('topright',inset=0.02,legend=c("ROV tracks","Sponge Species Observations", "Live Sponge Substrate",
                                      "Dead Sponge Substrate"), col=c("black","chartreuse4","red","dimgray"), pch=c(95,3,4,4))
dev.off()



# Nanaimo zoomed in ... just sponge species observations
pdf("PDFs/Nanaimo_spongespecies.pdf")
plot(reefs, xlim=c(441800,442200), ylim=c(5452300,5452800), axes=TRUE, border="gray", main="Nanaimo")
# Add sponge species observations as red crosses
lines(tracks, lty=1)
points(spngs, pch=16, col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                            "darkorchid","darkmagenta","burlywood")[as.numeric(spngs$SpecsNm)])
legend('topright',inset=0.02,pch=16,col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                                          "darkorchid","rosybrown"),legend=levels(spngs$SpecsNm),bg="white")
dev.off()


############################################
### Parksville 
############################################
# Overview
plot(reefs, xlim=c(403500,405900), ylim=c(5467000,5468500), axes=TRUE, border="gray", main="Parksville")
# Add sponge species observations as red stars
points(spngs, pch=8, col="red")

# Parksville zoomed in ... for better resolution
pdf("PDFs/Parksville_substrate.pdf")
  plot(reefs, xlim=c(403600,404500), ylim=c(5467850,5468200), axes=TRUE, border="gray", main="Parksville")
  # Add sponge species observations as red crosses
  lines(tracks, lty=1)
  points(spngs, pch=3, col="chartreuse4",cex=1.5)
  # Add dominant live & dead sponge as red & black X's
  points(live1, pch=4, col="red")
  points(dead1, pch=4, col="dimgray")
  # Add legend
  legend('topright',inset=0.02,legend=c("ROV tracks","Sponge Species Observations", "Live Sponge Substrate",
        "Dead Sponge Substrate"), col=c("black","chartreuse4","red","dimgray"), pch=c(95,3,4,4))
dev.off()

# Parksville zoomed in ... just sponge species observations
pdf("PDFs/Parksville_spongespecies.pdf")
plot(reefs, xlim=c(403600,403860), ylim=c(5468070,5468100), axes=TRUE, border="gray", main="Parksville")
# Add sponge species observations as red crosses
lines(tracks, lty=1)
points(spngs, pch=16, col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                           "darkorchid","darkmagenta","burlywood")[as.numeric(spngs$SpecsNm)])
legend('topright',inset=0.02,pch=16,col=c("red","darkorange1","chartreuse4","navy","deepskyblue",
                                          "darkorchid","rosybrown"),legend=levels(spngs$SpecsNm),bg="white")
dev.off()
