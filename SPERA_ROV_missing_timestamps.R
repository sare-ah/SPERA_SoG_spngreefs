############################################################################################
# SPERA - Sponge reef ROV tracking
#   Cruise Pac2013-070 - December 2013
#   Cruise Pac2012-068 - September 2012
# 
# Objective:  Estimate the position and depth for each timestamp          
#
# Summary:    Read in nav data csv, loop through each dive and 
#             interpolate missing timestamps and associated 
#             attributes
#
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       December, 2015
#   Cruise Pac2013-070 - December 2013
#   Cruise Pac2012-068 - September 2012
#
############################################################################################

# start fresh
rm(list=ls())

setwd("~/SPERA/SoG sponge manuscript/Data/Data4Prep")


################ Functions #####################################
################################################################

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
UsePackages( pkgs=c("plyr","dplyr","tidyr","Hmisc","stringr","zoo") ) 

################ Main ##########################################
################################################################

# Read in the nav data and start/end times for transects, format date & time
# confirm date format beforehand...
### Change the next line for the appropriate filename
df <- read.csv("nav data PAC2012-068.csv", sep=",",header=TRUE)
df$date <- as.Date(df$date, format="%d/%m/%Y")
df$time <- as.character(df$time)
df$DateTime <- strptime( paste( df$date, df$time ),format="%Y-%m-%d %H:%M:%S", tz="GMT" ) 
df$DateTime <- as.POSIXct(df$DateTime) # filter() will not work with POSIXlt format
summary(df)  # Check to see if NA's are present in file b/c sometimes the file has blank rows at the end
df <- df[!is.na(df$DateTime),]
str(df)

# Remove columns that will not be used in output (extra)
# This line will change depending on the field names created by Hypack
df <- select(df, select=-c(time_diff,time_char))

# Remove unnecessary dives --- this list will change with each cruise
#df <- filter( df, dive!='Dive_0' & dive!='Dive_01' & dive!='Dive_30' & dive!='Dive_99' )

# Update dive name to a number
# The next three lines assumes that the df$dive format is dive_01 --- Check for each cruise!
df$dive <- as.character(df$dive)
df$dive <- str_sub(df$dive, 6) # subset a string from the sixth position onwards (read left-right)
df$dive <- as.numeric(df$dive)

# Create new, empty output dataframe that will be populated with interpolated data
intrp.nav <- df
intrp.nav <- intrp.nav[0,]

# Determine number of dives and print to screen
dives <- unique(na.omit(df$dive))
cat("...",length(dives)," dives in nav file")

# For each dive ... 
for( i in 1:length(dives) ){
  # Select records for dive[i]
  name <- dives[i]
  cat(" _DIVE_ ",name[])
  # Create a temporary df for dive[i]
  one.dive <- filter( df, df$dive==name )
  # Sort the data by time
  one.dive <- one.dive[order(one.dive$DateTime),]
  # Do we have duplicates timestamps in this dive?
  # Remove duplicates, df has unique timestamps only
  cnt <- length(unique(one.dive$time)) # Determine the number of unique timestamps, for debugging
  one.dive <- one.dive[!duplicated(one.dive$time),]
  # Determine the length of dataset
  data.length <- length(one.dive$DateTime)
  # Find min and max timestamp
  time.min <- one.dive$DateTime[1]
  time.max <- one.dive$DateTime[data.length]
  # Print number of timestamps
  cat("...no.records =", data.length)
  # Generate a time sequence with 1 sec intervals to fill in missing times
  # seq(from=2, to=10, by=2)
  all.times <- seq(time.min, time.max, by="secs")
  # Create a df of the time sequence list
  all.times.df <- data.frame(list(DateTime=all.times))
  cat(" seq() ...")  
  # Convert POSIXct to character in order to use merge() function
  all.times.df$DateTime <- as.character(all.times.df$DateTime)
  one.dive$DateTime <- as.character(one.dive$DateTime)
  # Merge time sequence with nav data
  new.df <- left_join(all.times.df, one.dive, by="DateTime")
  # Return DateTime to POSIXct
  new.df$DateTime <- as.POSIXct( new.df$DateTime, format="%Y-%m-%d %H:%M:%S", tz="GMT" ) 
  # Interpolate missing values
  cat(" interpolate  ...")
  # Approximate positions and depth
  new.df$x <- na.approx(new.df$x)
  new.df$y <- na.approx(new.df$y)
  new.df$lat_dd <- na.approx(new.df$lat_dd)
  new.df$lon_dd <- na.approx(new.df$lon_dd)
  new.df$depth <- na.approx(new.df$depth)
  # Add constants for dive[i] for new records
  # Apply unique dive number to all records with dive number = NA
  new.df$dive <- i
  new.df$N <- 'N'
  new.df$W <- 'W'
  # Document the interpolated records are not original
  new.df$original[is.na(new.df$original)] <- 0
  # Add date field
  new.df$date <- as.Date(new.df$DateTime)
  # Append to new nav dataframe
  cat(" append ...")
  intrp.nav <- merge(intrp.nav, new.df, all=TRUE )
}
cat("...interpolated ",i," dives!")
summary(intrp.nav)

# Write new interpolated nav data to file
### Change the next line for the appropriate filename
write.csv(intrp.nav, file="Pac2013-068_nav_data_R_interpolated_Apr22.csv", row.names=TRUE )

