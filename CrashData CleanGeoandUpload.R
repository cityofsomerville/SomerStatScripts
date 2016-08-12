# Script to clean MV crash data from T&I, geocode it, and upload to Socrata

## To-Do
## 1 figure out how to check for changes in the dataset since the last update and geocode those to splice back in. Useful function here might be dplyr::setdiff(y, z)
## 2 upload to socrata server, perhaps appending, maybe doing a full replace if necessary -- dan's uploading script should work here, as opposed to put.


# Section 1: the setup
setwd("//fileshare1/departments2/Somerstat Data/Transpo Infrastructure")

# load packages
library(ggmap)
library(dplyr)
library(tidyr)
library(ggplot2)


# Section 2: grab data, some test language, and clean the data
# load data
d <- read.csv("crash_data_master.csv")

# combine location, city, state for one single goecoding address
dgeo <- unite(d, Address, Location , City, State, sep = ", ", remove = TRUE)
dgeo$Address <- gsub("&", "and", dgeo$Address)        


# Section 3: Geocode new data
# geocode the address field, append lat and lon to the dataset
geo2 <- select(dgeo, Date:Notes) %>%
  slice(2001:3648) %>%
  mutate_geocode(Address, source = "google", messaging = TRUE)

# join the two datasets and write to disk to save while i'm still geocoding
interim <- bind_rows(geo, geo2)
write.csv(interim, file = "interimdata.csv")


# Section 4: upload to socrata


# Section 5: Bonus Mapping Functions
# Where is the center of your map?
ville <- "93 highland ave Somerville MA"

# Map details
somMap <- get_map(location = ville, source = "stamen", maptype = "toner", crop = FALSE, zoom = 14)

# point map
# ggmap(somMap)+
#  geom_point(aes(x = lon, y = lat), data = geo, alpha = .7, color = "maroon", size = 4)

# density map
ggmap(somMap) +
  stat_density2d(
    aes(x = lon, y = lat, fill = ..level..,
        alpha = ..level..),
    size = 2, bins = 10, data = geo,
    geom = "polygon")


## Saving this for later as we start to develop the hexbins
## this requires the package hexbin
## ggplot(homicides, aes(POINT_X, POINT_Y)) + stat_binhex() + scale_fill_gradientn(colours = c("white", "red"), name = "Frequency")

###### Map it! ######
#addresses <- paste(d$Address, "Somerville", "MA", sep=", ")

# Geocodes using the Google engine
#library(ggmap) 
#locs <- geocode(addresses)
#locs2 <- subset(locs, lat != 42.3875968 ) # Takes out the weird ones Google couldn't pin
# I map locs2 because when Google can't find something, it usually puts it int the center of the map
# This throws off the heat maps