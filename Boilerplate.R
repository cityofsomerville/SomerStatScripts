# Boilerplate R Code 
# Created by Daniel Hadley, 2014
# These packages are used at various points: 
# install.packages("reshape2", "plyr", "ggplot2", "ggmap", "scales")


#### Loading Data  #### 
# A nifty trick to load data from your clipboard: 
# d <- read.delim("clipboard")
# Or from CSV: 
# `myData` <- read.csv("C:/mypath/forward/slashes/myData.csv")

# But For this we will use randomly-generated data
# d = my dataframe. I use the letter d instead of something more descriptive because 
# I ended up typing it so often. 
d <- data.frame(col1 = sample(c(1,2), 10, replace = TRUE),
                    col2 = as.factor(sample(10)), col3 = letters[1:10],
                    col4 = sample(c(TRUE, FALSE), 10, replace = TRUE))
# I usually make a "1" column to make tabulations easier
d$Tab <- 1
# This adds a column with Somerville addresses for the mapping portion
d$Address <- sample(c("Highland AVe @ Somerville Ave", "41 Beacon St", "Weird St"), 10, replace = TRUE)
# And Finally I add a date column to the df for dat transformations
d$Date <- sample(c("1/1/2013", "3/20/2013", "6/22/2014"), 10, replace = TRUE) # make column


#### Review your Data ####
View(d) 
summary(d)
names(d)
class(d)
sapply(d[1,],class)
str(d) #number of observations, number of vars, class of variables
head(d, 20)


#### Clean and Transorm your Data ####
# I often need to transorm data that is stored as factor or character to numeric
# For example, if there is a "?" in one cell, it will be stored as non numeric
d$col2Numeric <- as.numeric(as.character(d$col2)) # Transforms to numeric

# Sometimes variables stored as factors have characters that generate unexpected results when as.numeric is applied. 
# Use gsub to remove these characters by removing them (technically, replacing them with nothing).
# For example, use this code if your values look like this: $1,000. For more on gsub see: http://www.endmemo.com/program/R/gsub.php
data$var.clean <- gsub("([/$,])", "", data$var)
data$var.clean <- as.numeric(data$var) #OR, more simply
data$var.clean <- as.numeric(gsub("([/$,])", "", data$var))

# Missing Values
missing <- is.na(d$col1) 
sum(missing) #Number of missing values
sum(!missing) #Number of non-missing values

#OR
good <- complete.cases(d$col1)
sum(!good) #Number of missing values
sum(good) #Number of non-missing values

# Dropping (removing)
remove(good) #object
remove(missing) # also works with a dataframe

# To drop variable/column:
d$col1 <- NULL #column OR
d <- subset(d, select = -c(col1)) #OR

# To drop an observation/row
d <- d[-917,]

# To drop observations with a given value:
# d <- subset(d, col3 %in% c("a","b"))

# To Select an observation
newdata <- d[ which(d$col3=='a' | d$col3 == 'b'), ]

# To reshape data: see: http://www.ats.ucla.edu/stat/r/faq/reshape.htm

# Date
d$Date <- as.Date(d$Date,"%m/%d/%Y") # Tell R it's a date
d$Month <- format(d$Date, format='%m') # Break it into month, day, year...
d$Day <- format(d$Date, format='%d')
d$Year <- format(d$Date, format='%Y')
d$Month <- as.numeric(as.character(d$Month)) # Transform month to numeric for ifelse
# There are more nuanced ways of dividing seasons, but I prefer 4 even periods:
d$Season <- ifelse((d$Month >= 3) & (d$Month <= 5), "Spring", 
                       ifelse((d$Month >= 6) & (d$Month <= 8), "Summer",
                              ifelse((d$Month >= 9) & (d$Month <= 11), "Fall", "Winter")))



# How to switch from Excel: the pivot table
aggregate(col2Numeric ~ Year, d, sum ) # makes a two-way table

# aggregate works for a couple of variables. 
# "Cast" from reshape2 works when you have more than two variables:
# http://marcoghislanzoni.com/blog/2013/10/11/pivot-tables-in-r-with-melt-and-cast/
library(reshape2)
sapply(d[1,],class) #look at these again to see which columns to include
names(d) #look at the names
data.m <- melt(d, id=c(2:4, 5, 9:12), measure=c(8)) # id = non-numeric; measure = numeric
data.c <- dcast(data.m, Year ~ variable, sum)

library(plyr)
d <- d[order(-d$col2Numeric),] # sort it
# just the top 2 most frequent seasons
d.top <- subset(d, Season %in% arrange(count(d, .(Season)), desc(freq))[1:2,]$Season)


# Export
write.csv(d, file = "mydf.csv")


####  Visualize ####
library(ggplot2)
library(scales) # for changing from scientific notation
# Example scale feature: scale_y_continuous(labels = comma) or scale_y_continuous(labels = dollar)

lime_green = "#2ecc71"
soft_blue = "#3498db"
pinkish_red = "#e74c3c"
purple = "#9b59b6"
teele = "#1abc9c"
nice_blue = "#2980b9"


my.theme <- 
  theme(plot.background = element_blank(), # Remove background
        panel.grid.major = element_blank(), # Remove gridlines
        panel.grid.minor = element_blank(), # Remove more gridlines
        panel.border = element_blank(), # Remove border
        panel.background = element_blank(), # Remove more background
        axis.ticks = element_blank(), # Remove axis ticks
        axis.text=element_text(size=24), # Enlarge axis text font
        axis.title=element_text(size=26), # Enlarge axis title font
        plot.title=element_text(size=42, hjust=0) # Enlarge, left-align title
        #,axis.text.x = element_text(angle=60, hjust = 1) # Uncomment if X-axis unreadable 
        )


p <- qplot(Year, data=d, geom="bar", fill=col4, alpha=I(.7), main="Incidents", ylab="Number of Incidents")
p + my.theme + facet_grid(. ~ Season) # Facet grid is the perfect way to add more to your X-axis

# A simple method is to use the "weight" function with qplot. This will even work with aggregate
p <- qplot(Year, weight = col2Numeric, data = data.c, geom = "bar", alpha=I(.7), main="Data By Year", ylab="Col2 Count")
p + my.theme + scale_y_continuous(labels = comma) # scale y is not useful here, but changes from e-notation


###### Map it! ######
addresses <- paste(d$Address, "Somerville", "MA", sep=", ")

# Geocodes using the Google engine
library(ggmap) 
locs <- geocode(addresses)
locs2 <- subset(locs, lat != 42.3875968 ) # Takes out the weird ones Google couldn't pin
# I map locs2 because when Google can't find something, it usually puts it int the center of the map
# This throws off the heat maps

# Contour Map
map.center <- geocode("Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 14)
SHmap + geom_density2d(
  aes(x=locs2$lon, y=locs2$lat,  
      fill = ..level.. , alpha = ..level..),size = 1.5, bins = 26, color="red", 
  data = locs2) 

# Dot map centered on Conway Park
map.center <- geocode("Conway Park, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 16)
SHmap + geom_point(
  aes(x=locs2$lon, y=locs2$lat),size = 10, alpha = .7, bins = 26, color="red", 
  data = locs2) 

# More traditional heat map
Somerville = c(lon = -71.1000, lat =  42.3875)
somerville.map = get_map(location = Somerville, zoom = 14, maptype="roadmap",color = "bw")
ggmap(somerville.map, extent = "panel", maprange=FALSE) %+% locs2 + aes(x = locs2$lon, y = locs2$lat) +
  # geom_density2d(data = locs2, aes(x = lon, y = lat)) + # uncomment for contour lines
  stat_density2d(data = locs2, aes(x = lon, y = lat,  fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))
