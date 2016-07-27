#### Created 8/15 by Daniel Hadley to load and analyze 311 Data ####
# Data comes from the final daily pull from the Intelligov Server
# (The FTP site was created by Ahmod at Intelligov)
# And the most recent QScend API data
# R 3.1+ is needed for this
# If you don't have these packages, install first:

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggmap)
library(lubridate)

#Then run from here to row 60
setwd("//fileshare1/Departments2/Somerstat Data/Constituent_Services/")

# Load data
intelligov <- read.csv("./data/2015_06_29_IntelligovFinalData.csv")
qscend <- read.csv("./data/311_Somerville.csv")

# #### Optional : remove common DPW reporters to winnow down to consitutent calls ####
# # alldpwreporters <- c("WORKER, DPW", "BUNKER, DAVE", "MAN, REPORT", ", DPW WORKER", "MORIN, CHRISTINE",
#                   ", GINO", "HARDY, DANNY", "HARDY, DANNY", "CASSESSO, CHRIS",
#                   "BARBIERE, JEFF", "HARDY, DANIEL", "BUNK, DAVE", "WOODS, JIMMY",
#                   "MACEACHERN, STEVEN", "ROSS, STEVE", "CORBIN, FRANKIE", "BROWN, KIM",
#                   "BUNKER, DAVID", "CORBIN, FRANK", "WALSH, JOHN")
# 
# # Note that most of these are reporting as, or on behalf of, citizens, so I use the shorter list
# # to distinguish work that is being tracked in real-time by DPW from legitimate reports from 
# # citizens or city workers
# 
# dpwreporters <- c("WORKER, DPW", "BUNKER, DAVE", "MAN, REPORT", ", DPW WORKER", ", GINO",
#                   "HARDY, DANNY", "HARDY, DANNY", "CASSESSO, CHRIS")
# 
# intelligov$dpwreporters <- ifelse(intelligov$Citizen.Name %in% dpwreporters, "yes", "no")
# 
# intelligov <- intelligov %>%
#   filter(dpwreporters == "no") %>%
#   select(-dpwreporters)


# Combine the two, but just the service type, date, and geo columns
intelligov <- intelligov %>% 
  mutate(Date = as.Date(intelligov$Date, "%m/%d/%Y"),
         lon = NA,
         lat = NA) %>% 
  select(Date, Service.Type, Location, lon, lat)

qscend <- qscend %>% 
  mutate(Date = as.Date(qscend$displayDate, "%m/%d/%Y"),
         Location = paste(streetNum, streetName),
         lon = longitude,
         lat = latitude) %>% 
  select(Date, typeName, Location, lon, lat)


#############################################################

## First look at work orders from qscend to find the variable of interest
sort(unique(qscend$typeName))
# Then copy and paste it into the quotes below 
workOrder <- "Arborist and tree maintenance"

## Now find the corresponding Intelligov work order
sort(unique(intelligov$Service.Type))
# Then copy and paste it into the quotes below 
workOrderIntelligov <- "DPW-Trees-arborist"


qscend <- qscend %>% 
  filter(typeName == workOrder) 

intelligov <- intelligov %>% 
  filter(Service.Type == workOrderIntelligov) %>% 
  mutate(Service.Type = gsub(workOrderIntelligov, workOrder, Service.Type)) 


# Combines by first renaming and then binding to the end
colnames(intelligov) <- c("Date", "typeName", "Location", "lon", "lat")

d <- rbind(intelligov, qscend)


# Converts data to tbl class. tbl's are easier to examine than data frames. R displays only the data that fits onscreen:
d <- tbl_df(d)


# dates
today <- Sys.Date()
yesterday <- today - 1
thirtyDaysAgo <- today - 30
sixtyDaysAgo <- today - 60
YearAgo <- today - 365
TwoYearsAgo <- YearAgo - 365
ThreeYearsAgo <- TwoYearsAgo - 365
FourYearsAgo <- ThreeYearsAgo - 365


d$Year <- format(d$Date, '%Y')
d$Year.Month <- format(d$Date, '%Y-%m')
d$Month <- format(d$Date, '%m')
d$YearDay <- yday(d$Date)

d$DaysAgo <- difftime(d$Date, today, units = "days")

workOrderData <- d
rm(intelligov, qscend)



####  Code to help Visualize in ggplot2 ####

lime_green = "#2ecc71"
soft_blue = "#3498db"
pinkish_red = "#e74c3c"
purple = "#9b59b6"
teele = "#1abc9c"
nice_blue = "#2980b9"

my_color = nice_blue


my.theme <- 
  theme(#plot.background = element_rect(fill="white"), # Remove background
    panel.grid.major = element_blank(), # Remove gridlines
    # panel.grid.minor = element_blank(), # Remove more gridlines
    # panel.border = element_blank(), # Remove border
    # panel.background = element_blank(), # Remove more background
    axis.ticks = element_blank(), # Remove axis ticks
    axis.text=element_text(size=6), # Enlarge axis text font
    axis.title=element_text(size=8), # Enlarge axis title font
    plot.title=element_text(size=12) # Enlarge, left-align title
    ,axis.text.x = element_text(angle=60, hjust = 1) # Uncomment if X-axis unreadable 
  )




#### Time Series ####
days <- workOrderData %>%
  group_by(Date) %>%
  summarise(Events = n())

d <- d[order(d$Date),]

allDays <- seq.Date(from=d$Date[1], to = d$Date[nrow(d)], by ='days')
allDays <- allDays  %>%  as.data.frame() 
colnames(allDays)[1] = "Date"

# After this we will have a df with every date and how many work orders
ts = merge(days, allDays, by='Date', all=TRUE)
ts[is.na(ts)] <- 0

remove(allDays, days)

ggplot(ts, aes(x=Date, y=Events)) + 
  geom_line(colour=my_color, size = .5) + 
  my.theme + ggtitle(paste(workOrder, "Calls Over Time")) + xlab("Time") +
  ylab("Daily Calls") + 
  scale_y_continuous(labels = comma)
  
ggsave(paste("./plots/OneOff/",workOrder, "_DailyTimeSeries.png", sep=""), dpi=250, width=5, height=3)


# Monthly time series 
tsm <- ts %>%
  mutate(Year.Month = format(Date, '%Y-%m')) %>%
  group_by(Year.Month) %>%
  summarise(Events = sum(Events)) %>%
  mutate(Year.Month = as.Date(paste(Year.Month,1,sep="-"),"%Y-%m-%d"))

ggplot(tsm, aes(x=Year.Month, y=Events, group = 1)) + 
  geom_line(colour=my_color, size = .5) + 
  my.theme + ggtitle(paste(workOrder, "Calls Over Time")) + xlab("Time") +
  ylab("Monthly Calls") + 
  scale_y_continuous(labels = comma) + scale_x_date(labels=date_format("%Y"))

ggsave(paste("./plots/OneOff/",workOrder, "_MonthlyTimeSeries.png", sep=""), dpi=250, width=5, height=3)

ggplot(tsm, aes(x=Year.Month, y=Events, group = 1)) + 
  geom_bar(stat='identity', fill=my_color) + 
  my.theme + ggtitle(paste(workOrder, "Calls Over Time")) + xlab("Time") +
  ylab("Monthly Calls") + 
  scale_y_continuous(labels = comma) + scale_x_date(labels=date_format("%Y"))

ggsave(paste("./plots/OneOff/",workOrder, "_MonthlyTimeSeriesBar.png", sep=""), dpi=250, width=5, height=3)


# Recent monthly time series
tsrm <- tsm %>%
  filter(Year.Month > YearAgo)

ggplot(tsrm, aes(x=Year.Month, y=Events, group = 1)) + 
  geom_bar(stat='identity', fill=my_color) + 
  my.theme + ggtitle(paste(workOrder, ": Last 12 Months")) + xlab("Month") +
  ylab("Monthly Calls") + 
  scale_y_continuous(labels = comma) + scale_x_date(labels=date_format("%b %Y"))

ggsave(paste("./plots/OneOff/",workOrder, "_MonthlyTimeSeriesBarRecent.png", sep=""), dpi=250, width=5, height=3)


# Very recent daily time series
tsr <- ts %>%
  filter(Date > sixtyDaysAgo)

ggplot(tsr, aes(x=Date, y=Events)) + 
  geom_line(colour=my_color, size = .5) + 
  my.theme + ggtitle(paste(workOrder, ": Last 60 Days")) + xlab("Day") +
  ylab("Daily Calls") + 
  scale_y_continuous(labels = comma)

ggsave(paste("./plots/OneOff/",workOrder, "_VeryRecentDailyTimeSeries.png", sep=""), dpi=250, width=5, height=3)

ggplot(tsr, aes(x=Date, y=Events)) + 
  geom_bar(stat='identity', fill=my_color) + 
  my.theme + ggtitle(paste(workOrder, ": Last 60 Days")) + xlab("Day") +
  ylab("Daily Calls") + 
  scale_y_continuous(labels = comma)

ggsave(paste("./plots/OneOff/",workOrder, "_VeryRecentDailyTimeSeriesBar.png", sep=""), dpi=250, width=5, height=3)


### Year to Date Yearly Comparison
JustYtD <- workOrderData %>%
  filter(YearDay <= yday(today))

AnnualYtD <- JustYtD %>%
  group_by(Year) %>%
  summarise(Events = n()) 

ggplot(AnnualYtD, aes(x=Year, y=Events)) + 
  geom_bar(stat='identity', fill=my_color) + 
  my.theme + ggtitle(paste(workOrder, ": Year to Date")) + xlab("Year") +
  ylab("YtD Calls") + 
  scale_y_continuous(labels = comma)

ggsave(paste("./plots/OneOff/",workOrder, "_YeartoDateBar.png", sep=""), dpi=250, width=5, height=3)


# More recent ytd
RecentYtD <- JustYtD %>%
  group_by(Year) %>%
  summarise(Events = n()) %>%
  filter(Year > 2010)

ggplot(RecentYtD, aes(x=Year, y=Events)) + 
  geom_bar(stat='identity', fill=my_color) + 
  my.theme + ggtitle(paste(workOrder, ": Year to Date")) + xlab("Year") +
  ylab("YtD Calls") + 
  scale_y_continuous(labels = comma)

ggsave(paste("./plots/OneOff/",workOrder, "_YeartoDateBarRecent.png", sep=""), dpi=250, width=5, height=3)


# Trailing 365 - a better comparison than YTD when it's early in the year

workOrderData$period <- 
  ifelse((workOrderData$Date >= YearAgo), "TrailingYear",
         ifelse((workOrderData$Date >= TwoYearsAgo) & (workOrderData$Date < YearAgo), "PrevPer1",
                ifelse((workOrderData$Date >= ThreeYearsAgo) & (workOrderData$Date < TwoYearsAgo), "PrevPer2",
                       ifelse((workOrderData$Date >= FourYearsAgo) & (workOrderData$Date < ThreeYearsAgo), "PrevPer3",
                              "LongAgo"))))

TrailingYear <- workOrderData %>%
  group_by(period) %>%
  summarise(Events = n()) %>%
  filter(period != "LongAgo") %>%
  mutate(period = factor(period, levels=c("PrevPer3", "PrevPer2", "PrevPer1", "TrailingYear")))

ggplot(TrailingYear, aes(x=period, y=Events)) + 
  geom_bar(stat='identity', fill=my_color) + 
  my.theme + ggtitle(paste(workOrder, ": Trailing 365 Days")) + xlab("Period") +
  ylab("Calls / 365 Days") + 
  scale_y_continuous(labels = comma)

ggsave(paste("./plots/OneOff/",workOrder, "_Trailing365Days.png", sep=""), dpi=250, width=5, height=3)




##### Descriptive stats ouput to a readme.txt ####

# Difference in YtD
thisYear <- nrow(AnnualYtD)
PerChange <- (AnnualYtD$Events[thisYear] - AnnualYtD$Events[thisYear - 1]) / AnnualYtD$Events[thisYear - 1]
GrowthOrDecline <- ifelse(PerChange > 0, "are up by", "are down by")

# Difference in Trailing 365
PerChangeLastYear <- (TrailingYear$Events[nrow(TrailingYear)] - TrailingYear$Event[(nrow(TrailingYear)) - 1]) / TrailingYear$Event[nrow(TrailingYear) - 1]
GrowthOrDeclineLastYear <- ifelse(PerChangeLastYear > 0, "are up by", "are down by")


# start writing out
# This makes the .txt report
sink(paste("./plots/OneOff/",workOrder, "_ReadMe.txt", sep=""))

cat(sprintf("Year to Date there have been %s calls for %s. Last year during the same time frame there were %s, which means calls for this work order %s %s percent \n", AnnualYtD$Events[thisYear], workOrder, AnnualYtD$Events[thisYear-1], GrowthOrDecline, round((PerChange * 100))))

cat("---\n")

cat("Trailing 365 - a better comparison than YTD when it's early in the year:\n")

cat(sprintf("During the last 365 days there have been %s calls for %s. During the previous 365-day time frame there were %s, which means calls for this work order %s %s percent \n", TrailingYear$Events[nrow(TrailingYear)], workOrder, TrailingYear$Event[(nrow(TrailingYear)) - 1], GrowthOrDeclineLastYear, round((PerChangeLastYear * 100))))


# Stop writing to the file
sink()



#############################################################
#### Maps ####

workOrderDataRecent <- filter(workOrderData, DaysAgo >= -30)

# If you need to compare past trends, you will have to geocode the location using google API
# addresses <- paste(workOrderDataRecent$Location, "Somerville", "MA", sep=", ")
# locs <- geocode(addresses)
# locs2 <- subset(locs, lat != 42.3875968 ) # Takes out the weird ones Google couldn't pin
# # I map locs2 because when Google can't find something, it usually puts it int the center of the map
# # This throws off the heat maps

locs <- select(workOrderDataRecent, lat, lon)
locs2 <- filter(locs, lat != 0)


# Dot map 
map.center <- geocode("Central Rd, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 14)
SHmap + geom_point(
  aes(x=locs2$lon, y=locs2$lat),size = 3, alpha = .7, bins = 26, color="red", 
  data = locs2) +
  ggtitle(paste(workOrder, "Calls Since", thirtyDaysAgo))

ggsave(paste("./plots/OneOff/",workOrder, "_map.png", sep=""), dpi=250, width=6, height=5)


# Dot map 
map.center <- geocode("East Somerville, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 15)
SHmap + geom_point(
  aes(x=locs2$lon, y=locs2$lat),size = 3, alpha = .7, bins = 26, color="red", 
  data = locs2) +
  ggtitle(paste(workOrder, "Calls Since", thirtyDaysAgo))

ggsave(paste("./plots/OneOff/",workOrder, "_map_East.png", sep=""), dpi=250, width=6, height=5)


# Dot map 
map.center <- geocode("West Somerville, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 15)
SHmap + geom_point(
  aes(x=locs2$lon, y=locs2$lat),size = 3, alpha = .7, bins = 26, color="red", 
  data = locs2) +
  ggtitle(paste(workOrder, "Calls Since", thirtyDaysAgo))

ggsave(paste("./plots/OneOff/",workOrder, "_map_West.png", sep=""), dpi=250, width=6, height=5)


# Dot map 
map.center <- geocode("Central Rd, Somerville, MA")
SHmap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom = 15)
SHmap + geom_point(
  aes(x=locs2$lon, y=locs2$lat),size = 3, alpha = .7, bins = 26, color="red", 
  data = locs2) +
  ggtitle(paste(workOrder, "Calls Since", thirtyDaysAgo))

ggsave(paste("./plots/OneOff/",workOrder, "_map_Central.png", sep=""), dpi=250, width=6, height=5)


# A for loop that will create a dot map for every neighborhood you specify
neighborhoodList <- c("Assembly Square", "Ball Square", "Davis Square", "East Somerville", "Gilman Square", "Magoun Square", "Porter Square", "Prospect Hill", "Spring Hill", "Teele Square", "Ten Hills", "Union Square", "Winter Hill")

for (n in 1:(length(neighborhoodList))) {
  map <- get_map(location = paste(neighborhoodList[n], "Somerville, MA", sep=", "), zoom=16, maptype="roadmap", color = "bw")
  ggmap(map) +
    geom_point(data=locs2,size=4, color = "red", alpha = .5,
               aes(x=lon,y=lat)) +
    labs(x="",y="") +
    theme(axis.text=element_blank(),axis.ticks=element_blank()) +
    ggtitle(paste(workOrder, neighborhoodList[n]))
  
  ggsave(paste("./plots/OneOff/",workOrder, "_NB_map_",neighborhoodList[n], ".png", sep=""), dpi=250, width=6, height=5)
  
}


# More traditional heat map
map.center <- geocode("Central Rd, Somerville, MA")
map.center <- c(lon=map.center$lon, lat=map.center$lat)
somerville.map = get_map(location = map.center, zoom = 13, maptype="roadmap",color = "bw")
ggmap(somerville.map, extent = "panel", maprange=FALSE) %+% locs2 + aes(x = locs2$lon, y = locs2$lat) +
  # geom_density2d(data = locs2, aes(x = lon, y = lat)) + # uncomment for contour lines
  stat_density2d(data = locs2, aes(x = lon, y = lat,  fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 12)) +
  ggtitle(paste(workOrder, "Calls Since", thirtyDaysAgo))


ggsave(paste("./plots/OneOff/",workOrder, "_map_Heat1.png", sep=""), dpi=250, width=6, height=5)


# More traditional heat map
map.center <- geocode("Central Rd, Somerville, MA")
map.center <- c(lon=map.center$lon, lat=map.center$lat)
somerville.map = get_map(location = map.center, zoom = 14, maptype="roadmap",color = "bw")
ggmap(somerville.map, extent = "panel", maprange=FALSE) %+% locs2 + aes(x = locs2$lon, y = locs2$lat) +
  # geom_density2d(data = locs2, aes(x = lon, y = lat)) + # uncomment for contour lines
  stat_density2d(data = locs2, aes(x = lon, y = lat,  fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(size = 12)) +
  ggtitle(paste(workOrder, "Calls Since", thirtyDaysAgo))


ggsave(paste("./plots/OneOff/",workOrder, "_map_Heat2.png", sep=""), dpi=250, width=6, height=5)



