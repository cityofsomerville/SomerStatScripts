## The intent of this script is to upload open checkbook data from the SomerStat Data drive and post it to the Socrata open data portal.
## It is expected that this will be set up through Windows Task Scheduler to run Monthly, the open checkbook data is posted to the SomerStat Data drive monthly on the 5th of the month.

## Set working Directory ##
setwd("//fileshare1/Departments2/Somerstat Data/IT/Open Checkbook Data")

# This pulls in the credentials you need
source("C:/Users/mmastrobuoni.CH2SOM-MMASTROB/Documents/GitHub/Somerville_Data_Pipes/config.R")

## Load Packages
library(httr) #for PUT function
library(xlsx) #for loading .xls files

## Load open checkbook data from the SomerStat Data drive and write locally (if necessary) 
checkbook <- read.xlsx("./csv_opencheckbook_2017.xls", header = TRUE)
#write.csv(checkbook, file = "checkbook.csv")

## upload to Socrata
PUT("https://data.somervillema.gov/resource/cyh9-gqxg.json",
    body = upload_file("./checkbook.csv"),
    authenticate(Socrata_username, Socrata_password), 
    add_headers("X-App-Token" = Socrata_token,
                "Content-Type" = "text/csv"))