# Script to clean up Budget Visualization data and save locally in a format ready to be uploaded to Socrata
# First iteration will be manually uploaded, but further versions may be able to be automatically uploaded to the open data portal.
# the goal will be to take the individual dataframes for revenue, funds, and expenses, join them together, cut out sum rows, and write locally.

## the script requires the following packages... uncomment if you need to install
#install.packages("dplyr")
#install.packages("zoo")


library("dplyr")
library("zoo")

## set working directory
setwd("/Users/michaelmastrobuoni/Coding/Budget/src/httpdocs/data")

## load the CSV files with the data
expenses <- read.csv("./processing/expenses.csv")
revenues <- read.csv("./processing/revenues.csv")
funds <- read.csv("./processing/funds.csv")

## Use zoo to fill in blank rows in column LEVEL2 for revenues & expenses - 
## note that this will cause some unwanted labels in rows that the script is about to remove
expenses$LEVEL2[expenses$LEVEL2 == ""] <- NA
expenses$LEVEL2 <- na.locf(expenses$LEVEL2)

revenues$LEVEL2[revenues$LEVEL2 == ""] <- NA
revenues$LEVEL2 <- na.locf(revenues$LEVEL2)

## clean the data for upload: remove totals, removing columns, renaming columns
expenses_clean <- filter(expenses, LEVEL == 3) %>%
                    select(-TOOLTIP, -SOURCE, -SOURCE.URL, -LEVEL) %>%
                    rename(FY2011 = X2011, FY2012 = X2012, FY2013 = X2013, FY2014 = X2014, FY2015 = X2015, FY2016 = X2016, FY2017 = X2017) %>%
                    mutate(TYPE = "Expense")

revenues_clean <- filter(revenues, !grepl('Total|TOTAL', LEVEL1), !grepl('Total', LEVEL2), !grepl('Total', LEVEL3)) %>%
                    select(-TOOLTIP, -SOURCE, -SOURCE.URL, -LEVEL) %>%
                    rename(FY2011 = X2011, FY2012 = X2012, FY2013 = X2013, FY2014 = X2014, FY2015 = X2015, FY2016 = X2016, FY2017 = X2017) %>%
                    mutate(TYPE = "Revenue")

funds_clean <- filter(funds, !grepl('Total|TOTAL', LEVEL1)) %>%
                    select(-TOOLTIP, -SOURCE, -SOURCE.URL, -LEVEL) %>%
                    rename(FY2011 = X2011, FY2012 = X2012, FY2013 = X2013, FY2014 = X2014, FY2015 = X2015, FY2016 = X2016, FY2017 = X2017) %>%
                    mutate(TYPE = "Fund Balance")

## union the three datasets into one set for upload, and reorder columns
combine <- rbind(expenses_clean, revenues_clean, funds_clean)
forSocrata <- select(combine, TYPE, LEVEL1:FY2017)

## write summary file locally
write.csv(forSocrata, file = "/Users/michaelmastrobuoni/Desktop/budget_socrata.csv")

## TBD: Upload to Socrata, chekc their API documentation to figure out how to append a new column for FY18 and beyond.
