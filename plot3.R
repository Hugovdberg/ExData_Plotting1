## plot3.R
## Author: Hugo van den Berg
## Date: 2016-08-15
##
## This file creates line plots of the power consumption per minute of
## a single household, for each submetered zone, over a two day period.

library(readr)

library(magrittr)
library(tidyr)
library(dplyr)
library(lubridate)

# Download and extract data if not already done
data.url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip' # Remote URL
data.zip <- 'household_power_consumption.zip' # Zipped textfile
data.file <- 'household_power_consumption.txt' # Raw textfile
data.subset <- 'household_power_consumption.Rdata' # Rdata file containing relevant subset
if (!file.exists(data.subset)) {
    if (!file.exists(data.file)) {
        if (!file.exists(data.zip)) {
            # Download if necessary
            download.file(url = data.url, destfile = data.zip)
        }
        # Unzip if necessary
        unzip(data.file)
    }

    # print(readLines(data.file, n=2)) # Check delimiters
    # Read data, NA's are specified as a ?
    data <- read_delim(data.file, delim = ';', na = c('?'),
                       col_names = TRUE,
                       col_types = 'ccddddddd')

    # Prepare and subset data
    data %<>%
        # Merge Date and Time to single variable
        unite(datetime, Date, Time, sep = ' ') %>%
        # Convert datatime variable to POSIXct
        mutate(datetime = dmy_hms(datetime)) %>%
        # Subset to the relevant days
        filter(datetime >= ymd_hms('2007/02/01 00:00:00') &
                   datetime <= ymd_hms('2007/02/03 00:00:00'))

    # Save the subset to an Rdata file for reuse
    save(data, file = data.subset)
} else {
    # Load the relevant data from the Rdata file if necessary
    load(data.subset)
}
rm(list = c('data.file', 'data.subset', 'data.url', 'data.zip'))

# Open png-graphics device for third plot
png(filename = 'plot3.png', height = 480, width = 480, units = "px")

# Create plot for sub_metering_1 and add second and third line
with(data = data,
     {
         plot(Sub_metering_1 ~ datetime,
              col = "black",
              type = "l",
              # Labels and titles will be added at the end
              xlab = "",
              ylab = "",
              main = "")
         lines(Sub_metering_2 ~ datetime,
               col = "red")
         lines(Sub_metering_3 ~ datetime,
               col = "blue")
     }
)
# Add title
title(
    ylab = "Energy sub metering"
)
# Add legend
legend("topright",
       lwd = 1,
       col = c("black", "red", "blue"),
       legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"))

# Close graphics device to save image
dev.off()
