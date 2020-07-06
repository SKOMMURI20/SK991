library(RMySQL)
library(DBI)
library(dplyr)
library(dbplyr)
library(lubridate)

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
# List the tables contained in the database 
dbListTables(con)
# Lists attributes contained in a table
dbListFields(con,'yr_2006')
## Use asterisk to specify all attributes for download
yr2006 <- dbGetQuery(con, "SELECT * FROM yr_2006")
yr2007 <- dbGetQuery(con, "SELECT * FROM yr_2007")
yr2008 <- dbGetQuery(con, "SELECT * FROM yr_2008")
yr2009 <- dbGetQuery(con, "SELECT * FROM yr_2009")
yr2010 <- dbGetQuery(con, "SELECT * FROM yr_2010")
colnames(yr2006)
## Combining tables
newDF <- bind_rows(yr2007, yr2008, yr2009)

newDF <-cbind(newDF, DateTime = paste(newDF$Date,newDF$Time))

#Creating columns based on timings
newDF$year <- year(newDF$DateTime)
# quarter, month, week, weekday, day, hour and minute
newDF$quarter <- quarter(newDF$DateTime)
newDF$month <- month(newDF$DateTime)
newDF$week <- week(newDF$DateTime)
newDF$weekday <- weekdays(newDF$DateTime)
newDF$day <- day(newDF$DateTime)
newDF$hour <- hour(newDF$DateTime)
newDF$minute <- minute(newDF$DateTime)
# second week of 2008 
houseWeek <- filter(newDF, year == 2008 & week == 2)
# Plotting 
plot(houseWeek$Sub_metering_1)

library(plotly)

## Subset the 21th day of January 2009 - All observations
houseDay <- filter(newDF, year == 2009 & month == 1 & day == 21)
## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

newDF$weekDay <- wday(ymd(newDF$Date),label = TRUE)

houseDay2 <- filter(newDF, year == 2008 & weekDay == 'Sun')
# Plot sub-meter 1, 2 and 3 with title, legend and labels
plot_ly(houseDay, x = ~houseDay2$DateTime, y = ~houseDay2$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay2$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay2$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption of all Sunday's in 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


#----------------------- TS ANALYSIS ----------------------
library(ggplot2)
library(ggfortify)
#  Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(newDF, weekDay == 'Mon' & hour == 20 & minute == 0)

tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))

plot.ts(tsSM3_070809weekly, xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3, Mondays, 20:00 ")


house07_Sep <- filter(newDF, year == 2007 & month == 9)
## Create TS object for September 2007 
TS_house07_Sep <- ts(house07_Sep$Sub_metering_1, frequency=1440)
## Plot sub-meter 3 with plot.ts
plot.ts(TS_house07_Sep, xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1, Sundays, 18:00 ")

# ----------------------- Forecast ---------------------
## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
library(forecast)
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

# sub-meter 3 forecast with labels
plot(forecastfitSM3c, ylab= "Watt-Hours", xlab="Time")

#----------------------- Decomposing -----------------------
#Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
# Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
# Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)

components_TS_house07_Sep <- decompose(TS_house07_Sep)
plot(components_TS_house07_Sep)

