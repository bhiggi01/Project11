##################################################################
# STAT9005 - Time Series Project
##################################################################

##################################################################
# Introduction
##################################################################

################
# Country chosen
################

# I choose the data from the US as there was a lot of data and fluctuations in the 
# data which is very usual and show give some interesting Time Series Analysis.

################
# Libraries
################
library(forecast)
library(ggplot2)
library(tseries)
library(astsa)
library(sarima)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(kableExtra)
library(lubridate)

##################################################################
# Data Cleaning and Manipulation
##################################################################

################
# Clean workspace
################

rm(list=ls())

################
# Load Data
################
# load all data
alldata <- read.csv("owid-covid-data.csv")

################
# Take only columns needed, drop the rest

# View all the data
# View(alldata)
dim(alldata) # 304355, 67 Columns
head(alldata) # Start Date: 2020-01-03
tail(alldata) # 2023-04-14
length(unique(alldata$location))# 255 countries and locations

# load all data
alldata <- read.csv("owid-covid-data.csv")

# Select only the United States
usdata <- subset(alldata, location=="United States")
# View(usdata) # Start Date: 2020-01-03

# Drop all the columns except Time, new_cases, new_deaths, new_tests, new_vaccinations
usdata <- select(usdata, date, new_cases, new_deaths, new_tests, new_vaccinations)

################
# what class is each column
str(usdata)
## one character and 4 numeric

# change date to a date format
usdata$date <- as.Date(usdata$date)

################
# Investigate which is the best to use for our project.
################

# Number of missing values in each column
colSums(is.na(usdata))
# date         new_cases        new_deaths         new_tests people_vaccinated 
# 0                 0                 1               363               347
## new_cases and new_deaths have the lowest number of missing values. New_cases as none.

# Percentage of missing values
round(colSums(is.na(usdata)) / nrow(usdata) * 100,2)
# date         new_cases        new_deaths         new_tests new_vaccinated 
# 0.00              0.00              0.08             30.17             28.84 
## New_tests and people_vaccianted have a high percentage of missing values so unlikely to pick these.

# Plot each one to see what they look like
plot1 <- ggplot(usdata, aes(x = date, y = new_cases)) +
  geom_line(color = "black", alpha = 0.7) +
  labs(title = "New Cases", x = "Date", y = "Number of Cases") + theme_light() + 
  scale_y_continuous(breaks = c(), limits = c(0, NA))

plot2 <- ggplot(usdata, aes(x = date, y = new_deaths)) +
  geom_line(color = "black", alpha = 0.7) +
  labs(title = "New Deaths", x = "Date", y = "Number of Deaths") + theme_light() +
  scale_y_continuous(breaks = c(), limits = c(0, NA))

plot3 <- ggplot(usdata, aes(x = date, y = new_tests)) +
  geom_line(color = "black", alpha = 0.7) +
  labs(title = "New Tests", x = "Date", y = "Number of tests") + theme_light() +
  scale_y_continuous(breaks = c(), limits = c(0, NA))

plot4 <- ggplot(usdata, aes(x = date, y = new_vaccinations)) +
  geom_line(color = "black", alpha = 0.7) +
  labs(title = "New Vaccinated", x = "Date", y = "Number of new vaccinated")+ theme_light() +
  scale_y_continuous(breaks = c(), limits = c(0, NA))

# plot the 4 in a 2x2 grid
compare_varibales <- grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

## The new tests has the most interesting plot so I want to use this one for my 
## time series project. The high amount of missing data is because the US had
## stopped testing people. Something every country would have done.
##New cases and New deaths has more overall data but less interesting when I did
## break up into three parts.
## Similar for New Vaccinated

# Save the image as a file
# ggsave("Plots/Compare Variables.png", compare_varibales, width = 10, height = 8, dpi = 300)

################
# Missing data
################

################
# Take just the date and new tests column.
usdata <- select(usdata, date, new_tests)

################
# Test for missing data
colSums(is.na(usdata)) ## there are 363 NA values
str(usdata)

# From the plot I know that there is data missing at the beginning and the end
# This is a good start point to delete NA points
# NA points in the middle of these points we will deal with differently
head(usdata) # NA at the beginning
tail(usdata) # Na at the end

# We can delete any NA's up to the first real value
first_na <- which.min(is.na(usdata$new_tests))
usdata <- slice(usdata, -(1:first_na))
head(usdata) # No na at the beginning now. Good.
colSums(is.na(usdata)) ## it has delete any NA valus at the beginning, 363-305 = 58 deleted
  
# Delete any of the NA at the end of the 
last_na <- rev(cumsum(rev(is.na(usdata$new_tests))))
usdata <- usdata[1:(nrow(usdata) - max(last_na)), ]
tail(usdata) # No NA at the end now. Good
colSums(is.na(usdata)) # This has removed all the remainding NA values.

## There are No NA values in the middle of the data so this is a good result.

# Plot the new data, without the NA values.
plot_ts_removed_na_values <- ggplot(usdata, aes(x = date, y = new_tests)) +
  geom_line( color = "red") +
  labs(title = "New Tests (Minus NA Values)", x = "Date", y = "Number of new tests") + theme_light() +
  scale_y_continuous(labels = scales::comma_format())

# Save plot
# ggsave("plots/new_tests_without_NA.png", plot_ts_removed_na_values, width = 8, height = 5) # save plot to "plots" folder

################
# Are all dates inlcuded between the first and the last date
all(diff(time(usdata$date))==1)
# Answer is TRUE, so no missing dates between the first and last date.

################
################
# Create new time series for weekly and monthly
# Discuss daily, weekly and monthly time series and compare
################
################

################
# Create a daily time series
usdata_daily_ts <- ts(usdata$new_tests, frequency=7)

# plot the daily time series
plot(usdata_daily_ts, main="Daily Tests in US", xlab="Time Line",  ylab="Number of tests")

# Save plot
ggsave("plots/daily_line.png",  width = 8, height = 5) # save plot to "plots" folder


################
# create weekly feature
usdata_weekly <- aggregate(usdata$new_tests, by =list(week=cut(usdata$date, "week")), sum)
usdata_weekly_ts <- ts(usdata_weekly$x, frequency = 52)

# change the y axis so more readable
options(scipen=10)

# Plot the weekly time series
plot(usdata_weekly_ts, main="Weekly Tests in US", xlab="Time Line",ylab="Number of test")

################
# create monthly feature
usdata_monthly <- aggregate(usdata$new_tests, by =list(month=format(usdata$date, "%Y-%m")), sum)
usdata_monthly_ts <- ts(usdata_monthly$x, frequency = 12)

# Plot the weekly time series
plot(usdata_monthly_ts, main="Monthly Tests in US", xlab="Time Line", ylab="Number of test")

################
# Look at the three time series side by side

par(mfrow = c(1,3))
plot(usdata_daily_ts, main="Daily Tests in US", xlab="Time Line", 
     ylab="Number of tests", col="blue")
plot(usdata_weekly_ts, main="Weekly Tests in US", xlab="Time Line", 
     ylab="Number of test", col="red")
plot(usdata_monthly_ts, main="Monthly Tests in US", xlab="Time Line", 
     ylab="Number of test", col="green")
par(mfrow = c(1,1))

##################################################################
# Preliminary Analysis
##################################################################

################
# Summary analysis and Plots
################

################
# Summary

# Daily Time Series
summary(usdata_daily_ts)

# Weekly Time Series
summary(usdata_weekly_ts)

# Monthly Time Series
summary(usdata_monthly_ts)

# Create a table to display the information in the summary statistics
summary_table <- data.frame(
  Daily = c(min(usdata_daily_ts), quantile(usdata_daily_ts, 
            probs = c(0.25, 0.5, 0.75)), mean(usdata_daily_ts), max(usdata_daily_ts)),
  Weekly = c(min(usdata_weekly_ts), quantile(usdata_weekly_ts, 
            probs = c(0.25, 0.5, 0.75)), mean(usdata_weekly_ts), max(usdata_weekly_ts)),
  Monthly = c(min(usdata_monthly_ts), quantile(usdata_monthly_ts, 
            probs = c(0.25, 0.5, 0.75)), mean(usdata_monthly_ts), max(usdata_monthly_ts))
)
rownames(summary_table) <- c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum")

summary_table %>%
  kbl(caption = "Summary statisics for each period:") %>%
  kable_classic(full_width = F, html_font = "Cambria")


##SUMMARY ANALYSIS::

## From the summary statistics we can see the number of tests per day, week and month varies significently.
## The daily TS has the samllest range and the monhtly TS the biggest range. The mean also increases in the same way.
## The IQR also increasesing suggesting thatthe data has a wider distribution as the time scale increases.

################
# histogram
daily_hist <- ggplot() + geom_histogram(aes(x = usdata_daily_ts), bins = 15) + 
  labs(title = "Daily Tests in US",x = "Number of tests (100,000)", y = "Frequency") +
  theme_light()
weekly_hist <- ggplot() + geom_histogram(aes(x = usdata_weekly_ts), bins = 15) + 
  labs(title = "Weekly Tests in US", x = "Number of tests (million)", y = "Frequency") +
  theme_light()
monthly_hist <- ggplot() + geom_histogram(aes(x = usdata_monthly_ts), bins = 15) + 
  labs(title = "Monthly Tests in US", x = "Number of tests (million)",y = "Frequency") +
  theme_light()

# plot the 3 in a side by side grid
plot_histograms <- grid.arrange(daily_hist , weekly_hist , monthly_hist , ncol=3)

# Save plot
#ggsave("plots/histograms.png", plot_histograms, width = 8, height = 5) # save plot to "plots" folder

## The histogram of the Daily TS shows a right skew with a peak of 1million per day. There are only a few days
## with very low or very high tests. Most falls into the range of 500k to 1.5million
## The weekly histogram is also right skewed with a peak of around 7.5million tests. 
## The monthly histogram shows a different pattern with a more evenly symmetrically distribution 
## around 30million tests. There is some variability but not as much as the daily and weekly time series.

################
# Boxplot: show how the test are distributed across different days, weeks and months and show
# any outliers.

daily_box <- ggplot() + geom_boxplot(aes(x = usdata_daily_ts)) + 
  labs(title = "Daily Tests in US",x = "Number of tests (million)", y = "Frequency") +
  theme_light() + coord_flip() + scale_y_continuous(breaks = c())
weekly_box <- ggplot() + geom_boxplot(aes(x = usdata_weekly_ts)) + 
  labs(title = "Weekly Tests in US", x = "Number of tests (million)", y = "Frequency") +
  theme_light()+ coord_flip()+ scale_y_continuous(breaks = c())
monthly_box <- ggplot() + geom_boxplot(aes(x = usdata_monthly_ts)) + 
  labs(title = "Monthly Tests in US", x = "Number of tests (million)",y = "Frequency") +
  theme_light()+ coord_flip()+ scale_y_continuous(breaks = c())

# plot the 3 in a side by side grid
plot_boxplots <- grid.arrange(daily_box , weekly_box , monthly_box , ncol=3)

# Save plot
#ggsave("plots/plot_boxplots.png", plot_boxplots, width = 8, height = 5) # save plot to "plots" folder

## Boxplot of Daily tests shows a median value of 1 million new tests per day. with a wide range
##  of values from 500k to 3million. There are a few outliers on the high end.
## The boxplot for weekly tests shows a median value of 7.5million new test with a range form 
## 8 million to 18 million. There is one outlier.
## The boxplot for monthly tests shows a median value for 30million with a range of 15million
## to 65 million. There are no outliers
## The boxplots are similar to the histograms with all three time series.

################
# Decompose the three time series
################

# What type of model is each one? Additive or multiplicative
# which is best to describe each one.
# Use plots and summaries to discuss

# Analyse importance of each component (if any) in the time series.
# Is this component conditioning the analysis in any way? 
# Explain how this issue could be addressed if required.


################
# Decompose Daily Time Series
################

usdata_daily_ts_ad <- decompose(usdata_daily_ts,"additive")
plot(usdata_daily_ts_ad)

# Need to convert the decompose object to a ggplot so i can save for better image clarity
# as I don't want to take a screen shot.

ggplot_obj <- autoplot(usdata_daily_ts_ad)
ggplot_obj + theme_light()
# save to file
# ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

usdata_daily_ts_mul <- decompose(usdata_daily_ts,"multiplicative")
plot(usdata_daily_ts_mul)
ggplot_obj <- autoplot(usdata_daily_ts_mul )
ggplot_obj + theme_light()
# ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

## Looking at the Trend its the same for both, there are small changes in the 
## seasonality but its hard to make out. The scale for seasonality for "multiplicative"
## is much smaller, whereas for "additive" is much bigger. This could indicate the 
## "additive" model is not capturing all the data. The scale is smaller in the "multiplicative"
## model and this suggests the "multiplicative" model is doing a better job at capturing the data.
## The Random component offers the biggest change. It does not look as random and the scale is
## much bigger for the "additive" model. 
## Because the "multiplicative" model has a smaller scale for both seasonal and random components
## this looks like the better model to use.

################
# Address the issues in the "additive" model
## Since the scale is larger for the additive model we could transform the data to reduce the 
## variability and bring the scale of seasonality and the random component down.

usdata_daily_ts_log <- log(usdata_daily_ts)
usdata_daily_ts_log_ad <- decompose(usdata_daily_ts_log,"additive")
plot(usdata_daily_ts_log_ad)
ggplot_obj <- autoplot(usdata_daily_ts_log_ad)
ggplot_obj + theme_light()
#ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

usdata_daily_ts_log_mul <- decompose(usdata_daily_ts_log,"multiplicative")
plot(usdata_daily_ts_log_mul)
ggplot_obj <- autoplot(usdata_daily_ts_log_mul)
ggplot_obj + theme_light()
# ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

## Both models look similar now. Since this has had less of an effect on either
## model. We will not use LOG as it only adds extra complexity to the models.
## So, the Multiplicative model from above, without log is still the best.

################
# Periodicity

mstl(usdata_daily_ts)
autoplot(mstl(usdata_daily_ts, s.window=7))
ggplot_obj <- autoplot(mstl(usdata_daily_ts, s.window=7))
ggplot_obj + theme_light()
# ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

autoplot(mstl(usdata_daily_ts, s.window="periodic"))
ggplot_obj <- autoplot(mstl(usdata_daily_ts, s.window="periodic"))
ggplot_obj + theme_light()
# ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

################
# Additional

plot(ets(usdata_daily_ts))

################
# Decompose Weekly Time Series
################
usdata_weekly_ts_ad <- decompose(usdata_weekly_ts,"additive")
plot(usdata_weekly_ts_ad )
ggplot_obj <- autoplot(usdata_weekly_ts_ad)
ggplot_obj + theme_light()
#ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

usdata_weekly_ts_mul <- decompose(usdata_weekly_ts,"multiplicative")
plot(usdata_weekly_ts_mul )
ggplot_obj <- autoplot(usdata_weekly_ts_mul)
ggplot_obj + theme_light()
#ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

## The trend line component of both models is stable over time.
## The seasonality stays the same and there looks to be two patterns.
## A MTSL model might work best here
## The scale of the additive model is much bigger which suggests the model is not
## capturing the data. The scale for the multiplicative model has a smaller scale so
## appears to be working better.
## The random component is not random as there is a flat line in both models.
## Also the scale is much larger in the additive model again.

################
# Address the issues in both models.

# first try logs
usdata_weekly_ts_log <- log(usdata_weekly_ts)
usdata_weekly_ts_log_ad <- decompose(usdata_weekly_ts_log,"additive")
plot(usdata_weekly_ts_log_ad)
ggplot_obj <- autoplot(usdata_weekly_ts_log_ad)
ggplot_obj + theme_light()
# ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

usdata_weekly_ts_log_mul <- decompose(usdata_weekly_ts_log,"multiplicative")
plot(usdata_weekly_ts_log_mul)
ggplot_obj <- autoplot(usdata_weekly_ts_log_mul)
ggplot_obj + theme_light()
# ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

## very little change, both models look the same.
## the scale is better but using transformations is not a good way to address the issues 
## of the weekly data

################
# Periodicity

mstl(usdata_weekly_ts)
autoplot(mstl(usdata_weekly_ts, s.window=52))
ggplot_obj <- autoplot(mstl(usdata_weekly_ts, s.window=7))
ggplot_obj + theme_light()
#ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

autoplot(mstl(usdata_weekly_ts, s.window="periodic"))
ggplot_obj <- autoplot(mstl(usdata_weekly_ts, s.window="periodic"))
ggplot_obj + theme_light()
# ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

## Both models are the same so the periodicity is capturing the same seasonal as the manual
## value of 52.
## the random component is still not looking great.

################
# Use logs and MSTL with a period of 52 to see if th

usdata_weekly_ts_log_mstl_52 <- mstl(usdata_weekly_ts_log, s.window = 52)
ggplot_obj <- autoplot(usdata_weekly_ts_log_mstl_52)
ggplot_obj + theme_light()
# ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

usdata_weekly_ts_log_mstl_52 <- stl(usdata_weekly_ts_log, s.window = 52)
ggplot_obj <- autoplot(usdata_weekly_ts_log_mstl_52)
ggplot_obj + theme_light()
# ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

################
# Decompose Monthly Time Series.
################
usdata_monthly_ts_ad <- decompose(usdata_monthly_ts,"additive")
plot(usdata_monthly_ts_ad )
ggplot_obj <- autoplot(usdata_monthly_ts_ad )
ggplot_obj + theme_light()
# ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

usdata_monthly_ts_mul <- decompose(usdata_monthly_ts,"multiplicative")
plot(usdata_monthly_ts_mul )
ggplot_obj <- autoplot(usdata_monthly_ts_mul)
ggplot_obj + theme_light()
# ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

# Similar issues, the lines for the monthly show much less variance.
# same issue with scale. same issues with non randomness with random component in both

################
# Periodicity

mstl(usdata_monthly_ts)
autoplot(mstl(usdata_monthly_ts, s.window=12))
ggplot_obj <- autoplot(mstl(usdata_monthly_ts, s.window=7))
ggplot_obj + theme_light()
# ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

autoplot(mstl(usdata_monthly_ts, s.window="periodic"))
ggplot_obj <- autoplot(mstl(usdata_monthly_ts, s.window="periodic"))
ggplot_obj + theme_light()
# ggsave("plots/saved_file.png", ggplot_obj, width = 8, height = 5)

## Once again periodicity is doing a good job of picking up the seaonality but again
## the random component is still not random.

################
################
# Only for the daily data set, identify three periods for different waves of COVID and 
# generate a new time series for the period of each wave (create 3 subsets of the original time series). 
# Repeat the previous analysis for the different waves and discuss on the differences observed.
################
################

# Plot the Daily time series data
plot_full_daily <- ggplot(usdata, aes(x = date, y = new_tests)) +
  geom_line() +
  labs(title = "Full Time Series Data", x = "Date", y = "Number of new tests") + theme_light() +
  scale_y_continuous(labels = scales::comma_format())
plot(plot_full_daily)

# Create three periods
start_1 <- as.Date("2020-03-02")
end_date_1 <- as.Date("2020-11-29")
start_2 <- as.Date("2020-11-30")
end_date_2 <- as.Date("2021-12-26")
start_3 <- as.Date("2022-01-02")
end_date_3 <- as.Date("2022-06-18")

mask1 <- usdata$date >= start_1 & usdata$date <= end_date_1
mask2 <- usdata$date >= start_2 & usdata$date <= end_date_2
mask3 <- usdata$date >= start_3 & usdata$date <= end_date_3

wave_1 <- usdata[mask1,]
wave_2 <- usdata[mask2,]
wave_3 <- usdata[mask3,]

# Plot the Daily time series data
plot_wave_1 <- ggplot(wave_1, aes(x = date, y = new_tests)) +
  geom_line() +
  labs(title = "Wave 1 Time Series", x = "Date", y = "Number of new tests") + theme_light() +
  scale_y_continuous(labels = scales::comma_format())
plot_wave_1 

plot_wave_2  <- ggplot(wave_2, aes(x = date, y = new_tests)) +
  geom_line() +
  labs(title = "Wave 2 Time Series", x = "Date", y = "Number of new tests") + theme_light() +
  scale_y_continuous(labels = scales::comma_format())
plot_wave_2 

plot_wave_3  <- ggplot(wave_3, aes(x = date, y = new_tests)) +
  geom_line() +
  labs(title = "Wave 3 Time Series", x = "Date", y = "Number of new tests") + theme_light() +
  scale_y_continuous(labels = scales::comma_format())
plot_wave_3 

# ggsave("plots/full_data.png", plot_full_daily, width = 8, height = 5)
# ggsave("plots/section_1.png", plot_wave_1, width = 8, height = 5)
# ggsave("plots/section_2.png", plot_wave_2, width = 8, height = 5)
# ggsave("plots/section_3.png", plot_wave_3, width = 8, height = 5)

##################################################################
# Time Series Modelling: Daily Time Series ONLY
##################################################################

## All the models have Trend and Seasonality

# Make four time series objects
fulldata_ts <- ts(usdata$new_tests, frequency = 7)
autoplot(fulldata_ts)

wave_1_ts <- ts(wave_1$new_tests, frequency = 7)
autoplot(wave_1_ts)

wave_2_ts <- ts(wave_2$new_tests, frequency = 7)
autoplot(wave_2_ts)

wave_3_ts <- ts(wave_3$new_tests, frequency = 7)
autoplot(wave_3_ts)

################
################
# Classical Models
################
################


################
# Full data
################
# options(max.print = 9000) # Print all the rows.
# Original data

# Have a look at the data
fulldata_ts 
autoplot(fulldata_ts, main="Full Data Time Series")

## Looking at the data there are two peaks (MSTL)
## There looks like a lot of variance with Trend and Seasonality.

################
# Decompose
autoplot(decompose(fulldata_ts))
## We can see trend, and maybe some seasonality as two periods. Is this cycinical
## The random component is not random

autoplot(decompose(fulldata_ts, type="multiplicative"))
## No change with Multiplicative model so we will not use. We do not need the extra
## complexity in the mode.

################
# STL, MSTL, Holts Winter

# Use STL to try and remove high frequency. But two peaks so MSTL might be better
fulldata_ts_stl <- stl(fulldata_ts , s.window = "periodic")
autoplot(fulldata_ts_stl)
## Picks up trend ok,even as it dips towards the end of the time series but slow to adjust 
## for this and two seasonal peaks which looks ok as there are peaks in Jan of each year
## But the random component does not look random, we can see some seasonal repeating component
acf2(as.numeric(fulldata_ts_stl$time.series[,"seasonal"]))

# Scale fulldata before STL to see how that works
fulldata_ts_log <- log(fulldata_ts)
fulldata_ts_log_stl <- stl(fulldata_ts_log , s.window = "periodic")
autoplot(fulldata_ts_log_stl)
# looks better, especially with the radnom component.
acf2(as.numeric(fulldata_ts_log_stl$time.series[,"seasonal"]))

# Will try MSTL as there are two peaks
fulldata_ts_mstl <- mstl(fulldata_ts , s.window = "periodic")
autoplot(fulldata_ts_mstl)
## No improvement trying mstl. Very similar results.

# Try Holts Winter as we have trend and seasonality (twice in week and year)
fulldata_ts_hw <- hw(fulldata_ts, frequency=365)
decompose(fulldata_ts_hw) # Did not work

################
## These models are not doing well at capturing the trends and seasonality in the data.
## Later we will look at stationary and ARIMA models.

################
# Wave_1
################

################
plot(wave_1_ts)

# First, Decomponse the Classical Model
autoplot(decompose(wave_1_ts, type="additive"))
autoplot(decompose(wave_1_ts, type="multiplicative"))
## A quick decompose shows that the Multiplicative model looks better. With the 
## "additive" model the random component does not look random. It looks better in the
## Multiplicative model.

################
# Find the best Classical Model.
# Try ets, stl, mstl (no need as only one peak)

# Try ETS
wave_1_ts_ets <- ets(wave_1_ts)
autoplot(decompose(fulldata_ts_ets))
## time series has no or less than 2 periods
## Not sure why we get this when there should be enough periods.

# Try STL.
wave_1_ts_stl <- stl(wave_1_ts, s.window = "periodic")
autoplot(wave_1_ts_stl)
acf2(as.numeric(wave_1_ts_stl$time.series[,"seasonal"]))

# Try STL with logs
wave_1_ts_log <- log(wave_1_ts)
wave_1_ts_log_stl <- stl(wave_1_ts_log , s.window = "periodic")
autoplot(wave_1_ts_log_stl)
# looks better, especially with the radnom component.
acf2(as.numeric(wave_1_ts_log_stl$time.series[,"seasonal"]))

# use hw with seasonality "multiplicative"
wave_1_ts_hw_mul <- hw(wave_1_ts, seasonal ="multiplicative")
autoplot(wave_1_ts_hw_mul)
## Does better as the Confidence level of looks like it is in a better range, but the
## black like is missing a trend component I believe.

# using Holts Winter.
wave_1_ts_hw_mul_damp <- hw(wave_1_ts, seasonal ="multiplicative", damped=TRUE)
autoplot(wave_1_ts_hw_mul_damp)
plot(decompose(wave_1_ts_hw_mul_damp))
## I can't decompose a holts winter

################
# Wave_2
################

################
plot(wave_2_ts)

# First, Decomponse the Classical Model
autoplot(decompose(wave_2_ts, type="additive"))
autoplot(decompose(wave_2_ts, type="multiplicative"))
## A quick decompose shows that the Multiplicative model looks better. With the 
## "additive" model the random component does not look random. It looks better in the
## Multiplicative model.

################
# Find the best Classical Model.
# Try ets, stl, mstl (no need as only one peak)

# Try ETS
wave_2_ts_ets <- ets(wave_2_ts)
autoplot(decompose(wave_2_ts_ets))
## time series has no or less than 2 periods
## Not sure why we get this when there should be enough periods.

# Try STL.
wave_2_ts_stl <- stl(wave_2_ts, s.window = "periodic")
autoplot(wave_2_ts_stl)
acf2(as.numeric(wave_2_ts_stl$time.series[,"seasonal"]))

# Try STL with logs
wave_2_ts_log <- log(wave_2_ts)
wave_2_ts_log_stl <- stl(wave_2_ts_log , s.window = "periodic")
autoplot(wave_2_ts_log_stl)
# Don't think it offers as much of an improvement for the extra complexity 
acf2(as.numeric(wave_2_ts_log_stl$time.series[,"seasonal"]))

# use hw with seasonality "multiplicative"
wave_2_ts_hw_mul <- hw(wave_2_ts, seasonal ="multiplicative")
autoplot(wave_2_ts_hw_mul)
## Holts Winter does offer some good results when we look at froecasting but we will
## look at this more later.

# Try MSTL
wave_2_ts_mstl <- mstl(wave_2_ts)
autoplot(wave_2_ts_mstl) 

wave_2_ts_log <- log(wave_2_ts)
wave_2_ts_log_mstl <- mstl(wave_2_ts_log)
autoplot(wave_2_ts_log_mstl)


################
# Wave_3
################

################
plot(wave_3_ts)

# First, Decomponse the Classical Model
autoplot(decompose(wave_3_ts, type="additive"))
autoplot(decompose(wave_3_ts, type="multiplicative"))


################
# Find the best Classical Model.
# Try ets, stl, mstl (no need as only one peak)

# Try ETS
wave_3_ts_ets <- ets(wave_3_ts)
autoplot(decompose(wave_3_ts_ets))
## time series has no or less than 2 periods
## Not sure why we get this when there should be enough periods.

# Try STL.
wave_3_ts_stl <- stl(wave_3_ts, s.window = "periodic")
autoplot(wave_3_ts_stl)
acf2(as.numeric(wave_3_ts_stl$time.series[,"seasonal"]))

# Try STL with logs
wave_2_ts_log <- log(wave_2_ts)
wave_2_ts_log_stl <- stl(wave_2_ts_log , s.window = "periodic")
autoplot(wave_2_ts_log_stl)
# Don't think it offers as much of an improvement for the extra complexity 
acf2(as.numeric(wave_2_ts_log_stl$time.series[,"seasonal"]))

# use hw with seasonality "multiplicative"
wave_2_ts_hw_mul <- hw(wave_2_ts, seasonal ="multiplicative")
autoplot(wave_2_ts_hw_mul)
## Holts Winter does offer some good results when we look at froecasting but we will
## look at this more later.

# Try MSTL
wave_2_ts_mstl <- mstl(wave_2_ts)
autoplot(wave_2_ts_mstl) 

wave_2_ts_log <- log(wave_2_ts)
wave_2_ts_log_mstl <- mstl(wave_2_ts_log)
autoplot(wave_2_ts_log_mstl)

################
################
# Stationary and ARIMA Models
################
################

################
# Full data
################

# data
plot(fulldata_ts)

################
# Formal Tests

# Augmented Dickey-Fuller (ADF) Test
print(adf.test(fulldata_ts))
# Dickey-Fuller = -1.8533, Lag order = 9, p-value = 0.6404

# Kwiathlowski Philips Schmidt Shin Test (KPSS)
print(kpss.test(fulldata_ts))
# KPSS Level = 2.5962, Truncation lag parameter = 6, p-value = 0.01

## For the ADF test the p-value is 0.6404 so fail to reject the Null Hypothesis.
## This suggests the time series is non stationary
## For the KPSS test the p-value is 0.01 and so we reject the Null Hypothesis (opposites).
## This also suggests the time series is non stationary.
### So we will look to transform the data to obtain a stationary time series.

################
# Graphical Tools
# Use ACF and PACF to look for stationary
ggtsdisplay(fulldata_ts)

################
# Fix the stationary issues

acf2(fulldata_ts)
## ACF shows very slow decay with clear seasonal component
## Some lags outside the range in PACF

# Use logs to remove variability
fulldata_ts_log <- log(fulldata_ts)
autoplot(fulldata_ts_log)
autoplot(decompose(fulldata_ts_log))
acf2(fulldata_ts_log)
## Using logs helps to reduce the height of the seasonality but it still has slow
## decay.
## Improves quiet a bit by reducing the height of the lags outside of the ranges.

# Use Differencing to remove trend
fulldata_ts_log_diff <- diff(fulldata_ts_log)
autoplot(fulldata_ts_log_diff)
autoplot(decompose(fulldata_ts_log_diff))
## Seasonality looks like it has the same two seasons. The random component still has
## the two periods and I still need to look to fix/capture this.
acf2(fulldata_ts_log_diff) 
## This does look better, the decay has gone but we are still seeing a seaonal element,
## perhaps this is the weekly seasonality we have in the dats
# Not so much an improvement.

# Use Differencing to remove seasonal 
fulldata_ts_log_diff_sdiff <- diff(fulldata_ts_log_diff, differences = 7)
autoplot(fulldata_ts_log_diff_sdiff)
autoplot(decompose(fulldata_ts_log_diff_sdiff))
acf2(fulldata_ts_log_diff_sdiff)
## There is some reduction in the height of the lag lines but not enough

# Try something else
fulldata_ts_log_diff_sdiff_lag7 <- diff(fulldata_ts_log_diff, lag=7, differences = 7)
autoplot(fulldata_ts_log_diff_sdiff_lag7)
autoplot(decompose(fulldata_ts_log_diff_sdiff_lag7))
acf2(fulldata_ts_log_diff_sdiff_lag7)
## Big improvement but still see a seasonal element in the lags in Both ACF and PACF
# This is a model worth considering to compare the AIC and MAE values
model_fulldata_1 <- arima(fulldata_ts_log_diff_sdiff_lag7)
summary(model_fulldata_1)
## AIC VALUE: 4737.47 
## MAE VALUE: 3.125646

### Try things = 
fulldata_ts_diff <- diff(fulldata_ts)
autoplot(fulldata_ts_diff)
autoplot(decompose(fulldata_ts_diff))
acf2(fulldata_ts_diff)
## No improvement

fulldata_ts_diff_diff <- diff(fulldata_ts_diff)
autoplot(fulldata_ts_diff_diff)
autoplot(decompose(fulldata_ts_diff_diff))
acf2(fulldata_ts_diff_diff)
## No improvement

fulldata_ts_log_diff_sdiff <- diff(fulldata_ts_log_diff_diff, differences = 7)
autoplot(fulldata_ts_log_diff_sdiff)
autoplot(decompose(fulldata_ts_log_diff_sdiff))
acf2(fulldata_ts_log_diff_sdiff)
## No improvement

fulldata_ts_log <- log(fulldata_ts)
fulldata_ts_log_sdiff <- diff(fulldata_ts_log, differences=7)
autoplot(fulldata_ts_log)
acf2(fulldata_ts)
## No improvement

fulldata_ts_log <- log(fulldata_ts)
fulldata_ts_log_sdiff <- diff(fulldata_ts_log, differences=7)
autoplot(fulldata_ts_log)
acf2(fulldata_ts)
## No improvement

################
# Finally Improvement 

fulldata_ts_log_diff_sdiff_sdiff_lag7 <- diff(fulldata_ts_log_diff_sdiff , lag=7, differences = 7)
autoplot(fulldata_ts_log_diff_sdiff_sdiff_lag7)
autoplot(decompose(fulldata_ts_log_diff_sdiff_sdiff_lag7))
acf2(fulldata_ts_log_diff_sdiff_sdiff_lag7)
### Improvement to seasonality finally
model_fulldata_2 <- arima(fulldata_ts_log_diff_sdiff_sdiff_lag7)

## AIC VALUE: 10883.82 
## MAE VALUE: 168.4057
plot(model_fulldata_2 )
# Check formal tests
print(adf.test(fulldata_ts_log_diff_sdiff_sdiff_lag7))
print(kpss.test(fulldata_ts_log_diff_sdiff_sdiff_lag7))

################
#  Choose Appropriate ACF and PACF parameters for the ARIMA model
fulldata_ts_log_diff_sdiff_sdiff_lag7
acf2(fulldata_ts_log_diff)
# p = 3
# q = 1

acf2(fulldata_ts_log_diff_sdiff_sdiff_lag7)
# p = 3
# q = 1

## ARIMA(1,1,1)(3,2,1)[7]
## Make Model
fulldata_model <- arima(fulldata_ts_log_diff_sdiff_sdiff_lag7, order = c(3,1,1), seasonal = list(order = c(1,2,1), period = 7))

fulldata_model <- arima(fulldata_ts, order = c(3,1,1), seasonal = list(order = c(1,2,1), period = 7))
plot(fulldata_model)
forecast_fulldata_model <- forecast(fulldata_model)
plot(forecast_fulldata_model)


################
# Wave 1
################

# data
plot(wave_1_ts)

################
# Formal Tests

# Augmented Dickey-Fuller (ADF) Test
print(adf.test(wave_1_ts))
# Dickey-Fuller = -1.4706, Lag order = 6, p-value = 0.799
# alternative hypothesis: stationary

# Kwiathlowski Philips Schmidt Shin Test (KPSS)
print(kpss.test(wave_1_ts))
# KPSS Level = 4.1647, Truncation lag parameter = 5, p-value = 0.01

## For the ADF test the p-value is 0.799 so fail to reject the Null Hypothesis.
## This suggests the time series is non stationary
## For the KPSS test the p-value is 0.01 and so we reject the Null Hypothesis (opposites).
## This also suggests the time series is non stationary.
### So we will look to transform the data to obtain a stationary time series.

################
# Graphical Tools
# Use ACF and PACF to look for stationary

ggtsdisplay(wave_1_ts)
## You can see in the top plot there is Trend and Seasonality
## The ACF plot is also a very slow decay in the ACF which also shows its not stationary.

################
# Fix the stationary issues

# diff once on trend component # First Ordering Differencing
wave_1_ts_diff <- diff(wave_1_ts)
autoplot(decompose(wave_1_ts_diff))
acf2(wave_1_ts_diff) 
## Much better, we can see the ACF is looking better but it still has a seasonality
## We can also see on the top plot that the trend is gone but there is still seasonality
## This is expected as we have a seasonality component as well.

# diff again on the seasonality component # Second Order Differing.
wave_1_ts_diff_sdiff <- diff(wave_1_ts_diff, differences = 7)
autoplot(decompose(wave_1_ts_diff_sdiff))
## This is a big improvement but looks like it still needs work.
acf2(wave_1_ts_diff_sdiff)
## There is still a spike at values 1,2,3.
## Perhaps we need to further reduce the seasonality again

# seasonal diff again on the seasonality component
# As this is something we have seen in the fulldata models
wave_1_ts_diff_sdiff_sdiff <- diff(wave_1_ts_diff_sdiff, differences = 7)
autoplot(decompose(wave_1_ts_diff_sdiff_sdiff ))
acf2(wave_1_ts_diff_sdiff_sdiff)
## A second round of seasonal differencing has not made a difference
## There appears to be some residual seasonality left in the data that is 
## difficult to remove.
## This makes no difference so I would not add it to the model.


################
# Additional attempts to deal with the regular peaks in the ACF plot.

# box-cox - DIDNT WORK
lambda <- BoxCox.lambda(wave_1_ts)
wave_1_ts_bc <- BoxCox(wave_1_ts, lambda)
wave_1_ts_bc_diff <- diff(wave_1_ts_bc)
wave_1_ts_bc_diff_sdiff <- diff(wave_1_ts_bc_diff, differences = 7)

autoplot(decompose(wave_1_ts_bc))
acf2(wave_1_ts_bc)
autoplot(decompose(wave_1_ts_bc_diff ))
acf2(wave_1_ts_bc_diff )
autoplot(decompose(wave_1_ts_bc_diff_sdiff))
acf2(wave_1_ts_bc_diff_sdiff)

wave_1_ts_diff_sdiff_lag7 <- diff(wave_1_ts_diff, lag=7, differences=7)
autoplot(decompose(wave_1_ts_diff_sdiff_lag7 ))
acf2(wave_1_ts_diff_sdiff_lag7)

wave_1_ts_diff_sdiff_sdiff_lag7 <- diff(wave_1_ts_diff_sdiff, lag=7, differences=7)
autoplot(decompose(wave_1_ts_diff_sdiff_sdiff_lag7 ))
acf2(wave_1_ts_diff_sdiff_sdiff_lag7)

wave_1_ts_log <- log(wave_1_ts)
wave_1_ts_log_diff <- diff(wave_1_ts_log)
wave_1_ts_log_diff_sdiff <- diff(wave_1_ts_log_diff, differences=7)
autoplot(decompose(wave_1_ts_log_diff_sdiff ))
acf2(wave_1_ts_log_diff_sdiff)

# Best model 
wave_1_ts_log_diff_sdiff_sdiff <- diff(wave_1_ts_log_diff_sdiff, differences=7)
autoplot(decompose(wave_1_ts_log_diff_sdiff_sdiff ))
acf2(wave_1_ts_log_diff_sdiff_sdiff)
## This is the best model with a very good ACF and PACF that no longer shows a 
## seaonally pattern.

##############
# formal tests.
# Augmented Dickey-Fuller (ADF) Test
print(adf.test(wave_1_ts_log_diff_sdiff_sdiff ))

# Kwiathlowski Philips Schmidt Shin Test (KPSS)
print(kpss.test(wave_1_ts_log_diff_sdiff_sdiff))

################
#  Choose Appropriate ACF and PACF parameters for the ARIMA model
wave_1_ts_log_diff_sdiff_sdiff
acf2(wave_1_ts_log_diff)
# p = 1
# q = 1

acf2(wave_1_ts_log_diff_sdiff_sdiff)
# p = 2
# q = 1

## ARIMA(1,1,1)(3,2,1)[7]
## Make Model
wave_1_model <- arima(wave_1_ts_log_diff_sdiff_sdiff, order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 7))

wave_1_model <- arima(wave_1_ts,  order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 7))
forecast_wave_1_model <- forecast(wave_1_model)
autoplot(forecast_wave_1_model)
  
################
# Wave 2
################

# data
plot(wave_2_ts)

################
# Formal Tests

# Augmented Dickey-Fuller (ADF) Test
print(adf.test(wave_2_ts))
# Dickey-Fuller = -1.7746, Lag order = 7, p-value = 0.6724

# Kwiathlowski Philips Schmidt Shin Test (KPSS)
print(kpss.test(wave_2_ts))
# KPSS Level = 4.1647, Truncation lag parameter = 5, p-value = 0.01

## Once again, both tests show non-stationary and need to deal with.

################
# Graphical Tools
# Use ACF and PACF to look for stationary

ggtsdisplay(wave_2_ts)

################
# Fix the stationary issues

wave_2_ts_log <- log(wave_2_ts)
autoplot(decompose(wave_2_ts_log))
acf2(wave_2_ts_log)

wave_2_ts_log_diff <- diff(wave_2_ts_log)
autoplot(decompose(wave_2_ts_log_diff))
acf2(wave_2_ts_log_diff)

wave_2_ts_log_diff_sdiff <- diff(wave_2_ts_log_diff , differences = 7)
autoplot(decompose(wave_2_ts_log_diff_sdiff))
acf2(wave_2_ts_log_diff_sdiff)

wave_2_ts_log_diff_sdiff_sdiff7 <- diff(wave_2_ts_log_diff_sdiff, differences=7)
autoplot(decompose(wave_2_ts_log_diff_sdiff_sdiff7))
acf2(wave_2_ts_log_diff_sdiff_sdiff7)

wave_2_ts_log_diff_sdiff_sdiff7_lags7 <- diff(wave_2_ts_log_diff_sdiff_sdiff7, lags = 7)
autoplot(decompose(wave_2_ts_log_diff_sdiff_sdiff7_lags7))
acf2(wave_2_ts_log_diff_sdiff_sdiff7_lags7)

################
#  Choose Appropriate ACF and PACF parameters for the ARIMA model
acf2(wave_2_ts_log_diff)
# p = 1
# q = 1

acf2(wave_2_ts_log_diff_sdiff_sdiff7_lags7)
# p = 2
# q = 1

## ARIMA(1,1,1)(2,2,1)[7]
## Make Model
wave_2_model <- arima(wave_2_ts_log_diff_sdiff_sdiff7_lags7, order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 7))

wave_2_model <- arima(wave_2_ts,  order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 7))
forecast_wave_2_model <- forecast(wave_2_model)
autoplot(forecast_wave_2_model)

################
# Wave 3
################

# data
plot(wave_3_ts)

################
# Formal Tests

# Augmented Dickey-Fuller (ADF) Test
print(adf.test(wave_3_ts))
# Dickey-Fuller = -2.4946, Lag order = 5, p-value = 0.3698

# Kwiathlowski Philips Schmidt Shin Test (KPSS)
print(kpss.test(wave_3_ts))
# KPSS Level = 2.2276, Truncation lag parameter = 4, p-value = 0.01

################
# Graphical Tools
# Use ACF and PACF to look for stationary

ggtsdisplay(wave_3_ts)

################
# Fix the stationary issues

autoplot(decompose(wave_3_ts))
acf2(wave_3_ts)

wave_3_ts_diff <- diff(wave_3_ts)
autoplot(decompose(wave_3_ts_diff))
acf2(wave_3_ts_diff)

wave_3_ts_diff_sdiff <- diff(wave_3_ts_diff, differences = 7)
autoplot(decompose(wave_3_ts_diff_sdiff))
acf2(wave_3_ts_diff_sdiff)

wave_3_ts_diff_sdiff_sdiff <- diff(wave_3_ts_diff_sdiff, differences = 7)
autoplot(decompose(wave_3_ts_diff_sdiff_sdiff))
acf2(wave_3_ts_diff_sdiff_sdiff)

wave_3_ts_diff_sdiff_sdiff_lag7 <- diff(wave_3_ts_diff_sdiff_sdiff, lag = 7)
autoplot(decompose(wave_3_ts_diff_sdiff_sdiff_lag7))
acf2(wave_3_ts_diff_sdiff_sdiff_lag7)

wave_3_ts_log <- log(wave_3_ts)
wave_3_ts_log_diff <- diff(wave_3_ts_log) 
wave_3_ts_log_diff_sdiff <- diff(wave_3_ts_log_diff, differences=7)
wave_3_ts_log_diff_sdiff_sdiff <- diff(wave_3_ts_log_diff_sdiff, differences=7)

################
#  Choose Appropriate ACF and PACF parameters for the ARIMA model
acf2(wave_3_ts_log_diff)
# p = 1
# q = 1

acf2(wave_3_ts_log_diff_sdiff_sdiff )
# p = 2
# q = 1

## ARIMA(1,1,1)(2,2,1)[7]
## Make Model
wave_3_model <- arima(wave_2_ts_log_diff_sdiff_sdiff7_lags7, order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 7))

wave_3_model <- arima(wave_3_ts,  order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 7))
forecast_wave_3_model <- forecast(wave_3_model)
autoplot(forecast_wave_3_model)

################
################
# Choose three models you think are good candiates for the dataset and analyse
# the residuals
################
################

# Best Models:

# Full data
fulldata_ts_log_sdiff_model <- arima(fulldata_ts_log_sdiff)
summary(fulldata_ts_log_sdiff_model)

fulldata_ts_log_diff_sdiff_model <- arima(fulldata_ts_log_diff_sdiff)
summary(fulldata_ts_log_diff_sdiff_model)

fulldata_ts_log_diff_sdiff_sdiff_lag7_model <- arima(fulldata_ts_log_diff_sdiff_sdiff_lag7)
summary(fulldata_ts_log_diff_sdiff_sdiff_lag7_model)


# Wave 1 Model
wave_1_ts_diff_sdiff_sdiff_model <- arima(wave_1_ts_diff_sdiff_sdiff)
summary(wave_1_ts_diff_sdiff_sdiff_model)

wave_1_ts_diff_sdiff_sdiff_lag7_model <- arima(wave_1_ts_diff_sdiff_sdiff_lag7)
summary(wave_1_ts_diff_sdiff_sdiff_lag7_model)

wave_1_ts_log_diff_sdiff_sdiff_model <- arima(wave_1_ts_log_diff_sdiff_sdiff)
summary(wave_1_ts_log_diff_sdiff_sdiff_model)

# Wave 2 Model

wave_2_ts_log_diff_sdiff_model <- arima(wave_2_ts_log_diff_sdiff)
summary(wave_2_ts_log_diff_sdiff_model)

wave_2_ts_log_diff_sdiff_sdiff_model <- arima(wave_2_ts_log_diff_sdiff_sdiff7)
summary(wave_2_ts_log_diff_sdiff_sdiff_model )

wave_2_ts_log_diff_sdiff_sdiff7_lags7_model <- arima(wave_2_ts_log_diff_sdiff_sdiff7_lags7)
summary(wave_2_ts_log_diff_sdiff_sdiff7_lags7_model)

# Wave 3 Model
wave_3_ts_diff_sdiff_sdiff_model <- arima(wave_3_ts_diff_sdiff_sdiff)
summary(wave_3_ts_diff_sdiff_sdiff_model)

wave_3_ts_diff_sdiff_sdiff_lag7_model <- arima(wave_3_ts_diff_sdiff_sdiff_lag7)
summary(wave_3_ts_diff_sdiff_sdiff_lag7_model)

wave_3_ts_log_diff_sdiff_sdiff_model <- arima(wave_3_ts_log_diff_sdiff_sdiff)
summary(wave_3_ts_log_diff_sdiff_sdiff_model)


################
################
# Auto.ARIMA
################
################

# Find model proposed by Auto.ARIMA and discuss
# Is it good. Are your models better? What is missing? Etc?#

################
# Full Data
################

# Look at generating an Auto.ARIMA model with the full_data period
fulldata_aa_model <- auto.arima(fulldata_ts)
summary(fulldata_aa_model)
## ARIMA(3,1,3)(0,1,0)[365] 
## AIC=13043.6 

fulldata_aa_model_seasonal <- auto.arima(fulldata_ts, seasonal=TRUE)
summary(fulldata_aa_model_seasonal)
## ARIMA(3,0,2)(0,1,2)[7]
## AIC=21572.28

################
# wave 1
################

# Look at generating an Auto.ARIMA model with Wave 1
wave_1_aa_model <- auto.arima(wave_1_ts)
summary(wave_1_aa_model)
## ARIMA(1,0,1)(0,1,1)[7] with drift  
## AIC=6637.4 

wave_1_aa_model_seasonal <- auto.arima(wave_1_ts, seasonal=TRUE)
summary(wave_1_aa_model_seasonal)
## ARIMA(1,0,1)(0,1,1)[7] 
## AIC=6637.4

################
# Wave 2
################

# Look at generating an Auto.ARIMA model with Wave 2
wave_2_aa_model <- auto.arima(wave_2_ts)
summary(wave_2_aa_model)
## ARIMA(3,0,2)(1,1,2)[7] 
## AIC=10093.69

wave_2_aa_model_seasonal <- auto.arima(wave_2_ts, seasonal=TRUE)
summary(wave_2_aa_model_seasonal)
## ARIMA(3,0,2)(1,1,2)[7] 
## AIC=10093.69 

################
# wave 3
################

# Look at generating an Auto.ARIMA model with Wave 3
wave_3_aa_model <- auto.arima(wave_3_ts)
summary(wave_3_aa_model)
## ARIMA(1,1,3)(0,1,1)[7] 
## AIC=4141.33

wave_3_aa_model_seasonal <- auto.arima(wave_3_ts, seasonal=TRUE)
summary(wave_3_aa_model_seasonal)
## ARIMA(3,0,2)(1,1,2)[7] 
## AIC=4141.33 

##################################################################
# Forecast: Make a 4 week forecast for both Classical and ARIMA models
##################################################################

################
# Classical Model
################

# Make 4 week predication

# Carry out prediction for multiplicative and additive scenarios and compare the results.

################
# Full Data

# Look at Holts Winter
fulldata_ts_hw <- hw(fulldata_ts)
forecast_fulldata_ts_hw <- forecast(fulldata_ts_hw)
autoplot(forecast_fulldata_ts_hw)

fulldata_ts_hw_mul <- hw(fulldata_ts, seasonal ="multiplicative")
forecast_fulldata_ts_hw_mul<- forecast(fulldata_ts_hw_mul)
autoplot(forecast_fulldata_ts_hw_mul)

################
# Wave 1

# Look at Holts Winter
wave_1_ts_hw <- hw(wave_1_ts)
forecast_wave_1_ts_hw <- forecast(wave_1_ts_hw)
autoplot(forecast_wave_1_ts_hw )

wave_1_ts_hw_mul <- hw(wave_1_ts, seasonal ="multiplicative")
forecast_wave_1_ts_hw_mul<- forecast(wave_1_ts_hw_mul)
autoplot(forecast_wave_1_ts_hw_mul)

################
# Wave 2

# Look at Holts Winter
wave_2_ts_hw <- hw(wave_2_ts)
forecast_wave_2_ts_hw <- forecast(wave_2_ts_hw)
autoplot(forecast_wave_2_ts_hw )

wave_2_ts_hw_mul <- hw(wave_2_ts, seasonal ="multiplicative")
forecast_wave_2_ts_hw_mul<- forecast(wave_2_ts_hw_mul)
autoplot(forecast_wave_2_ts_hw_mul)

################
# Wave 3
# Look at Holts Winter
wave_3_ts_hw <- hw(wave_3_ts)
forecast_wave_3_ts_hw <- forecast(wave_3_ts_hw)
autoplot(forecast_wave_3_ts_hw )

wave_3_ts_hw_mul <- hw(wave_3_ts, seasonal ="multiplicative")
forecast_wave_3_ts_hw_mul<- forecast(wave_3_ts_hw_mul)
autoplot(forecast_wave_3_ts_hw_mul)

################
# ARIMA models
################

# Make 4 week predication for both my model and for the Auto.ARIMA model

# Compare the results

################
# Full Data

# Manual ARIMA Model
fulldata_model <- arima(fulldata_ts, order = c(3,1,1), seasonal = list(order = c(1,2,1), period = 7))
forecast_fulldata_model <- forecast(fulldata_model)
autoplot(forecast_fulldata_model)
summary(forecast_fulldata_model)
# RMSE 108047.1

# Auto.ARIMA model
fulldata_aa_model_seasonal
forecast_fulldata_model_seasonl <- forecast(fulldata_aa_model_seasonal)
autoplot(forecast_fulldata_model_seasonl)
summary(forecast_fulldata_model_seasonl)
# RMSE 101529.7

################
# Wave 1

# Manual ARIMA Model
wave_1_model <- arima(wave_1_ts,  order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 7))
forecast_wave_1_model <- forecast(wave_1_model)
autoplot(forecast_wave_1_model)

# Auto.ARIMA model
wave_1_aa_model
forecast_wave_1_aa_model <- forecast(wave_1_aa_model)
autoplot(forecast_wave_1_aa_model)
summary(forecast_wave_1_aa_model)

################
# Wave 2

# Manual ARIMA Model
wave_2_model <- arima(wave_2_ts,  order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 7))
forecast_wave_2_model <- forecast(wave_2_model)
autoplot(forecast_wave_2_model)

# Auto.ARIMA model
wave_2_aa_model
forecast_wave_2_aa_model <- forecast(wave_2_aa_model)
autoplot(forecast_wave_2_aa_model)
summary(forecast_wave_2_aa_model)


################
# Wave 3

# Manual ARIMA Model
wave_3_model <- arima(wave_3_ts,  order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 7))
forecast_wave_3_model <- forecast(wave_3_model)
autoplot(forecast_wave_3_model)

# Auto.ARIMA model
wave_3_aa_model
forecast_wave_3_aa_model <- forecast(wave_3_aa_model)
autoplot(forecast_wave_3_aa_model)
summary(forecast_wave_3_aa_model)


################
# Power of Prediction
################

# Check the power of prediction of your models by forecasting the last periods 
# of your time series and compare them with the actual observed values for such 
# period you have predicted.

length(usdata_daily_ts) # 839
# Train = 839/100 *80 = 672
# Test = 839-672 = 167
train_data <- usdata_daily_ts[1:839]
test_data <- usdata_daily_ts[673:839]
# Use all the data as it does not say test and train

################
# Fulldata

# Check the full data
# Manual ARIMA model
# RMSE 108047.1
# Auto.ARIMA model
# RMSE 101529.7

#Mode the train data
model_usdata_daily_ts_forecast <- arima(fulldata_ts, order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 7))
summary(model_usdata_daily_ts_forecast)
# fulldata_ts_rmse
# 111041.7

# Check the last 10% of data before end.
ten_per_size <- floor(length(fulldata_ts)*0.9)
test_usdata_daily_ts <- tail(fulldata_ts, length(fulldata_ts) - ten_per_size)

model_usdata_daily_ts_ten_per_forecast <- arima(test_usdata_daily_ts, order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 7))
summary(model_usdata_daily_ts_ten_per_forecast)

# 10per_fulldata_ts_rmse
# 87501.62

################
# Wave 1

# Manual ARIMA model
# RMSE =61163.89
# Auto.ARIMA model
# RMSE =61163.89

#Mode the train data
model_wave_1_ts_forecast <- arima(wave_1_ts, order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 7))
summary(model_wave_1_ts_forecast)
# 62089.5

# Check the last 10% of data before end.
ten_per_size <- floor(length(wave_1_ts)*0.9)
test_wave_1_ts <- tail(wave_1_ts, length(wave_1_ts) - ten_per_size)

model_wave_1_ts_ten_per_forecast <- arima(test_wave_1_ts, order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 7))
summary(model_wave_1_ts_ten_per_forecast)
# 105486.5

################
# Wave 2

# Manual ARIMA model
# RMSE = 114641.3
# Auto.ARIMA model
# RMSE = 114641.3

#Mode the train data
model_wave_2_ts_forecast <- arima(wave_2_ts, order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 7))
summary(model_wave_2_ts_forecast)
# 126195.6

# Check the last 10% of data before end.
ten_per_size <- floor(length(wave_2_ts)*0.9)
test_wave_2_ts <- tail(wave_2_ts, length(wave_2_ts) - ten_per_size)

model_wave_2_ts_ten_per_forecast <- arima(test_wave_2_ts, order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 7))
summary(model_wave_2_ts_ten_per_forecast)
# 221604.5

################
# Wave 3

# Manual ARIMA model
# RMSE = 93676.55
# Auto.ARIMA model
# RMSE = 93676.55

#Mode the train data
model_wave_3_ts_forecast <- arima(wave_3_ts, order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 7))
summary(model_wave_3_ts_forecast)
# 97245.83

################
# End of code
################

