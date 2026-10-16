#!/usr/bin/env Rscript

## Reproducible Research
## Course Project 1

## plot1.R
## by Mark Chang

library("data.table")
library("ggplot2")
library("knitr")

# Windows
#setwd("C:/Users/mchang/Projects/RepData_PeerAssessment1")

# Mac
setwd("/Users/mchang/Projects/RepData_PeerAssessment1")

path <- getwd()
datasetpath <- file.path(path,"dataset")
datafilename <- "dataFiles.zip"

#if ( !dir.exists(datasetpath))
#{
#    dir.create(datasetpath)
#} else
#{
#   unlink(datasetpath, recursive = TRUE)
#}

# Download file
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
#download.file(url, file.path(path, datafilename))
#unzip(zipfile = datafilename, exdir = datasetpath)

activity <- read.csv("activity.csv",sep=",")
activity$date <- as.Date(activity$date,"%Y-%m-%d")

#Why does the aggregate function not return the correct graph when plotted?
#activityg <- aggregate(steps ~ date, data=activity,FUN=sum,na.rm=TRUE)

dailysteps <- tapply(activity$steps,activity$date,FUN=sum,na.rm=TRUE)

qplot(dailysteps)

head(dailysteps)
#hist(activityg$steps,main="Steps taken each day")
print("What is mean total number of steps taken per day?")
cat("Mean is ",mean(activityg$steps))
print("")
cat("Median is ", median(activityg$steps))
print("")

print("What is the average daily activity pattern?")

activity$interval <- sprintf("%04d", activity$interval)
activity$interval <- format(strptime(activity$interval, format="%H%M"), format = "%H:%M")

