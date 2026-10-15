#!/usr/bin/env Rscript

## Reproducible Research
## Course Project 1

## plot1.R
## by Mark Chang

#library("data.table")
#library("ggplot2")


# Windows
setwd("C:/Users/mchang/Projects/RepData_PeerAssessment1")

# Mac
#setwd("/Users/mchang/Projects/ProgrammingAssignment6")

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

data <- read.csv("activity.csv",sep=",")
data$date <- as.Date(data$date,"%Y-%m-%d")
datag <- aggregate(steps ~ date, data=data,FUN=sum,na.rm=TRUE)

head(datag)
hist(datag$steps,main="Steps taken each day")
print("What is mean total number of steps taken per day?")
cat("Mean is ",mean(datag$steps))
print("")
cat("Median is ", median(datag$steps))
print("")

print("What is the average daily activity pattern?")


