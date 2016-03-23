# Script to make Plot 1 of the Assignment
# don't forget to setwd("/Users/Devender/explo-assig2")
rm(list=ls())
gc()

if("futile.logger" %in% rownames(installed.packages()) == FALSE) {
    install.packages("futile.logger")
}
if("data.table" %in% rownames(installed.packages()) == FALSE) {
    install.packages("data.table")
}
if("dplyr" %in% rownames(installed.packages()) == FALSE) {
    install.packages("dplyr")
}
if("lubridate" %in% rownames(installed.packages()) == FALSE) {
    install.packages("lubridate")
}

library(futile.logger)
library(data.table)
library(dplyr)
library(lubridate)

download_data <- function() {
    if(!file.exists("data")) { 
        flog.info("creating data dir");dir.create("data")
    }else{
        flog.info("data dir already exists");
    }
    
    if(!file.exists("data/dataset.zip")){ 
        flog.info("downloading dataset")
        fileUrl = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        download.file(fileUrl,"data/dataset.zip",method="curl") 
    }else{
        flog.info("dataset already downloaded")
    }
    
    if(!file.exists("data/Source_Classification_Code.rds") || 
       !file.exists("data/summarySCC_PM25.rds")){
        flog.info("extracting dataset")
        unzip(zipfile = "data/dataset.zip", exdir="data")
    }else{
        flog.info("dataset already extracted")
    }
}

read_date <- function(){
    flog.info("reading summary into global variable NEI")
    if(!exists("NEI")){
        NEI <<- readRDS("data/summarySCC_PM25.rds")
    }
    flog.info("reading class code into global variable SCC")
    if(!exists("SCC")){
        SCC <<- readRDS("data/Source_Classification_Code.rds")
    }
}

#download the required data
download_data()

#read the data
read_date()

# group by year and summarize save to new frame
total_emession_by_year<-NEI %>% group_by(year) %>% summarise(total_pm25=sum(Emissions))

#plot
#set up graphics device and plot
png(file = "plot1.png")
plot(total_emession_by_year,
     xlab="Year",
     ylab="Total Emisions",
     main="Year Over Year Trend of Total Emissions in US",
     col="green",
     lwd=3,
     type="l")
trend<-lm(total_emession_by_year$total_pm25~total_emession_by_year$year)
abline(trend)
legend("topright",c("TotalEmissions","Trend"),bty="l",col=c("green","black"),lwd=2)
dev.off()