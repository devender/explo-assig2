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
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {
    install.packages("ggplot2")
}
library(futile.logger)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)


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

#extract "emissions from coal combustion-related sources"
coal_comb_codes<-SCC %>% 
    filter( grepl(".*[C|c]omb.*[C|c]oal.*",Short.Name)) %>% 
    select(SCC)

#only extract where the source is one of coal comb
only_from_coal<-inner_join(NEI, coal_comb_codes, by=c("SCC"="SCC"))

only_from_coal_by_year <- only_from_coal %>% 
                            group_by(year) %>% 
                            summarise(Emissions.Tons=log10(sum(Emissions)))

png(file = "plot4.png")

p<-qplot(year,Emissions.Tons,data=only_from_coal_by_year,geom="line",
         ylab="Log10(Emissions)") +
    ggtitle("US emissions from coal combustion-related sources from 1999–2008")

print(p)
dev.off()

