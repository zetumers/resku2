library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(stringr)
library(readxl)
library(stringr)
library(bezier)
library(reshape2)

#settings

dir <- "/Users/samuelzetumer/Desktop/AUA TEST"
filename <- "RegistryForStonesOfT_DATA_2018-10-07_1211.csv"
rosetta <- "Trial_Rosetta V6.xlsx"
#

aua_read_in <-function(filename){
        df <- read.csv(filename)
        df <- tbl_df(df)
        return(df)
}

aua_rosetta_in <- function(rosetta){
        aua_ros <- read_excel(rosetta)
        return(aua_ros)
}

#assumption 1: events are ordered chronologically.

aua_set_order <- function(wide_df){
        wide_df %>%
                group_by(record_id) %>%
                mutate(order = 1:length(redcap_event_name)) -> wide_df
        return(wide_df)
}

aua_coerce_to_string <- function(wide_df){
        n <- ncol(wide_df)
        for(i in 1:n){
                wide_df[,i] <- as.character(wide_df[,i])
        }
        return(wide_df)
}

aua_melt2 <- function(wide_df, rosetta){
        long_df <- melt(wide_df, id = c("record_id","order"))
        long_df$variable <- as.character(long_df)
        long_df %>%
                left_join(rosetta, by = c("variable" = "RC_Label")) %>%
                select(-Institution_Label, -Institution_Key, -Multi) -> long_df
        return(long_df)
}

aua_melt1 <- function(wide_df){
        long_df <- melt(wide_df, id.vars = c("record_id","order"))
        long_df$variable <- as.character(long_df)
        return(long_df)
}

#basic program
setwd(dir)
resku <- aua_read_in(filename)
rosetta <- aua_rosetta_in(rosetta)
resku1 <- aua_set_order(resku)
resku2 <- aua_coerce_to_string(resku1)
molten <- aua_melt1(resku1)
