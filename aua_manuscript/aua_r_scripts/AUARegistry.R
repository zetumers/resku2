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
#provides the column "order" to the wide
aua_set_order <- function(wide_df){
        wide_df %>%
                group_by(record_id) %>%
                mutate(order = 1:length(redcap_event_name)) %>%
                ungroup() -> wide_df
        return(wide_df)
}

#turn everything to character
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

#just melt by record and order
aua_melt1 <- function(wide_df){
        long_df <- melt(wide_df, id.vars = c("record_id","order"))
        return(long_df)
}

aua_melt3 <- function(wide_df){
        long_df <- melt(wide_df, id.vars = c("record_id"))
        return(long_df)
}

aua_elim_na <- function(long_df){
        long_df %>%
                filter(!is.na(value)) -> long_df
        return(long_df)
}

aua_elim_blank <- function(long_df){
        long_df %>%
                filter(!(value == "")) -> long_df
        return(long_df)
}

#basic program
#okay this is what the whole thing is going to be.
#each (record_id,order) pair is a unique line in the wide file.
setwd(dir)
resku <- aua_read_in(filename)
#rosetta <- aua_rosetta_in(rosetta)
resku1 <- aua_set_order(resku)
resku2 <- aua_melt1(resku1)
resku3 <- aua_elim_na(resku2)
resku4 <- tbl_df(aua_elim_blank(resku3))
small <- filter(resku3, record_id %in% 3:100)

