library(plyr)
library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)
library(readxl)
library(bezier)
library(reshape2)
library(hash)
library(grid)

filename <<- "resku_11.08.18.csv"

aua_read_in <-function(filename){
        df <- read.csv(filename)
        df <- tbl_df(df)
        return(df)
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

#just melt by record and order
aua_melt1 <- function(wide_df){
        long_df <- melt(wide_df, id.vars = c("record_id","order"))
        return(long_df)
}

#turn everything to character
aua_coerce_to_string <- function(wide_df){
        n <- ncol(wide_df)
        for(i in 1:n){
                wide_df[,i] <- as.character(wide_df[,i])
        }
        return(wide_df)
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

aua_pull_resku <- function(){
        resku <<- aua_read_in(filename)
        resku1 <<- tbl_df(aua_set_order(resku))
        resku2 <<- aua_melt1(resku1)
        resku3 <<- aua_elim_na(resku2)
        resku_df <<- aua_elim_blank(resku3)
        small <<- filter(resku3, record_id %in% 3:100)
}

