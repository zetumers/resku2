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

#this melts wide files without respect to order
aua_melt3 <- function(wide_df){
        long_df <- melt(wide_df, id.vars = c("record_id"))
        return(long_df)
}


aua_melt2 <- function(wide_df, rosetta){
        long_df <- melt(wide_df, id = c("record_id","order"))
        long_df$variable <- as.character(long_df)
        long_df %>%
                left_join(rosetta, by = c("variable" = "RC_Label")) %>%
                select(-Institution_Label, -Institution_Key, -Multi) -> long_df
        return(long_df)
}