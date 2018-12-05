install.packages(c("plyr","tidyverse","ggmap",
                   "maps","mapdata","readxl",
                   "mapdata","readxl","bezier",
                   "reshape2","hash","grid"))

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

filename <- "data/resku_11.08.18.csv"

source("basic_func/to_be_sorted.R", echo = TRUE)

source("basic_func/pull.R", echo = TRUE)

source("basic_func/op.R", echo = TRUE)

source("basic_func/op_burden.R", echo = TRUE)

source("basic_func/is.R", echo = TRUE)

source("basic_func/img_np.R", echo = TRUE)

source("basic_func/eval.R", echo = TRUE)

source("basic_func/cas.R", echo = TRUE)

source("basic_func/aux.R", echo = TRUE)

source("basic_func/_init_.R", echo = TRUE)

aua_pull_resku(filename)

#resku_df and small are the dataframes (dfs) you'll be working with
#to get the wide df, simply use the variable resku_wide
