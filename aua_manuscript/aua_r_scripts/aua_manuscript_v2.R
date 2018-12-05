library(broom)

curse2_key <- c(6,17, 3,4,5,14,15,16,7,18,8,19)
curse2_value <- c(3,3,2,2,2,2,2,2,1,1,4,4)
curse2_df <- data.frame(key = curse2_key,
                        value = factor(curse2_value,
                                       labels = c("SWL","URS","PCNL","open")))

size_cascade <- list(cas_is_sxs,
                     cas_has_imgnp,
                     cas_has_ct_size)

########Exclusion criteria filter: produces resku_df2#######

#get age
age_df <- read.csv("data/pt_age.csv")