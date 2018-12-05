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
library(hash)

###########SURGERY PERFORMED##########
#which surgery is performed.
#note - there may be multiple surgeries performed.
#each patient may have multiple surgeries, even during
#the same day.
aua_op_id <- function(long_df){
        long_df %>%
                filter(grepl("stone_op",variable)) %>%
                mutate(stone_op = aua_pull_nums(variable, "stone_op")) %>%
                filter(value == "1") %>%
                select(-variable) -> long_df
        return(long_df)
}


############

#How often is nephrectomy performed.
#27. Clinicians may perform nephrectomy when the involved kidney
#has negligible function in patients requiring treatment. 
#(Index Patients 1-14) 
#Conditional Recommendation; Evidence Level Grade C

############SYMPTOMATIC##########

#symptomatic during np encounter.
aua_sxs_np <- function(long_df){
        long_df %>%
                filter(variable == "stone_symptomatic") %>%
                select(-variable) %>%
                mutate(value = aua_sxs_coerce(value)) %>%
                rename(sxs_np = value) -> long_df
        return(long_df)
}

#symptomatic after po encounter.
#ABANDONED. NO PO SYMPTOMS SEEN
aua_sxs_po <- function(long_df){
        long_df %>%
                filter(variable == "stone_po_symptoms") %>%
                select(-variable) %>%
                mutate(value = aua_sxs_coerce_po(value)) %>%
                rename(sxs_po = value) -> long_df
        return(long_df)
}

############

############STONE BURDEN##########

#can either get the first ones or all of them.
aua_small_size <- function(long_df, func){
        l <- list(long_df,"1","small")
        result_df <- do.call(func, l)
        return(result_df)
}

#can either get the first ones or all of them.
aua_med_size <- function(long_df, func){
        l <- list(long_df,"2","med")
        result_df <- do.call(func, l)
        return(result_df)
}

#can either get the first ones or all of them.
aua_large_size <- function(long_df, func){
        l <- list(long_df,"3","large")
        result_df <- do.call(func, l)
        return(result_df)
}

#can either get the first ones or all of them.
aua_no_size <- function(long_df, func){
        l <- list(long_df,"4","no")
        result_df <- do.call(func, l)
        return(result_df)
}

#can either get the first ones or all of them.
aua_randall_size <- function(long_df, func){
        l <- list(long_df,"5","randall")
        result_df <- do.call(func, l)
        return(result_df)
}

#GUIDELINE 21 22 25.

#21. In symptomatic patients with a total non-lower pole renal stone
#burden ≤ 20 mm, clinicians may offer SWL or URS. (Index Patient 7)
#Strong Recommendation; Evidence Level Grade B

#22. In symptomatic patients with a total renal stone burden >20 mm,
#clinicians should offer PCNL as first-line therapy. (Index Patient 8)
#Strong Recommendation; Evidence Level Grade C

#25. In patients with total renal stone burden >20 mm, clinicians
#should not offer SWL as first-line therapy. (Index Patient 8)
#Moderate Recommendation; Evidence Level Grade C



############???? plan_np ?= WHAT IS DONE FOR ADMISSION
            # plan_fupo = WHAT IS DONE FOR FU PTS #############

#Give the visit and the plan
#np = new patient, fu = follow-up, fupo = ?????
#FOR THESE THREE ENCOUNTERS...
#8 = surgery, 6 = MET, 7 = obersvation, 4 = med change,
#9 = continuing medication, 7 = observation,
#10 = declined surgery
#11 = declined metabolic evaluation
aua_visit_plan <- function(long_df, visit, plan){
        visit <- c("plan_", visit)
        long_df %>%
                group_by(record_id) %>%
                filter(variable == visit) %>%
                filter(value == plan) %>%
                summarize(min(order)) -> long_df
        names(long_df)[2] <- "min_small_stone"
        return(long_df)
}

