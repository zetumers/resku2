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

############IMAGING TAKEN#########

#Transferred
#VERIFIED
#gives diagnosis on imaging np
aua_img_np <- function(long_df){
        long_df %>%
                #gives me only the "stone_op" variables
                filter(grepl("img_dx___",variable)) %>%
                #give me only the surgeries that actually happened
                filter(value == 1) %>%
                #replace the value with the number of the operation
                mutate(value = aua_pull_nums(variable, "img_dx")) %>%
                #get rid of the actual stone op part
                select(-variable) %>%
                #rename the value as the original variable
                dplyr::rename(img_np = value) %>%
                arrange(record_id, order) -> long_df
        return(long_df)
}

#VERIFIED
#gives diagnosis on imaging po
aua_img_po <- function(long_df){
        long_df %>%
                #gives me only the "stone_op" variables
                filter(grepl("imgpo___",variable)) %>%
                #give me only the surgeries that actually happened
                filter(value == 1) %>%
                #replace the value with the number of the operation
                mutate(value = aua_pull_nums(variable, "imgpo")) %>%
                #get rid of the actual stone op part
                select(-variable) %>%
                #rename the value as the original variable
                dplyr::rename(imgpo = value) %>%
                arrange(record_id, order) -> long_df
        return(long_df)
}

#gives np information on the specifics of the imaging
aua_img_loc_np <- function(long_df){
        long_df %>%
                #gives me only the "imgnp_r or l_loc" variables
                filter(grepl("imgnp_._loc___",variable)) %>%
                #give me only the imaging parts that are positive
                filter(value == 1) %>%
                #replace the value with the number of the image
                mutate(value = aua_pull_nums(variable, "imgnp_._loc")) %>%
                #rename the value as the original variable
                dplyr::rename(img_loc = value) %>%
                mutate(variable = sub("_loc___.{1,10}","",variable)) %>%
                mutate(variable = sub("imgnp_","",variable)) %>%
                arrange(record_id, order) -> long_df
        return(long_df)
}

aua_img_size_np <- function(long_df){
        long_df %>%
                #gives me only the "imgnp_r_loc" variables
                filter(grepl("imgnp_._size",variable)) %>%
                #replace the value with the number of the image                #rename the value as the original variable
                dplyr::rename(img_size = value) %>%
                mutate(variable = sub("_size","",variable)) %>%
                mutate(variable = sub("imgnp_","",variable)) %>%
                dplyr::rename(side = variable) %>%
                arrange(record_id, order) -> long_df
        return(long_df)
}

aua_img_num_np <- function(long_df){
        long_df %>%
                #gives me only the "imgnp_r_loc" variables
                filter(grepl("imgnp_._number",variable)) %>%
                #replace the value with the number of the image                #rename the value as the original variable
                dplyr::rename(img_num = value) %>%
                mutate(variable = sub("_number","",variable)) %>%
                mutate(variable = sub("imgnp_","",variable)) %>%
                dplyr::rename(side = variable) %>%
                arrange(record_id, order) -> long_df
        return(long_df)
}

############

############SURGERY PERFORMED##########

#VALIDATED w/ stone_op variable cluster, record_id < 10, and
#takes in the base long_df
#note - there may be multiple surgeries performed.
#each patient may have multiple surgeries, even during
#the same day.
aua_op_id <- function(long_df){
        long_df %>%
                #gives me only the "stone_op" variables
                filter(grepl("stone_op___",variable)) %>%
                #give me only the surgeries that actually happened
                filter(value == 1) %>%
                #replace the value with the number of the operation
                mutate(value = aua_pull_nums(variable, "stone_op")) %>%
                #get rid of the actual stone op part
                select(-variable) %>%
                #rename the value as the original variable
                dplyr::rename(stone_op = value) %>%
                arrange(record_id, order) -> long_df
        return(long_df)
}

#VERIFIED for record_id 3
aua_preop_dx <- function(long_df){
        long_df %>%
                #gives me only the "stone_op" variables
                filter(grepl("stone_dx_preop___",variable)) %>%
                #give me only the surgeries that actually happened
                filter(value == 1) %>%
                #replace the value with the number of the operation
                mutate(value = aua_pull_nums(variable, "stone_dx_preop")) %>%
                #get rid of the actual stone op part
                select(-variable) %>%
                #rename the value as the original variable
                dplyr::rename(stone_dx_preop = value) %>%
                arrange(record_id, order) -> long_df
        return(long_df)
}


#VERIFIED: 
#returns a list of patients
#their first operation identity
#and which encounter that was
#note: multiple operations performed during
#the same encounter will all be kept.
aua_first_op <- function(long_df){
        long_df %>%
                aua_op_id() %>%
                group_by(record_id) %>%
                filter(order == min(order)) -> result
        return(result)
}

############

############SYMPTOMATIC##########

#VALIDATED
#symptomatic during np encounter.
#return yes/no for patient's with first encounter.
aua_sxs_np <- function(long_df){
        long_df %>%
                filter(variable == "stone_symptomatic") %>%
                select(-variable) %>%
                mutate(value = aua_sxs_coerce(value)) %>%
                dplyr::rename(sxs_np = value) -> long_df
        return(long_df)
}

#ABANDONED. NO PO SYMPTOMS SEEN
#symptomatic after po encounter.
aua_sxs_po <- function(long_df){
        long_df %>%
                filter(variable == "stone_po_symptoms") %>%
                select(-variable) %>%
                mutate(value = aua_sxs_coerce_po(value)) %>%
                dplyr::rename(sxs_po = value) -> long_df
        return(long_df)
}

############

############STONE BURDEN##########

#get all the first stones.
aua_all_size <- function(long_df){
        long_df %>%
                group_by(record_id) %>%
                filter(variable == "stone_burden") -> long_df
        return(long_df)
}

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

#the first time the patient has stone of size "num"
aua_first_size<- function(long_df, num, name){
        long_df %>%
                group_by(record_id) %>%
                filter(variable == "stone_burden") %>%
                filter(value == num) %>%
                summarize(min(order)) -> long_df
        names(long_df)[2] <- paste(name,"_order", sep ='')
        return(long_df)
}

#returns all the times that stone size wwas given
aua_any_size <- function(long_df, num, name){
        long_df %>%
                group_by(record_id) %>%
                filter(variable == "stone_burden") %>%
                filter(value == num) %>%
                summarize(min(order)) -> long_df
        names(long_df)[2] <- paste(name,"_order", sep ='')
        return(long_df)
}

############


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