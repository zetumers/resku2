library(plyr)
library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)
library(readxl)
library(bezier)
library(reshape2)
library(hash)

aua_analysis_gl22_25 <- function(result, long_df, wide_df){
        
        setwd("/Users/samuelzetumer/Desktop/AUA TEST")
        
        dir.create("Guidelines 22-25")
        d <- getwd()
        setwd("./Guidelines 22-25")
        
        result$side_size_burden_op %>%
                filter(!is.na(img_size)) %>%
                group_by(record_id,side) %>%
                summarize(img_size = as.numeric(img_size)[1],
                          burden = as.numeric(burden)[1]) %>%
                filter(burden %in% c("1","2","3")) -> test3
        
        jpeg("Img Size vs Op Size Box.jpg", width = 600, height = 600)
        boxplot(img_size~burden, data = basic,
               main = "Correspondence between Burden on CT \n and Burden Recorded during Operation",
               xlab = "Burden during Op",
               ylab = "CT Burden (mm)")
        dev.off()
        
        jpeg("Img Size vs Op Size Dot.jpg", width = 500, height = 500)
        plot(img_size~burden, data = basic,
                main = "Correspondence between \n Burden on CT and Burden Recorded during Operation",
                xlab = "Burden during Op",ylab = "CT Burden (mm)")
        dev.off()
        
        result$side_size_burden_op %>%
                filter(!is.na(img_size))  %>%
                mutate(procedure = aua_bilat_op(as.numeric(stone_op))) %>%
                filter(procedure %in% c(3,4,5,6,7,8)) %>%
                mutate(proc_name = aua_op_name(procedure)) -> basic
         basic$img_size <- as.numeric(basic$img_size)

        a <- ggplot(basic, aes(x = img_size))
        a + geom_density(aes(y = ..count..,
                             color = proc_name)) +
                xlab("Stone Size by Imaging (mm)") +
                ylab("Count") +
                ggtitle("Count Distributions by Procedure")
        ggsave("Freq.jpg", plot = last_plot(), height = 7, width = 7*2.5)
        
        result$burden_and_op %>%
                mutate(procedure = as.character(aua_op_name((aua_bilat_op(as.numeric(result$burden_and_op$stone_op)))))) %>%
                mutate(burden = as.numeric(burden)) %>%
                arrange(burden) %>%
                mutate(burden = as.factor(aua_burden_name(burden))) -> second
        
        b <- ggplot(second, aes(burden, procedure))
        b + geom_jitter(width = .5, size=1) +
        xlab("Stone Burden") +
                ylab("Procedure") +
                ggtitle("Count Distributions by Procedure")
        ggsave("Burden vs Procedure.jpg", plot = last_plot(), height = 7, width = 7*2.5)
        
        result$large_stone_no_pcnl %>%
                filter(no_pcnl == TRUE) %>%
                select(record_id) -> third
        
        third <- third[!duplicated(third),]
        
        wide_df <- tbl_df(wide_df)
        wide_df%>%
                filter(record_id %in% third$record_id) -> no_pcnl
         
         write.csv(no_pcnl, "Heavy_Stone_No_PCNL.csv")
         
         long_df %>%
                 mutate(tier = 1) %>%
                 #present symptomatically
                 group_by(record_id) %>%
                 mutate(tier = tier + aua_is_sxs(variable,value)) %>%
                 #who has imaging on file
                 mutate(tier = tier + as.numeric((tier == 2))*any(grepl("imgnp_._size",variable))) %>%
                 #any patient with large sized stone on imaging as a new patient
                 mutate(tier = tier + aua_has_any_large(variable,value)*as.numeric(tier == 3)) -> third
                 #any patient with a large stone during a first operation
                 #mutate(tier = tier + as.numeric(tier == 3)*(record_id %in% aua_large_size(long_df, aua_first_size)$record_id)) %>%
                 #any patient from above whose first operation was not a pcnl
                 #ungroup() -> third
                 

        tiers <- c("All patients",
                   "...who present symptomatically",
                   "...who had imaging as a new patient",
                   "...and have > 20mm stone as np",
                   "...and have a large burden during the first surgery",
                   "...who's first operation is not a PCNL",
                   "...and who did not have any ",
                   "...")
        
        return(basic)
}

######do functions group by patient.
#evals return a new column
#is returns either the data frame or nothing
        #NB, using . in a summarize expression is a disaster. Way too slow.

#Transferred
#VALIDATED
#does the group have a symptomatic == 1
eval_sxs <- function(df){
        sxs <- any((df$variable == "stone_symptomatic") & (df$value == "1"))
        if(sxs == TRUE){
                df$sxs <- 1
        }
        else{df$sxs <- 0}
        return(df)
}

#transferred
is_sxs <- function(df){
        if(df$sxs[1] == 1){
                return(select(df, -sxs))
        }
        else{return(data.frame())}
}

#transferred
#VALIDATED
#does the patient have imaging at UCSF during new patient visit
is_np_img <- function(df){
        df %>%
                filter(grepl("img_np___",df$variable)) -> df1
        vars <- df1$variable
        vals <- df1$value
        if(all((!(vars == "img_np___8") &
            (vals == "0"))
           |
           ((vars == "img_np___8") &
            (vals == "1"))))
                {
                return(data.frame())
        }
        else(return(df))
}

######find encounter functions. is_np is_op is_postop is_fu######

#Transferred
is_np <- function(df){
        if((length(unique(df$record_id)) != 1) | (length(unique(df$order)) != 1)){
                print("ERROR: is_op on multiple encounters")
                return(df)
        }
        if(any(df$variable == "date_np")){
                return(df)
        }
        else{
                print("collapsed")
                return(data.frame())}
}

#transferred
is_op <- function(df){
        df1 <- df
        if((length(unique(df1$record_id)) != 1) | (length(unique(df1$order)) != 1)){
                print("ERROR: is_op on multiple encounters")
                return(df1)
        }
        
        if(any(df1$variable == "date_surgery_v2")){
                return(df1)
        }
        else(return(data.frame()))
}

#transferred
is_postop <- function(df){
        if((length(unique(df$record_id)) != 1) | (length(unique(df$order)) != 1)){
                print("ERROR: is_postop on multiple encounters")
                return(df)
        }
        if(any(df$variable == "date_fu_postop")){
                return(df)
        }
        else(return(data.frame()))
}

#Transferred
is_fu <- function(df){
        if((length(unique(df$record_id)) != 1) | (length(unique(df$order)) != 1)){
                print("ERROR: is_fu on multiple encounters")
                return(df)
        }
        if(any(df$variable == "date_fu")){
                return(df)
        }
        else(return(data.frame()))
}

########

#Transferred
#only use if stone size is up and running
#returns patients whose stones are larger than threshold
#patients without CT or who have no CT measurements will be removed.
is_size_above_threshold <- function(df, threshold = 20){
        
        df$l_ct_size <- as.numeric(df$l_ct_size)
        df$r_ct_size <- as.numeric(df$r_ct_size)
        
        #if left is NA
        if(all(is.na(df$l_ct_size))){
                #then return nothing if the right is not getting it done.
                if(all(is.na(df$r_ct_size)) || (max(df$r_ct_size) < threshold)){
                        return(data.frame())
                }
                if((max(df$r_ct_size) >= threshold)){
                        return(df)
                }
                else{print("ERROR IN CT SIZE THRESHOLD")
                        return(df)}
        }
        
        if(all(is.na(df$r_ct_size))){
                #then return nothing if the right is not getting it done.
                if(all(is.na(df$l_ct_size)) || (max(df$l_ct_size) < threshold)){
                        return(data.frame())
                }
                if((max(df$l_ct_size) >= threshold)){
                        return(df)
                }
                else{print("ERROR IN CT SIZE THRESHOLD")
                        return(df)}
        }
        
        if((df$r_ct_size >= threshold) || (df$l_ct_size >= threshold)){
                return(df)
        }
        
        if((df$r_ct_size < threshold) && (df$l_ct_size < threshold)){
                return(data.frame())
        }
        
        else{print("ERROR IN SIZE ESTIMATE")
                print(df$record_id[1])
                return(data.frame())}
}

#Transferred
#VALIDATED
#what is the stone size on imaging
eval_ct_size <- function(df){
        if(any(grepl("imgnp_l_size",df$variable))){
                df$l_ct_size <- df$value[grepl("imgnp_l_size",df$variable)] 
        }
        if(!any(grepl("imgnp_l_size",df$variable))){
                df$l_ct_size <- NA
        }
        if(any(grepl("imgnp_r_size",df$variable))){
                df$r_ct_size <- df$value[grepl("imgnp_r_size",df$variable)] 
        }
        if(!any(grepl("imgnp_r_size",df$variable))){
                df$r_ct_size <- NA 
        }
        return(df)
}

#transfer
#determines whether or not a certain key value pair happened during
#each patient-order encounter
eval_keyval_during_order <- function(df, keys, values){
        df1 <- df
        df1 %>%
                arrange(record_id,order) %>%
                group_by(record_id, order) %>%
                filter(variable %in% keys) %>%
                filter(value %in% values) %>%
                summarise(has_keyval = 1) -> df2
        left_join(df, df2, by = c("record_id","order")) -> new_df
        new_df$has_keyval[is.na(new_df$has_keyval)] <- 0
        return(new_df)
}

#Transfer
#VALIDATED
#doesn't need to be grouped by anything
#returns the multi value for each of the ones that are marked one
eval_multi <- function(df, prefix_name, column_name){
        df1 <- df
        df1 %>%
                select(record_id, order, variable, value) %>%
                filter(grepl(paste(prefix_name,"___",sep = ''), variable)) %>%
                filter(value == "1") %>%
                select(-value) %>%
                mutate(new_var = aua_pull_nums(variable, prefix_name)) -> df2
        left_join(df, df2, by = c("record_id","order","variable")) -> result_df
        names(result_df)[names(result_df) == "new_var"] <- column_name
        return(result_df)
}

#transferred
#QUASI VALIDATED
#determines whether or not a given row is in an op
#NA means it's not an op
#the number is the order of the op
eval_op_order <- function(df){
        ops <- unique(df$order[grepl("stone_op",df$variable)])
        df$op_order <- NA
        df$op_order[df$order %in% ops] <- df$order[df$order %in% ops]
        return(df)
}


aua_cascade <- function(df,func_list){
        res_l <- list()
        res_l[[1]] <- df
        for(i in 1:length(func_list)){
                new_df <- do.call(func_list[[i]], list(res_l[[i]]))
                res_l[[i+1]] <- new_df
        }
        return(res_l)
}

#get all patients that were symptomatic
cas_is_sxs <- function(df){
        df <- tbl_df(df)
        df %>%
                group_by(record_id) %>%
                do(eval_sxs(.)) %>%
                filter(sxs == 1) %>%
                select(-sxs) -> result
        return(result)
}

#VALIDATED
#who have imaging in new patient
cas_has_imgnp <- function(df){
        df %>%
                group_by(record_id) %>%
                do(is_np_img(.)) -> result_df
        return(result_df)
}

#VALIDATED
cas_has_ct_size <- function(df){
        df1 <- df
        df1 %>%
                group_by(record_id) %>%
                do(eval_ct_size(.)) %>%
                summarize(sized = any(!is.na(r_ct_size) | !is.na(l_ct_size))) -> chosen
        left_join(df, chosen, by = c("record_id")) -> results_df
        results_df %>%
                filter(sized == 1) %>%
                select(-sized) -> result_df
        return(result_df)
}

#VALIDATED
cas_has_large_ct_stone <- function(df){
        df1 <- df
        df1 %>%
                group_by(record_id) %>%
                do(eval_ct_size(.)) %>%
                group_by(record_id) %>%
                do(is_size_above_threshold(., 20)) %>%
                select(-l_ct_size) %>%
                select(-r_ct_size) -> result_df
        return(result_df)
}

#leaves only patients with a large burden
cas_has_large_first_op_burden <- function(df){
        df1 <- df
        df1 %>%
                group_by(record_id) %>%
                
}

cas_has_large_any_op_burden <- function(df){
        
}

simple_cascade <- list(cas_is_sxs,
                       cas_has_imgnp,
                       cas_has_ct_size,
                       cas_has_large_ct_stone)



######

#INCOMPLETE
#takes a dataframe, applies a filter function to that,
#looks at the values in a certain column and row, and
#then applies that value to each value in that column
#sum func returns a single value for each id, order pair,
#a dataframe that has been minaturized
#it should be able to calculate anything you would want
#and pair it with that operation.
spread_to_order <- function(df, mini_func, sum_sent, l1 = list(), l2 = list()){
        df1 <- df
        df1 %>%
                ungroup() %>%
                group_by(record_id, order) %>%
                do(do.call(mini_func, list(., l1))) -> df2
        df2 %>% summarize(new_var = any((variable %in% l1$var) & (value %in% l1$value))) %>%
                #parse_expr(paste("df2 %>% summarize(new_var = ",sum_sent,") %>%", sep = ''))
                ungroup() %>%
                left_join(df, ., by = c("record_id","order")) -> new_df
        return(new_df)
}


any_pcnl <- "any((variable %in% l1$var) & (value %in% l1$value))"

dummy <- function(df, l = list()){
        return(1)
}

#######

#sum_func
#returns NA if no variable in group
#returns 1 if vars have value
#returns 0 if vars don't have value
any_var_value <- function(df, l){
        df %>%
                summarize(new_var = any((variable %in% l$var) & (value %in% l$value))) -> bool
        print("HELLO")
        return(bool$new_var[1])
}

#for example, looking for any pcnl's would be:
any_pcnl <- function(df){
        l = list(var = c("stone_op___6","stone_op___17"), value = "1")
        return(any_var_value(df, l))
}

none_var_value <- function(df, l){
        bool <- !any(df$value[df$variable %in% l$var] %in% l$value)
        return(bool)
}

no_mini <- function(df, l = list()){
        return(df)
}

no_sum <- function(df, l = list()){
        return(df)
}