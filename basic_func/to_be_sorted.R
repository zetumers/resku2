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