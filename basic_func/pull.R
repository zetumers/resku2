####take a df and return a df that only has information
#	related to the information desired. Like eval, but does not
#	create a new column. Instead, just filters the df to return
#	only the information you want.

library(dplyr)

### A. pull dates
pull_date <- function(long_df){
        date_vars <- c("date_np","date_surgery_v2","date_fu_postop","date_fu")
        long_df %>%
                filter(variable %in% date_vars) %>%
                dplyr::rename(date = value) -> long_df
                return(long_df)
}

pull_date2 <- function(long_df){
        long_df %>%
                pull_date() -> long_df
        long_df$date <- as.numeric(as.Date(long_df$date, "%m/%d/%y"))
        long_df %>%
                group_by(record_id) %>%
                mutate(start = rep(min(date), length(date))) %>%
                mutate(date = date - start) %>%
                select(-start) -> long_df
                return(long_df)
}

### B. pull variable types: radio, multi, sided variables

#pulls all variables with a sidedness
pull_sided_radio_var <- function(long_df){
        long_df %>%
                filter(grepl("_(l|r)_",variable)) -> ans_df
        return(ans_df)
}

#pull_sided_vars plus filter by prefex and suffix
pull_sided_radio_var2 <- function(long_df, prefix, suffix){
        reg_ex <- paste(prefix,"_(l|r)_",suffix, sep = '')
        filter(long_df, grepl(reg_ex, variable)) -> ans_df
        return(ans_df)
}

#isolates exact sides with numbers eg imgnp_l_loc___1
pull_sided_multi_var <- function(long_df, prefix, suffix){
        reg_ex <- paste(prefix,"_(l|r)_",suffix,"___.+", sep = '')
        filter(long_df, grepl(reg_ex, variable)) -> ans_df
        return(ans_df)
}

pull_radio_var <- function(long_df, varname){
        filter(variable == varname) -> ans_df
        return(ans_df)
}

pull_multi_var <- function(long_df, varname){
	    reg_ex <- paste(varname,"___.+", sep = '')
        filter(long_df, grepl(reg_ex, variable)) -> ans_df
        return(ans_df)
}

### C. pull encounters


#this function pulls encounters of a very specific type
#pull all encounters with this property (is_np, etc.)
#for each of these encounters, choose only those where the following prop are met
#and choose from those encounters by some sort of rank function
pull_enc <- function(long_df, during_func = NA, prop_func = NA, rank_func = NA){
	
	
}