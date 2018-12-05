###All these functions return dataframes that meet the conditions
#	and empty dataframes when conditions are not met

##df contains encounter:

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

##df contains information:

#does df contain new patient imaging (that is not "no imaging")
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

##df contains information, requires pre-run of imaging:

#Requires eval_ct_size
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

#Requires eval_sxs
is_sxs <- function(df){
        if(df$sxs[1] == 1){
                return(select(df, -sxs))
        }
        else{return(data.frame())}
}


### D. is from other functions

#elim patients that are eliminated by a pull
is_from_pull <- function(long_df, pull_func){
	ans_df <- do.call(pull_func, list(long_df))
	recs <- ans_df$record_id
	long %>%
		filter(record_id %in% recs) -> long_df
	return(long_df)
}

#elim patient-record combos that aren't present in pull
is_from_pull2 <- function(long_df, pull_func){
	ans_df <- do.call(pull_func, list(long_df))
	ans_df %>%
		select(record_id, order) %>%
		mutate(present = rep(1, length(order))) %>%
		distinct() %>%
		right_join(long_df, by = c("record_id","order")) %>%
		filter(!is.na(present)) %>%
		select(-present) -> long_df
	return(long_df)
}