####I. Eval functions:
#	eval functions are functions that provide an additional column at
#	the end of the computation

###A. These functions should be applied to each id-order pair
#	separately

eval_sxs <- function(df){
        sxs <- any((df$variable == "stone_symptomatic") & (df$value == "1"))
        if(sxs == TRUE){
                df$sxs <- 1
        }
        else{df$sxs <- 0}
        return(df)
}

#returns eval ct_size
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

###B. These functioins can be applied to the entire df. They don't require that
#	the df is broken into components:

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


#determines whether or not a given row is in an op
#NA means it's not an op
#the number is the order of the op
eval_op_order <- function(df){
        ops <- unique(df$order[grepl("stone_op",df$variable)])
        df$op_order <- NA
        df$op_order[df$order %in% ops] <- df$order[df$order %in% ops]
        return(df)
}

### C. Eval the value of variables
eval_side_nums <- function(long_df, char_prefix, char_suffix, charornum = "char"){
        char_vec <- as.character(long_df$variable)
        result <- sub(paste(char_prefix,"_(r|l)_",char_suffix,"___",sep = ''),"", char_vec)
        long_df$num <- result
return(long_df)
}

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

#only for variables that aren't multi (eg img_np_l_size, not img_np_l_loc___1)
eval_side_sides <- function(long_df, char_prefix, char_suffix, charornum = "char"){
        char_vec <- as.character(long_df$variable)
        result <- sub(paste(char_prefix,"_",sep=''),"", char_vec)
        result <- sub(paste("_",char_suffix,sep =''),"", result)
        long_df$side <- result
        return(long_df)
}

eval_side_sides3 <- function(long_df, char_prefix, char_suffix, charornum = "char"){
        char_vec <- as.character(long_df$variable)
        result <- sub(paste(char_prefix,"_",sep=''),"", char_vec)
        result <- sub(paste("_",char_suffix,"___.+",sep =''),"", result)
        long_df$side <- result
        return(long_df)
}

eval_radio <- function(long_df, var_name){
	
}

#if it contains a letter in the middle, just put _(r|l)_ var name
#doesn't currently support free text
eval_var <- function(long_df, var_name){
	#sidedness
	
	#if radio
	
	
	#if multi
	
}