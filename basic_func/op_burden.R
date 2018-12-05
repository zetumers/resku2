####I. Extract information from operative reports

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