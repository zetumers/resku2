####I. These functions are the cascade functions
#each function takes a cohort and returns a cohort
#that meets the inclusion criteria. This cohort can be handed off
#to other cascade functions

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

cas_has_imgnp <- function(df){
        df %>%
                group_by(record_id) %>%
                do(is_np_img(.)) -> result_df
        return(result_df)
}

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

simple_cascade <- list(cas_is_sxs,
                       cas_has_imgnp,
                       cas_has_ct_size,
                       cas_has_large_ct_stone)

### C. This is starting the graphing function

#do a little recursive magic
