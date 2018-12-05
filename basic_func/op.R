####I. All these functions extract information from op dfs.


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