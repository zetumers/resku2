####I. These functions extract information about new patient imaging.
#	They do not have keywords is, has, pull, or eval.

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
                mutate(variable = sub("_loc___.+","",variable)) %>%
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