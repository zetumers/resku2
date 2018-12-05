####I. These functions are utility functions that allow you to combine pull, eval, etc. functions


#applies functions by patient id

by_id <- function(long_df, func){
	long_df %>%
		group_by(record_id) %>%
		do(do.call(func, list(.))) -> long_df
	return(long_df)
}

by_id_ord <- function(long_df, func){
	long_df %>%
		group_by(record_id, order) %>%
		do(do.call(func, list(.))) -> long_df
	return(long_df)
}

#takes a node of a tree and a connnection of that tree,
#and returns the node of the parent.
parent_node