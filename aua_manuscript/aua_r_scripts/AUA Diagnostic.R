ro_filter <- function(test1, record_id1, order1){
        result <- filter(test1, record_id == record_id1 & order == order1)
        return(result)
}

var_filter <- function(test1, variable1){
        result <- filter(test1, grepl(variable1, variable))
        return(result)
}


vv_filter <- function(test1, variable1, value1){
        result <- filter(test1, grepl(variable1, variable) & value == value1)
        return(result)
}

pt_present <- function(test1, record_id1){
        bool <- any(test1$record_id == record_id1)
        return(bool)
}