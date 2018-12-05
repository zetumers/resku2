#this melts wide files without respect to order
aua_melt3 <- function(wide_df){
        long_df <- melt(wide_df, id.vars = c("record_id"))
        return(long_df)
}

aua_melt2 <- function(wide_df, rosetta){
        long_df <- melt(wide_df, id = c("record_id","order"))
        long_df$variable <- as.character(long_df)
        long_df %>%
                left_join(rosetta, by = c("variable" = "RC_Label")) %>%
                select(-Institution_Label, -Institution_Key, -Multi) -> long_df
        return(long_df)
}

#VALIDATED on these variable sets "stone_op","stone_dx_preop","stone_dx_postop"
# 1) Filters out strings that don't have the prefix plus the little dashy part
# 2) Pulls out the number after the dashy part
#defaults to returning a char. Can return a number.
aua_pull_nums <- function(char_vec, char_prefix, charornum = "char"){
        char_vec <- as.character(char_vec)
        char_vec1 <- char_vec[grepl(paste(char_prefix,"___",sep = ''), char_vec)]
        result <- sub(paste(char_prefix,"___",sep = ''),"", char_vec1)
        res <- rep(NA, length(char_vec))
        res[grepl(paste(char_prefix,"___",sep = ''), char_vec)] <- result
        if(charornum == "char"){
                return(res)}
        else{return(as.numeric(res))}
}

#####Symptom AUX
#changes scoring of sxs to 1 = sympomatic 0 = asxs.
aua_sxs_coerce <- function(sxs_vec){
        vec <- as.character(sxs_vec)
        vec1 <- as.numeric(vec)
        return((vec1 - 2)*(-1))
}

aua_sxs_coerce_po <- function(sxs_vec){
        sxs_po <- list()
        sxs_po[[15]] <- 0
        sxs_po[[1]] <- 1
        sxs_po[[2]] <- 2
        sxs_po[[3]] <- 3
        sxs_po[[4]] <- 4
        sxs_po[[5]] <- 5
        sxs_po[[6]] <- 6
        sxs_po[[7]] <- 7
        sxs_po[[8]] <- 8
        sxs_po[[9]] <- 9
        sxs_po[[10]] <- 10
        sxs_po[[11]] <- 11
        sxs_po[[16]] <- 16
        sxs_po[[12]] <- 12
        sxs_po[[13]] <- 13
        vec <- as.character(sxs_vec)
        vec1 <- as.numeric(vec)
        result <- sxs_po[[vec1]]
        return(result)
}


#TRUE is right
#FALSE is left
#Note: cystolitholapaxy/cystolithotripsy (25) is considered a "neither" operation
#THIS IS THE MOST INEFFICIENT PROGRAM EVER
aua_side_stone_op <- function(vec){
        right_vec <- c(1,101,30,31,32,6,2,201,33,3,4,5,7,8,9,10,11,23,26,27,50,51)
        cysto <- c(25)
        is_right <- vec %in% right_vec
        is_bl <- vec %in% cysto
        is_left <- 1 - (is_right + is_bl - is_right*is_bl)
        encode <- is_right*3 + is_bl*1 + is_left*2
        encode <- as.character(encode)
        encode <- sub("3","r",encode)
        encode <- sub("1","na",encode)
        encode <- sub("2","l",encode)
        return(encode)
}

aua_bilat_op <- function(vec){
        l <- list()
        l[[1]] <- 1
        l[[12]] <- 1
        l[[101]] <- 101
        l[[121]] <- 101
        l[[30]] <- 30
        l[[34]] <- 30
        l[[31]] <- 31
        l[[35]] <- 31
        l[[32]] <- 32
        l[[36]] <- 32
        l[[6]] <- 6
        l[[17]] <- 6
        l[[2]] <- 2
        l[[13]] <- 2
        l[[201]] <- 201
        l[[131]] <- 201
        l[[33]] <- 33
        l[[37]] <- 33
        l[[3]] <- 3
        l[[14]] <- 3
        l[[4]] <- 4
        l[[15]] <- 4
        l[[5]] <- 5
        l[[7]] <- 7
        l[[16]] <- 5
        l[[18]] <- 7
        l[[8]] <- 8
        l[[19]] <- 8
        l[[9]] <- 9
        l[[20]] <- 9
        l[[10]] <- 10
        l[[21]] <- 10
        l[[11]] <- 11
        l[[22]] <- 11
        l[[23]] <- 23
        l[[24]] <- 23
        l[[26]] <- 26
        l[[28]] <- 26
        l[[27]] <- 27
        l[[29]] <- 27
        l[[50]] <- 50
        l[[501]] <- 50
        l[[51]] <- 51
        l[[511]] <- 51
        l[[25]] <- 25
        vec2 <- unlist(l[vec])
        return(vec2)
}

aua_op_name <- function(vec){
        l <- list()
        l[[1]] <- "cystourethroscopy w stent placement"
        l[[101]] <- "cystourethroscopy w stent removal"
        l[[30]] <- "retro pyelo"
        l[[31]] <- "renal US"
        l[[32]] <- "renal access and dilation"
        l[[6]] <- "PCNL"
        l[[2]] <- "NT placement"
        l[[201]] <- "NT removal"
        l[[33]] <- "ante nephrostogram"
        l[[3]] <- "ureteroscopy"
        l[[4]] <- "ureteroscopy w lith"
        l[[5]] <- "ureteroscopy wo lith"
        l[[7]] <- "SWL"
        l[[8]] <- "open"
        l[[9]] <- "lapro/robo"
        l[[10]] <- "open nephrectomy"
        l[[11]] <- "lapro/robo nephrectomy"
        l[[23]] <- "endopyelotomy"
        l[[26]] <- "lapro pyeloplasty"
        l[[27]] <- "open pyeloplasty"
        l[[50]] <- "infundibulotomy"
        l[[51]] <- "caliceal diverticulectomy"
        l[[25]] <- "cystolitho"
        vec2 <- unlist(l[vec])
        return(vec2)
}

aua_burden_name <- function(vec){
        l <- list()
        l[[1]] <- "< 10 mm"
        l[[2]] <- "10-20 mm"
        l[[3]] <- "> 20 mm"
        l[[4]] <- "Randall"
        l[[5]] <- "None"
        vec2 <- unlist(l[vec])
        return(vec2)
}

aua_is_sxs <- function(variable, value){
        return(as.numeric(any((variable == "stone_symptomatic")*(value == "1"))))
}

aua_has_any_large <- function(variable, value){
        return(as.numeric(any(grepl("imgnp_._size",variable)*(value >= 20))))
}

aua_graph_shape <- function(long_df, l, vec){
        long_df %>%
                filter(stone_op %in% vec) %>%
                mutate(ans = l[[stone_op]]) %>%
                select(record_id, side, img_size, burden, ans) %>%
                distinct() -> ans_df
        return(ans_df)
}

aua_cut_size <- function(size_vec, up_val = 50){
        over_50 <- as.numeric(size_vec > 50)
        scrap <- 1 - over_50
        rectified <- scrap*size_vec
        middle_vec <- rectified + 50*over_50
        rands <- runif(length(size_vec),-3.25,3.25)
        rands <- rands*over_50
        bump <- 3.25*over_50
        ans_vec <- middle_vec + bump + rands
        return(ans_vec)
}

#isolates only variables with sidedness
aua_side_isolate1 <- function(long_df){
        long_df %>%
                filter(grepl("_(l|r)_",variable)) -> ans_df
        return(ans_df)
}

#sided variables without numbers
aua_side_isolate2 <- function(long_df, prefix, suffix){
        reg_ex <- paste(prefix,"_(l|r)_",suffix, sep = '')
        filter(long_df, grepl(reg_ex, variable)) -> ans_df
        return(ans_df)
}

#isolates exact sides with numbers eg imgnp_l_loc___1
aua_side_isolate3 <- function(long_df, prefix, suffix){
        reg_ex <- paste(prefix,"_(l|r)_",suffix,"___.+", sep = '')
        filter(long_df, grepl(reg_ex, variable)) -> ans_df
        return(ans_df)
}

aua_side_pull_nums <- function(long_df, char_prefix, char_suffix, charornum = "char"){
        char_vec <- as.character(long_df$variable)
        result <- sub(paste(char_prefix,"_(r|l)_",char_suffix,"___",sep = ''),"", char_vec)
        long_df$num <- result
return(long_df)
}

aua_side_pull_side <- function(long_df, char_prefix, char_suffix, charornum = "char"){
        char_vec <- as.character(long_df$variable)
        result <- sub(paste(char_prefix,"_",sep=''),"", char_vec)
        result <- sub(paste("_",char_suffix,sep =''),"", result)
        long_df$side <- result
        return(long_df)
}

aua_side_pull_side2 <- function(long_df, char_prefix, char_suffix, charornum = "char"){
        char_vec <- as.character(long_df$variable)
        result <- sub(paste(char_prefix,"_",sep=''),"", char_vec)
        result <- sub(paste("_",char_suffix,"___.+",sep =''),"", result)
        long_df$side <- result
        return(long_df)
}
