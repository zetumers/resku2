#####1. Clinicians should obtain a non-contrast CT scan on patients prior
#to performing PCNL. Strong Recommendation; Evidence Level Grade C

##########
#VERIFIED
#return list of id's of all patients that got a non-con during
#their first visit
aua_ncct_np <- function(long_df){
        long_df %>%
                #get imaging
                aua_img_np() %>%
                #double check that it's the first visit
                filter(order == 1) %>%
                #select for CT abdomen/pelvis
                filter(img_np == "4") %>%
                #get only the ids
                select(record_id) %>%
                #get only the unique
                distinct() -> result_df
        return(result_df)
}

#VERIFIED
#list of patient's that got any imaging on their
#first visit.
aua_any_img_np <- function(long_df){
        long_df %>%
                #get imaging
                aua_img_np() %>%
                #double check that it's the first visit
                filter(order == 1) %>%
                filter(img_np != "8") %>%
                select(record_id) %>%
                distinct() -> result_df
        return(result_df)
}

#VERIFIED
#returns a list of patients who got a pcnl,
#when they got it,
#and what they got.
aua_pcnl_op <- function(long_df){
        long_df %>%
                aua_op_id() %>%
                filter(stone_op %in% c(6,17)) -> result_df
        return(result_df)
}

#VERIFIED
#selects all first operations.
#filters for operations that were pcnl.
aua_pcnl_first_op <- function(long_df){
        long_df %>%
                aua_first_op() %>%
                filter(stone_op %in% c(6,17)) -> result_df
        return(result_df)
}

##LOOK AT THIS
aua_guideline_1 <- function(long_df){
        
        #VERIFIED
        results <- list()
        long_df %>%
                aua_pcnl_op() -> results$any_pcnl_pt
        
        long_df %>%
                aua_pcnl_first_op() -> results$first_pcnl_pt
        
        results$any_pcnl_pt %>%
                group_by(record_id) %>%
                summarize(no_pcnl = length(record_id)) -> results$num_pcnl
        
        #result3: the patients who received PCNL at any time
        PCNL_patients <- results$any_pcnl_pt$record_id
        #patients that received a ct
        CT_patients <- aua_ncct_np(long_df)$record_id
        #patients who got a PCNL and A CT
        guideline1_pts <- PCNL_patients[PCNL_patients %in% CT_patients]
        results$any_gl_1 <- guideline1_pts
        
        results$any_ratio <- length(results$any_gl_1)/length(results$any_pcnl_pt$record_id)
        results$any_outlier <- unique(PCNL_patients[!(PCNL_patients %in% CT_patients)])
        
        #results4: the patients who received PCNL first
        PCNL_patients <- results$first_pcnl_pt$record_id
        #patients that received a ct
        CT_patients <- aua_ncct_np(long_df)$record_id
        #patients who got a PCNL and A CT
        guideline1_pts <- PCNL_patients[PCNL_patients %in% CT_patients]
        results$first_gl_1 <- guideline1_pts
        
        results$first_ratio <- length(results$first_gl_1)/length(results$first_pcnl_pt$record_id)
        results$first_outlier <- unique(PCNL_patients[!(PCNL_patients %in% CT_patients)])
        
        results$ct_pts <- unique(CT_patients)
        
        return(results)
}


##########

#How often is nephrectomy performed.
#27. Clinicians may perform nephrectomy when the involved kidney
#has negligible function in patients requiring treatment. 
#(Index Patients 1-14) 
#Conditional Recommendation; Evidence Level Grade C



#####21. In symptomatic patients with a total non-lower pole renal stone
#burden ≤ 20 mm, clinicians may offer SWL or URS. (Index Patient 7)
#Strong Recommendation; Evidence Level Grade B

#25. In patients with total renal stone burden >20 mm, clinicians
#should not offer SWL as first-line therapy. (Index Patient 8)
#Moderate Recommendation; Evidence Level Grade C

#Symptomatic Patients
aua_guideline_22_25 <- function(long_df){
        results <- list()
        
        #list of symptomatic patients
        long_df %>%
                aua_sxs_np() -> results$sxs_pt
        symp_ids <- filter(results$sxs_pt, sxs_np == 1)$record_id
        
        #of symptomatic patients...
        long_df %>%
                filter(record_id %in% symp_ids) %>%
                #get stone burden of each surgery
                filter(variable == "stone_burden") %>%
                select(-variable) %>%
                dplyr::rename(burden = value) -> results$burden
        
        long_df %>%
                filter(record_id %in% symp_ids) %>%
                aua_op_id() -> results$op_id
        
        results$burden %>%
                left_join(results$op_id, by = c("record_id","order")) -> results$burden_and_op
        
        #if high burden then pcnl
        results$burden_and_op %>%
                filter((burden == "3")) %>%
                filter(stone_op %in% c("6","17")) -> results$large_stone_pcnl
        
        #find patients with high burden and then no pcnl
        results$burden_and_op %>%
                #get all large burden operations
                filter(burden == "3") %>%
                #group by person and event
                group_by(record_id, order) %>%
                #and select for only the person and events that
                #don't have any encoding in pcnl.
                summarize(no_pcnl = !any(stone_op %in% c("6","17"))) %>%
                filter(no_pcnl == TRUE) -> results$large_stone_no_pcnl
        
        #find all patients with high burden that didn't get swl during same op
        results$burden_and_op %>%
                #get all large burden operations
                filter(burden == "3") %>%
                #group by person and event
                group_by(record_id, order) %>%
                summarize(no_swl = !any(stone_op %in% c("7","18"))) -> results$large_stone_no_swl

        long_df %>%
                aua_img_size_np() %>%
                select(-order) -> a_df
        
        results$burden_and_op %>%
                mutate(side = aua_side_stone_op(stone_op)) %>%
                group_by(record_id) %>%
                filter(order == min(order)) %>%
                left_join(a_df, by = c("record_id","side")) %>%
                select(record_id, side, img_size, burden, stone_op) -> results$side_size_burden_op
        
        return(results)
}


aua_guidelines_30 <- function(long_df){
        results <- list()
        
        #list of symptomatic patients
        long_df %>%
                aua_sxs_np() -> results$sxs_pt
        
        #symptomatic patients
        symp_ids <- filter(results$sxs_pt, sxs_np == 1)$record_id
        
        #of symptomatic patients...
        long_df %>%
                filter(record_id %in% symp_ids) %>%
                #get stone burden of each surgery
                filter(variable == "stone_burden") %>%
                select(-variable) %>%
                dplyr::rename(burden = value) -> results$burden
}


