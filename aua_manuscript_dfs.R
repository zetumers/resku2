######creates the img_type variable#######

base_df %>%
        filter(variable %in% c("img_dx___2","img_dx___4")) %>%
        mutate(value = as.numeric(value)) %>%
        select(-order) %>%
        dcast(record_id ~ variable, mean) %>%
        mutate(img_type = 2*img_dx___2 + 1*img_dx___4) %>%
        select(record_id, img_type) %>%
        filter(img_type != 0) %>%
        mutate(img_type = factor(img_type,
                                 labels = c("CT","US","Both")))-> img_type_df

########

######creates the stone_num variable######

base_df %>%
        aua_side_isolate1() %>%
        aua_side_isolate2("imgnp","number") %>%
        aua_side_pull_side("imgnp","number") %>%
        rename(stone_num = value) %>%
        select(-variable, -order) -> stone_num_df

########
########Create teh stone_loc variable#########
base_df %>%
        aua_side_isolate1() %>%
        aua_side_isolate3("imgnp","loc") %>%
        aua_side_pull_side2("imgnp","loc") %>%
        aua_side_pull_nums("imgnp","loc") %>%
        filter(value == "1") %>%
        rename(stone_loc = num) %>%
        select(-variable, -order, -value) -> stone_loc_df
###########

#############Add additional variables in################

resku_df %>%
        filter(variable == "surg_totaltime_v2") %>%
        group_by(record_id) %>%
        filter(order == min(order)) %>%
        select(record_id, value) %>%
        dplyr::rename(surg_time = value) %>%
        mutate(surg_time = as.numeric(surg_time)) -> surg_time_df

resku_df %>%
        filter(variable == "second_stage_plan_v2") %>%
        group_by(record_id) %>%
        filter(order == min(order)) %>%
        select(record_id, value) %>%
        dplyr::rename(stage_2 = value) %>%
        mutate(stage_2 = as.numeric(stage_2))-> stage2_df

resku_df %>%
        filter(variable == "asa_score_v2") %>%
        group_by(record_id) %>%
        filter(order == min(order)) %>%
        select(record_id, value) %>%
        dplyr::rename(asa = value) %>%
        mutate(asa = as.numeric(asa))-> asa_df

resku_df %>%
        filter(variable == "bmi") %>%
        group_by(record_id) %>%
        filter(order == min(order)) %>%
        select(record_id, value) %>%
        dplyr::rename(bmi = value) %>%
        mutate(bmi = as.numeric(bmi))-> bmi_df

resku_df %>%
        filter(variable == "prev_stone_tx___1") %>%
        group_by(record_id) %>%
        filter(order == min(order)) %>%
        select(record_id, value) %>%
        dplyr::rename(any_prev_int = value) %>%
        mutate(any_prev_int = as.numeric(any_prev_int)) %>%
        mutate(any_prev_int = 1 - any_prev_int) -> any_prev_int_df

resku_df %>%
        filter(variable == "prev_intervention___1") %>%
        group_by(record_id) %>%
        filter(order == min(order)) %>%
        select(record_id, value) %>%
        dplyr::rename(this_prev_int = value) %>%
        mutate(this_prev_int = as.numeric(this_prev_int)) %>%
        mutate(this_prev_int = 1 - this_prev_int) -> this_prev_int_df

resku_df %>%
        filter(variable == "gender") %>%
        group_by(record_id) %>%
        filter(order == min(order)) %>%
        select(record_id, value) %>%
        dplyr::rename(gender = value) %>%
        mutate(gender = as.numeric(gender)) %>%
        mutate(gender = gender - 1) -> gender_df

resku_df %>%
        filter(variable == "race___1") %>%
        group_by(record_id) %>%
        filter(order == min(order)) %>%
        select(record_id, value) %>%
        dplyr::rename(white = value) %>%
        mutate(white = as.numeric(white)) -> white_df

resku_df %>%
        filter(variable == "urologist") %>%
        select(record_id, value) %>%
        dplyr::rename(urologist = value) -> urologist_df

resku_df %>%
        pull_sided_multi_var("imgnp","loc") %>%
        eval_side_sides3("imgnp","loc") %>%
        eval_side_nums("imgnp","loc") %>%
        filter(value == 1) %>%
        group_by(record_id, side) %>%
        summarize(renal = any(num %in% c(1,2,3,10,4,5)),
                  l_pole = any(num %in% c(3))) -> loc_df

loc_df %>%
        filter(renal == 1) -> renal_df
