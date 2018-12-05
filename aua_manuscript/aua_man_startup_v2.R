library(broom)

curse2_key <- c(6,17,3,4,5,14,15,16,7,18,8,19)
curse2_value <- c(3,3,2,2,2,2,2,2,1,1,4,4)
curse2_df <- data.frame(key = curse2_key,
                        value = factor(curse2_value,
                                       labels = c("SWL","URS","PCNL","open")))

size_cascade <- list(cas_is_sxs,
                     cas_has_imgnp,
                     cas_has_ct_size)

########Exclusion criteria filter: produces resku_df2#######

        #get age
age_df <- read.csv("../data/pt_age.csv")

        #bleeding disorders
resku_df %>%
        filter(variable == "pmh___999" & value == "1") -> bleeding
resku_df %>%
        filter(record_id %in% bleeding$record_id) %>%
        filter(variable == "pmh_other") %>%
        mutate(value = tolower(value)) %>%
        filter(grepl("(hemophilia|hemophi|bleeding disorder)", value)) -> exclusion_bleeding

        #anticoag
resku_df %>%
        filter(variable %in% c("meds_np___43", "meds_np___127","meds_np___103")) %>%
        filter(value == 1) -> exclusion_anticoag

resku_df %>%
        mutate(record_id = as.numeric(record_id)) %>%
        #filter out pediatric patients
        left_join(age_df, by = "record_id") %>%
                #note: ages are +/- 2 years, so some 18 and 19 year olds will possibly be excluded.
        filter(anon_age > 18) %>%
        ungroup() %>%
        #filter pregnancy:
                #find all new-patient encounters
                #then determine whether pregnancy evals occurred during them
                #and eliminate patients who are pregnant!
        group_by(record_id, order) %>%
        mutate(np = any(variable == "date_np")) %>%
                #is the patient pregnant during this visit?
        mutate(preg_during_enc = any(variable == "stone_gest_age" &
                                             value %in% c("1","2","3"))) %>%
                #keep everything that's either not a new patient encounter
                #or during which she's not pregnant
        filter(np != 1 | preg_during_enc != 1) %>%
        #Filter out those with bleeding disorders:
        filter(!(record_id %in% exclusion_bleeding$record_id)) %>%
        filter(!(record_id %in% exclusion_anticoag$record_id)) %>%
        select(record_id, order, variable, value) -> resku_df2

########

cascade <- aua_cascade(resku_df2, size_cascade)
base_df <- cascade[[4]]
source("aua_r_scripts/aua_manuscript_dfs.R", echo = TRUE)
source("aua_r_scripts/aua_manuscript_func.R", echo = TRUE)

aua_guideline_22_25 <- function(long_df){
  results <- list()
  
  #list of symptomatic patients
  long_df %>%
    filter(variable == "stone_burden") %>%
    select(-variable) %>%
    dplyr::rename(burden = value) -> results$burden
  
  #those who have operation ids
  long_df %>%
    aua_op_id() -> results$op_id
  
  #operations and burden ids
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
  
  results$op_id %>%
    mutate(side = aua_side_stone_op(stone_op)) %>%
    group_by(record_id) %>%
    filter(order == min(order)) %>%
    left_join(a_df, by = c("record_id","side")) %>%
    select(record_id, side, img_size, stone_op) -> results$side_size_op
  
  return(results)
}

test2 <- aua_guideline_22_25(base_df)

test2$side_size_op %>%
  filter(!is.na(img_size)) %>%
  filter(as.numeric(stone_op) %in% curse2_key) %>%
  mutate(ans = curse2_df$value[match(stone_op, curse2_df$key)]) %>%
  arrange(factor(ans, levels = c(4,3,1))) %>%
  distinct() %>%
  select(-stone_op) %>%
  left_join(stone_num_df, by = c("record_id","side")) %>%
  mutate(stone_num = as.numeric(stone_num)) -> processed_df

processed_df %>%
  mutate(stone_num = as.character(stone_num)) %>%
  distinct() %>%
  left_join(img_type_df, by = c("record_id"))  %>%
  filter(!is.na(img_type)) %>%
  mutate(img_type = img_type) %>%
  filter(ans != "SWL")-> processed_df

processed_df$img_size <- as.numeric(processed_df$img_size)
processed_df$img_cat <- cut(processed_df$img_size, breaks = c(-1,10,20,200), labels = c(1,2,3))
processed_df$img_size2 <- aua_cut_size(processed_df$img_size)

processed_df %>%
  group_by(record_id, side) %>%
  summarize(surgery_types = n()) -> surg_num_df

processed_df <- left_join(processed_df, surg_num_df, by = c("record_id","side"))

base_df0 <- processed_df
base_df1 <- base_df0
base_df1$stone_num <- as.factor(base_df1$stone_num)
base_df1$stone_num <- mapvalues(base_df1$stone_num, c("1","2","3","5"), c("1","2","3",">3"))

base_df %>%
  eval_multi("urinary_anomaly","anom") %>%
  filter(!is.na(anom)) %>%
  select(record_id, anom) %>%
  mutate(anom = as.factor(anom)) %>%
  right_join(base_df1, by = "record_id") %>%
  mutate(anom2 = (anom != 1))-> base_df5

base_df5 %>%
  filter(surgery_types == 2) %>%
  select(-ans) %>%
  group_by(record_id, side) %>%
  summarize(img_size2 = img_size2[1],
            stone_num = stone_num[1],
            img_cat = img_cat[1],
            img_type = img_type[1],
            surgery_types = surgery_types[1],
            img_size = img_size[1],
            anom = anom[1],
            anom2 = anom2[1]) %>%
  mutate(ans = factor("BOTH")) %>%
  select(record_id, side, img_size2, ans,
         stone_num, img_cat, img_type, surgery_types,
         img_size, anom, anom2) -> both_surg_df

base_df5 %>%
  filter(surgery_types == "1") %>%
  select(record_id, side, img_size2, ans, stone_num, img_cat, img_type, surgery_types,
         img_size, anom, anom2) -> one_surg_df

total <- rbind(both_surg_df,one_surg_df)
total$ans <- factor(total$ans, levels = c("URS","PCNL","BOTH"))

burden <- select(test2$side_size_burden_op, record_id, side, burden)

total1 <- left_join(total, burden,
                    by = c("record_id","side")
                    )

######################GRAPHICAL ANALYSIS of version 1

gen_img(total1) #fix this

total$ans <- relevel(total$ans, "PCNL")

############################PRIMARY ANALYSIS

total %>%
  mutate(any_urs = (ans %in% c("BOTH","URS"))) %>%
  mutate(both = (ans == "BOTH")) %>%
  ungroup() %>%
  mutate(record_id = as.numeric(record_id)) %>%
  left_join(urologist_df, by = c("record_id")) -> total3

total3 %>%
  left_join(surg_time_df, by = "record_id") %>%
  left_join(stage2_df, by = "record_id") %>%
  left_join(asa_df, by = "record_id") %>%
  left_join(this_prev_int_df, by = "record_id") %>%
  left_join(any_prev_int_df, by = "record_id") %>%
  left_join(bmi_df, by = "record_id") %>%
  left_join(gender_df, by = "record_id") %>%
  left_join(white_df, by = "record_id") -> total4

pt_age_df <- read.csv("../data/pt_age.csv")
total4b <- left_join(total4, pt_age_df, by = "record_id")


#All patients analyzed
total4b %>%
  mutate(bmi2 = bmi/10,
         age2 = anon_age/10,
         size = img_size/10,
         ans2 = relevel(factor(ans),"PCNL")) -> primary_analysis

total4b %>%
  group_by(record_id) %>%
  summarize(gender = gender[1],
            bmi = bmi[1],
            white = white[1],
            anom2 = anom2[1],
            anon_age = anon_age[1]) -> total_pts

#among patients who have large stones... excluding patients who had both
total4b %>%
  filter(img_size >= 20) %>%
  filter(ans != "BOTH") %>%
  filter(img_type != "US") %>%
  mutate(any_urs = ans %in% c("URS","BOTH")) -> total5

total5 %>%
  group_by(record_id) %>%
  summarize(gender = gender[1],
            bmi = bmi[1],
            white = white[1],
            anom2 = anom2[1],
            anon_age = anon_age[1]) -> total_pts2

mean(total_pts2$anon_age, na.rm = TRUE)
sd(total_pts2$anon_age, na.rm = TRUE)

mean(total_pts2$bmi, na.rm = TRUE)
sd(total_pts2$bmi, na.rm = TRUE)

table(total_pts2$gender)
table(total_pts2$gender)/length(total_pts2$record_id)

table(total_pts2$white)
table(total_pts2$white)/length(total_pts2$record_id)

table(total_pts2$anom2)
table(total_pts2$anom2)/length(total_pts2$record_id)

mean(total5$img_size, na.rm = TRUE)
sd(total5$img_size, na.rm = TRUE)

table(total5$ans)
table(total5$ans)/dim(total5)[1]

table(total5$img_type)
table(total5$img_type)/dim(total5)[1]

table(total5$stone_num)
table(total5$stone_num)/dim(total5)[1]

table(total5$urologist)
table(total5$urologist)/dim(total5)[1]

table(total5$urologist)
table(total5$urologist)/dim(total5)[1]

table(total5$surg_size)
table(total5$surg_size)/dim(total5)[1]

total5 %>%
  mutate(bmi2 = bmi/10,
         age2 = anon_age/10,
         size = img_size/10,
         ans2 = relevel(factor(ans),"PCNL")) -> secondary_analysis

model_urs <- glm(any_urs ~ age2 + gender + bmi2 + white + anom2 +
                   stone_num + size + urologist,
                 family = binomial(link = "logit"), data = secondary_analysis)

summary(model_urs)
exp(coef(model_urs))
exp(confint(model_urs))