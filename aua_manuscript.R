library(broom)

curse2_key <- c(6,17, 3,4,5,14,15,16,7,18,8,19)
curse2_value <- c(3,3,2,2,2,2,2,2,1,1,4,4)
curse2_df <- data.frame(key = curse2_key,
                        value = factor(curse2_value,
                                       labels = c("SWL","URS","PCNL","open")))

size_cascade <- list(cas_is_sxs,
                     cas_has_imgnp,
                     cas_has_ct_size)

########Exclusion criteria filter: produces resku_df2#######

        #get age
age_df <- read.csv("data/pt_age.csv")

        #bleeding disorders
resku_df %>%
        filter(variable == "pmh___999", value == "1") -> bleeding
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

base_df <- aua_cascade(resku_df2, size_cascade)[[4]]

#######does a bunch of stuff#######
test2 <- aua_guideline_22_25(base_df)

test2$side_size_burden_op %>%
        filter(!is.na(img_size)) %>%
        filter(as.numeric(stone_op) %in% curse2_key) %>%
        mutate(ans = curse2_df$value[match(stone_op, curse2_df$key)]) %>%
        arrange(factor(ans, levels = c(4,3,1))) %>%
        distinct() %>%
        filter(burden %in% c("1","2","3")) %>%
        mutate(burden = as.numeric(burden), img_size = as.numeric(img_size)) %>%
        mutate(burden = burden * 5) %>%
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

processed_df$img_cat <- cut(processed_df$img_size, breaks = c(-1,10,20,200), labels = c(1,2,3))
processed_df$img_size2 <- aua_cut_size(processed_df$img_size)

processed_df %>%
        group_by(record_id, side) %>%
        summarize(surgery_types = n()) -> surg_num_df

processed_df <- left_join(processed_df, surg_num_df, by = c("record_id","side"))

base_df0 <- processed_df
base_df0$surg_size <- cut(base_df0$burden, breaks = c(-1,9,13,18), labels = c(1,2,3))
base_df0 <- mutate(base_df0, discord = (as.numeric(surg_size) < as.numeric(img_cat)))

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
                  burden = burden[1],
                  stone_num = stone_num[1],
                  img_cat = img_cat[1],
                  img_type = img_type[1],
                  surgery_types = surgery_types[1],
                  surg_size = surg_size[1],
                  discord = discord[1],
                  img_size = img_size[1],
                  anom = anom[1],
                  anom2 = anom2[1]) %>%
        mutate(ans = factor("BOTH")) %>%
        select(record_id, side, img_size2, burden, ans,
               stone_num, img_cat, img_type, surgery_types,
               surg_size, discord, img_size, anom, anom2) -> both_surg_df

base_df5 %>%
        filter(surgery_types == "1") %>%
        select(record_id, side, img_size2, burden, ans, stone_num, img_cat, img_type, surgery_types,
               surg_size, discord, img_size, anom, anom2) -> one_surg_df

total <- rbind(both_surg_df,one_surg_df)
total$ans <- factor(total$ans, levels = c("URS","PCNL","BOTH"))

######################GRAPHICAL ANALYSIS

gen_img(total)

total$ans <- relevel(total$ans, "PCNL")

############################PRIMARY ANALYSIS

total <- mutate(total, discord = (as.numeric(surg_size) < as.numeric(img_cat)))
total <- mutate(total, discord2 = (as.numeric(surg_size) != as.numeric(img_cat)))
total <- mutate(total, discord3 = (as.numeric(surg_size) > as.numeric(img_cat)))

total %>%
        mutate(any_urs = (ans %in% c("BOTH","URS"))) %>%
        mutate(both = (ans == "BOTH")) %>%
        ungroup() %>%
        mutate(record_id = as.numeric(record_id)) %>%
        left_join(urologist_df, by = c("record_id")) -> total3

#dates until encounter
resku_df %>%
        pull_date2() %>%
        arrange(date) -> dates

dates$variable = replace_df$value[match(dates$variable, replace_df$key)]

dates %>%
        filter(variable == "op") %>%
        filter(order == min(order)) %>%
        dplyr::rename(min = order) %>%
        select(record_id, min) %>%
        right_join(dates, by = "record_id") %>%
        filter(order <= min) %>%
        group_by(record_id) %>%
        summarize(enc_until = n()) -> dates2

total3 %>%
        left_join(surg_time_df, by = "record_id") %>%
        left_join(stage2_df, by = "record_id") %>%
        left_join(asa_df, by = "record_id") %>%
        left_join(dates2, by = "record_id") %>%
        left_join(this_prev_int_df, by = "record_id") %>%
        left_join(any_prev_int_df, by = "record_id") %>%
        left_join(bmi_df, by = "record_id") %>%
        left_join(gender_df, by = "record_id") %>%
        left_join(white_df, by = "record_id") -> total4

pt_age_df <- read.csv("pt_age.csv")
total4b <- left_join(total4, pt_age_df, by = "record_id")

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

#########Somehow measure complexity of patient care

#among patients who have large stones...
total4 %>%
        filter(img_size >= 20) %>%
        mutate(any_urs = ans %in% c("URS","BOTH")) -> total5

model_urs <- glm(any_urs ~ stone_num + img_size + 
                         anom2 + urologist + this_prev_int +
                         bmi + asa + gender + white,
                          family = binomial(link = "logit"), data = total5)

model_urs2 <- glm(any_urs ~ stone_num + img_size + 
                         anom2 + urologist + this_prev_int +
                         bmi + gender + white,
                 family = binomial(link = "logit"), data = total5)

model_urs3 <- glm(any_urs ~ bmi,
                  family = binomial(link = "logit"), data = total5)

model_urs4 <- glm(any_urs ~ img_size,
                  family = binomial(link = "logit"), data = total5)

#add the variables

model_urs <- glm(any_urs ~ stone_num + img_size + 
                         anom2 + urologist + this_prev_int +
                         bmi + asa + gender + white,
                 family = binomial(link = "logit"), data = total5a)

summary(model_urs)

model_urs2 <- glm(any_urs ~ stone_num + img_size + 
                          anom2 + urologist + this_prev_int +
                          bmi + gender + white,
                  family = binomial(link = "logit"), data = total5a)

summary(model_urs2)

total4a %>%
        filter(img_size >= 20) %>%
        mutate(any_urs = ans %in% c("URS","BOTH")) -> total5a

total5a %>%
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

mean(total5a$img_size, na.rm = TRUE)
sd(total5a$img_size, na.rm = TRUE)

table(total5a$ans)
table(total5a$ans)/dim(total5a)[1]

table(total5a$img_type)
table(total5a$img_type)/dim(total5a)[1]

table(total5a$stone_num)
table(total5a$stone_num)/dim(total5a)[1]

table(total4$urologist)
table(total4$urologist)/dim(total4)[1]

table(total5a$urologist)
table(total5a$urologist)/dim(total5a)[1]

table(total5a$surg_size)
table(total5a$surg_size)/dim(total5a)[1]

#####Visualize the outliers:

######
model_urs <- glm(any_urs ~ age2 + gender + bmi2 + white + anom2 +
                         stone_num + img_size + ,
                 family = binomial(link = "logit"), data = total5)


total4b %>%
        left_join(renal_df, by = c("record_id","side")) %>%
        filter(renal == 1) %>%
        mutate(bmi2 = bmi/10,
               age2 = anon_age/10,
               size = img_size/10,
               img_size3 = img_size/10) -> total4a

total4a %>%
        filter(img_size >= 20) %>%
        mutate(any_urs = ans %in% c("URS","BOTH")) %>%
        filter(ans != "BOTH") -> secondary_analysis

model_urs <- glm(any_urs ~ age2 + gender + bmi2 + white + anom2 +
                         stone_num + img_size3 + img_type + urologist,
                 family = binomial(link = "logit"), data = secondary_analysis)

summary(model_urs)

exp(coef(model_urs))
exp(confint(model_urs))

model_urs_data <- augment(model_urs) %>%
        mutate(index = 1:n())

ggplot(model_urs_data, aes(index, .std.resid)) + 
        geom_point(aes(color = stone_num), alpha = .5) +
        theme_bw()

model_urs_data %>%
        filter(abs(.std.resid) < 2.3) -> urs_sensitivity

model_urs_sensitivity <- glm(any_urs ~ age2 + gender + bmi2 + white + anom2 +
                                     stone_num + img_size3 + img_type + urologist,
                             family = binomial(link = "logit"), data = urs_sensitivity)

summary(model_urs_sensitivity)

exp(coef(model_urs_sensitivity))
exp(confint(model_urs_sensitivity))


total4a %>%
        filter(img_size >= 30) %>%
        mutate(any_urs = ans %in% c("URS","BOTH")) -> secondary_analysis2

model_urs2 <- glm(any_urs ~ age2 + gender + bmi2 + white + anom2 +
                         stone_num + img_size3 + img_type + urologist,
                 family = binomial(link = "logit"), data = secondary_analysis2)

summary(model_urs2)

exp(coef(model_urs2))
exp(confint(model_urs2))

#####sensitivity analysis for secondary analysis#####

secondary_analysis %>%
        filter(ans != "BOTH") -> secondary_analysis3

model_urs3 <- glm(any_urs ~ age2 + gender + bmi2 + white + anom2 +
                          stone_num + img_size3 + img_type + urologist,
                  family = binomial(link = "logit"), data = secondary_analysis3)

summary(model_urs3)

exp(coef(model_urs3))
exp(confint(model_urs3))

#####

model_urs4 <- glm(any_urs ~ age2 + gender + bmi2 + white + anom2 +
                          stone_num + img_size3 + img_type + urologist +
                          asa + this_prev_int,
                  family = binomial(link = "logit"), data = secondary_analysis3)

summary(model_urs4)

exp(coef(model_urs4))
exp(confint(model_urs4))

######


total4a %>%
        filter(img_size >= 25) %>%
        mutate(any_urs = ans %in% c("URS","BOTH")) %>%
        filter(ans != "BOTH") -> secondary_analysis4

model_urs4 <- glm(any_urs ~ age2 + gender + bmi2 + white + anom2 +
                          stone_num + img_size3 + img_type + urologist,
                  family = binomial(link = "logit"), data = secondary_analysis4)

summary(model_urs4)

exp(coef(model_urs4))
exp(confint(model_urs4))

######

model_urs5 <- glm(any_urs ~ age2 + gender + bmi2 + white + anom2 +
                          stone_num + img_size3 + img_type + urologist + asa + this_prev_int,
                  family = binomial(link = "logit"), data = secondary_analysis3)

summary(model_urs5)

exp(coef(model_urs5))
exp(confint(model_urs5))

######Analysis of missing information######

perc_missing <- function(wide_df){
        ans <- apply(wide_df, 2, function(col) sum(is.na(col))/length(col))
        return(ans)
}

perc_missing(total_pts)
perc_missing(total4b)