#######TABLE 1 FEATURES#########

mean(total_pts$anon_age, na.rm = TRUE)
sd(total_pts$anon_age, na.rm = TRUE)

table(total_pts$gender)

table(total_pts$anom2)/dim(total_pts)[1]
table(total_pts$gender)/dim(total_pts)[1]

mean(total_pts$bmi, na.rm = TRUE)
sd(total_pts$bmi, na.rm = TRUE)

mean(primary_analysis$img_size, na.rm = TRUE)
sd(primary_analysis$img_size, na.rm = TRUE)

table(primary_analysis$surg_size)
table(primary_analysis$surg_size)/dim(primary_analysis)[1]

#########

#########Univariate OR and factors TABLE 2#############

primary_analysis %>%
        group_by(discord, anom2) %>%
        summarize(n()) -> anom_or

primary_analysis %>%
        mutate(pcnl = (ans == "PCNL")) %>%
        group_by(discord, pcnl) -> pcnl_or

primary_analysis %>%
        filter(ans %in% c("PCNL","URS")) %>%
        group_by(discord, ans) %>%
        summarize(n())-> urs_or

primary_analysis %>%
        filter(ans %in% c("PCNL","BOTH")) %>%
        group_by(discord, ans) %>%
        summarize(n())-> both_or

primary_analysis %>%
        filter(img_type %in% c("CT","US")) %>%
        group_by(discord, img_type) %>%
        summarize(n())-> us_img_or

primary_analysis %>%
        filter(img_type %in% c("CT","Both")) %>%
        group_by(discord, img_type) %>%
        summarize(n())-> both_img_or

primary_analysis %>%
        filter(stone_num %in% c("1","2")) %>%
        group_by(discord, stone_num) %>%
        summarize(n())-> size_2_or

primary_analysis %>%
        filter(stone_num %in% c("1","3")) %>%
        group_by(discord, stone_num) %>%
        summarize(n())-> size_3_or

primary_analysis %>%
        filter(stone_num %in% c("1",">3")) %>%
        group_by(discord, stone_num) %>%
        summarize(n())-> size_g3_or

primary_analysis %>%
        filter(urologist %in% c("1","2")) %>%
        group_by(discord, urologist) %>%
        summarize(n())-> uro_or

###########

###########Multivariate Analysis TABLE 2###########

model_discord <- glm(discord ~ age2 + gender + bmi2 + white + anom2 +
                             stone_num + img_type +  ans + urologist,
                     family = binomial(link = "logit"), data = primary_analysis)

summary(model_discord)
exp(coef(model_discord))
exp(confint(model_discord))

###########

##############Sensitivity Analysis: eliminating influential patients#######

model_discord_data <- augment(model_discord) %>%
        mutate(index = 1:n())

mean(model_discord_data$.cooksd, na.rm = TRUE)

model_discord_data %>%
        filter(abs(.std.resid) < 3) -> discord_sensitivity

model_discord_sensitivity <- glm(discord ~ age2 + gender + bmi2 + white + anom2 +
                                         stone_num + img_type + ans2 + urologist,
                                 family = binomial(link = "logit"), data = discord_sensitivity)

summary(model_discord_sensitivity)
exp(coef(model_discord_sensitivity))
exp(confint(model_discord_sensitivity))

model_discord_data %>%
        filter(abs(.std.resid) < 2.5) -> discord_sensitivity

model_discord_sensitivity <- glm(discord ~ age2 + gender + bmi2 + white + anom2 +
                                         stone_num + img_type + ans2 + urologist,
                                 family = binomial(link = "logit"), data = discord_sensitivity)

summary(model_discord_sensitivity)

exp(coef(model_discord_sensitivity))
exp(confint(model_discord_sensitivity))

ggplot(model_discord_data, aes(index, .std.resid)) + 
        geom_point(aes(color = stone_num), alpha = .5) +
        theme_bw()

############

############Sensitivity Analysis NO PCNL###########

primary_analysis3 <- filter(primary_analysis, ans == "URS")
model_discord3 <- glm(discord ~ age2 + gender + bmi2 + white + anom2 +
                              stone_num + img_type + urologist,
                      family = binomial(link = "logit"), data = primary_analysis3)

summary(model_discord3)
exp(coef(model_discord3))
exp(confint(model_discord3))

#############

############Sensitivity Analysis NO URETERAL###########
primary_analysis %>%
        left_join(renal_df, by = c("record_id","side")) %>%
        filter(renal == 1) -> primary_analysis2

model_discord2 <- glm(discord ~ age2 + gender + bmi2 + white + anom2 +
                              stone_num + img_type +  relevel(factor(ans),"PCNL") + urologist,
                      family = binomial(link = "logit"), data = primary_analysis2)

summary(model_discord2)
exp(coef(model_discord2))
exp(confint(model_discord2))
##########

#######Overestimates and Underestimates#####

primary_analysis4 <- mutate(primary_analysis,
                            discord3 = 1*(as.numeric(surg_size) > as.numeric(img_cat)) +
                                    -1*(as.numeric(surg_size) < as.numeric(img_cat)))

table(primary_analysis4$discord3, primary_analysis4$stone_num)

chisq.test(table(primary_analysis4$discord3, primary_analysis4$stone_num))

#########
