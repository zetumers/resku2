library(plyr)
library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)
library(readxl)
library(bezier)
library(reshape2)
library(hash)
library(grid)

curse2_key <- c(6,17, 3,4,5,14,15,16,7,18,8,19)
curse2_value <- c(3,3,2,2,2,2,2,2,1,1,4,4)

curse2_df <- data.frame(key = curse2_key,
                        value = factor(curse2_value,
                                       labels = c("SWL","URS","PCNL","open")))

size_cascade <- list(cas_is_sxs,
                     cas_has_imgnp,
                     cas_has_ct_size)

resku4 <- tbl_df(resku4)

test1 <- aua_cascade(resku4, size_cascade)[[4]]

test1 %>%
        filter(variable %in% c("img_dx___2","img_dx___4")) %>%
        mutate(value = as.numeric(value)) %>%
        select(-order) %>%
        dcast(record_id ~ variable, mean) %>%
        mutate(img_type = 2*img_dx___2 + 1*img_dx___4) %>%
        select(record_id, img_type) %>%
        filter(img_type != 0) %>%
        mutate(img_type = factor(img_type,
                                 labels = c("CT","US","Both")))-> test6

test1 %>%
        aua_side_isolate1() %>%
        aua_side_isolate2("imgnp","number") %>%
        aua_side_pull_side("imgnp","number") %>%
        rename(stone_num = value) %>%
        select(-variable, -order) -> test7

test2 <- aua_guideline_22_25(test1)

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
        left_join(test7, by = c("record_id","side")) %>%
        select(-sxs) %>%
        mutate(stone_num = as.numeric(stone_num)) -> test3

test4 <- test3
test4$img_cat <- cut(test4$img_size, breaks = c(-1,10,20,200), labels = c(1,2,3))
test4$img_size <- aua_cut_size(test4$img_size)
test4$stone_num <- as.character(test4$stone_num)

test4 <- distinct(test4)

test4 %>%
        left_join(test6, by = c("record_id"))  %>%
        filter(!is.na(img_type)) %>%
        mutate(img_type = img_type) %>%
        filter(ans != "SWL")-> test4

test4 %>%
        group_by(record_id, side) %>%
        summarize(surgery_types = n()) -> test8

test4 <- left_join(test4, test8, by = c("record_id","side"))

####### LOGISTIC REGRESSION #######
test10 <- test4
test10$surg_size <- cut(test10$burden, breaks = c(-1,9,13,18), labels = c(1,2,3))
test10 <- mutate(test10, discord = (as.numeric(surg_size) < as.numeric(img_cat)))
#test10 <- filter(test10, ans == "URS")
#test10$stone_num <- as.factor(test10$stone_num)

test1 %>%
        aua_side_isolate1() %>%
        aua_side_isolate3("imgnp","loc") %>%
        aua_side_pull_side2("imgnp","loc") %>%
        aua_side_pull_nums("imgnp","loc") %>%
        filter(value == "1") %>%
        rename(stone_loc = num) %>%
        select(-variable, -order, -value) -> test9

left_join(test10, test9, by = c("record_id","side")) %>%
        mutate(stone_num = as.numeric(stone_num)) -> test11
test11$stone_loc <- as.factor(test11$stone_loc)

test11 <- test10
test11$stone_num <- as.factor(test11$stone_num)
test11$stone_num <- mapvalues(test11$stone_num, c("1","2","3","5"), c("1","2","3",">3"))



test11 <- mutate(test11, lower = as.numeric(stone_loc == 3))
test11 %>%
        group_by(record_id, side) %>%
        summarize(discord = discord[1],
                  lower = sum(lower, na.rm = TRUE),
                  stone_num = stone_num[1],
                  img_type = img_type[1],
                  img_size = img_size[1],
                  burden = burden[1],
                  ans = ans[1],
                  img_cat = img_cat[1],
                  surgery_types = surgery_types[1],
                  surg_size = surg_size[1]) -> test12

test13 <- filter(test11, surgery_types == 1)
#test13 <- filter(test13, img_type != "BOTH")
#test13 <- filter(test11, ans == "URS")
test13$stone_num <- as.factor(test13$stone_num)

model <- glm(discord ~ stone_num + img_type,
    family = binomial(link = "logit"), data = test10)

test1 %>%
        eval_multi("urinary_anomaly","anom") %>%
        filter(!is.na(anom)) %>%
        select(record_id, anom) %>%
        mutate(anom = as.factor(anom)) %>%
        right_join(test13, by = "record_id") %>%
        mutate(anom2 = (anom != 1))-> test15

model <- glm(discord ~ ans + stone_num + img_type + anom2,
             family = binomial(link = "logit"), data = test15)



test_rect1 <- data.frame(xmin = c(3,8,13,
                                  3,8,13),
                         xmax = c(7,12,17,
                                  7,12,17),
                         ymin = c(0,10.2,20.2,
                                  0,10.2,20.2),
                         ymax = c(9.8,19.8,60,
                                  9.8,19.8,60),
                         ans = c("URS","URS","URS","PCNL","PCNL","PCNL"),
                         alpha = c(.08,.08,.08,
                                   .08,.08,.08))

test_rect2 <- data.frame(xmin = c(3,8,
                                  3,8),
                         xmax = c(7,12,
                                  7,12),
                         ymin = c(10.2,20.2,
                                  10.2,20.2),
                         ymax = c(60,60,
                                  60,60),
                         ans = c("URS","URS","PCNL","PCNL"),
                         alpha = c(.08,.08,
                                   .08,.08))

test_rect3 <- data.frame(xmin = c(8,13,
                                  8,13),
                         xmax = c(12,17,
                                  12,17),
                         ymin = c(0,0,
                                  0,0),
                         ymax = c(9.8,19.8,
                                  9.8,19.8),
                         ans = c("URS","URS","PCNL","PCNL"),
                         alpha = c(.08,.08,
                                   .08,.08))

sp1 <- ggplot(test15, aes(x = burden,
                         y = img_size,
                         shape = stone_num))

sp1 + geom_point(position = position_jitter(width = 1.75, height = 2.1, seed = 102)) +
        theme(panel.grid.major.y = element_blank(),
              axis.text.x = element_text(angle = 0,
                                         hjust = .5,
                                         vjust = 0), 
              panel.background = element_rect(fill = NA)) +
        coord_cartesian(clip = 'off') +
        labs(shape = "Stone Number") +
        scale_color_gradient(low = "black", high = "red") +
        scale_shape_manual(values = c(19,1,4,8)) +
        scale_x_continuous(name = "Burden Estimated by Surgeon (mm)",
                           breaks = c(5,10,15),
                           labels = c("<10","10-20", ">20")) +
        scale_y_continuous(name = "Burden Estimated by Imaging (mm)",
                           breaks = c(0,10,20,30,40,50,55),
                           labels = c("0","10","20","30","40","50",">50"),
                           limits = c(0,60)) +
        ggtitle("Discrepancies in Stone Burden,\nby Operation Type and Stone Number") +
        #accurate
        geom_rect(data = test_rect1,
                  inherit.aes = FALSE,
                  aes(xmin = test_rect1$xmin,
                      ymin = test_rect1$ymin,
                      xmax = test_rect1$xmax,
                      ymax = test_rect1$ymax), color = "black", fill = NA, alpha = .20) +
        #surgeon underestimate
        geom_rect(data = test_rect2,
                  inherit.aes = FALSE,
                  aes(xmin = test_rect2$xmin,
                      ymin = test_rect2$ymin,
                      xmax = test_rect2$xmax,
                      ymax = test_rect2$ymax), color = "black", fill = "gray75", alpha = .20) +
        #surgeon overestimate
        geom_rect(data = test_rect3,
                  inherit.aes = FALSE,
                  aes(xmin = test_rect3$xmin,
                      ymin = test_rect3$ymin,
                      xmax = test_rect3$xmax,
                      ymax = test_rect3$ymax), color = "black", fill = "gray5", alpha = .20) +
        facet_grid( ~ ans)

bars_test <- data.frame(x_lower = c(3,8,13,
                                    3,8,13,
                                    8),
                        x_upper = c(7,12,17,
                                    7,12,17,
                                    12),
                        y_lower = c(0,0,0,
                                    10.2,10.2,20.2,
                                    20.2),
                        y_upper = c(9.8,9.8,19.8,
                                    60,19.8,60,
                                    60),
                        Discrepancy = factor(c("No discrepancy","Surgeon > Imaging","Surgeon > Imaging",
                                               "Surgeon < Imaging", "No discrepancy","No discrepancy",
                                               "Surgeon < Imaging"))
)

sp1 <- ggplot(test15, aes(x = burden,
                          y = img_size,
                          shape = stone_num))

#100, 96

sp1 + geom_rect(data = bars_test, inherit.aes = FALSE,
                aes(xmin = x_lower, xmax = x_upper,
                    ymin = y_lower, ymax = y_upper,
                    fill = Discrepancy), color = "black", alpha = 0.4) +
        scale_fill_manual(values = c("#FFFFFF","#010101","#A9A9A9")) +
        geom_point(position = position_jitter(width = 1.73, height = 2.1, seed = 100)) +
        theme(panel.grid.major.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_text(angle = 0,
                                         hjust = .5,
                                         vjust = 8.3),
              axis.title.x = element_text(vjust = 7.5),
              panel.background = element_rect(fill = NA)) +
        coord_cartesian(clip = 'off') +
        labs(shape = "Stone Number") +
        scale_color_gradient(low = "black", high = "red") +
        scale_shape_manual(values = c(19,1,4,8)) +
        scale_x_continuous(name = "Burden Estimated by Surgeon (mm)",
                           breaks = c(5,10,15),
                           labels = c("<10","10-20", ">20")) +
        scale_y_continuous(name = "Burden Estimated by Imaging (mm)",
                           breaks = c(0,10,20,30,40,50,55),
                           labels = c("0","10","20","30","40","50",">50"),
                           limits = c(0,60)) +
        ggtitle("Discrepancies in Stone Burden,\nby Operation Type and Stone Number") +
        labs(fill = "Burden Estimates") +
        facet_grid( ~ ans)
