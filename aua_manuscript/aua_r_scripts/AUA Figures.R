#curse = current surgeries
curse_key <- c(6,17, 31,35, 3,4,5,14,15,16, 7,18,8,19)
curse_value <- c("PCNL","PCNL","US","US","URS","URS","URS","URS","URS","URS","SWL","SWL","open","open")

curse2_df <- data.frame(key = curse2_key,
                        value = factor(curse2_value,
                                       levels = c("SWL","URS","PCNL")))

curse_df <- data.frame(key = curse_key, value = curse_value)

size_cascade <- list(cas_is_sxs,
                       cas_has_imgnp,
                       cas_has_ct_size)

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

test6 <- data.frame(record_id = test6$record_id,
                    img_type = factor(test6$img_type, levels = c("US","CT","Both")))

test2 <- aua_guideline_22_25(test1)

test2$side_size_burden_op %>%
        filter(!is.na(img_size)) %>%
        filter(as.numeric(stone_op) %in% curse_key) %>%
        mutate(ans = curse_df$value[match(stone_op, curse_df$key)]) %>%
        distinct() %>%
        filter(burden %in% c("1","2","3")) %>%
        mutate(burden = as.numeric(burden),
               img_size = as.numeric(img_size)) %>%
        mutate(burden = burden * 5) %>%
        select(-stone_op) %>%
        left_join(test7, by = c("record_id","side")) %>%
        mutate(stone_num = as.numeric(stone_num))-> test3

#curse = current surgeries
curse2_key <- c(6,17, 3,4,5,14,15,16,7,18,8,19)
curse2_value <- c(3,3,2,2,2,2,2,2,1,1,4,4)

curse2_df <- data.frame(key = curse2_key,
                        value = factor(curse2_value,
                                       labels = c("SWL","URS","PCNL","open")))

size_cascade <- list(cas_is_sxs,
                     cas_has_imgnp,
                     cas_has_ct_size)

test1 <- aua_cascade(resku4, size_cascade)

test2 <- aua_guideline_22_25(test1[[4]])

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
        mutate(stone_num = as.numeric(stone_num)) -> test3

test4 <- test3
test4$img_cat <- cut(test4$img_size, breaks = c(-1,10,20,200), labels = c(1,2,3))
test4$img_size <- aua_cut_size(test4$img_size)

test4 %>%
        left_join(test6, by = c("record_id"))  %>%
        filter(!is.na(img_type)) %>%
        mutate(img_type = img_type) %>%
        mutate(stone_num = as.character(stone_num))-> test4

curse2_df <- data.frame(key = curse2_key,
                        value = factor(curse2_value,
                                       levels = c("SWL","URS","PCNL")))

sp1 <- ggplot(test4, aes(x = burden,
                         y = img_size,
                         shape = img_type))

sp1 + geom_point(position = position_jitter(width = 1.7, height = 3.5, seed = 102)) +
        labs(shape = "Image Type") +
        scale_shape_manual(values = c(0,16,4)) +
        scale_x_continuous(name = "Burden Estimated by Surgeon (mm)",
                           breaks = c(5,10,15),
                           labels = c("<10","10-20", ">20")) +
        theme(axis.text.x = element_text(angle = 0,
                                         hjust = .5,
                                         vjust = 0)) +
        scale_y_continuous(name = "Burden Estimated by New Patient Imaging (mm)",
                           breaks = c(0,10,20,30,40,50,55),
                           labels = c("0","10","20","30","40","50",">50"),
                           limits = c(-1,65)) +
        ggtitle("Discrepancies in Imaging, by Operation Type and Imaging Type") +
        geom_rect(data = test_rect1,
                  inherit.aes = FALSE,
                  aes(xmin = test_rect1$xmin,
                      ymin = test_rect1$ymin,
                      xmax = test_rect1$xmax,
                      ymax = test_rect1$ymax), color = NA, fill = "green", alpha = .20) +
        geom_rect(data = test_rect2,
                  inherit.aes = FALSE,
                  aes(xmin = test_rect2$xmin,
                      ymin = test_rect2$ymin,
                      xmax = test_rect2$xmax,
                      ymax = test_rect2$ymax), color = NA, fill = "red", alpha = .20) +
        geom_rect(data = test_rect3,
                  inherit.aes = FALSE,
                  aes(xmin = test_rect3$xmin,
                      ymin = test_rect3$ymin,
                      xmax = test_rect3$xmax,
                      ymax = test_rect3$ymax), color = NA, fill = "yellow", alpha = .20) +
        facet_grid( ~ ans)


test_rect1 <- data.frame(xmin = c(3,8,13,
                                 3,8,13,
                                 3,8,13),
                        xmax = c(7,12,17,
                                 7,12,17,
                                 7,12,17),
                        ymin = c(0,10.2,20.2,
                                 0,10.2,20.2,
                                 0,10.2,20.2),
                        ymax = c(9.8,19.8,62.5,
                                 9.8,19.8,62.5,
                                 9.8,19.8,62.5),
                        ans = c("SWL","SWL","SWL","URS","URS","URS","PCNL","PCNL","PCNL"),
                        alpha = c(.08,.08,.08,
                                  .08,.08,.08,
                                  .08,.08,.08))

test_rect2 <- data.frame(xmin = c(3,8,
                                 3,8,
                                 3,8),
                        xmax = c(7,12,
                                 7,12,
                                 7,12),
                        ymin = c(10.2,20.2,
                                 10.2,20.2,
                                 10.2,20.2),
                        ymax = c(62.5,62.5,
                                 62.5,62.5,
                                 62.5,62.5),
                        ans = c("SWL","SWL","URS","URS","PCNL","PCNL"),
                        alpha = c(.08,.08,
                                  .08,.08,
                                  .08,.08))

test_rect3 <- data.frame(xmin = c(8,13,
                                  8,13,
                                  8,13),
                         xmax = c(12,17,
                                  12,17,
                                  12,17),
                         ymin = c(0,0,
                                  0,0,
                                  0,0),
                         ymax = c(9.8,19.8,
                                  9.8,19.8,
                                  9.8,19.8),
                         ans = c("SWL","SWL","URS","URS","PCNL","PCNL"),
                         alpha = c(.08,.08,
                                   .08,.08,
                                   .08,.08))



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


sp1 <- ggplot(test4, aes(x = burden,
                         y = img_size,
                         shape = stone_num,
                         color = surgery_types))

sp1 + geom_point(position = position_jitter(width = 1.7, height = 2, seed = 102)) +
        labs(shape = "Stone Number") +
        scale_color_gradient(low = "black", high = "red") +
        scale_shape_manual(values = c(19,1,4,8)) +
        scale_x_continuous(name = "Burden Estimated by Surgeon (mm)",
                           breaks = c(5,10,15),
                           labels = c("<10","10-20", ">20")) +
        theme(axis.text.x = element_text(angle = 0,
                                         hjust = .5,
                                         vjust = 0)) +
        scale_y_continuous(name = "Burden Estimated by New Patient Imaging (mm)",
                           breaks = c(0,10,20,30,40,50,55),
                           labels = c("0","10","20","30","40","50",">50"),
                           limits = c(-1,65)) +
        ggtitle("Discrepancies in Stone Burden, by Operation Type and Stone Number") +
        geom_rect(data = test_rect1,
                  inherit.aes = FALSE,
                  aes(xmin = test_rect1$xmin,
                      ymin = test_rect1$ymin,
                      xmax = test_rect1$xmax,
                      ymax = test_rect1$ymax), color = NA, fill = "green", alpha = .20) +
        geom_rect(data = test_rect2,
                  inherit.aes = FALSE,
                  aes(xmin = test_rect2$xmin,
                      ymin = test_rect2$ymin,
                      xmax = test_rect2$xmax,
                      ymax = test_rect2$ymax), color = NA, fill = "red", alpha = .20) +
        geom_rect(data = test_rect3,
                  inherit.aes = FALSE,
                  aes(xmin = test_rect3$xmin,
                      ymin = test_rect3$ymin,
                      xmax = test_rect3$xmax,
                      ymax = test_rect3$ymax), color = NA, fill = "yellow", alpha = .20) +
        facet_grid( ~ ans)

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
