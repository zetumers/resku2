testa <- pull_date2(resku_df)

filter(testa, record_id == "7")

replace_df <- data.frame(key = c("date_np","date_surgery_v2","date_fu_postop","date_fu",
                                 "np","op","po","fu"),
                         value = c("np","op","po","fu",
                                   "0","2","1","-2"))

replace2_df <- data.frame(key = c("np","op","po","fu"),
                         value = c(0,2,1,-2))


replace_df

testa %>%
        group_by(order, variable) %>%
        summarize(avg_date = mean(date, na.rm = TRUE),
                  sd_date = sd(date, na.rm = TRUE),
                  date_05 = quantile(date, .05, na.rm = TRUE),
                  date_25 = quantile(date, .25, na.rm = TRUE),
                  date_50 = quantile(date, .50, na.rm = TRUE),
                  date_75 = quantile(date, .75, na.rm = TRUE),
                  date_95 = quantile(date, .95, na.rm = TRUE)) -> testb

testa$variable = replace_df$value[match(testa$variable, replace_df$key)]

testa %>%
        group_by(record_id) %>%
        arrange(order) %>%
        #dplyr::rename(enc = order) %>%
        mutate(last_enc = lag(order)) %>%
        mutate(var1 = paste(variable,order, sep = '')) %>%
        mutate(last_date = lag(date)) %>%
        mutate(days_from = date - last_date) %>%
        mutate(to = var1) %>%
        mutate(from = lag(var1)) %>%
        mutate(id = paste(from,"_to_",to, sep ='')) %>%
        select(-last_date, -var1) -> testc
testc$days_from[is.na(testc$days_from)] <- 0


#quality control:
#remove all peeps who don't have dates after new patient records
testc %>%
        filter(order - last_enc > 1) %>%
        select(record_id, order, last_enc, from, to, days_from) -> no_date
testc %>%
        filter(!(record_id %in% no_date$record_id)) -> testd
#remove all peeps who don't have new patient dates
testc %>%
        filter(order == 2, is.na(from)) -> no_np_date
testd %>%
        filter(!(record_id %in% no_np_date$record_id)) -> teste
#remove all peeps who have dates that we know were entered incorrectly.
testc %>%
        filter(days_from < 0) -> incorrect_date
teste %>%
        filter(!(record_id %in% incorrect_date$record_id)) -> testf

testf %>%
        ungroup() %>%
        group_by(from, to) %>%
        summarize(count = n(),
                  order = order[1],
                  id = id[1],
                  enc_name = variable[1],
                  mean_interval = mean(days_from, na.rm = TRUE),
                  sd_interval = sd(days_from, na.rm = TRUE),
                  interval_05 = quantile(days_from, .05, na.rm = TRUE),
                  interval_25 = quantile(days_from, .25, na.rm = TRUE),
                  interval_50 = quantile(days_from, .50, na.rm = TRUE),
                  interval_75 = quantile(days_from, .75, na.rm = TRUE),
                  interval_95 = quantile(days_from, .95, na.rm = TRUE),
                  mean_date = mean(date, na.rm = TRUE)) -> edges

#there's a lot of edges that we don't
#need
edges %>%
        filter(order <= 6) %>%
        arrange(order) -> small_edges

small_edges %>%
        select(from, to, count, mean_interval) %>%
        filter(!is.na(from)) -> edges_df

small_edges %>%
        group_by(to) %>%
        summarize(count = sum(count),
                  order = order[1],
                  name = enc_name[1],
                  x = sum(mean_date*count)/sum(count)) %>%
        mutate(y = replace2_df$value[match(name, replace2_df$key)]) %>%
        dplyr::rename(id = to) %>%
        select(id, x, y, name, count, order) -> nodes_df

library(igraph)
library(ggraph)

gr <- graph_from_data_frame(edges_df, directed = TRUE)
gr2 <- create_layout(gr, layout = 'kk')
gr3 <- select(gr2, name, ggraph.origin_index, circular)
ggraph(gr) +
        geom_edge_link(aes(edge_width = count),
                       arrow = arrow(length = unit(4, 'mm')), 
                       end_cap = circle(1, 'mm')) +
        geom_node_point()


        scale_fill_manual(values = c(19,1,4,8,2))










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

library(ggplot)