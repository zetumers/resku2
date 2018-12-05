

gen_img <- function(df){
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

        sp1 <- ggplot(df, aes(x = burden,
                                 y = img_size2,
                                 shape = stone_num))
        
        sp1 + geom_rect(data = bars_test, inherit.aes = FALSE,
                        aes(xmin = x_lower, xmax = x_upper,
                            ymin = y_lower, ymax = y_upper,
                        fill = Discrepancy), color = "black", alpha = 0.4) +
                scale_fill_manual(values = c("#FFFFFF","#010101","#A9A9A9")) +
                geom_point(position = position_jitter(width = 1.74, height = 2.2, seed = 100)) +
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
}