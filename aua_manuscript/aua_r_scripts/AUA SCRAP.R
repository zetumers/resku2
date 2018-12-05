#pcnl
curse[[6]] <- 3
curse[[17]] <- 3
#ultrasound
curse[[31]] <- 4
curse[[35]] <- 4
#urs
curse[[3]] <- 2
curse[[4]] <- 2
curse[[5]] <- 2
curse[[14]] <- 2
curse[[15]] <- 2
curse[[16]] <- 2
#swl
curse[[7]] <- 1
curse[[18]] <- 1
#open
curse[[8]] <- 5
curse[[19]] <- 5


jpeg("Img Size vs Op Size Box.jpg", width = 600, height = 600)

boxplot(img_size~burden, data = test3,
        main = "Correspondence between Burden on CT \n and Burden Recorded during Operation",
        xlab = "Burden during Op",
        ylab = "CT Burden (mm)")

rect(xleft = .7, ybottom = 0, xright = 1.3, ytop = 10,
     border = NA, col= '#00ff0040')

rect(xleft = 1.7, ybottom = 10, xright = 2.3, ytop = 20,
     border = NA, col= '#00ff0040')

rect(xleft = 2.7, ybottom = 20, xright = 3.3, ytop = 110,
     border = NA, col= '#00ff0040')

dev.off()

jpeg("Img Size vs Op Size Dot.jpg", width = 500, height = 500)

plot(img_size~burden, data = test3,
     main = "Correspondence between \n Burden on CT and Burden Recorded during Operation",
     xlab = "Burden during Op",ylab = "CT Burden (mm)")

rect(xleft = .7, ybottom = 0, xright = 1.3, ytop = 10,
     border = NA, col= '#00ff0040')

rect(xleft = 1.7, ybottom = 10, xright = 2.3, ytop = 20,
     border = NA, col= '#00ff0040')

rect(xleft = 2.7, ybottom = 20, xright = 3.3, ytop = 110,
     border = NA, col= '#00ff0040')

dev.off()

jpeg("Img Size vs Op Size Dot v2.jpg", width = 500, height = 500)

sp1 <- ggplot(test3, aes(x = burden,
                         y = img_size,
                         shape = as.factor(ans)))

sp1 + geom_point(position = position_jitter(width = 2, height = 3))


rect(xleft = .7, ybottom = 0, xright = 1.3, ytop = 10,
     border = NA, col= '#00ff0040')

rect(xleft = 1.7, ybottom = 10, xright = 2.3, ytop = 20,
     border = NA, col= '#00ff0040')

rect(xleft = 2.7, ybottom = 20, xright = 3.3, ytop = 110,
     border = NA, col= '#00ff0040')


sp1 <- ggplot(test3, aes(x = burden,
                         y = img_size))

sp1 + geom_point(position = position_jitter(width = 2, height = 3)) +
        facet_grid( ~ ans)



rect(xleft = .7, ybottom = 0, xright = 1.3, ytop = 10,
     border = NA, col= '#00ff0040')

rect(xleft = 1.7, ybottom = 10, xright = 2.3, ytop = 20,
     border = NA, col= '#00ff0040')

rect(xleft = 2.7, ybottom = 20, xright = 3.3, ytop = 110,
     border = NA, col= '#00ff0040')

dev.off()