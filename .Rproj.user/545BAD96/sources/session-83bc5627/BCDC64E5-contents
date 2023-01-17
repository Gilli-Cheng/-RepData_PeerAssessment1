g <- ggplot(testdat, aes(x = myx, y = myy))
g <- ggplot(mpg, aes(x = displ, y = hwy, color = factor(year))) + 
        facet_grid(drv ~ cyl, margins = TRUE)
g + geom_point() + 
        facet_grid(drv ~ cyl, margins = TRUE) + 
        geom_smooth(method = "lm", se = FALSE, size = 2, color = "black")+
        labs(x="Displacement", y = "Highway Mileage", title = "Swirl Rules!")
g + geom_point(color = "pink", size = 4, alpha = 1/2) + 
        geom_smooth(method = "lm") + 
        facet_grid( . ~ drv) + 
        ggtitle("Swirl Rules!")
qplot(displ, hwy, data = mpg, color = drv, geom = c("point", "smooth"))
g + geom_point(aes(color = drv), size = 2, alpha = 1/2) + 
        geom_smooth(size = 4, linetype = 3, method = "lm", se = FALSE)
g + geom_point() + geom_smooth(method = "lm") + 
        facet_grid( . ~ drv) + ggtitle("Swirl Rules!")
ggplot(data = diamonds, aes(carat, price)) + geom_boxplot() + facet_grid(. ~ cut)
cutpoints <- quantile(diamonds$carat, seq(0,1,length = 4), na.rm = TRUE)
diamonds$car2 <- cut(diamonds$carat, cutpoints)
myd <- which(is.na(diamonds$car2))
myd <- diamonds[diamonds$car2 %in% NA,]