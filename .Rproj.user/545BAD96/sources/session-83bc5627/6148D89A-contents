url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
filename <- "peerreview.zip"
origin <- getwd()

if(!file.exists("data")){
        dir.create("data")
        setwd("./data")
        download.file(url = url,
                      destfile = filename,
                      method = "curl")
        unzip(zipfile = filename)
        setwd(origin)
}
NEI <- readRDS(file = "./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

NEIsub <- NEI[NEI$fips == "24510",]
yearsumsub <- with(NEIsub,tapply(Emissions,year,sum))

# how many row of each type in baltimore
sapply(split(NEIsub,NEIsub$type), nrow)
NEIsub$type <- as.factor(NEIsub$type)

#sum of emit in baltimore by type
with(NEIsub,tapply(Emissions,type,sum))


# sum the emit by type of each year
library("data.table")

sumsub1999 <- with(NEIsub[NEIsub$year==1999,],tapply(Emissions,type,sum))
sumsub1999 <- as.data.frame(sumsub1999)
names(sumsub1999) <- "value"
sumsub1999 <- data.frame(sumsub1999,year=rep(1999,4))
sumsub1999 <- setDT(sumsub1999,keep.rownames = TRUE)

sumsub2002 <- with(NEIsub[NEIsub$year==2002,],tapply(Emissions,type,sum))
sumsub2002 <- as.data.frame(sumsub2002)
names(sumsub2002) <- "value"
sumsub2002 <- data.frame(sumsub2002,year=rep(2002,4))
sumsub2002 <- setDT(sumsub2002,keep.rownames = TRUE)

sumsub2005 <- with(NEIsub[NEIsub$year==2005,],tapply(Emissions,type,sum))
sumsub2005 <- as.data.frame(sumsub2005)
names(sumsub2005) <- "value"
sumsub2005 <- data.frame(sumsub2005,year=rep(2005,4))
sumsub2005 <- setDT(sumsub2005,keep.rownames = TRUE)

sumsub2008 <- with(NEIsub[NEIsub$year==2008,],tapply(Emissions,type,sum))
sumsub2008 <- as.data.frame(sumsub2008)
names(sumsub2008) <- "value"
sumsub2008 <- data.frame(sumsub2008,year=rep(2008,4))
sumsub2008 <- setDT(sumsub2008,keep.rownames = TRUE)

mrg <- rbind(sumsub1999,sumsub2002,sumsub2005,sumsub2008)
names(mrg) <- c("type","value","year" )
mrg$type <- as.factor(mrg$type)
str(mrg)
# types <- row.names(mrg)
# rownames(mrg) <- NULL
# mrg <- cbind(mrg,types)
# mrg[,1:4] <- as.numeric(mrg[,1:4])
# mrg

library("ggplot2")

dev.cur()
windows(480,480)
g <- ggplot(data = mrg,aes(x = year,y = value,group = type,color = type))
g + geom_line(lwd = 1,alpha = 1/2) + 
        geom_point(size = 4,alpha = 1/2) + 
        theme_light() + 
        ggtitle("Total Emissions from PM2.5 in the Baltimore City by Type")
dev.copy(png,"./plot3.png", units = "px")
dev.off()

