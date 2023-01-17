# getwd()
data <- read.table(file = "./household_power_consumption.txt", header = TRUE, 
                   sep = ";", na.strings = "?")
x <- with(data, paste(Date,Time))
x <- strptime(x, "%d/%m/%Y %H:%M:%S")
data <- cbind(x,data)
data <- data[data$x >= "2007-02-01 00:00:00" & data$x < "2007-02-03 00:00:00",]


windows(width=10, height=10)
with(data,plot(x,Sub_metering_1, type = "n", ylab = "Energy sub metering"))
# rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray97")
lines(data$x,data$Sub_metering_1, col = "sienna1")
lines(data$x,data$Sub_metering_2, col = "steelblue" )
lines(data$x,data$Sub_metering_3, col = "lightgoldenrod1" )


legend("topright",lty = 1, lwd = 3,
       col = c("sienna1","steelblue","lightgoldenrod1"),
       legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       cex = 0.8, pt.cex = 1)

dev.cur()
dev.copy(png, file = "./plot3.png", height = 480, width =480, units = "px")
dev.off()
