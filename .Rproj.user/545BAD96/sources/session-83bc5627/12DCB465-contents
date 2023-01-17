# getwd()
data <- read.table(file = "./household_power_consumption.txt", header = TRUE, 
                   sep = ";", na.strings = "?")
x <- with(data, paste(Date,Time))
x <- strptime(x, "%d/%m/%Y %H:%M:%S")
data <- cbind(x,data)
data <- data[data$x >= "2007-02-01 00:00:00" & data$x < "2007-02-03 00:00:00",]

#set par
dev.off()
windows(10,10)
par(mfrow = c(2,2), cex = 0.8)

#1
with(data,plot(x, Global_active_power,ylab = "Global Active Power"
               ,xlab = "",type = "n"))
lines(data$x, data$Global_active_power, col = "pink")

#2
with(data,plot(x, Voltage, col = "pink", type = "n", xlab = "datetime"))
with(data,lines(x, Voltage, col = "pink"))

#3
with(data,plot(x, Sub_metering_1, type = "n",xlab = "",ylab = "Energy sub metering"))
lines(data$x,data$Sub_metering_1, col = "sienna1")
lines(data$x,data$Sub_metering_2, col = "steelblue" )
lines(data$x,data$Sub_metering_3, col = "lightgoldenrod1" )

legend("topright",lty = 1, lwd = 3,
       col = c("sienna1","steelblue","lightgoldenrod1"),
       legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       cex = 0.8, bty = "n")
#4
with(data, plot(x, Global_reactive_power, type = "n", ylim = c(0,0.5), xlab = "datetime"))
lines(data$x, data$Global_reactive_power, col = "pink")

#png
dev.cur()
dev.copy(png,file = "./plot4.png", height = 480, width = 480, units = "px")
dev.off()
