# getwd()
data <- read.table(file = "./household_power_consumption.txt", header = TRUE, 
                   sep = ";", na.strings = "?")
x <- with(data, paste(Date,Time))
x <- strptime(x, "%d/%m/%Y %H:%M:%S")
data <- cbind(x,data)
data <- data[data$x >= "2007-02-01 00:00:00" & data$x < "2007-02-03 00:00:00",]


dev.cur()
hist(data$Global_active_power, xlab = "Global Active Power (kilowatts)",
     main = "Global Active Power",col = "pink", ylim = c(0,1200))
dev.copy(png, file = "./plot1.png",width = 480, height = 480,units = "px")
dev.off()
