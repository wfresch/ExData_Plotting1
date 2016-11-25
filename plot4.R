constructPlot4 <- function() {
    
    # Store the household_power_consumption contents in a data-frame.
    hpc <- read.table("household_power_consumption.txt", 
                      header=TRUE, 
                      sep = ";", 
                      na.strings = "?")
    
    #Convert 'Date' column to an actual date.
    hpc[c("Date")] <- lapply(hpc[c("Date")], dmy)
    
    #Filter 'hpc' on 2/1/2007 and 2/2/2007
    hpc <- subset(hpc,Date == ymd("2007-02-01") | Date == ymd("2007-02-02"))

    #Get user-friendly Weekday labels for Date fields
    hpc$Weekday <- weekdays(hpc$Date)
    
    #Add Timestamp column for proper sorting
    hpc$Timestamp <- with(hpc, 
                          as.POSIXct(paste(Date, Time), 
                                     format = "%Y-%m-%d %H:%M:%S"))
    
    png(filename = "plot4.png", width = 480, height = 480)
    
    par(mfrow=c(2,2))
    
    #Graph 1
    plot(hpc$Timestamp,
         hpc$Global_active_power,
         type = "l",
         xlab="", 
         ylab = "Global Active Power (kilowatts)")
    
    #Graph 2
    plot(hpc$Timestamp,
         hpc$Voltage,
         type = "l",
         xlab="datetime",
         ylab = "Voltage")
        
    #Graph 3
    plot(hpc$Timestamp,
         hpc$Sub_metering_1,
         type = "l",
         xlab="",
         ylab = "Energy sub metering")
    
    lines(hpc$Timestamp, hpc$Sub_metering_2, type = "l", col = "red")
    lines(hpc$Timestamp, hpc$Sub_metering_3, type = "l", col = "blue")
    
    legend("topright",
           lty=1,
           col=c("black","red","blue"), 
           legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
           box.lty = 0,
           cex = 0.9,
           inset = 0.01)
    
    #Graph 4
    plot(hpc$Timestamp,
         hpc$Global_reactive_power,
         type = "l",
         xlab="datetime",
         ylab = "Global_reactive_power")
    
    dev.off()
}