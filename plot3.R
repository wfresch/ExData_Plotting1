constructPlot3 <- function() {

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
    
    png(filename = "plot3.png", width = 480, height = 480)
        
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
           legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    
    dev.off()
}