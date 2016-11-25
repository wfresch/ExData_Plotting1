constructPlot2 <- function() {
    
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
    
    png(filename = "plot2.png", width = 480, height = 480)
        
    plot(hpc$Timestamp,
         hpc$Global_active_power,
         type = "l",
         xlab="", 
         ylab = "Global Active Power (kilowatts)")
    
    dev.off()
}