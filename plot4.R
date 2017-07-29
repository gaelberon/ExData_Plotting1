# The purpose of this script is to generate 4 graphes that show:
# Graph 1: 'Global Active Power' Vs 'Time' (2/1/2007 and 2/2/2007)
# Graph 2: 'Voltage' Vs 'Time' (2/1/2007 and 2/2/2007)
# Graph 3: 'Energy sub metering' (for the 3 variables 'Sub_metering_1',
# 'Sub_metering_2' and 'Sub_metering_3') Vs 'Time' (2/1/2007 and 2/2/2007)
# Graph 4: 'Global_reactive_power' Vs 'Time' (2/1/2007 and 2/2/2007)

## the 'proceed_plot4' function generates the expected 4 graphes
proceed_plot4 <- function() {
        ########################################################################
        ## 1. Load the datafram 'hpc' using method 'read.table' from downloaded
        ##    file 'household_power_consumption.txt'
        hpc <- read.table(file = "household_power_consumption.txt",
                          header=TRUE,
                          sep=";",
                          na.strings = "?",
                          colClasses = c('character',
                                         'character',
                                         'numeric',
                                         'numeric',
                                         'numeric',
                                         'numeric',
                                         'numeric',
                                         'numeric',
                                         'numeric'))
        
        ########################################################################
        ## 2. Convert First 2 columns/variables 'Date' and 'Time' to Date/time
        ##    classes in R using the 'strptime()' and 'as.Date()' functions
        hpc$Date <- as.Date(hpc$Date, "%d/%m/%Y")
        
        ########################################################################
        ## 3. Keep only the data recorded on '01/02/2007' and '02/02/2007'
        hpc <- subset(hpc,Date == as.Date("2007-2-1") | Date == as.Date("2007-2-2"))
        
        ########################################################################
        ## 4. Keep only complete observations
        hpc <- hpc[complete.cases(hpc),]
        
        ########################################################################
        ## 5. Combine 'Date' and 'Time' into column 'Time' and remove 'Date'
        hpc$Time <- strptime(paste(hpc$Date, hpc$Time), "%Y-%m-%d %H:%M:%S")
        hpc <- hpc[, names(hpc) != 'Date']
        
        ########################################################################
        ## 6. Prepare the graph area with 2 lines and 2 columns and set the
        ##    inner and outer margins
        par(mfrow = c(2,2),
            mar=c(4,4,2,1),
            oma=c(0,0,2,0))
        
        ########################################################################
        ## 7. Generate graph 1: Plot the graph of 'Global_Active_Power'
        with(hpc, plot(Time,
                       Global_active_power,
                       type = "l",
                       xlab = '',
                       ylab = 'Global Active Power'))
        
        ########################################################################
        ## 8. Generate graph 2: Plot the graph of 'Voltage'
        with(hpc, plot(Time,
                       Voltage,
                       type = "l",
                       xlab = 'datetime'))
        
        ########################################################################
        ## 9. Generate graph 3: Plot the histogramme graph of
        ##    'Global_Active_Power'
        with(hpc, plot(Time,
                       Sub_metering_1,
                       type = "l",
                       xlab = '',
                       ylab = 'Energy sub metering'))
        
        ########################################################################
        ## 10. Plot the graph for 'Sub_metering_2'
        points(hpc$Time,
               hpc$Sub_metering_2,
               col = 'red',
               type = "l")
        
        ########################################################################
        ## 11. Plot the graph for 'Sub_metering_3'
        points(hpc$Time,
               hpc$Sub_metering_3,
               col = 'blue',
               type = "l")
        
        ########################################################################
        ## 12. Add the legend
        legend('topright',
               lwd = c('1','1','1'),
               legend = c('Sub_metering_1','Sub_metering_2','Sub_metering_3'),
               col = c('black', 'red', 'blue'))
        
        ########################################################################
        ## 13. Generate graph 4: Plot the graph of 'Global_reactive_power'
        with(hpc, plot(Time,
                       Global_reactive_power,
                       type = "l",
                       xlab = 'datetime'))
        
        ########################################################################
        ## 14. Copy the graph to file 'plot4.png'
        dev.copy(png,
                 file = 'plot4.png',
                 width = 480,
                 height = 480,
                 units = "px")
        
        ########################################################################
        ## 15. Close the 'png file' device
        dev.off()
        
        ########################################################################
        ## 16. reinitiate the graph area
        par(mfrow = c(1,1),
            mar=c(4,4,4,4),
            oma=c(0,0,0,0))
}