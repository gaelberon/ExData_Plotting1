# The purpose of this script is to generate a graph that shows the
# 'Global Active Power' (in kilowatts) generated in February the 1st and 2nd
# in year 2007

## the 'proceed_plot2' function generates the expected graph
proceed_plot2 <- function() {
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
        ## 6. Plot the graph of 'Global_Active_Power'
        with(hpc, plot(Time,
                       Global_active_power,
                       type = "l",
                       xlab = '',
                       ylab = 'Global Active Power (kilowatts)'))
        
        ########################################################################
        ## 7. Copy the graph to file 'plot2.png'
        dev.copy(png,
                 file = 'plot2.png',
                 width = 480,
                 height = 480,
                 units = "px")
        
        ########################################################################
        ## 8. Close the 'png file' device
        dev.off()
}
        