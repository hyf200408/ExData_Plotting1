options(stringsAsFactors = FALSE)
## Read in the data from the dates 2007-02-01 and 2007-02-02 and remove NAs.
readin <- function(filename = "household_power_consumption.txt", 
                   datesofinterest = c("2007-02-01","2007-02-02")) {
    ## Read in the whole dataset.
    library(data.table)
    alldata <- fread(filename, data.table = FALSE)
    ## Extract data from the needed dates.
    dates <- as.Date(alldata[,1], "%d/%m/%Y")
    datesofinterest <- as.Date(datesofinterest)
    totaldata <- alldata[dates %in% datesofinterest,]
    ## Remove lines with NA.
    NAtest <- sapply(as.data.frame(t(totaldata == "?")),sum)
    totaldata[NAtest == 0,]
    ## Set class of variable.
    types <- sapply(fread(filename,data.table = FALSE,nrows = 10),class)
    for (i in 1:ncol(totaldata)) {class(totaldata[,i]) <- types[i]}
    totaldata[,10] <- as.POSIXct(strptime(
        paste(totaldata[,1],totaldata[,2]), "%d/%m/%Y %H:%M:%S"))
    totaldata
}
dataset <- readin()
## Draw the line chart for global active power.
png(filename = "plot2.png", width = 480, height = 480)
plot(dataset[,10], dataset[,3], type="l", xlab = "", 
     ylab = "Global Active Power (kilowatts)")
dev.off()