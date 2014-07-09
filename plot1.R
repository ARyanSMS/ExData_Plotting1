## THE FIRST CHUNK OF CODE DOES THE FOLLOWING:
## 1. Determine where the relevant lines start (according to the date)
## 2. Determine where the relevant lines end (according to the date)
## 3. Read the appropriate lines to a data frame
## 4. Convert dates and times into date-time format and consolidate into a single column
## 5. Give the columns appropriate names
## This code is consistent amongst all 4 R scripts

## lineNum will be used to help identify which lines need to be read in
lineNum <- 0

## startLine and endLine will eventually store the first and last line to be stored
startLine <- 0
endLine <- 0

## The start and end dates to retrieve data from
startDate <- as.Date("01/02/2007", "%d/%m/%Y")
endDate <- as.Date("02/02/2007", "%d/%m/%Y")

## Create a connection to the text file to read in
inputFile <- "household_power_consumption.txt"
con <- file(inputFile, open = "r")

## Loop through the lines in the text file until we assign the endLine
while(endLine == 0) {
    ## Read a single line in from the connection, and keep track of which line it is
    lineNum <- lineNum + 1
    line <- readLines(con, n = 1)
    
    ## Split the line by the semi-colons
    line <- strsplit(line, ";")
    
    ## Is this the header row?
    if(lineNum == 1) {
        ## Store the header names for later
        heads <- unlist(line)
    } else {
        ## Throw away all values except the date in the first column
        line <- as.Date(line[[1]][1], "%d/%m/%Y")
        
        ## Is this the first row to be read in?
        if(line >= startDate && startLine == 0) {
            startLine <- lineNum
        }
        
        ## Should we have stopped on the last row?
        if(line > endDate) {
            endLine <- lineNum - 1
        }
    }
}

## Close the file connection
close(con)

## Read only the required data from the file, and assign the column names that were stored previously
data <- read.table(inputFile, header = TRUE, sep = ";", skip = startLine - 2, nrows = endLine - startLine + 1, col.names = heads)

## Convert the date and time columns to a single date-time column
dateTimeCol <- paste(data$Date, data$Time)
## COnvert the new column to an appropriate date-time format
dateTimeCol <- strptime(dateTimeCol, "%d/%m/%Y %H:%M:%S")
## Replace the first 2 columns of the data frame with this new single column
data <- cbind(dateTimeCol, data[3:9])
## Add an appropriate column name for the new column
colnames(data) <- c("DateTime",colnames(data[,2:8]))

## THE SECOND CHUNK OF CODE DOES THE FOLLOWING:
## 1. 
## This code is unique to this R script

## Set the PNG file to contain the histogram
png("plot1.png")

## Create the histogram, set the main title, x-axis title and bar colour
hist(data$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "#FF2500")

## Close the PNG file
dev.off()