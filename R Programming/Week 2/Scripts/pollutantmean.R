pollutantmean <- function(directory, pollutant, id = 1:332) {
        index <- list.files(directory, full.names = TRUE)[id]
        trivium <- data.frame()
        for (i in 1:length(index)) {
                m <- read.csv(index[i])
                trivium <- rbind(trivium, m)
        }
        trivium <- trivium[pollutant]
        zach <- trivium[!is.na(trivium)]
        wylde <- mean(zach)
        print(wylde)
}
        
        ## "directory is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## "pollutant" is a character vector of length 1 indicating the 
        ## name of the pollutant for which we will calculate the mean;
        ## either "sulfate" or "nitrate"
        
        ## "id" is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list in the
        ## id vector (ignoring "NA" values) mean(tablas, na.rm = TRUE)
        ## NOTE: Do not round the result!
        