weightmedian <- function(directory, day) { ## content of the function
        ## create a list of files
        files <- list.files(directory, full.names = TRUE) 
        ## create an empty data frame
        dat <- data.frame() 
        for (i in 1:length(files)) {
                dat <- rbind(dat, read.csv(files[i]))
        }
        ## subset the rows that match the "day" argument
        dat_subset <- dat[which(dat[, "Day"] == day), ]
        ## identifies the median weight while stripping out the NAs
        median(dat_subset[, "Weight"], na.rm = TRUE)
}
