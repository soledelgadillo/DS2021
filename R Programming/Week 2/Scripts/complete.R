complete <- function(directory, id = 1 : 332) {
        index <- list.files(directory, full.names = TRUE)[id]
        shine <- data.frame()
        for (i in 1 : length(index)) {
                boom <- read.csv(index[i])
                boom <- complete.cases(boom)
                smith <- sum(boom)
                shine <- rbind(shine, smith)
        }
        info <- cbind(id, shine)
        names(info) <- c("id", "nobs")
        print(info)
}
        ## "directory" is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## "id" is an integer vector indicating the monitor ID numbers to be used
        
        ##Return a data frame of the form: 
        ## id   nobs
        ## 1    117
        ## 2    1041
        ## ...
        ## where "id" is the monitor ID number and nobs is the number of 
        ## complete cases
