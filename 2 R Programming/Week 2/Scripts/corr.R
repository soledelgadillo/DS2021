corr <- function(directory, threshold = 0) {
        index <- list.files(directory, full.names = TRUE)
        data <- c()
        for (i in 1:332) {
                m <- read.csv(index[i])
                m <- data.frame(m[2], m[3])
                cases <- sum(complete.cases(m))
                nmb <- cor(m[1], m[2], use = "pairwise.complete.obs")
                if (cases > threshold) {
                        data <- c(data, nmb)
                }
        }
        print(data)
}
        ## "directory" is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## "threshold" is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!