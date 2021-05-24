colvecToNumber <- function(colvec) {
        
        ## convert a character vector of Excel column names to the integer vector
        ## of corresponding column numbers each entry of colvec should be a 
        ## column name between a and dz
        
        ## note a single character string is a character vector of length 1
        
        ## check the input is a non_empty character vector
        if (length(colvec) < 1) {stop("colvec input to colvecToNumber is empty")}
        if (!is.character(colvec)) {stop("colvec input to colvecToNumber is not a character")}
        
        ## create the integer vector to be output
        n <- length(colvec)
        colnumbers <- integer(n)
        colvec <- tolower(colvec)
        
        ## create the list of column names to compare
        colnames <- c(letters)
        for (i in 1:4) {
                ll <- paste(letters[i], letters, sep = "")
                names(ll) <- c("letters")
                colnames <- c(colnames, ll)
        }
        names(colnames) <- NULL
        
        colnumbers <- c(1:length(colvec))
        
        for (i in 1:n) {
                colnumbers[i] <- which(colvec[i] == colnames)
        }
        
        print(colnumbers)
}