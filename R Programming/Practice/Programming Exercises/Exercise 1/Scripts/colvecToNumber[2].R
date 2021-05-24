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
        ## use colnameToNumber to get each entry of colnumbers
        for (i in 1:n) {
                colname <- colvec[i]
                colnumbers[i] <- colnameToNumber(colvec[i])
        }
        
        print(colnumbers)
}