colnameToNumber <- function(colname) {
        
        ## convert an Excel column name to the corresponding column number;
        ## colname should be a lower case in the form of a character string, 
        ## for example "a" or "r" "between" a and z (including a and z)
        colname <- tolower(colname)
        colnames <- c(letters)
        for (i in 1:4) {
                ll <- paste(letters[i], letters, sep = "")
                names(ll) <- c("letters")
                colnames <- c(colnames, ll)
        }
        ## index <- c(1:nrow(letters_df))
        ## ref <- data.frame(letters_df, index)
        ## colnumber <- ref[which(ref[1] == colname), 2]
        names(colnames) <- NULL
        colnumber <- which(colnames == colname)
        if (length(colnumber) != 1) {stop("no match for colname")}
        return(colnumber)
}