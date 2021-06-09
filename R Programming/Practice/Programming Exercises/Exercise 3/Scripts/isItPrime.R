isItPrime <- function(n) {
        ## determine whether the positive integer n is prime
        ## using the mod function, return TRUE or FALSE accordingly
        ## check that the function argument is "admissible"
        ## test that n is a positive integer (or a real number that equals a 
        ## positive integer)
        ## if n was a real number n.int will be n truncated
        n.int <- as.integer(n)
        if (!(n.int == n)) {stop("n is not and integer")}
        if (n.int < 1) {stop ("n is not positive")}
        ## stop if n is "too large" to avoid a very long calculation
        if (n > 1000000) {stop("n > a million")}
        ## code to test if n is prime using R's mod function %%
        
        ## special cases
        if (n == 1) return(FALSE)
        if (n == 2) return(TRUE)
        ## if got to here, n is at least 3
        ## test if an integer between 2 and (n -1) evenly divides n
        lastq <- as.integer(sqrt(n)) + 1L
        ## the L in 1L "tells" R to treat 1 as an integer value rather than a
        ## eal (numeric) value. This could also have equivalently been done by 
        ## lasq <- as.integer(sqrt(n) + 1)
        for (q in 2:lastq) {
                if ((n %% q) == 0) return(FALSE)
        }
        ## if got to here, n is prime
        return (TRUE)
}