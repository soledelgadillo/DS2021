isItPrimeV2 <- function(n) {
        n.int <- as.integer(n)
        if (!(n.int == n)) stop("n is not an integer")
        if (n < 1) stop("n is not positive")
        if (n > 1000000) stop("n is > 1000000")
        
        if (n.int == 1) return (c(is_n_prime = 0, n = 1, firstq = 1))
        if (n.int == 2) return (c(is_n_prime = 1, n = 2, firstq = 2))
        
        lastq <- as.integer(sqrt(n)) + 1L
        ## the L in 1L "tells" R to treat 1 as an integer
        ## this could also have equivalently done by
        ## lastq <- as.integer(sqrt(n) + 1)
        
        for(q in 2: sqrt(lastq)) {
                if ((n %% q) == 0) return(c(is_n_prime = 0, n = n.int, firstq = q))
        }
                return(c(is_n_prime = 1, n = n.int, firstq = n))
}        