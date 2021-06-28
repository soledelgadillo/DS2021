getPrimeNumbers <- function(N) {
        ## check that N is admissible
        N.int <- as.integer(N)
        if (!(N.int == N)) stop("N is not an integer")
        if (N < 2) stop("N is not at least 2")
        if (N > 1000000) stop("N is > 1000000")
        
        ## initialize the answer data frame
        primes_up_to_N <- integer(N)
        k <- 0
        
        for (n in 2L : N) {
                if (isItPrimeV1(n)) {
                        k <- k + 1
                        primes_up_to_N[k] <- n
                }
        }
        primes_up_to_N <- primes_up_to_N[1:k]
        
        return(primes_up_to_N)
}