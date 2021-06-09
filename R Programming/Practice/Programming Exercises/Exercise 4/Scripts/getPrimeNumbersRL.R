getPrimeNumbersRL <- function(N) {
        ## check that N is admissible
        N.int <- as.integer(N)
        if (!(N.int == N)) stop("N is not an integer")
        if (N < 2) stop("N is not at least 2")
        ## if N is "too large" (>1000000) check with the user to see if the user
        ## wants to proceed
        if (N > 1000000) {
                cat("N = ", N, "/n") ## print N and also include going to a new output line
                yes.or.no <- readline("this N is too large, do you want to continue, type yes or no: ")
                if (yes.or.no != "yes") return("N was too large so exited getPrimeNumbers")
        }
        
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