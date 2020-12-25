# Largest prime factor
#
# The prime factors of 13195 are 5, 7, 13 and 29.
#
# What is the largest prime factor of the number 600851475143 ?


sieve <- function(n) {
    if (n == 1) return(NULL)
    if (n == 2) return(n)
    list <- 2:n # List of ints from 2 to n
    i <- 1
    p <- 2
    while (p^2 <= n) {
        # Remove all multiples of p from the list
        list <- list[list == p | list %% p!= 0]
        # Increment p to the next remaining int in the list
        i <- i + 1
        p <- list[i]
    }
    # The remaining ints in the list are primes
    return(list)
}

prime.factors <- function (n) {
    factors <- c() # A list of factors
    primes <- sieve(floor(sqrt(n)))
    # Prime divisors
    d <- which(n %% primes == 0)
    if (length(d) == 0)
        return(n)
    # Divide n by the primes down to 1
    for (q in primes[d]) {
        while (n %% q == 0) {
            factors <- c(factors, q)
            n <- n/q } }
            if (n > 1)
                factors <- c(factors, n)
    return(factors)
}

print(max(prime.factors(600851475143)))



##
