# Quadratic primes
#
# https://projecteuler.net/problem=27
#
# Find the product of the coefficients a and b for the quadratic expression that produces the maximum number of primes for consecutive values of n starting with n=0



source("largest_prime_factor.r", verbose = FALSE)


is_prime <- function(n) {
    if (n <= 1)
        return (F)
    if (n == 2)
        return (T)
	all_primes <- sieve(ceiling(sqrt(n)))
	divisible <- prod(n %% all_primes != 0)
	return (divisible == 1)
}


max_quad_coefs <- function(nvals) {
    seq_a <- seq((0-nvals+1), nvals+1, 2) # a's are odd
    seq_b <- (sieve(nvals)) # b's are prime
    max_n <- 0
    for (a in seq_a) {
        for (b in seq_b) {
            n <- 0
            # Sequences of primes for a and b
            while (is_prime(n^2 + a * n + b)) {
                n <- n + 1
            }
            if (n > max_n) {
                max_n <- n
                max_a <- a
                max_b <- b
            }
        }
    }
	return (list(max_a, max_b))
}


coefs <- max_quad_coefs(1000)
a = coefs[[1]]
b = coefs[[2]]
cat(sprintf("%d * %d = %d \n", a, b, a*b))



##
