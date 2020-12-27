# Summation of primes
#
# The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
#
# Find the sum of all the primes below two million.
#
# Bonus: Show that every integer > 2 can be represented as the sum of at least two primes



source("largest_prime_factor.r", echo=F)
primes <- sieve(2e6)
sum_primes <- sum(primes)
print(sum_primes)



goldbach <- function(n) {
    if (n %% 2 != 0) return(0)
    if (n == 2) return(0)
    primes <- rev(sieve(n))
    k <- length(primes)
    g <- 0
    for (i in 1 : k) {
        for (j in i : k) {
            if( (primes[i] + primes[j]) == n ) {
                g <- as.numeric(g + 1)
            }
        }
    }
    return(g)
}

library(ggplot2)
n <- seq(from = 2, by = 2, length.out = 5000)
df <- data.frame(n, g = sapply(n, goldbach))

ggplot(df, aes(n, g)) +
    geom_point(col = "#00b2b3", size = .2) +
    theme_bw(base_size = 10) +
    labs(title = "Number of ways to write an even number as the sum of two primes",
    subtitle = "Ex: 100 = 3+97 = 11+89 = 17+83 = 29+71 = 41+59 = 47+53",
    x = "Even Numbers", y = "Number of prime sums")

ggsave("summation_of_primes.png", width = 6, height = 4)



##
