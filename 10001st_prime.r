# 10001st prime
#
# By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
#
# What is the 10,001st prime number?



source("largest_prime_factor.r")
is_prime <- function(n) {
	rt_n = sqrt(n)
	all_primes <- sieve(ceiling(rt_n))
	divisible <- prod(n %% all_primes != 0)
	return(divisible == 1)
}


number <- 1
prime_cnt <- 1
nth_prime <- 2
while (prime_cnt < 10001) {
	number <- number + 2
	if (is_prime(number)) {
		nth_prime <- number
		prime_cnt <- prime_cnt + 1
	}

}


print(nth_prime)



library(ggplot2)
library(dplyr)

primes <- sieve(nth_prime)
p <- length(primes)
gaps <- tibble(Gap = primes[2:p] - primes[1:(p - 1)], PrimeGap = Gap %% 6 == 0) %>% count(Gap, PrimeGap)

ggplot(gaps, aes(factor(Gap), n, fill = PrimeGap)) +
    geom_col() +
    scale_fill_manual(values = c( "#00b2b3", "#198e54"), name="PrimeGaps") +
    theme_minimal(base_size = 10) + 
    labs(title = "Frequency o  f prime gaps for the first 10,000 primes",
        x = "Prime Gap",
        y = "Frequency")
        guide_legend(title="PrimeGaps")

ggsave("10001st_prime.png", width=6, height=4)



##
