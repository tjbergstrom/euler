# Smallest multiple
#
# 2520 is the smallest number that can be divided by
# each of the numbers from 1 to 10 without any remainder.
#
# What is the smallest positive number that is
# evenly divisible by all of the numbers from 1 to 20?


# One method

sieve <- function(n) {
    if (n == 1) return(NULL)
    if (n == 2) return(n)
    list <- 2:n
    i <- 1
    p <- 2
    while (p^2 <= n) {
        list <- list[list == p | list %% p!= 0]
        i <- i + 1
        p <- list[i]
    }
    return(list)
}

smallest <- function(n) {
	p <- sieve(n)
	mult = 1
	i <- 1
	while (i <= length(p)) {
		mult <- mult * ( p[[i]] ^ floor( log10(n) / log10(p[[i]]) ) )
		i <- i + 1
	}

	return (mult)
}

print(smallest(20))



# Another method

gcd = function (x, y) ifelse(x == 0, y, gcd(y %% x, x))
lcm = function (x, y) x * y / gcd(x, y)
Reduce(lcm, 1:20)
print(Reduce(lcm, 1:20, accumulate = TRUE))



##
