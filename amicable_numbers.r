# Amicable numbers
#
# Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
# If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
#
# For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110;
# therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
#
# Evaluate the sum of all the amicable numbers under 10000.



all_divisors <- function(x) {
    divisors <- vector()
    d <- 1
    for (i in 1 : floor(sqrt(x))) {
        if (x %% i == 0) {
            divisors[d] <- i
            if (i != x / i) {
                d <- d + 1
                divisors[d] <- x / i
            }
            d <- d + 1
        }
    }
    return(divisors)
}


amicable_nums <- function(x) {
    amicables <- c()
    n <- 2
    while (n <= x) {
        divs_sum <- sum(all_divisors(n)) - n
        if (n == sum(all_divisors(divs_sum)) - divs_sum & n != divs_sum) {
            amicables <- c(amicables, n, divs_sum)
            n <- divs_sum
        }
        n <- n + 1
    }
    return (amicables)
}


amicables <- amicable_nums(10000)
print(amicables)
print(sum(amicables))



##
