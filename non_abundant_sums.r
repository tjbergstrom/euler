# Non-abundant sums
#
# A perfect number is a number for which the sum of its proper divisors is exactly equal to the 
# number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, 
# which means that 28 is a perfect number.
#
# A number n is called deficient if the sum of its proper divisors is less than n and it is 
# called abundant if this sum exceeds n.
#
# As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be 
# written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown 
# that all integers greater than 28123 can be written as the sum of two abundant numbers. 
# However, this upper limit cannot be reduced any further by analysis even though it is known 
# that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.
#
# Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.


source("amicable_numbers.r", verbose=F)
all_abundant_nums <- function(n){
    abundants <- vector()
    a <- 1
    for (i in 1 : n) {
        sigma <- sum(all_divisors(i))
        if (sigma > 2 * i) {
            abundants[a] <- i
            a <- a + 1
        }
    }
    return (abundants)
}


non_abundants <- function() {
    abundants <- all_abundant_nums(28123)
    int_seq <- 1 : 20161
    for (i in 1 : length(abundants)) {
        for (j in i : length(abundants)) {
            if (abundants[i] + abundants[j] <= 20161)
                int_seq[abundants[i] + abundants[j]] <- NA
        }
    }
    all_non_abundants <- (int_seq[!is.na(int_seq)])
    return (all_non_abundants)
}


non_abunds = non_abundants()
print(sum(non_abunds))
