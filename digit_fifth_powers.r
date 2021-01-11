# Digit fifth powers
#
# Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
#
#    1634 = 14 + 64 + 34 + 44
#    8208 = 84 + 24 + 04 + 84
#    9474 = 94 + 44 + 74 + 44
#
# As 1 = 14 is not a sum it is not included.
#
# The sum of these numbers is 1634 + 8208 + 9474 = 19316.
#
# Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.



equation <- function(i, n, exp) {
    eval = sum(((1/10^n) * (i %% 10^(n + 1) - i %% 10^n))^exp)
    return (eval)
}


digit_sum <- function(exp){
    uppr_bound <- round(log10(9^exp)) * 9^exp
    digit_sum <- 0
    for (i in 2 : uppr_bound) {
        n <- 0 : log10(i)
        power_sum <- equation(i, n, exp)
        if (power_sum == i) {
            #print(i)
            digit_sum <- digit_sum + i
        }
    }
    return (digit_sum)
}


print(digit_sum(5))



##
