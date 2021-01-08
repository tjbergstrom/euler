# Reciprocal cycles
#
# A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions # with denominators 2 to 10 are given:
#
# 1/2	= 	0.5
# 1/3	= 	0.(3)
# 1/4	= 	0.25
# 1/5	= 	0.2
# 1/6	= 	0.1(6)
# 1/7	= 	0.(142857)
# 1/8	= 	0.125
# 1/9	= 	0.(1)
# 1/10	= 	0.1 
#
# Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has # a 6-digit recurring cycle.
#
# Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.



num_recurring_decimals <- function(num) {
    if (num == 0)
        return(NaN)
    if (num == 1)
        return(0)
    num <- floor(abs(num))
    decimal <- vector()
    remaindr <- vector()
    i <- 1
    r <- 10
    remaindr <- r
    # Long division
    repeat {
        decimal[i] <- floor(r / num)
        r <- 10 * (r %% num)
        # Terminating or repeating
        if (r == 0 | r %in% remaindr)
            break
        remaindr[i + 1] <- r
        i <- i + 1
    }
    # Number of recurring digits
    len_cycle <- ifelse(r != 0, length(remaindr) - which(r == remaindr) + 1, 0)
    return (len_cycle)
}


longest_recurring <- function(max_d) {
    decimals <- sapply(1:max_d, num_recurring_decimals)
    return (which.max(decimals))
}


print(longest_recurring(1000))



##
