# Factorial digit sum
#
# n! means n × (n − 1) × ... × 3 × 2 × 1
#
# For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
# and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
#
# Find the sum of the digits in the number 100!




# arbitrary-precision arithmetic


large_addition <- function(a, b) {
    if (nchar(a) < nchar(b))
        a <- paste0(paste(rep(0, nchar(b) - nchar(a)), collapse = ""), a)
    if (nchar(a)>nchar(b)) 
        b <- paste0(paste(rep(0, nchar(a) - nchar(b)), collapse = ""), b)
    solution <- vector()
    remainder <- 0
    for (i in nchar(b) : 1) {
        p <- as.numeric(substr(a, i, i))
        q <- as.numeric(substr(b, i, i))
        r <- p + q + remainder
        if (r >= 10 & i != 1) {
            solution <- c(solution, r %% 10)
            remainder <- (r - (r %% 10)) / 10
        } else {
            solution <- c(solution, r)
            remainder <- 0
        }
    }
    return(paste(rev(solution), collapse = ""))
}


large_factorial <- function(factorial) {
    factorial <- floor(factorial)
    digits <- 1
    if (factorial > 1) {
        for (i in 2 : factorial) {
            digits <- Reduce(large_addition, rep(digits, i))
        }
    }
    return (digits)

}


sum_digits <- function(digits) {
    return (sum(as.numeric(unlist(strsplit(as.character(digits), "")))))
}


digits <- large_factorial(100)
print(sum_digits(digits))



##
