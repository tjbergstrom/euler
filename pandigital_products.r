# Pandigital products
#
# We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly 
# once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
#
# The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, 
# and product is 1 through 9 pandigital.
#
# Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
#
# HINT Some products can be obtained in more than one way so be sure to only include it once in your sum.



is_pandigital <- function(n) {
    return (length(n) == 9 & sum(duplicated(n)) == 0 & sum(n == 0) == 0)
}


pandigital_prods <- function() {
    products <- vector()
    i <- 1
    for (a in 2 : 100) {
        if (a < 10)
            start <- 1234
        else
            start <- 123
        for (b in start: round(10000 / a)) {
            digits <- as.numeric(unlist(strsplit(paste0(a, b, a * b), "")))
            if (is_pandigital(digits)) {
                products[i] <- a * b
                i <- i + 1
                #print(paste(a, "*", b, "=", a * b))
                cat(sprintf("%d * %d = %d \n", a, b, a * b))
            }
        }
    }
    return (products)
}


products <- pandigital_prods()
cat(sprintf("%d \n", sum(unique(products))))



##
