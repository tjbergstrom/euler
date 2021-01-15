# Digit cancelling fractions
#
# The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify 
# it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
#
# We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
#
# There are exactly four non-trivial examples of this type of fraction, less than one in value, and 
# containing two digits in the numerator and denominator.
#
# If the product of these four fractions is given in its lowest common terms, find the value of the denominator.


is_digit_cancelling <- function(a, b, digs) {
    if (length(digs) != 2)
        return (F)
    return ( a / b == digs[1] / digs[2] )
}

is_trivial <- function(a, b) {
    return (a %% 10 == 0 | b && 10 == 0 | a %% 11 == 0 | b %% 11 == 0)
}

to_digit <- function(n) {
    return (as.numeric(unlist(strsplit(as.character(n), ""))))
}

nums <- vector()
dens <- vector()
for (a in 11 : 99) {
    for (b in (a + 1) : 99) {
        if (is_trivial(a, b))
            next
        digs <- c(to_digit(a), to_digit(b))
        digs <- digs[which(digs != digs[duplicated(digs)])]
        if (is_digit_cancelling(a, b, digs)) {
            nums <- c(nums, a)
            dens <- c(dens, b)
        }
    }
}

print(paste(nums, dens, sep="/"))
product_denominator <- prod(dens) / prod(nums)
print(product_denominator)

# 16/64 * 19/95 * 26/65 * 49/98   ==   1/4 * 1/5 * 2/5 * 4/8   ==   1/100
# 1 / 4   1 / 5   2 / 5   4 / 8



##
