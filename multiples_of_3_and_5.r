# Multiples of 3 and 5
#
# If we list all the natural numbers below 10 that are multiples of 3 or 5, 
# we get 3, 5, 6 and 9. The sum of these multiples is 23.
#
# Find the sum of all the multiples of 3 or 5 below 1000.


sums <- function(n, k) {
    p <- floor(k / n) * n
    return (0.5 * p * ((p / n) + 1))
}

multiples <- function(n) {
	return ( (sums(3, n) + sums(5, n) - sums(3*5, n)) )

}

print(multiples(1000))



##
