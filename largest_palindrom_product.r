# Largest palindrome product
#
# A palindromic number reads the same both ways.
# The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
#
# Find the largest palindrome made from the product of two 3-digit numbers.


reverse <- function(n) {
	r <- 0
	while (n > 0) {
		r <- 10 * r + (n %% 10)
		n <- floor(n / 10)
	}
	return(r)
}

largest_pal <- function() {
	for (i in 999:900) {
		for (j in 990:900) {
			p <- i * j
			if (p == reverse(p))
				break
		}
		if (p == reverse(p)) {
			break
		}
	}
	output <- list(i, j)
	return(output)
}

factors <- largest_pal()
a = factors[[1]]
b = factors[[2]]
cat(sprintf("%d * %d = %d \n", a, b, a*b))



##
