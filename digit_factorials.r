# Digit factorials
#
# 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
#
# Find the sum of all numbers which are equal to the sum of the factorial of their digits.
#
# Note: As 1! = 1 and 2! = 2 are not sums they are not included.


# The trick to make it pretty much O(N) is that you pre-calculate all factorials up to 9
# Because every number is made up of digits 0-9, so you just add the pre-calculated factorials


int_to_str <- function(n) {
	return (as.numeric(unlist(strsplit(as.character(n), ""))))
}


fact <- function(n) {
	f <- 1
	for (i in 1 : n) {
		f <- f * i
	}
	return (f)
}


dig_factorials <- function() {
	curious_nums <- vector()
	factorials <- vector()
	for (i in 1 : 10) {
		factorials <- c(factorials, fact(i))
	}
	for (num in 3 : 222222) {
		fact_sum <- 0
		for (d in int_to_str(num)) {
			if (d == 0) {
				fact_sum <- fact_sum + 1
				next
			}
			fact_sum <- fact_sum + factorials[d]
		}
		if (fact_sum == num) {
			curious_nums <- c(curious_nums, num)
		}
	}
	return (curious_nums)
}


curious_nums <- dig_factorials()
print(curious_nums)
print(sum(curious_nums))



##
