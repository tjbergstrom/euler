# Sum square difference
#
# The sum of the squares of the first ten natural numbers is
#
# 1^2 + ... 10^2 = 385
#
# The square of the sum of the first ten natural numbers is
#
# (1 + ... + 10)^2 = 3025
#
# Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 - 385 = 2640
#
# Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum



# R solution, O(n) time

diff <- sum(1:100)^2 - sum( (1:100)^2 )


# Gauss solution, O(1) time
# sum i from 0 to n = [n(n+1)]/2
# sum i^2 from 0 to n = [n(n+1)(2n+1)]/6

n = 100
square_of_sums <- ((n * (n+1)) / 2)^2
sum_of_squares <- (n * (n+1) * (2*n+1)) / 6
difference <- square_of_sums - sum_of_squares


print(difference)
print(diff)



##
