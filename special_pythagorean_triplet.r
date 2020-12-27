# Special Pythagorean triplet
#
# A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
# a^2 + b^2 = c^2
#
# For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2
#
# There exists exactly one Pythagorean triplet for which a + b + c = 1000.
# Find the product abc.



# O(1) solution
# m > n > 0
# a = m^2 - n^2
# b = 2mn
# c = m^2 + n^2

sum_of_abc <- 1000
x <- sum_of_abc / 2
min <- floor(sqrt(x/2))
max <- ceiling(sqrt(x))
m <- min:max
m <- m[x %% m == 0]
n <- ((x/m) - m)
a <- m * n * 2
b <- m^2 - n^2
c <- m^2 + n^2
product_of_abc <- a * b * c

cat(sprintf("%d^2 + %d^2 = %d^2 \n", a, b, c))
cat(sprintf("%d + %d + %d = %d \n", a, b, c, a+b+c))
cat(sprintf("%d * %d * %d = %d \n", a, b, c, product_of_abc))



##
