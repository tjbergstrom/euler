# Lexicographic permutations
#
# A permutation is an ordered arrangement of objects. For example, 3124 is one possible 
# permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or 
# alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
#
# 012   021   102   120   201   210
#
# What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?


nth_lex_perm <- function(nth) {
    digits <- 0:9
    remain <- nth - 1
    n <- length(digits)
    permutation <- vector(length=10)
    for (i in 1 : n) {
        j <- floor(remain / factorial(n - i))
        permutation[i] <- digits[j + 1]
        remain <- remain %% factorial(n - i)
        digits <- digits[-(j + 1)]
    }
    return (as.numeric(paste(permutation, collapse = "")))
}

millionth_lex_permu <- nth_lex_perm(1E6)
print(millionth_lex_permu)



##
