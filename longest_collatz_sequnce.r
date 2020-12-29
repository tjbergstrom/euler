# Longest Collatz sequence
#
# The following iterative sequence is defined for the set of positive integers:
#
# n → n/2 (n is even)
# n → 3n + 1 (n is odd)
#
# Using the rule above and starting with 13, we generate the following sequence:
# 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
#
# It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
# Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
#
# Which starting number, under one million, produces the longest chain?
#
# NOTE: Once the chain starts the terms are allowed to go above one million.


len_collatz <- vector(len = 1E6)
len_collatz[1] <- 0
for (i in 2 : 1E6) {
    x <- i
    n <- 0
    while (x != 1 & x >= i) {
        if (x %% 2 == 0) {
            x <- x / 2
            n <- n + 1
        }
        else {
            x <- (3 * x + 1) / 2
            n <- n + 2
        }
    }
    n <- n + len_collatz[x]
    len_collatz[i] <- n
}

longest_chain <- which.max(len_collatz)
print(longest_chain)



library(ggplot2)
collatz <- data.frame(n = 1:100000, steps = len_collatz[1:100000])
ggplot(collatz, aes(n, steps)) +
    geom_point(size = .3, col = "#00b2b3") +
    theme_bw(base_size = 10) +
    labs(title = "Collatz Sequence", subtitle = "Steps to reach 1")
ggsave("longest_collatz_sequnce.png", width=6, height=4)



##
