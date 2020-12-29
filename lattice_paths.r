

# Lattice paths
#
# Starting in the top left corner of a 2×2 grid, and only being able to move to the
# right and down, there are exactly 6 routes to the bottom right corner.
#
# How many such routes are there through a 20×20 grid?


N <- 20
lattice = matrix(ncol = N + 1, nrow = N + 1)

# Outisde edges
lattice[N + 1, -(N + 1)] <- 1
lattice[-(N + 1), N + 1] <- 1

# Paths from destination to source, O(N^2)
for (i in N:1) {
    for (j in N:1) {
        lattice[i, j] <- lattice[i + 1, j] + lattice[i, j + 1]
    }
}

print(lattice[1, 1])



##
