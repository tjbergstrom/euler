# Largest product in a grid
#
# In the 20×20 grid below, four numbers along a diagonal line have been marked in red.
#
# The product of these numbers is 26 × 63 × 78 × 14 = 1788696.
#
# What is the greatest product of four adjacent numbers in the same
# direction (up, down, left, right, or diagonally) in the 20×20 grid?



grid <- as.matrix(read.delim("largest_product_ina_grid.txt", header = FALSE, sep = " "))

# Vector arithmetic:
vertical <- grid[1:17,     ] * grid[2:18,     ] * grid[3:19,     ] * grid[4:20,     ]
horizont <- grid[    , 1:17] * grid[    , 2:18] * grid[    , 3:19] * grid[    , 4:20]
diagnl_1 <- grid[1:17, 1:17] * grid[2:18, 2:18] * grid[3:19, 3:19] * grid[4:20, 4:20]
diagnl_2 <- grid[4:20, 1:17] * grid[3:19, 2:18] * grid[2:18, 3:19] * grid[1:17, 4:20]

max_product <- max(vertical, horizont, diagnl_1, diagnl_2)
print(max_product)



##
