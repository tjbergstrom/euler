# Maximum path sum I
#
# By starting at the top of the triangle below and moving to adjacent 
# numbers on the row below, the maximum total from top to bottom is 23.
#
# 3
# 7 4
# 2 4 6
# 8 5 9 3
#
# That is, 3 + 7 + 4 + 9 = 23.
#
# Find the maximum total from top to bottom of the triangle below:



triangle <- matrix(ncol=15, nrow=15)

triangle[1, 1] 		<- 75
triangle[2, 1:2] 	<- c(95, 64)
triangle[3, 1:3] 	<- c(17, 47, 82)
triangle[4, 1:4] 	<- c(18, 35, 87, 10)
triangle[5, 1:5] 	<- c(20,  4, 82, 47, 65)
triangle[6, 1:6] 	<- c(19,  1, 23, 75,  3, 34)
triangle[7, 1:7] 	<- c(88,  2, 77, 73,  7, 63, 67)
triangle[8, 1:8] 	<- c(99, 65,  4, 28,  6, 16, 70, 92)
triangle[9, 1:9] 	<- c(41, 41, 26, 56, 83, 40, 80, 70, 33)
triangle[10, 1:10] 	<- c(41, 48, 72, 33, 47, 32, 37, 16, 94, 29)
triangle[11, 1:11] 	<- c(53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14)
triangle[12, 1:12] 	<- c(70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57)
triangle[13, 1:13] 	<- c(91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48)
triangle[14, 1:14] 	<- c(63, 66,  4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31)
triangle[15, 1:15] 	<- c( 4, 62, 98, 27, 23,  9, 70, 98, 73, 93, 38, 53, 60,  4, 23)


max_path <- function(triangle) {
    for (row in nrow(triangle) : 2) {
        for (col in 1 : (ncol(triangle)-1)) {
            max_level <- max(triangle[row, col:(col+1)]) + triangle[row-1, col]
            triangle[row-1, col] <- max_level
        }
        triangle[row, ] <- NA
    }
    return(max(triangle, na.rm=T))
}


print(max_path(triangle))



##
