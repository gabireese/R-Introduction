#Creating a vector that's between 1 and 50 inclusively
my_vector <- 1:50
print(my_vector)

#Creating a 5x10 matrix using my_vector
my_matrix <- matrix(my_vector, nrow = 5, ncol = 10, byrow = TRUE)
print(my_matrix)

#Creating a logical matrix divisible by 3 or a remainder 1 when divided by 6
my_logical <- (my_matrix %% 3 == 0) | (my_matrix %% 6 == 1)
print(my_logical)

#Extracting values from logical matrix
values <- my_matrix[my_logical]
print(values)

#Creating a hailstone function
hailstone <- function(x) {
  cycles <- 0
  while (x != 1) {
    if (x %% 2 == 0) {
      x <- x / 2
    } else {
      x <- x * 3 + 1
    }
    cycles <- cycles + 1
  }
  return(cycles)
}

#Applying extracted values to the hailstone
total_cycles <- sapply(values, hailstone)
print(total_cycles)

# Installing the ISLR library 
install.packages("ISLR")


# Load the ISLR library and the Auto dataset
library(ISLR)
data("Auto")
head(Auto)
#Using is.numeric to filter out the qualitative values
Auto <- Auto[, sapply(Auto, is.numeric)]

#Using for loop to get the range for each column
num_cols <- ncol(Auto)
for (i in 1:num_cols) {
  column_data <- Auto[, i]
  col_range <- max(column_data) - min(column_data)
  print(col_range)
}

#Creating an empty matrix then calculating mean and sd
num_cols <- ncol(Auto)
stats_matrix <- matrix(0, nrow = 2, ncol = num_cols)
colnames(stats_matrix) <- colnames(Auto)
for (col in 1:num_cols) {
  stats_matrix[1, col] <- mean(Auto[[col]])
  stats_matrix[2, col] <- sd(Auto[[col]])
}
print(stats_matrix)

#Finding rows less than mean of mpg and removing them
mean_mpg <- mean(Auto$mpg)
removed <- which(Auto$mpg < mean_mpg)
new_Auto <- Auto[-removed, ]
head(new_Auto)

#Recalculating the mean and sd
num_cols <- ncol(new_Auto)
stats_matrix <- matrix(0, nrow = 2, ncol = num_cols)
colnames(stats_matrix) <- colnames(new_Auto)
for (col in 1:num_cols) {
  stats_matrix[1, col] <- mean(new_Auto[[col]])
  stats_matrix[2, col] <- sd(new_Auto[[col]])
}
print(stats_matrix)
