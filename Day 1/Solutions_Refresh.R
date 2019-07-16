# Exercise 01

# 1. Create a vector of length 15, elements 1-7 increasing from 1, and 8-15 decreasing from 25

vec <- c(1:7, 25:18) 

# How many ways can you find of doing it? Try using c()

c(seq(1, 7), seq(25, 18, by=-1))

c(1, 2, 3, 4, 5, 6, 7, 25, 24, 23, 22, 21, 20, 19, 18)

c(1:7, rev(18:25))

# 2. Substitute the 3rd and 7th element with NA

vec[c(3, 7)] <- NA

# 3. Calculate mean of the vector

mean(vec, na.rm=TRUE)

# 4. Change all NA to 0

vec[is.na(vec)] <- 0

mean(vec)



# Exercise 02

# 1. Create a matrix with 10 rows and 15 columns, containing random numbers from a normal distribution

mat <- matrix(rnorm(150), ncol=15, nrow=10)

# 2. Subset the first 5 rows and columns of this matrix

sub <- mat[1:5,1:5]

# 3. Sum the matrix by row

apply(sub, 1, sum)

rowSums(sub)



# Exercise 03

# 1. Create one numeric vector of random numbers and one character vector
# of the same length

val <- runif(20, 1, 8)
# ?runif

val <- rnorm(20)

cha <- letters[1:20]

# 2. Create a data frame with them

df <- data.frame(val=val, cha=cha)

# 3. Add a column with 0 or 1  for values in numeric column > or <= 5

cls <- df$val

cls[cls <= 5] <- 1

cls[cls > 5] <- 0

df$cls <- cls



# 4. Extract rows with value of new column equal to 1

df[df$cls == 1,]
df
