##################
## Nathen Byford
## July 15, 2021
##################

library(tidyverse)

## Create a testing data set that is exactly Benfords Law
set.seed(123)

# 301 observations will start with 1

ones <- runif(301, 100, 199)

# 176 observations will start with 2

twos <- runif(176, 200, 299)

# 124 will start with 3

threes <- runif(124, 300, 399)

# 96 with 4

fours <- runif(96, 400, 499)

# 79 with 5

fives <- runif(79, 500, 599)

# 66 with 6

sixes <- runif(66, 600, 699)

# 57 with 7

sevens <- runif(57, 700, 799)

# 51 with 8

eights <- runif(51, 800, 899)

# 45 with 9

nines <- runif(45, 900, 999)

# Combine them all into one vector

Benford_data <- c(ones, twos, threes, fours, fives, sixes, sevens, eights, nines)


## Test sum-invariance property

sum = c()
for (i in 1:9) {
  sum[i] = sum(Benford_data[which(getFirstDigit(Benford_data) == i)])
}
sum
qplot(x = 1:9, y = sum, geom = "col")
