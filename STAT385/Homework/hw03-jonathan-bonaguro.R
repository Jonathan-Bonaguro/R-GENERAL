# First Name: Jonathan
# Last Name: Bonaguro
# NetID: jcb3
# Homework 3

###

## Exercise 1: Vectorized Code

## (a)

specific_sum_vec <- function(m, pos) {
  sum(colSums(m[ , pos]))
}

m <- matrix(data = c(114:133), nrow = 4, byrow = TRUE)
pos <- c(1, 4, 2)

specific_sum_vec(m = m, pos = pos)

## (b)

long_name_vec <- function(names) {
  names[nchar(names) >= 8]
}

my_names <- c("alexander", "david", "sebastian", "johnathan", "christopher", "ha",
              "washington", "lincoln", "maximo", "mason", "luca", "anthony", "kevin")

long_name_vec(names = my_names)

## (c)

roulette_vec <- function(many_bets) {
  win_lose_random <- sample(x = c(TRUE, FALSE), size = length(many_bets), replace = TRUE)
  many_bets[win_lose_random == FALSE] <- 0
  many_bets[many_bets %in% c("low", "high")] <- 10
  many_bets[many_bets %in% c("red", "black")] <- 20
  many_bets[many_bets %in% c("odd", "even")] <- 15
  many_bets[many_bets %in% c("first", "second", "third")] <- 50
  as.integer(many_bets)
}

long_vec <- rep(c("red", "black", "low", "high", "second", "first", "third",
                  "odd", "even"), 10000)

system.time(roulette_vec(many_bets = long_vec))

###---------------------------------------------------------------------------###

## Exercise 2

## (a) 

hist(x = iris$Sepal.Length, main = "Histogram of Sepal Length", xlab = "Sepal Length (cm.)",
     breaks = 15, border = "dodgerblue", probability = TRUE,
     ylim = c(0, 0.65), xlim = c(4, 8.2))
box()
grid()
abline(v = mean(iris$Sepal.Length), col="red", lwd = 2)
text(x = 7, y = 0.6, labels = paste("Mean of Sepal Length =", round(mean(iris$Sepal.Length), digits = 3)), col = "red")

## (b)

hist(x = faithful$eruptions, main = "Histogram of Old Faithful Geyser Eruption Time", 
     xlab = "Eruption Time (mins.)", probability = TRUE, border = "dodgerblue", breaks = 15,
     ylim = c(0.0, 0.8), xlim = c(1, 6))
box()
grid()
lines(density(faithful$eruptions), col = "orange", lwd = 2)   

## (c)

par(mfrow = c(1, 2))
hist(x = faithful$waiting, main = "Histogram of Waiting Time", xlab = "Waiting Time (mins.)",
     probability = TRUE, border = "dodgerblue", xlim = c(40, 100), ylim = c(0.00, 0.04))
box()
grid()
plot(eruptions ~ waiting, data = faithful, pch = 19, col = "dodgerblue", xlab = "Waiting Time (mins.)",
     ylab = "Eruption Time (mins.)", main = "Waiting Time vs. Eruption Time", xlim = c(40, 100),
     ylim = c(1.5, 5.0))
grid()

###---------------------------------------------------------------------------###