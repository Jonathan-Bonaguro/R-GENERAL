#### Excercise 1a

specific_sum_loop <- function(m, pos) {
  my_sum <- 0
  for (i in 1:nrow(m)) {
    for (j in pos) {
      my_sum <- my_sum + m[i, j]
    }
  }
  my_sum
}

specific_sum_vec <- function(m, pos) {
  sum(colSums(m[ , pos]))
}

m <- matrix(data = c(114:133), nrow = 4, byrow = TRUE)
pos <- c(1, 4, 2)

specific_sum_loop(m = m, pos = pos)

specific_sum_vec(m = m, pos = pos)

#### Excercise 1b

long_names_loop <- function(names) {
  output <- c()
  for (i in 1:length(names)) {
    name <- names[i]
    if (nchar(name) >= 8) {
      output <- c(output, name)
    }
  }
  output
}

my_names <- c("alexander", "david", "sebastian", "johnathan", "christopher", "ha",
              "washington", "lincoln", "maximo", "mason", "luca", "anthony", "kevin")

long_names_loop(names = my_names)

long_name_vec <- function(names) {
  names[nchar(names) >= 8]
}

long_name_vec(names = my_names)

#### Excercise 1c

roulette_loop <- function(many_bets) {
  
  # randomly decide whether you lose or win on each bet
  win_lose_random <- sample(x = c(TRUE, FALSE), size = length(many_bets),
                            replace = TRUE)
  
  total_prize <- c()
  
  # for each bet
  for (i in 1:length(many_bets)) {
    bet <- many_bets[i]
    prize <- 0
    
    # if we win, then check how much the prize is
    if (win_lose_random[i]) {
      if (bet == 'low') {
        prize <- 10
      } else if (bet == 'high'){
        prize <- 10
      } else if (bet == 'red') {
        prize <- 20
      } else if (bet == "black") {
        prize <- 20
      } else if (bet == "odd") {
        prize <- 15
      } else if (bet == "even") {
        prize <- 15
      } else if (bet == "first") {
        prize <- 50
      } else if (bet == "second") {
        prize <- 50
      } else if (bet == "third") {
        prize <- 50
      }
    }
    # add prize from this bet to the total prize
    total_prize <- c(total_prize, prize)
  }
  
  total_prize
}

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
system.time(roulette_loop(many_bets = long_vec))

long_vec <- rep(c("red", "black", "low", "high", "second", "first", "third",
                  "odd", "even"), 10000)
system.time(roulette_vec(many_bets = long_vec))

#### Excercise 2a
hist(x = iris$Sepal.Length, main = "Histogram of Sepal Length", xlab = "Sepal Length (cm.)",
     breaks = 15, border = "dodgerblue", probability = TRUE,
     ylim = c(0, 0.65), xlim = c(4, 8.2))
box()
grid()
abline(v = mean(iris$Sepal.Length), col="red", lwd = 2)
text(x = 7, y = 0.6, labels = paste("Mean of Sepal Length =", round(mean(iris$Sepal.Length), digits = 3)), col = "red")

hist(x = faithful$eruptions, main = "Histogram of Old Faithful Geyser Eruption Time", 
     xlab = "Eruption Time (mins.)", probability = TRUE, border = "dodgerblue", breaks = 15,
     ylim = c(0.0, 0.8), xlim = c(1, 6))
box()
grid()
lines(density(faithful$eruptions), col = "orange", lwd = 2)   

par(mfrow = c(1, 2))
hist(x = faithful$waiting, main = "Histogram of Waiting Time", xlab = "Waiting Time (mins.)",
     probability = TRUE, border = "dodgerblue", xlim = c(40, 100), ylim = c(0.00, 0.04))
box()
grid()
plot(eruptions ~ waiting, data = faithful, pch = 19, col = "dodgerblue", xlab = "Waiting Time (mins.)",
     ylab = "Eruption Time (mins.)", main = "Waiting Time vs. Eruption Time", xlim = c(40, 100),
     ylim = c(1.5, 5.0))
grid()

