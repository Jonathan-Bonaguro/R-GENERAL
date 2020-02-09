# First Name: Jonathan
# Last Name: Bonaguro
# NetID: jcb3
# Homework 2

###

## Exercise 1

## (a) read the data
wheel <- read.csv("https://nkha149.github.io/stat385-sp2020/files/data/roulette.csv")

## (b) function roulette()
roulette <- function(bet, amount = 1){
  spin <- sample(wheel$number, 1)
  red_wheel <- wheel[wheel$color =='red',]
  black_wheel <- wheel[wheel$color =='black',]
  reds <- c(seq(from = 1, to = 9, by = 2), seq(from = 12, to = 18, by = 2), seq(from = 19, to = 27, by = 2), seq(from = 30, to = 36, by = 2))
  if ((bet == "low") & (spin <= 18)){
    win = amount 
  } else if ((bet == "high") & (spin >=18)){
    win = amount 
  } else if ((bet == "red") & (spin %in% red_wheel$number)) {
    win = amount 
  } else if ((bet == "black") & (spin %in% black_wheel$number)) {
    win = amount 
  } else if ((bet == "even") & (spin %in% seq(from = 2, to = 36, by = 2))) {
    win = amount 
  } else if ((bet == "odd") & (spin %in% seq(from = 1, to = 35, by = 2))) {
    win = amount 
  } else if ((bet == "first") & (spin %in% c(1:12))) {
    win = amount * 2
  } else if ((bet == "second") & (spin %in% c(13:24))) {
    win = amount * 2
  } else if ((bet == "third") & (spin %in% c(25:36))) {
    win = amount * 2
  } else if (spin == bet){
    win = amount * 36
  } else {
    win = 0 - amount
  }
  if (win > 0){
    out = paste("$", toString(win), sep = "")
  } else {
      out = paste("-$", toString(abs(win)), sep = "")
  }
  cat(out)
}

###

## Exercise 2
roulette2 <- function(bet, amount = 1){
  spin <- sample(wheel$number, 1)
  red_wheel <- wheel[wheel$color =='red',]
  black_wheel <- wheel[wheel$color =='black',]
  reds <- c(seq(from = 1, to = 9, by = 2), seq(from = 12, to = 18, by = 2), seq(from = 19, to = 27, by = 2), seq(from = 30, to = 36, by = 2))
  if ((bet == "low") & (spin <= 18)){
    win = amount 
  } else if ((bet == "high") & (spin >=18)){
    win = amount 
  } else if ((bet == "red") & (spin %in% red_wheel$number)) {
    win = amount 
  } else if ((bet == "black") & (spin %in% black_wheel$number)) {
    win = amount 
  } else if ((bet == "even") & (spin %in% seq(from = 2, to = 36, by = 2))) {
    win = amount 
  } else if ((bet == "odd") & (spin %in% seq(from = 1, to = 35, by = 2))) {
    win = amount 
  } else if ((bet == "first") & (spin %in% c(1:12))) {
    win = amount * 2
  } else if ((bet == "second") & (spin %in% c(13:24))) {
    win = amount * 2
  } else if ((bet == "third") & (spin %in% c(25:36))) {
    win = amount * 2
  } else if (spin == bet){
    win = amount * 36
  } else {
    win = 0 - amount
  }
  print(win)
}

# Probability of winning with bet `red`
set.seed(385)
games_red <- replicate(roulette2(bet = "red", amount = 1), n = 5000)
games_won_red <- sum(games_red == 1)
prob_red <- games_won_red/5000
prob_red

# Probability of winning with bet 'first'

set.seed(385)
games_first <- replicate(roulette2(bet = "first", amount = 1), n = 5000)
games_won_first <- sum(games_first == 2)
prob_first <- games_won_first / 5000
prob_first

# Expected value of amount of money retained after 7500 bet on 'odd', and the amount '5'

set.seed(385)
odd_bet_simulation <- replicate(roulette2(bet = "odd", amount = 5), n = 7500)
money_odd <- sum(odd_bet_simulation) / 7500
money_odd

###

## Exercise 3

# Histogram of `Sepal.Length` for all three types of the Iris flowers
hist(iris$Sepal.Length, main = "Histogram of Sepal Length for all species of Iris Flowers (cm)", 
     xlab = "Sepal Length (cm)", ylab = "Probability", probability = TRUE, 
     breaks = seq (from = 4, to = 8, by = 0.25))

plot(formula = Sepal.Width ~ Sepal.Length, data = iris, main = "Iris Flowers Sepal Length vs. Sepal Width (cm)",
     xlab = "Sepal Length (cm)", ylab = "Sepal Width (cm)", xlim = c(4, 8), ylim = c(2, 4.5),
     col = c("green", "red", "blue")[as.numeric(iris$Species)], pch = 19)
legend('topright', legend = levels(iris$Species), col = c("green", "red", "blue"), pch = 19)
grid()
