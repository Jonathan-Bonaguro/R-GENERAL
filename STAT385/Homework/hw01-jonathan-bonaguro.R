# First Name: Jonathan
# Last Name: Bonaguro
# NetID: jcb3
# Homework 1

###

# Exercise 1
## (a)
# Read the data
wheel <- read.csv("https://nkha149.github.io/stat385-sp2020/files/data/roulette.csv")
wheel

## (b)
# Function roullete()
roulette <- function(bet = 14){
  spin <- sample(wheel$number, 1)
  bet == spin
}
set.seed(385)
roulette(bet = 10)
roulette()
roulette(bet = 21)

###

# Exercise 2
mortgage_num <- function(m,p,r){
  ceiling((log((r/(m/p-r)+1)))/log(1 + r))
}
mortgage_num(m = 2000, p = 200000, r = 0.005)
mortgage_num(m = 1800, p = 200000, r = 0.005)

###

# Exercise 3

## (a)
dim(iris)

## (b)
head(iris)

## (c)
mean(iris[iris$Species == 'virginica', ]$Sepal.Length)

## (d)
sd(iris[iris$Species == 'versicolor', ]$Petal.Length)

## (e)
median(iris[iris$Species == 'setosa', ]$Sepal.Width)

## (f)
nrow(iris[iris$Species == 'setosa', ])

###

# Exercise 4

## (a)
coronavirus <- read.csv("https://nkha149.github.io/stat385-sp2020/files/data/coronavirus-2020-01-26.csv")
str(coronavirus)


## (b)
coronavirus[coronavirus$Date.last.updated == "1/26/2020 11:00 AM", c("Province.State", "Country", "Date.last.updated", "Confirmed")]

###
