# Group Assignment for IS606
# Members: Honey Berk, Sonya Hong, Rajagopal Srinivasan, James Hamski
# See'Collaborative Assignment 1.pdf' for instructions

# loading these because they're super useful
library("dplyr")
library("tidyr")

details <- read.csv("details.csv", header = TRUE)
sales <- read.csv("sales.csv", header = TRUE)

profit.margin <- details$price - details$cost

ham.daily.cost <- sales$available.ham * details$cost[1]

plot(ham.daily.cost)