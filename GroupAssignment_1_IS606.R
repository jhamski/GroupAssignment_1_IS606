# Group Assignment for IS606
# Members: Honey Berk, Sonya Hong, Rajagopal Srinivasan, James Hamski
# See'Collaborative Assignment 1.pdf' for instructions

#setwd("/Users/jim/Documents/Graduate\ School/IS\ 606/GroupAssignment_1_IS606")
details <- read.csv("details.csv")
sales <- read.csv("sales.csv")

plot(sales$demand.ham)

profit.margin <- details$price - details$cost

ham.daily.cost <- sales$available.ham * details$cost[1]

plot(ham.daily.cost)