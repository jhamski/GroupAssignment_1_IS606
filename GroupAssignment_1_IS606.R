# Group Assignment for IS606
# Members: Honey Berk, Sonya Hong, Rajagopal Srinivasan, James Hamski
# See'Collaborative Assignment 1.pdf' for instructions
# Your job is to use the data to determine how many sandwiches of each type he should bring each day in order to maximize his expected profits. 


details <- read.csv("details.csv", header = TRUE)
sales <- read.csv("sales.csv", header = TRUE)

profit.margin <- details$price - details$cost

ham.revenue <- sales$demand.ham * 6.5
turkey.revenue <- sales$demand.turkey * 6.5
veggie.revenue <- sales$demand.veggie * 5.0

ham.cost <- sales$available.ham * 3.5
turkey.cost <- sales$available.turkey * 4.0
veggie.cost <- sales$available.veggie * 2.5

distrham <- table(sales$demand.ham)/length(sales$demand.ham)
distrturkey <- table(sales$demand.turkey)/length(sales$demand.turkey)
distrveggie <- table(sales$demand.veggie)/length(sales$demand.veggie)

# Ham has the greatest profit margin. Is there a relationship between Ham and the other variables?
# Maximizing Ham could maximize revenue

summary(sales)

rpois(1,10)*25-200

  





plot(ham.daily.cost)