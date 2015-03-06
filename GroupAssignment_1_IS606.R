# Group Assignment for IS606
# Members: Honey Berk, Sonya Hong, Rajagopal Srinivasan, James Hamski
# See'Collaborative Assignment 1.pdf' for instructions
# Your job is to use the data to determine how many sandwiches of each type he should bring each day in order to maximize his expected profits. 

library(ggplot2)

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

# figuring out average daily profit as a baseline, lambda, etc. 

profit.ham <- ham.revenue - ham.cost
profit.turkey <- turkey.revenue - veggie.cost
profit.veggie <- veggie.revenue - veggie.cost
ave.profit <-c(mean(profit.ham), mean(profit.turkey), mean(profit.veggie))
ave.profit <- as.data.frame(ave.profit, row.names = c('ham', 'turkey', 'veggie'))

######### HAM ######### 
#inventory balance
ham.bal <- sales$demand.ham - sales$available.ham

#######Need to check this - does this factor in sand. not sold? maybe don't use profit margin
#revenue
daily.revenue <- ifelse(ham.bal>=0,sales$demand.ham*3.0, sales$available.ham*3.0)
revenue<-sum(daily.revenue)

#what hits profit/loss harder - money left of the table because inventory is short or unsold inventory?
foregone.sales <- ifelse(ham.bal>0, (sales$demand.ham-sales$available.ham)*6.5,0)
unused.inventory <- ifelse(ham.bal<0, (sales$available.ham - sales$demand.ham)*3.5, 0)
sum(foregone.sales)
sum(unused.inventory)

# conclusion -> foregone sales hurts profit more than unused inventory
# let's use this conclusion to form a better inventory strategy

#empirical simulation of the demand curve using the Poisson distribution
lambda.ham <- mean(sales$demand.ham)
demand.sim <- sapply(1:130, function(x) rpois(1,lambda.ham))

# these looks fairly similar
hist(demand.sim, col=rgb(0,0,1,1/4), main ="Emperical & Simulated Demand - Ham")
hist(sales$demand.ham,col=rgb(1,0,0,1/4), ylim = 30, add=TRUE)

#Old inentory strategy - make an arbitraty number of sandwiches per day, trying a few different levels
#New inventory strategy - use a Poisson distribution to decide daily sandwiches made
#Example: James runs the following function every morning and makes that many ham sandwiches

inventory.sim <- sapply(1, function(x) rpois(1,9))
inventory.sim

#simplyify with a function that calculates profit

profit.func <- function(demand.sim,inventory.sim){
  profit.sim.func <- ifelse(demand.sim-inventory.sim>=0,demand.sim*3.0, inventory.sim*3.0)
  profit.sim.func<-sum(profit.sim.func)
  return(profit.sim.func)
}

############# THis works, likely need to fix profit calculation, see line 37

demand.sim <- sapply(1:130, function(x) rpois(1,15))
inventory.sim <-sapply(1:130, function(x) rpois(1,15))
test.1 <-profit.func(demand.sim, inventory.sim)
test.1

# iterate through lambda values to optimize?
lambda.iter <- 10:20

for lambda.iter
    inventory<-rpois(1,lambda.iter)
    demand <- rpois(1,15)


######### TURKEY ######### 



lambda.turkey <- mean(sales$demand.turkey)
lambda.veggie <- mean(sales$demand.veggie)



