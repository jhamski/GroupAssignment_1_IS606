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

######### HAM ######### 
#inventory balance
ham.bal <- sales$demand.ham - sales$available.ham

#profit function
demand.sim<-sales$demand.ham
inventory.sim<-rpois(1:130,lambda.inventory)
profit.func(demand.sim, inventory.sim)

profit.func <- function(demand.sim,inventory.sim){
  profit.sim.func <- ifelse(demand.sim-inventory.sim<=0,demand.sim*3.0-(inventory.sim-demand.sim)*3.5, inventory.sim*3.0)
  profit.sim.func<-sum(profit.sim.func)
  return(profit.sim.func)
}

obs.profit<-profit.func(sales$demand.ham, sales$available.ham)

#what hits profit/loss harder - money left of the table because inventory is short or unsold inventory?
foregone.sales <- ifelse(ham.bal>0, (sales$demand.ham-sales$available.ham)*6.5,0)
unused.inventory <- ifelse(ham.bal<0, (sales$available.ham - sales$demand.ham)*3.5, 0)
sum(foregone.sales)
sum(unused.inventory)

# conclusion -> foregone sales hurts profit more than unused inventory
# let's use this conclusion to form a better inventory strategy

#empirical simulation of the demand using the Poisson distribution
lambda.ham <- mean(sales$demand.ham)
demand.sim <- sapply(1:130, function(x) rpois(1,lambda.ham))

# these looks fairly similar
hist(demand.sim, col=rgb(0,0,1,1/4), main ="Emperical & Simulated Demand - Ham")
hist(sales$demand.ham,col=rgb(1,0,0,1/4), ylim = 30, add=TRUE)

#Old inentory strategy - make an arbitraty number of sandwiches per day, trying a few different levels

#write model using observed inventory as input

sales.model.obs<-function(){
  demand.sim <- rpois(1:130,16.17)
  inventory.sim <-sales$available.ham
  model.profit <-profit.func(demand.sim, inventory.sim)
}

model.simulated.profit <-sapply(1:10000, function(x) sales.model.obs())
#summary(model.simulated.profit)
difference <- obs.profit - mean(model.simulated.profit)
difference

#New inventory strategy - use a Poisson distribution to decide daily sandwiches made
#Example: James runs the following function every morning and makes that many ham sandwiches
inventory.sim <- sapply(1, function(x) rpois(1,9))
inventory.sim

#Inventory Strategy 2 - make a random number of sandwiches between 14 and 16 sandwiches 
sales.model.2<-function(){
  demand.sim <- rpois(1:130,16.17)
  inventory.sim <-sample(14:17, 130, replace=TRUE)
  model.profit <-profit.func(demand.sim, inventory.sim)
}

model.simulated.profit <-sapply(1:10000, function(x) sales.model.2())
summary(model.simulated.profit)
difference <-  mean(model.simulated.profit) - obs.profit
difference

#Inventory Strategy 3 - make a contant number of sandwiches, say 15 

sales.model.3<-function(sand.no){
  demand.sim <- rpois(1:130, 16.17)
  inventory.sim <-replicate(130, sand.no)
  model.profit <-profit.func(demand.sim, inventory.sim)
}
sand.no <- 14
model.simulated.profit <-sapply(1:10000, function(x) sales.model.3(sand.no))
summary(model.simulated.profit)
difference.14 <-  mean(model.simulated.profit) - obs.profit

sand.no <- 15
model.simulated.profit <-sapply(1:10000, function(x) sales.model.3(sand.no))
summary(model.simulated.profit)
difference.15 <-  mean(model.simulated.profit) - obs.profit

sand.no <- 16
model.simulated.profit <-sapply(1:10000, function(x) sales.model.3(sand.no))
summary(model.simulated.profit)
difference.16 <-  mean(model.simulated.profit) - obs.profit

sand.no <- 17
model.simulated.profit <-sapply(1:10000, function(x) sales.model.3(sand.no))
summary(model.simulated.profit)
difference.17 <-  mean(model.simulated.profit) - obs.profit

constant.count <-cbind(difference.14,difference.15,difference.16,difference.17)


######### TURKEY ######### 

lambda.turkey <- mean(sales$demand.turkey)
lambda.turkey

obs.profit.turkey<-profit.func(sales$demand.turkey, sales$available.turkey)

sales.model.obs<-function(){
  demand.sim <- rpois(1:130,21.6)
  inventory.sim <-sales$available.turkey
  model.profit <-profit.func(demand.sim, inventory.sim)
}

model.simulated.profit <-sapply(1:10000, function(x) sales.model.obs())
#summary(model.simulated.profit)
difference <- obs.profit.turkey - mean(model.simulated.profit)
difference


######### VEGGIE ######### 

lambda.veggie <- mean(sales$demand.veggie)


##### SCRAPYARD ########
# Model using Poisson to determine number of sandwiches

sales.model.4<-function(lambda.inventory){
  demand.sim <- rpois(1:130,16.17)
  inventory.sim <-rpois(1:130,lambda.inventory)
  model.profit <-profit.func(demand.sim, inventory.sim)
}

lambda.inventory = 15
model.simulated.profit <-sapply(1:10000, function(x) sales.model.4(lambda.inventory))
summary(model.simulated.profit)
difference <- mean(model.simulated.profit) - obs.profit
difference



