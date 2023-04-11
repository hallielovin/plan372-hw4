#Remove anything existing in my environment and load important packages
rm(list=ls(all=TRUE))

library(dplyr)

#Load in the data 
data <- read.csv("airport_pairs.csv")

#QUESTION 1
#Make a table with flights to and from RDU
rdu <- subset(data, origin == "RDU" | dest == "RDU")

#Only include origin, destination, and number of passengers in the table
rdu <- select(rdu, origin, dest, passengers)

#filter the data to only include pairs with more than 10,000 passengers 
rdu <- subset(rdu, passengers >= 10000)

rdu <- rdu %>% arrange(desc(passengers))

sum(rdu$passengers)

### THE MOST POPULAR NONSTOP DESTINATION FROM RDU IS ATLANTA

#QUESTION 2
#load in tidycensus
library(tidycensus)

#use cbsa to find the total population
cbsa_population <- get_acs(geography= "cbsa", variables = "B01003_001")

#join the total population to the airport data 
cbsa_population_origin <- cbsa_population %>% rename("origin_cbsa" = "GEOID") %>% rename("origin_pop" = "estimate")

cbsa_population_dest <- cbsa_population %>% rename("dest_cbsa" = "GEOID") %>% rename("dest_pop" = "estimate")

data <- merge(data, cbsa_population_origin, by = "origin_cbsa") 
data <- merge(data, cbsa_population_dest, by = "dest_cbsa")

#Clean up the data so it only includes that necessary columns from the join
data <- subset(data, select= -c(variable.x, moe.x, variable.y, moe.y))

#Take out the metropolitan area from the main data set
data <- data[!grepl("Micro Area", data$NAME.y),]
data <- data[!grepl("Micro Area", data$NAME.x),]

#Show the total CBSA to CBSA volumes 
cbsa_group <- data %>% group_by(dest_cbsa, origin_cbsa) %>% summarize(total_passengers = sum(passengers), distance = mean(distancemiles), origin_pop = mean(origin_pop), dest_pop = mean(dest_pop))
                                                        
#load in the tidyverse package to be able to plot a scatterplot
library(tidyverse)

#Generate a scatterplot between origin population and total passengers 
ggplot(cbsa_group, aes(x=origin_pop, y=total_passengers)) +
  geom_point(size=0.1)
#centered around the x axis because there are alot of flights to and from smaller palces-- positive trend 

#Generate a scatterplot between destination population and total passengers 
ggplot(cbsa_group, aes(x=dest_pop, y=total_passengers)) +
  geom_point(size=0.1)
#not as much of a positive trend becasye the distances are more variable 

#Generate a scatterplot between distance and total passengers 
ggplot(cbsa_group, aes(x=distance, y=total_passengers)) +
  geom_point(size=0.1)
# more passengers that do not travel far 

#QUESTION 3 
fit1 <- lm(total_passengers ~ origin_pop + dest_pop + distance, data = cbsa_group)

summary(fit1)
# as the origin pop increases total passengers increases
#when everything is 0 the interecept is the total passengers 

#QUESTION 4
#make a new table with the predicted data
new_routes <- tibble(
  origin_cbsa = c("RDU", "RDU", "RDU", "RDU"),
  dest_cbsa = c("PDX", "ELP", "TLH", "SAN"),
  distance = c(2363, 1606, 496, 2193),
  origin_pop = c(1391801, 1391801, 1391801, 1391801),
  dest_pop = c(2493429, 863807, 382747, 3296317
))

new_routes$demand <- predict(fit1, new_routes)
new_routes  

#Switch populations and run it again
new_routes2 <- tibble(
  origin_cbsa = c("PDX", "ELP", "TLH", "SAN"),
  dest_cbsa = c("RDU", "RDU", "RDU", "RDU"),
  distance = c(2363, 1606, 496, 2193),
  origin_pop = c(2493429, 863807, 382747, 3296317
  ),
  dest_pop = c(1391801, 1391801, 1391801, 1391801))

new_routes2$demand <- predict(fit1, new_routes2)
new_routes2
