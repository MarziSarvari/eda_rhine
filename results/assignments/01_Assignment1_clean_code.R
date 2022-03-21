tempratures = c(3,6,10,14)
weights = c(1,0.8,1.2,1)

library(data.table)

multiply = function(first_number, second_number){
  first_number * second_number
  }
results <- multiply(tempratures, weights)