##############################

#### Problem-2: AI Chip ####

##############################

library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# Define parameters based on the problem description
Pc <- c( 1150,1250)
C <- c( 5000000, 4200000) # in million chips
P<- c( 1950, 1850,  2000,  1800)
D<- c(3600000, 4600000, 1100000,3500000) # in million chips
Dc<- matrix(data = c(300, 400, 550, 450,
                     600, 300, 400, 250), 
            nrow = 2, ncol = 4, byrow = TRUE)

# Define the model
model2 <- MIPModel() %>%
  add_variable(x[i, j], i = 1:2, j = 1:4, type = "integer", lb = 0) %>%  # Adding variables for chips to sell from each fab to each customer
  set_objective(sum_expr((P[j] - Dc[i, j] - Pc[i]) * x[i, j], i = 1:2, j = 1:4), "max") %>%    # Objective function to maximize profit
  add_constraint(sum_expr(x[i, j], j = 1:4) <= C[i] , i = 1:2) %>% # Capacity constraints for each fab
  add_constraint(sum_expr(x[i, j], i = 1:2) <= D[j] , j = 1:4)  # Demand constraints for each customer
#Result  
Result <- solve_model(model2,with_ROI(solver = "glpk", verbose = TRUE)) 
print(Result) # Profit Euros
#Allocation Of Plant (Number of Chips)
Allocation <- get_solution(Result, x[i,j])%>%
  filter(value > 0)
print(Allocation)