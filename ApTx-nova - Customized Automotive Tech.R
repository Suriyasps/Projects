##############################

#### Problem-1: Customized Automotive Tech ####

##############################

library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# Define the capacities of each plant for each product
capacities <- matrix(
  c(1200, 0, 600, 1000,    # Plant 1 capacities for Products 1-4
    1400, 1200, 800, 1000,  # Plant 2 capacities for Products 1-4
    600, 0, 200, 600,      # Plant 3 capacities for Products 1-4
    800, 0, 0, 1200,      # Plant 4 capacities for Products 1-4
    800, 1400, 1000, 1600), # Plant 5 capacities for Products 1-4
  nrow = 5, byrow = TRUE)


# Define the model
model <- MIPModel() %>%
  # Add variables: x[i, j] = 1 if plant i produces product j, 0 otherwise
  add_variable(x[i, j], i = 1:5, j = 1:4, type = "binary") %>%
  # Objective: Maximize total production
  set_objective(sum_expr(capacities[i, j] * x[i, j], i = 1:5, j = 1:4), "max") %>%
  # Constraint: Each product is produced by exactly one plant
  add_constraint(sum_expr(x[i, j], i = 1:5) == 1, j = 1:4) %>%
  # Constraint: Each plant produces at most one product
  add_constraint(sum_expr(x[i, j], j = 1:4) <= 1, i = 1:5)

# Solve the model
solution <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
solution # Total number of batches to be produced

get_solution(solution,x[i,j]) %>%
  filter(value > 0)
 


