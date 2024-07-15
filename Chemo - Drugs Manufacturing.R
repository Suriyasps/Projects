##############################

#### Problem-3: Make-to-Stock Chemotherapy Drugs ####

##############################

install.packages("dplyr")
library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
 
# Define Model 
model99 <- MIPModel() %>%
  #Assigning Variables
  add_variable(x1, type = "integer", lb = 0) %>%
  add_variable(x2, type = "integer", lb = 0) %>%
  add_variable(x3, type = "integer", lb = 0) %>%
  add_variable(x4, type = "integer", lb = 0) %>%
  # Set Objective
  set_objective((1200*(x1 + x3) + 1400 *(x2 + x4)) - (800 * (x1 + x2)  + 1500 *(x3 + x4)) , "max") %>%
  add_constraint(x1 + x3 <= 200000) %>%  # Maximum Demand Constraint
  add_constraint(x1 + x3 >= 100000) %>%  # Minimum Delivery Constraint
  add_constraint(x2 + x4 <= 40000) %>%  # Maximum Demand Constraint
  add_constraint(x2 + x4 >= 10000)%>%   # Minimum Delivery Constraint 
  add_constraint(x1 +x2 <= 80000)%>%    # Inventory Constraints
  add_constraint(x3 + x4 <= 120000) %>%   # Inventory Constraints
  #P-metric and D-mertic Constraints
  add_constraint(((25*x1) + (15*x3))<= (23*(x1 + x3)))%>%
  add_constraint(((25*x2) + (15*x4))<= (23*(x2 + x4)))%>%
  add_constraint(((87*x1) + (98*x3)) >= (88*(x1 + x3)))%>%
  add_constraint(((87*x2) + (98*x4)) >= (93*(x2 + x4)))
# Result
Result <- solve_model(model99, with_ROI(solver = "glpk", verbose = TRUE))
Result
# Number of Vials
get_solution(Result,x1)
get_solution(Result,x2)
get_solution(Result,x3)
get_solution(Result,x4)


optimal_x1 <- get_solution(Result, x1)
optimal_x2 <- get_solution(Result, x2)
optimal_x3 <- get_solution(Result, x3)
optimal_x4 <- get_solution(Result, x4)

# Calculate D-metrics based on the optimal solution
Chemo1_D_metric <- round(((25 * optimal_x1) + (15 * optimal_x3)) / (optimal_x1 + optimal_x3),3)
Chemo2_D_metric <- round(((25 * optimal_x2) + (15 * optimal_x4)) / (optimal_x2 + optimal_x4))

# Calculate P-metrics based on the optimal solution
Chemo1_P_metric <- round(((87 * optimal_x1) + (98 * optimal_x3)) / (optimal_x1 + optimal_x3),3)
Chemo2_P_metric <- round(((87 * optimal_x2) + (98 * optimal_x4)) / (optimal_x2 + optimal_x4),3)

# D-metric and P-metric values
print(paste("Chemo-1 D-metric:", Chemo1_D_metric))
print(paste("Chemo-2 D-metric:", Chemo2_D_metric))
print(paste("Chemo-1 P-metric:", Chemo1_P_metric))
print(paste("Chemo-2 P-metric:", Chemo2_P_metric))