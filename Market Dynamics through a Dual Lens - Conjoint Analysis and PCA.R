#####################
### Conjoint Analysis ###
#####################

## Install Packages (if needed)
install.packages("conjoint")

## Load Packages and Set Seed
library(conjoint)
library(ggplot2)
library(readxl)

set.seed(40412519)

## Set up attributes and levels as a list
attrib.level <- list(Environmental_friendliness = c("0% CO2 reduction", "30% CO2 reduction", "50% CO2 reduction"),
                     Delivery_time = c("14 days", "21 days", "30 days"), 
                     Service_level = c("5-year warranty", "5-year warranty& free maintenance", "5-year warranty, free maintenance and installation, & upgradeability"),
                     Price = c("1000 GBP", "1200 GBP", "1500 GBP"), 
                     Quality_of_material = c("Market average", "A bit higher than market average"),
                     Marketing_proficiency = c("Not very proficient and poor communication","Very proficient and have good communication skills"))

## Create the fractional factorial design
design <-  read.csv("C:/Users/S.P.SURIYA/Downloads/Product Profiles.csv")
design <- subset(design, select = -Product_Profile)
design
## Check for correlation in fractional factorial design
print(cor(caEncodedDesign(design)))

# Compute the correlation matrix
correlation_matrix <- cor(caEncodedDesign(design))

# Define a happy and joyful color palette for the heatmap
colors <- c("#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A")

# Convert correlation matrix to a data frame
correlation_df <- as.data.frame(as.table(correlation_matrix))
names(correlation_df) <- c("Attribute1", "Attribute2", "Correlation")

# Create the heatmap using ggplot2 with the new colors
heatmap_plot <- ggplot(data = correlation_df, aes(x = Attribute1, y = Attribute2)) +
  geom_tile(aes(fill = Correlation), colour = "white") +
  scale_fill_gradient2(low = colors[1], mid = colors[3], high = colors[5], 
                       midpoint = 0, limits = c(-1, 1), name = "Correlation") +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) + # Add correlation values as text
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Display the heatmap plot
print(heatmap_plot)


# Print the heatmap
print(heatmap_plot)

## Export design for survey
write.csv(design, file = "C:/Users/S.P.SURIYA/OneDrive/Desktop/MarketingAnalytics/conjoint_profiles.csv", row.names = FALSE)
## Name the file conjoint_profiles.csv

## Run the conjoint analysis study

## Read in the survey preference results
pref <- read_excel("C:/Users/S.P.SURIYA/Downloads/Conjoint Prefernces.xlsx") ## Choose the file named conjoint_preferences.csv
pref <- pref[ , -1]

## Set up attributes and levels as a vector and Estimate the part-worths for each respondent
attrib.vector <- data.frame(unlist(attrib.level,use.names=FALSE))
colnames(attrib.vector) <- c("levels")
## Initialize part.worths
part.worths <- NULL

## Loop over each column of pref
for (i in 1:ncol(pref)) {
  temp <- caPartUtilities(pref[, i], design, attrib.vector)
  
  ## Pick the baseline case
  Base_Env <- temp[,"0% CO2 reduction"]
  Base_Del_Time <- temp[,"14 days"]
  Base_Service <- temp[,"5-year warranty"]
  Base_Price <- temp[,"1000 GBP"]
  Base_Quality <- temp[,"Market average"]
  Base_Marketing_proficiency <- temp[,"Not very proficient and poor communication"]
  
  ## Adjust Intercept
  temp[,"intercept"] <- temp[,"intercept"] - Base_Env - Base_Del_Time - Base_Service - Base_Price - Base_Quality - Base_Marketing_proficiency
  
  ## Adjust Coefficients
  L1 <- length(attrib.level$Environmental_friendliness) + 1
  for (j in 2:L1) {
    temp[,j] <- temp[,j] - Base_Env
  }
  
  L2 <- length(attrib.level$Delivery_time) + L1
  for (k in (L1+1):L2) {
    temp[,k] <- temp[,k] - Base_Del_Time
  }
  
  L3 <- length(attrib.level$Service_level) + L2
  for (l in (L2+1):L3) {
    temp[,l] <- temp[,l] - Base_Service
  }
  
  L4 <- length(attrib.level$Price) + L3
  for (m in (L3+1):L4) {
    temp[,m] <- temp[,m] - Base_Price
  }
  
  L5 <- length(attrib.level$Quality_of_material) + L4
  for (n in (L4+1):L5) {
    temp[,n] <- temp[,n] - Base_Quality
  }
  
  ## Marketing Proficiency
  L6 <- length(attrib.level$Marketing_proficiency) + L5
  for (o in (L5+1):L6) {
    temp[,o] <- temp[,o] - Base_Marketing_proficiency
  }
  
  ## Append temp to part.worths
  part.worths <- rbind(part.worths, temp)
}

rownames(part.worths) <- colnames(pref)
part.worths
## Export part-worths from analysis
write.csv(part.worths, file.choose(new=TRUE), row.names = FALSE) ## Name the file conjoint_partworths.csv

######################################
###Principle Component Analysis ####
######################################

######################################
### Perceptual and Preference Mapping ###
######################################

## Install Packages (if needed)
install.packages("data.table")

## Load Packages and Set Seed
library(data.table)
set.seed(40423910)

## Read in perception and preference data
per<- read.csv("C:/Users/S.P.SURIYA/Downloads/PCA data.csv")


## Run Princple Components Analysis on Perceptions
pca <- prcomp(per[,2:length(per)], retx=TRUE, scale=TRUE)

## Perceptual Map Data - Attribute Factors and CSV File
attribute <- as.data.table(colnames(per[,2:length(per)])); setnames(attribute, 1, "Attribute")
factor1 <- pca$rotation[,1]*pca$sdev[1]; factor2 <- pca$rotation[,2]*pca$sdev[2]; path <- rep(1, nrow(attribute))
pca_factors <- subset(cbind(attribute, factor1, factor2, path), select = c(Attribute, factor1, factor2, path))
pca_origin <- cbind(attribute, factor1 = rep(0,nrow(attribute)), factor2 = rep(0,nrow(attribute)), path = rep(0,nrow(attribute)))
pca_attributes <- rbind(pca_factors, pca_origin)
write.csv(pca_attributes, file = file.choose(new=TRUE), row.names = FALSE) ## Name file perceptions_attributes.csv

## Perceptual Map Data - Brand Factors and CSV File
score1 <- (pca$x[,1]/apply(abs(pca$x),2,max)[1])
score2 <- (pca$x[,2]/apply(abs(pca$x),2,max)[2])
pca_scores <- subset(cbind(per, score1, score2), select = c(Model, score1, score2))
write.csv(pca_scores, file = file.choose(new=TRUE), row.names = FALSE) ## Name file perceptions_scores.csv

## Calculate Singular Values
n <- nrow(per)
singular_values <- pca$sdev * sqrt(n - 1)
print("Singular Values:")
print(singular_values)

## Calculate Percentage of Variance Explained
pve <- (pca$sdev^2) / sum(pca$sdev^2)
print("Percentage of Variance Explained:")
print(pve)