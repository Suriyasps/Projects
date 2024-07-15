##############################
##############################

# Segmentation Analysis in R #

##############################
##############################
## Install Packages (if needed)
## Load Packages and Set Seed
library(dplyr)
library(readxl)
library(readr)
library(dplyr)
library(stringr)
#### Set Seed ####
set.seed(40423910)

#### Import Data ####
df <- read.csv("C:/Users/S.P.SURIYA/OneDrive/Desktop/MA/Assignment1.csv") 

####  Data Quality ####

# Change NA to Zero, followed by replacing it to unique numbers
df$CustomerID[is.na(df$CustomerID)] <- 0
zero_indices <- which(df$CustomerID == 0)
# Generate a sequence of numbers to replace 0 values, starting from 18288
replacement_values <- seq(from = 18288, 
                          by = 1, 
                          length.out = length(zero_indices))
df$CustomerID[zero_indices] <- replacement_values

# Ensure 'Date' is in the correct date format (if it's not already)
df$Date <- as.Date(df$InvoiceDate, format = "%Y-%m-%d")

# Sort by CustomerID and Date to ensure recency
df <- df %>% arrange(CustomerID, InvoiceDate)
# Update attributes to the most recent ones for each CustomerID
df_processed <- df %>%
  group_by(CustomerID) %>%
  mutate(Age = last(Age), 
         Married = last(Married),
         ReturnRate = last(ReturnRate),
         ZipCode = last(ZipCode),
         Income = last(Income),
         Work = last(Work),
         Edcation = last(Edcation)) %>%
  ungroup()

# Save the file as a new one
write.csv(df_processed, "C:/Users/S.P.SURIYA/OneDrive/Desktop/MA/Assignment1_processed.csv")

##########################################################
#### Run hierarchical clustering with bases variables ####
##########################################################

seg <-  read.csv("C:/Users/S.P.SURIYA/OneDrive/Desktop/MA/Assignment1_processed.csv")

#### Complete ####
hc_complete <- hclust(dist(scale(cbind(seg$UnitPrice, seg$ReturnRate, seg$Quantity,seg$InvoiceNo,seg$StockCode,seg$Description,seg$CustomerID))), method="complete")
plot(hc_complete, main = "Complete Linkage Dendrogram", sub = "", xlab = "")

# Elbow plot for first 10 segments

# Open a new graphics window with specified width and height (in inches)
windows(width = 5, height = 5)

# Your plotting code
x <- c(1:10)
sort_height <- sort(hc_complete$height, decreasing = TRUE) # Ensure hc_complete$height is defined
y <- sort_height[1:10]
plot(x, y, type = "b", col = "blue", pch = 19)
lines(x, y, col = "blue")


#### Single linkage ####
hc_single <-  hclust(dist(scale(cbind(seg$UnitPrice, seg$ReturnRate, seg$Quantity,seg$InvoiceNo,seg$StockCode,seg$Description,seg$CustomerID))), method = "single")
plot(hc_single, main = "Single Linkage Dendrogram", sub = "", xlab = "")

# Elbow plot for first 10 segments
# Open a new graphics window with specified width and height (in inches)
windows(width = 5, height = 5)

# Your plotting code
x <- c(1:10)
sort_height <- sort(hc_single$height, decreasing = TRUE) # Ensure hc_complete$height is defined
y <- sort_height[1:10]
plot(x, y, type = "b", col = "green", pch = 19)
lines(x, y, col = "green")


#### Average linkage ####
hc_average <-  hclust(dist(scale(cbind(seg$UnitPrice, seg$ReturnRate, seg$Quantity,seg$InvoiceNo,seg$StockCode,seg$Description,seg$CustomerID))), method = "average")
plot(hc_average, main = "Average Linkage Dendrogram", sub = "", xlab = "")
# Elbow plot for first 10 segments
# Open a new graphics window with specified width and height (in inches)
windows(width = 5, height = 5)

# Your plotting code
x <- c(1:10)
sort_height <- sort(hc_average$height, decreasing = TRUE) # Ensure hc_complete$height is defined
y <- sort_height[1:10]
plot(x, y, type = "b", col = "red", pch = 19)
lines(x, y, col = "red")

#### Centroid linkage ####
hc_centroid <-  hclust(dist(scale(cbind(seg$UnitPrice, seg$ReturnRate, seg$Quantity,seg$InvoiceNo,seg$StockCode,seg$Description,seg$CustomerID))), method = "centroid") 
plot(hc_centroid, main = "Centroid Linkage Dendrogram", sub = "", xlab = "")

# Elbow plot for first 10 segments
# Open a new graphics window with specified width and height (in inches)
windows(width = 5, height = 5)

# Your plotting code
x <- c(1:10)
sort_height <- sort(hc_centroid$height, decreasing = TRUE) # Ensure hc_complete$height is defined
y <- sort_height[1:10]
plot(x, y, type = "b", col = "purple", pch = 19)
lines(x, y, col = "purple")



############################
############################

#### K-means clustering ####

############################

install.packages("fastDummies")
install.packages("dplyr")
install.packages(read.xl)
library(fastDummies)
library(dplyr)
library(readxl)

df<- read.csv("C:/Users/S.P.SURIYA/OneDrive/Desktop/MA/Assignment1_processed.csv")

## Create dummy variables for the base nominal variables
df_dummy <- df %>%
  dummy_cols(select_columns = c("InvoiceNo", "CustomerID", "Description", "StockCode"), 
             remove_selected_columns = TRUE, remove_first_dummy = TRUE)

## Select and scale the numeric variables along with the dummy variables
df_for_clustering <- df_dummy %>% 
  select(starts_with("UnitPrice"), starts_with("ReturnRate"), starts_with("Quantity"), starts_with("InvoiceNo_"), starts_with("CustomerID_"), starts_with("Description_"), starts_with("StockCode_")) %>%
  scale()

####  K-means clustering with Different Start Points ####
set.seed(40423910)  # T0 Ensure reproducibility
kmeans_result1 <- kmeans(df_for_clustering,  3)
kmeans_result2 <- kmeans(df_for_clustering,  3,nstart = 10)
kmeans_result3 <- kmeans(df_for_clustering,  3,nstart = 25)
kmeans_result4 <- kmeans(df_for_clustering,  3,nstart = 50)

# View the results
table(kmeans_result1$cluster)
table(kmeans_result2$cluster)
table(kmeans_result3$cluster)
table(kmeans_result4$cluster)
table(kmeans_result5$cluster)

#Within Cluster variation and Inter-CLuster Variation 
# Within-cluster sum of squares (WCSS) 
within_cluster_variation <- kmeans_result1$tot.withinss
# Calculate the mean for each column, handling possible NAs
total_mean <- colMeans(df_for_clustering, na.rm = TRUE)
# Calculate the Total Sum of Squares (TSS), handling possible NAs in the data
total_variation <- sum(sapply(1:nrow(df_for_clustering), function(i) sum((df_for_clustering[i, ] - total_mean)^2, na.rm = TRUE)))
# Inter-cluster variation
inter_cluster_variation <- total_variation - within_cluster_variation
# Print results
cat("Within-cluster variation:", within_cluster_variation, "\n")
cat("Inter-cluster variation:", inter_cluster_variation, "\n")


## Add segment number back to original data
segmentation_result <- cbind(df,kmeans_result1$cluster,kmeans_result2$cluster,kmeans_result3$cluster,kmeans_result4$cluster)
# Export data to a CSV file
write.csv(segmentation_result, file = "C:/Users/S.P.SURIYA/OneDrive/Desktop/MA//segmentation_result.csv", row.names = FALSE)





##################################################
#### Discriminant Analysis and Classification in R ####
##################################################
set.seed(40423910)
## Install Packages (if needed)
install.packages("MASS")

## Load Packages and Set Seed
library(MASS)

class <- read.csv("C:/Users/S.P.SURIYA/OneDrive/Desktop/MA/Assignment-1/segmentation_result.csv") 

## Run Discriminant Analysis - Segment -1 
fit1 <- lda(cluster4 ~ Married + Edcation + Income + Age + Work, data = class)
fit1 ## print the summary statistics of your discriminant analysis

## Check which Discriminant Functions are Significant
ldaPred <- predict(fit1, class)
ld <- ldaPred$x
anova(lm(ld[,1] ~ class$cluster4))
anova(lm(ld[,2] ~ class$cluster4))

## Check Disciminant Model Fit
pred.seg1 <- predict(fit1)$ class
tseg1 <- table(class$cluster4, pred.seg1)
tseg1 # print table
sum(diag(tseg1))/nrow(class) # print percent correct

## Run Classification Using Discriminant Function
pred.class1 <- predict(fit1, class)$class
tclass1 <- table(pred.class1)
tclass1 # print table

## Add Predicted Segment to Classification Data
class.seg <- cbind(class, pred.class1)
write.csv(class.seg, file = file.choose(new=TRUE), row.names = FALSE)


###################
#### RFM Analysis ####
###################
library(dplyr)
library(lubridate)

# Set seed for reproducibility
set.seed(40423910)

# Read in RFM data
rfm <- read.csv("C:/Users/S.P.SURIYA/OneDrive/Desktop/MA/Assignment-1/segmentation_result.csv")

# Ensure InvoiceDate is in the correct format
rfm$InvoiceDate <- as.Date(rfm$InvoiceDate, format="%Y-%m-%dT%H:%M")

# Calculate Monetary value if not directly available
rfm$Monetary <- rfm$Quantity * rfm$UnitPrice

# Set the analysis date to one day after the last purchase in your dataset
analysis_date <- max(rfm$InvoiceDate) + days(1)

# How many levels for each
groups <- 3 # This will use tertiles to sort and give 27 total groups (3x3x3)

# Independent Sort with RFM Score
rfm_data <- rfm %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.numeric(difftime(analysis_date, max(InvoiceDate), units = "days")),
    Frequency = n_distinct(InvoiceNo),
    Monetary = sum(Monetary)
  ) %>%
  ungroup() %>%
  mutate(
    R = ntile(Recency, groups),
    F = ntile(Frequency, groups),
    M = ntile(Monetary, groups),
    RFM_Score = paste0(R, F, M) # Calculated RFM Score for Independent Sort
  )

# Sequential Sort with RFM Score
rfm_data_seq <- rfm_data %>%
  arrange(Recency) %>%
  mutate(R_Seq = ntile(Recency, groups)) %>%
  arrange(Frequency) %>%
  mutate(F_Seq = ntile(Frequency, groups)) %>%
  arrange(Monetary) %>%
  mutate(M_Seq = ntile(Monetary, groups)) %>%
  mutate(
    RFM_Score_Seq = paste0(R_Seq, F_Seq, M_Seq) # Calculated RFM Score for Sequential Sort
  )

# Export RFM Results with Independent and Sequential Sort
write.csv(rfm_data, file = "rfm_independent_sort.csv", row.names = FALSE)
write.csv(rfm_data_seq, file = "rfm_final.csv", row.names = FALSE)


