library(tidyverse)
library(ggplot2)
boxplot(ames_processed$sale_price)
ames<- read_excel("C:/Users/S.P.SURIYA/OneDrive/Desktop/Statics For Business/ames (1).xlsx")
ames_processed <- ames %>%
  mutate(
    zone = str_replace(zone, "A\\(agr\\)", "A"),
    zone = str_replace(zone, "C\\(all\\)", "C"),
    zone = str_replace(zone, "I\\(all\\)", "I"),
    building = str_replace(building, "Twhs", "TwhsI")
  ) %>%
  filter(
    lot_area >= 100,
    house_quality <= 10,
    rooms_tot != 0,
    sale_price <= 650000
  ) %>%
  mutate(floor_tot = floor1_sf + floor2_sf)
library(stats)
library(openxlsx)
write.xlsx(ames_processed,"C:/Users/S.P.SURIYA/OneDrive/Desktop/Statics For Business/ames_processed.xlsx")
summary(ames$zone)

cor(ames_processed$sale_price, ames_processed$house_quality, method = "spearman")

result <- aov(sale_price~house_quality,data= ames_processed)

cor(lm(sale_price ~ house_quality, data = ames_processed),) 
eta_squared <- summary(result2)[[1]]$'Sum Sq'[1] / sum(summary(result2)[[1]]$'Sum Sq')
eta_squared



## Load necessary libraries
library(ggplot2)
library(reshape2)  # For melting the data frame

# Calculate the correlations
correlations <- data.frame(
  house_quality = cor(ames_processed$sale_price, ames_processed$house_quality, method = "spearman"),
  floor1_sf = cor(ames_processed$sale_price, ames_processed$floor1_sf, method = "pearson"),
  year_built = cor(ames_processed$sale_price, ames_processed$year_built, method = "spearman"),
  full_bath = cor(ames_processed$sale_price, ames_processed$full_bath, method = "spearman"),
  year_remod = cor(ames_processed$sale_price, ames_processed$year_remod, method = "spearman"),
  rooms_tot = cor(ames_processed$sale_price, ames_processed$rooms_tot, method = "spearman")
)

# Transpose the data frame to get variables as rows
correlations <- as.data.frame(t(correlations))

# Add a variable name column
correlations$variable <- rownames(correlations)

# Melt the correlations data frame for ggplot2
melted_correlations <- melt(correlations, id.vars = "variable")

# Create the heatmap
ggplot(melted_correlations, aes(x = variable, y = variable, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank()) +
  labs(fill = "Correlation") +
  geom_text(aes(label = sprintf("%.2f", round(value, 2))), size = 4)

# Print the plot
print(correlation_heatmap)

library(ggplot2)
ggplot(ames_processed, aes(x = house_quality, y = sale_price)) +
  geom_boxplot() +
  labs(x = "House Quality", y = "Sale Price", 
       title = "Correlation between Sale Price and House Quality") +
  theme_minimal()

ggplot(ames_processed, aes(x = house_quality, y = sale_price)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(x = "House Quality", y = "Sale Price", 
       title = "Correlation between Sale Price and House Quality") +
  theme_minimal()

library(ggplot2)

# Assuming 'ames' is your data frame
# Plotting the correlation
ggplot(ames_processed, aes(x = house_quality, y = sale_price)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +  # Linear regression line
  labs(title = "Correlation between Sale Price and House Quality",
       x = "House Quality",
       y = "Sale Price") +
  theme_minimal()

ggplot(ames_processed, aes(x = sale_price , y = floor1_sf)) +
  geom_point(aes(color = floor1_sf), alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  scale_color_viridis_c() + 
  labs(title = "Relationship between Sale Price and floor1_sf",
       x = "Sale Price",
       y = "floor1_sf") +
  theme_minimal() +
  theme(legend.position = "none") 

?theme_minimal

ggplot(ames_processed, aes(x = floor1_sf, y = sale_price)) + 
  geom_point() +  # This adds the scatter plot points
  labs(title = "Sale Price vs. Floor 1 Square Footage",
       x = "Floor 1 Square Footage (floor1_sf)",
       y = "Sale Price") + 
  theme_minimal()  




ggplot(data = ames_processed, aes(x = floor1_sf, y = sale_price)) +
  geom_point() +
  labs(title = "Sale Price vs. First Floor Square Footage",
       x = "First Floor Square Footage (floor1_sf)",
       y = "Sale Price") +
  theme_minimal()  



ggplot(ames_processed, aes(x = floor1_sf, y = sale_price)) + 
  geom_line() +
  labs(title = "Sale Price vs. Floor 1 Square Feet",
       x = "Floor 1 Square Feet",
       y = "Sale Price") +
  theme_minimal()



ggplot(ames_processed, aes(x = floor_tot, y = sale_price)) + 
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Sale Price vs. Floor1 Square Footage",
       x = "Floor1 Square Footage",
       y = "Sale Price") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  


ggplot(ames_processed, aes(x = year_built, y = sale_price)) + 
  geom_smooth(method = 'loess', span = 0.3, se = FALSE) +
  labs(title = "Sale Price vs. Floor Total Square Feet",
       x = "Floor Total Square Feet",
       y = "Sale Price") +
  theme_minimal()

library(tidyverse)
ames_processed <- ames_processed %>%
  mutate(floor_tot = floor1_sf + floor2_sf)



ggplot(ames_processed, aes(x = year_built, y = sale_price)) + 
  geom_point(aes(color = year_built)) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Sale Price vs. Year Built",
       x = "Year Built",
       y = "Sale Price") +
  theme_minimal()



ggplot(ames_processed, aes(x = full_bath, y = sale_price)) + 
  geom_point() +
  labs(title = "Sale Price vs. Number of Full Bathrooms",
       x = "Number of Full Bathrooms",
       y = "Sale Price") +
  theme_minimal()




ggplot(ames_processed, aes(x = factor(full_bath), y = sale_price)) + 
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Average Sale Price by Number of Full Bathrooms",
       x = "Number of Full Bathrooms",
       y = "Average Sale Price") +
  theme_minimal()


ggplot(ames_processed, aes(x = factor(full_bath), y = sale_price, fill = factor(full_bath))) + 
  geom_bar(stat = "summary", fun = "mean") +
  scale_fill_brewer(palette = "Set3") + 
  labs(title = "Average Sale Price by Number of Full Bathrooms",
       x = "Number of Full Bathrooms",
       y = "Average Sale Price") +
  theme_minimal() +
  theme(legend.position = "none")



###Regression Analysis###
data<- ames_processed
install.packages("caret")
library(caret)
set.seed(40423910)
index <- createDataPartition(data$sale_price, times=1, p=0.8,list = FALSE)
train <- data[index,]
test <- data[-index,]
data$house_quality <- as.factor(data$house_quality)
formula <- (sale_price ~ house_quality + floor_tot + year_built + full_bath + year_remod + rooms_tot)
model1 <- lm(formula = formula, data = train)
summary(model1)

formula2 <- (sale_price ~ house_quality + floor_tot + year_built + full_bath + year_remod + rooms_tot + lot_area + zone+ house_condition)
model2 <- lm(formula = formula2, data = train)
summary(model2)


formula3 <-  (sale_price ~ house_quality + floor_tot + year_built + full_bath + year_remod + rooms_tot + lot_area) 
model3 <- lm(formula = formula3, data = train)
summary(model3)

formula4 <-  (sale_price ~ house_quality + floor_tot + year_built + full_bath + year_remod + rooms_tot + lot_area + zone ) 
model4 <- lm(formula = formula4, data = train)
summary(model4)

formula5 <-  (sale_price ~ house_quality + floor_tot + year_built + full_bath + year_remod + rooms_tot + lot_area + zone + house_condition) 
model4 <- lm(formula = formula4, data = train)
summary(model4)



formula5 <- (sale_price ~ house_quality + floor_tot + year_built + full_bath + year_remod + rooms_tot + lot_area + zone + house_condition +roof_type + stories) 
model5 <- lm(formula = formula5, data = train)
summary(model5)


formula6 <- (sale_price ~ house_quality + floor_tot + year_built + full_bath + year_remod + rooms_tot + lot_area + zone + house_condition +roof_type + stories + building) 
model6 <- lm(formula = formula6, data = train)
summary(model6)


formula7 <- (sale_price ~ house_quality + floor_tot + year_built + full_bath + year_remod + rooms_tot + lot_area + zone + house_condition +roof_type + stories + garage_area) 
model7 <- lm(formula = formula7, data = train)
summary(model7)

formula8 <- (sale_price ~ house_quality + floor_tot + year_built + full_bath + year_remod + rooms_tot + lot_area + zone + house_condition +roof_type + stories + garage_area + neighbourhood) 
model8 <- lm(formula = formula8, data = train)
summary(model8)


formula9 <- (sale_price ~ house_quality + floor_tot + year_built + full_bath + year_remod + rooms_tot + lot_area + zone + house_condition +roof_type + stories + garage_area + neighbourhood + kitchen_qual) 
model9 <- lm(formula = formula9, data = train)
summary(model9)

formula10 <- (sale_price ~ house_quality + floor_tot + year_built + full_bath + year_remod + rooms_tot + lot_area + zone + house_condition +roof_type + stories + garage_area + neighbourhood + kitchen_qual + veneer_area) 
model10 <- lm(formula = formula10, data = train)
summary(model10)

formula11 <- (sale_price ~ house_quality + floor_tot + year_built + full_bath + year_remod + rooms_tot + lot_area + zone + house_condition +roof_type + stories + garage_area + neighbourhood + kitchen_qual + veneer_area + roof_type ) 
model11 <- lm(formula = formula11, data = train)
summary(model11)




model12<- lm(formula = formula11, data= test)
summary(model12)

install.packages("stargazer")
library(stargazer)
stargazer(model12, type = "html",out = "model12.html")

test$predictions <- predict(model12,test)



head(resid(model12))

head(rstandard(model12))
     
install.packages("lmtest")
library(lmtest)
dwtest(model11)

library(ggplot2)

ggplot(ames_processed, aes(x = year_remod, y = sale_price)) +
  geom_point(color = "green") +  
  labs(title = 'Sale Price vs. Year of Remodeling',
       x = 'Year of Remodeling',
       y = 'Sale Price') +
  theme_minimal()




ggplot(ames_processed, aes(x = rooms_tot, y = sale_price, color = additional_variable)) +
  geom_point(alpha = "0.5") +  
  scale_color_viridis_d() +  
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  labs(title = 'Sale Price vs. Total Number of Rooms',
       x = 'Total Number of Rooms',
       y = 'Sale Price') +
  theme_minimal() +
  theme(legend.position = "bottom")



###Regression Analysis###
data<- ames_processed
as.factor(ames_processed$neighbourhood)
install.packages("caret")
library(caret)
set.seed(40423910)
index <- createDataPartition(data$sale_price, times=1, p=0.8,list = FALSE)
train <- data[index,]
test <- data[-index,]
data$house_quality <- as.factor(data$house_quality)
formula <- (sale_price ~ house_quality + floor_tot + year_built + full_bath + year_remod + rooms_tot)
model1 <- lm(formula = formula, data = train)
summary(model1)

formula2 <- (sale_price ~ house_quality + floor_tot + year_built + full_bath + year_remod + rooms_tot + lot_area + house_condition)
model2 <- lm(formula = formula2, data = train)
summary(model2)

summary(FinalModel)


formula3<- (sale_price ~ house_quality + floor_tot + year_built + full_bath + year_remod + rooms_tot + lot_area  + house_condition +roof_type + stories + building) 
model3 <- lm(formula = formula3, data = train)
summary(model3)

formula4 <- (sale_price ~ house_quality + floor_tot + year_built + full_bath + year_remod + rooms_tot + lot_area + house_condition +roof_type + stories + garage_area + kitchen_qual) 
model4 <- lm(formula = formula4, data = train)
summary(model4)






model6<- lm(formula = formula4, data= test)
summary(model6)

install.packages("stargazer")
library(stargazer)
stargazer(model1,model2,model3,model4,model5, type = "html",out = "FinalModel2.html")

library(caret)
predictions <- predict(model5, newdata = test)
test$predictions <- predictions
postResample(predictions,test$sale_price)
stargazer(predictions$test, type = "html",out = "Prediction")
###Doubts

head(resid(model5))
diag<- train
diag$residuals <- resid(model5)
diag$standardized_residuals <- rstandard(model5)
diag$cooks_distance <- cooks.distance(model5)
diag$dfbeta <- dfbeta(model5)
diag$dffits <- dffits(model5)
diag$leverage <- hatvalues(model5)
diag$covariance_ratios <- covratio(model5)
summary(diag)
View(diag)

library(ggplot2)

ggplot(ames_processed, aes(x = rooms_tot, y = sale_price, color = sale_price)) +
  geom_point(alpha = 0.6) + 
  theme_minimal() + 
  labs(title = "Relationship between Total Rooms and Sale Price",
       x = "Total Rooms",
       y = "Sale Price",
       color = "Sale Price") +
  scale_color_gradient(low = "blue", high = "red") + 
  geom_smooth(method = "lm", color = "black")


#Correlation matrix
# Install the GGally package
install.packages("GGally")

# Load the GGally package
library(GGally)
library(tidyverse)
cor1 <- cor(ames_processed$sale_price, ames_processed$floor1_sf, method = "pearson")%>%
cor(ames_processed$sale_price, ames_processed$year_built, method = "spearman")%>%
cor(ames_processed$sale_price, ames_processed$full_bath, method = "spearman")%>%
cor(ames_processed$sale_price, ames_processed$year_remod, method = "spearman")%>%
cor(ames_processed$sale_price, ames_processed$rooms_tot, method = "spearman")

ggcorr(cor1, size = 3, label = TRUE, label_size = 4, label_round = 2, label_alpha = TRUE)

mean(ames_processed$sale_price)
median(ames_processed$sale_price)

install.packages("car")
library(car)
vif(model5)
sum(cooks.distance(model5)>1)
install.packages("lmtest")
library(lmtest)
dwtest(model5)
library(car)
vif(model5)
plot(model5)


formula5 <- (sale_price ~ house_quality + floor_tot + year_built + full_bath + year_remod + rooms_tot + lot_area  + house_condition +roof_type + stories + garage_area  + kitchen_qual + veneer_area ) 
model5 <- lm(formula = formula5, data = train)
summary(model5)
vif(model5)
hist(resid(model5), breaks = 100)
hist(rstandard(model5,breaks=100))

ggplot(ames_processed, aes(x = floor_tot, y = sale_price)) +
  geom_smooth() +
  theme_minimal()+
  labs(title = " Floor Total Square Feet vs Sale Price",
       x = "Total Floor Square Feet ",
       y = "Sale Price")
summary(ames)
mean(ames$zone)
boxplot(ames$lot_area)
boxplot(ames$sale_price)
boxplot(ames$house_quality)
summary(ames$house_quality)


