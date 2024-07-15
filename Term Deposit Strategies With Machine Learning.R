#### Package s####
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(car)

#### Import Data ####
library(readxl)
term <- read_excel("term.xlsx")

##### Data Quality ####
term_processed <- term %>%
  filter(age <= 95) %>%
  mutate(day_of_week = ifelse(day_of_week == "tues", "tue",day_of_week)) %>%
  mutate(month = ifelse(month == "july", "jul",month)) %>%
  mutate(marital_status = na_if(marital_status,"")) %>%
  replace_na(list(marital_status="unknown")) %>%
  mutate(subscribed_numeric = ifelse(subscribed == "yes", 1, 0)) 

##### Correlation #####

cor(term_processed$subscribed_numeric, term_processed$contact_duration, method = "pearson")
cor(term_processed$subscribed_numeric, term_processed$previous_contacts, method = "pearson")
cor(term_processed$subscribed_numeric, term_processed$cons_conf_idx, method = "pearson")
cor(term_processed$subscribed_numeric, term_processed$age, method = "pearson")
cor(term_processed$subscribed_numeric, term_processed$campaign, method = "pearson")

#### Visualization - 1 ####

term_summarized <- df %>%
  group_by(contact_duration_min, subscribed) %>%
  summarize(count = n(), .groups = 'drop')
library(ggplot2)
ggplot(term_summarized, aes(x = contact_duration_min, y = count, group = subscribed, color = subscribed)) +
  geom_line() +
  scale_color_manual(values = c("yes" = "green", "no" = "red")) +
  labs(title = "Contact Duration vs Count of Subscriptions",
       x = "Contact Duration",
       y = "Count of Subscriptions") +
  theme_minimal()

#### Visualization - 2 ####

ggplot(term_processed, aes(x = as.factor(previous_contacts), fill = subscribed)) +
  geom_bar(position = "dodge", stat = "count") +
  scale_fill_manual(values = c("yes" = "green", "no" = "red")) +
  labs(title = "Relationship Between Previous Contacts and Subscription",
       x = "Number of Previous Contacts",
       y = "Count",
       fill = "Subscription Status") 

#### Visualization - 3 #####

ggplot(term_processed, aes(x = cons_conf_idx, fill = subscribed)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  scale_fill_manual(values = c("yes" = "green", "no" = "red")) +
  labs(title = "Consumer Confidence Index vs Subscription",
       x = "Consumer Confidence Index",
       y = "Count",
       fill = "Subscribed") +
  theme_minimal()

##### Visualization - 4 ####

ggplot(term_processed, aes(x = age, fill = subscribed)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("yes" = "green", "no" = "red")) +
  labs(title = "Age Density by Subscription Status",
       x = "Age",
       y = "Density")


##### Visualization - 5 #####

ggplot(term_processed, aes(x = campaign, fill = subscribed)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~subscribed) +
  scale_fill_manual(values = c("yes" = "green", "no" = "red")) +
  labs(title = "Campaign Contacts by Subscription Status",
       x = "Number of Contacts During Campaign",
       y = "Subcription")

#### Data Partition ####
library(caret)
set.seed(40423910)
index <- createDataPartition(term_processed$subscribed,p=0.8, list = FALSE)
train <-  term_processed[index,]
test <- term_processed[-index,]
as.factor(train$subscribed)

##### Logistic Regression ####
formula1<- subscribed_numeric ~  previous_contacts + cons_conf_idx +age + campaign 
model1<-glm(formula1, data=train,family="binomial")
summary(model1)

formula2 <- subscribed_numeric ~  previous_contacts + cons_conf_idx + age + campaign + education_level + credit_default 
model2<-glm(formula2, data=train,family = "binomial")
summary(model2)


formula3<- subscribed_numeric ~ previous_contacts + cons_conf_idx + age + campaign + education_level + credit_default +contact_method+ day_of_week + n_employed
model3 <- glm(formula3, data=train,family="binomial")
summary(model3)
exp(model2$coefficients)


library(stargazer)
stargazer(model1,model2,model3, type = "html",out = "LogisticModel-Term.html")


exp(model3$coefficients)

logisticPseudoR2s <- function(LogModel){
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev / nullDev
  R.cs <- 1 - exp (-(nullDev - dev)/ modelN)
  R.n <- R.cs/(1-(exp(-(nullDev/modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2", round(R.l,3),"\n")
  cat("Cox and Snell R^2", round(R.l,3),"\n")
  cat("Nagelkerke R^2",round(R.n,3), "\n")
}
logisticPseudoR2s(model3)


exp(confint(model3))
library(dplyr)
train$predictedProbabilities <- fitted(model3)
head(data.frame(train$predictedProbabilities,train$subscribed))
train$standardisedResiduals <- rstandard(model3)
train$studentisedResiduals <- rstudent(model3)
sum(train$standardisedResiduals > 1.96)
sum(train$standardisedResiduals > 2.58)
sum(train$standardisedResiduals > 3)
train$cook <- cooks.distance(model3)
sum(train$cook > 1)
summary(train$cook)
max(train$cook)
train$leverage <- hatvalues(model3)
sum(train$leverage > 0.0009)

library(car)
vif(model3)

####Predictions####


predictions <- predict(model3, test, type = "response")
head(predictions)
class_pred <- as.factor(ifelse(predictions > 0.5, "yes","no"))
head(class_pred)
postResample(class_pred,test$subscribed)
confusionMatrix(class_pred,test$subscribed)






