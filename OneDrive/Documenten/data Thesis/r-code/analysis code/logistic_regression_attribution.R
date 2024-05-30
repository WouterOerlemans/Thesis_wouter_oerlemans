library(dplyr)
library(pROC)
install.packages("randomForest")
library(randomForest)
library(tidyverse)
install.packages("AICcmodavg")
library(AICcmodavg) 
install.packages("caret")
library(caret)
install.packages("car")
library(car)
data <- read.csv("journey_nl_juist.csv")
ticket_data <- read.csv("ticket_purchase_clean.csv")



data <- data %>% rename(visitor_id = visitor...id)
merged_data <- left_join(data, ticket_data, by = "visitor_id")
merged_data <- merged_data %>%
  mutate(waarde_conversie = udo_ticket_id * aantal_tickets)
merged_data <- merged_data %>%
  mutate_at(vars(-eventtime, -country), ~replace_na(., 0))

merged_data <- merged_data %>%
  distinct(visitor_id, .keep_all = TRUE)


names(merged_data) <- tolower(names(merged_data))
names(merged_data)[names(merged_data) == "datum_refferal"] <- "datum_referral"
merged_data <- merged_data %>%
  mutate(conversion = ifelse(udo_ticket_id != 0, 1, 0))
set.seed(1234)





conversion <- glm(conversion ~ aantal_cpc + aantal_direct + aantal_affiliate + aantal_display + aantal_email + aantal_organic + aantal_social, 
                  family = binomial, data = merged_data)

library(car)

# Voeg een kleine constante toe aan de variabelen
epsilon <- 1e-6
merged_data$aantal_cpc_adj <- merged_data$aantal_cpc + epsilon
merged_data$aantal_direct_adj <- merged_data$aantal_direct + epsilon
merged_data$aantal_affiliate_adj <- merged_data$aantal_affiliate + epsilon
merged_data$aantal_display_adj <- merged_data$aantal_display + epsilon
merged_data$aantal_email_adj <- merged_data$aantal_email + epsilon
merged_data$aantal_organic_adj <- merged_data$aantal_organic + epsilon
merged_data$aantal_social_adj <- merged_data$aantal_social + epsilon

# Voeg log getransformeerde termen en interactietermen toe aan de dataset
merged_data$log_aantal_cpc <- log(merged_data$aantal_cpc_adj)
merged_data$interaction_aantal_cpc <- merged_data$aantal_cpc_adj * merged_data$log_aantal_cpc

merged_data$log_aantal_direct <- log(merged_data$aantal_direct_adj)
merged_data$interaction_aantal_direct <- merged_data$aantal_direct_adj * merged_data$log_aantal_direct

merged_data$log_aantal_affiliate <- log(merged_data$aantal_affiliate_adj)
merged_data$interaction_aantal_affiliate <- merged_data$aantal_affiliate_adj * merged_data$log_aantal_affiliate

merged_data$log_aantal_display <- log(merged_data$aantal_display_adj)
merged_data$interaction_aantal_display <- merged_data$aantal_display_adj * merged_data$log_aantal_display

merged_data$log_aantal_email <- log(merged_data$aantal_email_adj)
merged_data$interaction_aantal_email <- merged_data$aantal_email_adj * merged_data$log_aantal_email

merged_data$log_aantal_organic <- log(merged_data$aantal_organic_adj)
merged_data$interaction_aantal_organic <- merged_data$aantal_organic_adj * merged_data$log_aantal_organic

merged_data$log_aantal_social <- log(merged_data$aantal_social_adj)
merged_data$interaction_aantal_social <- merged_data$aantal_social_adj * merged_data$log_aantal_social

# Pas een model met de interactietermen voor elke variabele afzonderlijk
box_tidwell_model <- glm(conversion ~ aantal_cpc + interaction_aantal_cpc +
                           aantal_direct + interaction_aantal_direct + 
                           aantal_affiliate + interaction_aantal_affiliate +
                           aantal_display + interaction_aantal_display + 
                           aantal_email + interaction_aantal_email + 
                           aantal_organic + interaction_aantal_organic + 
                           aantal_social + interaction_aantal_social, 
                         family = binomial, data = merged_data)

# Bekijk de samenvatting van het model
summary(box_tidwell_model)

dataset_index <- sample(2, nrow(merged_data), replace = T, prob = c(0.8, 0.2))

conversion <- glm(conversion~aantal_cpc + aantal_direct  + aantal_affiliate + aantal_display + aantal_email + aantal_organic + aantal_social, family ="binomial", data = merged_data)
summary(conversion)

train <- merged_data[dataset_index==1,]
test <- merged_data[dataset_index==2,]



glm_conversion <- glm(conversion~aantal_cpc + aantal_direct + aantal_affiliate + aantal_display + aantal_email + aantal_organic + aantal_social, family ="binomial", data = train)
predictions <- predict(glm_conversion, test, type = "response")
summary(glm_conversion)
roc <- roc(test$conversion, predictions)
auc(roc)
BIC(glm_conversion)  

vif(glm_conversion)

######lastclickmodel#####
last_click <- read.csv("merged_data_nl.csv")

last_click <- last_click[order(last_click$visitor_id, last_click$datum, decreasing = TRUE), ]

# Selecteer de laatste observatie van elke visitor_id op basis van datum
last_obs_per_visitor <- last_click[!duplicated(last_click$visitor_id), ]

install.packages("caret")
library(caret)
installed.packages("pROC")
library(pROC)

# Split de data in train- en testsets (80-20 verdeling)
set.seed(42) # Voor reproduceerbaarheid
train_index <- createDataPartition(last_obs_per_visitor$conversie, p = 0.8, list = FALSE)
train_data <- last_obs_per_visitor[train_index, ]
test_data <- last_obs_per_visitor[-train_index, ]

# Trainen van een logistisch regressiemodel
logit_model <- glm(conversie ~ kanaal, data = train_data, family = binomial)

# Voorspel conversies op de testdata
test_predictions <- predict(logit_model, newdata = test_data, type = "response")

summary(logit_model)

# Bereken de AUC
roc_auc_last <- roc(test_data$conversie, test_predictions)
auc_value_last <- auc(roc_auc)
print(auc_value_last)
variance_last <- var(roc_auc_last)
sqrt(variance_last)
######firstclickattribution#####
first_click <- read.csv("last_click_data_nl.csv")
first_click <- first_click %>%
  arrange(datum) %>%
  group_by(visitor_id) %>%
  slice(1)                        

set.seed(42)

# Creëer trainings- en testdatasets voor het first_click model
train_index_first <- createDataPartition(first_click$conversie, p = 0.8, list = FALSE)
train_data_first <- first_click[train_index_first, ]
test_data_first <- first_click[-train_index_first, ]

# Trainen van een logistisch regressiemodel voor het first_click model
logit_model_first <- glm(conversie ~ kanaal, data = train_data_first, family = binomial)

# Voorspel conversies op de testdata voor het first_click model
test_predictions_first <- predict(logit_model_first, newdata = test_data_first, type = "response")

# Samenvatting van het first_click model
summary(logit_model_first)

# Bereken de AUC voor het first_click model
roc_auc_first <- roc(test_data_first$conversie, test_predictions_first)
auc_value_first <- auc(roc_auc_first)
print(auc_value_first)
variance_first <- var(roc_auc_first)
sqrt(variance_first)