#dowloads
install.packages("tibble")
install.packages("AICcmodavg")
install.packages("tidyverse")
install.packages("randomForest")
install.packages("pROC")
install.packages("kableExtra")
install.packages("knitr")
install.packages("stargazer")
install.packages("tidyr")
install.packages("ggplot2")
#packages
library(ChannelAttributionPro)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stargazer)
library(knitr)
library(kableExtra)
library(pROC)
library(randomForest)
library(tidyverse)
library(AICcmodavg) 
library(tibble)
#loading data

data <- read.csv("journey_conversie_nl.csv")
data_hash <- read.csv("journey_nl_juist.csv")
ticket_data <- read.csv("ticket_purchase_clean.csv")


password="0Diee$l$uwT!nnmrijtg1!lrU9g3sduapeOtwre4dimEZuvB4zapwso7ybG7"
res=choose_order(data, perc_test = 0.2, var_path="kanaal", var_conv="total_conversie",var_null="total_null", 
                 plot=TRUE, cha_sep=">",password=password)
print(res$suggested_order)
res$auc
kable(res$auc, format = "latex")
#transforming data heuristics

H=heuristic_models(Data=data, var_path="kanaal", var_conv="total_conversie", 
                   cha_sep=">", 
                   password=password)
print(H)

Heuristic_results <- H %>%
  group_by(channel) %>%
  summarize(
    channel_position = first(channel_position),
    first_touch_conversions = sum(first_touch_conversions, na.rm = TRUE),
    last_touch_conversions = sum(last_touch_conversions, na.rm = TRUE),
    linear_touch_conversions = sum(linear_touch_conversions, na.rm = TRUE),
  )


#markov model

M=markov_model(Data=data, var_path="kanaal", order= 3, var_conv="total_conversie",
               var_null="total_null", cha_sep=">", password=password, verbose = TRUE,flg_out_tran_mtx=1)
path_attribution=M$attribution
print(path_attribution)
transitie <- M$transition_matrix

Markov_france <- path_attribution %>%
  group_by(channel) %>%
  summarize(total_conversion = sum(total_conversions))

Markov_france<- Markov_france %>%
  rename(total_conversion_Markov = total_conversion)

Heuristic_results_with_markov <- left_join(Heuristic_results, Markov_france, by = "channel")

Heuristic_results_with_markov <- Heuristic_results_with_markov %>%
  rename("First Touch" = first_touch_conversions,
         "Last Touch" = last_touch_conversions,
         "Linear Value" = linear_touch_conversions,
         "Markov Value"= total_conversion_Markov)


Heuristic_results_with_markov$total_value = rowSums(Heuristic_results_with_markov[, c("First Touch", "Last Touch", "Linear Value", "Markov Value")], na.rm = TRUE)

#######logistic regression#####


data_hash <- data_hash %>% rename(visitor_id = visitor...id)
merged_data <- left_join(data_hash, ticket_data, by = "visitor_id")
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

dataset_index <- sample(2, nrow(merged_data), replace = T, prob = c(0.8, 0.2))

conversion <- glm(conversion~aantal_cpc + aantal_direct  + aantal_affiliate + aantal_display + aantal_email + aantal_organic + aantal_social, family ="binomial", data = merged_data)
summary(conversion)

train <- merged_data[dataset_index==1,]
test <- merged_data[dataset_index==2,]



glm_conversion <- glm(conversion~aantal_cpc + aantal_direct + aantal_affiliate + aantal_display + aantal_email + aantal_organic + aantal_social, family ="binomial", data = train)
predictions <- predict(glm_conversion, test, type = "response")
roc <- roc(test$conversion, predictions)
auc(roc)


kanalen <- c("cpc", "direct", "affiliate", "display", "email", "organic", "social")

# Maak een vector met de bijbehorende attributiewaarden
attributie_waarden <- c( 0.16968, 0.16081,0.17053, 0.17651 , 0.15516,0.16866,0.10664)


dataset <- tibble(
  channel = kanalen,
  "Logistic regression" = attributie_waarden
)

print(dataset)

#####put together#####
merged_dataset <- left_join(Heuristic_results_with_markov, dataset, by = c("channel"))
print(merged_dataset)



#######normalizing results#########
total_first_touch <- sum(merged_dataset$`First Touch`)
total_last_touch <- sum(merged_dataset$`Last Touch`)
total_linear_value <- sum(merged_dataset$`Linear Value`)
total_markov_value <- sum(merged_dataset$`Markov Value`)
total_logistic_regression <- sum(merged_dataset$`Logistic regression`)


merged_dataset <- merged_dataset %>%
  mutate(`First Touch` = `First Touch` / total_first_touch,
         `Last Touch` = `Last Touch` / total_last_touch,
         `Linear Value` = `Linear Value` / total_linear_value,
         `Markov Value` = `Markov Value` / total_markov_value,
         `Logistic regression` = `Logistic regression` / total_logistic_regression)


print(merged_dataset)

result_long <- merged_dataset %>%
  pivot_longer(cols = c("First Touch", "Last Touch", "Linear Value","Markov Value", "Logistic regression"),
               names_to = "Conversions Model",
               values_to = "Conversions")

model_colors <- c("First Touch" = "#0000a2",    # Zachter blauw
                  "Last Touch" = "#e9c716",     # Zachter rood
                  "Linear Value" = "#bc272d",  # Zachter groen
                  "Markov Value"= "#50adad",   # Zachter geel
                  "Logistic regression" = "#f47a00"  # Zachter roze
)

# Plot met de toegevoegde "Logistic Regression" kolom
ggplot(result_long, aes(x = channel, y = Conversions, fill = `Conversions Model`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Conversions by Channel and Model Germany", 
       y = "Conversions",
       x = "Channel",
       fill = "Conversions Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = model_colors)


merged_dataset <- select(merged_dataset, !channel_position)
merged_dataset <- select(merged_dataset, !total_value)


# Namen van alle kolommen behalve "channel"
cols_to_round <- names(merged_dataset)[!names(merged_dataset) %in% "channel"]

# Afgeronde dataset, behalve de kolom "channel"
rounded_dataset <- merged_dataset
rounded_dataset[, cols_to_round] <- round(rounded_dataset[, cols_to_round], 3)



kable(rounded_dataset, format = "latex")

#plot H1
data <- data.frame(
  Naam = c("last touch", "markov"),
  Waarde = c(0.1820238, 0.07752417) * 100
)


ggplot(data, aes(x = Naam, y = Waarde, fill = Naam)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.2f%%", Waarde)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +  
  labs(x = "Attribution Model", y = "attributed value (%)") +  
  ggtitle("attributed value Direct") +
  scale_fill_manual(values = c("last touch" = "blue", "markov" = "red")) 


#plot h2
channels <- c('Display', 'Social')
first_click <- c(0.002475353, 0.009320985)
markov <- c(0.001237941, 0.003984328)

first_click_percentage <- first_click * 100
markov_percentage <- markov * 100
data <- data.frame(
  Channel = rep(channels, times = 2),
  Value = c(first_click_percentage, markov_percentage),
  Model = rep(c('First Click', 'Markov'), each = 2)
)

ggplot(data, aes(x = Channel, y = Value, fill = Model)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'grey') +
  geom_text(aes(label = sprintf("%.2f%%", Value)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(x = "Channel", y = "Attributed Value (%)") +
  ggtitle("First Click vs Markov Values for Display and Social") +
  scale_fill_manual(values = c('First Click' = 'yellow', 'Markov' = 'blue')) +
  theme_minimal()


#plot H3
channels <- c('Social Media', 'Display', 'Email')
last_click <- c(0.0020741752, 0.0007682131, 0.2930732790)
markov <- c(0.003984328, 0.001237941, 0.234427836)

last_click_percentage <- last_click * 100
markov_percentage <- markov * 100

data <- data.frame(
  Channel = rep(channels, times = 2),
  Value = c(last_click_percentage, markov_percentage),
  Model = rep(c('Last Click', 'Markov'), each = 3)
)

ggplot(data, aes(x = Channel, y = Value, fill = Model)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'grey') +
  geom_text(aes(label = sprintf("%.2f%%", Value)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(x = "Channel", y = "Attributed Value (%)") +
  ggtitle("Last Click vs Markov Values for Different Channels") +
  scale_fill_manual(values = c('Last Click' = 'red', 'Markov' = 'blue')) +
  theme_minimal()

#plot H4

channels <- c('Social Media', 'Display', 'Affiliate')
Logistic <- c(9.62, 15.93, 15.39)
markov <- c(0.40, 0.12, 1.45)

last_click_percentage <- Logistic
markov_percentage <- markov

data <- data.frame(
  Channel = rep(channels, times = 2),
  Value = c(last_click_percentage, markov_percentage),
  Model = rep(c('Logistic', 'Markov'), each = 3)
)

ggplot(data, aes(x = Channel, y = Value, fill = Model)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'grey') +
  geom_text(aes(label = sprintf("%.2f%%", Value)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(x = "Channel", y = "Attributed Value (%)") +
  ggtitle("Last Click vs Markov Values for Different Channels") +
  scale_fill_manual(values = c('Logistic' = 'orange', 'Markov' = 'blue')) +
  theme_minimal()

#plot h5
# Gegeven data
# Gegeven data
channels <- c('Display', 'Social media', 'Email')
countries <- c('France', 'Germany', 'UK', 'Netherlands')
data <- matrix(c(
  0.008, 0.012, 0.174,
  0.012, 0.007, 0.183,
  0.052, 0.014, 0.206,
  0.001, 0.004, 0.234
), nrow=4, byrow=TRUE)

# Zet data om in een dataframe
df <- as.data.frame(data)
colnames(df) <- channels
rownames(df) <- countries

# Pastelkleuren
pastel_colors <- c("orange", "blue", "red")

# Plot de gegevens
barplot(t(df), beside=TRUE, col=pastel_colors, ylim=c(0, 0.25),
        main="Percentage of Marketing Channels by Country",
        xlab="Countries", ylab="Percentage")
# Voeg percentages toe aan elke balk
text(x = barplot(t(df), beside=TRUE, col=pastel_colors, ylim=c(0, 0.25), plot = FALSE), 
     y = as.vector(t(df)), 
     labels = paste0(round(as.vector(t(df)) * 100, 1), "%"),
     pos = 3)
# Plaats legenda buiten de grafiek
legend("topleft", legend=channels, fill=pastel_colors, bty = "n", inset = c(0.05, 0.05))




