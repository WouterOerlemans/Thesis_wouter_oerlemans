library(dplyr)
library(ggplot2)
library(stargazer)
dataset <- read.csv("ticket_eng_clean.csv")
dataset_60_days <- read.csv("ticket_eng_clean_60_days.csv")

dataset_grouped <- dataset %>%
  group_by(visitor_id) %>%
  summarise(total_bezoeken = sum(aantal_bezoeken))

dataset_60days_grouped <- dataset_60_days %>%
  group_by(visitor_id) %>%
  summarise(total_bezoeken = sum(aantal_bezoeken))


# Bekijk de eerste paar rijen van de gegroepeerde dataset
head(dataset_grouped)


t_test_result <- t.test(dataset_grouped$total_bezoeken, dataset_60days_grouped$total_bezoeken, paired = TRUE)

# Maak een dataset voor het grafiek
t_test_data <- data.frame(
  Dataset = c("Dataset", "Dataset_60days"),
  Mean_Total_Bezoeken = c(mean(dataset_grouped$total_bezoeken), mean(dataset_60days_grouped$total_bezoeken))
)

print(t_test_result)

# Maak de plot
ggplot(t_test_data, aes(x = Dataset, y = Mean_Total_Bezoeken)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = Mean_Total_Bezoeken - t_test_result$conf.int[1], 
                    ymax = Mean_Total_Bezoeken + t_test_result$conf.int[1]),
                width = 0.2, color = "black") +
  labs(x = "Dataset", y = "Gemiddeld Total Bezoeken per Visitor_id", 
       title = "Gemiddeld Total Bezoeken per Visitor_id per Dataset") +
  theme_minimal()

alpha <- 0.05

# Bepaal het aantal vrijheidsgraden (degrees of freedom) van je t-test
# Dit is gelijk aan het aantal observaties minus 1 voor een gepaarde t-test
df <- nrow(dataset_grouped) - 1

# Bereken de kritieke waarde voor de t-test
critical_value <- qt(1 - alpha/2, df)

# Print de kritieke waarde
print(critical_value)
stargazer(t_test_result)
