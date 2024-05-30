library(ggplot2)
library(dplyr)

data_nl <- read.csv("samengevoegd_nl.csv")
data_fr <-read.csv("samengevoegd_fr.csv")
data_de <- read.csv("samengevoegd_de.csv")
data_eng <- read.csv("samengevoegd_eng.csv")

process_dataset <- function(dataset) {
  dataset$aantal_bezoeken <- ifelse(is.na(dataset$aantal_bezoeken), 3, dataset$aantal_bezoeken)
  
  # Voer de rest van je bewerkingen uit
  # ...
  
  return(samengevoegd) # Retourneer het samengevoegde gegevensframe
}

# Voer het script uit voor elke dataset en combineer de resultaten
datasets <- list(data_de, data_eng, data_fr, data_nl)
processed_datasets <- lapply(datasets, process_dataset)
combined_data <- do.call(rbind, processed_datasets)

# Plot de gegevens van de vier datasets
ggplot() +
  geom_line(data = data_de, aes(x = aantal_bezoeken, y = cumulatieve_proportie, color = "Germany"), size = 0.75) +
  geom_line(data = data_eng, aes(x = aantal_bezoeken, y = cumulatieve_proportie, color = "England"), size = 0.75)+
  geom_line(data = data_fr, aes(x = aantal_bezoeken, y = cumulatieve_proportie, color = "France"), size = 0.75) +
  geom_line(data = data_nl, aes(x = aantal_bezoeken, y = cumulatieve_proportie, color = "Netherlands"), size = 0.75) +
  labs(x = "amount of touchpoints", y = "cumulative conversion (%)") +
  ggtitle("Cumulative proportion of conversion value") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("blue", "green", "red", "purple"),
                     labels = c("Germany", "England", "France", "Netherlands")) +
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +  # Hier voegen we de gewenste breuken toe
  xlim(0, 40)


