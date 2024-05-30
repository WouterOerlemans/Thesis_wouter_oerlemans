install.packages("dplyr")
library(dplyr)


journey_data <- read.csv("merged_nl_flow_60_days.csv")

journey_data <- select(journey_data, -visitor_id)

# Voeg een nieuwe kolom 'total_conversie' toe en bereken de som van alle conversies per unieke waarde van 'kanaal'
journey_conversie <- journey_data %>%
  group_by(kanaal) %>%
  summarise(total_conversie = sum(conversie == 1),
            total_null = sum(conversie == 0),
            aantal_tickets = sum(aantal_tickets),
            conversie_totaal_waarde = sum(conversie_totaal_waarde))

print(journey_conversie)

View(journey_conversie)

journey_conversie <- data.frame(journey_conversie)

# Toon het resulterende data.frame
print(journey_conversie)


write.csv(journey_conversie, file = "journey_conversie_nl_60_days.csv", row.names = FALSE)
