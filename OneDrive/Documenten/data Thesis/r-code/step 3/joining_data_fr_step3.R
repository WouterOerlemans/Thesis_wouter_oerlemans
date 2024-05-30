ticket_purchase_clean <- read.csv("ticket_purchase_clean.csv")
journey_fr <- read.csv("ticket_nl_clean.csv")

library(dplyr)
library(tidyr)

journey_fr$visitor_id <- gsub('__efteling_main__5120_', '', journey_fr$visitor_id)
journey_fr$visitor_id <- gsub('__$', '', journey_fr$visitor_id)


#standaard is 0 voor converse
journey_fr$conversie <- 0

# join
overeenkomst <- intersect(journey_fr$visitor_id, ticket_purchase_clean$visitor_id)

# Update conversie 1
journey_fr$conversie[journey_fr$visitor_id %in% overeenkomst] <- 1

journey_fr <- journey_fr %>%
  arrange(conversie)

merged_fr <- journey_fr %>%
  left_join(ticket_purchase_clean %>% select(visitor_id, aantal_tickets, udo_ticket_id), by = "visitor_id") %>%
  replace_na(list(aantal_tickets = 0)) %>%
  arrange(desc(conversie))
merged_fr$datum <- as.POSIXct(merged_fr$datum, format = "%Y-%m-%d %H:%M:%S")


merged_fr <- merged_fr %>% 
  arrange(visitor_id, desc(datum))
write.csv(merged_fr, "last_click_data_nl_60_days.csv", row.names = FALSE)




# Groepeer de dataset op visitor_id en pas de waarden aan
merged_fr <- merged_fr %>%
  group_by(visitor_id) %>%
  mutate(
    conversie = ifelse(row_number() == 1, conversie, 0),
    aantal_tickets = ifelse(row_number() == 1, aantal_tickets, 0)
  ) %>%
  ungroup()  # Verwijder
# Voeg een nieuwe kolom toe met een sequentiële order per visitor_id op basis van de kolom datum
merged_fr <- merged_fr %>%
  arrange(visitor_id, datum) %>%
  group_by(visitor_id) %>%
  mutate(path_order = row_number()) %>%
  ungroup()  # Verwijder

# Voeg een nieuwe kolom toe aan merged_fr waar NA-waarden in udo_ticket_id worden vervangen door 0
# Vervang NA-waarden in udo_ticket_id door 0
merged_fr$udo_ticket_id[is.na(merged_fr$udo_ticket_id)] <- 0
merged_fr <- merged_fr %>%
  mutate(totaal_waarde_conversie = aantal_tickets * udo_ticket_id)

print(merged_fr)

write.csv(merged_fr, file = "merged_data_nl_60_days.csv", row.names = FALSE)


# Sorteer
merged_data_fr_2 <- merged_fr %>%
  arrange(visitor_id, datum, path_order)



# Groepeer
merged_data_fr_2 <- merged_data_fr_2 %>%
  group_by(visitor_id) %>%
  summarise(
    kanaal = paste(kanaal, collapse = ' > '),
    aantal_bezoeken = max(aantal_bezoeken),
    datum = paste(datum, collapse = ','),
    conversie = sum(conversie),
    aantal_tickets = sum(aantal_tickets),
    conversie_totaal_waarde = sum(totaal_waarde_conversie)
  )
merged_data_fr_2 <- merged_data_fr_2[, !(names(merged_data_fr_2) %in% c("datum", "aantal_bezoeken"))]

total_visits <- sum(merged_fr$aantal_bezoeken)

# Stap 2: Bepaal het aantal unieke waarden in de kolom "visitor_id"
unique_visitors <- length(unique(merged_fr$visitor_id))

# Stap 3: Bereken het gemiddelde aantal bezoeken per unieke bezoeker
gemiddeld_aantal_bezoeken_per_bezoeker <- total_visits / unique_visitors

# Print het resultaat
print(gemiddeld_aantal_bezoeken_per_bezoeker)



print(merged_data_fr_2)
write.csv(merged_data_fr_2, "merged_nl_flow_60_days.csv", row.names = FALSE)

aantal_conversie_1 <- sum(merged_data_fr_2$conversie == 1)

totaal_aantal_rijen <- nrow(merged_data_fr_2)

percentage_conversie_1 <- (aantal_conversie_1 / totaal_aantal_rijen) * 100


print(percentage_conversie_1)


