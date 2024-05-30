#####kans op conversie per aantal touches

library(ggplot2)
library(scales)
merged_fr <- read.csv("merged_data_eng.csv")
merged_fr$aantal_bezoeken <- ifelse(is.na(merged_fr$aantal_bezoeken), 3, merged_fr$aantal_bezoeken)

aantal_bezoeken_en_conversie_per_visitor <- aggregate(cbind(aantal_bezoeken, conversie, totaal_waarde_conversie) ~ visitor_id, data = merged_fr, FUN = sum)

aantal_bezoeken_en_conversie_per_visitor$kans_op_conversie <- aantal_bezoeken_en_conversie_per_visitor$conversie / aantal_bezoeken_en_conversie_per_visitor$aantal_bezoeken

print(aantal_bezoeken_en_conversie_per_visitor)

gemiddelde_kans_op_conversie <- aggregate(kans_op_conversie ~ aantal_bezoeken, data = aantal_bezoeken_en_conversie_per_visitor, FUN = mean)
totaal_waarde_per_aantal_bezoeken <- aggregate(totaal_waarde_conversie ~ aantal_bezoeken, data = aantal_bezoeken_en_conversie_per_visitor, FUN = sum)



samengevoegd <- merge(gemiddelde_kans_op_conversie, totaal_waarde_per_aantal_bezoeken, by = "aantal_bezoeken")

samengevoegd <- samengevoegd[order(samengevoegd$aantal_bezoeken),]

# Bereken de cumulatieve som van de conversiewaarden
samengevoegd$cumulatieve_conversie <- cumsum(samengevoegd$totaal_waarde_conversie)

# Bereken de totale som van de conversiewaarden
totale_conversie <- sum(samengevoegd$totaal_waarde_conversie)

# Bereken de cumulatieve proportie
samengevoegd$cumulatieve_proportie <- samengevoegd$cumulatieve_conversie / totale_conversie

num_obs_more_than_100 <- sum(aantal_bezoeken_en_conversie_per_visitor$aantal_bezoeken > 100)


samengevoegd <- samengevoegd[samengevoegd$aantal_bezoeken <= 100, ]

# Voeg een rij toe voor aantal bezoeken 1
eerste_rij <- data.frame(aantal_bezoeken = 1, 
                         kans_op_conversie = NA, 
                         totaal_waarde_conversie = 0,
                         cumulatieve_conversie = 0,
                         cumulatieve_proportie = 0)
samengevoegd <- rbind(eerste_rij, samengevoegd)

library(scales)


write.csv(samengevoegd, file = "samengevoegd_eng.csv", row.names = FALSE)
# Maak de plot
ggplot(samengevoegd, aes(x = aantal_bezoeken, y = cumulatieve_proportie)) +
  geom_line() +
  labs(x = "amount of touchpoints", y = "cumulative conversion (%)") +
  ggtitle("Cumulative proportion of conversion value Germany") +
  scale_y_continuous(labels = scales::percent)+
  xlim(0, 150)


# Bekijk het resultaat
print(gemiddelde_kans_op_conversie)

# Installeer en laad de ggplot2-bibliotheek (als deze nog niet is geïnstalleerd)
# install.packages("ggplot2")
# Verwijder rijen met een kans op conversie van 0
gemiddelde_kans_op_conversie_filtered <- gemiddelde_kans_op_conversie %>%
  filter(kans_op_conversie != 0, aantal_bezoeken <= 100)


# Maak een scatterplot van het gefilterde gemiddelde kans op conversie per aantal bezoeken
ggplot(gemiddelde_kans_op_conversie_filtered, aes(x = aantal_bezoeken, y = kans_op_conversie)) +
  geom_point() +
  geom_line() +  # Vervang geom_smooth() door geom_line() voor een lijn door de punten
  labs(x = "interactions", y = "conversion probability") +
  ggtitle("probability of converting based on length of customer journey") +
  theme_minimal()



gemiddelde_kans_op_conversie <- merged_fr

gemiddelde_kans_op_conversie$kans_op_conversie <- gemiddelde_kans_op_conversie$conversie / gemiddelde_kans_op_conversie$aantal_bezoeken

gemiddelde_kans_op_conversie_filtered <- gemiddelde_kans_op_conversie %>%
  filter(kans_op_conversie != 0, aantal_bezoeken <= 2000) %>%
  group_by(aantal_bezoeken) %>%
  summarise(proportie_conversie = (sum(totaal_waarde_conversie) / sum(gemiddelde_kans_op_conversie$totaal_waarde_conversie)) * 100)


