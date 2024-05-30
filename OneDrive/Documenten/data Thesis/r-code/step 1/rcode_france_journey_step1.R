# Laad de gegevens in vanuit het CSV-bestand met puntkomma's als scheidingstekens
journey_fr <- read.csv2("journey_FR_hash_2.csv")
names(journey_fr) <- tolower(names(journey_fr))
names(journey_fr)[names(journey_fr) == "datum_refferal"] <- "datum_referral"

kolommen <- c("aantal_cpc","aantal_direct" , "aantal_affiliate", "aantal_display", "aantal_email", "aantal_organic", "aantal_referral", "aantal_social")

# Vervang NA-waarden door 0 in de opgegeven kolommen
for (col in kolommen) {
  journey_fr[[col]][is.na(journey_fr[[col]])] <- 0
}

names(journey_fr)[names(journey_fr) == "visitor...id"] <- "visitor_id"

journey_fr$datum_cpc <- as.POSIXct(journey_fr$datum_cpc, format = "%d-%m-%Y %H:%M")
journey_fr$datum_direct <- as.POSIXct(journey_fr$datum_direct, format = "%d-%m-%Y %H:%M")
journey_fr$datum_affiliate <- as.POSIXct(journey_fr$datum_affiliate, format = "%d-%m-%Y %H:%M")
journey_fr$datum_display <- as.POSIXct(journey_fr$datum_display, format = "%d-%m-%Y %H:%M")
journey_fr$datum_email <- as.POSIXct(journey_fr$datum_email, format = "%d-%m-%Y %H:%M")
journey_fr$datum_organic <- as.POSIXct(journey_fr$datum_organic, format = "%d-%m-%Y %H:%M")
journey_fr$datum_Refferal <- as.POSIXct(journey_fr$datum_referral, format = "%d-%m-%Y %H:%M")
journey_fr$datum_social <- as.POSIXct(journey_fr$datum_social, format = "%d-%m-%Y %H:%M")


library(tidyr)
library(dplyr)
# Selecteer de relevante kolommen
# Correcte namen voor kanalen en datums
kanalen <- c("aantal_cpc", "aantal_direct", "aantal_affiliate", "aantal_display", "aantal_email", "aantal_organic", "aantal_referral", "aantal_social")
datums <- c("datum_cpc", "datum_direct", "datum_affiliate", "datum_display", "datum_email", "datum_organic", "datum_referral", "datum_social")

# Lijst van kanalen waarin je geïnteresseerd bent
kanalen_interesse <- c("cpc", "direct", "affiliate", "display", "email", "organic", "referral", "social")

# Maak een lege lijst om de resultaten per kanaal op te slaan
resultaten_per_kanaal <- list()

# Voor elk kanaal waarin je geïnteresseerd bent
for (kanaal in kanalen_interesse) {
  # Filter de dataset voor het huidige kanaal
  resultaten_kanaal <- journey_fr %>%
    select(visitor_id, matches(paste0("aantal_", kanaal, "|datum_", kanaal))) %>%
    rename(aantal_bezoeken = !!paste0("aantal_", kanaal), datum = !!paste0("datum_", kanaal)) %>%
    filter(!is.na(aantal_bezoeken)) %>%
    mutate(kanaal = kanaal)
  
  # Voor het kanaal "display", hernoem de datumkolom expliciet naar "datum"
  if (kanaal == "display") {
    resultaten_kanaal <- resultaten_kanaal %>%
      rename(datum_display = datum)
  }
  
  # Zorg ervoor dat de datumkolom een uniform gegevenstype heeft (bijv. datetime)
  resultaten_kanaal$datum <- as.POSIXct(resultaten_kanaal$datum, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # Voeg de resultaten toe aan de lijst
  resultaten_per_kanaal[[kanaal]] <- resultaten_kanaal
}

# Combineer alle resultaten in één dataframe
resultaten <- bind_rows(resultaten_per_kanaal) %>%
  arrange(visitor_id) %>%
  select(visitor_id, kanaal, aantal_bezoeken, datum)

resultaten <- resultaten %>%
  filter(!is.na(datum) & aantal_bezoeken != 0)

write.csv(resultaten, file = "ticket_fr_clean.csv", row.names = FALSE)


######statistics per dataset######
totaal_bezoeken_per_visitor <- resultaten %>%
  group_by(visitor_id) %>%
  summarize(totaal_bezoeken = sum(aantal_bezoeken))

# Bereken het totale aantal bezoeken in de dataset
totaal_aantal_bezoeken <- sum(resultaten$aantal_bezoeken)

# Voeg de kolom toe met het percentage van het totaal aantal bezoeken per visitor_id
totaal_bezoeken_per_visitor <- totaal_bezoeken_per_visitor %>%
  mutate(percentage_van_totaal = (totaal_bezoeken / totaal_aantal_bezoeken) * 100)

# Bekijk de resultaten
print(totaal_bezoeken_per_visitor)


totaal_aantal_bezoeken <- sum(resultaten$aantal_bezoeken)
print(totaal_aantal_bezoeken)
# Bereken het aantal unieke visitor_id's
aantal_unieke_visitor_ids <- length(unique(resultaten$visitor_id))
print(aantal_unieke_visitor_ids)
# Bereken het gemiddelde aantal bezoeken per unieke visitor_id
gemiddeld_aantal_bezoeken_per_visitor <- totaal_aantal_bezoeken / aantal_unieke_visitor_ids

# Bekijk de resultaten
print(gemiddeld_aantal_bezoeken_per_visitor)

gemiddelde_aantal_bezoeken <- resultaten %>%
  group_by(visitor_id) %>%
  summarize(gemiddeld_aantal_bezoeken = mean(aantal_bezoeken))

# Bereken de standaarddeviatie van het aantal bezoeken per unieke visitor_id
standaard_deviatie_aantal_bezoeken <- sd(gemiddelde_aantal_bezoeken$gemiddeld_aantal_bezoeken)

# Bekijk de resultaten
print(standaard_deviatie_aantal_bezoeken)


#frequency analyse
kanalen <- journey_fr[, c("aantal_cpc", "aantal_direct", "aantal_affiliate", "aantal_display", "aantal_email", "aantal_organic", "aantal_social")]

# Bereken het gemiddelde voor elke kolom
gemiddelden <- colMeans(kanalen)

# Bereken het totaal van alle gemiddelden
totaal_gemiddelden <- sum(gemiddelden)

# Bereken de percentages voor elk kanaal
percentages <- (gemiddelden / totaal_gemiddelden) * 100

# Maak een dataframe met gemiddelden en percentages
resultaten <- data.frame(kanaal = names(gemiddelden), gemiddelde = gemiddelden, percentage = percentages)

# Voeg een rij toe voor het totaal percentage
totaal_percentage <- sum(percentages)
totaal_bezoek <- sum(totaal_gemiddelden)
resultaten <- rbind(resultaten, c("Totaal", totaal_bezoek, totaal_percentage))

# Print de resultaten
print(resultaten)

