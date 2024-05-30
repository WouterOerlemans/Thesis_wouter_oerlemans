# Lees de dataset in
ticket_purchase <- read.csv2("ticket_purchase.csv")

# Zet de kolomnamen naar kleine letters
names(ticket_purchase) <- tolower(names(ticket_purchase))

# Hernoem de kolom "udo_online_email" naar "visitor_id"
names(ticket_purchase)[names(ticket_purchase) == "udo_online_email"] <- "visitor_id"

# Laad de dplyr library
library(dplyr)

# Filter de dataset en selecteer de relevante kolommen
ticket_purchase_clean <- ticket_purchase %>%
  filter(!is.na(visitor_id) & 
           !is.na(eventtime) & 
           !is.na(udo_ticket_id) & 
           !is.na(aantal_tickets)) %>%
  select(visitor_id, eventtime, udo_ticket_id, aantal_tickets)

# Stel de optie stringsAsFactors in op FALSE voor de kolom udo_ticket_id
ticket_purchase_clean$udo_ticket_id <- as.character(ticket_purchase_clean$udo_ticket_id)

# Verwijder "ticket-" en "WEB-" uit elke cel van de kolom udo_ticket_id
ticket_purchase_clean$udo_ticket_id <- gsub("(ticket-|WEB-)", "", ticket_purchase_clean$udo_ticket_id)

ticket_purchase_clean$udo_ticket_id <- gsub("[^0-9]+", "", ticket_purchase_clean$udo_ticket_id)


# Maak een dictionary met de gegeven waarden
values_dict <- c(
  "1899 = 38", "1900 = 38", "1901 = 38", "1902 = 38", "1903 = 42",
  "1904 = 42", "1905 = 42", "1906 = 42", "1907 = 45", "1908 = 45",
  "1909 = 45", "1910 = 45", "1911 = 48", "1912 = 48", "1913 = 48",
  "1914 = 48", "1965 = 52", "1966 = 52", "1967 = 52", "1968 = 52",
  "1974 = 38", "1975 = 38", "1976 = 38", "1977 = 38", "1978 = 44",
  "1979 = 44", "1980 = 44", "1981 = 44", "1982 = 47", "1983 = 47",
  "1984 = 47", "1985 = 47", "1986 = 51", "1987 = 51", "1988 = 51",
  "1989 = 51", "00003 = 52", "00001 = 52", "1955 = 27", "1956 = 27",
  "1957 = 27", "1958 = 27"
)

# Maak een lege lijst om de waarden in op te slaan
values_map <- list()

# Itereer over elke waarde in de dictionary en sla de waarden op in de map
for (val in values_dict) {
  pair <- strsplit(val, " = ")[[1]]
  key <- pair[1]
  value <- as.numeric(pair[2])
  values_map[[key]] <- value
}

# Vervang de waarden in de kolom udo_ticket_id door de waarden in de map
ticket_purchase_clean$udo_ticket_id <- unlist(values_map[ticket_purchase_clean$udo_ticket_id])

# Print de aangepaste dataset
print(ticket_purchase_clean)


# Schrijf de aangepaste dataset naar een CSV-bestand
write.csv(ticket_purchase_clean, file = "ticket_purchase_clean.csv", row.names = FALSE)
