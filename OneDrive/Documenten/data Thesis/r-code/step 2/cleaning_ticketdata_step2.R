ticket_purchase <- read.csv2("ticket_purchase.csv")
names(ticket_purchase) <- tolower(names(ticket_purchase))
names(ticket_purchase)[names(ticket_purchase) == "udo_online_email"] <- "visitor_id"

library(dplyr)

ticket_purchase_clean <- ticket_purchase %>%
  filter(!is.na(visitor_id) & 
           !is.na(eventtime) & 
           !is.na(udo_ticket_id) & 
           !is.na(aantal_tickets)) %>%
  select(visitor_id, eventtime, udo_ticket_id, aantal_tickets)

# Bekijk de resulterende dataset
print(ticket_purchase_clean)

write.csv(ticket_purchase_clean, file = "ticket_purchase_clean.csv", row.names = FALSE)
