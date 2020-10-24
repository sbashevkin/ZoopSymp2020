require(googlesheets4)
require(dplyr)
require(readr)
require(lubridate)

emails<-read_sheet("https://docs.google.com/spreadsheets/d/1uqkViNAyUZ6eEYjXZt05nbD_0ge5xhGainiV0IUiLO4", sheet=1)%>%
  select(Email)%>%
  bind_rows(read_sheet("https://docs.google.com/spreadsheets/d/1uqkViNAyUZ6eEYjXZt05nbD_0ge5xhGainiV0IUiLO4", sheet=2))%>%
  mutate(Email=tolower(Email))%>%
  distinct()

#write_csv(emails, "~/Zoop emails.csv")

New_emails<-read_sheet("https://docs.google.com/spreadsheets/d/1uqkViNAyUZ6eEYjXZt05nbD_0ge5xhGainiV0IUiLO4", sheet=1)%>%
  mutate(Datetime=force_tz(Timestamp, "America/Los_Angeles"))%>%
  filter(Datetime>as.POSIXct("2020-10-20 11:00"))

write_csv(New_emails, "~/New Zoop emails.csv")

New_emails2<-read_sheet("https://docs.google.com/spreadsheets/d/1uqkViNAyUZ6eEYjXZt05nbD_0ge5xhGainiV0IUiLO4", sheet=1)%>%
  mutate(Datetime=force_tz(Timestamp, "America/Los_Angeles"))%>%
  filter(Datetime>as.POSIXct("2020-10-24 10:15"))

write_csv(New_emails2, "~/New Zoop emails2.csv")
