require(googlesheets4)
require(dplyr)
require(readr)

emails<-read_sheet("https://docs.google.com/spreadsheets/d/1uqkViNAyUZ6eEYjXZt05nbD_0ge5xhGainiV0IUiLO4", sheet=1)%>%
  select(Email)%>%
  bind_rows(read_sheet("https://docs.google.com/spreadsheets/d/1uqkViNAyUZ6eEYjXZt05nbD_0ge5xhGainiV0IUiLO4", sheet=2))%>%
  distinct()

write_csv(emails, "~/Zoop emails.csv")

