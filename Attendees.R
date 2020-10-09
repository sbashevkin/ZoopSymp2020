require(googlesheets4)
require(dplyr)
require(tidyr)
require(stringr)
require(ggplot2)
require(ggrepel)

attendees<-read_sheet("https://docs.google.com/spreadsheets/d/1uqkViNAyUZ6eEYjXZt05nbD_0ge5xhGainiV0IUiLO4")%>%
  rename(Datetime=Timestamp, Days="What days will you attend?", Zooplankter="Name your favorite zooplankter")%>%
  group_by(Email)%>%
  summarise(Name=unique(Name)[1], Datetime=min(Datetime), Organization=unique(Organization)[1], 
            Days=unique(Days)[length(unique(Days))], Zooplankter=if_else(all(is.na(Zooplankter)), NA_character_, unique(Zooplankter)[which(!is.na(unique(Zooplankter)))[1]]),
            .groups="drop")%>%
  group_by(Name)%>%
  summarise(Email=unique(Email)[length(unique(Email))], Datetime=min(Datetime), Organization=unique(Organization)[1], 
            Days=unique(Days)[length(unique(Days))], Zooplankter=if_else(all(is.na(Zooplankter)), NA_character_, unique(Zooplankter)[which(!is.na(unique(Zooplankter)))[1]]),
            .groups="drop")%>%
  mutate(Day1=if_else(Days%in%c("Day 1, Day 2", "Day 1"), TRUE, FALSE),
         Day2=if_else(Days%in%c("Day 1, Day 2", "Day 2"), TRUE, FALSE))%>%
  mutate(Zooplankter=recode(Zooplankter, Mysids="Mysida", `Neomysis Mercedes`="Neomysis mercedis",
                            `Sheldon J. Plankton (aka Plankton) from SpongeBob`= "Sheldon J. Plankton",
                            `Any ichthyoplanker!`="Ichthyoplankton", `Daphnia Magna`="Daphnia magna",
                            Jellyfish="Cnidaria", cnidarians="Cnidaria", isopods="Isopoda",
                            `Daphia pulex bro`="Daphnia pulex"),
    Zooplankter=case_when(
    str_detect(tolower(Zooplankter), fixed("cladocera")) ~ "Cladocera",
    str_detect(tolower(Zooplankter), fixed("eurytemora")) ~ "Eurytemora affinis",
    str_detect(tolower(Zooplankter), fixed("bosmina")) ~ "Bosmina",
    str_detect(tolower(Zooplankter), fixed("amphipod")) ~ "Amphipoda",
    str_detect(tolower(Zooplankter), fixed("forbesi")) ~ "Pseudodiaptomus forbesi",
    str_detect(tolower(Zooplankter), fixed("pontella princeps")) ~ "Pontella princeps",
    str_detect(tolower(Zooplankter), fixed("eucalanus californicus")) ~ "Eucalanus californicus",
    str_detect(tolower(Zooplankter), fixed("copepod")) ~ "Copepoda",
    Zooplankter%in%c("Daphnia pulex", "Ceriodaphnia",  "Daphnia melanica", "Daphnia magna")~ Zooplankter,
    Zooplankter%in%c("Must I pick just one...", "na", "da", "All of them!")~ NA_character_,
    str_detect(tolower(Zooplankter), fixed("daphnia")) ~ "Daphnia",
    str_detect(tolower(Zooplankter), fixed("zoe")) | Zooplankter=="Porcelain crab larva"~ "Zoea",
    str_detect(tolower(Zooplankter), fixed("megalopa")) ~ "Megalopa",
    str_detect(tolower(Zooplankter), fixed("michelle")) ~ "Michelle Avila",
    str_detect(tolower(Zooplankter), fixed("kimmerer")) ~ "Wim Kimmerer",
    
    TRUE ~ Zooplankter
  ))%>%
  group_by(Zooplankter)%>%
  mutate(N=n())%>%
  ungroup()

p<-ggplot(filter(attendees, !is.na(Zooplankter)), aes(x=reorder(Zooplankter, -N), fill=N))+
  geom_bar()+
  geom_label_repel(data=filter(attendees, !is.na(Zooplankter) & N>1)%>%select(N, Zooplankter)%>%distinct(), aes(y=N, label=Zooplankter, color=if_else(N<=4, TRUE, FALSE)),
                   min.segment.length = 100)+
  coord_cartesian(expand = FALSE)+
  scale_fill_viridis_c()+
  scale_color_manual(values=c("black", "white"))+
  xlab("Favorite zooplankter")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.position="none", plot.margin = margin(l=45, t=10, r=10, b=10))

ggsave(p, filename="Favoriate zooplankters.png", device="png", width=10, height=8, units="in")  
