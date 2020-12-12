#scrape local football fixtures - only interested in home matches
#try the soccerway.com website (seems to have a full list of historical fixtures + results)

library(tidyverse)
library(lubridate)
library(rvest)
library(glue)

#read hearts fixtures page:
hearts_page <- read_html("https://uk.soccerway.com/teams/scotland/heart-of-midlothian-fc/1900/matches/")

hearts_matches <- hearts_page %>% html_nodes(".matches") %>%
                                  html_table(fill = TRUE) %>%
                                  as.data.frame()

write.csv(hearts_matches, here::here("data", "hearts-matches-raw.csv"))

#repeat for hibs:
hibs_page <- read_html("https://uk.soccerway.com/teams/scotland/hibernian-football-club/1904/matches/")

hibs_matches <- hibs_page %>% html_nodes(".matches") %>%
                              html_table(fill = TRUE) %>%
                              as.data.frame()

write.csv(hibs_matches, here::here("data", "hibs-matches-raw.csv"))

#combine match data and flag match winners:
football_matches <- hearts_matches %>% bind_rows(hibs_matches) %>%
                                       mutate(date = dmy(date),
                                              notes = ifelse(date > Sys.Date(), "future", str_remove_all(str_remove_all(score, "[0-9]"), " - ")),
                                              score = ifelse(notes == "CANC" | notes == "future", NA_character_, str_remove_all(score, "[A-z]")),
                                              home_score = as.integer(str_replace(score, " -.*", "")),
                                              away_score = as.integer(str_replace(score, ".*- ", ""))
                                              ) %>%
                                       arrange(date)

saveRDS(football_matches, here::here("data", "football-fixtures.rda"))

