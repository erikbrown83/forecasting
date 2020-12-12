#scrape scotland rugby international fixtures
#use the espn website: https://www.espn.com/rugby/results/_/team/2/season/2011

library(tidyverse)
library(rvest)
library(lubridate)
library(glue)

#happily each years results are accessible via their own link so we
#try the 2020 link first:
link_2020 <- "https://www.espn.com/rugby/results/_/team/2/season/2020"

results <- read_html(link_2020)

#all information we need is held under the td tag:
result_text <- results %>% html_nodes("td") %>%
                           html_text()

#each match has 6 elements: date, home team + score (annoying), away team, match status, competition, and venue:
#extract each and combine into one data frame:
match_data <- tibble(date = result_text[seq(1, length(result_text) - 5, 6)],
                     home_team = result_text[seq(2, length(result_text) - 4, 6)],
                     away_team = result_text[seq(3, length(result_text) - 3, 6)],
                     match_status = result_text[seq(4, length(result_text) - 2, 6)],
                     competition = result_text[seq(5, length(result_text) - 1, 6)],
                     venue = result_text[seq(6, length(result_text), 6)]
                     )

#then tidy up the columns for output:
match_data <- match_data %>% mutate(date = str_replace_all(date, ".*,", "2020"),
                                    date = ymd(date),
                                    score = str_remove_all(home_team, "[A-z]"),
                                    home_team = str_trim(str_remove_all(home_team, "[0-9]|[-]"), side = "right")
                                    )


#convert to a function and run over each year from 2010 to 2020:
scrape_scotland_rugby <- function(year, link = "https://www.espn.com/rugby/results/_/team/2/season/"){
  
  message(glue("Loading results for { year }"))
  
  #combine year and link:
  web_link <- glue("{ link }{ year }")
  
  #read results page:
  results <- read_html(web_link)
  
  #all information we need is held under the td tag:
  result_text <- results %>% html_nodes("td") %>%
                             html_text()
  
  #each match has 6 elements: date, home team + score (annoying), away team, match status, competition, and venue:
  #extract each and combine into one data frame:
  match_data <- tibble(date = result_text[seq(1, length(result_text) - 5, 6)],
                       home_team = result_text[seq(2, length(result_text) - 4, 6)],
                       away_team = result_text[seq(3, length(result_text) - 3, 6)],
                       match_status = result_text[seq(4, length(result_text) - 2, 6)],
                       competition = result_text[seq(5, length(result_text) - 1, 6)],
                       venue = result_text[seq(6, length(result_text), 6)]
                       )
  
  #then tidy up the columns for output:
  match_data <- match_data %>% mutate(date = str_replace_all(date, ".*,", as.character(year)),
                                      date = ymd(date),
                                      score = str_remove_all(home_team, "[A-z]"),
                                      home_team = str_trim(str_remove_all(home_team, "[0-9]|[-]"), side = "right")
                                      )
  
  return(match_data)
  
  
}

#make a vector of years and then combine into one data frame:
years <- (2010:2020)

rugby_fixtures <- years %>% map_df(~scrape_scotland_rugby(year = .x))
