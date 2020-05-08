#go through some of the concepts in the forecasting: principles & practice book
#using data from the nhs r community - a&e attendances?

library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(fable)
library(NHSRdatasets)

#load the a&e dataset
data <- ae_attendances

#and lets have a look:
data %>% glimpse() #contains attendances, 4-hour breaches, and admissions

data %>% count(org_code) #274 sites! not all of them have the same amount of data

data %>% count(type) #sites broken up into 3 main types:
# Type 1: Consultant led 24-hour a&e service
# Type 2: Specialty specific service
# Other types include minor injuries, and walk in centres

data %>% filter(org_code == "AAH") %>% 
         ggplot(aes(x = period, y = attendances)) +
          geom_line()
