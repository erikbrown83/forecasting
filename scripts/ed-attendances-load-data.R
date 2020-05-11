#go through some of the concepts in the forecasting: principles & practice book
#using data from the nhs scotland open data portal monthly ed activity

#effects to consider later:
#1) population size
#2) temperature
#note historic weather data available here: https://catalogue.ceda.ac.uk/uuid/dbd451271eb04662beade68da43546e1
#3) tweets?
#4) sporting events

library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(fable)

#set ggplot theme:
theme_set(theme_light())

#load function for downloading data from open data portal:
source("scripts/download-openData.R")

#load scottish a&e data from nhs scotland open data portal
data <- read_csv("https://www.opendata.nhs.scot/dataset/997acaa5-afe0-49d9-b333-dcf84584603d/resource/2a4adc0a-e8e3-4605-9ade-61e13a85b3b9/download/monthly_ae_waitingtimes_202001.csv") %>%
          mutate(date = parse_date(as.character(Month), format = "%Y%m")) %>%
          select(date, 
                 hospital = TreatmentLocation,
                 departmentType = DepartmentType,
                 arrivals = NumberOfAttendancesAggregate,
                 met_fourHourTarget = NumberMeetingTargetAggregate,
                 breaches_eightHours = AttendanceGreater8hrs,
                 breaches_twelveHours = AttendanceGreater12hrs,
                 admissions = DischargeDestinationAdmissionToSame
                 )

#fudge to exclude faulty looking data:
data <- data %>% group_by(date, hospital, departmentType) %>%
         filter(arrivals == max(arrivals)) %>%
         ungroup()

#and a hospital reference file - match on hospital name + health board
hospital_reference <- read_csv("https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-d7bdcfb120f0/resource/c698f450-eeed-41a0-88f7-c1e40a568acc/download/current_nhs_hospitals_in_scotland_030320.csv") %>%
                        select(Location, LocationName, HB)

#and health board reference:
hb_reference <- read_csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/geography_codes_and_labels_hb2014_01042019.csv") %>%
                  select(HB, HBName)

#and finally use the download_nhsOpenData function to load list size histories:
listSizes <- download_nhsOpenData(package.id = "gp-practice-populations")

#convert table so that we have only one row per location and date:
#we're only interested in combined sex
listSizes <- listSizes %>% select(Date, HB, PracticeCode, Sex, AllAges:`Ages85plus`, -contains("QF")) %>%
  pivot_longer(AllAges:`Ages85plus`) %>%
  mutate(Sex = ifelse(Sex == "All", paste0("Both_", name), paste0(Sex, "_", name))) %>%
  select(-name) %>%
  spread(key = Sex, value = value)

#combine hospital and health board name with attendance data:
ed_arrivals <- data %>% inner_join(hospital_reference, by = c("hospital" = "Location")) %>%
                        inner_join(hb_reference, by = "HB") %>%
                        select(date, 
                               healthBoard = HBName, hospital = LocationName, hospital_code = hospital,
                               departmentType,
                               arrivals:admissions)

#calculate 4-hour compliance and % inpatient conversion:
ed_arrivals <- ed_arrivals %>% mutate(pct_compliance = 1 - (met_fourHourTarget / arrivals),
                       pct_admitted = admissions / arrivals
                       )
