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
                        select(Location, LocationName, HB2014)

#and health board reference:
hb_reference <- read_csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/geography_codes_and_labels_hb2014_01042019.csv") %>%
                  select(HB2014, HB2014Name)

#combine hospital and health board name with attendance data:
ed_arrivals <- data %>% inner_join(hospital_reference, by = c("hospital" = "Location")) %>%
                        inner_join(hb_reference, by = "HB2014") %>%
                        select(date, 
                               healthBoard = HB2014Name, hospital = LocationName, hospital_code = hospital,
                               departmentType,
                               arrivals:admissions)

#calculate 4-hour compliance and % inpatient conversion:
ed_arrivals %>% mutate(pct_compliance = 1 - (met_fourHourTarget / arrivals),
                       pct_admitted = admissions / arrivals
                       )

#we'll look at RIE only:
rie_arrivals <- ed_arrivals %>% filter(hospital_code == "S314H") %>% select(-(met_fourHourTarget:admissions))

#convert to a tsibble:
rie_tsibble <- rie_arrivals %>% transmute(date,
                                          date_index = yearmonth(date), #convert data object to a tsibble-mth type to enforce 12-month period
                                          hospital, departmentType,
                                          arrivals) %>%
                                as_tsibble(index = date_index,
                                           key = hospital)

#trend over time:
rie_tsibble %>% ggplot(aes(x = date, y = arrivals, group = hospital)) +
                  geom_line() +
                  geom_smooth() #up, up, up!

# #are the number of arrivals normally distributed?
# rie_tsibble %>% ggplot(aes(x = arrivals)) +
#                   geom_histogram()
# 
# shapiro.test(rie_tsibble$arrivals) #looks pretty normal :)

#check if we can smooth out the seasonal variation by using the daily average ed arrivals:
rie_tsibble %>% mutate(daily_average = arrivals / days_in_month(date)) %>%
  autoplot(.vars = daily_average) #doesn't make too much of a difference

#seasonal plot: 
rie_tsibble %>% gg_season(y = arrivals) #peaks around the Festival in August, December, January, May, and March

#what are the monthly averages and how have they changed over time?
rie_tsibble %>% gg_subseries(y = arrivals) #big increases over every month! this needs to be accounted for in addition to the seasonal trends

#autocorrelation?
rie_tsibble %>% ACF(value = arrivals, lag_max = 48) %>%
                autoplot() #indicates seasonality but also a shift over time

#might be appropriate to use a box-cox transformation:
#estimated a value for lambda using this function:
#forecast::BoxCox.lambda(rie_tsibble$arrivals)
rie_tsibble %>% mutate(boxcox_arrivals = box_cox(arrivals, lambda = 2)) %>%
                select(date, arrivals, boxcox_arrivals) %>%
                gather(key = "measure", "value", arrivals:boxcox_arrivals) %>%
                ggplot(aes(date, value, group = measure, colour = measure)) +
                  geom_line() +
                  facet_wrap(~measure, scales = "free") #makes very little difference

#before attempting any forecasting we split the data into training and test sets:
rie_training <- rie_tsibble %>% filter(date < "2019-01-01")

rie_test <- rie_tsibble %>% filter(date >= "2019-01-01") 

#and save outputs:
saveRDS(rie_training, "./data/rie_training.rda")
saveRDS(rie_test, "./data/rie_test.rda")


