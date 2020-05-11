#use some of the functions from tsibble, etc to explore the rie dataset:

library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(fable)

#load ed attendances:
source("scripts/ed-attendances-load-data.R")

#we'll be looking at RIE only:
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

#finally calculate the lothian population for each period:
lothian_registerdPatients <- listSizes %>% filter(HB == "S08000024") %>%
  group_by(HB, Date) %>%
  summarise_at(vars(Both_Ages0to4:Male_AllAges), sum, na.rm = T) %>%
  ungroup()

#and save outputs:
saveRDS(rie_training, "./data/rie_training.rda")
saveRDS(rie_test, "./data/rie_test.rda")
saveRDS(lothian_registerdPatients, "./data/lothian_population.rda")
