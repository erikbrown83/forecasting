#look at different methods of time series decomposition using the rie ed arrivals data
#from the nhs scotland open data portal

library(tidyverse)
library(lubridate)
library(fable)
library(feasts)
library(tsibble)

#load training data:
rie_training <- readRDS("./data/rie_training.rda")

#try decomposing the training data using the STL method:
#this assumes the trend, season, and remainder are additive:
stl_training <- rie_training %>% model(STL(arrivals))

#output is table containing the original dataset, along with the trend, season, and remaining components
components(stl_training)

#plot all 3 components:
components(stl_training) %>% autoplot() #looks to me like the remainder part is still quite large...

#overlay the trend onto the arrivals data:
rie_training %>%
  autoplot(arrivals, color='gray') +
  autolayer(components(stl_training), trend, color='red')
#evidence of several step changes in the number of arrivals and an overall upward trend

#and the seasonal-adjusted component
#this removes the seasonal component from the data
rie_training %>%
  autoplot(arrivals, color='gray') +
  autolayer(components(stl_training), season_adjust, color='red')

##classical time-series decomposition:
#the traditional way of estimating the trend component is to use the seasonal average
#for our purpose we'll use a 3-month window:
rie_training %>% mutate(movingAverage_3Month = slide_dbl(arrivals, mean, 
                                                         .size = 3, .align = "centre-left")) %>%
                  ggplot(aes(date, arrivals)) +
                  geom_line(colour = "grey") +
                  geom_line(aes(y = movingAverage_3Month), color='red')

#however as the data is strongly seasonal we'd have to use the moving average of the moving average:
#for monthly data we use a 2x12 moving average:
rie_training %>% mutate(movingAverage_3Month = slide_dbl(arrivals, mean, 
                                                         .size = 12, .align = "centre-left"),
                        movingAverage_MA = slide_dbl(movingAverage_3Month, mean,
                                                     .size = 2., .align = "centre-right")) %>%
  ggplot(aes(date, arrivals)) +
  geom_line(colour = "grey") +
  geom_line(aes(y = movingAverage_MA), color='red')
#which looks a bit more like the auto-generated component seen previously

#also possible to use a fable function:
rie_training %>% model(classical_decomposition(arrivals, type = "additive")) %>%
                 components() %>%
                 autoplot()

#can change type to multiplicative quite easily:
rie_training %>% model(classical_decomposition(arrivals, type = "multiplicative")) %>%
  components() %>%
  autoplot()
#in this case we end up with seasonal indexes rather than a figure to add/subtract

#a more complex decomposition method based on moving averages is the X11 method.
#this has a built in method for handling the effect of holidays
#and allows for the seasonal component to change over time:
x11_decomposition <- rie_training %>% model(x11 = feasts:::X11(arrivals, type = "multiplicative")) %>%
                                      components()

#results look like:
x11_decomposition %>% autoplot()

#the trend and seasonally-adjusted component:
x11_decomposition  %>%
  ggplot(aes(x = date_index)) +
  geom_line(aes(y = arrivals, colour = "Data")) +
  geom_line(aes(y = season_adjust, colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))

#can also check how the seasonal component has changed over time:
x11_decomposition %>% gg_subseries(seasonal)

#there is another method similar to x11 called SEATS
rie_training %>% model(seats = feasts:::SEATS(arrivals)) %>%
                 components() %>%
                 autoplot() #returns a scaled value rather than a multiplier (i.e. +500 instead of 1.0x)

#NOTE: X11 and SEATS only work on quarterly/monthly seasonal data
#not hourly/daily/weekly

#Seasonal and Trend decomposition using Loess (STL) can handle any level of seasonality (e.g. hourly)
#allows the seasonal component to change over time, and can be robust to outliers
#however the effect of holidays/trading days are not captured automatically and only additive decomposition is possible

rie_training %>% model(stl_default = STL(arrivals ~ trend(window = 21) + season(window = 13)),
                       stl_longer = STL(arrivals ~ trend(window = 41) + season(window = 25)),
                       stl_shorter = STL(arrivals ~ trend(window = 7) + season(window = "periodic"))
                       ) %>%
                 components(stl_shorter) %>%
                 autoplot()
#the trend and season windows control how rapidly the trend & season can change

##Time-series features:
#we can calculate features from a time series using the features() function:
rie_training %>% features(arrivals, mean)

#multiple features can be calculated like this:
rie_training %>% features(arrivals, list(mean = mean,
                                         min = min,
                                         max = max))

#features can be extracted from autocorrelations which tell us how much autocorrelation there is in a series:
rie_training %>% features(arrivals, feat_acf) %>% glimpse()
#the feasts package provides a helper function for this which returns many fifferent variations of autocorrelation
#including the first autocorrelation coefficient, the sum of the first 10 coefficients, 
#and the autocorrelation of the change from point to point

#STL features can also be extracted similarly:
rie_training %>% features(arrivals, feat_stl) %>% glimpse()
#the trend & seasonal strengths are between 0 and 1 where 1 represents the strong prescence of a trend/seasonality
#also returned are the periods with the highest/lowest values
#the variance and autocorrelation in the remainder values
#a measure of linear/curved the trend is
        
