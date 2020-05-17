#ed forecasts with transformations + decomposition

library(tidyverse)
library(lubridate)
library(fable)
library(feasts)
library(tsibble)

#load training data:
rie_training <- readRDS("./data/rie_training.rda")

#when using a transformatipn (e.g. box-cox) we first make a forecast using the
#transformed data and then back transform the results

#common transformations:
#log transforms which ensure the results remain positive
#logit transformations which ensure the forecast remains between 2 values a & b
#where f = log((x-a) / (b-x))

#to implement a custom transformation we define it as:
scaled_logit <- new_transformation(
                    transformation = function(x, lower = 0, upper = 1){
                      log((x - lower) / (upper - x))
                    },
                    inverse = function(x, lower=0, upper=1){
                      (upper-lower)*exp(x)/(1+exp(x)) + lower
                    }
                                  )

#apply to the arrivals dataset
scaled_forecast <- rie_training %>% model(SNAIVE(scaled_logit(arrivals, lower = 8000, upper = 12000) ~ lag("year") + drift()))

#then use those models to make 12-month forcasts:
rie_forecasts <- scaled_forecast %>% forecast(h = "12 months")

#create a plot to visualise:
rie_forecasts %>% autoplot(rie_training)

#bias adjustment is applied by default in fable...
#this means that the forecasted value always represents the mean of the 
#forecast distribution rather than the median (which can be the result of backward transformation)
#the difference between the mean and median is referred to as the bias (hence bias adjustment)

#forecast by decomposition is done in 2 steps:
#forecast the seasonal and seasonally adjusted components separately
#it is often the case that a naive seasonal forecast is used for the 
#the seasonal component:

decomposition <- rie_training %>% 
                  model(stl_default = STL(arrivals ~ trend(window = 3))) %>%
                  components() %>%
                  select(-.model)

decomposition %>%
  model(NAIVE(season_adjust)) %>%
  forecast() %>%
  autoplot(decomposition) + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")

#we can then add the seasonal component back in using a snaive model
#or we can this all in one step using fable:

stl_decomposition <- rie_training %>%
                      model(stlf = decomposition_model(STL(arrivals ~ trend(window = 3), robust = TRUE),
                                                       NAIVE(season_adjust))
                            )

stl_decomposition %>% 
  forecast() %>% 
  autoplot(rie_training)

#however this method normally result in significant autocorrelations in the 
#forecast residuals because the naive seasonal forecast does not fully
#capture the changing trend.
stl_decomposition %>% gg_tsresiduals()
