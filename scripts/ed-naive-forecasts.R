#try out some simple forecast methods on the ed arrivals dataset:

library(tidyverse)
library(lubridate)
library(fable)
library(feasts)
library(tsibble)

#load training data:
rie_training <- readRDS("./data/rie_training.rda")

#create several possible time series models
rie_models <- rie_training %>% model(Mean = MEAN(arrivals), #forecast is the average value of all previous values
                       `Naïve` = NAIVE(arrivals), #forecast is the last value
                       `Seasonal naïve` = SNAIVE(arrivals ~ lag("year")), #forecast is the last seasonal value
                       Drift = SNAIVE(arrivals ~ lag("year") + drift()) #seasonal naive with a drift value (equivalent to the average trend)
                       )

#then use those models to make 12-month forcasts:
rie_forecasts <- rie_models %>% forecast(h = "12 months")

#create a plot to visualise:
rie_forecasts %>% autoplot(rie_training, level = NULL)
#these methods can be used as benchmarks for other forecast models

#we can apply the model coefficients back on to the training dataset to test
#whether all information in the data has been captured:
rie_diagnostics <- rie_models %>% augment()

#a good forecast method willyield residulas which are:
#1) uncorrelated. correlated residuals indicate that there is information left in the data which could be used to improve the forecast
#2) have a mean of zero. a mean other than zero indicates that the forecast is biased.

#plot the residuals for the various methods:
rie_diagnostics %>% ggplot(aes(x = date_index, y = .resid, group = .model, colour = .model)) +
                      geom_line() +
                      facet_wrap(~.model, ncol = 1)

#and as a histogram:
rie_diagnostics %>% ggplot(aes(x = .resid)) +
                      geom_histogram() +
                      facet_wrap(~.model, ncol = 2)
#looks like only the seasonal naive + drift models are not biased...
#which makes sense because the time series has a strong trend element 
#the residuals for the drift value look normal but the seasonal naive is slightly skewed

#what about autocorrelations?
rie_diagnostics %>% ACF(.resid) %>%
                    autoplot()

#the residuals for the mean and naive methods seems to show a fair bit of autocorrelation indicating the 
#forecasts are not describing the data completely...
#we can make this assertion more formally using a ljung-box test:
rie_diagnostics %>% features(.resid, c(box_pierce, ljung_box),
                             lag = 6, #we set this to 2 x the seasonal period (guess this is quarterly)
                             K = 1 #this represents the degrees of freedom in each model (they are naive so have only used 1)
                             )
#these tests indicate that none of these methods are distinguishable from white noise
#what are these tests looking at?
#they are both called portmanteau tests which are used when there are many unknown
#ways that a model may depart from a known outcome and gives us a check of how well
#the data fits are expectations. in this case we are testing that that the autocorrelations
#of the forecast residuals are on average equal to 0.  a significant result here
#would be a low value for the bp/lb statistic with a significant p-value



