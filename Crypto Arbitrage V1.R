library(crypto)
library(Quandl)
library(dplyr)
library(ggplot2)
library(cusum)
library(tidyverse)

# H = crypto_history(coin='btc')
# 
# z = crypto_exchanges(coin = 'btc',start_date = NULL, end_date = NULL, )
# 
# f = crypto_timeseries(coin = 'btc')
# 
# 
# Quandl.api_key('zoouLKJNuSAmm9_XWMJx')
# btc = Quandl('BITFINEX/BTCUSD', start_date='2018-07-29', end_date='2019-07-29')
# 
# Quandl('BCHARTS/BITFLYERUSD', start_date='2019-04-21', end_date='2019-04-21') 

# This 
# collects data specific to a certain bitcoin exchange

exchange_data <- list()

#exchanges <- c('LAKEUSD','OKCOINUSD','BITSTAMPUSD','KRAKENUSD') # These are the exchanges we want to focus on
exchanges <- c('COINSBANKUSD','OKCOINUSD','BITSTAMPUSD','KRAKENUSD')

for (i in exchanges){
  exchange_data[[i]] <- Quandl(paste0('BCHARTS/', i)) #paste0 combines stringes so we can iterate through list to collect 
  #relevent data
}

#Introduce loops to make the tasks less manual and tedious

data1 = data.frame(exchange_data[1])
data2 = data.frame(exchange_data[2])
data3 = data.frame(exchange_data[3])
data4 = data.frame(exchange_data[4])

#This allows us to quickly compare the closing prices in each market

plot(data1$COINSBANKUSD.Date,data1$COINSBANKUSD.Close, type = 'l')
lines(data2$OKCOINUSD.Date,data2$OKCOINUSD.Close,col='RED')
lines(data3$BITSTAMPUSD.Date,data3$BITSTAMPUSD.Close,col='Blue')
lines(data4$KRAKENUSD.Date,data4$KRAKENUSD.Close,col='Dark Green')


#Attempting comparison with BITFLYERUSD Market 

test_merge = dplyr::inner_join(data1,data2, by = c("COINSBANKUSD.Date" = "OKCOINUSD.Date"))
test_merge2 = dplyr::inner_join(data1,data3, by = c("COINSBANKUSD.Date" = "BITSTAMPUSD.Date"))
test_merge3 = dplyr::inner_join(data1,data4, by = c("COINSBANKUSD.Date" = "KRAKENUSD.Date"))

test_merge$diff = test_merge$COINSBANKUSD.Close - test_merge$OKCOINUSD.Close  
test_merge2$diff = test_merge2$COINSBANKUSD.Close - test_merge2$BITSTAMPUSD.Close
test_merge3$diff = test_merge3$COINSBANKUSD.Close - test_merge3$KRAKENUSD.Close

plot(test_merge$COINSBANKUSD.Date ,test_merge$diff, type = 'l', col = 'Dark Green', ylim = c(-150,100), 
     xlim=as.Date(c("2019-01-01", "2019-09-03")))

lines(test_merge2$COINSBANKUSD.Date,test_merge2$diff, col = 'Red')
lines(test_merge3$COINSBANKUSD.Date,test_merge3$diff, col = 'Blue')

# LAKEUSD was attempted at first but I saw a big difference between all the rest. 

#Look into CumSum from mean? This may help indicate which markets is reacting more quickly / which is slower
# Is there a trend? 

test_merge3$mean = mean(test_merge3$diff)
test_merge3$from_mean = test_merge3$diff - test_merge3$mean
test_merge3$cusum = cumsum(test_merge3$from_mean)

plot(test_merge3$BITFLYERUSD.Date, test_merge3$cusum, ylim = c(-400, 600))

#There isnt a strong indication that one market is faster/slower than the other. It changes which one 
#reacts first







