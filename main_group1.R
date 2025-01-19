##############################################################
# Project - group 1                                            #
##############################################################
# 
# period of 2022-01 – 2024-12 and is divided in 12 quarterly files.
# given for 2022Q1, 2022Q3, 2022Q4, 2023Q2, 2023Q4, 2024Q1 and 2024Q2
# data of in xts format in 1 minute frequency 
#
# Group 1 – two assets (1 min frequency, traded during NYSE sessions - on working days between 9:30 and 16:00 CET):
#  SP – futures contract for S&P 500 index (transaction cost = 12$, point value = 50$).
#  NQ – futures contract for NASDAQ index (transaction cost = 12$, point value = 20$).

# Common assumptions for group 1:
# do not use in calculations the data from the first and last 10 minutes of the session (9:31-9:40 and 15:51-16:00) – put missing values there,
# do not hold positions overnight (exit all positions 20 minutes before the session end, i.e. at 15:40),
# do not trade within the first 25 minutes of stocks quotations (9:31-9:55), but DO use the data for 9:41-9:55 in calculations of signal, volatility, etc.


#

#install.packages("dplyr")
#install.packages("xts")
#install.packages("quantmod")
#install.packages("chron")
#install.packages("TTR")
#install.packages("caTools")
#install.packages("lubridate")
library(xts)
library(quantmod)
library(chron)
library(urca) # for cointegration tests
library(TTR)
library(caTools)
library(lubridate)
#install.packages("tseries") # for maxdrawdown()
library(tseries)
# install.packages("dplyr")
library(dplyr) # for if_else()

# install.packages("lattice")
library(lattice) # for levelplot()

# install.packages("grDevices") # for colorRampPalette
library(grDevices)
library(dplyr) # for if_else()


## Get present location
LOC_CODE = dirname(rstudioapi::getSourceEditorContext()$path)

print(LOC_CODE)
## Set it as current working directory
setwd(LOC_CODE)

rm(list = ls())

source("https://raw.githubusercontent.com/ptwojcik/HFD/master/functions_plotHeatmap.R")

source("functions/load_and_pre_process.R")
source("functions/define_entry_exit.R")
source("functions/execute_strategies.R")
source("functions/define_daily_filters.R")
source("functions/execute_one_at_a_time.R")


data2 <- load_and_pre_process("data/data1_2024_Q2.RData")
daily.calc <- define_daily_filters(data2)
daily.calc <- daily.calc[-nrow(daily.calc)]

#fid av.ratio, sds.ratio and respective spread for both prices and returns
data2 <- define_entry_exit(data2)
data2 <- data2[-nrow(data2)]


#start_date <- as.Date(index(data2)[1])
#end_date <- start_date+20

#rm(test_data)
#test_data <- data2[paste0(start_date, "/", end_date)]

weeks_points <- endpoints(data2, "weeks")

rm(max_sr)

max_sr <- period.apply(data2, INDEX = weeks_points, 
                            FUN = execute_strategies)

#last_date <- end(data2)
#max_sr <- org_sr
#org_sr <- max_sr
#max_sr <- max_sr[wday(index(max_sr)) == 6]

# q1 = 1, q2=14, q3=26, q4=40

excl_first_week_index <- index(data2[week(index(data2))!=14]) # need to check for other quarters


#index(max_sr) <- index(org_sr) + 3 * 16.5 * 60 * 60
index(max_sr) <- index(max_sr) + (2 * 24 * 60 * 60 + .75*24*60*60 - 60*30)


#first_date <-start(max_sr)

max_sr <- max_sr[-nrow(max_sr)]


#daily_dates <- seq(from = first_date, to = last_date, by = "min")
test1 <- merge(max_sr,excl_first_week_index)

# Filled the missing with last non-missing values
test1$spread.name <- na.locf(test1$spread.name)
test1$m <- na.locf(test1$m)
test1$volat.sd <- na.locf(test1$volat.sd)
test1$net.SR <- na.locf(test1$net.SR)

data2 <- merge(data2, test1)

#data2["T09:30/T09:40",] <- NA 
#data2["T15:51/T16:00",] <- NA

day_points <- endpoints(data2, "days")

rm(daily.pnls.xts)

daily.pnls.xts <- period.apply(data2, INDEX = day_points, 
                       FUN = execute_one_at_a_time)

# we need to remove hours from this index too
index(daily.pnls.xts) <- as_date(index(daily.pnls.xts))

index(daily.calc) <- as_date(index(daily.calc))

# then we can apply merging
daily.pnls.xts <- merge(daily.pnls.xts, daily.calc)




############################################################
# applying filteration of cointegration
############################################################

# lets calculate the gross and net SR 
# for strategy using spread 1
# without filtering
# (exlude 1st day, as there was not trading yet)

av.daily.ntrans <- mean(daily.pnls.xts$ntrans, na.rm = TRUE)

ann_gross_sr <- mySR(daily.pnls.xts$pnl.gross[-5],
     scale = 252)

ann_net_sr <- mySR(daily.pnls.xts$pnl.net[-5], 
     scale = 252)
  
myCalmarRatio <- function(x, # x = series of returns
                          # scale parameter = Nt
                          scale) {
  scale * mean(coredata(x), na.rm = TRUE) / 
    maxdrawdown(cumsum(x))$maxdrawdown
  
} # end of definition

# sample column 
ann_gross_cr <- myCalmarRatio(daily.pnls.xts$pnl.gross[!is.na(daily.pnls.xts$pnl.gross)],
              scale = 252)

ann_net_cr <-myCalmarRatio(daily.pnls.xts$pnl.net[!is.na(daily.pnls.xts$pnl.net)],
              scale = 252)

gross_cumPnL <- tail(cumsum(daily.pnls.xts$pnl.gross[!is.na(daily.pnls.xts$pnl.gross)]),1)
net_cumPnL <- tail(cumsum(daily.pnls.xts$pnl.net[!is.na(daily.pnls.xts$pnl.net)]),1)

# pnl plot

# lets see it on the plot
myTheme <- chart_theme()
myTheme$col$line.col <- "darkblue"

par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))  # This sets the margins

chart_Series(cumsum(daily.pnls.xts$pnl.gross[!is.na(daily.pnls.xts$pnl.gross)]), 
             theme = myTheme)
par(mfrow = c(1, 1))  # This resets the margins


add_TA(cumsum(daily.pnls.xts$pnl.net[!is.na(daily.pnls.xts$pnl.net)]), 
       col = "red", 
       on = 1)
  
# lets check the KPSS.stat vs critical values
chart_Series(daily.pnls.xts$KPSS.stat, 
             theme = myTheme)
add_TA(daily.pnls.xts$KPSS.cval.1pct, 
       col = "red", 
       on = 1)
add_TA(daily.pnls.xts$KPSS.cval.2.5pct, 
       col = "darkgreen", 
       on = 1)
add_TA(daily.pnls.xts$KPSS.cval.5pct, 
       col = "orange", 
       on = 1)
add_TA(daily.pnls.xts$KPSS.cval.10pct, 
       col = "brown",
       on = 1)
par(mfrow = c(1, 1))  # This resets the margins

# KPSS.stat is only two times below the 1% critical value, 
# so cointegration is found very very rarely

# lets calculate SR for cointegration filtered data

# if cointergration => trade (take calculated pnl),
# otherwise don't (take 0)

# apply only if net pnl is negative
if (net_cumPnL < 0) {

  gross1_filtered <- ifelse(daily.pnls.xts$KPSS.stat <
                            daily.pnls.xts$KPSS.cval.5pct,
                          daily.pnls.xts$pnl.gross, 0)

  net1_filtered <- ifelse(daily.pnls.xts$KPSS.stat <
                          daily.pnls.xts$KPSS.cval.5pct,
                        daily.pnls.xts$pnl.net, 0)
  gross1_filtered[is.na(gross1_filtered)] <- 0
  net1_filtered[is.na(net1_filtered)] <- 0
  
  mySR(gross1_filtered[-1],
       scale = 252)
  
  mySR(net1_filtered[-1], 
       scale = 252)
  }



# replace every missing value with 0)



# pnl plot
chart_Series(cumsum(gross1_filtered), 
             theme = myTheme)
add_TA(cumsum(net1_filtered[-1]), 
       col = "red", 
       on = 1)

########################################
# Preparing performance matrix
#########################################
# gross SR – annualized Sharpe ratio based on gross daily P&L (without transaction costs, denoted in monetary terms),
# net SR – annualized Sharpe ratio based on net daily P&L (with transaction costs included, denoted in monetary terms),
# gross CR – annualized Calmar ratio based on gross daily P&L (without transaction costs, denoted in monetary terms),
# net CR – annualized Calmar ratio based on net daily P&L (with transaction costs included, denoted in monetary terms),
# gross cumP&L – cumulative profit and loss at the end of the investment period (last value of the cumP&L series) without transaction costs, denoted in monetary terms,
# net cumP&L – cumulative profit and loss at the end of the investment period (last value of the cumP&L series) with transaction costs included, denoted in monetary terms,
# av.ntrades – average daily number of trades.

