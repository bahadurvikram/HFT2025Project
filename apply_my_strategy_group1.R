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

###############################################
# Install mandatory packages
###############################################
#install.packages("dplyr") # for if_else()
#install.packages("xts")
#install.packages("quantmod")
#install.packages("chron")
#install.packages("TTR")
#install.packages("caTools")
#install.packages("lubridate")
#install.packages("tseries") # for maxdrawdown()
#install.packages("urca") # for cointegration tests
# install.packages("lattice")
# install.packages("grDevices") # for colorRampPalette
library(xts)
library(quantmod)
library(chron)
library(urca)
library(TTR)
library(caTools)
library(lubridate)
library(tseries)
library(lattice)
library(grDevices)
library(dplyr)


## Get present location
LOC_CODE = dirname(rstudioapi::getSourceEditorContext()$path)

print(LOC_CODE)
## Set it as current working directory
setwd(LOC_CODE)

rm(list = ls())

###############################################
# Install dependent source code
###############################################
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/functions_plotHeatmap.R")
source("functions/load_and_pre_process.R")
source("functions/define_entry_exit.R")
source("functions/execute_strategies.R")
source("functions/define_daily_filters.R")
source("functions/execute_one_at_a_time.R")

final_results <- data.frame()

files_list <- c("2022_Q1", "2022_Q3", "2022_Q4", "2023_Q2", "2023_Q4", "2024_Q1", "2024_Q2") 

for (quarter_name in files_list) {
  ###############################################
  # Load data with calculated returns
  ###############################################
  file_name <- paste0("data/data1_", quarter_name, ".RData")
  data2 <- load_and_pre_process(file_name)
  
  ###############################################
  # Filtration parameters calculations for each day and moved to next day
  ###############################################
  # Calculating daily linear model regression coefficient,p values
  # correlation etc as regP.b, regP.p, regR.b, corP, corR, 
  # KPSS unit root test (KPSS)
  daily.calc <- define_daily_filters(data2)
  # remove the last row as it crossess the last quarter last day
  daily.calc <- daily.calc[-nrow(daily.calc)]
  
  ###############################################
  # Entry/Exit parameters calculations
  ###############################################
  #fid av.ratio, sds.ratio and respective spread for both prices and returns
  data2 <- define_entry_exit(data2)
  data2 <- data2[-nrow(data2)]
  
  
  ###############################################
  # Pair trading best parameters estimations
  ###############################################
  # Find the weekly best (based on max SR values) parameters for
  # a) volat.sd, b) m_, c) spread_name = 1 (spread_avratio) or 2 (spread_sdsratio) 
  # d) strategy_name = mr (default)
  # these will be need for next week trading decisions
  # FUN=execute_strategies iterate on weekly data to perform the task
  weeks_points <- endpoints(data2, "weeks")
  
  max_sr <- period.apply(data2, INDEX = weeks_points, FUN = execute_strategies)
  
  # q1 = 1, q2=14, q3=26, q4=40
  # find week number for running quarter 
  # as we need to exclude first week for every quarterly run
  # because we are not trading first week of every quarter
  first_week_of_quarter <- week(index(data))[1]
  
  message("First week of running quarter is ", first_week_of_quarter, " one of (1,14,26,40)")
  weekly_daily_index <- index(data2[week(index(data2))!=first_week_of_quarter])
  
  
  # set time from 16:00:00 to 9:30:00 by next two days
  index(max_sr) <- index(max_sr) + (2 * 24 * 60 * 60 + .75*24*60*60 - 60*30)
  
  # remove the last row as data moved by week ana created out of quarter row
  max_sr <- max_sr[-nrow(max_sr)]
  
  # form daily minute frequency result
  daily_best_sr <- merge(max_sr,weekly_daily_index)
  
  # Filled the missing with last non-missing values
  daily_best_sr$spread.name <- na.locf(daily_best_sr$spread.name)
  daily_best_sr$m <- na.locf(daily_best_sr$m)
  daily_best_sr$volat.sd <- na.locf(daily_best_sr$volat.sd)
  daily_best_sr$net.SR <- na.locf(daily_best_sr$net.SR)
  
  # Add the daily best SR based parameters to main data
  data2 <- merge(data2, daily_best_sr)
  
  ###############################################
  # Execution of trading
  ###############################################
  # FUN=execute_one_at_a_time calculate the daily values
  # for various ask, like gross pnl, net pnl, ntrans etc
  day_points <- endpoints(data2, "days")
  daily.pnls.xts <- period.apply(data2, INDEX = day_points, 
                         FUN = execute_one_at_a_time)
  
  # we need to remove hours from this index
  index(daily.pnls.xts) <- as_date(index(daily.pnls.xts))
  
  index(daily.calc) <- as_date(index(daily.calc))
  
  # then we can apply merging
  daily.pnls.xts <- merge(daily.pnls.xts, daily.calc)
  
  ###############################################
  # Collecting performance values
  ###############################################
  
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
  
  ############################################################
  # applying filteration of cointegration
  ############################################################
  # apply only if net pnl is negative
  if (net_cumPnL < 0) {
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
    
    # pnl plot
    chart_Series(cumsum(gross1_filtered), 
                 theme = myTheme)
    add_TA(cumsum(net1_filtered[-1]), 
           col = "red", 
           on = 1)
    
  } # end of kpss testing
  
  #𝑠𝑡𝑎𝑡=𝑛𝑒𝑡𝐶𝑅∗𝑚𝑎𝑥(0,𝑙𝑜𝑔(𝑎𝑏𝑠(𝑛𝑒𝑡.𝑃𝑛𝐿/1000)))
  stat_ <- ann_net_cr * max(0, log(abs(as.numeric(net_cumPnL)/1000)))

  final_results <- rbind(final_results, 
                         data.frame("Quarter Name" = quarter_name, "Gross SR" = ann_gross_sr, "Net SR" = ann_net_sr, 
                                    "Gross CR" = ann_gross_cr, "Net CR" = ann_net_cr, 
                                    "Gross cumP&L" = as.numeric(gross_cumPnL), 
                                    "Net cumP&L" = as.numeric(net_cumPnL),
                                    "Av.ntrades" = av.daily.ntrans,
                                    "Stat" = stat_))
  #Clean up variables for next run
  rm(data2, daily.calc, weeks_points, max_sr, first_week_of_quarter, weekly_daily_index,
     daily_best_sr, day_points, daily.pnls.xts, av.daily.ntrans, ann_gross_sr,
     ann_net_sr, ann_gross_cr,ann_net_cr,gross_cumPnL, net_cumPnL, stat_)

}
# Save the final results dataframe as CSV
write.csv(final_results, "final_results.csv")


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

