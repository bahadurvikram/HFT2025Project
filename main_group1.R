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
rm(list = ls())

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

source("https://raw.githubusercontent.com/ptwojcik/HFD/master/functions_plotHeatmap.R")

source("functions/load_and_pre_process.R")
source("functions/define_entry_exit.R")
source("functions/execute_strategies.R")
source("functions/define_daily_filters.R")
source("functions/execute_one_at_a_time.R")


data2 <- load_and_pre_process("data/data1_2022_Q1.RData")
daily.calc <- define_daily_filters(data2)
data2 <- define_entry_exit(data2)
summary.pair.trading <- execute_strategies(data2)

#options(digits = 9)
# 25 av.ratio       90 3 2022-01-03 - 2022-03 5.899015 5.327766  71275.63 64315.87        2.092308
max_sr <- summary.pair.trading[which.max(summary.pair.trading$net.SR),]
if (max_sr["spread"]=='av.ratio') { spread_name <- "spread_avratio"}
if (max_sr["spread"]=='sds.ratio') { spread_name <- "spread_sdsratio"}
volat.sd <- max_sr["volat.sd"]
m <- as.numeric(max_sr["m"])

daily.pnls.xts <- execute_one_at_a_time(data2, volat.sd, m, spread_name, "mr")


# lets put it together with daily filtering calculations
#index(daily.calc)
#index(daily.pnls.xts)
# we need to remove hours from this index too
index(daily.calc) <- as_date(index(daily.calc))

# then we can apply merging
daily.pnls.xts <- merge(daily.pnls.xts, daily.calc)



# lets see the results on the heatmap graph

# net.SR - spread av_ratio
plotHeatmap(data_plot = summary.pair.trading[summary.pair.trading$spread == "av.ratio",], # dataset (data.frame) with calculations
            col_vlabels = "volat.sd", # column name with the labels for a vertical axis (string)
            col_hlabels = "m", # column name with the labels for a horizontal axis (string)
            col_variable = "net.SR", # column name with the variable to show (string)
            main = "Sensitivity analysis for pair trading - spread based on prices ratio")

# net.Pnl - spread av_ratio
plotHeatmap(data_plot = summary.pair.trading[summary.pair.trading$spread == "av.ratio",], # dataset (data.frame) with calculations
            col_vlabels = "volat.sd", # column name with the labels for a vertical axis (string)
            col_hlabels = "m", # column name with the labels for a horizontal axis (string)
            col_variable = "net.PnL", # column name with the variable to show (string)
            main = "Sensitivity analysis for pair trading - spread based on prices ratio")


# av.daily.ntrans
plotHeatmap(data_plot = summary.pair.trading[summary.pair.trading$spread == "av.ratio",], # dataset (data.frame) with calculations
            col_vlabels = "volat.sd", # column name with the labels for a vertical axis (string)
            col_hlabels = "m", # column name with the labels for a horizontal axis (string)
            col_variable = "av.daily.ntrans", # column name with the variable to show (string)
            main = "Sensitivity analysis for pair trading - spread based on prices ratio",
            label_size = 4)

############################################################
# applying filteration of cointegration
############################################################

# lets calculate the gross and net SR 
# for strategy using spread 1
# without filtering
# (exlude 1st day, as there was not trading yet)
  
mySR(daily.pnls.xts$pnl.gross[-1],
     scale = 252)

mySR(daily.pnls.xts$pnl.net[-1], 
     scale = 252)
  

# pnl plot

# lets see it on the plot
myTheme <- chart_theme()
myTheme$col$line.col <- "darkblue"

par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))  # This sets the margins

chart_Series(cumsum(daily.pnls.xts$pnl.gross[-1]), 
             theme = myTheme)
par(mfrow = c(1, 1))  # This resets the margins


add_TA(cumsum(daily.pnls.xts$pnl.net[-1]), 
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

gross1_filtered <- ifelse(daily.pnls.xts$KPSS.stat <
                            daily.pnls.xts$KPSS.cval.1pct,
                          daily.pnls.xts$pnl.gross1, 0)

net1_filtered <- ifelse(daily.pnls.xts$KPSS.stat <
                          daily.pnls.xts$KPSS.cval.1pct,
                        daily.pnls.xts$pnl.net1, 0)


# replace every missing value with 0)

gross1_filtered[is.na(gross1_filtered)] <- 0
net1_filtered[is.na(net1_filtered)] <- 0

mySR(gross1_filtered[-1],
     scale = 252)

mySR(net1_filtered[-1], 
     scale = 252)

# pnl plot
chart_Series(cumsum(gross1_filtered[-1]), 
             theme = myTheme)
add_TA(cumsum(net1_filtered[-1]), 
       col = "red", 
       on = 1)
