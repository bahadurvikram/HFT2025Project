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
source("functions/positionVB_double_new.R")

rm(data2)
data <- load_and_pre_process("data/data1_2024_Q2.RData")


# lets also fill every missing position with the most recent one

# lets create a pos_flat vector and fill it with 0s
pos_flat <- xts(rep(0, nrow(data)), index(data))

# we do not trade within the first 25 mins (9:30-9:55) 
# but also before that time since midnight
# and last 20 mins of the session (15:39-16:00)
# but also after this time until midnight

pos_flat["T00:00/T09:55"] <- 1
pos_flat["T15:39/T23:59"] <- 1

volat.sd = 120
mv.mean = 60
m_ = 3

for(volat.sd in c(60, 90, 120, 150, 180)) { # different volatility memories
  for (mv.mean in c(c(60, 90, 120, 150, 180))) {
  for(m_ in c( 2, 2.5, 3, 3.5)) { # different multipliers
    
    message(paste0("volat.sd = ", volat.sd,", m_ = ", m_)) 
    
    # calculating elements of the strategy
    signal <- data$NQ.close
    
    sma <- runmean(data, k = mv.mean, endrule = "NA", align = "right") 
    sda <- runsd(signal, volat.sd, endrule = "NA", align = "right")
    
    upper_entry <- sma + (m_+2) * sda
    lower_entry <- sma - (m_+2) * sda
    
    upper_exit <- sma + m_ * sda
    upper_exit <- sma - m_ * sda
  
    # position for mean-reverting strategy
    pos.mr <- positionVB_double_new(signal, lower_entry, lower_exit,
                                    upper_entry, upper_exit,
                                    pos_flat = pos_flat,
                                    strategy = "mr" # important !!!
    )
    
    # number of transactions
    ntrans <- abs(diff.xts(pos.mr))

    # gross pnl
    gross.pnl <- (pos.mr) *
      (diff.xts(NQ_price) * 20 # point value for NQ
       - coredata(data$av.ratio) * diff.xts(SP_price) * 50 ) # point value for SP
    
    # pnl after  costs
    net.pnl <- gross.pnl - ntrans * (12 + coredata(data$av.ratio) * 12)
    
    # aggregate to daily
    ends_ <- endpoints(data, "days")
    
    pnl.gross.d <- period.apply(gross.pnl, INDEX = ends_, 
                                FUN = function(x) sum(x, na.rm = TRUE))
    pnl.net.d <- period.apply(net.pnl, INDEX = ends_,
                              FUN = function(x) sum(x, na.rm = TRUE))
    ntrans.d <- period.apply(ntrans, INDEX = ends_, 
                             FUN = function(x) sum(x, na.rm = TRUE))
    
    # calculate summary measures
    gross.SR <- mySR(pnl.gross.d, scale = 252)
    
    net.SR <- mySR(pnl.net.d, scale = 252)
    
    gross.PnL <- sum(pnl.gross.d, na.rm = TRUE)
    
    net.PnL <- sum(pnl.net.d, na.rm = TRUE)
    
    
    av.daily.ntrans <- mean(ntrans.d, na.rm = TRUE)
    av.daily.ntrans2 <- mean(ntrans2.d, na.rm = TRUE) 
    
    # summary of a particular strategy
    summary_ <- data.frame(
                           volat.sd = volat.sd,
                           m = m_,
                           period = "2022-01-03 - 2022-03",
                           gross.SR,
                           net.SR,
                           gross.PnL,
                           net.PnL,
                           av.daily.ntrans,
                           stringsAsFactors = FALSE)
    
    # putting all summaries together
    
    if(!exists("summary.pair.trading")) summary.pair.trading <- rbind(summary_) else
      summary.pair.trading <- rbind(summary.pair.trading, summary_)
    
    # deleting working files not needed any more
    rm(gross.SR, net.SR,
       gross.PnL, net.PnL,
       av.daily.ntrans,
       pnl.gross.d, pnl.net.d, 
       ntrans.d,
       pnl.gross, pnl.net, 
       ntrans,
       pos.mr, ends_, summary_,
       signal, lower_entry, lower_exit, upper_entry, upper_exit)
    
  } # end of loop for m_
  } # end of loop for mean
} # end of loop for volatility  

max_sr <- summary.pair.trading[which.max(summary.pair.trading$net.SR),]
print("rows")
# first we create column of zero's


# and calculate position for every minute


# lets see number of transactions per day

ntrans4.mr_daily <- period.apply(data$ntrans4.mr,
                                 endpoints(x = data, "days"),
                                 FUN = function(x) sum(x, na.rm = TRUE) )

# definition of a new (user) function

mySR <- function(x, # x = series of returns
                 # scale parameter = Nt
                 scale) {
  sqrt(scale) * mean(coredata(x), na.rm = TRUE) / 
    sd(coredata(x), na.rm = TRUE)
  
} # end of definition

# number of minutes within a day - 
# we assumed no trading in the first and last 10 minutes
# so trading can be done between 9:40 and 15:50

# lets create a numeric object to store it:
(TradingMinsInDay <- (6.5 * 60 - 2 * 10))

# gross SR for the momentum strategy 
# based on one MA

mySR(x = data$pnl_gross1.mr, scale = 252 * TradingMinsInDay)

# quite positive

# if you prefer to use a function from tseries package
sharpe(cumsum(coredata(data$pnl_gross1.mr)),
       scale = sqrt(252 * TradingMinsInDay))

# net SR for the same strategy
mySR(data$pnl_net1.mr,
     scale = 252 * TradingMinsInDay)

maxdrawdown(cumsum(data$pnl_gross1.mr))
# CAUTION!
# we need to convert our data to numeric vector


sharpe_daily <- sapply(data.daily[, grep("pnl", names(data.daily))], # data
                       function(x) mySR(x, scale = 252) # function applied
) 

# values might slightly differ from the ones calculated for 1-minute data


# average daily no of transactions for all strategies

sapply(data.daily[, grep("ntrans", names(data.daily))], # data
       function(x) mean(x, na.rm = TRUE) # function applied
)



# definition of a new (user) function

myCalmarRatio <- function(x, # x = series of returns
                          # scale parameter = Nt
                          scale) {
  scale * mean(coredata(x), na.rm = TRUE) / 
    maxdrawdown(cumsum(x))$maxdrawdown
  
} # end of definition

# sample column 
myCalmarRatio(data.daily$pnl_net1.mr,
              scale = 252)

# all compared

calmar_daily <- sapply(data.daily[, grep("pnl", names(data.daily))], # data
                       function(x) myCalmarRatio(x, scale = 252) # function applied
)

calmar_daily

