
library(kableExtra)
library(quantmod)
library(dplyr)
library(zoo)
library(urca)
library(lubridate)
library(xts)
library(chron)
library(TTR)
library(knitr)
library(tseries)
library(PerformanceAnalytics)
library(caTools)
library(ggplot2)
library(xts)
library(quantmod)
library(TTR)
library(caTools)
library(roll)


# mySR function
mySR <- function(x, scale) {
  sqrt(scale) * mean(coredata(x), na.rm = TRUE) / 
    sd(coredata(x), na.rm = TRUE)
} 

# myCalm function
myCalmarRatio <- function(x,
                          scale) {
  scale * mean(coredata(x), na.rm = TRUE) / 
    maxdrawdown(cumsum(x))$maxdrawdown
  
}

setwd("C:/Users/User/Downloads/HFD project1/HFD project") # Pleaes set the local file location
source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_positionVB_new.R")
all_quarters_daily_data2 <- data.frame()
for (selected_quarter in c("2024_Q2")) {
  
  message("Running for quarter", selected_quarter)
  
  # Step 1. Load the data
  filename_ <- paste0("data/data2_", selected_quarter, ".RData")
  load(filename_)
  
  data.group2 <- get(paste0("data2_", selected_quarter))
  times_ <- substr(index(data.group2), 12, 19)
  
  
  # Ignore first and last 10 minutes of the session (9:31--9:40 and 15:51--16:00)
  # Fill missing values with NA
  data.group2["T09:31/T09:40",] <- NA 
  data.group2["T15:51/T16:00",] <- NA
  
  # Let's also assume we LEAVE ALL POSITIONS before the break starts.
  pos_flat <- xts(rep(0, nrow(data.group2)), index(data.group2))
  pos_flat["T16:46/T18:15"] <- 1
  
  # 3. Ignore weekends positions (Fri, 16:00 - Sun, 18:00).
  dweek_ <- wday(data.group2)
  head(dweek_)
  table(dweek_)
  time_ <- substr(index(data.group2), 12, 19)
  pos_flat[(dweek_ == 6 & times(time_) > times("16:00:00")) |
             (dweek_ == 7) |
             (dweek_ == 1 & times(time_) <= times("18:00:00")),] <- 1
  
  
  
  
  # let's calculate the average ratio for XAU, XAG and its spread
  my.endpoints <- endpoints(data.group2, "days")
  
  av.ratio.r1 <- period.apply(data.group2,
                                  INDEX = my.endpoints,
                                  function(x) mean(x$XAU/x$XAG, 
                                                   na.rm = TRUE)
  )
  
  index(av.ratio.r1) <- 
    ceiling_date(index(av.ratio.r1), "day") + 
    hours(0) + 
    minutes(0) +
    if_else(wday(index(av.ratio.r1)) == 6, 
            days(1),
            days(0))
  
  data.group2 <- merge(data.group2,
                       av.ratio.r1)
  
  data.group2$av.ratio.r1 <- na.locf(data.group2$av.ratio.r1,
                                         na.rm = FALSE)
  
  data.group2$spread_sdsratio <- 
    data.group2$XAU -
    data.group2$av.ratio.r1 * data.group2$XAG
  
  data.group2$spread_sdsratio_rollsd120 <- 
    runsd(data.group2$spread_sdsratio,                                    
          120, 
          endrule = "NA",
          align = "right")
  
  data.group2$spread_sdsratio_rollsd120[is.na(data.group2$XAG)] <- NA
  
  data.group2$upper <- 0.9 * data.group2$spread_sdsratio_rollsd120
  data.group2$lower <- (-0.9 * data.group2$spread_sdsratio_rollsd120)
  
  # let's calculate the position for the VB strategy of AUD
  pos_flat <- xts(rep(0, nrow(data.group2)), 
                  index(data.group2))
  pos_flat["T16:45/T18:15"] <- 1
  signalSMA <- 30
  slowSMA <- 90
  volat.sd <- 100
  m_ <- 1.25
  
  data.group2$pos.AUD <- positionVB_new(signal = roll_mean(na.locf(data.group2$AUD, na.rm = FALSE),
                                                    signalSMA),
                                        lower = roll_mean(na.locf(data.group2$AUD, na.rm = FALSE), 
                                                          slowSMA) - 
                                          m_ * runsd(na.locf(data.group2$AUD, na.rm = FALSE), 
                                                     volat.sd, 
                                                     endrule = "NA", 
                                                     align = "right"),
                                        upper = roll_mean(na.locf(data.group2$AUD, na.rm = FALSE), 
                                                    slowSMA) +
                                          m_ * runsd(na.locf(data.group2$AUD, na.rm = FALSE),
                                                     volat.sd, 
                                                     endrule = "NA", 
                                                     align = "right"),
                                        pos_flat = pos_flat,
                                        strategy = "mr")
  
  # let's calculate the position for the pair trading strategy of XAU and XAG
  data.group2$pos.XX <- positionVB_new(signal = data.group2$spread_sdsratio,
                                       lower = data.group2$lower,
                                       upper = data.group2$upper,
                                       pos_flat = pos_flat,
                                       strategy = "mr"
  )
  
  
  # calculating gross pnl - remember to multiply by the point value !!!!
  data.group2$pnl_gross.AUD <- data.group2$pos.AUD * diff.xts(data.group2$AUD) * 100000
  data.group2$pnl_gross.XX <- (data.group2$pos.XX) *
    (diff.xts(data.group2$XAU) * 100 -
       data.group2$av.ratio.r1 * diff.xts(data.group2$XAG)*5000)
  
  
  # number of transactions
  data.group2$ntrans.AUD <- abs(diff.xts(data.group2$pos.AUD))
  data.group2$ntrans.AUD[is.na(data.group2$ntrans.AUD)] <- 0
  data.group2$ntrans.XX <- abs(diff.xts(data.group2$pos.XX))
  data.group2$ntrans.XX[is.na(data.group2$ntrans.XX)] <- 0
  
  # net pnl
  data.group2$pnl_net.AUD <- data.group2$pnl_gross.AUD - data.group2$ntrans.AUD * 7
  data.group2$pnl_net.XX <- data.group2$pnl_gross.XX -
    data.group2$ntrans.XX * (12 + data.group2$av.ratio.r1 * 7)
  
  
  # aggregate pnls and number of transactions to daily
  my.endpoints <- endpoints(data.group2, "days")
  
  data.group2.daily <- period.apply(data.group2[,c(grep("pnl", names(data.group2)),
                                                   grep("ntrans", names(data.group2)))],
                                    INDEX = my.endpoints, 
                                    FUN = function(x) colSums(x, na.rm = TRUE))
  
  # Calculating gross and net pnls
  
  data.group2.daily$pnl_gross <- 
    data.group2.daily$pnl_gross.AUD +
    data.group2.daily$pnl_gross.XX
  
  data.group2.daily$pnl_net <- 
    data.group2.daily$pnl_net.AUD +
    data.group2.daily$pnl_net.XX
  
  # lets SUM number of transactions (with the same weights)
  
  data.group2.daily$ntrans <- 
    data.group2.daily$ntrans.AUD +
    data.group2.daily$ntrans.XX
  
  
  # summarize the strategy for this quarter
  
  # SR
  grossSR = mySR(x = data.group2.daily$pnl_gross, scale = 252)
  netSR = mySR(x = data.group2.daily$pnl_net, scale = 252)
  # CR
  grossCR = myCalmarRatio(x = data.group2.daily$pnl_gross, scale = 252)
  netCR = myCalmarRatio(x = data.group2.daily$pnl_net, scale = 252)
  
  # average number of transactions
  av.daily.ntrades = mean(data.group2.daily$ntrans, 
                          na.rm = TRUE)
  # PnL
  grossPnL = sum(data.group2.daily$pnl_gross)
  netPnL = sum(data.group2.daily$pnl_net)
  
  # stat
  stat = netCR * max(0, log(abs(netPnL/1000)))
  
  # collecting all statistics for a particular quarter
  
  quarter_stats <- data.frame(quarter = selected_quarter,
                              assets.group = 2,
                              grossSR,
                              netSR,
                              grossCR,
                              netCR,
                              av.daily.ntrades,
                              grossPnL,
                              netPnL,
                              stat,
                              stringsAsFactors = FALSE
  )
  
  # collect summaries for all quarters
  if(!exists("quarter_stats.all.group2")) quarter_stats.all.group2 <- quarter_stats else
    quarter_stats.all.group2 <- rbind(quarter_stats.all.group2, quarter_stats)
  
  # create a plot of gros and net pnl and save it to png file
  
  png(filename = paste0("pnl_group2_", selected_quarter, ".png"),
      width = 1000, height = 600)
  print( 
    plot(cbind(cumsum(data.group2.daily$pnl_gross),
               cumsum(data.group2.daily$pnl_net)),
         multi.panel = FALSE,
         main = paste0("Gross and net PnL for asset group 2 \n quarterly ", selected_quarter), 
         col = c("#9A2727", "#279A92"),
         major.ticks = "weeks", 
         grid.ticks.on = "weeks",
         grid.ticks.lty = 3,
         legend.loc = "topleft",
         cex = 1)
  )
  dev.off()
  
  rm(my.endpoints, grossSR, netSR,grossCR,netCR, av.daily.ntrades, 
     grossPnL, netPnL, stat, quarter_stats)
  
  gc()
  
  all_quarters_daily_data2 <- rbind(all_quarters_daily_data2, data.group2.daily)
  
} # end of the loop

write.csv(quarter_stats.all.group2, 
          "quarter_stats.all.group2.csv",
          row.names = FALSE)
print(quarter_stats.all.group2)

# Performance mearures
dates <- as.POSIXct(rownames(all_quarters_daily_data2), format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Remove invalid rows
valid_rows <- !is.na(dates) & !is.nan(dates) & is.finite(dates)


all_quarters_daily_data2_filtered <- all_quarters_daily_data2[valid_rows, ]
valid_dates <- dates[valid_rows]

# Creating xts object
all_quarters_daily_data2 <- xts(all_quarters_daily_data2_filtered, order.by = valid_dates)
chart_Series(cumsum(all_quarters_daily_data2$pnl_gross), name = "PnL for Group 2")
add_TA(cumsum(all_quarters_daily_data2$pnl_net), on = 1, col = "blue")

# summarize

# SR
grossSR = mySR(x = all_quarters_daily_data2$pnl_gross, scale = 252)
netSR = mySR(x = all_quarters_daily_data2$pnl_net, scale = 252)
# CR
grossCR = myCalmarRatio(x = all_quarters_daily_data2$pnl_gross, scale = 252)
netCR = myCalmarRatio(x = all_quarters_daily_data2$pnl_net, scale = 252)

# average number of transactions
av.daily.ntrades = mean(all_quarters_daily_data2$ntrans, 
                        na.rm = TRUE)
# PnL
grossPnL = sum(all_quarters_daily_data2$pnl_gross)
netPnL = sum(all_quarters_daily_data2$pnl_net)

# stat
stat = netCR * max(0, log(abs(netPnL/1000)))

overall_stat2 <- data.frame(grossSR, netSR,
                            grossCR, netCR,
                            av.daily.ntrades,
                            grossPnL, netPnL, stat)
print(overall_stat2)


