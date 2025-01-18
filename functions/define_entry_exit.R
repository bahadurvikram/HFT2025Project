define_entry_exit <- function(data2) {
  # data_file "data/data1_2022_Q1.RData"
  # data_set_name data1_2022_Q1
  require(xts)
  require(quantmod)
  require(chron)
  require(TTR)
  require(caTools)
  require(lubridate)
  require(dplyr) # for if_else()
  require(lattice) # for levelplot()
  require(grDevices)
  require(dplyr) # for if_else()
  ###################################################################
  # lets formulate a spread: P1 - m * P2 (P_NQ - m * P_SP) 
  # where m = m1/m2 is based on average ratio between the prices
  # on the PREVIOUS day
  
  # spread is a signal to our model, which shows whether to take 
  # position or not (volatility bands around the spread)
  
  # CAUTION! we assume the mean reverting behavior of the spread!
  
  ####################################################################
  # lets calculate average ratio of prices on the daily basis
  my.endpoints <- endpoints(data2, "days")
  
  av.ratio <- period.apply(data2,
                           INDEX = my.endpoints,
                           function(x) mean(x$NQ.close/x$SP.close, 
                                            na.rm = TRUE)
  )
  
  names(av.ratio) <- "av.ratio"
  
  # but calculations based on the first day, will be used on the second day, etc.
  # lets adjust the dataset accordingly by moving the time index to 9:30 of the next day
  # and in case of Friday move to 2 days 9 hours and 30 mins
  # lets apply the changes in our data object
  index(av.ratio) <- 
    ceiling_date(index(av.ratio), "day") + 
    hours(9) + 
    minutes(30) +
    if_else(wday(index(av.ratio)) == 6, 
            days(2),
            days(0))
  
  
  ###################################################################
  # alternative spread based on RETURNS:
  # r1 - ms * r2 (r_NQ - ms * r_SP) 
  # where ms = s1/s2 is based on the ratio of standard
  # deviations of returns on the PREVIOUS day
  
  sds.ratio <- period.apply(data2,
                            INDEX = my.endpoints,
                            function(x) sd(x$NQ.return, na.rm = TRUE) /
                              sd(x$SP.return, na.rm = TRUE)
  )
  
  
  names(sds.ratio) <- "sds.ratio"
  
  index(sds.ratio) <- 
    ceiling_date(index(sds.ratio), "day") +
    hours(9) + 
    minutes(30) +
    if_else(wday(index(sds.ratio)) == 6, 
            days(2), 
            days(0))
  #Merge all
  data3 <- merge(data2, av.ratio, sds.ratio)
  
  rm(data2, av.ratio, sds.ratio)
  
  # Filled the missing with last non-missing values
  data3$av.ratio <- na.locf(data3$av.ratio,na.rm = FALSE)
  data3$sds.ratio <- na.locf(data3$sds.ratio,na.rm = FALSE)
  
  # lets make sure that we exclude weekends from our data
  
  table(wday(data3))
  # there are no rows with 1 (Sunday) and 7 (Saturday)
  
  # now we can calculate the spread (in 2 variants)
  data3$spread_avratio <- 
    data3$NQ.close -
    data3$av.ratio * data3$SP.close
  
  data3$spread_sdsratio <- 
    data3$NQ.return - 
    data3$sds.ratio * data3$SP.return
  
  # Return the xts object
  return(data3)
  
}