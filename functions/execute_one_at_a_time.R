execute_one_at_a_time <- function(data) {
  #spread_name like spread_avratio or spread_sdsratio
  #strategy_name like mr or mom
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
  source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_mySR.R")
  source("https://raw.githubusercontent.com/ptwojcik/HFD/master/function_positionVB_new.R")
  
  
  
  all_na <- all(is.na(data$m)) # Assuming we are checking the first column
  #message("all na" , all_na)
  # Print the result
  if (all_na) { #ntrans = as.numeric(ntrans)
    result1 <- c(pnl.gross = NA, pnl.net = NA, ntrans = NA)
    #result1 <- c(a="a")
    #message("result " , result1)
    return (result1)
  }
  temp <- which(!is.na(data$m))[1]
  #message("temp ", data[temp,]$m)
  
  
  # lets create a pos_flat vector and fill it with 0s
  pos_flat <- xts(rep(0, nrow(data)), index(data))

  # we do not trade within the first 25 mins (9:30-9:55) 
  # but also before that time since midnight
  # and last 20 mins of the session (15:39-16:00)
  # but also after this time until midnight
  
  pos_flat["T00:00/T09:55"] <- 1
  pos_flat["T15:39/T23:59"] <- 1
  
  # setting the current parameters
  #volat.sd <- coredata(data$volat.sd)[1]
  volat.sd <- as.numeric(data[temp,]$volat.sd)
  #m_ <- coredata(data$m)[1]
  m_ <- as.numeric(data[temp,]$m)
  #spread_name <- coredata(data$spread.name)[1]
  spread_name <- as.numeric(data[temp,]$spread.name)
  strategy_name = "mr"
  message(paste0("\n volat.sd = ", volat.sd, ", m_ = ", m_, " spread_name = ", spread_name, " strategy_name = ", strategy_name)) 
  if (spread_name==1) {
    sname <- "spread_avratio"
  } else {
    sname<-"spread_sdsratio"
  }
  # calculating elements of the strategy
  NQ_price <- coredata(data$NQ.close)
  SP_price <- coredata(data$SP.close)
  
  signal <- coredata(data[,sname])
  #print(class(m_))
  #print(length(runsd(signal, volat.sd, endrule = "NA", align = "right")))
  
  upper <- m_ * runsd(signal, volat.sd, 
                      endrule = "NA", align = "right")
  lower <- -m_ * runsd(signal, volat.sd, 
                       endrule = "NA", align = "right")
  
  # position for MEAN-REVERTING strategy
  # spread1
  pos.mr <- positionVB_new(signal, lower, upper,
                           pos_flat = coredata(pos_flat),
                           strategy = strategy_name) # important !!!
  
  # number of transactions
  ntrans <- abs(diff.xts(pos.mr))
  
  # gross pnl
  gross.pnl <- (pos.mr) *
    (diff.xts(NQ_price) * 20 # point value for NQ
     - coredata(data$av.ratio) * diff.xts(SP_price) * 50 ) # point value for SP
  
  # pnl after  costs
  # costs = 12$ for AAPL and 4$ for NASDAQ = (4+m*12) in total
  # there is NO minus "-" in the costs - they are always positive !!!
  net.pnl <- gross.pnl - ntrans * (12 + coredata(data$av.ratio) * 12)
  
  # aggregate to daily
  ends_ <- endpoints(data, "days")
  
  pnl.gross.d <- period.apply(gross.pnl, INDEX = ends_, 
                              FUN = function(x) sum(x, na.rm = TRUE))
  
  pnl.net.d <- period.apply(net.pnl, INDEX = ends_,
                            FUN = function(x) sum(x, na.rm = TRUE))
  
  # CAUTION!!! daily objects are not xts objects!!!
  #str(pnl.gross.d)
  
  # we operated on simple vectors to speed-up
  # the calculations and in this case the results
  # of period.apply are also just column vectors!
  
  # lets collect daily.pnls into a data.frame
  
  #daily.pnls <- data.frame(pnl.gross = as.numeric(pnl.gross.d),
   #                        pnl.net = as.numeric(pnl.net.d))
  
  # but we have the endpoints of days
  #days_ <- index(data)[ends_]
  
  #head(days_)
  
  # the as_date() function will cut-out the time part
  #(days_ <- as_date(days_))
  
  # now we can transform our results to xts object
  #daily.pnls.xts <- xts(daily.pnls, days_)
  
  # lets delete objects that are not needed any more
  #print(pnl.gross.d)
  #print(pnl.net.d)
  #print(sum(ntrans, na.rm = TRUE))
  
  result <- c(pnl.gross = as.numeric(pnl.gross.d),
              pnl.net = as.numeric(pnl.net.d), ntrans = as.numeric(sum(ntrans, na.rm = TRUE)))
  
  rm(gross.pnl, net.pnl, 
     pnl.gross.d,  pnl.net.d, 
     ntrans,
     pos.mr, ends_,
     NQ_price, SP_price,
     signal,  lower, upper)
  
  return (result)
}