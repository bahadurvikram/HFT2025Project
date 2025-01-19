execute_strategies <- function(data) {
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
  
  
  # lets create a pos_flat vector and fill it with 0s
  pos_flat <- xts(rep(0, nrow(data)), index(data))
  
  # we do not trade within the first 25 mins (9:30-9:55) 
  # but also before that time since midnight
  # and last 20 mins of the session (15:39-16:00)
  # but also after this time until midnight
  
  pos_flat["T00:00/T09:55"] <- 1
  pos_flat["T15:39/T23:59"] <- 1
  
  volat.sd = 120
  m_ = 3
  
  for(volat.sd in c(60, 90, 120, 150, 180)) { # different volatility memories
    for(m_ in c(0.5, 1, 1.5, 2, 2.5, 3, 3.5)) { # different multipliers
      
      message(paste0("volat.sd = ", volat.sd,
                     ", m_ = ", m_)) 
      
      # calculating elements of the strategy
      NQ_price <- coredata(data$NQ.close)
      SP_price <- coredata(data$SP.close)
      
      signal <- coredata(data$spread_avratio)
      signal2 <- coredata(data$spread_sdsratio)
      
      upper <- m_ * runsd(signal, volat.sd, 
                          endrule = "NA", 
                          align = "right")
      lower <- -m_ * runsd(signal, volat.sd, 
                           endrule = "NA", 
                           align = "right")
      
      upper2 <- m_ * runsd(signal2, volat.sd, 
                           endrule = "NA", 
                           align = "right")
      lower2 <- -m_ * runsd(signal2, volat.sd, 
                            endrule = "NA", 
                            align = "right")
      
      # position for mean-reverting strategy
      pos.mr <- positionVB_new(signal, lower, upper,
                               pos_flat = pos_flat,
                               strategy = "mr" # important !!!
      )
      pos.mr2 <- positionVB_new(signal2, lower2, upper2,
                                pos_flat = pos_flat,
                                strategy = "mr" # important !!!
      )
      # number of transactions
      ntrans <- abs(diff.xts(pos.mr))
      ntrans2 <- abs(diff.xts(pos.mr2))
      
      # gross pnl
      gross.pnl <- (pos.mr) *
        (diff.xts(NQ_price) * 20 # point value for NQ
         - coredata(data$av.ratio) * diff.xts(SP_price) * 50 ) # point value for SP
      
      gross.pnl2 <- (pos.mr2) *
        (diff.xts(NQ_price) * 20  # point value for NQ
         - coredata(data$sds.ratio) * diff.xts(SP_price) * 50 ) # point value for SP
      
      # pnl after  costs
      # costs = 12$ for SP and 4$ for NQ = (4+m*12) in total
      # there is NO minus "-" in the costs - they are always positive !!!
      
      net.pnl <- gross.pnl - ntrans * (12 + coredata(data$av.ratio) * 12)
      net.pnl2 <- gross.pnl2 - ntrans2 * (12 + coredata(data$sds.ratio) * 12)
      
      # aggregate to daily
      ends_ <- endpoints(data, "days")
      
      pnl.gross.d <- period.apply(gross.pnl, INDEX = ends_, 
                                  FUN = function(x) sum(x, na.rm = TRUE))
      pnl.gross2.d <- period.apply(gross.pnl2, INDEX = ends_, 
                                   FUN = function(x) sum(x, na.rm = TRUE))
      pnl.net.d <- period.apply(net.pnl, INDEX = ends_,
                                FUN = function(x) sum(x, na.rm = TRUE))
      pnl.net2.d <- period.apply(net.pnl2, INDEX = ends_,
                                 FUN = function(x) sum(x, na.rm = TRUE))
      ntrans.d <- period.apply(ntrans, INDEX = ends_, 
                               FUN = function(x) sum(x, na.rm = TRUE))
      ntrans2.d <- period.apply(ntrans2, INDEX = ends_, 
                                FUN = function(x) sum(x, na.rm = TRUE))
      
      # calculate summary measures
      gross.SR <- mySR(pnl.gross.d, scale = 252)
      gross.SR2 <- mySR(pnl.gross2.d, scale = 252)
      net.SR <- mySR(pnl.net.d, scale = 252)
      net.SR2 <- mySR(pnl.net2.d, scale = 252)
      gross.PnL <- sum(pnl.gross.d, na.rm = TRUE)
      gross.PnL2 <- sum(pnl.gross2.d, na.rm = TRUE)
      net.PnL <- sum(pnl.net.d, na.rm = TRUE)
      net.PnL2 <- sum(pnl.net2.d, na.rm = TRUE)
      
      av.daily.ntrans <- mean(ntrans.d, na.rm = TRUE)
      av.daily.ntrans2 <- mean(ntrans2.d, na.rm = TRUE) 
      
      # summary of a particular strategy
      summary_ <- data.frame(spread = "spread_avratio",
                             volat.sd = volat.sd,
                             m = m_,
                             period = "2022-01-03 - 2022-03",
                             gross.SR,
                             net.SR,
                             gross.PnL,
                             net.PnL,
                             av.daily.ntrans,
                             stringsAsFactors = FALSE)
      
      summary2_ <- data.frame(spread = "spread_sdsratio",
                              volat.sd = volat.sd,
                              m = m_,
                              period = "2022-01-03 - 2022-03",
                              gross.SR = gross.SR2,
                              net.SR = net.SR2,
                              gross.PnL = gross.PnL2,
                              net.PnL = net.PnL2,
                              av.daily.ntrans = av.daily.ntrans2,
                              stringsAsFactors = FALSE)
      
      # putting all summaries together
      
      if(!exists("summary.pair.trading")) summary.pair.trading <- rbind(summary_, summary2_) else
        summary.pair.trading <- rbind(summary.pair.trading, summary_, summary2_)
      
      # deleting working files not needed any more
      rm(gross.SR, gross.SR2, net.SR, net.SR2,
         gross.PnL, gross.PnL2, net.PnL, net.PnL2,
         av.daily.ntrans, av.daily.ntrans2,
         pnl.gross.d, pnl.gross2.d, pnl.net.d, pnl.net2.d, 
         ntrans.d, ntrans2.d,
         pnl.gross, pnl.gross2, pnl.net, pnl.net2, 
         ntrans, ntrans2,
         pos.mr, pos.mr2, ends_, summary_, summary2_,
         NQ_price, SP_price,
         signal, signal2, lower, lower2, upper, upper2)
      
    } # end of loop for m_
  } # end of loop for volatility  
  
  max_sr <- summary.pair.trading[which.max(summary.pair.trading$net.SR),]
  print("rows")
  print(nrow(max_sr))
  if (nrow(max_sr)==0) {
    result <- c( spread.name=NA , net.SR=NA, m = NA, volat.sd = NA)
    return(result)
  }
  if (max_sr$spread=="spread_avratio") {
    sname<-1
  } else {
    sname<-2
  }
  result <- c( spread.name=sname , net.SR=as.double(max_sr$net.SR), m = as.double(max_sr$m), volat.sd = as.double(max_sr$volat.sd))
  

  return(result)
}