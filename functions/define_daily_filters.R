define_daily_filters <- function(data) {
  #######################################################################
  # lets add some filtering rules
  
  # Strategy setup:
  # lets assume we trade only if on the previous day:
  # - correlation was strong enough
  # - regression was strong enough
  # - cointegration was found
  
  # So similarly as last time, we need to do some 
  # calculation on a daily basis and use them on 
  # the next trading day
  ####################################################################
  require(xts)
  require(quantmod)
  require(chron)
  require(TTR)
  require(urca) # for cointegration tests
  require(caTools)
  require(lubridate)
  require(dplyr) # for if_else()
  require(lattice) # for levelplot()
  require(grDevices)
  require(dplyr) # for if_else()
  my.endpoints <- endpoints(data, "days")
  
  # regression coefficient between prices P1 and P2
  US.regP <- period.apply(data,
                          INDEX = my.endpoints,
                          function(x) coef(lm(NQ.close ~ SP.close - 1,
                                              data = x))
  )
  
  names(US.regP) <- "regP.b"
  

  ####################################################################
  # p-value of regression coefficient between P1 and P2
  
  # day by day
  US.regP.p <- period.apply(data,
                            INDEX = my.endpoints,
                            function(x) summary(lm(NQ.close~SP.close - 1,
                                                   data = x))$coefficients[1, 4]
  )
  
  names(US.regP.p) <- "regP.p"
  
  ####################################################################
  # regression coefficient between returns R1 and R2
  
  US.regR <- period.apply(data,
                          INDEX = my.endpoints,
                          function(x) coef(lm(NQ.return ~ SP.return - 1,
                                              data = x))
  )
  
  names(US.regR) <- "regR.b"
  
  ####################################################################
  # p-value of regression coefficient between R1 and R2
  
  lmcoef <- function(x) {
    coef <- summary(lm(NQ.return~SP.return - 1,
               data = x))$coefficients
    print(coef)
    if (nrow(coef)==0) {
      return (NA)
    }
    return (coef[1,4])
  } 
  
  US.regR.p <- period.apply(data,
                            INDEX = my.endpoints,
                            FUN = lmcoef
  )
  
  names(US.regR.p) <- "regR.p"
  

  ####################################################################
  # correlation coefficient between P1 and P2
  cor_loc <- function(x) {
    cortp <- cor(x$SP.close, 
                x$NQ.close,
                use = "complete.obs")
    print(cortp)
    if (is.null(cortp) || nrow(cortp)==0) {
      return (NA)
    }
    return (cortp)
  } 
  
  US.corP <- period.apply(data,
                          INDEX = my.endpoints,
                          FUN=cor_loc)
  
  names(US.corP) <- "corP"
  

  ####################################################################
  # correlation coefficient between R1 and R2
  
  US.corR <- period.apply(data,
                          INDEX = my.endpoints,
                          function(x) cor(x$SP.return, 
                                          x$NQ.return,
                                          use = "complete.obs"))
  
  names(US.corR) <- "corR"
  

  ####################################################################
  # cointegration test between P1 and P2
  # lets use KPSS test
  
  model.coint <- lm(NQ.close ~ SP.close - 1,
                    data = data)
  
  # for the whole sample
  kpss.test <- ur.kpss(model.coint$residuals, 
                       type = c("mu")) # constant deterministic component
  
  summary(kpss.test)
  
  # the KPSS test statistic (98.1446) higher than 
  # the 5% critical value (0.463)
  # so we REJECT the null about STATIONARITY
  
  # In KPSS if the test statistic is higher than the critical value, 
  # we reject the null hypothesis and when test statistic is lower 
  # than the critical value, we cannot reject the null hypothesis.
  
  # the test statistic
  kpss.test@teststat
  
  # lets store all 4 critical values
  kpss.test@cval
  
  # keep their names
  (cval.levels <- colnames(kpss.test@cval))
  
  # lets check it day by day
  
  # test statistic
  US.KPSS.stat <- period.apply(data,
                               INDEX = my.endpoints,
                               function(x) ur.kpss(lm(NQ.close ~ SP.close - 1,
                                                      data = x)$residuals,
                                                   type = c("mu"))@teststat)
  
  names(US.KPSS.stat) <- "KPSS.stat"
  

  # critical values
  US.KPSS.cval <- period.apply(data,
                               INDEX = my.endpoints,
                               function(x) ur.kpss(lm(NQ.close ~ SP.close - 1,
                                                      data = x)$residuals,
                                                   type = c("mu"))@cval)
  

  # lets assign names kept few steps before
  names(US.KPSS.cval) <- paste0("KPSS.cval.", cval.levels)
  

  # lets put all these results together 
  # (all are xts objects with a common index)
  
  US.daily.calc <- merge(US.regP, US.regP.p,
                         US.regR, US.regR.p,
                         US.corP, US.corR, 
                         US.KPSS.stat, US.KPSS.cval)
  
  US.daily.calc
  
  # but calculations based on the first day
  # will be used to filter on the second day, etc.
  
  # lets adjust the dataset accordingly
  # by moving the time index to 9:30 of the next day
  
  index(US.daily.calc)
  
  # we can do it similarly as last time
  # using functions from lubridate package:
  # - ceiling_date() rounds the date up to midnight
  #   (in fact start of the next day)
  # - hours(n), minutes(n) - create a period object
  #   with specified values
  
  index(US.daily.calc) <-
    ceiling_date(index(US.daily.calc),
                 "day") + 
    hours(9) + 
    minutes(30) +
    if_else(wday(index(US.daily.calc)) == 6, 
            days(2), 
            days(0))
  
  # and check the index after changes
  #index(US.daily.calc)
  
  # seems to be OK
  
  return(US.daily.calc)
}
  