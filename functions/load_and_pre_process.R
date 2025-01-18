load_and_pre_process <- function(data_file) {
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
  
  env <- new.env()  # Create a new environment to load the data into
  load(data_file, envir = env)  # Load the file into the new environment
  
  # Identify xts object in the environment
  xts_objects <- Filter(function(obj) inherits(obj, "xts"), mget(ls(env), envir = env))
  data <- xts_objects[[1]]
  
  # based on the close column 
  # lets calculate log-returns for all series
  # in terms of basis points (bps) 1bps = 0.01% = 0.0001
  
  data.r <- 10000*diff.xts(log(data))
  
  data2 <-merge(data, data.r)
  
  rm(data, data.r)
  
  names(data2)[1:4] <- c("NQ.close","SP.close","NQ.return", "SP.return")
  
  data2["T09:31/T09:40",] <- NA 
  data2["T15:51/T16:00",] <- NA
  return (data2)
}