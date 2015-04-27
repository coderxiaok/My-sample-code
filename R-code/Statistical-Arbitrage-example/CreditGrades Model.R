#write a function to handel the data input
CDS.model <- function(ticker, dir, startdate, enddate, L_bar, lamda, R, Time){
  
  #load the libraries
  library(quantmod)
  
  #download the stock price
  stock <- getSymbols(ticker, from = "1996-09-01", to = "2015-01-01", auto.assign= FALSE)
  stock_close <- stock[,4]
  names(stock_close) <- "sclose"
  stock_return <- (stock_close - lag(stock_close))/lag(stock_close)
  stock_return[1] <- as.numeric(stock_return[2])
  names(stock_return) <- "sreturn"
  
  #down risk-free rate data
  rf <- getSymbols("DGS5", src = "FRED", auto.assign = FALSE)
  rf <- rf * 0.01
  rf <- rf["1996-09-01/2015-01-01"]
  rf <- rf[!is.na(rf)]
  names(rf) <- "rf"
  
  #load the debt data from WRDS csv file and to compute the debt per share
  dir_temp <- paste(dir, "/compstat.csv", sep="")
  compstat <- read.csv(file = dir_temp)
  rm(dir_temp)
  #if 
  if(!(ticker %in% compstat$tic)){
    stopmassage <- paste("Can not find", ticker, "in compstat.csv file, 
                         please try another ticker or update the compstat.csv file from WRDS",
                         sep="")
    stop(stopmassage)
  }
  else{
    bls <- compstat[which(compstat$tic==ticker),]
    bls$dps <- bls$ltq / bls$cshoq
    bls$date <- NA
    for(i in 1:length(bls[,1])){
      qdate <- as.character(bls$datafqtr[i])
      y=substring(qdate,1,4)
      q=substring(qdate,6,6)
      if(q=="1"){
        m="01"
        d="01"
      }
      if(q=="2"){
        m="04"
        d="01"
      }
      if(q=="3"){
        m="07"
        d="01"
      }
      if(q=="4"){
        m="10"
        d="01"
      }
      temp_date <- paste(y, "-", m, "-", d, sep="")
      bls$date[i] <- temp_date
    }
    dps <- as.xts(bls$dps, order.by = as.Date(bls$date))
    names(dps) <- "dps"
  }
  
  #merge the data and convert debt data to daily
  data <- merge(stock_close, rf, dps, stock_return)
  data <- data["1996-09-01/2015-01-01"]
  data <- data[!is.na(data$sclose)]
  data <- data[!is.na(data$rf)]
  data$rf <- na.locf(data$rf)
  #if
  NonNAindex <- which(!is.na(data$dps))
  firstNonNA <- min(NonNAindex)
  if(is.na(data$dps[firstNonNA])){
    stop("dps data at first date is missing, please fix this problem!")
  }
  else{
    data$dps[1] = data$dps[firstNonNA]
    for(i in 2:length(data[,1])){
      if(is.na(data[i,3])){
        data[i,3] <- data[i-1,3]
      }
    }
  }
  
  #calculate the sigma using 1000 last day simple standard deviation
  data$sigma <- NA
  for(i in 1001:length(data[,1])){
    data$sigma[i] <- sqrt(252) * sd(data$sreturn[(i-1000):i])
  }
  data <- data[1001:length(data[,1])]
  
  #choose the duration compared to the market data
  data$CDS_model <- NA
  data$CDS_delta <- NA
  duration <- paste(startdate, "/", enddate, sep="")
  data <- data[duration]
  
  ################### CreditGrades Model Part #############################
  ##Capital Structure Arbitrage Team Project
  ##R code for CreditGrades Model
  ##Copyright @2015 Ming Tian
  
  ##Introduction: This document is a implementation of CreditGrades model in R
  
  #   ##Function1 Default Barrier, LD
  #   LD <- function(L_bar, D, lamda, n){
  #     #calculation
  #     temp <- L_bar * D * exp(lamda*rnorm(n) - 0.5*(lamda^2))
  #     #return result
  #     return(temp)
  #   }
  #   
  ##Function2 Survival Probability, q(t)
  q_t <- function(L_bar, D, lamda, S_0, S_t, sigma_St, t){
    #calculation
    d <- (S_0+L_bar*D)*exp(lamda^2)/(L_bar*D)
    A_t <- sqrt((sigma_St*S_t/(S_t+L_bar*D))^2 * t + lamda^2)
    temp <- pnorm((-A_t/2) + log(d)/A_t) - d*pnorm((-A_t/2) - log(d)/A_t)
    #return result
    return(temp)
  }
  
  ##Function3 G(u)
  ##this is just a function used in CDS spread calculation
  G <- function(L_bar, D, lamda, sigma_St, r, u, S_0, S_t){
    #Calculation
    d <- (S_0+L_bar*D)*exp(lamda^2)/(L_bar*D)
    sigma_At <- sigma_St*S_t/(S_t+L_bar*D)
    z <- sqrt(1/4 + 2*r/sigma_At^2)
    temp <- d^(z+1/2) * pnorm(-(log(d)/(sigma_At*sqrt(u)))-z*sigma_At*sqrt(u)) + 
      d^(-z+1/2) * pnorm(-(log(d)/(sigma_At*sqrt(u)))+z*sigma_At*sqrt(u))
    #Return result
    return(temp)
  }
  
  ##Function4 CDS spread for maturity T, CDS_0T
  CDS_0T <- function(L_bar, D, lamda, S_0, S_t, sigma_At, r, R, t, endt){
    #calculation
    q_0 <- q_t(L_bar, D, lamda, S_0, S_t, sigma_At, t)
    q_T <- q_t(L_bar, D, lamda, S_0, S_t, sigma_At, endt)
    sigma_At <- sigma_St*S_t/(S_t+L_bar*D)
    epsl <- lamda^2 / sigma_At^2
    H_T <- exp(r*epsl) * (G(L_bar, D, lamda, sigma_At, r, endt+epsl, S_0, S_t) - 
                            G(L_bar, D, lamda, sigma_At, r, epsl, S_0, S_t))
    temp <- r * (1-R) * (1-q_0+H_T)/(q_0-q_T*exp(-r*endt)-H_T)
    #return result
    return(temp*(10000))
  }
  
  ##Function5 CDS delta hedge ratio
  CDS_Delta <- function(L_bar, D, lamda, S_0, S_t, sigma_At, r, R, t, endt){
    #calculation
    q_0 <- q_t(L_bar, D, lamda, S_0, S_t, sigma_At, t)
    q_T <- q_t(L_bar, D, lamda, S_0, S_t, sigma_At, endt)
    sigma_At <- sigma_St*S_t/(S_t+L_bar*D)
    epsl <- lamda^2 / sigma_At^2
    H_T <- exp(r*epsl) * (G(L_bar, D, lamda, sigma_At, r, endt+epsl, S_0, S_t) - 
                            G(L_bar, D, lamda, sigma_At, r, epsl, S_0, S_t))
    CDS_diff <- (CDS_0T(L_bar, D, lamda, S_0, S_t*1.01, sigma_At, r, R, t, endt) - 
                   CDS_0T(L_bar, D, lamda, S_0, S_t, sigma_At, r, R, t, endt))
    temp <- CDS_diff/(0.01*S_t)
    #return result
    return(temp)
  }
  
  #   ##Function6 CDS contact value
  #   pi_0T <- function(L_bar, D, lamda, S_0, S_t, sigma_At, r, R, t, endt){
  #     #calculation
  #     c_ini <- CDS_0T(L_bar, D, lamda, S_0, S_t, sigma_At, r, R, 0, endt)
  #     c_t <- CDS_0T(L_bar, D, lamda, S_0, S_t, sigma_At, r, R, t, endt)
  #     q_0 <- q_t(L_bar, D, lamda, S_0, S_t, sigma_At, t)
  #     q_T <- q_t(L_bar, D, lamda, S_0, S_t, sigma_At, endt)
  #     sigma_At <- sigma_St*S_t/(S_t+L_bar*D)
  #     epsl <- lamda^2 / sigma_At^2
  #     H_T <- exp(r*epsl) * (G(L_bar, D, lamda, sigma_At, r, endt+epsl) - 
  #                             G(L_bar, D, lamda, sigma_At, r, epsl))
  #     temp <- (c_t - c_ini) / r * (q_0 - q_T*exp(-r*endt) - H_T)
  #     #return result
  #     return(temp)
  #   }
  
  
  ################### The Model part end here #############################
  
  #compute the therotical CDS spread from the CreditGrades model
  S_0 <- as.numeric(data$sclose[1])
  for(i in 1:length(data[,1])){
    D <- as.numeric(data$dps[i])
    S_t <- as.numeric(data$sclose[i])
    sigma_St <- as.numeric(data$sigma[i])
    r <- as.numeric(data$rf[i])
    endt <- Time
    data$CDS_model[i] <- CDS_0T(L_bar, D, lamda, S_0, S_t, sigma_St, r, R, 1/252, endt)
    data$CDS_delta[i] <- CDS_Delta(L_bar, D, lamda, S_0, S_t, sigma_St, r, R, 1/252, endt)
  }
  
  #return the result
  return(data)
}

# #test
# ticker="COG"
# dir="D:/Rdata/Stat_Arbi" 
# startdate="2010-03-11" 
# enddate="2015-03-11" 
# L_bar=0.5
# lamda=0.3
# R=0.3
# Time=5
# 
# test_CDS_model <- CDS.model(ticker, dir, startdate, enddate, L_bar, lamda, R, Time)
# plot(test_CDS_model$CDS_model)
# write.csv(test_CDS_model, file="D:/Rdata/Stat_Arbi/test/test.csv")
# dir_temp <- paste("D:/Rdata/Stat_Arbi/NEW data/", ticker, ".csv", sep="")
# marketcds <- read.csv(file = dir_temp)
# library(quantmod)
# marketcds <- as.xts(marketcds[,2], order.by = as.Date(marketcds[,1]))
# names(marketcds) = "MCDS"
# data_new <- merge(test_CDS_model, marketcds)
# data_new <- data_new[!is.na(data_new$CDS_model)]
# data_plot <- data_new[,c(6,8)]
# zoo.basket <- as.zoo(data_plot)
# tsRainbow <- rainbow(ncol(zoo.basket))
# plot(x = zoo.basket, ylab = "CDS spread/bps", main = "CDS spread",
#      col = tsRainbow, screens = 1)
