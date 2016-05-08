
###############################################################################
## Helper Funnctions for Bollinger Pops
#############################################################################

library(plyr)
library(quantmod)

init<-function(){
  symbols = read.csv("symbols.csv")
  return(symbols)
}


getBollingerBands<-function(asset,startDate,endDate,lookback=20){
  # Notes: %B = (Price - Lower Band)/(Upper Band - Lower Band)
  prices =  tryCatch(Cl(getSymbols(asset,from = startDate, to = endDate,env=NULL)),error= function(e) return(NA))
  # prices =  OHLCV(getSymbols(asset,from = startDate, to = endDate,env=NULL))
 if(class(prices)[1]=="xts" ) { 
   if( nrow(prices)>lookback){
  bb= round(BBands(prices,n=lookback),2)
  vlty20 = round(100*volatility(prices,n=20)/sqrt(260),2) # this is daily volatility of 20 days
  bb = cbind(bb,vlty20)
  
      if(as.Date(last(index(prices)))!=Sys.Date()){
        stockPrice = getQuote(asset)$Last  
        close = rbind(prices,xts(stockPrice,order.by = Sys.Date()))
        bb = rbind(bb,bb[nrow(bb)])
        time(bb)[nrow(bb)]<-Sys.Date()
      }else {close = prices}
  
  changeP = round(100*diff(log(close)),2)
  # dividends = getDividends(asset,from = startDate, to = endDate,env=NULL,warnings = FALSE)
  # dy = sum(dividends)/stockPrice  
  
  bb = merge(bb,close,changeP)
  colnames(bb)<- c("dn","avg","up","pB","vlty20","close","changeP")
  
   return(na.omit(bb))
   }
 }
}

filterPops<-function(BB){
  # if(class(BB)=="logical") return( BB)
  BB = BB[BB$close>BB$up & BB$changeP>BB$vlty20,]
  return( BB)
}

filterSinks<-function(BB){
  # if(class(BB)=="logical") return( BB)
   BB= BB[BB$close<BB$dn & abs(BB$changeP)>BB$vlty20,]
 return( BB)
}

processData<-function(sym,endDate=Sys.Date(),nObs=60){
  startDate = endDate-nObs*1.5
   cat(sym,"\n")
  BB <- getBollingerBands(sym,startDate,endDate)
  if(is.null(BB)) return( data.frame(symbol=sym))
   BB <-  cbind(dates=as.Date(index(BB)),  as.data.frame(BB),symbol=sym)
 
  return(rbind(filterPops(BB),filterSinks(BB)))
}

processAll<-function(){
  symbols=init()
  marketdf = tryCatch(ldply(as.vector(symbols[,"symbol"]),processData,endDate=Sys.Date(),nObs=30),error=NA)
  marketdf = na.omit(marketdf)
  return(arrange(marketdf,desc(dates),desc(changeP)))
}
 

