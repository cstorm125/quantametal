library(jsonlite)
library(lubridate)
library(XML)
library(plyr)
library(dplyr)
library(zoo)
library(data.table)
library(ggplot2)
library(sqldf)



personal_token <- '71ad495cc7a4406160e97462c9c0df43:3a63dcf021aaf23abaf7f8e86603281e'
input_file <- 'files/'
raw_file <- 'files/raw/'
set_hist_path <- 'files/set_historical/'
current_year <- year(today())
begin_year <- 2002

##########################
# Daily Price
##########################


set_hist_files <- dir(set_hist_path)
set_hist <- NULL
for (day in set_hist_files){
  print(day)
  temp <- fread(paste0(set_hist_path,day))
  set_hist <- rbind(set_hist,temp)
}

colnames(set_hist) <- c('symbol','date','open','high','low','close','volume')
set_hist$date <- ymd(set_hist$date)
saveRDS(set_hist,paste0(input_file,'set_hist.rds'))

##########################
# Benchmark
##########################

#Download and save as set.html
# download.file(url='http://www.set.or.th/set/fetchfile.do?filename=roi/market.xls',
#               destfile = paste0(input_file,'set.html'))

#Read as html table
se<-readHTMLTable(paste0(input_file,'set.html'),header=TRUE)
se<-as.data.frame(se)

#Edit column names
colnames(se)<-gsub('NULL.','',colnames(se))

#convert to numeric
se[,2:6] <- apply(se[,2:6],2,FUN=function(x) as.numeric(gsub(',','',x)))

#Set date and fiscalyear
se$datetime<-dmy(se$Date)
se$year <- substr(se$datetime,1,4)
se$month <- substr(se$datetime,6,7)
se$fiscalyear <- paste(se$year,se$month, sep='-')

#rank
se <- data.table(se)
se[,date_rank:=rank(datetime,ties.method = 'random'),by='fiscalyear']
se[,max_date:=max(date_rank),by='fiscalyear']

#get returns based on end of quarter price
set_returns <- se[se$max_date==se$date_rank,]
#remove ranks
set_returns <- subset(set_returns, select=-c(date_rank,max_date))
#calculate returns
set_returns$fwd_price <- lead(set_returns$SET.TRI,4)
set_returns$set_return <- set_returns$fwd_price / set_returns$SET.TRI - 1
#get only end of quarter
set_returns <- set_returns[set_returns$month %in% c('03','06','09','12'),]
set_returns <- subset(set_returns, select=c(fiscalyear,set_return))
#add real set return
set_returns$real_set_return <- lead(set_returns$set_return)

#risk-free rates
risk_free <- data.frame(fiscalyear=begin_year:current_year,rate=0.02)

##########################
# Stocks
##########################

#all stocks without utilities and finance
all_stocks<-scan(paste0(input_file,"all_stocks.txt"), what="character", sep=',')
all_stocks<-unique(sapply(all_stocks,FUN=function(x) gsub('-F','',x)))

# for(i in all_stocks[265:469]){
#   print(paste('Downloading:', i))
#   document <- fromJSON(paste0('https://www.gurufocus.com/api/public/user/',personal_token,
#                               '/stock/',i,'/financials'))
#   saveRDS(document,paste0(raw_file,i,'.rds'))
# }

##########################
# quarterly: Loop cleaning operations through all symbols
##########################

lst<-list()
for (stock_file in dir(raw_file)){
  
  #Load stock info from rds
  j<-readRDS(paste0(raw_file,stock_file))
  
  #stock name
  stock_name <- gsub('.rds','',stock_file)
  
  #check if there's data
  if (is.null(j$financials$quarterly$`Fiscal Year`)) next
  
  #get all elemenets
  all_elements <- as.data.frame(j$financials$quarterly)
  
  #convert unusual quarter
  #1 to 12, 4 to 3, 7 to 6, and 10 to 9
  all_elements[,'Fiscal.Year'] <- as.yearmon(all_elements[,'Fiscal.Year'])
  
  #function to correct date
  correctDate <- function (x){
    if (month(x) %in% c(1,4,7,10)) {
      result <- x - 1/12
    } else {
      result <- x
    }
    format(result,'%Y-%m')
  }
  #apply the function
  all_elements[,'Fiscal.Year'] <- sapply(all_elements[,'Fiscal.Year'],correctDate)

  #Set symbol and year to data.frame
  #all combinations of quarters from 2002
  years <- begin_year:current_year
  months <- c('03','06','09','12')
  quarters <- c()
  for (i in years){
    for (j in months){
      quarters <- c(quarters,paste(i,j,sep='-'))
    }
  }
  
  result<-data.frame(symbol=stock_name, `Fiscal.Year` = quarters)
  
  #Bind all elements to data.frame
  result<-join(result,all_elements,by='Fiscal.Year',type='left')

  #Change header to lowercase, remove non-alphanumeric characters, trim spaces
  colnames(result)<-tolower(colnames(result))
  colnames(result)<-gsub('[^a-zA-Z0-9]','',colnames(result))
  colnames(result)<-trimws(colnames(result))
  
  #Convert all columns except dates and symbols to numeric
  for (i in 1:length(result)){
    if (!grepl('(filing|symbol|fiscalyear)',colnames(result)[i],ignore.case = TRUE)) {
      result[,i]<-as.numeric(as.character(result[,i]))
    }
  }
  
  #Add forward price and price growth in %
  end_price <-result[['persharedataarraymonthendstockprice']]
  result$real_end_price <- lead(end_price)

  result$fwd_price <- lead(end_price,4)
  result$real_fwd_price <- lead(result$real_end_price,4)
  result$priceg<- result$fwd_price/end_price-1
  result$real_priceg <- result$real_fwd_price / result$real_end_price - 1
  
  #Remove infinities and NaN
  result$priceg<-gsub('(NaN|Inf|-Inf)',NA,result$priceg)
  result$real_priceg<-gsub('(NaN|Inf|-Inf)',NA,result$real_priceg)
  #Have to convert back because gsubbed
  result$priceg<-as.numeric(result$priceg)
  result$real_priceg <-as.numeric(result$real_priceg)
  
  #create ebit ttm
  result$ebit <- result$persharedataarrayebitpershare * result$valuationandqualitysharesoutstandingeop
  result$ebit_ttm <- rollapply(result$ebit, width=4, FUN=sum,fill=NA,align='right')
  
  #create ey
  result$ey <- result$ebit_ttm / result$valuationandqualityenterprisevalue
  #create roc
  result$net_fixed_asset <- result$balancesheettotalassets - result$balancesheettotalcurrentassets -
                            result$balancesheetaccumulateddepreciation -
                            result$balancesheetintangibleassets -
                            result$balancesheetgoodwill
  result$working_capital <- result$balancesheettotalcurrentassets - result$balancesheettotalcurrentliabilities
  result$working_capital <- ifelse(result$working_capital < 0, 0, result$working_capital)
  result$roc <- result$ebit_ttm / ((result$net_fixed_asset+result$working_capital)/2)
  
  #Remove infinities and NaN
  result$roc<-gsub('(NaN|Inf|-Inf)',NA,result$roc)
  result$ey<-gsub('(NaN|Inf|-Inf)',NA,result$ey)
  #Have to convert back because gsubbed
  result$roc<-as.numeric(result$roc)
  result$ey <-as.numeric(result$ey)

  lst[[stock_name]] <- result
}

#Combine all data.frames in list
quarterly <- ldply(lst, data.frame)

#create year and month
quarterly$year <- substr(quarterly$fiscalyear,1,4)
quarterly$month <- substr(quarterly$fiscalyear,6,7)

#attach set returns
quarterly <- join(quarterly,set_returns,by='fiscalyear',type='inner')

#attach risk-free rates
#quarterly <- join(quarterly, risk_free, by = 'fiscalyear',type='inner')

#get rid of NAs
quarterly <-quarterly[!is.na(quarterly$incomestatementrevenue),]
saveRDS(quarterly,paste0(input_file,'quarterly.rds'))


# ##########################
# # annuals: Loop cleaning operations through all symbols
# ##########################
# lst<-list()
# for (stock_file in dir(raw_file)){
#   
#   #Load stock info from rds
#   j<-readRDS(paste0(raw_file,stock_file))
#   
#   #stock name
#   stock_name <- gsub('.rds','',stock_file)
#   
#   #check if there's data
#   if (is.null(j$financials$annuals$`Fiscal Year`)) next
#   
#   
#   #Set symbol and year to data.frame
#   result<-data.frame(symbol=rep(stock_name,length(j$financials$annuals$`Fiscal Year`)))
#   
#   #Bind all elements to data.frame
#   result<-cbind(result,j$financials$annuals)
#   
#   #Remove TTM
#   result<-result[result[['Fiscal Year']]!='TTM',]
#   
#   #convert yearmon to year
#   result[['Fiscal Year']] <- substr(result[['Fiscal Year']],1,4)
#   
#   #Change header to lowercase, remove non-alphanumeric characters, trim spaces
#   colnames(result)<-tolower(colnames(result))
#   colnames(result)<-gsub('[^a-zA-Z0-9]','',colnames(result))
#   colnames(result)<-trimws(colnames(result))
#   
#   #Convert all columns except dates and symbols to numeric
#   for (i in 1:length(result)){
#     if (!grepl('(filing|symbol)',colnames(result)[i],ignore.case = TRUE)) {
#       result[,i]<-as.numeric(as.character(result[,i]))
#     }
#   }
#   
#   #Add forward price and price growth in %
#   end_price <-result[['persharedataarraymonthendstockprice']]
#   result$fwd_price <- lead(end_price)
#   result$priceg<- result$fwd_price/end_price-1
#   
#   #Remove infinities and NaN
#   result$priceg<-gsub('(NaN|Inf|-Inf)',NA,result$priceg)
#   #Have to convert back because gsubbed
#   result$priceg<-as.numeric(result$priceg)
#   
#   lst[[stock_name]] <- result
# }
# 
# #Combine all data.frames in list
# annuals <- ldply(lst, data.frame)
# 
# #add year and month
# annuals$year <- annuals$fiscalyear
# annuals$month <- '12'
# 
# #attach set returns
# set_returns_annual <- subset(set_returns,select=c(year,month,set_return))
# colnames(set_returns_annual) <-c('fiscalyear','month','set_return')
# annuals <- join(annuals,set_returns_annual[set_returns_annual$month=='12',],by='fiscalyear',type='inner')
# 
# #attach q1 monthendprice, priceg to q4 (annual)
# real_price <- quarterly[quarterly$month=='03',c('symbol','year',
#                                          'persharedataarraymonthendstockprice',
#                                          'fwd_price','priceg','set_return')]
# colnames(real_price) <- c('symbol','fiscalyear','real_end_price',
#                           'real_fwd_price','real_priceg','real_set_return')
# annuals <- join(annuals,real_price, by=c('symbol','fiscalyear'))
# 
# #get rid of NAs
# annuals <-annuals[!is.na(annuals$incomestatementrevenue),]
# saveRDS(annuals,paste0(input_file,'annuals.rds'))



##########################
# Little book
##########################

#create data set
little_book <- quarterly[,c('fiscalyear','year','month','symbol',
                          'ey','roc',
                          'valuationandqualitymarketcap',
                          'real_priceg','priceg',
                          'set_return','real_set_return')]
colnames(little_book) <- c('fiscalyear','year','month',
                           'symbol','ey','roc','marketcap','real_priceg','priceg',
                           'set_return','real_set_return')

#get rid of those with few data 
little_book$yearmon <- as.yearmon(little_book$fiscalyear)
little_book <- little_book[little_book$yearmon >= 'Dec 2009'
                           & little_book$year <= 'Jun 2015',]
little_book <- little_book[complete.cases(little_book),]

#filter those below 2B marketcap
little_book <- little_book[little_book$marketcap >=2000,]

#rank them using data table
little_book <- data.table(little_book)
little_book[,ey_rank:=rank(-ey,ties.method = 'random'),by='fiscalyear']
little_book[,roc_rank:=rank(-roc,ties.method = 'random'),by='fiscalyear']
little_book[,magic_score:=ey_rank+roc_rank]
little_book[,magic_rank:=rank(magic_score,ties.method = 'random'),by='fiscalyear']

#save
saveRDS(little_book,paste0(input_file,'little_book.rds'))

#bought stocks
bought_little <- little_book[little_book$magic_rank<=30,]

#profit by year
profit_little <- sqldf("
                       select
                       fiscalyear,
                       `year`,
                       `month`,
                       avg(priceg) as fake_return,
                       avg(real_priceg) as real_return,
                       avg(set_return) as set_return,
                       avg(real_set_return) as real_set_return
                       from bought_little
                       group by fiscalyear,`year`,`month`
                       ")
profit_little$fake_return_5y <- rollapply(profit_little$fake_return,width=5,
                                          FUN=function(x) exp(mean(log(1+x)))-1,fill=NA,
                                          align='right')
profit_little$real_return_5y <- rollapply(profit_little$real_return,width=5,
                                          FUN=function(x) exp(mean(log(1+x)))-1,fill=NA,
                                          align='right')
profit_little$set_return_5y <- rollapply(profit_little$set_return,width=5,
                                          FUN=function(x) exp(mean(log(1+x)))-1,fill=NA,
                                          align='right')
profit_little$real_set_return_5y <- rollapply(profit_little$real_set_return,width=5,
                                          FUN=function(x) exp(mean(log(1+x)))-1,fill=NA,
                                          align='right')


##########################
# Piotrovski
##########################
#create data set
pio <- annuals[,c('fiscalyear','symbol',
                          'valuationandqualitypiotroskifscore',
                          'valuationandqualitymarketcap',
                          'real_priceg',
                          'set_return')]
pio <- pio[complete.cases(pio),]
colnames(pio) <- c('fiscalyear','symbol','pio','marketcap','real_priceg','set_return')

#filter those below 2B marketcap
pio <- pio[pio$marketcap >=2000,]

#get rid of those with few data (less than 2009)
pio <- pio[pio$fiscalyear >=2009,]

#see pio distribution
ggplot(data=pio, aes(x=pio)) + geom_histogram()
quantile(pio$pio)

#filter f-score above 7
bought_pio <- pio[pio$pio>=7,]
mean(bought_pio$real_priceg)

#profit by year
profit_year_pio <- sqldf("
                     select
                     fiscalyear,
                     avg(real_priceg) as return,
                     avg(set_return) as set_return
                     from bought_pio
                     group by fiscalyear
                     ")


