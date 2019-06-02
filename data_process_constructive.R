##### output:
### daily features:
# spots_daily
# forwards_daily
# returns_daily
# carry_daily
### tenured features:
# spots
# returns
# forwards
# carry
### additional (tenured) features:
# implied vol
# historical vol
# historial skew
# risk reversal

### parameters:
tenure = '1M' #can be changed to 1M/3M
rebalance_dates = NA #fixed list of rebalance dates can be input

# load data and packages
data = read.csv('data.ts.csv')
library(dplyr)
library(tidyr)
library(zoo)

data$date = as.Date(data$date)
data = data[,c(2:4)] #drop 'X' variable
data = data %>% spread(Ticker, PX_LAST)
data = data %>% filter(date > as.Date('2010-01-01')) #filter from 2010 onwards

# stack maturity dates by maturity:
maturity_dates = read.csv('maturity.dates.csv')
maturity_dates$date = as.Date(maturity_dates$date)
maturity_dates$SETTLE_DT = as.Date(maturity_dates$SETTLE_DT)
maturity_dates = maturity_dates %>% spread(Maturity, SETTLE_DT)

#dictionary
fx_names = c('INR','IDR','MYR','PHP','KRW','PLN','HUF',
             'RUB','TRY','ZAR','MXN','CLP','COP','PEN','BRL')
fwd_names = c('IRN','IHO','MRO','PPO','KWO','PLN','HUF',
              'RUB','TRY','ZAR','MXN','CHN','CLN','PSN','BCN')

"
Name updates:
-Upates all labels to country code consistent with FX labels
-Drops 'curncy' suffix from labels

"

data_names = names(data)
data_names = as.character(sapply(data_names,gsub,pattern=" Curncy",replacement=""))
data_names = as.character(sapply(data_names,gsub,pattern=" BGN", replacement=""))
for(i in 1:length(fx_names))
  data_names = as.character(sapply(data_names,gsub,pattern=fwd_names[i],replacement=fx_names[i]))
names(data) = data_names


###################


## Aligning realized return to forward maturities:

#backfill missing entries due to weekend, holiday
#reduces NAs from 6857 to 2358
data_filled = as.data.frame(na.locf(zoo(data), maxgap=4))
data_filled$date = as.Date(data_filled$date)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
for(i in 2:length(data_filled)){
  data_filled[,i] = as.numeric.factor(data_filled[,i])
}

########################

## Compute daily data
spot_names = c()
fwd_names = c()
for(i in 1:length(fx_names)){
  spot_names = append(spot_names,paste("USD",fx_names[i],sep=""))
  fwd_names = append(fwd_names,paste(fx_names[i],tenure,sep=""))
}

spots_daily = data_filled %>% select(c("date",spot_names))
forwards_daily = data_filled %>% select(c("date",fwd_names))
fwd_matured_daily = spots_daily

## Forwards are quoted in different units
## This function rescales appropriately
fwd_units = c(100,1,10000,1,1,10000,100,10000,10000,10000,10000,1,1,10000,10000)
for(i in 1:length(fx_names)){
  forwards_daily[,i+1] = forwards_daily[,i+1]/fwd_units[i]
}

# Calculate realized spot at maturity for contract on given day
for(i in 1:nrow(forwards_daily)){
  spotDay = forwards_daily$date[i]
  matDay = maturity_dates[maturity_dates$date==spotDay,tenure]
  if(matDay > spots_daily$date[nrow(spots_daily)]){
    break
  }
  fwd_matured_daily[i,-1] = spots_daily[spots_daily$date==matDay,-1]
}

# compute return realized by carry position on given day
# (Note: this is future information, caution advised for using these in model tuning)
returns_daily = (spots_daily[,-1] + forwards_daily[,-1])/fwd_matured_daily[,-1] - 1
returns_daily = returns_daily %>% mutate(date=forwards_daily$date)
carry_daily = (forwards_daily[,-1]/spots_daily[,-1])
carry_daily = carry_daily %>% mutate(date=forwards_daily$date)


# Spread daily set to appropriate tenure

# pulls first day of each month/quarter
if(is.na(rebalance_dates)){
  if(tenure == '1M'){
    rebalance_dates = spots_daily %>% 
      group_by(strftime(date, "%Y-%m")) %>% #Groups by the yearmonths
      filter(date == min(date)) %>%        #Take the first date of each group
      .$date    
  }
  if(tenure == '3M'){
    rebalance_dates = spots_daily %>% 
      group_by(as.yearqtr(spots_daily$date,format="%Y-%m-%d")) %>% 
      filter(date==min(date)) %>% 
      .$date
  }
}


returns = returns_daily[returns_daily$date %in% rebalance_dates,]
spots = spots_daily[spots_daily$date %in% rebalance_dates,]
forwards = forwards_daily[forwards_daily$date %in% rebalance_dates,]
carry = carry_daily[carry_daily$date %in% rebalance_dates,]
#carry = forwards[,-1] /spots[,-1]
#carry = carry %>% mutate(date = forwards$date)

## additional data features
iv1m = data.frame(date=rebalance_dates) #1 month implied vol
rr1m = data.frame(date=rebalance_dates) #25 delta risk reversal
hv1m = data.frame(date=rebalance_dates) #1 month historical vol
hq1m = data.frame(date=rebalance_dates) #1 month realized skewness

for(i in 1:length(fx_names)){
  iv = paste("USD",fx_names[i],"V",tenure,sep="")
  hq = paste("USD",fx_names[i],"Q",tenure,sep="")
  hv = paste("USD",fx_names[i],"H",tenure,sep="")
  rr = paste("USD",fx_names[i],"25R",tenure,sep="")
  
  
  set = data_filled %>% select(c("date",iv,rr,hv,hq))
  set = set[set$date %in% rebalance_dates,]
  
  iv1m = iv1m %>% mutate(set[,2])
  rr1m = rr1m %>% mutate(set[,3])
  hv1m = hv1m %>% mutate(set[,4])
  hq1m = hq1m %>% mutate(set[,5])
  
  names(iv1m)[i+1] = iv
  names(rr1m)[i+1] = rr
  names(hv1m)[i+1] = hv
  names(hq1m)[i+1] = hq
  
}

##### output:
### daily features:
# spots_daily
# forwards_daily
# returns_daily
# carry_daily
### tenured features:
# spots
# returns
# forwards
# carry
### additional (tenured) features:
# implied vol (iv1m)
# historical vol (hv1m)
# historial skew (hq1m)
# risk reversal (rr1m)