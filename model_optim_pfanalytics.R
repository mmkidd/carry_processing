## Model optimizer with portfolio analytics

library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

# data must be in xts form
returns_xts = xts(returns[,-16],order.by=returns[,16])

# Portfolio Fit Using Daily Data for Mean/Var
# Inputs:
# returns - xts series of monthly returns
# daily - daily series used to compute mean/var
# objective - 'long','short','neutral',or 'longshort'
# saple_period - number of months to sample for mean/var calculation
# Note: uses lagging days to calculate mean/var, but risks data contamination

fit_portfolio_daily = function(returns,daily,objective='long',sample_period = 2){
  assetNames = names(returns)
  month_years = unique(format(daily$date,"%Y-%m"))
  weights = returns*0
  
  for(i in (sample_period+1):length(month_years)){
    print(paste('Fitting Month #', i))
    current_month = (format(daily$date,"%Y-%m") %in% month_years[(i-2):(i-sample_period)])
    current_ret = xts(daily[current_month,-16],order.by=daily[current_month,16])
    names(current_ret) = names(returns)
    current_ret = current_ret[,!is.na(colMeans(current_ret))]
    
    pf = portfolio.spec(names(current_ret))
    if(objective=='long'){
      pf <- add.constraint(pf, type='weight_sum', min_sum = 1, max_sum = 1)
      pf <- add.constraint(pf, type='box', min=0, max=1)
    }
    
    if(objective=='short'){
      pf <- add.constraint(pf, type='weight_sum', min_sum = -1, max_sum = -1)
      pf <- add.constraint(pf, type='box', min=-1, max=0)
    }
    
    if(objective=='neutral'){
      pf <- add.constraint(pf, type='weight_sum', min_sum = 0, max_sum = 0)
      pf <- add.constraint(pf, type='box', min=-1, max=1)
    }
    
    if(objective=='longshort'){
      pf <- add.constraint(pf, type='weight_sum', min_sum = 1, max_sum = 1)
      pf <- add.constraint(pf, type='box', min=-1, max=1)
    }
    
    pf <- add.objective(pf, type='return', name='mean')
    pf <- add.objective(pf, type='risk', name='StdDev')
    
    opt_pf = optimize.portfolio(R=current_ret, portfolio=pf,
                                optimize_method="ROI",
                                trace=TRUE)
    
    weights[i,names(current_ret)] = opt_pf$weights
  }
  
  
 return(weights) 
}

#Tests using different portfolios

meanvar_weights = fit_portfolio_daily(returns_xts,returns_daily)
meanvar_short = fit_portfolio_daily(returns_xts,returns_daily,objective='short')
meanvar_neutral = fit_portfolio_daily(returns_xts,returns_daily,objective='neutral')
meanvar_longshort = fit_portfolio_daily(returns_xts,returns_daily,objective='longshort')

carryvar_weights = fit_portfolio_daily(returns_xts,carry_daily)
carryvar_short = fit_portfolio_daily(returns_xts,carry_daily,objective='short')
carryvar_neutral = fit_portfolio_daily(returns_xts,carry_daily,objective='neutral')
carryvar_longshort = fit_portfolio_daily(returns_xts,carry_daily,objective='longshort')

ret_long = rowSums(meanvar_weights * returns_xts,na.rm=T)
ret_short = rowSums(meanvar_short * returns_xts,na.rm=T)
ret_neutral = rowSums(meanvar_neutral * returns_xts,na.rm=T)
ret_longshort = rowSums(meanvar_longshort * returns_xts,na.rm=T)

carry_long = rowSums(carryvar_weights * returns_xts,na.rm=T)
carry_short = rowSums(carryvar_short * returns_xts,na.rm=T)
carry_neutral = rowSums(carryvar_neutral * returns_xts,na.rm=T)
carry_longshort = rowSums(carryvar_longshort * returns_xts,na.rm=T)

#sharpe
mean(ret_long)/sd(ret_long)*sqrt(12)
mean(ret_short)/sd(ret_short)*sqrt(12)
mean(ret_neutral)/sd(ret_neutral)*sqrt(12)
mean(ret_longshort)/sd(ret_longshort)*sqrt(12)

mean(carry_long)/sd(carry_long)*sqrt(12)
mean(carry_short)/sd(carry_short)*sqrt(12)
mean(carry_neutral)/sd(carry_neutral)*sqrt(12)
mean(carry_longshort)/sd(ret_longshort)*sqrt(12)

cum_ret_long = cumprod(1+rowSums(meanvar_weights * returns_xts,na.rm=T))
cum_carry = cumprod(1+rowSums(carryvar_weights * returns_xts,na.rm=T))
cum_ret_short = cumprod(1+rowSums(meanvar_short * returns_xts,na.rm=T))
cum_ret_neutral = cumprod(1+rowSums(meanvar_neutral * returns_xts,na.rm=T))
cum_ret_longshort = cumprod(1+rowSums(meanvar_longshort * returns_xts,na.rm=T))

cum_ret_long

plot(ret_long,ylab='Return',xlab='Months',main='Long-Only, Returns Optimized')
plot(ret_short,ylab='Return',xlab='Months',main='Short-Only, Returns Optimized')
plot(ret_neutral,ylab='Return',xlab='Months',main='Neutral, Returns Optimized')
plot(ret_longshort,ylab='Return',xlab='Months',main='Long-Short, Returns Optimized')
plot(carry_long,ylab='Return',xlab='Months',main='Long-Only, Carry Optimized')
plot(ret_longshort)

# Portfolio Fit Using Monthly Data for Mean/Var
# Inputs:
# returns - xts series of monthly returns
# objective - 'long','short','neutral',or 'longshort'
# saple_period - number of months to sample for mean/var calculation

fit_portfolio_monthly = function(returns,target='mean',objective='long',sample_period = 3){
  assetNames = names(returns)
  #month_years = unique(format(daily$date,"%Y-%m"))
  month_years = time(returns)
  weights = returns*0
  
  for(i in (sample_period+1):length(month_years)){
    print(paste('Fitting Month #', i))
    #current_month = (format(daily$date,"%Y-%m") %in% month_years[(i-1):(i-sample_period)])
    #current_ret = xts(daily[current_month,-16],order.by=daily[current_month,16])
    current_ret = returns[1:(i-1),]
    names(current_ret) = names(returns)
    current_ret = current_ret[,!is.na(colMeans(current_ret))]
    
    pf = portfolio.spec(names(current_ret)) 
    
    if(objective=='long'){
      pf <- add.constraint(pf, type='weight_sum', min_sum = 1, max_sum = 1)
      pf <- add.constraint(pf, type='box', min=0, max=1)
    }
    
    if(objective=='short'){
      pf <- add.constraint(pf, type='weight_sum', min_sum = -1, max_sum = -1)
      pf <- add.constraint(pf, type='box', min=-1, max=0)
    }
    
    if(objective=='neutral'){
      pf <- add.constraint(pf, type='weight_sum', min_sum = 0, max_sum = 0)
      pf <- add.constraint(pf, type='box', min=-1, max=1)
    }
    
    if(objective=='longshort'){
      pf <- add.constraint(pf, type='weight_sum', min_sum = 1, max_sum = 1)
      pf <- add.constraint(pf, type='box', min=-1, max=1)
    }
    
    pf <- add.objective(pf, type='return', name='mean')
    pf <- add.objective(pf, type='risk', name='StdDev')
    
    opt_pf = optimize.portfolio(R=current_ret, portfolio=pf,
                                optimize_method="ROI",
                                trace=TRUE)
    weights[i,names(current_ret)] = opt_pf$weights
  }
  return(weights)
}



meanvar_weights = fit_portfolio_monthly(returns_xts,sample_period=30)
meanvar_short = fit_portfolio_monthly(returns_xts,sample_period=30,objective='short')
meanvar_neutral = fit_portfolio_monthly(returns_xts,sample_period=30,objective='neutral')
meanvar_longshort = fit_portfolio_monthly(returns_xts,sample_period=30,objective='longshort')

cumprod(1+rowSums(meanvar_weights * returns_xts,na.rm=T))
cumprod(1+rowSums(carryvar_weights * returns_xts,na.rm=T))
cumprod(1+rowSums(meanvar_short * returns_xts,na.rm=T))
cumprod(1+rowSums(meanvar_neutral * returns_xts,na.rm=T))
cumprod(1+rowSums(meanvar_longshort * returns_xts,na.rm=T))