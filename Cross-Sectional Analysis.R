library(xts)
library(fields)
library(plotly)

# Read in data of sentiment of placebo words
sentSeries = read.csv('placebo_sentiments2019-08-25.csv')
# convert to .xts format
sentSeries.xts = xts(sentSeries[,-1], order.by=as.Date(sentSeries$Date, '%m/%d/%Y'))

# read in data of sentiment of 'Trump' tweets
trumpSent = read.csv('trumpSent2019-08-25.csv')
trumpSent.xts = xts(trumpSent$allMean, order.by=as.Date(trumpSent$date))

# read in presidential approval data from fivethirtyeight
# downloaded from https://projects.fivethirtyeight.com/trump-approval-ratings/
trend = read.csv('approval_topline2019-08-30.csv')
# convert trend for Adult approval and disapproval to .xts format
trend.xts = xts(trend[trend$subgroup=='Adults',which(names(trend) %in% c('approve_estimate', 'disapprove_estimate'))],
                order.by=as.Date(trend$modeldate[trend$subgroup=='Adults'], format='%m/%d/%Y'))
trend.xts = trend.xts[index(trend.xts)<=as.Date('2019-08-25'),]

# smoothing and lag functions
kSmooth = function(data.xts, k){
  # k-day smoothing for data in xts form
  # takes the average of the current and previous k-1 days, even if some of those days are missing data
  beginDate = index(data.xts)[1] # first date in xts data
  xtsDates = index(data.xts)[index(data.xts)-beginDate >= (k-1)] # dates to interate over
  newData = matrix(NA, nrow=length(xtsDates), ncol=ncol(data.xts)) # create new data frame of smoothed data
  colnames(newData) = names(data.xts) # same column names as original data
  for(date in xtsDates){
    date = as.Date(date)
    subset = data.xts[date-index(data.xts)>=0 & date-index(data.xts)<=(k-1),] # data of current and previous k-1 days
    newData[which(xtsDates==date),] = colMeans(subset, na.rm=TRUE) # means of each variable, ignoring NA's
  }
  dataNew.xts = xts(newData, order.by=as.Date(xtsDates))
  return(dataNew.xts)
}

ccfLag = function(ts1, ts2, max.lag){
  # time series 1, time series 2, maximum lag
  # shift time series 1 by -lag to lag days
  lagDF = data.frame()
  for(lag in -max.lag:max.lag){
    new = ts1
    index(new) = index(new)+lag
    merged = merge(new, ts2)
    corr = cor(merged[,1], merged[,2], use='pairwise.complete')
    tempDF = data.frame('lag'=lag, 'corr'=corr)
    lagDF = rbind(lagDF, tempDF)
  }
  names(lagDF) = c('lag', 'correlation')
  return(lagDF)
}

## unsmoothed sentiment with error bars
plot(as.Date(trumpSent$date), trumpSent$allMean, pch='.', xlab='Date', ylab='Mean Sentiment',
     main='Unsmoothed Sentiment of "Trump" Tweets Over Time')
for(i in 1:nrow(trumpSent)){
  segments(x0=as.Date(trumpSent$date[i]), y0=trumpSent$allMean[i]-2*trumpSent$sd[i]/sqrt(1000),
           x1=as.Date(trumpSent$date[i]), y1=trumpSent$allMean[i]+2*trumpSent$sd[i]/sqrt(1000))
}

# set window of smoothing and lag parameters
ks = 1:45 # number of days to smooth over
lag = 30 # maximum lag

# correlations between sentiment of placebo words and presidential approval
klCorrs = array(0, dim=c(length(ks), 2*lag+1, ncol(sentSeries)-1))
vars = names(sentSeries)[names(sentSeries) %in% c('Date')==FALSE]
for(j in 1:dim(klCorrs)[3]){ # iterate through each reference word
  # time series of sentiment of word
  series = sentSeries.xts[,which(names(sentSeries.xts)==vars[j])]
  for(k in ks){ # iterate through each smoothing level
    # smooth sentiment of Twitter sentiment
    smoothed = kSmooth(series, k)
    cc = ccfLag(smoothed, trend.xts$approve_estimate, lag)
    klCorrs[which(ks==k),,j] = cc$correlation
  }
  #print(paste(j, 'of', dim(klCorrs)[3]))
}

# find maximum correlation for each placebo word and location of optimal smoothing, lag parameter
placeboCorrs = data.frame()
for(i in 1:length(vars)){
  var = vars[i]
  varCorrs = klCorrs[,,i]
  if(sum(is.na(varCorrs))!=length(ks)*(2*lag+1)){
    kl = which(abs(varCorrs)==max(abs(varCorrs), na.rm=TRUE), arr.ind=TRUE)
    maxCorr = varCorrs[kl]
    temp = data.frame('word'=var, 'k'=ks[kl[,1]], 'l'=c(-lag:lag)[kl[,2]], 'maxCorr' = maxCorr)
    placeboCorrs = rbind(placeboCorrs, temp)
  }
}
placeboCorrs = placeboCorrs[abs(placeboCorrs$maxCorr)<1,]

# histogram of maximum absolute correlations
hist(placeboCorrs$maxCorr, xlab='Maximum Absolute Correlation', main='')

# smoothing and lag values where maximum absolute correlation occurs
plot(placeboCorrs$k, placeboCorrs$l, xlab='smoothing value', ylab='lag value', pch=19)

# look at heatmap of correlations based on smoothing, lag parameters
set.seed(1234)
is = sample(1:length(vars), 6)

f_label = list(size=24); f_tick = list(size=18)
plot_ly(x=ks, y=c(-lag:lag), z=t(klCorrs[,,is[1]]), type='contour', reversescale=T) %>%
  layout(title=vars[is[1]], font=f_label, 
         xaxis=list(title='Smoothing', titlefont=f_label, tickfont=f_tick), 
         yaxis=list(title='Lag', titlefont=f_label, tickfont=f_tick),
         margin=list(b=50, t=50))
plot_ly(x=ks, y=c(-lag:lag), z=t(klCorrs[,,is[2]]), type='contour', reversescale=T) %>%
  layout(title=vars[is[2]], font=f_label, 
         xaxis=list(title='Smoothing', titlefont=f_label, tickfont=f_tick), 
         yaxis=list(title='Lag', titlefont=f_label, tickfont=f_tick),
         margin=list(b=50, t=50))
plot_ly(x=ks, y=c(-lag:lag), z=t(klCorrs[,,is[3]]), type='contour', reversescale=T) %>%
  layout(title=vars[is[3]], font=f_label, 
         xaxis=list(title='Smoothing', titlefont=f_label, tickfont=f_tick), 
         yaxis=list(title='Lag', titlefont=f_label, tickfont=f_tick),
         margin=list(b=50, t=50))
plot_ly(x=ks, y=c(-lag:lag), z=t(klCorrs[,,is[4]]), type='contour', reversescale=T) %>%
  layout(title=vars[is[4]], font=f_label, 
         xaxis=list(title='Smoothing', titlefont=f_label, tickfont=f_tick), 
         yaxis=list(title='Lag', titlefont=f_label, tickfont=f_tick),
         margin=list(b=50, t=50))
plot_ly(x=ks, y=c(-lag:lag), z=t(klCorrs[,,is[5]]), type='contour', reversescale=T) %>%
  layout(title=vars[is[5]], font=f_label, 
         xaxis=list(title='Smoothing', titlefont=f_label, tickfont=f_tick), 
         yaxis=list(title='Lag', titlefont=f_label, tickfont=f_tick),
         margin=list(b=50, t=50))
plot_ly(x=ks, y=c(-lag:lag), z=t(klCorrs[,,is[6]]), type='contour', reversescale=T) %>%
  layout(title=vars[is[6]], font=f_label, 
         xaxis=list(title='Smoothing', titlefont=f_label, tickfont=f_tick), 
         yaxis=list(title='Lag', titlefont=f_label, tickfont=f_tick),
         margin=list(b=50, t=50))

# correlation between Trump tweets and presidential approval
klTrump = matrix(0, nrow=length(ks), ncol=2*lag+1)
for(k in ks){
  smoothed = kSmooth(trumpSent.xts[,1], k)
  linked = merge(smoothed, trend.xts$approve_estimate)
  cc = ccfLag(smoothed, trend.xts$approve_estimate, lag)
  klTrump[which(ks==k),] = cc$correlation
}

# maximum correlation and where it occurs
kl = which(abs(klTrump)==max(abs(klTrump), na.rm=TRUE), arr.ind=TRUE)
trumpCorr = klTrump[kl]
print(trumpCorr)
c(-lag:lag)[kl[1,2]]
ks[kl[1,1]]

# how correlation between sentiment of Trump tweets and presidential approval changes with smoothing, lag parameters
f_label = list(size=24); f_tick = list(size=18)
plot_ly(x=ks, y=c(-lag:lag), z=t(klTrump), type='contour', reversescale=T) %>%
  layout(title='Trump', font=f_label, 
         xaxis=list(title='Smoothing', titlefont=f_label, tickfont=f_tick), 
         yaxis=list(title='Lag', titlefont=f_label, tickfont=f_tick),
         margin=list(b=50, t=50))

# in comparison to placebo words
hist(placeboCorrs$maxCorr, freq=TRUE, main='', breaks=seq(-1, 1, by=.1), ylab='', xlab='', cex.axis=1.5)
abline(v=trumpCorr, col='red', lwd=6)
mtext("Maximum Absolute Correlation", side=1, cex=2, line=2.75)
mtext("Frequency", side=2, cex=2, line=2.75)


# robustness to change in end date
endDates = as.Date('2017-05-20'):as.Date('2019-08-25')
robust.df = data.frame() # endDate, k, l, maxCorr
for(end in endDates){
  trumpTemp.xts = trumpSent.xts[index(trumpSent.xts)<=as.Date(end), ]
  # optimal k, l
  klTrumpTemp = matrix(nrow=length(ks), ncol=2*lag+1)
  approvalTemp = trend.xts$approve_estimate[index(trend.xts)<=as.Date(end)]
  for(k in ks){
    smoothed = kSmooth(trumpTemp.xts[,1], k)
    cc = ccfLag(smoothed, approvalTemp, lag)
    klTrumpTemp[which(ks==k),] = cc$correlation
  }
  klTemp = which(abs(klTrumpTemp)==max(abs(klTrumpTemp), na.rm=TRUE), arr.ind=TRUE)
  corrTemp = klTrumpTemp[klTemp]
  dfTemp = data.frame('endDate'=as.Date(end), 'k'=ks[klTemp[1,1]], 'l'=c(-lag:lag)[klTemp[1,2]], 'maxCorr' = corrTemp)
  robust.df = rbind(robust.df, dfTemp)
  print(as.Date(end))
}

optK = robust.df$k[length(robust.df$k)]
optL = robust.df$l[length(robust.df$l)]

corrs = c()
for(end in endDates){
  trumpTemp.xts = trumpSent.xts[index(trumpSent.xts)<=as.Date(end), ]
  approvalTemp = trend.xts$approve_estimate[index(trend.xts)<=as.Date(end)]
  smoothed = kSmooth(trumpTemp.xts[,1], optK)
  index(smoothed) = index(smoothed) + optL
  merged = merge(smoothed, approvalTemp)
  corrs = c(corrs, cor(merged[,1], merged[,2], use='pairwise.complete'))
}
robust.df$corrs = corrs

par(mfrow=c(2,1))
plot(robust.df$endDate, robust.df$k, ylab='Optimal Smoothing', xlab='', pch=19)
plot(robust.df$endDate, robust.df$l, ylab='Optimal Lag', xlab='Data End Date', pch=19)
par(mfrow=c(1,1))

plot(robust.df$endDate, robust.df$maxCorr, ylab='Correlation', xlab='Data End Date', pch=20)
lines(robust.df$endDate, robust.df$corrs, lty=2)
legend('bottomright', c('Maximum Absolute Correlation', 'With 30 day lag, 45 day smoothing'), lty=c(1,2), lwd=c(3, 1))


# 'p-value'
pval = sum(abs(placeboCorrs$maxCorr)>=trumpCorr)/nrow(placeboCorrs)

# how 'p-value' changes over time
allOverTime = list()
monthlyEndDates = seq(as.Date('2017-06-01'), as.Date('2019-08-01'), "month")
pval_changes = data.frame()
placeboCorrs_list = vector(mode='list', length=length(monthlyEndDates))
for(date in monthlyEndDates){
  temp = array(0, dim=c(length(ks), 2*lag+1, ncol(sentSeries)-1))
  for(j in 1:dim(klCorrs)[3]){ # iterate through each reference word
    # time series of sentiment of word
    series = sentSeries.xts[index(sentSeries.xts)<=date,which(names(sentSeries.xts)==vars[j])]
    for(k in ks){ # iterate through each smoothing level
      # smooth sentiment of Twitter sentiment
      smoothed = kSmooth(series, k)
      cc = ccfLag(smoothed, trend.xts$approve_estimate[index(trend.xts)<=date], lag)
      temp[which(ks==k),,j] = cc$correlation
    }
    allOverTime[[which(monthlyEndDates==date)]] = temp
    print(paste(j, 'of', dim(klCorrs)[3]))
  }
  # calculate optimal correlation and location of optimal parameters for each word
  for(i in 1:length(vars)){
    var = vars[i]
    varCorrs = temp[,,i]
    if(sum(is.na(varCorrs))!=length(ks)*(2*lag+1)){
      kl = which(abs(varCorrs)==max(abs(varCorrs), na.rm=TRUE), arr.ind=TRUE)
      maxCorr = varCorrs[kl]
      placeboTemp = data.frame('word'=var, 'k'=ks[kl[,1]],'l'=c(-lag:lag)[kl[,2]], 'maxCorr'=maxCorr)
      placeboCorrs_list[[which(monthlyEndDates==date)]] = rbind(placeboCorrs_list[[which(monthlyEndDates==date)]], placeboTemp)
    }
  }
  placeboCorrs_list[[which(monthlyEndDates==date)]] = na.omit(placeboCorrs_list[[which(monthlyEndDates==date)]])
  # calculate 'p-value' at end date
  trumpCorrEnd = robust.df$maxCorr[robust.df$endDate==date]
  pvalEnd = sum(abs(placeboCorrs_list[[which(monthlyEndDates==date)]]$maxCorr)>=abs(trumpCorrEnd))/nrow(placeboCorrs_list[[which(monthlyEndDates==date)]])
  pval_changes = rbind(pval_changes, data.frame('endDate'=date, 'trumpCorr'=trumpCorrEnd, 'pval'=pvalEnd))
  print(as.Date(date))
}
pval_changes = rbind(pval_changes, data.frame('endDate'=as.Date('2019-08-25'), 'trumpCorr'= trumpCorr,'pval'=pval))

plot(as.Date(pval_changes$endDate), pval_changes$pval, xlab='End Date', ylab='Proportion', pch=19, ylim=c(0, 1))


# smaller window for smoothing and lag for comparison
newMaxK = 45
newMaxLag = 7

placeboCorrs_list_instKL = vector(mode='list', length=length(placeboCorrs_list))
pval_changes_instKL = data.frame()
for(j in 1:length(placeboCorrs_list_instKL)){
  temp = allOverTime[[j]]
  temp = temp[1:newMaxK, (30-newMaxLag):(30+newMaxLag), ]
  # calculate optimal correlation in updated smoothing, lag window for each word
  for(i in 1:length(vars)){
    var = vars[i]
    varCorrs = temp[,,i]
    if(sum(is.na(varCorrs))!=newMaxK*(2*newMaxLag+1)){
      kl = which(abs(varCorrs)==max(abs(varCorrs), na.rm=TRUE), arr.ind=TRUE)
      maxCorr = varCorrs[kl]
      placeboTemp = data.frame('word'=var, 'k'=c(1:newMaxK)[kl[,1]], 'l'=c(-newMaxLag:newMaxLag)[kl[,2]], 'maxCorr'=maxCorr)
      placeboCorrs_list_instKL[[j]] = rbind(placeboCorrs_list_instKL[[j]], placeboTemp)
    }
  }
  placeboCorrs_list_instKL[[j]] = na.omit(placeboCorrs_list_instKL[[j]])
  # calcualte "Trump" correlation using new window
  trumpTemp.xts = trumpSent.xts[index(trumpSent.xts)<=as.Date(monthlyEndDates[j]), ]
  # optimal k, l
  klTrumpTemp = matrix(nrow=newMaxK, ncol=2*newMaxLag+1)
  approvalTemp = trend.xts$approve_estimate[index(trend.xts)<=as.Date(monthlyEndDates[j])]
  for(k in 1:newMaxK){
    smoothed = kSmooth(trumpTemp.xts[,1], k)
    cc = ccfLag(smoothed, approvalTemp, newMaxLag)
    klTrumpTemp[k,] = cc$correlation
  }
  klTemp = which(abs(klTrumpTemp)==max(abs(klTrumpTemp), na.rm=TRUE), arr.ind=TRUE)
  corrTemp = klTrumpTemp[klTemp]
  # calculate 'p-value'
  pval_end_new = sum(abs(placeboCorrs_list_instKL[[j]]$maxCorr)>=abs(corrTemp))/nrow(placeboCorrs_list_instKL[[j]])
  pval_changes_instKL = rbind(pval_changes_instKL, data.frame('endDate'=monthlyEndDates[j], 'trumpCorr'=corrTemp, 'pval'=pval_end_new))
  print(j)
}

plot(pval_changes_instKL$endDate, pval_changes_instKL$pval, ylim=c(0,1), pch=19, 
     main="p-value over time with max lag of 7 days")

plot(as.Date(pval_changes$endDate), pval_changes$trumpCorr, col='red', pch=19, main='"Trump" correlations')
points(pval_changes_instKL$endDate, pval_changes_instKL$trumpCorr, col='blue', pch=19)
legend('bottomright', legend=c("Max. Lag=30", "Max. Lag=7"), col=c('red', 'blue'), pch=c(19,19))

plot(as.Date(pval_changes$endDate), pval_changes$pval, col='gray50', pch=19, ylim=c(0,1), 
     xlab="End Date", ylab='Proportion')
points(pval_changes_instKL$endDate, pval_changes_instKL$pval, col='black', pch=0)
legend('topright', legend=c("Max. Lag=30", "Max. Lag=7"), col=c('gray50', 'black'), pch=c(19,0))

