
## Longitudinal Analysis

library(xts)

# import presidential approval
trend = read.csv('approval_topline.csv')

# import Republican, Democratic tweets
Republican_tweets = read.csv('Republican_tweets.csv')
Democratic_tweets = read.csv('Democratic_tweets.csv')

# convert to list
repTweets = list()
for(i in 1:length(unique(Republican_tweets$userCode))){
  repTweets[[i]] = Republican_tweets[Republican_tweets$userCode==i, 3:5]
  print(i)
}
demTweets = list()
for(i in 1:length(unique(Democratic_tweets$userCode))){
  demTweets[[i]] = Democratic_tweets[Democratic_tweets$userCode==i+length(repTweets), 3:5]
  print(i)
}

### smooth sentiment for each user
smoothPreds = function(data, dates, predDates, k){
  h = function(diff){return(1/(diff+1))}
  dates = as.Date(dates); predDates = as.Date(predDates)
  preds = c()
  for(day in predDates){
    day = as.Date(day, origin='1970-01-01')
    index = day-dates<=k & day-dates>0
    if(sum(index)==0){preds = c(preds, NA)}
    else{
      relevantTweets = data[index]
      weights = h(as.integer(day-dates[index]))
      preds = c(preds, sum(relevantTweets*weights)/sum(weights))
    }
  }
  df = data.frame('dates'=predDates, 'estSent'=preds)
  return(df)
}

npr_rep = list() # list of smoothed sentiments for Republicans
npr_dem = list() # list of smoothed sentiments for Democrats
predDates = as.Date('2017-01-01'):as.Date('2018-05-20')
for(i in 1:length(repTweets)){
  tweets = repTweets[[i]]
  tweets =  tweets[tweets$rt=='False',]
  created = as.Date(substr(tweets$created, 1, 10))
  npr_rep[[i]] = smoothPreds(tweets$vader, as.Date(created), as.Date(predDates, origin='1970-01-01'), 30)
  print(i)
}
for(i in 1:length(demTweets)){
  tweets = demTweets[[i]]
  tweets = tweets[tweets$rt=='False',]
  created = as.Date(substr(tweets$created, 1, 10))
  npr_dem[[i]] = smoothPreds(tweets$vader, as.Date(created), as.Date(predDates, origin='1970-01-01'), 30)
  print(i)
}

# median sentiment for Democrats, Republicans
predCenters = data.frame()
for(day in predDates){
  day = as.Date(day, origin='1970-01-01')
  estSents_rep = c()
  estSents_dem = c()
  for(series in npr_rep){
    estSents_rep = c(estSents_rep, series$estSent[series$dates==day])
  }
  for(series in npr_dem){
    estSents_dem = c(estSents_dem, series$estSent[series$dates==day])
  }
  df = data.frame(as.Date(day), median(estSents_rep, na.rm=TRUE), median(estSents_dem, na.rm=TRUE))
  predCenters = rbind(predCenters, df)
  print(as.Date(day))
}
names(predCenters) = c('date', 'medianRep', 'medianDem')


# plot sentiments
plot(0, 0, xlim=as.Date(range(predDates), origin='1970-01-01'), ylim=c(-1, 1), main='Republican Sentiment', xlab='date', ylab='sentiment')
for(i in 1:length(npr_rep)){
  lines(npr_rep[[i]]$dates, npr_rep[[i]]$estSent, lty=1, col='grey')
}
lines(predCenters$date, predCenters$medianRep, lty=1, col='red')

plot(0,0, xlim=as.Date(range(predDates), origin='1970-01-01'), ylim=c(-1,1), main='Democratic Sentiment', xlab='date', ylab='sentiment')
for(i in 1:length(npr_dem)){
  lines(npr_dem[[i]]$dates, npr_dem[[i]]$estSent, lty=1, col='grey')
}
lines(predCenters$date, predCenters$medianDem, lty=1, col='blue')

plot(predCenters$date, predCenters$medianRep, type='l', col='red', xlab='date', ylab='sentiment',
     main='Democratic and Republican Sentiment')
lines(predCenters$date, predCenters$medianDem, lty=1, col='blue')

plot(predCenters$date, predCenters$medianRep-predCenters$medianDem, type='l', xlab='date', ylab='sentiment', 
     main='Difference in Sentiment')

# link up with presidential approval
trend.xts = xts(trend[trend$subgroup=='Adults',which(names(trend) %in% c('approve_estimate', 'disapprove_estimate'))],
                order.by=as.Date(trend$modeldate[trend$subgroup=='Adults'], format='%m/%d/%Y'))
tw_sent.xts = xts(predCenters[,-1], order.by=as.Date(predCenters$date))
all.xts = merge(trend.xts, tw_sent.xts)
all.xts$diff = all.xts$medianRep - all.xts$medianDem

cor(all.xts, use='pairwise.complete')

