library(ggplot2)

### Longitudinal Analysis
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

## create data frame for daily frequency, sentiment of Democratic, Republican tweets
repDaily = data.frame()
demDaily = data.frame()
dates = as.Date('2016-01-01'):as.Date('2017-05-01')

repFirstTweetDates = as.Date(sapply(repTweets, function(x)return(as.Date(substr(min(as.Date(x$created)), 1, 10)))), origin=as.Date('1970-01-01'))
demFirstTweetDates = as.Date(sapply(demTweets, function(x)return(as.Date(substr(min(as.Date(x$created)), 1, 10)))), origin=as.Date('1970-01-01'))

for(date in dates){
  date = as.Date(date, origin=as.Date('1970-01-01'))
  demTemp = Democratic_tweets[Democratic_tweets$rt=='False' & substr(Democratic_tweets$created, 1, 10)==as.Date(date),]
  repTemp = Republican_tweets[Republican_tweets$rt=='False' & substr(Republican_tweets$created, 1, 10)==as.Date(date),]
  # number of original tweets from that day
  nRepTweets = nrow(repTemp)
  nDemTweets = nrow(demTemp)
  # number of users
  nRepUsers = sum(repFirstTweetDates<=date)
  nDemUsers = sum(demFirstTweetDates<=date)
  # mean sentiment of tweets
  RepMean = mean(repTemp$vader)
  DemMean = mean(demTemp$vader)
  # positive and negative means
  RepPosMean = mean(repTemp$vader[repTemp$vader>0])
  RepNegMean = mean(repTemp$vader[repTemp$vader<0])
  DemPosMean = mean(demTemp$vader[demTemp$vader>0])
  DemNegMean = mean(demTemp$vader[demTemp$vader<0])
  # put in data frame
  repTemp = data.frame('date'=as.Date(date), 'nOrigTweets'=nRepTweets, 'nUsers'=nRepUsers, 'meanSent'=RepMean, 'PosMean'=RepPosMean, 'NegMean'=RepNegMean)
  repDaily = rbind(repDaily, repTemp)
  demTemp = data.frame('date'=as.Date(date), 'nOrigTweets'=nDemTweets, 'nUsers'=nDemUsers, 'meanSent'=DemMean, 'PosMean'=DemPosMean, 'NegMean'=DemNegMean)
  demDaily = rbind(demDaily, demTemp)
  print(as.Date(date))
}

## Frequency of original tweets
# days with highest frequency of tweets
nTopDays = 4
repDaily[order(repDaily$nOrigTweets/repDaily$nUsers, decreasing=TRUE)[1:nTopDays],]
demDaily[order(demDaily$nOrigTweets/demDaily$nUsers, decreasing=TRUE)[1:nTopDays],]

# plots of frequency for Democrats and Republicans
ggplot(repDaily, aes(x=date, y=nOrigTweets/nUsers)) + geom_point() + xlab('Date') + 
  ylab('Average Number of Original Tweets') + ggtitle('Frequency of Republican Tweets') +
  geom_vline(xintercept=c(as.Date('2016-11-08'), as.Date('2017-01-20'))) +
  geom_vline(xintercept=c(as.Date('2016-11-09'), as.Date('2016-10-20'), as.Date('2016-10-10'), as.Date('2016-11-08')),
             linetype='dashed')
ggplot(demDaily, aes(x=date, y=nOrigTweets/nUsers)) + geom_point() + xlab('Date') +
  ylab('Average Number of Original Tweets') + ggtitle('Frequency of Democratic Tweets') +
  geom_vline(xintercept=c(as.Date('2016-11-08'), as.Date('2017-01-20'))) +
  geom_vline(xintercept=c(as.Date('2016-10-10'), as.Date('2016-11-09'), as.Date('2016-10-20'), as.Date('2016-09-27')),
             linetype='dashed')

## Sentiment of original tweets
# all sentiment
bothSentiment = data.frame('date'=c(demDaily$date, repDaily$date), 'sent'=c(demDaily$meanSent, repDaily$meanSent),
                           'party'=c(rep('Democratic', nrow(demDaily)), rep('Republican', nrow(repDaily))))
ggplot(bothSentiment, aes(x=date, y=sent, group=party)) + geom_line(aes(color=party)) +
  scale_color_manual(values=c('blue', 'red')) + ggtitle('Mean Sentiment of Democratic and Republican Tweets') +
  xlab('Date') + ylab('Mean Sentiment') + geom_vline(xintercept=c(as.Date('2016-11-08'), as.Date('2017-01-20')))

nMostExtremeDays = 5
repDaily[order(repDaily$meanSent)[1:nMostExtremeDays],]
repDaily[order(repDaily$meanSent, decreasing=TRUE)[1:nMostExtremeDays],]

demDaily[order(demDaily$meanSent)[1:nMostExtremeDays],]
demDaily[order(demDaily$meanSent, decreasing=TRUE)[1:nMostExtremeDays],]

# difference in sentiment +/- 2 months around election
diffSentiment = data.frame('date'=demDaily$date, 'diff'=demDaily$meanSent-repDaily$meanSent,
                           'posDiff'=demDaily$PosMean-repDaily$PosMean, 'negDiff'=demDaily$NegMean-repDaily$NegMean)
ggplot(diffSentiment, aes(x=date, y=diff)) + geom_line() + xlim(as.Date('2016-09-08'), as.Date('2017-01-08')) +
  geom_vline(xintercept=as.Date('2016-11-08'), color='green') + geom_hline(yintercept=0, color='grey') +
  xlab('Date') + ylab('Difference in Mean Sentiment') + ggtitle('Difference (Democratic-Republican) in Mean Sentiment')

# Difference in positive means
ggplot(diffSentiment, aes(x=date, y=posDiff)) + geom_line(color='grey') +
  geom_vline(xintercept=c(as.Date('2016-11-08'), as.Date('2017-01-20'))) +
  geom_smooth(color='orange', se=FALSE, span=.01) +
  geom_smooth(color='purple', se=FALSE, span=.1) +
  ggtitle('Difference (Democratic-Republican) in Mean of Positive Tweets') +
  xlab('Date') + ylab('Difference in Mean Sentiment')

# Difference in negative means
ggplot(diffSentiment, aes(x=date, y=negDiff)) + geom_line(color='grey') +
  geom_vline(xintercept=c(as.Date('2016-11-08'), as.Date('2017-01-20'))) +
  geom_smooth(color='orange', se=FALSE, span=.01) +
  geom_smooth(color='purple', se=FALSE, span=.1) +  
  ggtitle('Difference (Democratic-Republican) in Mean of Negative Tweets') +
  xlab('Date') + ylab('Difference in Mean Sentiment')
