## create_placebo_sentiments2019-08-25.py
## calculate daily mean sentiment for tweets containing placebo words

import csv
import os
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
import numpy as np
import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer
import operator
import datetime
import string

sid = SentimentIntensityAnalyzer()

# function to specify a word and output the mean sentiment of tweets containing that word
def wordTimeSeries(word, tweets, dates, verbose=True):
	uniqueDates = list(set(dates))
	dailySentiment = list()
	nDays = len(uniqueDates)
	for day in uniqueDates:
		# get all tweets for that day that include word
		dayTweets = [tweets[i] for i in range(len(tweets)) if dates[i]==day and word in tweets[i].lower()]
		dailySentiment.append(np.mean([sid.polarity_scores(tweet)['compound'] for tweet in dayTweets]))
		if verbose:
			print(uniqueDates.index(day), 'of', nDays)
	# return as pandas data frame
	sentSeries = pd.Series(dailySentiment, index=uniqueDates)
	return sentSeries.to_frame()


# read in stream of tweets
# each csv contains 5000 tweets per day for an entire week
os.chdir(' ')
allFiles = os.listdir()

text = list()
created = list()
for file in allFiles:
	fileRead = csv.reader(open(file, encoding="ISO-8859-1"))
	i = 0
	for row in fileRead:
		if i>0:
			text.append(row[1])
			created.append(row[5][0:10])
		i = i+1
created = [datetime.datetime.strptime(created[i], '%Y-%m-%d') for i in range(len(created))]

# get list of all words that appear nearly daily
words = list()
vectorizer = CountVectorizer()
for day in set(created):
	if day <= datetime.datetime.strptime('2019-08-25', '%Y-%m-%d'):
		relevantTweets = [text[i] for i in range(len(text)) if created[i]==day]
		X = vectorizer.fit_transform(relevantTweets)
		words.append([v[0] for v in sorted(vectorizer.vocabulary_.items(), key=operator.itemgetter(1))])
dailyWordsStop = set(words[0]).intersection(*words) # get intersection--words that appear every day

# remove stopwords
stopwords = ["ourselves", "hers", "between", "yourself", "but", "again", "there", "about", "once", "during", "out", "very", "having", "with", "they", "own", "an", "be", "some", "for", "do", "its", "yours", "such", "into", "of", "most", "itself", "other", "off", "is", "s", "am", "or", "who", "as", "from", "him", "each", "the", "themselves", "until", "below", "are", "we", "these", "your", "his", "through", "don", "nor", "me", "were", "her", "more", "himself", "this", "down", "should", "our", "their", "while", "above", "both", "up", "to", "ours", "had", "she", "all", "no", "when", "at", "any", "before", "them", "same", "and", "been", "have", "in", "will", "on", "does", "yourselves", "then", "that", "because", "what", "over", "why", "so", "can", "did", "not", "now", "under", "he", "you", "herself", "has", "just", "where", "too", "only", "myself", "which", "those", "i", "after", "few", "whom", "t", "being", "if", "theirs", "my", "against", "a", "by", "doing", "it", "how", "further", "was", "here", "than", "trump"]
dailyWords = [w for w in dailyWordsStop if w not in stopwords]

# combine time series for each word time series into one dataframe
wordSeries = wordTimeSeries(dailyWords[0], text, created)
wordSeries.columns = ['meanSent'+dailyWords[0]]
for word in dailyWords[1:]:
	timeSeries = wordTimeSeries(word, text, created, verbose=False)
	timeSeries.columns = ['meanSent'+word]
	wordSeries = pd.concat([wordSeries, timeSeries], axis=1)
	print('Finished word ', dailyWords.index(word), ' of ', len(dailyWords))

# write to csv
os.chdir(' ')
wordSeries.to_csv('placebo_sentiments2019-08-25.csv')
