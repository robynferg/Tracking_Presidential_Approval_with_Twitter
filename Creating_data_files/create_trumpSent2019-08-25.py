## calculate daily mean and standard deviation of "Trump" tweets

import os
import csv
import nltk
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
import numpy as np

sid = SentimentIntensityAnalyzer()

# change directory to folder containing csv files of "Trump" tweets
# each csv contains a week of "Trump" tweets with columns: 
os.chdir(' ')

allFiles = os.listdir()
date = list() # date of "Trump" tweets
means = list() # daily mean sentiments
sds = list() # daily standard deviation of sentiments

for file in allFiles:
	# read in csv file
	fileRead = csv.reader(open(file, encoding="ISO-8859-1"))
	tweets = list()
	fileDates = list()
	for row in fileRead:
		tweets.append(row[1])
		fileDates.append(row[5][0:10])
	tweets = tweets[1:]
	fileDates = fileDates[1:]

	# calculate daily sentiment for each day in csv file
	for day in list(set(fileDates)):
		date.append(day)
		allSents = [sid.polarity_scores(tweets[i])['compound'] for i in range(len(tweets)) if fileDates[i]==day]
		means.append(np.mean(allSents))
		sds.append(np.std(allSents))


# write to csv file
os.chdir(' ')
with open('trumpSent2019-08-25.csv', 'w', encoding="ISO-8859-1") as output:
	writer = csv.writer(output, lineterminator='\n')
	writer.writerow(['date', 'allMean', 'sd'])
	for i in range(len(date)):
		writer.writerow([date[i], means[i], sds[i]])
