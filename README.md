# Tracking_Presidential_Approval_with_Twitter
Data and scripts for 'A Critical Evaluation of Tracking Surveys with Social Media: A Case Study in Presidential Approval' by Ferg, Conrad, Gagnon-Bartsch

Data sets:
- approval_topline2019-08-30.csv: Daily presidential approval data as downloaded from https://projects.fivethirtyeight.com/trump-approval-ratings/?ex_cid=rrpromo on August 30, 2019.
- placebo_sentiments2019-08-25.csv: Daily mean sentiment of tweets containing each of the placebo words from January 16, 2017 through August 25, 2019.
- trumpSent2019-08-25.csv: Daily mean sentiment of 1000 tweets per day containing the word "Trump" from January 17, 2017 through August 23, 2019.
- Tweets from our set of Democrats and Republicans are under 'Releases'. To protect the privacy of these users, we do not include any identifying information about the users or their tweets. 

Scripts:
- Cross-Sectional Analysis.R: R script for cross-sectional analysis. Using 'approval_topline2019-08-30.csv', 'placebo_sentiments2019-08-25.csv', and 'trumpSent2019-08-25.csv' data files, calculates optimal correlation between "Trump" tweets and presidential approval, finds reference distribution, and how these change over time as more data is gathered. 
- Longitudinal Analysis.R: R script for longitudinal analysis. Creates plots for frequency and sentiment of tweets for Democrats and Republicans from 2016 through mid-2017 to find evidence of a political signal.

We also provide code for how we create 'trumpSent2019-08-25.csv' from csv files of raw tweets in the 'Creating_data_files' folder.
