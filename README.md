# Tracking_Presidential_Approval_with_Twitter
Data and scripts for 'Tracking Presidential Approval with Twitter: A Critical Comparison of Cross-Sectional and Longitudinal Analyses'

Data sets:
- approval_topline2019-08-30.csv: Daily presidential approval data downloaded from https://projects.fivethirtyeight.com/trump-approval-ratings/?ex_cid=rrpromo on August 30, 2019. 
- placebo_sentiments2019-08-25.csv: Daily mean sentiment of tweets containing each of the placebo words from January 16, 2017 through August 25, 2019.
- trumpSent2019-08-25.csv: Daily mean sentiment of 1000 tweets per day containing the word "Trump" from January 17, 2017 through August 23, 2019.

Scripts:
- Cross-Sectional Analysis.R: R script for cross-sectional analysis. Using 'approval_topline2019-08-30.csv', 'placebo_sentiments2019-08-25.csv', and 'trumpSent2019-08-25.csv' data files, calculates optimal correlation between "Trump" tweets and presidential approval, finds reference distribution, and how these change over time as more data is gathered. 
- Longitudinal Analysis.R: R script for longitudinal analysis. Creates plots for frequency and sentiment of tweets for Democrats and Republicans from 2016 through mid-2017 to find political signal.
