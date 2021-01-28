library(rtweet)
library(twitteR)

#Get data, Twitter APIs
api_key <- "l012iwWaXGlUiM3joJ6GcNvUu"
api_secret <- "iIhHjnGSc6uAnqLwU96E0OMxoLaMgN5TtEuzYEpgd3Z64XjiZu"
access_token <- "633716914-ut2Sas6DnQBMyx8cKCd6WSxcSdDzEni2vNjKl9Vw"
access_token_secret <- "Ms8dWfVyBRDHKcP8EHdMVsG4ZfuEnIHGkFjZfL0mq3DGA"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

vac_tweets <- search_tweets("covid vaccine", n=500, include_rts = FALSE, lang="en")
write.csv(vac_tweets,file = 'datasettweet.csv')