
library(twitteR)
install.packages('rtweet')
library(rtweet)
install.packages('ggmap')
library('ggmap')
install.packages("openxlsx", dependencies = TRUE)
library(openxlsx)
doInstall <- TRUE
toInstall <- c( "twitteR","dismo", "maps", "ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

api_key <- 'Jp60fiXqcrPERr99huKZHo0j2'
api_secret_key <- 'FshigJziR49mXK4xooijtTTHb5JsciNuxJhoThPWwYGzJLzCrj'
access_token <- '398887985-YNwM0zv5Jviy5dXIMgdyNah17G4UpSlUrTXBgoSa'
access_token_secret <- 'sS8QkFoOx0pJPJsAsL6ttmDIt90t2rBwpfb8g8nTNGUce'

install.packages('rtweet')
library(rtweet)

setup_twitter_oauth(api_key, api_secret_key, access_token, access_token_secret)

#Scraping the tweets based on keyword
tweets <- searchTwitter(c('#GameStop',"#StopTheSteal"), n=5000, lang = 'en')
tweets

#removing retweets
noretweets <- strip_retweets(tweets, strip_manual = TRUE)

tweetFrame <- twListToDF(noretweets)  # Convert to a nice dF
tweetFrame
summary(tweetFrame)

#user profile of the above tweets
userInfo <- lookupUsers(tweetFrame$screenName)  # Batch lookup of user info
userFrame <- twListToDF(userInfo)  # Convert to a nice dF
summary(userFrame)

#Substring from the status source will be extracted to obtain the source device,client of the tweet
install.packages("stringr")
library(stringr)

#extracting the device type used for tweets

tweetFrame$statusSource <- str_sub(sapply(strsplit(tweetFrame$statusSource, ">"), "[", 2),,-4)

#common column in both data frames
userFrame$screenName
tweetFrame$screenName


#merging the tweet and user dataframes

df <- merge(tweetFrame,userFrame, by = "screenName")
df
summary(df)

write.xlsx(df, file="gamestop.xlsx")



