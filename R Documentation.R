##### resume working #####
data.folder <- "D:\\Stuff\\Uni\\MA Sozialwissenschaften Humboldt-Universität Berlin\\17 (SoSe)\\Web Data Collection\\Twitter Bots\\data"
setwd(data.folder)
source("packages.r")
source("functions.r")
library(httr)
library(scales)
load("rkeys.RDa") 
load("twittertoken.RDa")
load("BTW17_TwitterData.RDa")
load("BTW17_users_botscore_final.RDa")
load("BTW17_botscore_vis.RDa")
load("BTW17_botscore_gone.RDa")
load("BTW17_bot_classification.RDa")
load("BTW17_TwitterData_bots.RDa")



########## Documentation ################

## peparations            -------------------

source("packages.r")
source("functions.r")
library(httr)

TwitterTor_accesstoken <-  "881861582715850752-IP5Mzv5kbiD0HyivFGKh85gJMzfVwco"
TwitterTor_accesssecret <-  "PBhUuri04d9c3QvuO3ID3UOJRC8DPNsbzDRGgbp3svqbE"
TwitterToR_twitterkey <- "e7TlZjM6fpmLWlgxc6wBeyrYO"
TwitterToR_twittersecret <- "oxnbth4ZSzR4UrfWevgdtLsZlBdqVpJqnAjfBKi0WTSZbos2uO"
MashapeKey <- "w47kdFujXYmshjVF6zCGGMaf9vW8p1oGJs1jsnZYNe084jVhPj"

save(TwitterToR_twitterkey,
     TwitterToR_twittersecret,
     MashapeKey,
     TwitterTor_accesssecret,
     TwitterTor_accesstoken,
     file = "rkeys.RDa") # <--- this is where your keys are locally stored!

load("rkeys.RDa") 

dir.create("D:\\Stuff\\Uni\\MA Sozialwissenschaften Humboldt-Universität Berlin\\17 (SoSe)\\Web Data Collection\\Twitter Bots\\data")

data.folder <- "D:\\Stuff\\Uni\\MA Sozialwissenschaften Humboldt-Universität Berlin\\17 (SoSe)\\Web Data Collection\\Twitter Bots\\data"
setwd(data.folder)



## Twitter Setup          -------------------------------



twitter_token <- create_token(
  app = "rtweet_tokes",
  TwitterToR_twitterkey,
  TwitterToR_twittersecret)


save(twitter_token, file = "twittertoken.RDa")

### Twitter Search      ----------------------------------

test <- search_tweets("#afd OR #TrauDichDeutschland", n = 18000, token = twitter_token)
View(test)
save(test, file = "testtweetsafd.rda")
# Tweets am 2.8.17: 5684
# Tweets am 1.8.17: 6511

test2 <- search_tweets("#spd", n = 18000, token = twitter_token)
View(test2)
save(test2, file = "testtweetsspd.rda")
# nur 15,222 Tweets zwischen dem 25.7. und 3.8.17 im Gegensatz zu 17,436 zwischen 31.7. und 3.8.17 für #afd OR #TrauDichDeutschland -> deutlich höheres Tweetvolumen bei weniger followern lässt auf angeregtere Diskussion ODER Bots schließen


# Search API filtert Tweets nach "Relevanz" - unbekannter bias. Stream API evtl geeigneter, aber müsste konstant laufen (-> Ressourcen)



afdtweets <- search_tweets("#afd OR #TrauDichDeutschland", n = 180000, token = twitter_token, retryonratelimit = TRUE) # funktioniert mit retryonratelimit und n>18.000?
View(afdtweets)
save(afdtweets, file = "afdtweets.rda")





#### Botometer API ----------------------

# requires: twitter_token; MashapeKey
# returns "universal" (non-english specific) botscore, since the content to be checked is in german 
BotOmeteR <- function(screen_name) {
  
  user.url <- "https://api.twitter.com/1.1/users/show.json?screen_name="
  timeline.url <- "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="
  mentions.url <- "https://api.twitter.com/1.1/search/tweets.json?q=%40"
  
  user <- GET(paste0(user.url, screen_name), twitter_token)
  # user <- content(user)[1:4]  ?benchmark?
  timeline <- GET(paste0(timeline.url, screen_name, "&count=200&include_rts=true"), twitter_token)
  mentions <- GET(paste0(mentions.url, screen_name, "&count=100"), twitter_token)
  
  payload <- list(
    user = content(user, type="application/json")[1:4],
    timeline = content(timeline, type="application/json"),
    mentions = content(mentions, type="application/json"))
  
  payload.json <- RJSONIO::toJSON(payload, auto_unbox = T)
  
  result = POST("https://osome-botometer.p.mashape.com/2/check_account",
                add_headers(
                  "X-Mashape-Key"=MashapeKey,
                  "Content-Type" = "application/json",
                  "Accept" = "application/json"),
                body=payload.json,  
                encode = "json")
  
  result = content(result, as = "parsed")
  if (is.null(result$scores$universal)){
    return((result$error))}             # catch errors
  else {
    return(result$scores$universal)}

}



#### Data: BTW17 Stream -final- -see btw17 file ---------
load("BTW17_TwitterData.RDa")


#### Data processing ---------------------

#small dataframe to test functions quickly
testDF <- TwitterData[1:200,]
#save(testDF, file = "testDF.RDa")
#load("testDF.RDa")

#### number of tweets per unique user ####
## with plyr ##
# test
user.tweets.test <- ddply(testDF,~screen_name,summarise,number_of_tweets=length(unique(status_id)))

# final
user.tweets <- plyr::ddply(TwitterData,~screen_name,summarise,number_of_tweets=length(unique(status_id)), .progress = "text")
save(user.tweets, file="usersDF.RDa")
load("usersDF.RDa")


## without plyr (with base function aggregate()) to use on severs ##
# test
test.user <- aggregate.data.frame(testDF["screen_name"], by=list(testDF$screen_name), FUN=length)
names(test.user)[names(test.user)=="screen_name"] <- "Tweets"
names(test.user)[names(test.user)=="Group.1"] <- "screen_name"

# final
user.tweets <- aggregate.data.frame(TwitterData["screen_name"], by=list(TwitterData$screen_name), FUN=length)
names(user.tweets)[names(user.tweets)=="screen_name"] <- "Tweets"
names(user.tweets)[names(user.tweets)=="Group.1"] <- "screen_name"
save(user.tweets, file="BTW17_users.RDa")
  # = 200,233 unique users

## Filter for users with a minimum number of tweets (for relevance) ##
# test
active_users.test <- subset.data.frame(user.tweets.test, subset = (Tweets > 3), select = screen_name)
View(active_users.test)

# Threshold: minimum of 27 tweets (at least 1 tweet per day)
active_users <- subset.data.frame(user.tweets, subset = (Tweets > 26))
View(active_users)
  # = 11,898 active users with at least 1 Tweet per day

#### Automated BotOMeter Loop ####
# Test
bottest <- test.user[1:3,]
 # with progress bar
pb = txtProgressBar(min = 0, max = nrow(bottest), initial = 0) 
for(i in 1:length(bottest$screen_name)) {
  user <- bottest$screen_name[i]
  result <- BotOmeteR(user)
  bottest$bot_score[i] <- result
  setTxtProgressBar(pb, i)
}


# final
pb = txtProgressBar(min = 0, max = nrow(active_users), initial = 0) 
for(i in 1:length(active_users$screen_name)) {
  user <- active_users$screen_name[i]
  result <- BotOmeteR(user)
  active_users$bot_score[i] <- result
  setTxtProgressBar(pb, i)
}
save(active_users, file="BTW17_users_botscore.RDa")

### API calls (reruns due to errors)####
#continued 1
pb = txtProgressBar(min = 0, max = nrow(active_users), initial = 1213) 
for(i in 1213:length(active_users$screen_name)) {
  user <- active_users$screen_name[i]
  result <- BotOmeteR(user)
  active_users$bot_score[i] <- result
  setTxtProgressBar(pb, i)
}

save(active_users, file="BTW17_users_botscore2.RDa")


#continued 2
pb = txtProgressBar(min = 0, max = nrow(active_users), initial = 3536) 
for(i in 3536:length(active_users$screen_name)) {
  user <- active_users$screen_name[i]
  result <- BotOmeteR(user)
  active_users$bot_score[i] <- result
  setTxtProgressBar(pb, i)
}

save(active_users, file="BTW17_users_botscore3.RDa")

#continued 3
pb = txtProgressBar(min = 0, max = nrow(active_users), initial = 8321) 
for(i in 8321:length(active_users$screen_name)) {
  user <- active_users$screen_name[i]
  result <- BotOmeteR(user)
  active_users$bot_score[i] <- result
  setTxtProgressBar(pb, i)
}

save(active_users, file="BTW17_users_botscore4.RDa")

#continued 4
pb = txtProgressBar(min = 0, max = nrow(active_users), initial = 10307) 
for(i in 10307:length(active_users$screen_name)) {
  user <- active_users$screen_name[i]
  result <- BotOmeteR(user)
  active_users$bot_score[i] <- result
  setTxtProgressBar(pb, i)
}

save(active_users, file="BTW17_users_botscore5.RDa")




#### Troubleshooting ####

### rerun users that produced errors ###
# json errors
json_errors <- active_users[active_users$bot_score == 'JSON Error',]
pb = txtProgressBar(min = 0, max = nrow(json_errors), initial = 0) 
for(i in 1:length(json_errors$screen_name)) {
  user <- json_errors$screen_name[i]
  result <- BotOmeteR(user)
  json_errors$bot_score[i] <- result
  setTxtProgressBar(pb, i)
}
save(json_errors, file="BTW17_users_botscore_errors1.RDa")
# -> JSON errors not resolvable, need extensive checking - see below
# JSON errors seem to vary (sometimes rerunning returns a score if run on the same user on multiple occasions) -> check API doc


#### bad requests ####
bad_requests <- active_users[active_users$bot_score == 'Bad Request',]
pb = txtProgressBar(min = 0, max = nrow(bad_requests), initial = 0) 
for(i in 1:length(bad_requests$screen_name)) {
  user <- bad_requests$screen_name[i]
  result <- BotOmeteR(user)
  bad_requests$bot_score[i] <- result
  setTxtProgressBar(pb, i)
}
save(bad_requests, file="BTW17_users_botscore_errors2.RDa")
 # "Bad Requests" become JSON Errors -> see below for check


#### default score 0.26 introduced by botometer loop (just in case) ####
sc_026 <- active_users[active_users$bot_score == '0.26',]
pb = txtProgressBar(min = 0, max = nrow(sc_026), initial = 0) 
for(i in 1:length(sc_026$screen_name)) {
  user <- sc_026$screen_name[i]
  result <- BotOmeteR(user)
  sc_026$bot_score[i] <- result
  setTxtProgressBar(pb, i)
}
save(sc_026, file="BTW17_users_botscore_errors2.RDa")

## JSON errors in sc_026
json_errors2 <- sc_026[sc_026$bot_score == 'JSON Error',]
pb = txtProgressBar(min = 0, max = nrow(json_errors2), initial = 0) 
for(i in 1:length(json_errors2$screen_name)) {
  user <- json_errors2$screen_name[i]
  result <- BotOmeteR(user)
  json_errors2$bot_score[i] <- result
  setTxtProgressBar(pb, i)
}
# -> (mostly) resolved




#### checking specific bot scores and error causes ####
BotOmeteR_full <- function(screen_name) {
  
  user.url <- "https://api.twitter.com/1.1/users/show.json?screen_name="
  timeline.url <- "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="
  mentions.url <- "https://api.twitter.com/1.1/search/tweets.json?q=%40"
  
  user <- GET(paste0(user.url, screen_name), twitter_token)
  # user <- content(user)[1:4]  ?benchmark?
  timeline <- GET(paste0(timeline.url, screen_name, "&count=200&include_rts=true"), twitter_token)
  mentions <- GET(paste0(mentions.url, screen_name, "&count=100"), twitter_token)
  
  payload <- list(
    user = content(user, type="application/json")[1:4],
    timeline = content(timeline, type="application/json"),
    mentions = content(mentions, type="application/json"))
  
  payload.json <- RJSONIO::toJSON(payload, auto_unbox = T)
  
  result = POST("https://osome-botometer.p.mashape.com/2/check_account",
                add_headers(
                  "X-Mashape-Key"=MashapeKey,
                  "Content-Type" = "application/json",
                  "Accept" = "application/json"),
                body=payload.json,  
                encode = "json")
  
  result = content(result, as = "parsed")
  return(result)
}


## 1: user with 7709 Tweets, whose botcheck produced a json error

BotOmeteR_full("Teletubbies007")  
# $message
# [1] "'user' must be provided as a Twitter User object"

GET("https://api.twitter.com/1.1/users/show.json?screen_name=Teletubbies007", twitter_token) %>% content(as = "parsed")
# $errors[[1]]$message
# [1] "User has been suspended."


## 2: user with 3550 Tweets, whose botcheck produced a json error
BotOmeteR_full("Video_Wuppertal")
# $message
# [1] "'user' must be provided as a Twitter User object"
GET("https://api.twitter.com/1.1/users/show.json?screen_name=Video_Wuppertal", twitter_token) %>% content(as = "parsed")
# $errors[[1]]$message
# [1] "User has been suspended."


#### JSON Errors ####

## loop for overview of error causes
for(i in 1:length(json_errors$screen_name)) {
  user <- json_errors$screen_name[i]
  result <- BotOmeteR_full(user)
  if (!is.null(result$scores$universal)){
    json_errors$bot_score[i] <- result$scores$universal
    json_errors$error_message[i] <- "no error"
  } else
    if(!is.null(result$error) & !is.null(result$message)){
  json_errors$bot_score[i] <- result$error
  json_errors$error_message[i] <- result$message}
      else
        json_errors$error_message[i] <- "other"
}
save(json_errors, file="BTW17_json_errors.RDa")

## check cause of error for 'timeline' and 'user' errors in json_errors via Twitter API 
json_errors$error_cause <- NA
for(i in 1:length(json_errors$screen_name)) {
  screen_name <- json_errors$screen_name[i]
  
  # try to get user data error message if user data is the cause
  if (str_detect(json_errors$error_message[i], "'user' must be provided as a Twitter User object")==TRUE){
    user.url <- "https://api.twitter.com/1.1/users/show.json?screen_name="
    error_cause <- GET(paste0(user.url, screen_name), twitter_token) %>% content(as = "parsed")
    if (!is.null(error_cause$errors[[1]]$message[1])){
      json_errors$error_cause[i] <- error_cause$errors[[1]]$message[1]}
    else json_errors$error_cause[i] <- paste0("Error: ", error_cause$error[[1]])} 
  
# try to get timeline data error message
  if (str_detect(json_errors$error_message[i], "'timeline' must be provided as an array of Tweet objects")==TRUE){
    timeline.url <- "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name="
    error_cause <- GET(paste0(timeline.url, screen_name, "&count=200&include_rts=true"), twitter_token) %>% content(as = "parsed")
    if (!is.null(error_cause$errors[[1]]$message[1])){
    json_errors$error_cause[i] <- error_cause$errors[[1]]$message[1]}
    else json_errors$error_cause[i] <- paste0("Error: ", error_cause$error[[1]])}

# try to get mentions data error message 
if (str_detect(json_errors$error_message[i], "'mentions' must be provided as an array of Tweet objects")==TRUE){
  mentions.url <- "https://api.twitter.com/1.1/search/tweets.json?q=%40"
  error_cause <- GET(paste0(mentions.url, screen_name, "&count=100"), twitter_token)
  if (!is.null(error_cause$errors[[1]]$message[1])){
    json_errors$error_cause[i] <- error_cause$errors[[1]]$message[1]}
  else json_errors$error_cause[i] <- paste0("Error: ", error_cause$error[[1]])}
}
View(json_errors)
save(json_errors, file="BTW17_json_errors.RDa")

# rerun 'mentions' errors (seemingly no cause of error)
for (i in 1:length(json_errors$screen_name)){
  if (str_detect(json_errors$error_message[i], "'mentions' must be provided as an array of Tweet objects")==TRUE){
    user <- json_errors$screen_name[i]
    result <- BotOmeteR(user)
    json_errors$bot_score[i] <- result}
}
for (i in 1:length(json_errors$screen_name)){
  if (str_detect(json_errors$bot_score[i], "JSON Error")==FALSE){
    json_errors$error_message[i] <- NA
    json_errors$error_cause[i] <- NA
  }}
 # -> resolved
save(json_errors, file="BTW17_json_errors.RDa")

# Timeline errors cause "Not authorized" indicates protected timelines, i.e. only confirmed followers have access

# User errors: caused either by deleted ("User not found") or suspended ("User hase been suspended) accounts -> might indicate bot accounts


## recode user json errors into cause of the error
json_errors[is.na(json_errors)] <- "none"
for (i in 1:nrow(json_errors)){
  if (str_detect(json_errors$error_message[i], "'user' must be provided as a Twitter User object")==TRUE){
    json_errors$bot_score[i] <- json_errors$error_cause[i]}
  if (str_detect(json_errors$error_cause[i], "Error: Not authorized."))
    json_errors$bot_score[i] <- "Timeline Error: Not authorized"}
save(json_errors, file="BTW17_json_errors.RDa")


### "Bad Requests" subset
bad_requests$error_message <- NA
for(i in 1:length(bad_requests$screen_name)) {
  user <- bad_requests$screen_name[i]
  result <- BotOmeteR_full(user)
  if (!is.null(result$scores$universal)){
    bad_requests$bot_score[i] <- result$scores$universal
    bad_requests$error_message[i] <- "no error"
  } else
    if(!is.null(result$error) & !is.null(result$message)){
      bad_requests$bot_score[i] <- result$error
      bad_requests$error_message[i] <- result$message}
  else
    bad_requests$error_message[i] <- "other"
}
# one error resolved by re-running, else server error
# 13 faulty entries here, can be disregarded (no further troubleshooting)



#### combine subsets of re-checked botscores with main data ####
active_users_1 <- active_users
save(active_users_1, file = "BTW17_users_botscore_v1.RDa")

# (mostly resolved) JSON errors in sc_026
sc_026_2 <- rbind(sc_026[!sc_026$bot_score == 'JSON Error',], json_errors2)
View(sc_026_2)
active_users_2 <- rbind(active_users[!active_users$bot_score == '0.26',], sc_026_2)
View(active_users_2)
save(active_users_2, file="BTW17_users_botscore_v2.RDa")

# rechecked, recoded and partly resolved json errors
json_errors_sm <- subset.data.frame(json_errors, select = c(screen_name, Tweets, bot_score))
active_users_3 <- rbind(active_users_2[!active_users_2$bot_score == 'JSON Error',], json_errors_sm)
View(active_users_3)
save(active_users_3, file="BTW17_users_botscore_v3.RDa")
# 1 entry lost since rechecking 0.26 scores produced 1 Botscore:"JSON error" entry which the application of rbind does not catch

# bad requests
bad_requests_sm <- subset.data.frame(bad_requests, select = c(screen_name, Tweets, bot_score))
active_users_4 <- rbind(active_users_3[!active_users_3$bot_score == 'Bad Request',], bad_requests_sm)
View(active_users_4)
save(active_users_3, file="BTW17_users_botscore_v4.RDa")

## Final Dataframe of active users, including their botscore and error messages ##
active_users <- active_users_4
save(active_users, file="BTW17_users_botscore_final.RDa")




#### Data Visualisation ----------------------

#### Amount of Tweets (% of total population) by "active_users" ####
TweetDistribution <- data.frame(
  user_group = c("active users", "other users"),
  Tweets = c(sum(active_users$Tweets), (nrow(TwitterData)-sum(active_users$Tweets))),
  users = c(nrow(active_users), nrow(user.tweets)))

### Pie chart ###
# make blank theme
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

TweetDistribution_pie <- ggplot(data=TweetDistribution, aes(x="", y=Tweets, fill=user_group)) +
   geom_bar(stat="identity") +
   coord_polar("y") +
   scale_fill_brewer(palette = "Purples", direction = -1) +
   blank_theme +
   theme(axis.text.x=element_blank()) +
   geom_text(aes(label = Tweets/1000), position = position_stack(vjust = 0.5))
ggsave(TweetDistribution_pie, "TweetDistribution.jpeg")
# Tweets by user group, in housands

# 1,533,311 of a total 2,066,488 Tweets are published by the "active_users" -> about 3/4th of the tweets published are checked for the likelihood of their author being a bot
# (11,897 of 200,223 total users in the sample are classified as "active_users with 27 or more Tweets published during the 27 day period of the observation)

  

#### Make dataset for visualisation ####

##set bot_score variable to num, catch deleted and suspended Users in extra DF
active_users_vis_na <- active_users
active_users_vis_na$bot_score <- as.numeric(active_users_vis_na$bot_score)
#delete NAs for easier handling:
active_users_vis <- na.omit(active_users_vis_na)

active_users_gone <- subset(active_users, bot_score == "User not found."| active_users$bot_score == "User has been suspended.")
# Errors "Bad Request", "Not Authorized", "No Timeline" are gone from the graph dataset (all non-numerical values are set to NA); "User not found" and "User suspended" are caught in the active_users_gone dataset since they may be an indicator for (now deleted or banned) bot accounts
save(active_users_vis, file="BTW17_botscore_vis.RDa")
save(active_users_gone, file = "BTW17_botscore_gone.RDa")



#### Density of botscores ####

## Pointplot for first Overview
ggplot(data=active_users_vis,aes(x=bot_score, y=Tweets)) + 
      geom_point()

## Density plot
density(active_users_vis$bot_score)
DensityBotscoreGraph <- ggplot(data = active_users_vis) + geom_density(aes(x=bot_score))
  # shows probability of a user reaching a certain botscore 
  # -> comparable to Bessi/Ferrara density score ?



#### Categorization: Bot, Not-Bot, unknown ####

## Threshold for Bots ##
 # classified as Bot if bot_score at least 0.5, NAs (errors) classified as "unkown"
active_bots <- active_users_vis_na
active_bots$bot <- NA
active_bots[is.na(active_bots$bot_score), "bot"] <- "unknown"
active_bots[is.na(active_bots$bot_score), "bot_score"] <- "-1"
active_bots[active_bots$bot_score >= 0.5, "bot"] <- "yes"
active_bots[active_bots$bot_score <= 0.5 & !active_bots$bot_score == -1, "bot"] <- "no"
save(active_bots, file="BTW17_bot_classification.RDa")
 # 53 users with botscore exactly 0.5
 # 1,632 users with botscore between 0.4 and 0.6


## Summarize Bot Tweets ##
table(active_bots$bot)
str_detect(active_users_gone$bot_score, "User not found.") %>% sum()
str_detect(active_users_gone$bot_score, "User has been suspended.") %>% sum()
 # 10,630 human users, 389 bots, 878 unkown (3.3% bots)
        # of 878 unkown 495 users "not found" and 267 "suspended"
          # if "suspendend" and "not found" (likely deleted) accounts are considered bots, the sample contains 9.7% bots
        

ggplot(data = active_bots, aes(x = bot, y = Tweets)) + 
  geom_histogram(stat = "identity") +
  scale_y_continuous(labels = comma)

sum(active_bots$Tweets[active_bots$bot == "yes"])
 # 62,459 Tweets by bots (160.563 avg. Tweets / User)

sum(active_bots$Tweets[active_bots$bot == "no"])
 # 1,330,793 Tweets by humans (125.1922 avg. Tweets / Use)

sum(active_bots$Tweets[active_bots$bot == "unknown"])
sum(active_users_gone$Tweets[active_users_gone$bot_score == "User not found."])
sum(active_users_gone$Tweets[active_users_gone$bot_score == "User has been suspended."])
 # 140,059 Tweets by "unkown" (159.5205 avg. Tweets / Use)
  # of which 65,392 were published by "not found" users and 55,160 by "suspended" users



#### Timeline of Tweet activity by category ####

### Merge Bot categorization with original dataset of Tweets (keep only active users) ###
## test
test_active <- testDF[which(testDF$screen_name %in% active_users$screen_name),]
test_active <- merge.data.frame(testDF, active_users, by = "screen_name")
test_active <- merge.data.frame(test_active, active_bots[c("screen_name", "bot")], by = "screen_name")
View(test_active)

## active user-only Datasaet of Tweets with botscore and bot: yes/no/unkown classification
TwitterData_bots <- TwitterData[which(TwitterData$screen_name %in% active_users$screen_name),]
TwitterData_bots <- merge.data.frame(TwitterData, active_users, by = "screen_name")
TwitterData_bots <- merge.data.frame(TwitterData_bots, active_bots[c("screen_name", "bot")], by = "screen_name")
save(TwitterData_bots, file="BTW17_TwitterData_bots.RDa")


### Timeline of Tweet activity for bots and non-bots ###
## test
random_sample <- TwitterData_bots[sample(nrow(TwitterData_bots), 50000),]
ggplot(data = random_sample, aes(x = created_at, color = bot)) +
  geom_line(stat="bin", bins = 45) + scale_color_discrete() + labs(title = "Timeline of Tweet Volume for Bots and non-Bots", x = "Time", y ="Tweets")

# drop "unkown"s
random_sample2 <- random_sample[!str_detect(random_sample$bot,"unknown"),]

# relabel
random_sample2$bot[random_sample2$bot == "yes"] <- "bot"
random_sample2$bot[random_sample2$bot == "no"] <- "human"
names(random_sample2)[names(random_sample2) == 'bot'] <- 'Botometer'

# plot
ggplot(data = random_sample2, aes(x = created_at, color = Botometer)) +
  geom_line(stat="bin") + labs(x = "Time", y ="Tweets")


## final

# relabel
TwitterData_bots2 <- TwitterData_bots
TwitterData_bots2$bot[TwitterData_bots2$bot == "yes"] <- "bot"
TwitterData_bots2$bot[TwitterData_bots2$bot == "no"] <- "human"
names(TwitterData_bots2)[names(TwitterData_bots2) == 'bot'] <- 'Botometer'

# with "unknown"s
ggplot(data = TwitterData_bots2, aes(x = created_at, color = Botometer)) +
  geom_line(stat="bin", bins = 45) +
  labs(x = "Time", y ="Tweets") +
  scale_y_continuous(labels = comma) +
  theme_bw(base_size=15) 
  



# without "unkown"s
TwitterData_bots3 <- TwitterData_bots2[!str_detect(TwitterData_bots2$Botometer,"unknown"),]

ggplot(data = TwitterData_bots3, aes(x = created_at, color = Botometer)) +
  geom_line(stat="bin", bins = 45) + 
  labs(x = "Time", y ="Tweets") +
  scale_y_continuous(labels = comma) +
  theme_bw(base_size=15) 

### !! pronounced drop in Tweets on September 16th due to failure of the stream collecting the tweets. Even though measures were taken to retrieve the lost data, it was not possible to recollect most of the tweets published that day 
  # (all other stream failures were recollected and show no disruption in the data)




#### Retweet / Tweet ratio ####
sum(TwitterData_bots$is_retweet[TwitterData_bots$bot == "yes"])
 # 36,764 of 62,459 tweets by bots are retweets (0.5886101)

sum(TwitterData_bots$is_retweet[TwitterData_bots$bot == "no"])
 # 949,087 of 1,330,793 tweets by humans are retweets (0.713174)

sum(TwitterData_bots$is_retweet[TwitterData_bots$bot == "unknown"])
 # 92,993 of 140,059 tweets by "unkown" are retweets (0.6639559)





#### import results from Python Botometer package (not needed)####
# List of users to be exported to Python as a .csv file:
user.list.test <- users.test$screen_name
write.table(users.test$screen_name, file = "users.test.csv", row.names = F, col.names = F)

user.list <- users$screen_name
write.table(users$screen_name, file = "users.csv", row.names = FALSE, col.names = FALSE)


# Results from the Python Botometer package
Botometer.results <- fromJSON("botometer-results.test.json", simplifyVector = FALSE) %>% ldply(data.frame)
results <- Botometer.results[ , colSums(is.na(Botometer.results)) == 0]
View(results)







