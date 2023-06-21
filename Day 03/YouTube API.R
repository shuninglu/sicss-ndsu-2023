#######################################
############SICSS-NDSU 2023############
#######################################

##Day 3 Collecting Digital Trace Data##
##        Part 3: YouTube API        ## 
##        Author: Shuning Lu         ##


#install package::httr, tidyverse, ggplot2, here, jsonlite, dplyr, tuber, purrr
#load package

library(httr)
library(jsonlite)
library(here)
library(dplyr)
library(ggplot2)
library(tuber)  
library(tidyverse)  
library(purrr)  


##Use the basic API
key <- "INSERT YOUR KEY HERE"

#go to this site to find out channel id: https://commentpicker.com/youtube-channel-id.php
HYBELABELID<-"UC3IZKseVpdzPSBaWxBxundA" #channel id
BTSID <- "BTS" #user id
base<- "https://www.googleapis.com/youtube/v3/"

#get channel info with channel id for HYBELABEL channel
api_params <- 
  paste(paste0("key=", key), 
        paste0("id=", HYBELABELID), 
        "part=statistics",
        sep = "&")
api_call <- paste0(base, "channels", "?", api_params)
api_result <- GET(api_call)
json_result <- httr::content(api_result, "text", encoding="UTF-8")

#format json into dataframe
channel.json <- fromJSON(json_result, flatten = T)
channel.HYBE <- as.data.frame(channel.json)

#alternatively, you can use a username for BTS channel. 
#For part you can also insert more at once to get additional information.
api_params2 <- 
  paste(paste0("key=", key), 
        paste0("forUsername=", BTSID), 
        "part=snippet,contentDetails,statistics",
        sep = "&")
api_call2 <- paste0(base, "channels", "?", api_params2)
api_result2 <- GET(api_call2)
json_result2 <- httr::content(api_result2, "text", encoding="UTF-8")

#format json into dataframe
channel.json2 <- fromJSON(json_result2, flatten = T)
channel.BTS <- as.data.frame(channel.json2)


#Compare the two channels by some indicators like views etc. We need to subset the data and create new indicators.
HYBEchannel <- channel.HYBE %>% mutate(Channel = "HYBE",
                                       Views= as.numeric(items.statistics.viewCount), 
                                       Videos = as.numeric(items.statistics.videoCount),
                                       averViewsVideo = Views/Videos, 
                                       Subscriber = as.numeric(items.statistics.subscriberCount)) %>%
  select(Channel,Views,Videos, averViewsVideo, Subscriber)

BTSchannel <- channel.BTS %>% mutate(Channel = "BTS",
                                     Views= as.numeric(items.statistics.viewCount), 
                                     Videos = as.numeric(items.statistics.videoCount),
                                     averViewsVideo = Views/Videos, 
                                     Subscriber = as.numeric(items.statistics.subscriberCount)) %>%
  select(Channel,Views,Videos, averViewsVideo, Subscriber)

#aggregate data into the same dataframe
AGGchannels <- rbind(HYBEchannel, BTSchannel)
AGGchannels


##Use OAuth credentials to get more information
#documentation of tuber package https://cran.r-project.org/web/packages/tuber/tuber.pdf

client_id<-"INSERT YOUR CLIENT ID HERE"
client_secret<-"INSERT YOUR SECRET HERE"

yt_oauth(app_id = client_id,
         app_secret = client_secret,
         token = '')

#get playlist data
#use the tuber function get_playlist_items to collect the videos into a data frame
go_bts <- get_playlist_items(filter = c(playlist_id = "PL5hrGMysD_GuhcQJ2qaMDi__3MlZSsddA"), 
                               part = "contentDetails") 

#Based on the video ids of all videos in that playlist, let's extract video id´s 
go_bts_ids <- (go_bts$contentDetails.videoId)

#Use the map_df command of the purrr package which needs a function as a parameter
#create a function using the get_stats function of the tuber package 
get_all_stats <- function(id) {
  get_stats(video_id = id)
} 

#Store the stats information in a data frame with the map_df command which needs an object .x and a function .f
go_bts_AllStats <- map_df(.x = go_bts_ids, .f = get_all_stats)

#Overview of the statistics of all videos in the playlist
go_bts_AllStats

#get all comments from the 12th video
comments12 <- get_all_comments("hANHIFPst5k")

str(comments12)
head(comments12)

#how to get related videos of a given video
#read the documentation and try it by yourself
#insert your code here
#...
#...
#...
#...
#...


