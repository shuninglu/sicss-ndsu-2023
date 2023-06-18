#######################################
############SICSS-NDSU 2023############
#######################################

##Day 3 Collecting Digital Trace Data##
##       Part 2: Reddit API          ## 
##       Author: Shuning Lu        ####


#install and load packages::RedditExtractoR, pacman, dplyr, ggplot2
#The Reddit API allows user to make 60 requests per minute (1 request per second)

#load library
library(RedditExtractoR)
library(dplyr)
library(ggplot2)
library(pacman)
p_load('kableExtra')

#subreddit query using keyword.
#get subreddits that contain the keyword "fargo" as the attributes
subreddits_fargo <- find_subreddits("fargo")

head(subreddits_fargo)
str(subreddits_fargo)

#use this to save your dataset for future use
saveRDS(subreddits_fargo, file = 'subreddits_fargo.RDS')

#get top 20 subreddits that contains "fargo" 
subreddits_fargo1 <- subreddits_fargo %>%
  select(subreddit, title, subscribers) %>% #select variables you want to explore
  mutate(subscribers_million = subscribers/1000000, subscribers = NULL, title = paste0('"',title,'"')) %>% # creates new variables
  arrange(desc(subscribers_million)) #arrange data from highest subscriber count
subreddits_fargo1 
rownames(subreddits_fargo1) <- NULL #remove the unique id of each subreddit

#use a table to display results
kable(head(subreddits_fargo1, 20)) %>%
  kable_styling(position = 'center')


#thread query using keyword.
#get top posts containing "fargo" in a month
fargo_urls <- find_thread_urls(
  keywords = "fargo", 
  sort_by = "comments", #you can also sort by (relevance, comments, new, hot, top)
  period = "month" #you can use (hour, day, week, month, year, all)
  )

#click on collected data or use head/str function below, answer:
#what else does this query help you accomplish?

head(fargo_urls)
str(fargo_urls)

#which subreddits have the highest comments related to "fargo"?
fargo_urls2 <- fargo_urls %>% 
  group_by(subreddit) %>%  mutate(count = n()) %>% 
  filter(count>2)
ggplot(fargo_urls2, aes(x = reorder(subreddit,-count))) +
  geom_bar(stat="count")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90, hjust=1))


#thread query in a given subreddit 
#get top posts from r/fargo subreddit
top_fargo_urls <- find_thread_urls(
  subreddit = "fargo", 
  sort_by = "top"
  ) 

#click on collected data or use head/str function below, answer:
#what does this query help you accomplish?

head(top_fargo_urls)
str(top_fargo_urls)


#how to get top posts from r/fargo subreddit having keywords "fargo" in a year
#try it by yourself
#please use ?find_thread_urls for usage 
#insert your code here
#...
#...
#...
#...
#...


#get comments in the first ten of the obtained threads
threads_contents <- get_thread_content(top_fargo_sub_urls$url[1:10])  

#it returns two dataframes from the specified threads
#one is metadata of the thread, the other is comment in each thread
str(threads_contents$threads) #thread metadata
str(threads_contents$comments) #comments metadata

#get the first two comments
threads_contents$comments$comment[1:2] 

#does upvotes relate to comments in a given thread?
ggplot(threads_contents$threads, aes(x=upvotes, y=comments)) + 
  geom_point()

#does upvotes related to the word count of a given comment?
#get the word count of a given comment
threads_contents$comments$wordcount <- 
  sapply(strsplit(threads_contents$comments$comment, " "), length)

#insert your code here
#...
#...
#...
#...
#...


#get user information, using nasa as an example
nasa <- get_user_content('nasa') #you can also use c('user1', 'user2') to collect data from multiple users

#get the user's account description
nasa_about <- unlist(nasa$nasa$about) 

#get the threads initiated by the user
nasa_threads <- nasa$nasa$threads 

#get the user's comments
nasa_comments <- nasa$nasa$comments 

# check the most popular threads
# clean the dataset
nasa_threads <- nasa_threads %>%
  select(subreddit, title, score) %>% #get the variables we want to use
  mutate(title = paste0('"',title,'"')) %>% #add quotation marks to the titles
  arrange(desc(score)) #arrange the data from with the highest score at the top

rownames(nasa_threads) <- NULL
# a table for viewing where sana posts
kable(head(nasa_threads, 10)) %>% #you can change 10 into 20, 30 to get more info
  kable_styling(position = 'center')

#Now it's your turn to check the most popular comments
#insert your code here
#...
#...
#...
#...
#...


