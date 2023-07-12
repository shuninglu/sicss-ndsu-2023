library(rvest)
library(tidyverse)

# Function to extract review comments from a given URL
extract_reviews <- function(url) {
  page <- read_html(url)
  
  # Extract review text
  review_text <- page %>%
    html_elements(xpath="//p[starts-with(@class, 'comment')]") %>%
    html_text()
  
  return(review_text)
}

# Base URL of the restaurant's Yelp page
base_url <- "https://www.yelp.com/biz/seva-indian-cuisine-astoria-5?osq=Bangladeshi+Restaurant"

# Initialize an empty vector to store all review comments
all_reviews <- c()

# Iterate over all 143 pages of reviews
for (page_num in 0:142) {
  start_index <- page_num * 10
  url <- paste0(base_url, "?start=", start_index)
  reviews <- extract_reviews(url)
  all_reviews <- c(all_reviews, reviews)
}

# Print all review comments
print(all_reviews)

# Convert all_reviews into a dataframe
IndianReviews <- data.frame(review_text = all_reviews)

# Print the dataframe
print(IndianReviews)

write.csv(IndianReviews, file = "IndianReviews.csv", row.names = FALSE)

##################################
### Indian Text Pre-processing ###
##################################
india <- read.csv("/Users/leeanne/IndianReviews.csv")
# Load required packages
library(tm)
library(stringr)

# Define your custom stop words
custom_stopwords <- c("indian", "food", "curry", "astoria", "lunch", "dinner", "naan", "chicken", "vegetable", "samosa", "restaurant")  # Add your custom stop words here

# Convert text to lowercase
india$review_text <- tolower(india$review_text)

# Create a Corpus
corpus <- Corpus(VectorSource(india$review_text))

# Define a custom function to remove stop words
removeCustomStopwords <- function(x) {
  x <- removePunctuation(x)
  x <- removeWords(x, c(stopwords("en"), custom_stopwords))
  x <- stripWhitespace(x)
  return(x)
}

# Apply preprocessing steps to the Corpus
corpus <- tm_map(corpus, content_transformer(removeCustomStopwords))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c("not"))  # Exclude "not" from removal
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

# Convert the preprocessed Corpus back to a data frame
preprocessed_text <- sapply(corpus, as.character)
india2 <- data.frame(review_text = preprocessed_text)


##################################
#### Indian Authenticity Count ###
##################################


##this code set consists of the code to count authenticity indicators##

#define positive authenticity indicators 
indian_indicators <- c("authentic", "real", "traditional", "genuine", "original", "expert", "professional", "honest", "pure")

# Step 2: Preprocess the Text
# Load required packages
#library(tm)
#library(stringr)

# Convert text to lowercase
#india$review_text <- tolower(india$review_text)

# Remove punctuation
#india$review_text <- removePunctuation(india$review_text)

# Remove stop words

# Define your custom stop words
#custom_stopwords <- c("indian", "food", "curry", "astoria", "lunch", "dinner", "naan", "chicken", "vegetable", "samosa", "restaurant")  # Add your custom stop words here

#stopwords <- c(stopwords("en"), custom_stopwords)  # Assuming English language stop words
#india$review_text <- lapply(india$review_text, function(review) {
# review[!review %in% stopwords]
#})

# Split into individual words or tokens
india2$review_text <- unlist(india2$review_text)
india2$review_text <- str_split_fixed(india2$review_text, "\\s+")


# Create a new column for counting Indian indicators. This will create a column that includes the number of auth indicators are present in the review
india2$indian_count <- sapply(india2$review_text, function(review) {
  sum(str_count(review, paste(indian_indicators, collapse = "|")))
})

## Part 2: Conduct a sentiment analysis on only reviews that contain at least one authenticity indicator. here we can determine if the authenticity coded review is used in a positive way versus a negative way ##

# Step 4: Filter Reviews with Authenticity Indicators. this filters the reviews to only include reviews that contain more than 0 auth indicators
authentic_reviews <- india2[india2$indian_count > 0, ]

# Step 5: Sentiment Analysis on Entire Review Text: Here I have coded words as positive and inauthentic words as negative. this way, since we have already limited the reviews to those that contain an auth indicator, i will categorize the reviews based on whether they are negative or positive
#example: if a review contains the word "authentic" AND the word "disgusting" or "terrible", it will be considered a NEGATIVE authenticity 
#example: if a review contains the word "authentic" and the word "delicious" or "amazing", then it will be considered a POSITIVE authenticity
positive_patterns <- c("delicious", "amazing", "flavorful", "kind", "tasty", "cozy", "comfy", "great", "really good", "very good","friendly", "so good", "excellent", "obsessed", "nice", "favorite", "I love")  # Example positive sentiment patterns
negative_patterns <- c("inauthentic", "fake", "american", "scam", "dishonest", "deceptive", "artificial", "untraditional", "modern", "unconventional", "disappointing", "food poisoning", "sick", "inconsistent", "idiots", "damaged", "undercooked", "burnt", "mediocre", "just ok", "not good", "dirty", "uncleanly", "poor", "gross" )  # Example negative sentiment patterns

#this adds a sentiment score to the dataset. the score is determined by taking the number of positive counts and subtracting the negative counts
authentic_reviews$sentiment_score <- sapply(authentic_reviews$review_text, function(review) {
  positive_count <- sum(str_count(review, paste(positive_patterns, collapse = "|")))
  negative_count <- sum(str_count(review, paste(negative_patterns, collapse = "|")))
  sentiment_score <- positive_count - negative_count
  return(sentiment_score)
})

#ifelse() statement is used to check if the sentiment score is greater than 0 (positive), less than 0 (negative), or equal to 0 (neutral). If the sentiment score is 0, the review is categorized as "Neutral" using the "Neutral" label.
authentic_reviews$sentiment_label <- ifelse(authentic_reviews$sentiment_score > 0, "Positive",
                                            ifelse(authentic_reviews$sentiment_score < 0, "Negative", "Neutral"))

#here I am just renaming the dataset so I can tell which is the Indian and which is the Chinese restaurant 
IndianAuthenticReviews <- authentic_reviews
write.csv(IndianAuthenticReviews, file = "IndianAuthenticReviews.csv", row.names = FALSE)

##################################
### Japan Text Pre-processing ###
##################################
japan <- read.csv("/Users/leeanne/Desktop/NDSU/JapanReviews.csv")
# Load required packages
library(tm)
library(stringr)

# Define your custom stop words
custom_stopwords <- c("Japan", "food", "matcha", "udon", "lunch", "dinner", "japanese", "raku", "noodles", "tempura", "restaurant", "favorite", "eat", "ny", "new york", "nyc", "new york city", "hungry", "outside", "chicken")  # Add your custom stop words here

# Convert text to lowercase
japan$review_text <- tolower(japan$review_text)

# Create a Corpus
corpus <- Corpus(VectorSource(japan$review_text))

# Define a custom function to remove stop words
removeCustomStopwords <- function(x) {
  x <- removePunctuation(x)
  x <- removeWords(x, c(stopwords("en"), custom_stopwords))
  x <- stripWhitespace(x)
  return(x)
}

# Apply preprocessing steps to the Corpus
corpus <- tm_map(corpus, content_transformer(removeCustomStopwords))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, c("not"))  # Exclude "not" from removal
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

# Convert the preprocessed Corpus back to a data frame
preprocessed_text <- sapply(corpus, as.character)
japan2 <- data.frame(review_text = preprocessed_text)


##################################
#### Japan Authenticity Count ####
##################################

#define positive authenticity indicators 
japan_indicators <- c("authentic", "real", "traditional", "genuine", "original", "expert", "professional", "honest", "pure")

# Step 2: Preprocess the Text
# Load required packages
#library(tm)
#library(stringr)

# Convert text to lowercase
#japan2$review_text <- tolower(japan2$review_text)

# Remove punctuation
#japan2$review_text <- removePunctuation(japan2$review_text)

# Remove stop words
#stopwords <- stopwords("en")  # Assuming English language stop words
#japan2$review_text <- lapply(japan2$review_text, function(review) {
#review[!review %in% stopwords]
#})

# Split into individual words or tokens
japan2$review_text <- str_split(japan2$review_text, "\\s+")

# Create a new column for counting Indian indicators. This will create a column that includes the number of auth indicators are present in the review
japan2$japan_count <- sapply(japan2$review_text, function(review) {
  sum(str_count(review, paste(japan_indicators, collapse = "|")))
})

## Part 2: Conduct a sentiment analysis on only reviews that contain at least one authenticity indicator. here we can determine if the authenticity coded review is used in a positive way versus a negative way ##

# Step 4: Filter Reviews with Authenticity Indicators. this filters the reviews to only include reviews that contain more than 0 auth indicators
japan_authentic_reviews <- japan2[japan2$japan_count > 0, ]

# Step 5: Sentiment Analysis on Entire Review Text: Here I have coded words as positive and inauthentic words as negative. this way, since we have already limited the reviews to those that contain an auth indicator, i will categorize the reviews based on whether they are negative or positive
#example: if a review contains the word "authentic" AND the word "disgusting" or "terrible", it will be considered a NEGATIVE authenticity 
#example: if a review contains the word "authentic" and the word "delicious" or "amazing", then it will be considered a POSITIVE authenticity
positive_patterns <- c("delicious", "amazing", "flavorful", "kind", "tasty", "cozy", "comfy", "great", "really good", "very good","friendly", "so good", "excellent", "obsessed", "nice", "favorite", "I love")  # Example positive sentiment patterns
negative_patterns <- c("inauthentic", "fake", "american", "scam", "dishonest", "deceptive", "artificial", "untraditional", "modern", "unconventional", "disappointing", "food poisoning", "sick", "inconsistent", "idiots", "damaged", "undercooked", "burnt", "mediocre", "just ok", "not good", "dirty", "uncleanly", "poor", "gross" )  # Example negative sentiment patterns

#this adds a sentiment score to the dataset. the score is determined by taking the number of positive counts and subtracting the negative counts
japan_authentic_reviews$sentiment_score <- sapply(japan_authentic_reviews$review_text, function(review) {
  positive_count <- sum(str_count(review, paste(positive_patterns, collapse = "|")))
  negative_count <- sum(str_count(review, paste(negative_patterns, collapse = "|")))
  sentiment_score <- positive_count - negative_count
  return(sentiment_score)
})

#ifelse() statement is used to check if the sentiment score is greater than 0 (positive), less than 0 (negative), or equal to 0 (neutral). If the sentiment score is 0, the review is categorized as "Neutral" using the "Neutral" label.
japan_authentic_reviews$sentiment_label <- ifelse(japan_authentic_reviews$sentiment_score > 0, "Positive",
                                                  ifelse(japan_authentic_reviews$sentiment_score < 0, "Negative", "Neutral"))

#here I am just renaming the dataset so I can tell which is the Indian and which is the Japanese restaurant 
JapanAuthenticReviews <- japan_authentic_reviews
write.csv(JapanAuthenticReviews, file = "JapanAuthenticReviews.csv", row.names = FALSE)