# Using Quanteda for text analysis
# https://github.com/quanteda/quanteda
# Excellent summary: https://quanteda.io/articles/pkgdown/quickstart.html

library(quanteda)
library(devtools)
library(tidyverse)
devtools::install_github("quanteda/quanteda.corpora")
devtools::install_github("quanteda/quanteda.sentiment")
devtools::install_github("quanteda/quanteda.textplots")
devtools::install_github("quanteda/quanteda.textstats")
library(quanteda.corpora)
library(quanteda.textplots)
library(quanteda.textstats)
library(quanteda.sentiment)
library(arrow)

### Quanteda can import a variety of text formats directly, from .txt files to JSON from the Twitter API, 
### using the `corpus` command.
### Here, we'll use a built-in corpus of all inaugural addresses by U.S. presidents.
### Other corpora can be accessed like so: https://github.com/quanteda/quanteda.corpora

my_corpus <- data_corpus_inaugural

### Twitter Data
#my_corpus <- read_parquet('War_full_03.parquet')
#my_corpus <- corpus(tweets$text)

### Reddit WSB Data
#my_corpus <- read_csv('reddit_wsb.csv')
### Let's look at the corpus more closely ... this one requires some subject-matter knowledge
#my_corpus <- corpus(my_corpus$title)

### Let's take a look inside:
View(summary(corpus))
docvars(my_corpus)

### If you want to add your own doc variables, you can use a function like:
# docvars(corp, field = "Century") <- floor(docvars(corp, field = "Year") / 100) + 1


### ... next, create tokens:
### Q: what are tokens in NLP?
tokens <- tokens(my_corpus)

### From here on, we'll always use tokens, not plain text.

clean_tokens <- tokens(
  tokens,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_twitter = TRUE,
  remove_url = TRUE,
  remove_hyphens = TRUE,
  include_docvars = TRUE
)

clean_tokens # Coding is like cooking. Always taste at every step.

### Always verify that tokens you look for make sense in context:
keyword_context <- kwic(clean_tokens, pattern='war')
head(keyword_context, 10)

### Where in the documents does a keyword appear?
keyword_context %>% textplot_xray()


### Create word frequency matrix:
data_freq_matrix <- dfm(clean_tokens,
                        tolower=TRUE,
                        stem=TRUE, # why stem?
                        remove=stopwords('english')
                        )

data_freq_matrix
topfeatures(data_freq_matrix)
### What would the top features look like if we hadn't cleaned the corpus or removed stopwords?
### Note: sometimes you WANT stopwords. Always think about RQs, goals, genre of text, before removing


### Less sparse; i.e., focus on more frequent words only
trim_freq_matrix <- dfm_trim(data_freq_matrix,
                             min_docfreq = 0.175,
                             max_docfreq = 0.9,
                             docfreq_type = "prop"
) 

topfeatures(trim_freq_matrix)


### Or use tfidf to weigh unique words heavier
tfidf_freq_matrix <- dfm_tfidf(data_freq_matrix)
topfeatures(tfidf_freq_matrix)


### DFM by group:
grouped_freq_matrix <- dfm_group(data_freq_matrix, groups=Party)
grouped_freq_matrix


### Only want part of a corpus?
subset <- corpus_subset(my_corpus, Year > 2000)

tokens <- tokens(subset)
clean_tokens <- tokens(
  subset,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_twitter = TRUE,
  remove_url = TRUE,
  remove_hyphens = TRUE,
  include_docvars = TRUE
)

### Create a DFM from this subset:

data_freq_matrix <- dfm(clean_tokens,
                        tolower=TRUE,
                        stem=TRUE,
                        remove=stopwords('english')
                        )

data_freq_matrix
topfeatures(data_freq_matrix)


### Fancier topfeatures:
features <- textstat_frequency(data_freq_matrix, n=30)
View(features)

features$feature <- with(features, reorder(feature, -frequency))
ggplot(features, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### If you MUST have a wordcloud, here's a simple way to build one:
textplot_wordcloud(data_freq_matrix)


### Keyness
t_o_subset <- corpus_subset(subset, President %in% c('Obama', 'Trump'))
t_o_dfm <- tokens(t_o_subset, remove_punct = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_group(groups = President) %>%
  dfm()

keyness <- textstat_keyness(t_o_dfm, target='Trump')
textplot_keyness(keyness)
### Here's why subject-matter/language-situation expertise matters: is it 'us' or 'U.S.'?



### Next, let's do a feature co-occurrence matrix
### for when we want to compare how often terms co-occur in a given document
obama_subset <- corpus_subset(t_o_subset, President %in% 'Obama')
obama_dfm <- tokens(obama_subset, remove_punct=TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm()
obama_fcm <- fcm(obama_dfm)
obama_fcm
topfeatures(obama_fcm)

### And for Trump:
trump_subset <- corpus_subset(t_o_subset, President %in% 'Trump')
trump_dfm <- tokens(trump_subset, remove_punct=TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  dfm()
trump_fcm <- fcm(trump_dfm)
trump_fcm
topfeatures(trump_fcm)


active_tags <- dfm_select(trump_dfm, pattern="*ing")
toptag <- names(topfeatures(active_tags, 10))
toptag ### Again, always do sanity checks. Which one doesn't fit?
active_tags <- dfm_remove(active_tags, 'bring')
toptag <- names(topfeatures(active_tags, 10))
toptag

active_tags_fcm <- fcm(active_tags)
#active_tags_fcm <- fcm_select(active_tags_fcm, pattern = toptag, selection = "keep") ### or selection = "remove"
active_tags_fcm

textplot_network(active_tags_fcm)
### What could this dfm_select feature be really useful for?
### E.g., with Twitter data, you can select only hashtags and map their co-occurence
### with `dfm_select(dfm, pattern="#")`

### Cosine similarity (explain)
obama_simil <- textstat_simil(data_freq_matrix, data_freq_matrix[c("2009-Obama" , "2013-Obama"),], 
                              margin = "documents", method = "cosine")
obama_simil
### Let's go back to the full corpus for these by re-running the var assignment for `data_freq_matrix` for the full corpus
### What RQs could emerge? E.g., are D more similar to other D speeches? Rel'ship between first and second inaugurals?


### Polarity/Basic Sentiment
textstat_polarity(my_corpus, dictionary=data_dictionary_LSD2015)
# What do you observe?


### Advanced stuff ... moving toward Machine Learning with Latent Semantic Scaling
install.packages("LSX")
library(LSX)

my_corpus <- read_parquet('War_full_03.parquet')
my_corpus <- corpus(my_corpus$text)
tokens <- tokens(my_corpus)

clean_tokens <- tokens(
  tokens,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_twitter = TRUE,
  remove_url = TRUE,
  remove_hyphens = TRUE,
  include_docvars = TRUE
)

clean_tokens <- tokens_remove(clean_tokens, stopwords('en'))

dfm_sent <- dfm(clean_tokens,
                tolower=TRUE,
                stem=TRUE,
                remove=stopwords('english')
)

topfeatures(dfm_sent)

seed <- as.seedwords(data_dictionary_sentiment)
seed
typeof(seed)
### You can see that this is a pretty basic dictionary-based sentiment system.
### It also is simply a named list. You can construct your own.

query_pattern <- char_context(clean_tokens, pattern="trust", p=0.05)
query_pattern

tmod_lss <- textmodel_lss(dfm_sent, seeds = seed,
                          terms = query_pattern, k = 30, cache = TRUE)

head(coef(tmod_lss))

textplot_terms(tmod_lss, data_dictionary_LSD2015["negative"])

### Potential RQs ###

### Compare related corpora, like inaugurals (addressed to public) and SOTU (addressed to peer professionals)
### Compare change over time
### Compare party differences
### ...
### Look for advanced features, like seeded topic models, here: https://tutorials.quanteda.io/machine-learning/
