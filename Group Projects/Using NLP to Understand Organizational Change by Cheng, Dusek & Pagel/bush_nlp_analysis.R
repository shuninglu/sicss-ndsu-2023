library(quanteda)
library(devtools)
library(tidyverse)
library(quanteda.corpora)
library(quanteda.textplots)
library(quanteda.textstats)
library(quanteda.sentiment)
library(arrow)
library(readtext)
library(lubridate)
library(stm)
library(wordcloud)
library(tesseract)
library(magrittr)
library(pdftools)

setwd("~/Desktop/2023-06 SICSS/project")

#Dataset of text from Bush Foundation Annual Reports
text <- readRDS(file = "all_reports.rds")
text <- text[order(text$year),]
class(text$text)
text$text <- as.character(text$text)
my_corpus <- corpus(text$text)

### Let's take a look inside:
View(my_corpus)
View(summary(my_corpus))
docnames(my_corpus) <- text$year

#adding decades
text$decade <- c(1960,1960,1970,1970,1970,1970,
                 1970,1970,1970,1970,1970,1980,
                 1980,1980,1980,1980,1980,1980,
                 1980,1980,1980,1990,1990,1990,
                 1990,1990,1990,1990,1990,1990,
                 1990,2000,2000,2000,2000,2000,
                 2000,2000,2000,2000,2000,2010,
                 2010,2010)

#Adding president to years
text$president <- ifelse(text$year %in% 1971:1997, "Humphrey Doermann",
                         ifelse(text$year %in% 1998:2007, "Anita Pampusch",
                                ifelse(text$year %in% 2008:2012, "Peter Hutchinson", "None")))

text$president_anita <- ifelse(text$year %in% 1971:1997, 0,
                         ifelse(text$year %in% 1998:2007, 1,
                                ifelse(text$year %in% 2008:2012, 0, 0)))

text$president_humphrey <- ifelse(text$year %in% 1971:1997, 1,
                               ifelse(text$year %in% 1998:2007, 0,
                                      ifelse(text$year %in% 2008:2012, 0, 0)))

text$president_peter <- ifelse(text$year %in% 1971:1997, 0,
                               ifelse(text$year %in% 1998:2007, 0,
                                      ifelse(text$year %in% 2008:2012, 1, 0)))


docvars(my_corpus) <- text[,c(1:8)]
presidents <- docvars(my_corpus)$president
presidents <- factor(presidents, levels=c("None","Humphrey Doermann","Anita Pampusch","Peter Hutchinson"))
docvars(my_corpus)$president <- presidents

### Next, create tokens:
tokens <- tokens(my_corpus)
View(tokens)

#Clean Tokens
clean_tokens <- tokens(
  tokens,
  remove_numbers = TRUE,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_url = TRUE,
  split_hyphens = TRUE,
  include_docvars = TRUE,
  remove_separators = TRUE,
)

#Limit word character count to 4 to eliminate nonsense words from OCR scans
clean_tokens <- tokens_remove(clean_tokens, min_nchar = 5)

clean_tokens <- tokens_remove(clean_tokens, 
                              pattern=list("minnesota","minneapolis",
                                           "foundation","program",
                                           "grant","bush"))

# Some lexical dispersion plots
keyword_context <- kwic(clean_tokens, pattern='family')
keyword_context %>% textplot_xray()
head(keyword_context, 10)

keyword_context <- kwic(clean_tokens, pattern= 'tribal')
keyword_context %>% textplot_xray()
head(keyword_context, 10)

### Create word frequency matrix:
data_freq_matrix <- dfm(clean_tokens,
                        tolower=TRUE,
                        remove=stopwords('english')
)

data_freq_matrix
topfeatures(data_freq_matrix, n = 100)

### Less sparse; i.e., focus on more frequent words only
trim_freq_matrix <- dfm_trim(data_freq_matrix,
                             min_docfreq = 2, #if features (words) occur in less then 17.5% of documents, remove them.
                             max_docfreq = 40,  #if features (words) occur in more than 90% of documents, remove them
                             docfreq_type = "count"
) 

topfeatures(trim_freq_matrix)

###########
# Keyness #
###########

# Keyness by President
dfm_president <- dfm(tokens_group(clean_tokens, groups=president))

keyness <- textstat_keyness(dfm_president, target='Anita Pampusch')
textplot_keyness(keyness)

keyness <- textstat_keyness(dfm_president, target='Humphrey Doermann')
textplot_keyness(keyness)

keyness <- textstat_keyness(dfm_president, target='Peter Hutchinson')
textplot_keyness(keyness)

# Keyness by decade
dfm_decade <- dfm(tokens_group(clean_tokens, groups=decade))

keyness <- textstat_keyness(dfm_decade, target='1970')
textplot_keyness(keyness)

keyness <- textstat_keyness(dfm_decade, target='1980')
textplot_keyness(keyness)

keyness <- textstat_keyness(dfm_decade, target='1990')
textplot_keyness(keyness)

keyness <- textstat_keyness(dfm_decade, target='2000')
textplot_keyness(keyness)

##################################################
# Topic Modeling
##################################################

#Prepare stm data
report.stm <- convert(trim_freq_matrix, to = "stm") #Transforms DFM data to STM, TM, etc...
out <- prepDocuments(report.stm$documents, #Use STM documents
                     report.stm$vocab, #Use STM vocabulary
                     report.stm$meta, #Use STM Metadata
                     lower.thresh = 1) #Occurs at least 1 times = Least words we want to use to model the STM, change from 10 to 1 --Shuning
set.seed(8844)

storage <- searchK(out$documents, out$vocab, K = c(3:10),
                   data=out$meta)

# 5-topic model  
model <- stm(out$documents, out$vocab,
             K = 5, 
             max.em.its = 22, #its number should follow searchK output
             data = out$meta, 
             init.type = "Spectral")

#FREX = Frequent Exclusive = Distinguish the topic
summary(model)
labelTopics(model, c(1:5), frexweight = 0.5,n = 10)

#Plot topics
par(bty="n",col="grey40",lwd=6)
plot(model, type = c("summary"),
     labeltype = c("frex"),
     topic.names = c("Topic 1: Native and Community",
                     "Topic 2: Programs",
                     "Topic 3: Higher Education",
                     "Topic 4: Corporate and Financial",
                     "Topic 5: Grants and Proposals"),
     main = c("Topic distribution"),xlim = c(0, 0.5),
     custom.labels="")

# Estimating model effects
# By president
president_effects <- estimateEffect(1:5 ~ president,
                          model,
                          meta = out$meta,
                          uncertainty = "Global")

summary(president_effects, topics = c(1:5))

# By decade
decade_effects <- estimateEffect(1:5 ~ decade,
                          model,
                          meta = out$meta,
                          uncertainty = "Global")

summary(decade_effects, topics = c(1:5))

# Topic 1: Higher Education
# Topic 2: Tribal and Community
# Topic 3: Corporate and Financial
# Topic 4: Programs and Economy
# Topic 5: Grants and Proposals

#distribution of specific topic by year
plot(x = president_effects, 
     covariate = "president", 
     topic = c(4), 
     model = model, 
     method = "pointestimate")

# Not many differences in point estimates across presidents
# Topic 2 seems more prevalent with President Peter Hutchinson

plot(x = decade_effects, 
     covariate = "decade", 
     topic = c(1,4), 
     model = model, 
     method = "continuous")

# Topics 2 and 3 seem to change across the decades
# Clear positive trend for "Tribal and Community" words
# Clear negative trend for "Corporate and Financial" words
