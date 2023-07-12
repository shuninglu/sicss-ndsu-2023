## Native Plant Subreddit Comparison

# This is the code used to collect and analyze the data associated with 
# the r/GuerrillaGardening vs. r/NativePlantGardening final project 
# at 2023 SICSS-NDSU.
# Group Members: Tim Jaafaru, Haley Netherton-Morrison, and Brule Woods


# Load Libraries ----------------------------------------------------------


#libraries
library(tidyverse)
library(RedditExtractoR) #for extracting Reddit content
library(quanteda) #for text analysis
library(quanteda.textstats) #for text analysis
library(rvest) #for web scraping
library(igraph) #for network figure
library(quanteda.textplots) #for text analysis plotting
library(stm) #for topic modeling



# Scrape Posts from Both Subreddits ---------------------------------------


#Scrape r/GuerrillaGardening and r/NativePlantGardening
#Note: pulling top 1000 posts from both
gg.subred <- find_thread_urls(
  subreddit = "GuerrillaGardening", 
  sort_by = "top",
  period = "all"
)

npg.subred <- find_thread_urls(
  subreddit = "NativePlantGardening", 
  sort_by =  "top",
  period = "all"
)


# #Save the post dataframes
# write_csv(gg.subred, file = "YOUR_PATH")
# write_csv(npg.subred, file = "YOUR_PATH")


# #Load post data
# gg.subred <- read_csv("YOUR_PATH")
# npg.subred <- read_csv("YOUR_PATH")



# Scrape Comments from Top 1000 Posts ---------------------------------------


#Guerrilla Gardening Posts
##create list to store data
gg_comments_list <- list()

##loop through the post urls to extract the comments
for (i in 2:length(gg.subred$url)){
  url <- gg.subred$url[i]
  comments <- get_thread_content(url)
  gg_comments_list[[i]] <- comments
}


#Native Plant Gardening Posts
##create list to store data
npg_comments_list <- list()

##loop through the post urls to extract the comments
for (i in 2:length(npg.subred$url)){
  url <- npg.subred$url[i]
  comments <- get_thread_content(url)
  npg_comments_list[[i]] <- comments
}



# Merge Comments Data -------------------------------------------


#Guerrilla Gardening
##create empty dataframe to store the comments
gg.comments.df <- data.frame()

##merge the comment lists into one dataframe
for (x in 1:length(gg_comments_list)){
  comments <- gg_comments_list[[x]]$comments
  gg.comments.df <- rbind(gg.comments.df, comments)
}


#Native Plant Gardening
##create empty dataframe to store the comments
npg.comments.df <- data.frame()

##merge the comment lists into one dataframe
for (x in 1:length(npg_comments_list)){
  comments <- npg_comments_list[[x]]$comments
  npg.comments.df <- rbind(npg.comments.df, comments)
}


# #Save the comments as RDS and CSV
# saveRDS(gg_comments_list, file = "YOUR_PATH")
# write_csv(gg.comments.df, file = "YOUR_PATH")
# 
# saveRDS(npg_comments_list, file = "YOUR_PATH")
# write_csv(npg.comments.df, file = "YOUR_PATH")



# Clean and Prepare the Comment Data ----------------------------------------------------------


# #Read in the comments data
# gg.comments.df <- read_csv(file = "YOUR_PATH")
# npg.comments.df <- read_csv(file = "YOUR_PATH")


#There are many solo "s", "t", and "m" from people mistyping contractions/possessives;
#however, not all of these are because of that (e.g., M. fistulosa is a plant)
#Fix typos in the original comments
fix_typos <- function(x){
  x %>%
    str_replace_all("t s ", "t's ") %>%
    str_replace_all("I m ", "I'm ") %>%
    str_replace_all("i m ", "i'm ") %>%
    str_replace_all("n t ", "n't ") %>%
    str_replace_all("I ve ", "I've ") %>%
    str_replace_all("i ve ", "i've ")
}
gg.comments.df$comment <- gg.comments.df$comment %>% fix_typos
npg.comments.df$comment <- npg.comments.df$comment %>% fix_typos


#Clean the data
clean_comments <- function(x) {
  x %>%
    # Remove URLs
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    # Remove mentions e.g. "@my_account"
    str_remove_all("@[[:alnum:]_]{4,}") %>%
    # Replace "&" character reference with "and"
    str_replace_all("&amp;", "and") %>%
    # Replace all non-alphanumeric characters with a space
    str_replace_all("[^[:alnum:]]", " ") %>%
    # Replace any newline characters with a space
    str_replace_all("\\\n", " ") %>%
    # Make everything lowercase
    str_to_lower() %>%
    # Remove emoji
    str_remove_all('[:emoji:]')%>%
    # Remove any trailing whitespace around the text
    str_trim("both")
}
gg.comments.df$comment_clean <- gg.comments.df$comment %>% clean_comments
npg.comments.df$comment_clean <- npg.comments.df$comment %>% clean_comments


#Tokenization
gg.corpus <- corpus(gg.comments.df$comment_clean)
gg.corpus <- gg.corpus %>%
  tokens(what = "word",
         split_hyphens = FALSE,
         remove_punct = FALSE,
         remove_symbols = FALSE,
         include_docvars = TRUE) %>%
  tokens_remove(stopwords("english"))

npg.corpus <- corpus(npg.comments.df$comment_clean)
npg.corpus <- npg.corpus %>%
  tokens(what = "word",
         split_hyphens = FALSE,
         remove_punct = FALSE,
         remove_symbols = FALSE,
         include_docvars = TRUE) %>%
  tokens_remove(stopwords("english"))


#Add additional variables of interest to corpus
##subreddit
docvars(gg.corpus, field = "subreddit") <- "r/GuerrillaGardening"
docvars(npg.corpus, field = "subreddit") <- "r/NativePlantGardening"


##date
docvars(gg.corpus, field = "date") <- gg.comments.df$date
docvars(npg.corpus, field = "date") <- npg.comments.df$date


#Remove tokens that are less than two characters in length
gg.corpus <- tokens_keep(gg.corpus, min_nchar = 2)
npg.corpus <- tokens_keep(npg.corpus, min_nchar = 2)


#Convert corpus to document frequency matrix
gg.dfm <- dfm(gg.corpus) 
topfeatures(gg.dfm, n = 200, scheme = "docfreq")

npg.dfm <- dfm(npg.corpus) 
topfeatures(npg.dfm, n = 200, scheme = "docfreq")


#Create a combined dfm
combined.dfm <- rbind(gg.dfm, npg.dfm)
topfeatures(combined.dfm, n = 200, scheme = "docfreq")



# Frequency of Themes of Interest -------------------------------------------------------


#Note: We are interested in comparing how often specific topics/themes  
#(e.g., guerrilla gardening, pollinators, ownership, etc.) come up in both groups


#Frequency of all words, by subreddit
word.freq <- textstat_frequency(combined.dfm, 
                                groups = combined.dfm@docvars$subreddit, 
                                ties_method = "average")


#Subset to words of interest
##Invasive
inv.words <- word.freq[grep("invasiv", word.freq$feature), ] #select all rows that match the keyword
inv.freq <- inv.words[c(1,2,8,9),] %>% #select only rows of interest (this should be updated for each topic depending on when data is pulled)
  group_by(group) %>% #group by subreddit
  summarize(Frequency = sum(frequency)) #summarize the frequency of all related words by subreddit
inv.freq$Theme <- "Invasive" #add a labeling variable 

##Native
nat.words <- word.freq[grep("native", word.freq$feature), ] 
nat.freq <- nat.words[c(1,2,14,15),] %>% 
  group_by(group) %>%
  summarize(Frequency = sum(frequency))
nat.freq$Theme <- "Native"

##Pollinator
pol.words <- word.freq[grep("pollin", word.freq$feature), ]
pol.freq <- pol.words[c(1:7,9:15),] %>% 
  group_by(group) %>%
  summarize(Frequency = sum(frequency))
pol.freq$Theme <- "Pollination"

##Ownership
##Note: This one requires some additional cleaning due to "own" being common
own.words <- word.freq[grep("own", word.freq$feature), ] 
own.words2 <- own.words[grep("owner|owns|owned", own.words$feature), ] 
own.words3 <- own.words2[!grepl("down|town|brown|crown|renown|know|frown|drown|clown", own.words2$feature), ] 
own.freq <- own.words3 %>% 
  group_by(group) %>%
  summarize(Frequency = sum(frequency))
own.freq$Theme <- "Ownership"

##Ecosystem
eco.words <- word.freq[grep("ecosystem", word.freq$feature), ] 
eco.freq <- eco.words %>% 
  group_by(group) %>%
  summarize(Frequency = sum(frequency))
eco.freq$Theme <- "Ecosystem"

##Guerrilla
guer.words <- word.freq[grep("Guerrilla|guerilla|gorilla", word.freq$feature), ] 
guer.freq <- guer.words[c(1:3,7,8,13,14,16:19,21), ] %>% 
  group_by(group) %>%
  summarize(Frequency = sum(frequency))
guer.freq$Theme <- "Guerrilla"

##Benefits
ben.words <- word.freq[grep("benef", word.freq$feature), ] 
ben.freq <- ben.words %>% 
  group_by(group) %>%
  summarize(Frequency = sum(frequency))
ben.freq$Theme <- "Benefits"

##Harms
harm.words <- word.freq[grep("harm", word.freq$feature), ] 
harm.words2 <- harm.words[!grepl("charm|harmony|pharm|harmonicas|harmonious|harmless", harm.words$feature), ]
harm.freq <- harm.words2 %>% 
  group_by(group) %>%
  summarize(Frequency = sum(frequency))
harm.freq$Theme <- "Harms"


#Combine the dataframes into one 
freq.int <- rbind(inv.freq, nat.freq, pol.freq, own.freq, eco.freq, guer.freq, ben.freq, harm.freq)

##calculate proportion of posts containing the topic by subreddit
freq.int <- freq.int %>%
  mutate(Proportion = ifelse(group == "r/GuerrillaGardening", 
                             Frequency/13904, 
                             Frequency/21134)) 


# #Get colors for plotting
# scico::scico(10, palette = "batlow")


#Create bar plot
theme.freq.p <- ggplot(data = freq.int, aes(x = fct_reorder(Theme, Proportion, .desc = FALSE), y = Proportion, fill = group)) +
  geom_bar(stat="identity", position=position_dodge()) + coord_flip() +
  xlab("Theme") +
  ylab("Proportion of Total Comments") +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c("#1C5460", "#F9A380"), name = "Group", guide = guide_legend(reverse = TRUE)) + 
  theme(text = element_text(size=12), plot.title = element_text(hjust = 0.5), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))


# #Save plot
# ggsave("YOUR_PATH", plot = theme.freq.p, 
#        path = NULL, scale = 1, width = 7.5, height = 3.75, units = ("in"), dpi = 300, 
#        limitsize = TRUE)



# Network Analysis - Related Communities -----------------------------------


#Scraping the related communities for both subreddits
npg_url <- "https://www.reddit.com/r/NativePlantGardening/"
npg_related <- npg_url %>% 
  read_html() %>% 
  html_nodes("._3BFvyrImF3et_ZF21Xd8SC") %>% #found using the inspection tool
  html_attr(., "title")

gg_url <- "https://www.reddit.com/r/GuerrillaGardening/"
gg_related <- gg_url %>% 
  read_html() %>% 
  html_nodes("._3BFvyrImF3et_ZF21Xd8SC") %>% #found using the inspection tool
  html_attr(., "title")


#Get those communities' related communities
##merge the two lists and keep unique subreddits only
related_unique <- unique(c(gg_related, npg_related))

##get the related communities for each of the related subreddits
all.related.df <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("related", "subreddit")) #empty df for storing the data
for (i in 1:length(related_unique)){
  #create url for the subreddit
  url <- paste0("https://www.reddit.com/", related_unique[[i]])
  
  #scrape the related communities
  related <- url %>% 
    read_html() %>% 
    html_nodes("._3BFvyrImF3et_ZF21Xd8SC") %>% #found using the inspection tool
    html_attr(., "title")
  
  #create a dataframe (only for subreddits with related communities)
  ifelse(length(related) > 0, related.df <- as.data.frame(related), related)
  related.df$subreddit <- related_unique[[i]]
  
  #append the related communities to the master df
  all.related.df <- rbind(all.related.df, related.df)
}
#Note: related communities are not enabled/available for all of these


#Build a dataframe with the connections between communities
##convert the gg and npg lists into dfs
gg.related.df <- as.data.frame(gg_related)
colnames(gg.related.df) <- "related"
gg.related.df$subreddit <- "r/GuerrillaGardening"

npg.related.df <- as.data.frame(npg_related)
colnames(npg.related.df) <- "related"
npg.related.df$subreddit <- "r/NativePlantGardening"

##append to the master dataframe
all.related.df <- rbind(all.related.df, gg.related.df, npg.related.df)


#Plot as a network
##create a nodes column
##the numbers will need to be updated depending on when the data is scraped
all.related.df$level <- c(rep("Secondary, Shared", 83), rep("Secondary, r/GuerrillaGardening", 118), rep("Secondary, Shared", 6),
                          rep("Secondary, r/GuerrillaGardening", 6), rep("Secondary, r/NativePlantGardening", 38),
                          rep("Primary, Shared", 2), rep("Primary, r/GuerrillaGardening", 5), "Primary, Shared", 
                          "Primary, r/GuerrillaGardening", "Primary, r/NativePlantGardening", rep("Primary, Shared", 2),
                          rep("Primary, r/NativePlantGardening", 4), "Primary, Shared") 


##create network object
###only focusing on first-order connections for clarity
related.net <- graph_from_data_frame(all.related.df[grep("Guerrilla|NativePlant", all.related.df$subreddit),], directed = F) 

##create a vector of color 
batlow_three <- c("#1C5460", "#F9A380", "#9A872D")
vertex_attr(related.net) #what are the vertices?
vertex_attr(related.net)$vert_color <- c(rep("#9A872D80", 2), rep("#1C546080", 5), "#9A872D80", "#1C546080", "#F9A38080", 
                                         rep("#F9A38080", 4), "#1C546080", "#F9A38080") #last two digits adjust opacity for hex codes
vertex_attr(related.net) #check that colors were assigned correctly

##plot
plot(related.net, 
     vertex.color = V(related.net)$vert_color, 
     vertex.label.cex = 0.75,
     vertex.label.color = "black",
     vertex.label.family = "sans",
     vertex.label.font = 2, #to make the labels bold
     edge.label.cex = .5,
     edge.label.color = "gray50") 
legend("bottomleft", legend = levels(as.factor(all.related.df[grep("Guerrilla|NativePlant", all.related.df$subreddit),]$level)), 
       bty = "n", pch = 20 , pt.cex = 3, cex = 1, col = batlow_three,
       text.col = batlow_three, horiz = FALSE)


#Bar plot summarizing connections
##create a dataframe for the plot
connections.sum <- all.related.df %>%
  group_by(level) %>%
  count() 
connections.sum$Connection_Type <- c(rep("Primary", 3), rep("Secondary", 3))
connections.sum$Subreddit <- c("Shared", "r/GuerrillaGardening", "r/NativePlantGardening", 
                               "Shared", "r/GuerrillaGardening", "r/NativePlantGardening")

##update colors
batlow_six <- c("#1C5460", "#F9A380", "#9A872D", "#3E6C54", "#FDB6BC", "#D49347")

##plot
connections.p <- ggplot(data = connections.sum, aes(x = Subreddit, y = n, fill = fct_rev(level))) +
  geom_bar(stat="identity") +
  xlab("Subreddit") +
  ylab("Number of Related Communities") +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = rev(batlow_six), name = "Connection Type", guide = guide_legend(reverse = TRUE)) + 
  theme(text = element_text(size=10), plot.title = element_text(hjust = 0.5), 
        panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))
connections.p

# ##save plot
# ggsave("YOUR_PATH", plot = connections.p, 
#        path = NULL, scale = 1, width = 7.5, height = 3.75, units = ("in"), dpi = 300, 
#        limitsize = TRUE)



# Keyness -----------------------------------------------------------------


#Keyness
##need to group the combined dfm for this to work
combined.dfm <- combined.dfm %>% dfm_group(groups = combined.dfm@docvars$subreddit)

##now can calculate keyness and plot
keyness <- textstat_keyness(combined.dfm) 
textplot_keyness(keyness, color = c("#1C5460", "#F9A380"))



# Mean Comments Per Post --------------------------------


#Mean comments per post
gg.subred %>% summarize(mean_comments = mean(comments, na.rm = TRUE))
npg.subred %>% summarize(mean_comments = mean(comments, na.rm = TRUE))



# Structural Topic Modeling -----------------------------------------------


#Structural topic modeling with subreddit as a covariate

#Prepare the stm data using the combined dfm
comb.stm <- convert(combined.dfm, to = "stm")
out.comb <- prepDocuments(comb.stm$documents, 
                          comb.stm$vocab, 
                          comb.stm$meta, 
                          lower.thresh = 10) 


#Find optimal K (this will take a long time)
set.seed(1234)
storage.comb <- searchK(out.comb$documents, out.comb$vocab, K = c(3:8),
                        data=out.comb$meta)
print(storage.comb$results)


#Run models for combined data
##comparing K4-6 models
model4.comb <- stm(out.comb$documents, out.comb$vocab,
                   K = 4, 
                   max.em.its = 29, #its number should follow searchK output
                   data = out.comb$meta, 
                   init.type = "Spectral")


model5.comb <- stm(out.comb$documents, out.comb$vocab,
                   K = 5, 
                   max.em.its = 33, #its number should follow searchK output
                   data = out.comb$meta, 
                   init.type = "Spectral") 


model6.comb <- stm(out.comb$documents, out.comb$vocab,
                   K = 6, 
                   max.em.its = 37, #its number should follow searchK output
                   data = out.comb$meta, 
                   init.type = "Spectral") 


#Using FREX to compare
labels4.comb <- labelTopics(model4.comb, n = 20)
topwords4.comb <- data.frame("features" = t(labels4.comb$frex))
colnames(topwords4.comb) <- paste("Topics", c(1:4))
topwords4.comb[1:4]

labels5.comb <- labelTopics(model5.comb, n = 20)
topwords5.comb <- data.frame("features" = t(labels5.comb$frex))
colnames(topwords5.comb) <- paste("Topics", c(1:5))
topwords5.comb[1:5]

labels6.comb <- labelTopics(model6.comb, n = 20)
topwords6.comb <- data.frame("features" = t(labels6.comb$frex))
colnames(topwords6.comb) <- paste("Topics", c(1:6))
topwords6.comb[1:6]
#We decided to move forward with the 5 topic model for the final presentation


#Now, look at the effect of subreddit on the model
subred.model <- estimateEffect(1:5 ~ subreddit, model5.comb,
                               meta = out.comb$meta, uncertainty = "Global")
topiceffect <- summary(subred.model, topics = c(1:5))
topiceffect


#Perspective plots to compare two of the topics by subreddit
plot(model5.comb,
     type="perspectives",
     labeltype = "frex",
     covarlevels = "r/GuerrillaGardening",
     topics=c(1,5), 
     plabels = c("Information-seeking","Community Effort"))


plot(model5.comb,
     type="perspectives",
     labeltype = "frex",
     covarlevels = "r/NativePlantGardening",
     topics=c(1,5), 
     plabels = c("Information-seeking","Community Effort"))


#Plot the topic distribution for the combined corpus
plot(model5.comb, type = c("summary"),
     labeltype = c("frex"),
     topic.names = c("Topic 1: information-seeking",
                     "Topic 2: urban, suburban",
                     "Topic 3: descriptors, benefits",
                     "Topic 4: rules",
                     "Topic 5: community effort"),
     main = c("Topic distribution"),xlim = c(0, 0.5),
     custom.labels="")



# Topic Modeling by Subreddit --------------------------------------


#NativePlantGardening
##prepare stm data
npg.stm <- convert(npg.dfm, to = "stm")
out.npg <- prepDocuments(npg.stm$documents,
                         npg.stm$vocab,
                         npg.stm$meta,
                         lower.thresh = 10)

##find the optimal K
storage.npg <- searchK(out.npg$documents, out.npg$vocab, K = c(3:6),
                       data=out.npg$meta)
print(storage.npg$results)

##now, let's run the 4 and 5-topic models
model4.npg <- stm(out.npg$documents, out.npg$vocab,
                  K = 4,
                  max.em.its = 30, #its number should follow searchK output
                  data = out.npg$meta,
                  init.type = "Spectral")

model5.npg <- stm(out.npg$documents, out.npg$vocab,
                  K = 5,
                  max.em.its = 34, #its number should follow searchK output
                  data = out.npg$meta,
                  init.type = "Spectral")

##words used in topics
labelTopics(model4.npg, c(1:4), frexweight = 0.5,n = 15)
labelTopics(model5.npg, c(1:5), frexweight = 0.5,n = 15)

##look at FREX
labels4.npg <- labelTopics(model4.npg, n = 20)
topwords4.npg <- data.frame("features" = t(labels4.npg$frex))
colnames(topwords4.npg) <- paste("Topics", c(1:4))
topwords4.npg[1:4]

labels5.npg <- labelTopics(model5.npg, n = 20)
topwords5.npg <- data.frame("features" = t(labels5.npg$frex))
colnames(topwords5.npg) <- paste("Topics", c(1:5))
topwords5.npg[1:5]



#GuerillaGardening
##prepare stm data
gg.stm <- convert(gg.dfm, to = "stm")
out.gg <- prepDocuments(gg.stm$documents,
                        gg.stm$vocab,
                        gg.stm$meta,
                        lower.thresh = 10)

##find optimal K
storage.gg <- searchK(out.gg$documents, out.gg$vocab, K = c(3:6),
                      data=out.gg$meta)
print(storage.gg$results)

##run the 4-6 topic models
model4.gg <- stm(out.gg$documents, out.gg$vocab,
                 K = 4,
                 max.em.its = 30, #its number should follow searchK output
                 data = out.gg$meta,
                 init.type = "Spectral")

model5.gg <- stm(out.gg$documents, out.gg$vocab,
                 K = 5,
                 max.em.its = 34, #its number should follow searchK output
                 data = out.gg$meta,
                 init.type = "Spectral")

model6.gg <- stm(out.gg$documents, out.gg$vocab,
                 K = 6,
                 max.em.its = 42, #its number should follow searchK output
                 data = out.gg$meta,
                 init.type = "Spectral")

##words used in topics
labelTopics(model5.gg, c(1:5), frexweight = 0.5,n = 15)

##look at FREX
labels4.gg <- labelTopics(model4.gg, n = 20)
topwords4.gg <- data.frame("features" = t(labels4.gg$frex))
colnames(topwords4.gg) <- paste("Topics", c(1:4))
topwords4.gg[1:4]

labels5.gg <- labelTopics(model5.gg, n = 20)
topwords5.gg <- data.frame("features" = t(labels5.gg$frex))
colnames(topwords5.gg) <- paste("Topics", c(1:5))
topwords5.gg[1:5]

