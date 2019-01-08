#import data 
work_dir<-"C:/Users/Desktop/ABA/ABA Project Data" 
getwd() 
setwd(work_dir) 
install.packages("data.table",repos = "https://cran.r-project.org") 
#Read Data 
library(data.table) 
data <- fread("h111_rep_tweets.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?") , stringsAsFactors = FALSE) 
summary(data) View(data) colnames(data) nrow(data) 
# Creating dataframe for tokenization install.packages("tidytext", repos = "https://cran.r-project.org") install.packages("dplyr", repos = "https://cran.r-project.org") library(dplyr) library(tidytext) 
text_df <- data_frame(line = 1:68365, text = data$content) head(text_df) #tokenizing ino words text_df <- data%>% unnest_tokens(word, content) #removing stopwords data(stop_words) text_df <- text_df %>%   anti_join(stop_words)  
#stemming library(SnowballC) tidy_text <- data %>%   unnest_tokens(word, content) %>%   mutate(word = wordStem(word)) data(stop_words) tidy_text<- tidy_text %>%   anti_join(stop_words) tidy_text %>%   count(word, sort = TRUE) 
library(ggplot2) tidy_text %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 4000) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +   geom_bar(stat = "identity") + 
  xlab(NULL) + 
  coord_flip() 
#counting most frequent words 
text_df %>%   count(word, sort = TRUE)  
library(ggplot2) text_df %>% 
  count(word, sort = TRUE) %>%   filter(n > 2000) %>%   mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +   geom_bar(stat = "identity") + 
  xlab(NULL) +   coord_flip() #text mining install.packages('slam') library(tm) 
setwd("C:/Users/ASUS/Desktop/ABA/ABA Project Data") mydata <-read.csv("h111_rep_tweets.csv",stringsAsFactors = FALSE) mydata_content <-paste(mydata$content,collapse = " ") mydata_source <- VectorSource(mydata_content) corpus<- Corpus(mydata_source) 
#cleaning corpus <- tm_map(corpus, content_transformer(tolower)) corpus <- tm_map(corpus, removePunctuation) corpus <- tm_map(corpus, stripWhitespace) 
corpus <- tm_map(corpus, removeWords, stopwords("english")) 
corpus <- tm_map(corpus, removeWords, c("amp", "rt", "tcot", "bit.ly", "tinyurl.com", 
                                        "http","new","just","today")) 
#making document term matrix dtm <-DocumentTermMatrix(corpus) dtm dtm2 <-as.matrix(dtm) #finding most frequent terms frequency <- colSums(dtm2) 
frequency <-sort(frequency,decreasing = TRUE) 
install.packages('wordcloud') library(wordcloud) words <- names(frequency) 
wordcloud(words[1:100],frequency[1:200]) 
#LDA Tuning #perplexity install.packages("purrr") 
library(purrr) library(ggplot2) perplexity(lda) 
n_topics <- c(2, 4, 10, 20, 50, 100) lda_compare <- n_topics %>%   map(LDA,x = dtm, control = list(seed = 1234)) data.frame(k = n_topics, 
                                                                                                                            perplex = map_dbl(lda_compare, perplexity)) %>%   ggplot(aes(k, perplex)) + 
  geom_point() +   geom_line() + 
  labs(title = "Evaluating LDA topic models",        subtitle = "Optimal number of topics (smaller is better)",        x = "Number of topics",        y = "Perplexity") 
#select number of ideal k values  install.packages("ldatuning") install.packages("devtools") 
devtools::install_github("nikita-moor/ldatuning") install.packages('slam') library("slam") library("ldatuning") library("topicmodels") result <- FindTopicsNumber( 
  dtm, 
  topics = seq(from = 2, to = 15, by = 1), 
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),   method = "Gibbs",   control = list(seed = 77),   mc.cores = 2L, 
  verbose = TRUE 
) 
FindTopicsNumber_plot(result) 
#topic modelling install.packages("RTextTools", repos = "https://cran.r-project.org") install.packages("tm", repos = "https://cran.r-project.org") install.packages("topicmodels", repos = "https://cran.r-project.org") install.packages("slam", repos = "https://cran.r-project.org") library(RTextTools) library(tm) library(wordcloud) library(topicmodels) 
lda <- LDA(dtm, k = 6, control = list(seed = 1234)) 
lda 
library(tidytext) lda_td <- tidy(lda) 
lda_td 
library(ggplot2) 
library(dplyr) top_terms <- lda_td %>%   group_by(topic) %>%   top_n(15, beta) %>%   ungroup() %>% 
  arrange(topic, -beta) top_terms %>% 
  mutate(term = reorder(term, beta)) %>%   ggplot(aes(term, beta, fill = factor(topic))) +   geom_bar(stat = "identity", show.legend = FALSE) +   facet_wrap(~ topic, scales = "free") + 
  coord_flip() 
#Topic Aggregation #  Install Requried Packages installed.packages("SnowballC") installed.packages("tm") installed.packages("twitteR") installed.packages("syuzhet") # Load Requried Packages library("SnowballC") library("tm") library("twitteR") library("syuzhet") library(data.table) 
data_republicans <- fread("C:/Users/uia91182/Desktop/aba project/datasets/republicans_tweets (1).csv", sep=",", header=T, 
                          strip.white = T, na.strings = c("NA","NaN","","?"))  # install.packages("tidytext", repos = "https://cran.r-project.org") # install.packages("dplyr", repos = "https://cran.r-project.org") library(dplyr) library(tidytext) 
text_df <- data_frame(line = 1:38415, text = data_republicans$Content) head(text_df) 
#removing hashtag , urls and other special charactersR 
tweets.df2 <- gsub("http.*","",text_df) 
tweets.df2 <- gsub("https.*","",tweets.df2) 

tweets.df2 <- gsub("#.*","",tweets.df2) tweets.df2 <- gsub("@.*","",tweets.df2) head(tweets.df2) 
text = data_republicans$Content 
text_df %>% 
unnest_tokens(word, text) # This means that in data frame text_df, tokenize column "text" by each republicans_with_hcr <-text[grep(pattern = "hcr", text, ignore.case = T)] getwd() 
write.csv(republicans_with_hcr, "republicans_with_hcr.csv") 
republicans_with_hcrjobs <-republicans_with_hcr[grep(pattern = "jobs", text, ignore.case = T)] write.csv(republicans_with_hcrjobs, "republicans_with_hcrjobs.csv") republicans_with_obama<-text[grep(pattern = "Obama", text, ignore.case = T)] write.csv(republicans_with_obama, "republicans_with_obama.csv") 
republicans_with_obamacare <-republicans_with_obama[grep(pattern = "care", text, ignore.case = T)] write.csv(republicans_with_obamacare , "republicans_with_obamacare.csv") republicans_with_energy <-text[grep(pattern = "energy", text, ignore.case = T)] write.csv(republicans_with_energy, "republicans_with_energy.csv") 
republicans_with_energytax <-republicans_with_energy[grep(pattern = "tax", text, ignore.case = T)] write.csv(republicans_with_energytax , "republicans_with_energytax.csv") 
#Sentiment Analysis #import data 
work_dir<-"C:/Users/Desktop/ABA/ABA Project Data" 
getwd() setwd(work_dir) 
install.packages("data.table",repos = "https://cran.r-project.org") 
#Read Data 
library(data.table) 
data <- fread("republicans_with_obamacare.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?") , stringsAsFactors = FALSE) View(data) 
summary(data) 
install.packages("tidytext", repos = "https://cran.r-project.org") install.packages("dplyr", repos = "https://cran.r-project.org") library(dplyr) library(tidytext) 
text_df <- data_frame(line = 1:319, text = data$x) head(text_df) #cleaning 
text_df <- gsub("http.*","",text_df$text) text_df2 <- gsub("https.*","",text_df) 
head(text_df2) #Sentiment R if (!require("pacman")) install.packages("pacman") pacman::p_load(sentimentr, dplyr, magrittr) 
library("sentimentr") library("syuzhet") 
sentiment_value<-sentiment(text_df2) 
category_sentiments <- ifelse(sentiment_value$sentiment < 0, "Negative", ifelse(sentiment_value$sentiment 
                                                                                > 0, "Positive", "Neutral")) 
category_sentiments2 <- cbind(text_df2,category_sentiments) head(category_sentiments2) category_sentiments  
table(category_sentiments) 
write.csv(category_sentiments, "category_sentiments_republican_with_obamacare1.csv") 
#Syuzhet library("syuzhet") words_df <- as.vector(text_df) 
text_df 
sentiment_df <- get_nrc_sentiment(words_df) sentiment_df2 <- cbind(text_df, sentiment_df)  
head(sentiment_df2) #getting positive sentiments 
sentiment_value <- get_sentiment(words_df) 
most_positive <- words_df[sentiment_value == max(sentiment_value)] most_positive 
#getting negative sentiments 
most_negative <- words_df[sentiment_value <= min(sentiment_value)]  
most_negative 
sentiment_value 
positive_tweets <- words_df[sentiment_value > 0] head(positive_tweets) 
negative_tweets <- words_df[sentiment_value < 0]  head(negative_tweets) 
neutral_tweets <- words_df[sentiment_value == 0]  head(neutral_tweets) 
# Alternate way to classify as Positive, Negative or Neutral tweets 
category_sentiments <- ifelse(sentiment_value < 0, "Negative", ifelse(sentiment_value > 0, "Positive", 
                                                                      "Neutral")) 
head(category_sentiments)   
category_sentiments2 <- cbind(text_df,category_sentiments) 
head(category_sentiments2) 
category_sentiments  
table(category_sentiments)
category_sentiments 
write.csv(category_sentiments, "category_sentiments_republican_with_obamacare2.csv") 