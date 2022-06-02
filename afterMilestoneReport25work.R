rm(list=ls())
install.packages("data.table")
install.packages("sbo")
install.packages("devtools")

install.packages(sbo)
install.packages("dplyr")
install.packages("sentimentr")
install.packages("tidyverse")
install.packages("arules")
library(sentimentr)
library(quanteda)
library(readtext)
library(tidyverse)
library(arules)
library(devtools)
library(sbo)
library(data.table)
dat_blogs <- readLines("en_US.blogs.txt", encoding = "UTF-8",skipNul = TRUE)
dat_news <- readLines("en_US.news.txt", encoding = "UTF-8",skipNul = TRUE)
dat_twitter <- readLines("en_US.twitter.txt", encoding = "UTF-8",skipNul = TRUE)



# 1. Summary statistics of downloaded data
lines = sapply(list(dat_blogs,dat_news,dat_twitter), length)
characters <- sapply(list(dat_twitter,dat_blogs,dat_news), function(x){sum(nchar(x))})

summary = data.frame(files =c("en_US.blogs.txt","en_US.news.txt","en_US.twitter.txt"),characters,lines, size=c("200.4 MB","196.3 MB", "200.4 MB"))
summary

#take 10000 rows from each data set
part_dat_twitter<-sample(dat_twitter, 10000)
part_dat_blogs<-sample(dat_blogs, 10000)
part_dat_news<-sample(dat_news, 10000)

head(part_dat_twitter)
head(part_dat_blogs)
head(part_dat_news)

#take 10000 rows from each data set
merged_file= c(part_dat_twitter,part_dat_blogs, part_dat_news )
lines_merged_file<-length(merged_file)
merged_file<-sample(merged_file)
head(merged_file)

## Checking the text for profanity. Let's check the text
#using three profanity dictionaries in "sentimentr" packege:

alvares<-lexicon::profanity_alvarez
head(alvares)
arr_bad<-lexicon::profanity_arr_bad
banned<-lexicon::profanity_banned
head(arr_bad)
head(banned)

#Let's find the approximate number of profanity words in the text, given that
#the data has not been cleared

m1<-get_sentences(merged_file)

profanity1<-sum(profanity(m1, profanity_list = lexicon::profanity_alvarez)$profanity_count)
profanity2<-sum(profanity(m1, profanity_list = lexicon::profanity_arr_bad)$profanity_count)
profanity3<-sum(profanity(m1, profanity_list = lexicon::profanity_banned)$profanity_count)
sum_profanity<-profanity1+profanity2+profanity3
sum_profanity


# make a corpus
corp_merged_file<-corpus(merged_file)
head(merged_file)

#We perform tokenization using functions from the quanteda package. As a result,
#we get documents divided into words. Additionally, we perform important steps:
#clear the text and lower the case of each word. We also remove bad words from 
#the text using three dictionaries
#for further construction of a predictive model, we do not remove stop words


tok_merged_file<-tokens(corp_merged_file,what = "word", remove_punct = TRUE,
                             remove_symbols = TRUE,
                             remove_numbers = TRUE,
                             remove_separators = TRUE,
                             split_hyphens = TRUE,
                             include_docvars = TRUE,
                             remove_url=TRUE,
                             verbose = quanteda_options("verbose"))

#Applied dictionaries to clean up profanity
tok_merged_file1 <- tokens_select(tok_merged_file, c(alvares, arr_bad,banned),
                              selection="remove" )


# для уменьшения покрытия стемизация
#trial_token4 <- tokens_wordstem(trial_token3)
#number of deleted bad words in tokens:
sum_bw<-sum(ntoken(tok_merged_file))-sum(ntoken(tok_merged_file1))
sum_bw
# The number of tokens in the corpus:
sum_nt<-sum(ntoken(tok_merged_file1))
sum_nt
#To view the result, we will create a small selection
sample(tok_merged_file1, 1, FALSE)


#Make dfm objekt.We use the universal function dfm() of the package "quanteda"
dfm_tok_merged_file<-dfm(tok_merged_file1)

## find out the 50 most frequent words:
library(quanteda)
library(quanteda.textplots)
topfeatures(dfm_tok_merged_file, 50)
#We have not deleted stop words, so we see them in our top 50.
# I assume that stop words may be useful for creating a prediction model, 
# but this is only the initial version.


#Plotting the 50 most frequent terms using ggplot2:

library(quanteda.textstats)
library(ggplot2)

textstat_frequency(dfm_tok_merged_file, n = 50) %>% 
  ggplot(aes(x = reorder(feature, -rank), y = frequency)) +
  geom_bar(stat = "identity") + coord_flip() + 
  labs(x = "", y = "Term Frequency as a Percentage")

# To visualize the data analysis of the text corpus, we will build a word cloud:
library("quanteda.textplots")
set.seed(100)
textplot_wordcloud(dfm_tok_merged_file, min_count = 6, max_words=300, random_order = FALSE, rotation = 0.25,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))

### 4.For further text analysis, we will create a set of n-grams. let's create 
# a set of ipidgams -1 word, bigrams- 2 words, trigrams- 3 words
library(data.table)
library(quanteda.textstats)


unigrams_freq <- textstat_frequency(dfm_tok_merged_file)  # unigram frequency
unigrs <- as.data.table(subset(unigrams_freq,select=c(feature,frequency)))
names(unigrs) <- c("ngram","freq")
fwrite(unigrs,"unigrs.csv")
head(unigrs)


bigrams <- dfm(tokens_ngrams(tok_merged_file1, n = 2))
bigrams_freq <- textstat_frequency(bigrams)                  # bigram frequency
bigrs <- subset(bigrams_freq,select=c(feature,frequency))
names(bigrs) <- c("ngram","freq")
bigrs <- as.data.table(bigrs)
fwrite(bigrs,"bigrs.csv")
head(bigrs)

trigrams <- dfm(tokens_ngrams(tok_merged_file1, n = 3))
trigrams_freq <- textstat_frequency(trigrams)                  # trigram frequency
trigrs <- subset(trigrams_freq,select=c(feature,frequency))
names(trigrs) <- c("ngram","freq")
trigrs <- as.data.table(trigrs)
fwrite(trigrs,"trigrs.csv")
head(trigrs)

#Using the topfeatures() function, we visualize the number of the most get
#n-grams from the dfm. Let's build the top 50 most frequently used bigrams 
#and trigrams in our text. To build, we use ggplot2

library(ggplot2)
textstat_frequency(trigrams, n = 50) %>% 
  ggplot(aes(x = reorder(feature, -rank), y = frequency)) +
  geom_bar(stat = "identity") + coord_flip() + 
  labs(x = "", y = "Term Frequency as a Percentage")

textstat_frequency(bigrams, n = 50) %>% 
  ggplot(aes(x = reorder(feature, -rank), y = frequency)) +
  geom_bar(stat = "identity") + coord_flip() + 
  labs(x = "", y = "Term Frequency as a Percentage")
#How many unique words  need in a frequency sorted dictionary to cover 50% of
#all word instances in the language? 90%?
fw<-moby_dfm(dfm_tok_merged_file)

corp_merged_file_fr<-get_frequencies_words(corp_merged_file, 1)
summary(sample_fr$Freq)
wordcoverage(merged_file$Freq,0.5)

wordcoverage(tok_merged_file1$Freq,0.9)
head(dfm_tok_merged_file)


kw <- kwic(dfm_tok_merged_file, pattern = "case of*", valuetype = "glob", window = 3)
kwic(tok_merged_file1, pattern = phrase("case of*"), valuetype = "glob", window = 2)
gp<-grep("case of",trigrs,10)

head(kw)
?grep