install.packages("quanteda")
install.packages("quanteda.textmodels")
install.packages("quanteda.textstats")
install.packages ("quanteda.textplot") 
devtools::install_github("quanteda/quanteda") 
devtools::install_github("kbenoit/quanteda.dictionaries")
install.packages("readtext")
install.packages("spacyr")
install.packages("caret")
library("quanteda")
library("readtext")
library(dplyr)



#https://rdocumentation.org/packages/base/versions/3.6.2 Icon

#Loading in files using the readtext package
dat_twitter <- readtext("C:/Users/Toshiba/Documents/Capstone/Data_Science_Capstone/en_US.twitter.txt", cache = FALSE )
dat_blogs <- readtext("C:/Users/Toshiba/Documents/Capstone/Data_Science_Capstone/en_US.blogs.txt", cache = FALSE )
dat_news <- readtext("C:/Users/Toshiba/Documents/Capstone/Data_Science_Capstone/en_US.news.txt", cache = FALSE )


#combine three data arrays into one
allData<-c(dat_twitter,dat_blogs,dat_news)
names(allData)



#make a sample size of 25% of the combined array
set.seed(123)
sub_allData<-allData[sample(1:length(allData),length(allData)*0.4)]
sub_allData<-as.character(sub_allData)
#create a corpus from the received data array
corp_sub_allData<-corpus(sub_allData)


#delete used datasets
rm(list=c("dat_twitter", "dat_blogs", "dat_news"))


library(dplyr)
# we will tokenize the corpus by removing numbers, punctuation marks and separators
dfmat_sub_allData<-tokens(corp_sub_allData,what = "word", remove_numbers = TRUE,
                          remove_punct = TRUE, remove_separators = TRUE )

# make a dfm, removing stopwords and applying stemming
dfmat_sub_allData<- dfm_remove(dfmat_sub_allData,  stopwords("english"))

# The number of tokens in the corpus
ntoken(dfmat_sub_allData)

## 20 most frequent words
topfeatures(dfmat_sub_allData, 20)

# Plotting a word cloud
set.seed(100)
library("quanteda.textplots")
textplot_wordcloud(dfmat_sub_allData, min_count = 2, max_words=200, random_order = FALSE, rotation = 0.35,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))




