## Exploratory Data Analysis

## Filter non-english texts

corpus.df <- data.frame(rawtext = sapply(corpus, as.character), stringsAsFactors=FALSE)

sample(corpus.df$rawtext,10,replace=FALSE)

rm(corpus)

gc()

## Percentage of non-english words

## Package cldr from http://cran.r-project.org/web/packages/cldr/  brings Google Chrome's language detection to R.

# we can install it from archive

# url <- "http://cran.us.r-project.org/src/contrib/Archive/cldr/cldr_1.1.0.tar.gz"
# pkgFile<-"cldr_1.1.0.tar.gz"
# download.file(url = url, destfile = pkgFile)
# install.packages(pkgs=pkgFile, type="source", repos=NULL)
# unlink(pkgFile)

library(dplyr)
library(cldr)

corpus.df <- corpus.df %>%
  mutate(language = detectLanguage(rawtext)$detectedLanguage)

table(corpus.df$language)

sum(corpus.df$language == "ENGLISH")
sum(corpus.df$language != "ENGLISH")

# Let's check some of the non-english detected texts

sample_n(filter(corpus.df, language != "ENGLISH"), 20)

# We can remove this

# Using textcat library, we can notice too many misclassifications

library(textcat)

corpus.df <- corpus.df %>%
  mutate(language2 = textcat(rawtext))

table(corpus.df$language2)

sum(corpus.df$language2 == "english", na.rm = T)
sum(corpus.df$language2 != "english", na.rm = T)
sum(is.na(corpus.df$language2))

# Let's check some of the non-english detected texts

set.seed(31102017)
sample_n(filter(corpus.df, language2 != "english"), 20)

# Many english texts were misclassified as non-english
# We will eliminate non-english texts using the cldr package instead.

corpus.df <- corpus.df %>%
  filter(language == "ENGLISH") %>%
  select(rawtext)

### Distributions of word frequencies

## Generate the Word cloud

## The importance of words can be illustrated as a word cloud as follow :

library(RWeka)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)


ngram.1 <- data.frame(table(NGramTokenizer(corpus.df, Weka_control(min=1, max=1))));

names(ngram.1)[names(ngram.1)=='Var1']<-'Word';

ngram.1 <- ngram.1[order(ngram.1$Freq, decreasing = TRUE),] 

wordcloud(words = ngram.1$Word, freq = ngram.1$Freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

barplot(ngram.1[1:50,]$Freq, las = 2, names.arg = ngram.1[1:50,]$Word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")




ngram.2 <- data.frame(table(NGramTokenizer(corpus.df, Weka_control(min=2, max=2))));

names(ngram.2)[names(ngram.2)=='Var1']<-'Word';

ngram.2 <- ngram.2[order(ngram.2$Freq, decreasing = TRUE),] 

wordcloud(words = ngram.2$Word, freq = ngram.2$Freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

barplot(ngram.2[1:50,]$Freq, las = 2, names.arg = ngram.2[1:50,]$Word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


ngram.3 <- data.frame(table(NGramTokenizer(corpus.df, Weka_control(min=3, max=3))))

names(ngram.3)[names(ngram.3)=='Var1']<-'Word';

ngram.3 <- ngram.3[order(ngram.3$Freq, decreasing = TRUE),] 

wordcloud(words = ngram.3$Word, freq = ngram.3$Freq, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

barplot(ngram.3[1:50,]$Freq, las = 2, names.arg = ngram.3[1:50,]$Word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

words.dictionary <- ngram.1 %>%
  mutate(Freq_cumsum = cumsum(Freq)) %>%
  mutate(Dictionary_perc = Freq_cumsum/sum(Freq))

# Number of words forming 50% of the dictionary

n_words_50 <- words.dictionary %>%
  group_by(Dictionary_perc >= 0.5) %>%
  mutate(Cut = first(Dictionary_perc)) %>%
  ungroup() %>%
  mutate(Cut = max(Cut)) %>%
  filter(Dictionary_perc <= Cut) %>%
  select(Word, Freq, Dictionary_perc) 

## The number of words that form 50% of the dictionary is 

words_50 <- as.character(n_words_50$Word)

length(words_50)


# Number of words forming 90% of the dictionary

n_words_90 <- words.dictionary %>%
  group_by(Dictionary_perc >= 0.9) %>%
  mutate(Cut = first(Dictionary_perc)) %>%
  ungroup() %>%
  mutate(Cut = max(Cut)) %>%
  filter(Dictionary_perc <= Cut) %>%
  select(Word, Freq, Dictionary_perc) 

## The number of words that form 50% of the dictionary is 

words_90 <- as.character(n_words_90$Word)

length(words_90)

#Calculate probabilities

ngram.1_count <- sum(final_unigram$freq)
ngram.1_count <- sum(final_bigram$freq)
ngram.1_count <- sum(final_trigram$freq)
ngram.1_count <- sum(final_fourgram$freq)
