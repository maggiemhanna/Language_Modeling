setwd("/Users/az01661/Dropbox/NLP Predicting Next Word in Text")

## Loading only samples of data

## Read a sample of 50,000 lines of data only

n <- 50000


con1 <- file("final/en_US/en_US.blogs.txt", open = "r")
con2 <- file("final/en_US/en_US.news.txt", open = "r")
con3 <- file("final/en_US/en_US.twitter.txt", open = "r")


readLinesSampled <- function(con, n) {
  texts <- readLines(con, n)
  
  k <- n
  
  while (length(curline <- readLines(con, 1))) {
    k <- k + 1
    if (runif(1) < n/k) {
      texts[sample(n, 1)] <- curline
    }
  }
  
  close(con)
  return(texts)
}

set.seed(251017)
blogs <- readLinesSampled(con1, n)
set.seed(251017)
news <- readLinesSampled(con2, n)
set.seed(251017)
twitter <- readLinesSampled(con3, n)

## We will use the library tm to perform some text mining

library(tm)

corpus <- c(blogs, news, twitter)

rm(twitter);
rm(blogs);
rm(news);

corpus <- VCorpus(VectorSource(corpus))

inspect(corpus[[1]])

## Corpus Preprocessing 


corpus<-tm_map(corpus, removeNumbers)

corpus<-tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)

corpus<-tm_map(corpus, content_transformer(tolower))


# In this types of problems, we don't want to remove stopwords
# In this types of applications, we can't to stem

## Profanity Filtering

getProfanityWords <- function() {
  profanityFileName <- "profanity.txt"
  if (!file.exists(profanityFileName)) {
    profanity.url <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
    download.file(profanity.url, destfile = profanityFileName, method = "curl")
  }
  if (sum(ls() == "profanity") < 1) {
    ProfanityWords <- readLines(profanityFileName)
  }
  return(ProfanityWords)
}


corpus <- tm_map(corpus, removeWords, getProfanityWords())

corpus <- tm_map(corpus, stripWhitespace)



