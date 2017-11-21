## Modeling

### Before modeling, let's divide our data.frame into training and testing sets

### Since the total size is almost 145000, we will use 130000 for training and the rest for testing

#### Shuffle corpus.df
corpus.df <- sample_n(corpus.df, size = nrow(corpus.df), replace = F)

train.df <- corpus.df[1:130000, ]
test.df <- corpus.df[130001:nrow(corpus.df), ]

writeLines(test.df, 'final/testset.txt')

require(reshape)
library(RWeka)

generateNgramData <- function(n, corpus = train.df){
  ngram <- data.frame(table(NGramTokenizer(corpus, Weka_control(min=n, max=n))))
  names(ngram)[names(ngram)=='Var1']<-'Word';
  ngram <- ngram[order(ngram$Freq, decreasing = TRUE),] 
  
  if(n == 2) columns <- c('one', 'two')
  if(n == 3) columns <- c('one', 'two', 'three')
  if(n == 4) columns <- c('one', 'two', 'three', 'four')
  
  if(n > 1) {
    ngram <- transform(ngram, Word = colsplit(Word, split = " ", names = columns ))
  }
  
  rownames(ngram) <- NULL
  ngram
}

ngram.1 <- generateNgramData(1)
ngram.2 <- generateNgramData(2)
ngram.3 <- generateNgramData(3)
ngram.4 <- generateNgramData(4)


## Out of the selected 15000 lines in the corpus

## The number of unigrams (tokens) is
sum(ngram.1$Freq)

## The number of bigrams (tokens) is
sum(ngram.2$Freq)

## The number of trigrams (tokens) is
sum(ngram.3$Freq)

#Calculate probabilities

# ngram.1.count <- sum(ngram.1$Freq)
# ngram.2.count <- sum(ngram.2$Freq)
# ngram.3.count <- sum(ngram.3$Freq)
# ngram.4.count <- sum(ngram.4$Freq)

# ngram.1 <- transform(ngram.1)

## We need to perform some transformations on column namess

ngram.2 <- transform(ngram.2, Wordone = Word$one, Wordtwo = Word$two, Word = NULL)
ngram.3 <- transform(ngram.3, Wordone = Word$one, Wordtwo = Word$two, Wordthree = Word$three, Word = NULL)
ngram.4 <- transform(ngram.4, Wordone = Word$one, Wordtwo = Word$two, Wordthree = Word$three, Wordfour = Word$four, Word = NULL)

# We can calculate conditional probabilities p(w_n|w_(n-1),...,w_(n-k)) in case we want to display them at prediction
# These probabilities are equivalent to Freq, they are not needed for modeling

# ngram.2 <- ngram.2 %>%
#   group_by(Wordone) %>%
#   mutate(pw = Freq/sum(Freq)) %>%
#   ungroup()
# 
# ngram.3 <- ngram.3 %>%
#   group_by(Wordone, Wordtwo) %>%
#   mutate(pw = Freq/sum(Freq)) %>%
#   ungroup()
# 
# ngram.4 <- ngram.4 %>%
#   group_by(Wordone, Wordtwo, Wordthree) %>%
#   mutate(pw = Freq/sum(Freq)) %>%
#   ungroup()
