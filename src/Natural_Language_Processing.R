# Examples Using Katz Backoff Model

library(dplyr)
library(tm)
library(plyr)

file <- "KatzBackOffModel"

load(file = paste0(file, "/data.Rdata"))
source(file = paste0(file, "/Predictor.R"))

if(!existsFunction("Predictor")) {
  Predictor <- Katz_Predictor
  rm(Katz_Predictor)
}


s_probability <- function(s){
  if(nchar(s) > 0) {
    
    s_ngrams <- c(NGramTokenizer(s, Weka_control(min=1, max=1))[1], 
                  NGramTokenizer(s, Weka_control(min=2, max=2))[1], 
                  NGramTokenizer(s, Weka_control(min=3, max=3))[1], 
                  NGramTokenizer(s, Weka_control(min=4, max=4)))
    
    s_ngrams <- s_ngrams[!is.na(s_ngrams)]
    
    return(prod(sapply(s_ngrams, wc_probability)))
  }
}

# conditional probabilities of the ngrams in the sentence

wc_probability <- function(s_ngram) {
  
  s_ngram_list = strsplit(s_ngram, " ")[[1]]
  
  if(length(s_ngram_list) == 1)
    pr <- katz.backoff.ngram.1.probability(s_ngram_list[1])
  
  if(length(s_ngram_list) == 2)
    pr <- katz.backoff.ngram.2.probability(s_ngram_list[1], s_ngram_list[2])
  
  if(length(s_ngram_list) == 3)
    pr <- katz.backoff.ngram.3.probability(s_ngram_list[1], s_ngram_list[2], s_ngram_list[3])
  
  if(length(s_ngram_list) == 4)
    pr <- katz.backoff.ngram.4.probability(s_ngram_list[1], s_ngram_list[2], s_ngram_list[3], s_ngram_list[4])
  
  pr
  
}


katz.backoff.ngram.1.probability <- function(word){
  pr <- katz.backoff.ngram.1.search() %>%
    filter(Word == word) %>%
    select(P_bo)
  
  pr <- pr$P_bo
  
  if(length(pr) == 0)
    pr <-0
  
  pr
}

# pr(w_2|w_1)

katz.backoff.ngram.2.probability <- function(word1, word2){
  # Using katz.backoff.ngram.2.search(word1), we can find all the possible word2 that follows word1 with there corresponding probabilities
  # The size of katz.backoff.ngram.2.search(word1) should always be the same which is equal to the total number of possible words in the training set
  
  pr <- katz.backoff.ngram.2.search(word1) %>%
    filter(Wordtwo == word2) %>%
    select(P_bo)
  
  pr <- pr$P_bo
  
  if(length(pr) == 0)
    pr <-0
  
  pr
}


katz.backoff.ngram.3.probability <- function(word1, word2, word3){
  # Using katz.backoff.ngram.2.search(word1), we can find all the possible word2 that follows word1 with there corresponding probabilities
  # The size of katz.backoff.ngram.2.search(word1) should always be the same which is equal to the total number of possible words in the training set
  
  pr <- katz.backoff.ngram.3.search(word1, word2) %>%
    filter(Wordthree == word3) %>%
    select(P_bo)
  
  pr <- pr$P_bo
  
  if(length(pr) == 0)
    pr <-0
  
  pr
}


katz.backoff.ngram.4.probability <- function(word1, word2, word3, word4){
  # Using katz.backoff.ngram.2.search(word1), we can find all the possible word2 that follows word1 with there corresponding probabilities
  # The size of katz.backoff.ngram.2.search(word1) should always be the same which is equal to the total number of possible words in the training set
  
  pr <- katz.backoff.ngram.4.search(word1, word2, word3) %>%
    filter(Wordfour == word4) %>%
    select(P_bo)
  
  pr <- pr$P_bo
  
  if(length(pr) == 0)
    pr <-0
  
  pr
}


s_probability("arctic monkeys this weekend")
s_probability("arctic monkeys this decade")
s_probability("arctic monkeys this month")
s_probability("arctic monkeys this morning")

s_probability("helps reduce your hunger")
s_probability("helps reduce your stress")
s_probability("helps reduce your sleepiness")
s_probability("helps reduce your happiness")

s_probability("to take a look")
s_probability("to take a walk")
s_probability("to take a minute")
s_probability("to take a picture")

s_probability("to settle the case")
s_probability("to settle the account")
s_probability("to settle the matter")
s_probability("to settle the incident")

s_probability("bottom to the side")
s_probability("bottom to the center")
s_probability("bottom to the middle")
s_probability("bottom to the top")






