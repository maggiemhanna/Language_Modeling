# Perplexity is theoretically elegant
# as its logarithm is an upper bound on the number of bits per word
# expected in compressing (in-domain) text employing the measured
# model. Unfortunately, while language models with lower perplexities
# tend to have lower word-error rates, there have been numerous
# examples in the literature where language models providing a large
# improvement in perplexity over a baseline model have yielded little
# or no improvement in word-error rate. In addition, perplexity
# is inapplicable to unnormalized language models (i.e., models that
# are not true probability distributions that sum to 1), and perplexity
# is not comparable between language models with different vocabularies.

# In our example, the simple backoff model we built is not a true probability distribution.
# And the vocabulary of the test set and the train set is not really exactly the same.
# We might have some words that appeared in the test set which didnot appear in the training set.

# We'll try and build a very simple evaluation model.
# In our model, we try and predict the next word by giving 5 choices.
# Using our model, we can calculate the percentage of times the next word is found among these choices.

# Remove and clean all memory

rm(list = ls())
gc()

library(dplyr)
library(tm)
library(plyr)
library(stringr)
library(readr)
library(RWeka)


s_probability <- function(s){
  if(nchar(s) > 0) {
    s_ngrams <- c(NGramTokenizer(s, Weka_control(min=1, max=1))[1], 
    NGramTokenizer(s, Weka_control(min=2, max=2))[1], 
    NGramTokenizer(s, Weka_control(min=3, max=3))[1], 
    NGramTokenizer(s, Weka_control(min=4, max=4)))
    
    s_ngrams <- s_ngrams[!is.na(s_ngrams)]
    
    return(mean(sapply(s_ngrams, wc_probability, USE.NAMES = FALSE)))
  } else {
    return(NA)
  }
    
}


wc_probability <- function(s_ngram) {
  
  last_word <- word(s_ngram, -1)
  input_text <- word(s_ngram, 1, -2)
  
  choices <- Predictor(input_text)
  
  if(last_word %in% choices) {
    return(1) 
  } else {
    return(0)
  }
    
}

testset <- readLines('final/testset.txt')

ModelPerformance <- data.frame(Model = character(), LoadTime = numeric(), AvgPredictionTime = numeric(), AvgScore = numeric(), ModelSize = character())

Examples <-c("you are so", "a lot of", "one of the", "out of the", "as well as", "going to be", 
             "the united states", "thanks for the", "it would be", "some of the")



for(file in list.files(pattern = "Model")){
  ## Calculating loading time
  load.time.start <- Sys.time()
  load(file = paste0(file, "/data.Rdata"))
  source(file = paste0(file, "/Predictor.R"))
  load.time <- Sys.time() - load.time.start
  
  if(!existsFunction("Predictor")) {
    Predictor <- Katz_Predictor
    rm(Katz_Predictor)
  }
  
  ## Calculating average prediction time
  pred.time.start <- Sys.time()
  sapply(Examples, Predictor)
  pred.time <- Sys.time() - pred.time.start
  pred.time <- pred.time/length(Examples)
  
  objects.size <- sapply(ls()[grepl("^ngram", ls())],function(x){format(object.size(get(x)), units = "Mb")})
  objects.size <- paste0(sum(parse_number(objects.size)), units = " Mb")
  
  # testset is already shuffled
  # let's use only 20 lines for now, as it takes too much time
  
  score <- mean(sapply(testset[1:20], s_probability), na.rm = T)
  
  ModelPerformance <- rbind(ModelPerformance, data.frame(Model = file, LoadTime = load.time, AvgPredictionTime = pred.time, AvgScore = score, ModelSize = objects.size))
  
  rm(list=setdiff(ls(), c("ModelPerformance", "Examples", "testset", "s_probability", "wc_probability")))
  gc()
}



