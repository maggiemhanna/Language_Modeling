# Keep only the First 5 most powerful predictions in the sample
# This will help reduce significantly the size of the ngrams data.frames

ngram.1.sample <- ngram.1 %>%
  arrange(desc(Freq)) %>%
  filter(row_number() <= 5) %>%
  ungroup()

ngram.2.sample <- ngram.2 %>%
  group_by(Wordone) %>%
  arrange(Wordone, desc(Freq)) %>%
  filter(row_number() <= 5) %>%
  ungroup()

ngram.3.sample <- ngram.3 %>%
  group_by(Wordone, Wordtwo) %>%
  arrange(Wordone, Wordtwo, desc(Freq)) %>%
  filter(row_number() <= 5) %>%
  ungroup()

ngram.4.sample <- ngram.4 %>%
  group_by(Wordone, Wordtwo, Wordthree) %>%
  arrange(Wordone, Wordtwo, Wordthree, desc(Freq)) %>%
  filter(row_number() <= 5) %>%
  ungroup()


unigramsearch <- function(nb_words = 5, remove_words = "") {
  next_words <-  ngram.1.sample %>% 
    filter(!(Word %in% remove_words)) %>%
    filter(row_number() <= nb_words) %>%
    select(Word)
  
  next_words <- as.vector(next_words$Word)
  next_words
}

bigramsearch <- function(word1, nb_words = 5, remove_words = "") {
  next_words <- ngram.2.sample %>% 
    filter(Wordone == word1) %>% 
    select(Wordtwo) %>%
    filter(!(Wordtwo %in% remove_words)) %>%
    filter(row_number() <= nb_words)
  
  next_words <- as.vector(next_words$Wordtwo)
  
  if(length(next_words) < nb_words)
    next_words <- c(next_words, unigramsearch(nb_words - length(next_words), next_words))
  
  next_words
}

trigramsearch <- function(word1, word2, nb_words = 5, remove_words = "") {
  next_words <- ngram.3.sample %>% 
    filter(Wordone == word1, Wordtwo == word2) %>% 
    select(Wordthree) %>%
    filter(!(Wordthree %in% remove_words)) %>%
    filter(row_number() <= nb_words)
  
  next_words <- as.vector(next_words$Wordthree)
  
  if(length(next_words) < nb_words)
    next_words <- c(next_words, bigramsearch(word2, nb_words - length(next_words), next_words))
  
  next_words
}

fourgramsearch <- function(word1, word2, word3, nb_words = 5, remove_words = "") {
  next_words <- ngram.4.sample %>% 
    filter(Wordone == word1, 
           Wordtwo == word2,
           Wordthree == word3) %>%
    select(Wordfour) %>%
    filter(!(Wordfour %in% remove_words)) %>%
    filter(row_number() <= nb_words)
  
  next_words <- as.vector(next_words$Wordfour)
  
  if(length(next_words) < nb_words)
    next_words <- c(next_words, trigramsearch(word2, word3, nb_words - length(next_words), next_words))
  
  next_words
}


Predictor <- function(text_input) {
  
  # if(nchar(text_input) > 0) {
    
    corpus_input <- VCorpus(VectorSource(text_input))
    
    corpus_input<-tm_map(corpus_input, removeNumbers)
    
    corpus_input<-tm_map(corpus_input, removePunctuation, preserve_intra_word_dashes = TRUE)
    
    corpus_input<-tm_map(corpus_input, content_transformer(tolower))
    
    text_input <- sapply(corpus_input, as.character)
    
    text_input_list = strsplit(text_input, " ")[[1]]
    
    # Predicting first word
    if(length(text_input_list) == 0){
      # We use unigrams to predict the next word
      next_words <- unigramsearch()
    }
    
    if(length(text_input_list) == 1){
      # We use bigrams to predict the next word
      next_words <- bigramsearch(text_input_list[1])
    }
    
    if(length(text_input_list) == 2){
      # We use trigrams to predict the next word
      next_words <- trigramsearch(text_input_list[1], text_input_list[2])
    } 
    
    if(length(text_input_list) >= 3){
      # We use 4 grams to predict the next word
      n = length(text_input_list)
      next_words <- fourgramsearch(text_input_list[n-2], text_input_list[n-1], text_input_list[n])
    }    
    
    next_words <- as.vector(next_words)
    next_words
  # }
}


dir.create("SimpleBackOffModelOptimized")

save(ngram.1.sample, ngram.2.sample, ngram.3.sample, ngram.4.sample, file = "SimpleBackOffModelOptimized/data.Rdata")

dump(list = c("Predictor", "unigramsearch", "bigramsearch", "trigramsearch", "fourgramsearch"), file = "SimpleBackOffModelOptimized/Predictor.R")
