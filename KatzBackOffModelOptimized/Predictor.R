Katz_Predictor <-
function(text_input) {
  
  # if(nchar(text_input) > 0) {
  
  corpus_input <- VCorpus(VectorSource(text_input))
  
  corpus_input<-tm_map(corpus_input, removeNumbers)
  
  corpus_input<-tm_map(corpus_input, removePunctuation, preserve_intra_word_dashes = TRUE)
  
  corpus_input<-tm_map(corpus_input, content_transformer(tolower))
  
  text_input <- sapply(corpus_input, as.character)
  
  text_input_list = strsplit(text_input, " ")[[1]]
  
  if(length(text_input_list) == 0){
    
    next_words <- katz.backoff.ngram.1.search() %>%
      select(Word) %>% 
      filter(row_number() <= 5)
    
    next_words <- next_words$Word
  }
  
  if(length(text_input_list) == 1){
    
    next_words <- katz.backoff.ngram.2.search(text_input_list[1]) %>%
      select(Wordtwo) %>% 
      filter(row_number() <= 5)
    
    next_words <- next_words$Wordtwo
  }
  
  if(length(text_input_list) == 2){
    
    next_words <- katz.backoff.ngram.3.search(text_input_list[1], text_input_list[2]) %>%
      select(Wordthree) %>% 
      filter(row_number() <= 5)
    
    next_words <- next_words$Wordthree
  }
  
  if(length(text_input_list) >= 3){
    
    n = length(text_input_list)
    
    next_words <- katz.backoff.ngram.4.search(text_input_list[n-2], text_input_list[n-1], text_input_list[n]) %>%
      select(Wordfour) %>% 
      filter(row_number() <= 5)
    
    next_words <- next_words$Wordfour
  }
  return(as.character(next_words)) 
  # }
}
katz.backoff.ngram.1.search <-
function(unigram = ngram.1.sample, remove_words = NULL){
  ngram.1.f <- unigram %>% # return words with their P_bo and sum P_bo depending on unigram
    filter(!(Word %in% remove_words)) 
  
  if(!("P_bo_sum" %in% names(unigram)))
    ngram.1.f <- ngram.1.f %>% 
      mutate(P_bo_sum = sum(P_bo))
  
  ngram.1.f
}
katz.backoff.ngram.2.search <-
function(word1, bigram = ngram.2.sample,  remove_words = NULL){
  
  # We filter ngram.2 to include only bigrams such that wordone is the text_input
  ngram.2.f <- bigram %>% 
    filter(!(Wordtwo %in% remove_words)) %>%
    filter(Wordone == word1)
  
  if(nrow(ngram.2.f) > 0){
    alpha.2 <- ngram.2.f$alpha[1]
  } else{
    alpha.2 <- 1
  } 
  
  # We add the discounted probability (backoff probability) c*/c to the bigram words (k>0)
  ngram.2.f <- ngram.2.f %>% 
    filter(!(is.na(P_bo) & Freq == 1)) %>%
    mutate(P_bo = ifelse(is.na(P_bo) & Freq > 1, p_ml, P_bo))
  
  # We filter ngram.1 to include only Words that don't appear as Wordtwo in the filtered bigrams
  remove_words <- c(remove_words, as.character(ngram.2.f$Wordtwo))
  
  ngram.1.f <- katz.backoff.ngram.1.search(remove_words = remove_words) %>%
    mutate(P_bo = alpha.2*(P_bo/P_bo_sum)) %>%
    dplyr::rename(Wordtwo = Word)
  
  ngram.2.f <- rbind.fill(ngram.2.f, ngram.1.f) %>%
    arrange(desc(P_bo)) 
  
  if(!("P_bo_sum" %in% names(bigram)))
    ngram.2.f <- ngram.2.f %>% 
    mutate(P_bo_sum = sum(P_bo))
  
  ngram.2.f
}
katz.backoff.ngram.3.search <-
function(word1, word2, trigram = ngram.3.sample, remove_words = NULL) {
  
  ngram.3.f <- trigram %>% 
    filter(!(Wordthree %in% remove_words)) %>%
    filter(Wordone == word1, Wordtwo == word2)
  
  if(nrow(ngram.3.f) > 0){
    alpha.3 <- ngram.3.f$alpha[1]
  } else{
    alpha.3 <- 1
  }
  
  ngram.3.f <- ngram.3.f %>% 
    filter(!(is.na(P_bo) & Freq == 1)) %>%
    mutate(P_bo = ifelse(is.na(P_bo) & Freq > 1, p_ml, P_bo))
  
  # If all trigrams have the same count with n=1, the assocaited P_bo will be NA, 
  # the probability estimated combined proportion of all undetected species is 1
  # these elements should be filtered, so that they are not removeed from the bigrams
  
  # If all trigrams have the same count with n>1, the assocaited P_bo will be NA, 
  # the probability estimated combined proportion of all undetected species is 0
  # P_bo should be then replaced with p_ml
  
  remove_words <- c(remove_words, as.character(ngram.3.f$Wordthree))
  
  ngram.2.f <- katz.backoff.ngram.2.search(word2, remove_words = remove_words) %>%
    mutate(P_bo = alpha.3*(P_bo/P_bo_sum)) %>%
    dplyr::rename(Wordthree = Wordtwo, Wordtwo = Wordone)
  
  
  ngram.3.f <- rbind.fill(ngram.3.f, ngram.2.f) %>%
    arrange(desc(P_bo)) 
  
  if(!("P_bo_sum" %in% names(trigram)))
    ngram.3.f <- ngram.3.f %>% 
    mutate(P_bo_sum = sum(P_bo))
  
  ngram.3.f
}
katz.backoff.ngram.4.search <-
function(word1, word2, word3, fourgram = ngram.4.sample, remove_words = NULL) {
  
  ngram.4.f <- fourgram %>% 
    filter(!(Wordfour %in% remove_words)) %>%
    filter(Wordone == word1, Wordtwo == word2, Wordthree == word3)
  
  if(nrow(ngram.4.f) > 0){
    alpha.4 <- ngram.4.f$alpha[1]
  } else{
    alpha.4 <- 1
  }
  
  ngram.4.f <- ngram.4.f %>%
    filter(!(is.na(P_bo) & Freq == 1)) %>%
    mutate(P_bo = ifelse(is.na(P_bo) & Freq > 1, p_ml, P_bo))
  
  remove_words <- c(remove_words, as.character(ngram.4.f$Wordfour))
  
  ngram.3.f <- katz.backoff.ngram.3.search(word2, word3,  remove_words = remove_words) %>%
    mutate(P_bo = alpha.4*(P_bo/P_bo_sum)) %>%
    dplyr::rename(Wordfour = Wordthree, Wordthree = Wordtwo, Wordtwo = Wordone)
  
  ngram.4.f <- rbind.fill(ngram.4.f, ngram.3.f) %>%
    arrange(desc(P_bo)) 
  
  if(!("P_bo_sum" %in% names(fourgram)))
    ngram.4.f <- ngram.4.f %>% 
    mutate(P_bo_sum = sum(P_bo))
  
  ngram.4.f
}
