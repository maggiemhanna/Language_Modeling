# Katz's BackOff Model is useful in N-gram language modeling to estimate the conditional probability of a word, 
# given its history (actually, its preceding words, normally 2-3-4 words). 
# The problem is that the corpus for training must be large to cover as much the diversity of language as possible. 
# Nevertheless, there are cases where "large" is not "large enough". 
# Katz's approach is to fall back to lower-order N-gram in this case.

# However, one cannot just fall back like this because this naive approach is unfair. 
# Let's say: A-B-C appears 15 times, while A-B-? totally appear 100 times. As a result, Probability(A-B-C|A-B) = 15%. 
# But A-B-N does not appear, so we fall back to B-N and similarly, find that Probability(B-N|B) = 40%. 
# It is unfair because "A-B" gives more context than just "B", but it is NOT chosen!

# Katz fixes this issue by redistributing some probability of high-order N-gram to lower-order N-gram, 
# so that all of the probabilities accumulate to 1. But first we have to reap some probability of the high-order N-gram, 
# making it available to lower-order N-gram. It is done by using Good-Turing Discounting.

# After having some left-over probability for lower-order N-gram, we distribute it fairly. 
# That is, in the lower-order N-gram, 
# the N-grams who have more probability to appear will have more share in this left-over probability.

# The Katz's approach makes sense.


# This model generally works well in practice, but fails in some circumstances. 
# For example, suppose that the bigram "a b" and the unigram "c" are very common, 
# but the trigram "a b c" is never seen. Since "a b" and "c" are very common, 
# it may be significant (that is, not due to chance) that "a b c" is never seen. 
# Perhaps it's not allowed by the rules of the grammar. 
# Instead of assigning a more appropriate value of 0, 
# the method will back off to the bigram and estimate P(c | b), which may be too high.


library(edgeR)
library(plyr)

GoodTuringProportions <- function(frequency){
  goodTuringProportions(frequency)[,1]
}

GoodTuringAlpha <- function(frequency){
  goodTuring(frequency)$P0
}

# Calculate the p_ml for unigrams
# p_ml is the probability calculated using the maximum likelihood 

ngram.1 <- ngram.1 %>% 
  mutate(p_ml = Freq/sum(Freq), P_bo = p_ml)

# For bigrams, trigrams and fourgrams, we need to calculate back-off probabilities by group and α, 
# α is the probability estimated combined proportion of all undetected species by group

ngram.2 <- ngram.2 %>%
  dplyr::group_by(Wordone) %>%
  dplyr::mutate(p_ml = Freq/sum(Freq), P_bo = GoodTuringProportions(Freq), 
                alpha = GoodTuringAlpha(Freq)) %>%
  dplyr::arrange(Wordone, desc(Freq)) %>%
  dplyr::ungroup()

ngram.3 <- ngram.3 %>%
  dplyr::group_by(Wordone, Wordtwo) %>%
  dplyr::mutate(p_ml = Freq/sum(Freq), P_bo = GoodTuringProportions(Freq), 
                alpha = GoodTuringAlpha(Freq)) %>%
  dplyr::arrange(Wordone, Wordtwo, desc(Freq)) %>%
  dplyr::ungroup()

ngram.4 <- ngram.4 %>%
  dplyr::group_by(Wordone, Wordtwo, Wordthree) %>%
  dplyr::mutate(p_ml = Freq/sum(Freq), P_bo = GoodTuringProportions(Freq), 
                alpha = GoodTuringAlpha(Freq)) %>%
  dplyr::arrange(Wordone, Wordtwo, Wordthree, desc(Freq)) %>%
  dplyr::ungroup()


katz.backoff.ngram.1.search <- function(unigram = ngram.1, remove_words = NULL){
  ngram.1.f <- unigram %>% # return words with their P_bo and sum P_bo depending on unigram
    filter(!(Word %in% remove_words)) 
  
  if(!("P_bo_sum" %in% names(unigram)))
    ngram.1.f <- ngram.1.f %>% 
      mutate(P_bo_sum = sum(P_bo))
  
  ngram.1.f
}

katz.backoff.ngram.2.search <- function(word1, bigram = ngram.2,  remove_words = NULL){
  
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



katz.backoff.ngram.3.search <- function(word1, word2, trigram = ngram.3, remove_words = NULL) {
  
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
  

katz.backoff.ngram.4.search <- function(word1, word2, word3, fourgram = ngram.4, remove_words = NULL) {
  
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


Katz_Predictor <- function(text_input) {
  
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

## Examples

Katz_Predictor("you are so")
Katz_Predictor("a lot of")
Katz_Predictor("one of the")
Katz_Predictor("out of the")
Katz_Predictor("as well as")
Katz_Predictor("going to be")
Katz_Predictor("the united states")
Katz_Predictor("thanks for the")
Katz_Predictor("it would be")
Katz_Predictor("some of the")


dir.create("KatzBackOffModel")

save(ngram.1, ngram.2, ngram.3, ngram.4, file = "KatzBackOffModel/data.Rdata")

dump(list = c("Katz_Predictor", "katz.backoff.ngram.1.search", "katz.backoff.ngram.2.search", "katz.backoff.ngram.3.search", "katz.backoff.ngram.4.search"), file = "KatzBackOffModel/Predictor.R")


