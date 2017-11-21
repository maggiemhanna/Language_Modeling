## Model Evaluation of Katz Backoff model

# When calculating P_bo in katz.backoff.ngram.i.search (i = 1:4), we are actually calculating the probability of all 
# possible combinations of ngrams and then selecting the top 5.
# We can use these functions to find probabilities

# Ex: to find pr(w_3|w_2,w_1), we find using katz.backoff.ngram.3.search all combinations of w_1, w_2 and w (w list of all words)
# and then filter to find pr(w_3|w_2,w_1)

# probability of a sentence s
# in a 4 gram model
# pr(s) = pr(w_1)pr(w_2|w_1)pr(w_3|w_1,w_2)pr(w_4|w_1,w_2,w_3)pr(w_5|w_2,w_3,w_4) ...

## Note: we can't evaluate our simple backoff model since we don't have associated probabilities

# if we have a sentence $s$ that contains nn words, its perplexity $Perplexity(s)$ is:
# $\Perplexity(s) = \sqrt[n]{\frac{1}{p(w_1^n)}}$

# If we want to know the perplexity of the whole corpus $C$ that contains $m$ sentences and $N$ words, 
# we have to find out how well the model can predict all the sentences together. 
# So, let the sentences $(s_1,s_2,...,s_m)$ be part of $C$ The perplexity of the corpus, per word, is given by:

# $\Perplexity(C) = \sqrt[N]{\frac{1}{P(s_1,s_2,...,s_m)}}$

# The probability of all those sentences being together in the corpus $C$ (if we consider them as independent) is:

# $P(s_1,...,s_m) =  \prod_{i=1}^{m} p(s_{i})$ #

# The probability of a sentence appear in a corpus, in a ngram model, is given by 
# $p(s)=\prod_{i=1}^{n}p(w_i|w_{i-n+1}^{i-1})$, where $p(w_i)$ is the probability of the word $w_i$ occurs.

# Since probabilities are given as a real number between 0 and 1, 
# the product $\prod_{i=1}^{m} p(s_{i})$ gets small quickly, and you can have an error in some computer systems
# (underflow). So, we can use the following transformations to replace the multiplications by additions:
#   
# $ \begin{align}
# \Perplexity(C) &= \sqrt[N]{\frac{1}{\prod_{i=1}^{m} p(s_{i})}}  \\
# &= 2^{\log_{2}{[\prod_{i=1}^{m} p(s_{i})]}^{-N}}  \\
# &= 2^{-\frac{1}{N}\log_{2}{[\prod_{i=1}^{m} p(s_{i})]}}  \\ 
# &= 2^{-\frac{1}{N}\sum_{i=1}^{m}\log_{2}{p(s_i)}}
# \end{align}$


perplexity <- function(C) {
  N <- sum(generateNgramData(1, C)$Freq) # total number of words in the corpus
  m <- length(C) # number of sentences in the corpus
  
  # many of the sentences are
  probability_vector <- sapply(C, s_probability, USE.NAMES = FALSE)
  
  # update N to include only sentences that contain non-unique words that didn't occur in train.df (prob = 0)
  N <- sum(probability_vector != 0)
  
  probability_vector <- probability_vector[probability_vector != 0]
  
  perplexity <- 2^-(1/N*sum(log2(probability_vector)))
  perplexity
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

## Example of conditional probabilities

Katz_Predictor("some of the")

katz.backoff.ngram.4.probability("some", "of", "the", "other")
katz.backoff.ngram.4.probability("some", "of", "the", "money")
katz.backoff.ngram.4.probability("some", "of", "the", "things")
katz.backoff.ngram.4.probability("some", "of", "the", "time")
katz.backoff.ngram.4.probability("some", "of", "the", "nations")
