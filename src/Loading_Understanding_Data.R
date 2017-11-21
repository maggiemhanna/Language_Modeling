setwd("/Users/az01661/Dropbox/3 Data Science Specialization/NLP Predicting Next Word in Text")

con <- file("final/en_US/en_US.blogs.txt", "r")
blogs <- readLines(con)
close(con)

con <- file("final/en_US/en_US.news.txt", "r")
news <- readLines(con)
close(con)

con <- file("final/en_US/en_US.twitter.txt", "r")
twitter <- readLines(con)
close(con)


## memory that is being used to store objects:
  
  object.size(blogs)
  object.size(news)
  object.size(twitter)
  
## Number of lines in every object
  
  length(blogs)
  length(news)
  length(twitter)
  
## Longest line in every object
  
  max(nchar(blogs))
  max(nchar(news))
  max(nchar(twitter))
  
## Ratio of the number of lines where the word "love" (all lowercase) occurs by the number of lines the word "hate" (all lowercase) occurs in the twitter dataset  
  
  sum(grepl("love", twitter))/sum(grepl("hate", twitter))

## The one tweet in the en_US twitter data set that matches the word "biostats"   

  twitter[grep("biostats", twitter)]
  
## The number of tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing

  sum(twitter == "A computer once beat me at chess, but it was no match for me at kickboxing")
  
## Cleaning Data and creating a complete data frame
    
  # Check data to see if there are missing values.
  length(which(!complete.cases(blogs)))
  length(which(!complete.cases(news)))
  length(which(!complete.cases(twitter)))
  
  #let's get a feel for the distribution of text lengths of the texts 

  blogs_TextLength <- nchar(blogs)
  news_TextLength <- nchar(news)
  twitter_TextLength <- nchar(twitter)
  
  summary(blogs_TextLength)
  summary(news_TextLength)
  summary(twitter_TextLength)
  
  # Creating a data.frame of complete database
  
  df <- data.frame(Text = c(blogs, news, twitter), 
                    TextLength = c(blogs_TextLength, news_TextLength, twitter_TextLength), 
                    Source = c(rep('blogs', length(blogs)), rep('news', length(news)), rep('twitter', length(twitter))))
  
  
  
  dim(df)
  
  # The datatable is huge. We will only consider a sample of 500,000 lines
  
  library(dplyr)  
  #set.seed(12345)
  df <- sample_n(df, size = 500000)
  rownames(df) <- 1:500000
  
  df$Text <- as.character(df$Text)
  
  # Visualize distribution with ggplot2
  library(ggplot2)
  
  ggplot(df, aes(x = TextLength, fill = Source)) +
    theme_bw() +
    geom_histogram(binwidth = 5) +
    labs(y = "Text Count", x = "Length of Text",
         title = "Distribution of Text Lengths with different sources") + 
    xlim(c(0,2000))
  
rm(list=ls())
  
  
  
  
  
  

