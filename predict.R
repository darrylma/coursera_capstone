library(tm)
library(tidyr)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
load("ngrams.Rda")

predict_word <- function(input_string = NULL) {
  
  prediction <- data.frame()
  
  if (length(input_string) > 0 && input_string != "") {
    
    #Clean input string
    profanities = readLines('profanities.txt')
    input <- VCorpus(VectorSource(input_string))
    toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
    
    input <- tm_map(input, content_transformer(tolower))
    input <- tm_map(input, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
    input <- tm_map(input, toSpace, "@[^\\s]+")
    input <- tm_map(input, removeWords, profanities)
    input <- tm_map(input, removeWords, stopwords("en"))
    input <- tm_map(input, removePunctuation)
    input <- tm_map(input, removeNumbers)
    input <- tm_map(input, stripWhitespace)
    input <- tm_map(input, PlainTextDocument)
    
    input_array <- strsplit(x = input[[1]]$content, split = " ")[[1]]
    if(length(input_array) == 0){
      return(NULL)
    }
    if(input_array[1]==""){
      input_array <- input_array[-1]
    }
    
    # Use trigrams to predict next word
    quadgram_predict <- function(x) {
      y <- quadgram_df[(quadgram_df$word1==x[1] & quadgram_df$word2==x[2] & quadgram_df$word3==x[3]),]
      return(y)
    }
    
    # Use trigrams to predict next word
    trigram_predict <- function(x) {
      y <- trigram_df[(trigram_df$word1==x[1] & trigram_df$word2==x[2]),]
      return(y)
    }
    
    # Use bigrams to predict next word
    bigram_predict <- function(x) {
      y <- bigram_df[bigram_df$word1==x[1],]
      return(y)
    }
    
    # Use unigrams to predict next word
    unigram_predict <- function() {
      y <- unigram_df
      return(y)
    }
    
    # If input is 3 or more words
    if(length(input_array) >= 3){
      input_array <- tail(input_array, 3)
      prediction <- quadgram_predict(input_array)
      
      if(is.na(prediction$predicted[1])) {
        input_array <- tail(input_array, 2)
        prediction <- trigram_predict(input_array)
        
        if(is.na(prediction$predicted[1])) {
          input_array <- tail(input_array, 1)
          prediction <- bigram_predict(input_array)
          
          if(is.na(prediction$predicted[1])) {
            prediction <- unigram_predict()
          }
        }
      }
    }
    
    # If input is exactly 2 words
    else if(length(input_array) == 2){
      input_array <- tail(input_array, 2)
      prediction <- trigram_predict(input_array)
      
      if(is.na(prediction$predicted[1])) {
        input_array <- tail(input_array, 1)
        prediction <- bigram_predict(input_array)
        
        if(is.na(prediction$predicted[1])) {
          prediction <- unigram_predict()
        }
      }
    }
    
    # If input is exactly 1 word 
    else if(length(input_array) == 1){
      prediction <- bigram_predict(input_array)
      
      if(is.na(prediction$predicted[1])) {
        prediction <- unigram_predict()
      }
    }
    
    else {
      prediction <- unigram_predict()
    }
    
    prediction$prob <- round(prediction$freq/sum(prediction$freq), digits = 3)
    return(prediction)
  }
  else {
    #print("Requires more words to predict next word")
    return(NULL)
  }
}
