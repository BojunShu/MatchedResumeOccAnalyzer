


install.packages(memoise)
library(shiny)
library(tidyverse)
library(tidytext)
library(tm)
library(readr)
library(text2vec)
library(wordcloud)
library(memoise)

rep_names <- function(X) {
  rep(names(X), X)
  
}

name_vec <- function(X) {
  paste(names(X), X, sep = '-')
  
}

cosineSimilarity = function(matrix) {
  numerator = matrix %*% t(matrix)
  A = sqrt(apply(matrix ^ 2, 1, sum))
  denumerator = A %*% t(A)
  return(numerator / denumerator)
}


function(input, output, session) {
  #occupation data linked
  occupation <- read.csv("data/OccupationData.csv")
  ####output first Tab####
  #resume uploaded by user
  uploadResume <- reactive({
    if (is.null(input$resume)) {
      return()
    }
    else
      resumeUpload <- input$resume
    resumedf <- read_file(resumeUpload$datapath)
    resumedf <- tibble(Title = "Occupation", Description = resumedf)
    workingSourceData <- bind_rows(resumedf, occupation)
    
    workingdata <- workingSourceData %>% select(1:2)
    #Change column name
    names(workingdata)[1] = "doc_id"
    names(workingdata)[2] = "text"
    
    #create working data frame
    workingdf <- DataframeSource(workingdata)
    
    #setup corpus
    workingdfCorpus <- VCorpus(workingdf)
    
    #Cleaning the data
    #remove punctuation
    cleaneddfCorpus <- tm_map(workingdfCorpus, removePunctuation)
    #change text to lower-case
    cleaneddfCorpus <-
      tm_map(workingdfCorpus, content_transformer(tolower))
    #stop word removal
    cleaneddfCorpus <-
      tm_map(workingdfCorpus, removeWords, stopwords("english"))
    cleaneddfCorpus <-
      tm_map(workingdfCorpus, removeWords, c("the", "and", "The", "And"))
    #strip white-space
    cleaneddfCorpus <- tm_map(workingdfCorpus, stripWhitespace)
    #stemming
    cleaneddfCorpus <-
      tm_map(workingdfCorpus, stemDocument, language = "english")
    
    #TF-IDF Process
    #Generate the document term matrix
    workingDtm <- DocumentTermMatrix(cleaneddfCorpus)
    inspect(workingDtm)
    
    #convert to a matrix
    WorkingDtmMatwix = as.matrix(workingDtm)
    
    #generate a frequency table
    workingDtmFrequency <-
      sort(colSums(as.matrix(workingDtm)), decreasing = TRUE)
    
    #concert frequency table to a data frame
    workingDtmDf <-
      data.frame(word = names(workingDtmFrequency), freq = workingDtmFrequency)
    
    #generate the TF-IDF
    workingTfidf <-
      DocumentTermMatrix(cleaneddfCorpus, control = list(weighting = weightTfIdf))
    workingTfidfMatrix = as.matrix(workingTfidf)
    
    #cosine similarity matrix
    # Calculate cosine similarity with TF-IDF Matrix
    
    cosineSimilarityM = cosineSimilarity(workingTfidfMatrix)
    
    # Create a new column for similarity_score of data frame
    workingdata["similarity_score"] = cosineSimilarityM[1:1037]
    # Sort data frame based on similarity_score
    occupationVSResume = workingdata[order(-workingdata$similarity_score), ]
    occupationVSResume = occupationVSResume[2:21, ]
    
    return(occupationVSResume)
    
  })
  
  output$upload <- renderDataTable({
    uploadResume()
  })
  
  
  words <- reactive({
    if (is.null(input$analyzer)) {
      return()
    }
    else
      doc_ids = as.vector(uploadResume()$doc_id)
    matchedIdx = which(occupation$Title %in% doc_ids)
    #matchedDf = occpation[matchedIdx,]
    matchedDf <- occupation[matchedIdx, ] %>% select(4:6)
    
    #Change column name
    names(matchedDf)[1] = "doc_id"
    names(matchedDf)[2] = "text"
    
    keywordsdf <- DataframeSource(matchedDf)
    matchedCorpus <- VCorpus(keywordsdf)
    
    #Cleaning the data
    #remove punctuation
    cleanedmatchedCorpus <- tm_map(matchedCorpus, removePunctuation)
    #change text to lower-case
    cleanedmatchedCorpus <-
      tm_map(matchedCorpus, content_transformer(tolower))
    #stop word removal
    cleanedmatchedCorpus <-
      tm_map(matchedCorpus, removeWords, stopwords("english"))
    cleanedmatchedCorpus <-
      tm_map(matchedCorpus, removeWords, c("the", "and", "The", "And","such"))
    #strip white-space
    cleanedmatchedCorpus <- tm_map(matchedCorpus, stripWhitespace)
    #stemming
    cleanedmatchedCorpus <-
      tm_map(matchedCorpus, stemDocument, language = "english")
    
    #making a document-term matrix
    keywordsDtm <- DocumentTermMatrix(cleanedmatchedCorpus)
    keywordsDtmMatrix <- as.matrix(keywordsDtm)
    
    #find the most frequent terms
    keywordsFrequency <- colSums(keywordsDtmMatrix)
    keywordsFrequency  <- sort(keywordsFrequency, decreasing = TRUE)
    
    
    keywordsFrequency <- data.frame(Word = names(keywordsFrequency),
                                    Frequency = keywordsFrequency) %>%
      head(100)
    
    return(keywordsFrequency)
    
    
  })
  
  
  output$word <- renderDataTable({
    words()
  })
  
  
  tech <- reactive({
    if (is.null(input$tech)) {
      return()
    }
    else
      doc_ids = as.vector(uploadResume()$doc_id)
    matchedIdx1 = which(occupation$Title %in% doc_ids)
    #matchedDf = occpation[matchedIdx,]
    matchedDf1 <- occupation[matchedIdx1, ] %>% select(7:8)
    
    #Change column name
    names(matchedDf1)[1] = "doc_id"
    names(matchedDf1)[2] = "text"
    
    keywordsdf1 <- DataframeSource(matchedDf1)
    matchedCorpus1 <- VCorpus(keywordsdf1)
    
    #making a document-term matrix
    keywordsDtm1 <- DocumentTermMatrix(matchedCorpus1)
    keywordsDtmMatrix1 <- as.matrix(keywordsDtm1)
    
    #find the most frequent terms
    keywordsFrequency1 <- colSums(keywordsDtmMatrix1)
    keywordsFrequency1  <-
      sort(keywordsFrequency1, decreasing = TRUE)
    
    return(keywordsFrequency1)
    
    
  })
  
  output$cloud <- renderPlot({
    V <- tech()
    wordcloud(names(V),
                  V,
                  scale = c(4, 0, 5),
                  )
  })
  
  
  
  
}
