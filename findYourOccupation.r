#install package 


library(tidyverse)
library(tidytext)
library(tm)
library(readr)
library(text2vec)


#input data
resume<- read_file("data/feifei.txt")
occpation<-read.csv(file.path("data/OccupationData.csv"),stringsAsFactors = FALSE)

#create data frame for resume
resumedf <-tibble(Title="Occupation",Description=resume)

#Combine resume and job description to get working dataset
workingSourceData <- bind_rows(resumedf,occpation)

workingdata<- workingSourceData%>%select(1:2)
#Change column name
names(workingdata)[1]="doc_id"
names(workingdata)[2]="text"

#create working data frame 
workingdf<-DataframeSource(workingdata)

#setup corpus
workingdfCorpus<- VCorpus(workingdf)

#Cleaning the data
#remove punctuation
cleaneddfCorpus <-tm_map(workingdfCorpus,removePunctuation)
#change text to lower-case
cleaneddfCorpus <-tm_map(workingdfCorpus,content_transformer(tolower))
#stop word removal
cleaneddfCorpus <-tm_map(workingdfCorpus,removeWords,stopwords("english"))
cleaneddfCorpus <- tm_map(workingdfCorpus,removeWords,c("the","and","The","And",))
#strip white-space
cleaneddfCorpus <-tm_map(workingdfCorpus,stripWhitespace)
#stemming
cleaneddfCorpus<-tm_map(workingdfCorpus, stemDocument, language = "english")  

#TF-IDF Process
#Generate the document term matrix 
workingDtm <- DocumentTermMatrix(cleaneddfCorpus)
inspect(workingDtm)

#use the following code to understand your data


#Docs(workingDtm)#list of docs in the matrix
#nDocs(workingDtm)#No. of docs in the matrix
#Terms(workingDtm)#list of terms in the matrix
#nTerms(workingDtm)#No.of terms in the matrix

#convert to a matrix
WorkingDtmMatwix = as.matrix(workingDtm)

#inspect a specific term
#WorkingDtmMatwix['']
#find terms that have as least 5 times
#findFreqTerms(workingDtm,#)

#generate a frequency table
workingDtmFrequency <- sort(colSums(as.matrix(workingDtm)),decreasing = TRUE)

#concert frequency table to a data frame
workingDtmDf <- data.frame(word=names(workingDtmFrequency),freq=workingDtmFrequency)


#generate the TF-IDF
workingTfidf <- DocumentTermMatrix(cleaneddfCorpus,control = list(weighting = weightTfIdf))
workingTfidfMatrix = as.matrix(workingTfidf )

#cosine similarity matrix
# Calculate cosine similarity with TF-IDF Matrix
cosineSimilarity = function(matrix){
  numerator = matrix %*% t(matrix)
  A = sqrt(apply(matrix^2, 1, sum))
  denumerator = A %*% t(A)
  return(numerator / denumerator)
}

cosineSimilarityM = cosineSimilarity(workingTfidfMatrix)

# Create a new column for similarity_score of data frame
workingdata["similarity_score"] = cosineSimilarityM[1:1037]
# Sort data frame based on similarity_score
occupationVSResume = workingdata[order(-workingdata$similarity_score),]

#write.csv(occupationVSResume,"occupationVSResume.csv")


#Keywords Word Frequency

#input data for keywords term frequency
matchedTop20 <- occupationVSResume %>% head(n=21) 

#for user selected input
getKeywords = function(doc_ids) {
  # todo
}

doc_ids = as.vector(matchedTop20$doc_id)
matchedIdx = which(occpation$Title %in% doc_ids)
#matchedDf = occpation[matchedIdx,]
matchedDf<- occpation[matchedIdx,]%>%select(4:6)

###Most Frequency Words###
#Change column name
names(matchedDf)[1]="doc_id"
names(matchedDf)[2]="text"

keywordsdf<-DataframeSource(matchedDf)
matchedCorpus <- VCorpus(keywordsdf)

#Cleaning the data
#remove punctuation
cleanedmatchedCorpus <-tm_map(matchedCorpus,removePunctuation)
#change text to lower-case
cleanedmatchedCorpus <-tm_map(matchedCorpus,content_transformer(tolower))
#stop word removal
cleanedmatchedCorpus <-tm_map(matchedCorpus,removeWords,stopwords("english"))
#strip white-space
cleanedmatchedCorpus <-tm_map(matchedCorpus,stripWhitespace)
#stemming
cleanedmatchedCorpus<-tm_map(matchedCorpus, stemDocument, language = "english") 

#making a document-term matrix
keywordsDtm <- DocumentTermMatrix(cleanedmatchedCorpus)
keywordsDtmMatrix <- as.matrix(keywordsDtm)

#find the most frequent terms
keywordsFrequency <- colSums(keywordsDtmMatrix)
keywordsFrequency  <- sort(keywordsFrequency,decreasing = TRUE)
keywordsFrequency 

###Hot Technology###
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
keywordsFrequency1 
