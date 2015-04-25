###########################################################################################

#Text mining example: topics classfication using knn based on Reuters-21578 corpus
#Author: Ming Tian (NYU-Poly FE)
#Copyright(c) 2015 Ming Tian All Right Reserved
#Last Updated date: April,9 2015
#Acknowledgement: Timothy D'Auria (Boston Decision)
#Some functions are modified by Ming Tian based on Timothy D'Auria's work
#The source codes of Timothy D'Auria are available at: 
#http://bostondecision.com/2012/05/14/the-text-classifier-determine-the-author-of-a-document-with-machine-learning-in-r/

###########################################################################################

#please make sure all of the packages are installed successfuly before load them

#load the required packages
library(tm)
library(XML)
library(kernlab)
library(plyr)
library(class)
library(SnowballC)
#this package need to be installed from local path
library("tm.corpus.Reuters21578")

#check the data of Reuters-21578 corpus firstly
data(Reuters21578)
length(Reuters21578)

#define a variable to contain the topics we need
topics <- c("earn", "acq", "money-fx", "grain", "crude", "trade", 
            "interest", "ship", "wheat", "corn")
x=Reuters21578

#select topics
s_topics <- topics
n <- length(s_topics)
result_tmp <- list()
for(i in 1:n){
  keep_tmp <- grep(s_topics[i], meta(x, tag="topics_cat"))
  result_tmp[[i]] <- x[keep_tmp]
}

#several functions from Author: Timothy D'Auria
# Function to convert pretty apostrophe
convertPrettyApostrophe <- function(x) gsub("â", "'", x)

# Function to clean Corpus text
cleanCorpus <- function(corpus) {
  
  # Apply Text Mining Clean-up Functions
  corpus.tmp <- tm_map(corpus, convertPrettyApostrophe)
  corpus.tmp <- tm_map(corpus.tmp, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, tolower)
  corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords("english"))
  corpus.tmp <- tm_map(corpus.tmp, PlainTextDocument)
  #corpus.tmp <- tm_map(corpus.tmp, stemDocument, language = "english")
  
  return(corpus.tmp)
}

# Function to generate term document matrices (Modified by Ming Tian)
generateTDM <- function(x, i) {
  # Instantiate Corpus
  s.cor <- x
  # Clean corpus
  s.cor.cl <- cleanCorpus(s.cor)
  # Create term document matrix
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  # Remove sparse terms
  s.tdm <- removeSparseTerms(s.tdm, 0.9)
  # Construct return object
  result <- list(topic = topics[i], tdm = s.tdm)
  return(result)
}

# Bind Candidate Name to Term Document Matrices
bindTopicToTDM <- function(tdm) {
  s.mat <- t(data.matrix(tdm[["tdm"]]))
  s.df <- as.data.frame(s.mat, stringsAsfactors = FALSE)
  s.df <- cbind(s.df, rep(tdm[["topic"]], nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "topics"
  return(s.df)
}

#apply the functions above
comb <- list()
for(i in 1:n){
  tdm_tmp <- generateTDM(result_tmp[[i]], i)
  comb[[i]] <- bindTopicToTDM(tdm_tmp)
}

# Rbind list 
tdm_comb <- do.call(rbind.fill, comb)
tdm_comb[is.na(tdm_comb)] <- 0

#return the result
tdm_comb$topics <- as.character(tdm_comb$topics)
tdm_comb$topics[tdm_comb$topics != "earn"] <- "No"
tdm_comb$topics <- as.factor(tdm_comb$topics)
result <- tdm_comb

#build a function to implement the Knn model
KNN <- function(result){
#read the data
tdm.stack <- result
# Random sample 70% for training of data mining model; remainder for test
train.idx <- sample(nrow(tdm.stack), ceiling(nrow(tdm.stack) * .70))
test.idx <- (1:nrow(tdm.stack))[- train.idx]
# Extract candidate name
tdm.cand <- tdm.stack[, "topics"]
tdm.stack.nl <- tdm.stack[,!colnames(tdm.stack) %in% "topics"]
# K-nearest Neighbor
knn.pred <- knn(tdm.stack.nl[train.idx, ], tdm.stack.nl[test.idx, ], tdm.cand[train.idx])
knn.train.data <- tdm.stack[train.idx, ]
# Confusion Matrix
conf.mat <- table("Predictions" = knn.pred, Actual = tdm.cand[test.idx])
# Accuracy
accuracy <- sum(diag(conf.mat))/length(test.idx) * 100
return(conf.mat)
}

#calculate the accuracy and recall
test <- KNN(result)
precision <- test[1,1] / (test[1,1] + test[1,2])
recall <- test[1,1] / (test[1,1] + test[2,1])
print(paste("precision: ", precision, "and recall: ", recall, sep=""))

# #run the KNN model 20 times to see the hist of accuracy
# acc_vec <- c(1:20)
# for(i in 1:20){
#   acc_tmp <- KNN(result)
#   acc_vec[i] <- acc_tmp
#   print(i)
# }
# 
# #check the hist
# hist(acc_vec)
