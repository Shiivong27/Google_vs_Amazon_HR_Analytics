library(qdap)
library(tm)
library(readxl)
library(RWeka)
library(wordcloud)
library(pyramid)
library(plotrix)

Amazon_pros <- read_excel("Amazon_pros.xlsx")
Amazon_cons <- read_excel("Amazon_cons.xlsx")
Google_pros <- read_excel("Google_pros.xlsx")
Google_cons <- read_excel("Google_cons.xlsx")

Amazon_pros <- as.data.frame(Amazon_pros)
Amazon_cons <- as.data.frame(Amazon_cons)
Google_pros <- as.data.frame(Google_pros)
Google_cons <- as.data.frame(Google_cons)



qdap_clean <- function(x){
              x <- replace_abbreviation(x)
              x <- replace_contraction(x)
              x <- replace_number(x)
              x <- replace_ordinal(x)
              x <- replace_symbol(x)
              x <- tolower(x)
              return(x)}

tm_clean <- function(corpus){
            corpus <- tm_map(corpus, removePunctuation)
             corpus <- tm_map(corpus, stripWhitespace)
              corpus <- tm_map(corpus, removeWords, 
                   c(stopwords("en"), "cats", "fashion"))
  return(corpus)}

qdap_clean(Amazon_pros$Amazon_pros)
qdap_clean(Amazon_cons$Amazon_cons)
qdap_clean(Google_pros$Google_pros)
qdap_clean(Google_cons$Google_cons)

Amazon_pros_corp <- VCorpus(VectorSource(Amazon_pros$Amazon_pros))
Amazon_cons_corp <- VCorpus(VectorSource(Amazon_cons$Amazon_cons))
Google_pros_corp <- VCorpus(VectorSource(Google_pros$Google_pros))
Google_cons_corp <- VCorpus(VectorSource(Google_cons$Google_cons))

Amazon_pros_corpus <- tm_clean(Amazon_pros_corp)
Amazon_cons_corpus <- tm_clean(Amazon_cons_corp)
Google_pros_corpus <- tm_clean(Google_pros_corp)
Google_cons_corpus <- tm_clean(Google_cons_corp)

tokenizer <- function(x){
  NGramTokenizer(x,Weka_control(min = 3,max = 3))
}


#Amazon pros start
Amazon_pros_tdm <- TermDocumentMatrix(
  Amazon_pros_corpus,
  control = list(tokenize = tokenizer)
)

Amazon_pros_tdm_matrix <- as.matrix(Amazon_pros_tdm)
Amazon_pros_freq <- rowSums(Amazon_pros_tdm_matrix)
wordcloud(names(Amazon_pros_freq),Amazon_pros_freq,
          max.words = 25,color = c("blue"))

#Heirarchical clustering of Amazon pros
Amazon_pros_tdm_NoSparse <- removeSparseTerms(
  Amazon_pros_tdm,
  sparse = 0.993
)

Amazon_pros_hc <- hclust(dist(Amazon_pros_tdm_NoSparse,method = "euclidean"),
                         method = "complete")
plot(Amazon_pros_hc)

Amazon_pros_freq_sorted <- sort(Amazon_pros_freq,decreasing = TRUE)
Amazon_pros_freq_sorted[1:5]
findAssocs(Amazon_pros_tdm,terms = "fast paced",corlimit = 0.2)
#Amazon pros end



#Amazon cons start
Amazon_cons_tdm <- TermDocumentMatrix(
  Amazon_cons_corpus,
  control = list(tokenize = tokenizer)
)

Amazon_cons_tdm_matrix <- as.matrix(Amazon_cons_tdm)
Amazon_cons_freq <- rowSums(Amazon_cons_tdm_matrix)
wordcloud(names(Amazon_cons_freq),Amazon_cons_freq,
          max.words = 25,color = c("red"))

#Heirarchical clustering of Amazon cons
Amazon_cons_tdm_NoSparse <- removeSparseTerms(
  Amazon_cons_tdm,
  sparse = 0.993
)

Amazon_cons_hc <- hclust(dist(Amazon_cons_tdm_NoSparse,method = "euclidean"),
                         method = "complete")
plot(Amazon_cons_hc)

Amazon_cons_freq_sorted <- sort(Amazon_cons_freq,decreasing = TRUE)
Amazon_cons_freq_sorted[1:5]
#Amazon cons end



#Google pros start
Google_pros_tdm <- TermDocumentMatrix(
  Google_pros_corpus,
  control = list(tokenize = tokenizer)
)

Google_pros_tdm_matrix <- as.matrix(Google_pros_tdm)
Google_pros_freq <- rowSums(Google_pros_tdm_matrix)
wordcloud(names(Google_pros_freq),Google_pros_freq,
          max.words = 25,color = c("green"))

Google_pros_tdm_NoSparse <- removeSparseTerms(
  Google_pros_tdm,
  sparse = 0.993
)

Google_pros_hc <- hclust(dist(Google_pros_tdm_NoSparse,method = "euclidean"),
                         method = "complete")
plot(Google_pros_hc)

Google_pros_freq_sorted <- sort(Google_pros_freq,decreasing = TRUE)
Google_pros_freq_sorted[1:5]
#Google pros end



#Google cons start
Google_cons_tdm <- TermDocumentMatrix(
  Google_cons_corpus,
  control = list(tokenize = tokenizer)
)

Google_cons_tdm_matrix <- as.matrix(Google_cons_tdm)
Google_cons_freq <- rowSums(Google_cons_tdm_matrix)
wordcloud(names(Google_cons_freq),Google_cons_freq,
          max.words = 25,color = c("purple"))

Google_cons_tdm_NoSparse <- removeSparseTerms(
  Google_cons_tdm,
  sparse = 0.993
)

Google_cons_hc <- hclust(dist(Google_cons_tdm_NoSparse,method = "euclidean"),
                         method = "complete")

plot(Google_cons_hc)

Google_cons_freq_sorted <- sort(Google_cons_freq,decreasing = TRUE)
Google_cons_freq_sorted[1:5]
#Google cons end

All_Amazon <- cbind(Amazon_pros,Amazon_cons)
All_Google <- cbind(Google_pros,Google_cons)

qdap_clean(All_Amazon)
qdap_clean(All_Google)

All_Amazon_corp <- VCorpus(VectorSource(All_Amazon))
All_Google_corp <- VCorpus(VectorSource(All_Google))

All_Amazon_corpus <- tm_clean(All_Amazon_corp)
All_Google_corpus <- tm_clean(All_Google_corp)

#All Amazon commonality cloud

All_Amazon_tdm <- TermDocumentMatrix(
  All_Amazon_corpus,
  control = list(tokenize = tokenizer)
)

colnames(All_Amazon_tdm) <- c("Amazon Pros","Amazon Cons")
All_Amazon_tdm_matrix <- as.matrix(All_Amazon_tdm)
All_Amazon_tdm_freq <- rowSums(All_Amazon_tdm_matrix)
comparison.cloud(All_Amazon_tdm_matrix,
                 colors = c("#F44336", "#2196f3"),max.words = 100)

#All Google commonality cloud

All_Google_tdm <- TermDocumentMatrix(
  All_Google_corpus,
  control = list(tokenize = tokenizer)
)

colnames(All_Google_tdm) <- c("Google Pros","Google Cons")
All_Google_tdm_matrix <- as.matrix(All_Google_tdm)
All_Amazon_tdm_freq <- rowSums(All_Google_tdm_matrix)
comparison.cloud(All_Google_tdm_matrix
                 ,colors = c("#F44336", "#2196f3"),max.words = 100)

#Pyramid plot of Google and Amazon pros
Amazon_pros[501,] <- "NA"
Amazon_Google_pros <- cbind(Amazon_pros,Google_pros)
qdap_clean(Amazon_Google_pros)
Amazon_Google_pros_corp <- VCorpus(VectorSource(Amazon_Google_pros))
Amazon_Google_pros_corpus <- tm_clean(Amazon_Google_pros_corp)

Amazon_Google_pros_tdm <- TermDocumentMatrix(
  Amazon_Google_pros_corpus,
  control = list(tokenize = tokenizer)
)

colnames(Amazon_Google_pros_tdm) <- c("Amazon Pros","Google Pros")
Amazon_Google_pros_tdm_matrix <- as.matrix(Amazon_Google_pros_tdm)
Amazon_Google_pros_tdm_freq <- rowSums(Amazon_Google_pros_tdm_matrix)

common_words_pros <- subset(Amazon_Google_pros_tdm_matrix,
                       Amazon_Google_pros_tdm_matrix[,1] > 0 &
                       Amazon_Google_pros_tdm_matrix[,2])

difference_pros <- abs(common_words_pros[,1] - common_words_pros[,2])

common_words_pros <- cbind(common_words_pros,difference_pros)

common_words_pros <- common_words_pros[order(common_words_pros[,3], decreasing = TRUE),]

top15_df_pros <- data.frame(
                       x = common_words_pros[1:15,1],
                       y = common_words_pros[1:15,2],
                       labels = rownames(common_words_pros[1:15,])
)


pyramid.plot(top15_df_pros$x,top15_df_pros$y,
             labels = top15_df_pros$labels,gap = 12,
             top.labels = c("Amazon","Pros Words","Google"),
             main = "Words in Common",unit = NULL)

#Pyramid plot of Google and Amazon cons
Amazon_cons[501,] <- "NA"
Amazon_Google_cons <- cbind(Amazon_cons,Google_cons)
qdap_clean(Amazon_Google_cons)
Amazon_Google_cons_corp <- VCorpus(VectorSource(Amazon_Google_cons))
Amazon_Google_cons_corpus <- tm_clean(Amazon_Google_cons_corp)

Amazon_Google_cons_tdm <- TermDocumentMatrix(
  Amazon_Google_cons_corpus,
  control = list(tokenize = tokenizer)
)

colnames(Amazon_Google_cons_tdm) <- c("Amazon Cons","Google Cons")
Amazon_Google_cons_tdm_matrix <- as.matrix(Amazon_Google_cons_tdm)
Amazon_Google_cons_tdm_freq <- rowSums(Amazon_Google_cons_tdm_matrix)

common_words_cons <- subset(Amazon_Google_cons_tdm_matrix,
                            Amazon_Google_cons_tdm_matrix[,1] > 0 &
                            Amazon_Google_cons_tdm_matrix[,2])

difference_cons <- abs(common_words_cons[,1] - common_words_cons[,2])

common_words_cons <- cbind(common_words_cons,difference_cons)

common_words_cons <- common_words_cons[order(common_words_cons[,3], decreasing = TRUE),]

top15_df_cons <- data.frame(x = common_words_cons[1:15,1],
                            y = common_words_cons[1:15,2],
                            labels = rownames(common_words_cons[1:15,]))

pyramid.plot(top15_df_cons$x,top15_df_cons$y,
             labels = top15_df_cons$labels,gap = 12,
             top.labels = c("Amazon","Cons Words","Google"),
             main = "Words in Common",unit = NULL)

