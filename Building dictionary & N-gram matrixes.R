rm(list = ls())

library(tm)
library(RWeka)
library(ngram)
library(stringdist)
library(nnet)
library("EnvStats")
library(stylo)

load("speeches.RData")
load("last_name.RData")

## import the text corpus
docs <- VCorpus(DirSource("text"))
#docs <- iconv(docs,"ISO-8859-1","UTF-8")
docs
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs


## unigram data fram
unigram_dtm <- DocumentTermMatrix(docs)
unigram_count <- sort(colSums(as.matrix(unigram_dtm)), decreasing = T)
length(unigram_count)
unigram <- names(unigram_count)
unigram_df <- data.frame(unigram, unigram_count, stringsAsFactors = F, row.names = NULL)


## bigram data frame
# if throw Error: vector memory exhausted (limit reached?), visit here for solution:
# https://stackoverflow.com/questions/51295402/r-on-macos-error-vector-memory-exhausted-limit-reached
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
ctrl <- list(tokenize = BigramTokenizer)
bigram_dtm <- DocumentTermMatrix(docs, control = ctrl)
inspect(bigram_dtm)
#bigram_matrx<-as.matrix(sort(colSums(as.matrix(bigram_dtm)), decreasing = T))

bigram_count <- sort(colSums(as.matrix(bigram_dtm)), decreasing = T)
length(bigram_count)
bigram <- names(bigram_count)
length(bigram)

term_split <- unlist(strsplit(bigram, split = " "))
first_term <- term_split[seq(1,length(term_split), by = 2)]
length(first_term)
second_term<- term_split[seq(2, length(term_split), by = 2)]
length(second_term)
bigram_df <- data.frame(bigram, first_term, second_term, bigram_count, stringsAsFactors = F, row.names = NULL)
sum(bigram_df$bigram_count)

## trigran data frame
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
ctrl <- list(tokenize = TrigramTokenizer, wordLengths=c(1,Inf))
trigram_dtm <- DocumentTermMatrix(docs, control = ctrl)
inspect(trigram_dtm)

trigram_count <- sort(colSums(as.matrix(trigram_dtm)), decreasing = T)
length(trigram_count)
trigram <- names(trigram_count)
length(trigram)
term_split <- unlist(strsplit(trigram, split = " (?=[^ ]+$)", perl=TRUE))
first_term <- term_split[seq(1,length(term_split), by = 2)]
length(first_term)
second_term<- term_split[seq(2, length(term_split), by = 2)]
length(second_term)
trigram_df <- data.frame(trigram, first_term, second_term, trigram_count, stringsAsFactors = F, row.names = NULL)
sum(trigram_df$trigram_count)
middle_term <- unlist(strsplit(trigram_df$first_term, split = " "))[seq(2, length(unlist(strsplit(trigram_df$first_term, split = " "))), by = 2)]
trigram_df$middle_term <- middle_term

## build dictionary 
## build a dictionary from the text corpus
dictionary <- unigram_df$unigram[which(unigram_df$unigram_count>=5)]
## add the speaker's last name to the dictionary
names_not_dic <- setdiff(tolower(last_name), dictionary)
names_not_dic
dictionary <- c(dictionary, names_not_dic)

## append speaker's names into unigram table
names_uni_count<-table(names_not_dic)
names_uni_count
names_unigram <- data.frame(names_uni_count, stringsAsFactors = F, row.names = NULL)
colnames(names_unigram)<- c("unigram", "unigram_count")
unigram_df <- rbind(unigram_df, names_unigram)

for (i in 1:length(data)) {
  txt=data[i]
  txt=gsub("[0-9]","",txt)	
  data[i]=txt
}

sample_speeches_201_250_num_rm <- data[201:250]
save(data, file = "speeches number removed.RData")
