
library(tm)
library(RWeka)
library(ngram)
library(stringdist)
library(nnet)
library("EnvStats")
library(stylo)
library(topicmodels) 
library(NLP)
library(SnowballC)
library(tm)
library(gpuR)
library(fastR)


load("speech_237_240.RData")
load("dictionary.RData")
load("speeches.RData")
docs <- VCorpus(VectorSource(data))
docs <- tm_map(docs, stripWhitespace)
dtm <- DocumentTermMatrix(docs)
dtm <- DocumentTermMatrix(docs[200:300])
inspect(dtm)

###############################-Functions-#########################################
## string to words function (DFS)

string_to_words <- function(string, dictionary, subst=""){
  if (nchar(string)==0){
    result <- substr(subst, 2, nchar(subst))
    print(result)
    #print(paste("result", result))
    result <- concatenate(unlist(strsplit(result, split = " "))[which(nchar(unlist(strsplit(result, split = " ")))>0)])
    results<-c(results, result)
    r<- results
    assign("results", r, envir = .GlobalEnv)
    #return(result)
  }else {
    for (i in 1: nchar(string)) {
      #print(string)
      #print(paste("i",i))
      for (y in i:nchar(string)) {
        #print(paste("y",y))
        if (substr(string, i, y) %in% dictionary){
          #print(paste(subst, substr(string, 1, i-1), substr(string, i, y)))
          string_to_words(substr(string, y+1, nchar(string)), dictionary, paste(subst, substr(string, 1, i-1), substr(string, i, y)))
        }
      }
      
    }
    while (i == nchar(string)) {
      #print(paste("while",subst, substr(string, 1, i)))
      string_to_words(substr(string, y+1, nchar(string)), dictionary, paste(subst, substr(string, 1, i)))
      break
    }
  }
}

## ngram correction function without MLE
ngram_correction <- function(string, original_string) {
  
  # input "string" is the results string from string_to_words()
  # input "original _string" is the original string needed to be corrected
  results<-string
  ori_string <- original_string
  
  ## step 2:select the closest results compared to original sentence
  dl_dist <- stringdist(results, ori_string, method = "dl")
  names(dl_dist) <- results
  # this cutt_off of distance could be tuned
  results<- names(which(dl_dist<=5))
  print(ori_string)
  print(results)
  ## step 3: ngram correction
  corrected_results <-c()
  dl_dist <- c()
  #gm_frq<-c()
  num_error <-c()
  #num_cuts <- c()
  for (x in results){
    # split the speech into terms by space
    speech_split <- unlist(strsplit(x, split = " "))
    # index of real word in speech
    correct_index <-which(speech_split %in% dictionary =="TRUE")
    # index of non-word error in speech
    error_index <- which(speech_split %in% dictionary =="FALSE")
    corrected_index <- c()
    if (length(error_index)>=1){
      for (y in error_index){
        # calculate the edit distance and get the index words in dictionary that have 1 or 2 edit distance
        edit_distance <- stringdist(speech_split[y], dictionary, method = "dl")
        index_within_2 <- which(edit_distance<=2)
        if (length(index_within_2)>0) {
          if (length(index_within_2)==1){
            # have only one candidate word within 2 edit distance in dictionary
            speech_split[y]<- dictionary[index_within_2]
            corrected_index <- c(corrected_index, y)
            #print(dictionary[index_within_2])
          }else {
            # have more than one candidates within 2 edit distance 
            # match the candidates with trigram\bigram and get frequency counts
            if (((y-1) %in% correct_index | (y-1) %in% corrected_index) & ((y+1) %in% correct_index)) {
              # + y + : one preceding word and one successor word bigram check
              trigram_cand <- paste(speech_split[y-1], dictionary[index_within_2],speech_split[y+1])
              trigram_found <- which(trigram_cand %in% trigram_df$trigram)
              if (length(trigram_found>0)){
                # trigram found
                trigram_cand_index <- which(!is.na(match(trigram_df$trigram, trigram_cand[trigram_found])))
                trigram_cand_count <- trigram_df$trigram_count[trigram_cand_index]
                names(trigram_cand_count) <- trigram_df$middle_term[trigram_cand_index]
                correction <- names(sort(trigram_cand_count, decreasing = T)[1])
                #print(paste(correction, "+tri+"))
                speech_split[y] <- correction
                corrected_index <- c(corrected_index, y)
              }else{
                # trigram not found, check bigram
                bigram_cand_bef <- paste(speech_split[y-1], dictionary[index_within_2], sep = " ")
                bigram_cand_bef_index <- which(!is.na(match(bigram_df$bigram, bigram_cand_bef)))
                bigram_cand_aft <- paste(dictionary[index_within_2], speech_split[y+1], sep = " ")
                bigram_cand_aft_index <- which(!is.na(match(bigram_df$bigram, bigram_cand_aft)))
                if (length(bigram_cand_bef_index)==0 & length(bigram_cand_aft_index)==0){
                  # bigram not found, check unigram and count
                  unigram_cand_index <- which(!is.na(match(unigram_df$unigram, dictionary[index_within_2])))
                  unigram_cand_count <- unigram_df$unigram_count[unigram_cand_index]
                  names(unigram_cand_count) <- unigram_df$unigram[unigram_cand_index]
                  correction <- names(sort(unigram_cand_count, decreasing = T)[1])
                  #print(paste(correction, "+un+"))
                  speech_split[y] <- correction
                  corrected_index <- c(corrected_index, y)
                }else{
                  # bigram found
                  bigram_cand_bef_count <- bigram_df$bigram_count[bigram_cand_bef_index]
                  bigram_cand_aft_count <- bigram_df$bigram_count[bigram_cand_aft_index]
                  names(bigram_cand_bef_count) <- bigram_df$second_term[bigram_cand_bef_index]
                  names(bigram_cand_aft_count) <- bigram_df$first_term[bigram_cand_aft_index]
                  correction <- names(sort(c(bigram_cand_bef_count, bigram_cand_aft_count), decreasing = T)[1])
                  #print(paste(correction, "+bi+"))
                  speech_split[y] <- correction
                  corrected_index <- c(corrected_index, y)
                }
              }
            }else if (((y-1) %in% correct_index | (y-1) %in% corrected_index) & !((y+1) %in% correct_index)){
              # + y -
              bigram_cand_bef <- paste(speech_split[y-1], dictionary[index_within_2], sep = " ")
              bigram_cand_bef_index <- which(!is.na(match(bigram_df$bigram, bigram_cand_bef)))
              if (length(bigram_cand_bef_index)>0){
                # bigram found
                bigram_cand_bef_count <- bigram_df$bigram_count[bigram_cand_bef_index]
                names(bigram_cand_bef_count) <- bigram_df$second_term[bigram_cand_bef_index]
                correction <- names(sort(bigram_cand_bef_count, decreasing = T)[1])
                #print(paste(correction, "+bi-"))
                speech_split[y] <- correction
                corrected_index <- c(corrected_index, y)
              }else{
                # bigram not found, check unigram and count
                unigram_cand_index <- which(!is.na(match(unigram_df$unigram, dictionary[index_within_2])))
                unigram_cand_count <- unigram_df$unigram_count[unigram_cand_index]
                names(unigram_cand_count) <- unigram_df$unigram[unigram_cand_index]
                correction <- names(sort(unigram_cand_count, decreasing = T)[1])
                #print(paste(correction, "+un-"))
                speech_split[y] <- correction
                corrected_index <- c(corrected_index, y)
              }
            }else if (!((y-1) %in% correct_index | (y-1) %in% corrected_index) & ((y+1) %in% correct_index)) {
              # - y +
              bigram_cand_aft <- paste(dictionary[index_within_2], speech_split[y+1], sep = " ")
              bigram_cand_aft_index <- which(!is.na(match(bigram_df$bigram, bigram_cand_aft)))
              if (length(bigram_cand_aft_index)>0){
                # bigram found
                bigram_cand_aft_count <- bigram_df$bigram_count[bigram_cand_aft_index]
                names(bigram_cand_aft_count) <- bigram_df$first_term[bigram_cand_aft_index]
                correction <- names(sort(bigram_cand_aft_count, decreasing = T)[1])
                #print(paste(correction, "-bi+"))
                speech_split[y] <- correction
                corrected_index <- c(corrected_index, y) 
              }else{
                # bigram not found, check unigram and count
                unigram_cand_index <- which(!is.na(match(unigram_df$unigram, dictionary[index_within_2])))
                unigram_cand_count <- unigram_df$unigram_count[unigram_cand_index]
                names(unigram_cand_count) <- unigram_df$unigram[unigram_cand_index]
                correction <- names(sort(unigram_cand_count, decreasing = T)[1])
                #print(paste(correction, "-un+"))
                speech_split[y] <- correction
                corrected_index <- c(corrected_index, y)
              }
            }else {
              # bigram not found, check unigram and count
              unigram_cand_index <- which(!is.na(match(unigram_df$unigram, dictionary[index_within_2])))
              unigram_cand_count <- unigram_df$unigram_count[unigram_cand_index]
              names(unigram_cand_count) <- unigram_df$unigram[unigram_cand_index]
              correction <- names(sort(unigram_cand_count, decreasing = T)[1])
              #print(paste(correction, "-un-"))
              speech_split[y] <- correction
              corrected_index <- c(corrected_index, y)
            }
          }
        }
      }
    }
    # re-calcualte the correct words in the speech after correction
    num_err <- length(which(!speech_split %in% dictionary))
    num_error <- c(num_error, num_err)
    corrected_results <- c(corrected_results,concatenate(speech_split))
    print(concatenate(speech_split))
    dl <- stringdist(concatenate(speech_split), ori_string, method = "dl")
    dl_dist <- c(dl_dist, dl)
  }
  dist_frq_df <- data.frame(corrected_results, num_error, dl_dist, stringsAsFactors = F, row.names = NULL)
  return(dist_frq_df)
  #print(dist_frq_df)
}

## topic modeling function
TopicModelling <- function(candidate_results, DocumentTermMatrix) {
  # the input "candidate_results" takes the output of corrected results from ngram_correction().
  # the input "DocumentTermMatrix" takes the dtm that is built by N near speeches. 
  # step 4: select most possible correction from the corrected results
  dtm <- DocumentTermMatrix
  rowTotals <- apply(dtm , 1, sum)
  dtm.new   <- dtm[rowTotals> 0, ] 
  ldaModel <- LDA(dtm.new, k = 5)
  termlist <- colnames(dtm)
  prob<- c()
  for (i in candidate_results) {
    result_split <- unlist(strsplit(i, split = " "))
    term_index <- which(termlist %in% result_split)
    if (length(term_index) == 0) {
      max_p <- 0
      prob <- c(prob, max_p)
    }else {
      topic1<- sum(exp(ldaModel@beta)[1,term_index])/length(result_split)
      topic2<- sum(exp(ldaModel@beta)[2,term_index])/length(result_split)
      topic3<- sum(exp(ldaModel@beta)[3,term_index])/length(result_split)
      topic4<- sum(exp(ldaModel@beta)[4,term_index])/length(result_split)
      topic5<- sum(exp(ldaModel@beta)[5,term_index])/length(result_split)
      max_p <- max(c(topic1, topic2, topic3, topic4, topic5))
      prob <- c(prob, max_p)
    }
  }
  #final_correction <- candidate_results[which.is.max(prob)]
  return(prob)
  print(prob)
}

########################## concatenate neighbors ##############################################

## program applied to speeches
corrected_speeches <- c()
for (d in speech[3]) {
  #dtm <- DocumentTermMatrix(docs[200:300])
  sp_split <- unlist(strsplit(d, split = " "))
  #print(sp_split)
  # vector to collect the words that have been checked
  checked <- sp_split[1]
  index <- 0
  for (s in 2:(length(sp_split)+1)) {
    #print(s)
    #print(sp_split[s])
    checked <- c(checked, sp_split[s])
    #print(checked)
    if (s == index) {
      next
    }else {
      if (!sp_split[s-1] %in% dictionary) {
        index <- s+1
        ori_string<- paste(tail(checked[which(!is.na(checked))], 3), collapse = " ")
        #print(ori_string)
        conca_string <- paste(tail(checked[which(!is.na(checked))], 3), collapse = "")
        #print(conca_string)
        checked <- checked[-c(length(checked),length(checked)-1, length(checked)-2)]
        #print(checked)
        ### perform correction
        ## step 1: string to words
        results<-c()
        string_to_words(string = conca_string, dictionary = dictionary)
        results <- unique(results)
        print(results)
        ## step 2 and 3
        dist_frq_df<- ngram_correction(string = results, original_string = ori_string)
        #print(dist_frq_df)
        ## step 4: select most possible correction by topic modelling
        ## the near n speeches
        corrected_results<- dist_frq_df$corrected_results
        prob<-TopicModelling(candidate_results = corrected_results, DocumentTermMatrix = dtm)
        print(prob)
        dist_frq_df$prob <- prob
        dist_frq_df$prob <- as.numeric(dist_frq_df$prob)
        dist_frq_df<- dist_frq_df[with(dist_frq_df, order(num_error, dl_dist, -prob)), ]
        final_correction <-dist_frq_df$corrected_results[1]
        print(final_correction)
        bingo <- unlist(strsplit(final_correction, split = " "))
        #count <- count+1
        #print(count)
        checked <- c(checked, bingo)
        #print(checked)
      }
    }
  }
  corrected_speech <- paste(checked[which(!is.na(checked))], collapse = " ")
  corrected_speeches <- c(corrected_speeches, corrected_speech)
}

speech[4]
corrected_speeches
results

#speech1_3 <- c()
speech1_3 <- c(speech1_3, corrected_speeches)
speech1_3
gc()

#############################################################################################
## test with individual string
conca_string <- "ofnominationsalready"
ori_string <- "of nominations already"
results<-c()
string_to_words(string = conca_string, dictionary = dictionary)
results <- unique(results)
results
dist_frq_df<- ngram_correction(string = results, original_string = ori_string)
corrected_results<- dist_frq_df$corrected_results
prob<-TopicModelling(candidate_results = corrected_results, DocumentTermMatrix = dtm)
print(prob)
dist_frq_df$prob <- prob
dist_frq_df$prob <- as.numeric(dist_frq_df$prob)
dist_frq_df<- dist_frq_df[with(dist_frq_df, order(num_error, dl_dist, -prob)), ]


######################## do not concatenate neighbors ################################
corrected_speeches <- c()
error_after_cor<- c()
error <-c()
err_len <- 0
err_len_after_cor <-0
for (s in sample_speeches_201_250) {
  #dtm <- DocumentTermMatrix(docs[200:300])
  sp_split <- unlist(strsplit(s, split = " "))
  #print(sp_split)
  # record the error and its length
  err_len <- err_len+length(sp_split[which(sp_split %in% dictionary =="FALSE")])
  err<- concatenate(sp_split[which(sp_split %in% dictionary =="FALSE")])
  error <-c(error, err)
  correct_index <-which(sp_split %in% dictionary =="TRUE")
  # index of non-word error in speech
  #error_index <- which(sp_split %in% dictionary =="FALSE")
  mod_correct_index <- c(0, correct_index)
  words_vec <- c()
  for (d in 2:length(mod_correct_index)){
    if ((mod_correct_index[d]-mod_correct_index[d-1])>1){
      conca_error <- paste(sp_split[(mod_correct_index[d-1]+1):(mod_correct_index[d]-1)], collapse = "")
      if (nchar(conca_error)<30){
        ### perform correction
        ## step 1: string to words
        results<-c()
        string_to_words(string = conca_error, dictionary = dictionary)
        results <- unique(results)
        print(results)
        ## step 2 and 3
        if (mod_correct_index[d-1]==0){
          results_neighbor<- paste(results, sp_split[mod_correct_index[d]], sep = " ")
          ori_string <- paste(sp_split[mod_correct_index[d-1]:mod_correct_index[d]], collapse = " ")
          dist_frq_df<- ngram_correction(string = results_neighbor, original_string = ori_string)
          #print(dist_frq_df)
          ## step 4: select most possible correction by topic modelling
          ## the near n speeches
          corrected_results<- dist_frq_df$corrected_results
          prob<-TopicModelling(candidate_results = corrected_results, DocumentTermMatrix = dtm)
          print(prob)
          dist_frq_df$prob <- prob
          dist_frq_df$prob <- as.numeric(dist_frq_df$prob)
          dist_frq_df<- dist_frq_df[with(dist_frq_df, order(num_error, dl_dist, -prob)), ]
          final_correction <-dist_frq_df$corrected_results[1]
          print(final_correction)
          bingo <- unlist(strsplit(final_correction, split = " "))
          #bingo <- bingo[1:(length(bingo)-1)]
          words_vec <- c(words_vec, bingo)
        }else{
          results_neighbor<- paste(sp_split[mod_correct_index[d-1]], results, sp_split[mod_correct_index[d]], sep = " ")
          ori_string <- paste(sp_split[mod_correct_index[d-1]:mod_correct_index[d]], collapse = " ")
          dist_frq_df<- ngram_correction(string = results_neighbor, original_string = ori_string)
          #print(dist_frq_df)
          ## step 4: select most possible correction by topic modelling
          ## the near n speeches
          corrected_results<- dist_frq_df$corrected_results
          prob<-TopicModelling(candidate_results = corrected_results, DocumentTermMatrix = dtm)
          print(prob)
          dist_frq_df$prob <- prob
          dist_frq_df$prob <- as.numeric(dist_frq_df$prob)
          dist_frq_df<- dist_frq_df[with(dist_frq_df, order(num_error, dl_dist, -prob)), ]
          final_correction <-dist_frq_df$corrected_results[1]
          print(final_correction)
          bingo <- unlist(strsplit(final_correction, split = " "))
          bingo <- bingo[2:length(bingo)]
          words_vec <- c(words_vec, bingo)
        }
      }else {
        words_vec <- c(words_vec, paste(sp_split[(mod_correct_index[d-1]+1):mod_correct_index[d]], collapse = " "))
      }
    }else{
      words_vec <- c(words_vec, sp_split[mod_correct_index[d]])
    }
    while (d == length(mod_correct_index)) {
      if (mod_correct_index[d]!= length(sp_split)){
        ## errors occur at the end of sentence
        conca_error <- paste(sp_split[(mod_correct_index[d]+1):length(sp_split)], collapse = "")
        if (nchar(conca_error)<30){
          ### perform correction
          ## step 1: string to words
          results<-c()
          string_to_words(string = conca_error, dictionary = dictionary)
          results <- unique(results)
          print(results)
          ## step 2 and 3
          results_neighbor<- paste(sp_split[mod_correct_index[d]], results, sep = " ")
          ori_string <- paste(sp_split[mod_correct_index[d]:length(sp_split)], collapse = " ")
          dist_frq_df<- ngram_correction(string = results_neighbor, original_string = ori_string)
          #print(dist_frq_df)
          ## step 4: select most possible correction by topic modelling
          ## the near n speeches
          corrected_results<- dist_frq_df$corrected_results
          prob<-TopicModelling(candidate_results = corrected_results, DocumentTermMatrix = dtm)
          print(prob)
          dist_frq_df$prob <- prob
          dist_frq_df$prob <- as.numeric(dist_frq_df$prob)
          dist_frq_df<- dist_frq_df[with(dist_frq_df, order(num_error, dl_dist, -prob)), ]
          final_correction <-dist_frq_df$corrected_results[1]
          print(final_correction)
          bingo <- unlist(strsplit(final_correction, split = " "))
          bingo <- bingo[2:length(bingo)]
          words_vec <- c(words_vec, bingo)
        }else {
          words_vec <- c(words_vec, paste(sp_split[(mod_correct_index[d-1]+1):length(sp_split)], collapse = " "))
        }
      }
      break
    }
  }
  # re-calcualte the non-word error in the speech after correction
  err_len_after_cor <- err_len_after_cor+length(words_vec[which(words_vec %in% dictionary =="FALSE")])
  err_after_cor<- concatenate(words_vec[which(words_vec %in% dictionary =="FALSE")])
  error_after_cor<- c(error_after_cor, err_after_cor)
  corrected_speech <- paste(words_vec, collapse = " ")
  corrected_speeches <- c(corrected_speeches, corrected_speech)
}

corrected_speeches
error
err_len
error_after_cor
err_len_after_cor

