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
library(stringr)
library(prodlim)
library(gpuR)
library(fastR)


#########################################################################
#################             Method 5               ####################
#########################################################################


load("speeches number removed.RData")
dictionary <- c(dictionary, "_")
docs <- VCorpus(VectorSource(data))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)
dtm <- DocumentTermMatrix(docs)
dtm <- DocumentTermMatrix(docs[150:300])
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
ngram_correction <- function(string, original_string, dictionary) {
  
  # input "string" is the results string from string_to_words()
  # input "original _string" is the original string needed to be corrected
  results<-string
  ori_string <- original_string
  
  ## step 2:select the closest results compared to original sentence
  dl_dist <- stringdist(results, ori_string, method = "dl")
  names(dl_dist) <- results
  # this cutt_off of distance could be tuned
  results<- names(which(dl_dist<=4))
  print(ori_string)
  print(results)
  ## step 3: ngram correction
  corrected_results <-c()
  dl_dist <- c()
  num_error <-c()
  error <-c()
  #indicator_vec <- c()
  
  for (x in results){
    # split the speech into terms by space
    speech_split <- unlist(strsplit(x, split = " "))
    # record the error and its length
    err<- concatenate(speech_split[which(speech_split %in% dictionary =="FALSE")])
    error <-c(error, err)
    # second row categorical values (-2, -1, 0, +1)
    indicator <- character(length = length(speech_split))
    # index of real word in speech
    correct_index <-which(speech_split %in% dictionary =="TRUE")
    indicator[correct_index] <- 0
    # index of non-word error in speech
    error_index <- which(speech_split %in% dictionary =="FALSE")
    # error word length of 1 or 2
    indicator[which(nchar(speech_split)<=2)] <- -2
    # errors with word length more than 2
    error_index_3_more <- error_index[which(nchar(speech_split[error_index])>2)]
    
    corrected_index <- c()
    if (length(error_index_3_more)>=1){
      for (y in error_index_3_more){
        print(speech_split[y])
        # calculate the edit distance and get the index words in dictionary
        edit_distance <- stringdist(speech_split[y], dictionary, method = "dl")
        if (nchar(speech_split[y])>2 & nchar(speech_split[y])<=5){
          # set dl distance curoff
          if(grepl("_", speech_split[y])==TRUE){
            cap <- 2
          }else{cap <- 1}
          for (q in 1:cap) {
            index_dis_cutoff <- which(edit_distance<=q)
            if (length(index_dis_cutoff)>0){break}
          }
        }else if (nchar(speech_split[y])>5 & nchar(speech_split[y])<10){
          # set dl distance curoff
          if(grepl("_", speech_split[y])==TRUE){
            cap <- 3
          }else{cap <- 2}
          for (q in 1:cap) {
            index_dis_cutoff <- which(edit_distance<=q)
            if (length(index_dis_cutoff)>0){break}
          }
        }else { # set dl distance curoff
          if(grepl("_", speech_split[y])==TRUE){
            cap <- 4
          }else{cap <- 3}
          for (q in 1:cap) {
            index_dis_cutoff <- which(edit_distance<=q)
            if (length(index_dis_cutoff)>0){break}
          }
        }
        if (length(index_dis_cutoff)>0) {
          if (length(index_dis_cutoff)==1){
            # have only one candidate word within 2 edit distance in dictionary
            speech_split[y]<- dictionary[index_dis_cutoff]
            corrected_index <- c(corrected_index, y)
            indicator[y] <- +1
            #print(dictionary[index_dis_cutoff])
          }else {
            # have more than one candidates within 2 edit distance 
            # match the candidates with trigram\bigram and get frequency counts
            if (((y-1) %in% correct_index | (y-1) %in% corrected_index) & ((y+1) %in% correct_index)) {
              # + y + : one preceding word and one successor word bigram check
              trigram_cand <- paste(speech_split[y-1], dictionary[index_dis_cutoff],speech_split[y+1])
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
                indicator[y] <- +1
              }else{
                # trigram not found, check bigram
                bigram_cand_bef <- paste(speech_split[y-1], dictionary[index_dis_cutoff], sep = " ")
                bigram_cand_bef_index <- which(!is.na(match(bigram_df$bigram, bigram_cand_bef)))
                bigram_cand_aft <- paste(dictionary[index_dis_cutoff], speech_split[y+1], sep = " ")
                bigram_cand_aft_index <- which(!is.na(match(bigram_df$bigram, bigram_cand_aft)))
                if (length(bigram_cand_bef_index)==0 & length(bigram_cand_aft_index)==0){
                  # bigram not found, check unigram and count
                  unigram_cand_index <- which(!is.na(match(unigram_df$unigram, dictionary[index_dis_cutoff])))
                  unigram_cand_count <- unigram_df$unigram_count[unigram_cand_index]
                  names(unigram_cand_count) <- unigram_df$unigram[unigram_cand_index]
                  correction <- names(sort(unigram_cand_count, decreasing = T)[1])
                  #print(paste(correction, "+un+"))
                  speech_split[y] <- correction
                  corrected_index <- c(corrected_index, y)
                  indicator[y] <- +1
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
                  indicator[y] <- +1
                }
              }
            }else if (((y-1) %in% correct_index | (y-1) %in% corrected_index) & !((y+1) %in% correct_index)){
              # + y -
              bigram_cand_bef <- paste(speech_split[y-1], dictionary[index_dis_cutoff], sep = " ")
              bigram_cand_bef_index <- which(!is.na(match(bigram_df$bigram, bigram_cand_bef)))
              if (length(bigram_cand_bef_index)>0){
                # bigram found
                bigram_cand_bef_count <- bigram_df$bigram_count[bigram_cand_bef_index]
                names(bigram_cand_bef_count) <- bigram_df$second_term[bigram_cand_bef_index]
                correction <- names(sort(bigram_cand_bef_count, decreasing = T)[1])
                #print(paste(correction, "+bi-"))
                speech_split[y] <- correction
                corrected_index <- c(corrected_index, y)
                indicator[y] <- +1
              }else{
                # bigram not found, check unigram and count
                unigram_cand_index <- which(!is.na(match(unigram_df$unigram, dictionary[index_dis_cutoff])))
                unigram_cand_count <- unigram_df$unigram_count[unigram_cand_index]
                names(unigram_cand_count) <- unigram_df$unigram[unigram_cand_index]
                correction <- names(sort(unigram_cand_count, decreasing = T)[1])
                #print(paste(correction, "+un-"))
                speech_split[y] <- correction
                corrected_index <- c(corrected_index, y)
                indicator[y] <- +1
              }
            }else if (!((y-1) %in% correct_index | (y-1) %in% corrected_index) & ((y+1) %in% correct_index)) {
              # - y +
              bigram_cand_aft <- paste(dictionary[index_dis_cutoff], speech_split[y+1], sep = " ")
              bigram_cand_aft_index <- which(!is.na(match(bigram_df$bigram, bigram_cand_aft)))
              if (length(bigram_cand_aft_index)>0){
                # bigram found
                bigram_cand_aft_count <- bigram_df$bigram_count[bigram_cand_aft_index]
                names(bigram_cand_aft_count) <- bigram_df$first_term[bigram_cand_aft_index]
                correction <- names(sort(bigram_cand_aft_count, decreasing = T)[1])
                #print(paste(correction, "-bi+"))
                speech_split[y] <- correction
                corrected_index <- c(corrected_index, y) 
                indicator[y] <- +1
              }else{
                # bigram not found, check unigram and count
                unigram_cand_index <- which(!is.na(match(unigram_df$unigram, dictionary[index_dis_cutoff])))
                unigram_cand_count <- unigram_df$unigram_count[unigram_cand_index]
                names(unigram_cand_count) <- unigram_df$unigram[unigram_cand_index]
                correction <- names(sort(unigram_cand_count, decreasing = T)[1])
                #print(paste(correction, "-un+"))
                speech_split[y] <- correction
                corrected_index <- c(corrected_index, y)
                indicator[y] <- +1
              }
            }else {
              # bigram not found, check unigram and count
              unigram_cand_index <- which(!is.na(match(unigram_df$unigram, dictionary[index_dis_cutoff])))
              unigram_cand_count <- unigram_df$unigram_count[unigram_cand_index]
              names(unigram_cand_count) <- unigram_df$unigram[unigram_cand_index]
              correction <- names(sort(unigram_cand_count, decreasing = T)[1])
              #print(paste(correction, "-un-"))
              speech_split[y] <- correction
              corrected_index <- c(corrected_index, y)
              indicator[y] <- +1
            }
          }
        }else{indicator[y] <- -1}
      }
    }
    
    # convert "_" back to blank
    speech_split <- str_replace_all(speech_split, pattern = "_", replacement = " ")
    #print(paste(speech_split, "error"))
    # re-calcualte the correct words in the speech after correction, ignore those less than 2 characters
    #num_err <- length(which(!speech_split[which(nchar(speech_split)>2)] %in% dictionary))
    # remove the extra white space
    con_not_ws <- str_squish(concatenate(speech_split))
    corrected_results <- c(corrected_results,con_not_ws)
    print(con_not_ws)
    # re-calcualte the correct words in the speech after correction
    con_not_ws_split <- unlist(strsplit(con_not_ws, split = " "))
    num_err <- length(which(!con_not_ws_split %in% dictionary))
    num_error <- c(num_error, num_err)
    dl <- stringdist(con_not_ws, ori_string, method = "dl")
    dl_dist <- c(dl_dist, dl)
    #indicator_vec <- c(indicator_vec, concatenate(indicator))
    
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
    print(termlist[term_index])
    if (length(term_index) == 0) {
      max_p <- 0
      prob <- c(prob, max_p)
    }else {
      topic1<- sum(exp(ldaModel@beta)[1,term_index])
      print(exp(ldaModel@beta)[1,term_index])
      topic2<- sum(exp(ldaModel@beta)[2,term_index])
      print(exp(ldaModel@beta)[2,term_index])
      topic3<- sum(exp(ldaModel@beta)[3,term_index])
      print(exp(ldaModel@beta)[3,term_index])
      topic4<- sum(exp(ldaModel@beta)[4,term_index])
      print(exp(ldaModel@beta)[4,term_index])
      topic5<- sum(exp(ldaModel@beta)[5,term_index])
      print(exp(ldaModel@beta)[5,term_index])
      max_p <- max(c(topic1, topic2, topic3, topic4, topic5))
      prob <- c(prob, max_p)
    }
  }
  #final_correction <- candidate_results[which.is.max(prob)]
  return(prob)
  print(prob)
}

######################## do not concatenate neighbors (only concat two consecutive errors) ################################
corrected_speeches <- c()
error_after_cor<- c()
error <-c()
err_len <- 0
err_len_after_cor <-0
indicator_speeches <- c()
num_speech <- 0
speech_list_method_5 <- list()

for (d in sample_speeches_201_250_num_rm) {
  # count the speeches
  num_speech <- num_speech +1
  #dtm <- DocumentTermMatrix(docs[200:300])
  sp_split <- unlist(strsplit(d, split = " "))
  
  # index of non-word error in speech
  error_index <- which(sp_split %in% dictionary =="FALSE")
  # errors with word length less than 2
  error_index_3_less <- error_index[which(nchar(sp_split[error_index])<=2)]
  short_words <- sp_split[error_index_3_less]
  dictionary_with_short <- c(dictionary, short_words)
  
  #print(sp_split)
  # record the error and its length
  err_len <- err_len+length(sp_split[which(sp_split %in% dictionary_with_short =="FALSE")])
  err<- concatenate(sp_split[which(sp_split %in% dictionary_with_short =="FALSE")])
  error <-c(error, err)
  # vector to collect the words that have been checked
  checked <- sp_split[1]
  index <- 0
  #indicator_speech <- c()
  for (s in 2:(length(sp_split)+1)) {
    #print(s)
    #print(sp_split[s])
    last_check <- checked[length(checked)-1]
    print(last_check)
    checked <- c(checked, sp_split[s])
    #print(checked)
    if (s == index) {
      next
    }else {
      if (!sp_split[s-1] %in% dictionary_with_short) {
        index <- s+1
        #print(ori_string)
        if (sp_split[s] %in% dictionary_with_short){
          ori_string<- paste(tail(checked[which(!is.na(checked))], 3), collapse = " ")
          conca_string <- sp_split[s-1]
          ### perform correction
          ## step 1: string to words
          results<-c()
          string_to_words(string = conca_string, dictionary = dictionary)
          results <- unique(results)
          print(results)
          if(s<=2){
            results_neighbor <- paste(results, sp_split[s], sep = " ")
          }else{
            results_neighbor <- paste(last_check, results, sp_split[s], sep = " ")
          }
        }else{
          if(s == (length(sp_split)+1)){
            ori_string<- paste(tail(checked[which(!is.na(checked))], 2), collapse = " ")
            conca_string <- paste(tail(checked[which(!is.na(checked))], 1), collapse = "")
            ### perform correction
            ## step 1: string to words
            results<-c()
            string_to_words(string = conca_string, dictionary = dictionary)
            results <- unique(results)
            print(results)
            results_neighbor <- paste(last_check, results, sep = " ")
          }else{
            ori_string<- paste(tail(checked[which(!is.na(checked))], 3), collapse = " ")
            conca_string <- paste(tail(checked[which(!is.na(checked))], 2), collapse = "_")
            ### perform correction
            ## step 1: string to words
            results<-c()
            string_to_words(string = conca_string, dictionary = dictionary)
            results <- unique(results)
            print(results)
            results_neighbor <- paste(last_check, results, sep = " ")
            results_neighbor <- trimws(results_neighbor)
          }
        }
        #print(conca_string)
        checked <- checked[-c(length(checked),length(checked)-1, length(checked)-2)]
        #print(checked)
        ## step 2 and 3
        print(results_neighbor)
        dist_frq_df<- ngram_correction(string = results_neighbor, original_string = ori_string, dictionary = dictionary)
        #print(dist_frq_df)
        ## step 4: select most possible correction by topic modelling
        ## the near n speeches
        corrected_results<- dist_frq_df$corrected_results
        prob<-TopicModelling(candidate_results = corrected_results, DocumentTermMatrix = dtm)
        print(prob)
        dist_frq_df$prob <- prob
        dist_frq_df$prob <- as.numeric(dist_frq_df$prob)
        dist_frq_df<- dist_frq_df[with(dist_frq_df, order(num_error, dl_dist, -prob)), ]
        # randomly select the correction with same factors
        num_error_top <- dist_frq_df$num_error[1]
        dl_dist_top <- dist_frq_df$dl_dist[1]
        prob_top <- dist_frq_df$prob[1]
        same_factors <- subset(dist_frq_df, num_error ==num_error_top & dl_dist == dl_dist_top & prob == prob_top, select = corrected_results)
        final_correction <-sample(same_factors$corrected_results, 1)
        print(final_correction)
        bingo <- unlist(strsplit(final_correction, split = " "))
        #count <- count+1
        #print(count)
        checked <- c(checked, bingo)
        #print(checked)
        #indicator_bingo <- unlist(strsplit(dist_frq_df$indicator_vec[1], split = " "))
        #indicator_speech <- c(indicator_speech, indicator_bingo)
      }else{
        #indicator_speech <- c(indicator_speech, 0)
      }
    }
  }
  corrected_speech <- paste(checked[which(!is.na(checked))], collapse = " ")
  corrected_speeches <- c(corrected_speeches, corrected_speech)
  
}


corrected_speeches
error
err_len
error_after_cor
err_len_after_cor

length(unlist(strsplit(data, split = " ")))
length(unlist(strsplit(corrected_speeches, split = " ")))
length(indicator_speeches)
which(nchar(indicator_speeches)==0)

length(speech_list_method_5)

corrected_speeches

## unit test
a <- "cont estedelection"
# replace blanks
a <- str_replace_all(a, pattern = " ", "_")
ori_string <- "cont estedelection"
results<-c()
string_to_words(string = a, dictionary = dic)
results <- unique(results)
print(results)
results_neighbor <- paste(results, sep = " ")
dist_frq_df<- ngram_correction(string = results_neighbor, original_string = ori_string, dictionary = dictionary)
corrected_results<- dist_frq_df$corrected_results
print(corrected_results)
prob<-TopicModelling(candidate_results = corrected_results, DocumentTermMatrix = dtm)
print(prob)
dist_frq_df$prob <- prob
dist_frq_df$prob <- as.numeric(dist_frq_df$prob)
dist_frq_df<- dist_frq_df[with(dist_frq_df, order(num_error, dl_dist, -prob)), ]
# randomly select the correction with same factors
num_error_top <- dist_frq_df$num_error[1]
dl_dist_top <- dist_frq_df$dl_dist[1]
prob_top <- dist_frq_df$prob[1]
same_factors <- subset(dist_frq_df, num_error ==num_error_top & dl_dist == dl_dist_top & prob == prob_top, select = corrected_results)
final_correction <-sample(same_factors$corrected_results, 1)
final_correction <- str_squish(final_correction)
print(final_correction)