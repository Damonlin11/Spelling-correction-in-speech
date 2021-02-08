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
library(gpuR)
library(fastR)




#########################################################################
#################             Method 1               ####################
#########################################################################

data <- sample_speeches_201_250_num_rm

## correct the non-word error by unigram
corrected_speech <-c()
indicator_vec <- c()
speech_list_method_1 <- list()
num_speech <- 0
error <-c()
error_3 <-c()

for (i in data){
  # count the speeches
  num_speech <- num_speech +1
  # split the speech into terms by space
  speech_split <- unlist(strsplit(i, split = " "))
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
  # errors with word length more that 2
  error_index_3_more <- error_index[which(nchar(speech_split[error_index])>2)]
  
  # record the error and its length
  err_3<- concatenate(speech_split[error_index_3_more])
  error_3 <-c(error_3, err_3)
  
  if (length(error_index_3_more)>=1){
    for (y in error_index_3_more){
      print(speech_split[y])
      # calculate the edit distance and get the index words in dictionary
      edit_distance <- stringdist(speech_split[y], dictionary, method = "dl")
      if (nchar(speech_split[y])>2 & nchar(speech_split[y])<=5){
        # set dl distance curoff
        cap <- 1
        for (q in 1:cap) {
          index_dis_cutoff <- which(edit_distance<=q)
          if (length(index_dis_cutoff)>0){break}
        }
      }else if (nchar(speech_split[y])>5 & nchar(speech_split[y])<10){
        # set dl distance curoff
        cap <- 2
        for (q in 1:cap) {
          index_dis_cutoff <- which(edit_distance<=q)
          if (length(index_dis_cutoff)>0){break}
        }
      }else { # set dl distance curoff
        cap <- 3
        for (q in 1:cap) {
          index_dis_cutoff <- which(edit_distance<=q)
          if (length(index_dis_cutoff)>0){break}
        }
      }
      if (length(index_dis_cutoff>0)){
        unigram_cand_index <- which(!is.na(match(unigram_df$unigram, dictionary[index_dis_cutoff])))
        unigram_cand_count <- unigram_df$unigram_count[unigram_cand_index]
        names(unigram_cand_count) <- unigram_df$unigram[unigram_cand_index]
        correction <- names(sort(unigram_cand_count, decreasing = T)[1])
        print(paste(correction, "un"))
        speech_split[y] <- correction
        indicator[y] <- +1
      }else{
        indicator[y] <- -1
      }
    }
  }
  # collect speech after correction
  corrected_speech <- c(corrected_speech,concatenate(speech_split))
  indicator_vec <- c(indicator_vec, indicator)
  
  ori_speech <- unlist(strsplit(i, split = " "))
  print(length(ori_speech))
  first_row <- speech_split
  print(length(first_row))
  second_row <- indicator
  print(length(second_row))
  matrix <- rbind(ori_speech, first_row, second_row)
  print(matrix)
  speech_list_method_1[[num_speech]] <- matrix
  
}
corrected_speech
indicator_vec
error
error_3

length(unlist(strsplit(data, split = " ")))
length(unlist(strsplit(corrected_speech, split = " ")))
length(indicator_vec)

which(nchar(indicator_vec)==0)
length(which(indicator_vec==0))
length(which(indicator_vec==1))
length(which(indicator_vec==-1))
length(which(indicator_vec==-2))

length(speech_list_method_1)

corrected_speech

#########################################################################
#################             Method 2               ####################
#########################################################################

## consider one preceding word (bigram) to make correction
corrected_speech <-c()
indicator_vec <- c()
speech_list_method_2 <- list()
num_speech <- 0
error <-c()

for (i in data){
  # count the speeches
  num_speech <- num_speech +1
  # split the speech into terms by space
  speech_split <- unlist(strsplit(i, split = " "))
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
        cap <- 1
        for (q in 1:cap) {
          index_dis_cutoff <- which(edit_distance<=q)
          if (length(index_dis_cutoff)>0){break}
        }
      }else if (nchar(speech_split[y])>5 & nchar(speech_split[y])<10){
        # set dl distance curoff
        cap <- 2
        for (q in 1:cap) {
          index_dis_cutoff <- which(edit_distance<=q)
          if (length(index_dis_cutoff)>0){break}
        }
      }else { # set dl distance curoff
        cap <- 3
        for (q in 1:cap) {
          index_dis_cutoff <- which(edit_distance<=q)
          if (length(index_dis_cutoff)>0){break}
        }
      }
      if (length(index_dis_cutoff)>0) {
        print(length(index_dis_cutoff))
        if (length(index_dis_cutoff)==1){
          # have only one candidate word within 2 edit distance in dictionary
          speech_split[y]<- dictionary[index_dis_cutoff]
          print(paste(dictionary[index_dis_cutoff], "+1"))
          corrected_index <- c(corrected_index, y)
          indicator[y] <- +1
          print(dictionary[index_dis_cutoff])
        }else {
          # have more than one candidates within 2 edit distance 
          # match the candidates with bigram and get frequency counts
          if ((y-1) %in% correct_index | (y-1) %in% corrected_index) {
            bigram_cand <- paste(speech_split[y-1], dictionary[index_dis_cutoff], sep = " ")
            bigram_cand_index <- which(!is.na(match(bigram_df$bigram, bigram_cand)))
            if (length(bigram_cand_index)>0){
              # bigram found
              bigram_cand_count <- bigram_df$bigram_count[bigram_cand_index]
              names(bigram_cand_count) <- bigram_df$second_term[bigram_cand_index]
              correction <- names(sort(bigram_cand_count, decreasing = T)[1])
              print(paste(correction, "+bi"))
              speech_split[y] <- correction
              corrected_index <- c(corrected_index, y)
              indicator[y] <- +1
            }else{
              # bigram not found, check unigram and count
              unigram_cand_index <- which(!is.na(match(unigram_df$unigram, dictionary[index_dis_cutoff])))
              unigram_cand_count <- unigram_df$unigram_count[unigram_cand_index]
              names(unigram_cand_count) <- unigram_df$unigram[unigram_cand_index]
              correction <- names(sort(unigram_cand_count, decreasing = T)[1])
              print(paste(correction, "+un"))
              speech_split[y] <- correction
              corrected_index <- c(corrected_index, y)
              indicator[y] <- +1
            }
          }else{
            # bigram not found, check unigram and count
            unigram_cand_index <- which(!is.na(match(unigram_df$unigram, dictionary[index_dis_cutoff])))
            unigram_cand_count <- unigram_df$unigram_count[unigram_cand_index]
            names(unigram_cand_count) <- unigram_df$unigram[unigram_cand_index]
            correction <- names(sort(unigram_cand_count, decreasing = T)[1])
            print(paste(correction, "+un"))
            speech_split[y] <- correction
            corrected_index <- c(corrected_index, y)
            indicator[y] <- +1
          }
        }
      }else{indicator[y] <- -1}
    }
  }
  # collect speech after correction
  corrected_speech <- c(corrected_speech,concatenate(speech_split))
  indicator_vec <- c(indicator_vec, indicator)
  
  ori_speech <- unlist(strsplit(i, split = " "))
  print(length(ori_speech))
  
  first_row <- speech_split
  print(length(first_row))
  
  second_row <- indicator
  print(length(second_row))
  
  matrix <- rbind(ori_speech, first_row, second_row)
  print(matrix)
  speech_list_method_2[[num_speech]] <- matrix
}

corrected_speech
indicator_vec
error

length(unlist(strsplit(data, split = " ")))
length(unlist(strsplit(corrected_speech, split = " ")))
length(indicator_vec)
which(nchar(indicator_vec)==0)

length(which(indicator_vec==0))
length(which(indicator_vec==1))
length(which(indicator_vec==-1))
length(which(indicator_vec==-2))

length(speech_list_method_2)
corrected_speech

#########################################################################
#################             Method 3               ####################
#########################################################################

## consider two preceding word (trigram) to make correction
corrected_speech <-c()
indicator_vec <- c()
speech_list_method_3 <- list()
num_speech <- 0
error <-c()

for (i in data){
  # count the speeches
  num_speech <- num_speech +1
  # split the speech into terms by space
  speech_split <- unlist(strsplit(i, split = " "))
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
        cap <- 1
        for (q in 1:cap) {
          index_dis_cutoff <- which(edit_distance<=q)
          if (length(index_dis_cutoff)>0){break}
        }
      }else if (nchar(speech_split[y])>5 & nchar(speech_split[y])<10){
        # set dl distance curoff
        cap <- 2
        for (q in 1:cap) {
          index_dis_cutoff <- which(edit_distance<=q)
          if (length(index_dis_cutoff)>0){break}
        }
      }else { # set dl distance curoff
        cap <- 3
        for (q in 1:cap) {
          index_dis_cutoff <- which(edit_distance<=q)
          if (length(index_dis_cutoff)>0){break}
        }
      }
      if (length(index_dis_cutoff)>0) {
        if (length(index_dis_cutoff)==1){
          # have only one candidate word within 2 edit distance in dictionary
          speech_split[y]<- dictionary[index_dis_cutoff]
          print(paste(dictionary[index_dis_cutoff], "+1"))
          corrected_index <- c(corrected_index, y)
          indicator[y] <- +1
          print(dictionary[index_dis_cutoff])
        }else {
          # have more than one candidates within 2 edit distance 
          # match the candidates with trigram\bigram and get frequency counts
          if ((y-1) %in% correct_index | (y-1) %in% corrected_index) {
            # trigram check
            if ((y-2) %in% correct_index | (y-2) %in% corrected_index){
              # combine the candidate with preceding two words
              trigram_cand <- paste(speech_split[y-2], speech_split[y-1], dictionary[index_dis_cutoff],sep = " ")
              # get the position and counts of matched trigrams in trigram_df
              trigram_cand_index <- which(!is.na(match(trigram_df$trigram,trigram_cand)))
              if (length(trigram_cand_index) >0){
                # trigrams found
                trigram_cand_count <- trigram_df$trigram_count[trigram_cand_index]
                names(trigram_cand_count) <- trigram_df$second_term[trigram_cand_index]
                correction <- names(sort(trigram_cand_count, decreasing = T)[1])
                print(paste(correction, "++tr"))
                speech_split[y]<- correction
                corrected_index <- c(corrected_index, y)
                indicator[y] <- +1
              }else{
                # no trigram found, then check bigram and counts
                bigram_cand <- paste(speech_split[y-1], dictionary[index_dis_cutoff], sep = " ")
                bigram_cand_index <- which(!is.na(match(bigram_df$bigram, bigram_cand)))
                if (length(bigram_cand_index)>0){
                  # bigram found
                  bigram_cand_count <- bigram_df$bigram_count[bigram_cand_index]
                  names(bigram_cand_count) <- bigram_df$second_term[bigram_cand_index]
                  correction <- names(sort(bigram_cand_count, decreasing = T)[1])
                  print(paste(correction, "++bi"))
                  speech_split[y] <- correction
                  corrected_index <- c(corrected_index, y)
                  indicator[y] <- +1
                }else{
                  # bigram not found, check unigram and count
                  unigram_cand_index <- which(!is.na(match(unigram_df$unigram, dictionary[index_dis_cutoff])))
                  unigram_cand_count <- unigram_df$unigram_count[unigram_cand_index]
                  names(unigram_cand_count) <- unigram_df$unigram[unigram_cand_index]
                  correction <- names(sort(unigram_cand_count, decreasing = T)[1])
                  print(paste(correction, "++un"))
                  speech_split[y] <- correction
                  corrected_index <- c(corrected_index, y)
                  indicator[y] <- +1
                }
              }
            }else{
              # y-2 is not correct, check bigram
              bigram_cand <- paste(speech_split[y-1], dictionary[index_dis_cutoff], sep = " ")
              bigram_cand_index <- which(!is.na(match(bigram_df$bigram, bigram_cand)))
              if (length(bigram_cand_index)>0){
                # bigram found
                bigram_cand_count <- bigram_df$bigram_count[bigram_cand_index]
                names(bigram_cand_count) <- bigram_df$second_term[bigram_cand_index]
                correction <- names(sort(bigram_cand_count, decreasing = T)[1])
                print(paste(correction, "+bi"))
                speech_split[y] <- correction
                corrected_index <- c(corrected_index, y)
                indicator[y] <- +1
              }else{
                # bigram not found, check unigram and count
                unigram_cand_index <- which(!is.na(match(unigram_df$unigram, dictionary[index_dis_cutoff])))
                unigram_cand_count <- unigram_df$unigram_count[unigram_cand_index]
                names(unigram_cand_count) <- unigram_df$unigram[unigram_cand_index]
                correction <- names(sort(unigram_cand_count, decreasing = T)[1])
                print(paste(correction, "+un"))
                speech_split[y] <- correction
                corrected_index <- c(corrected_index, y)
                indicator[y] <- +1
              }
            }
          }else {
            # bigram not found, check unigram and count
            unigram_cand_index <- which(!is.na(match(unigram_df$unigram, dictionary[index_dis_cutoff])))
            unigram_cand_count <- unigram_df$unigram_count[unigram_cand_index]
            names(unigram_cand_count) <- unigram_df$unigram[unigram_cand_index]
            correction <- names(sort(unigram_cand_count, decreasing = T)[1])
            print(paste(correction, "un"))
            speech_split[y] <- correction
            corrected_index <- c(corrected_index, y)
            indicator[y] <- +1
          }
        }
      }else{indicator[y] <- -1}
    }
  }
  # collect speech after correction
  corrected_speech <- c(corrected_speech,concatenate(speech_split))
  indicator_vec <- c(indicator_vec, indicator)
  
  ori_speech <- unlist(strsplit(i, split = " "))
  print(length(ori_speech))
  
  first_row <- speech_split
  print(length(first_row))
  
  second_row <- indicator
  print(length(second_row))
  
  matrix <- rbind(ori_speech, first_row, second_row)
  print(matrix)
  speech_list_method_3[[num_speech]] <- matrix
}

corrected_speech
indicator_vec
error
length(which(indicator_vec== -2))

length(unlist(strsplit(data, split = " ")))
length(unlist(strsplit(corrected_speech, split = " ")))
length(indicator_vec)
which(nchar(indicator_vec)==0)

length(which(indicator_vec==0))
length(which(indicator_vec==1))
length(which(indicator_vec==-1))
length(which(indicator_vec==-2))

length(speech_list_method_3)

corrected_speech

#########################################################################
#################             Method 4               ####################
#########################################################################

## consider one preceding word and one successor word to make correction
corrected_speech <-c()
indicator_vec <- c()
speech_list_method_4 <- list()
num_speech <- 0
error <-c()

for (i in data){
  # count the speeches
  num_speech <- num_speech +1
  # split the speech into terms by space
  speech_split <- unlist(strsplit(i, split = " "))
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
        cap <- 1
        for (q in 1:cap) {
          index_dis_cutoff <- which(edit_distance<=q)
          if (length(index_dis_cutoff)>0){break}
        }
      }else if (nchar(speech_split[y])>5 & nchar(speech_split[y])<10){
        # set dl distance curoff
        cap <- 2
        for (q in 1:cap) {
          index_dis_cutoff <- which(edit_distance<=q)
          if (length(index_dis_cutoff)>0){break}
        }
      }else { # set dl distance curoff
        cap <- 3
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
          print(paste(dictionary[index_dis_cutoff], "+1"))
          indicator[y] <- +1
          print(dictionary[index_dis_cutoff])
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
              print(paste(correction, "+tri+"))
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
                print(paste(correction, "+un+"))
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
                print(paste(correction, "+bi+"))
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
              print(paste(correction, "+bi-"))
              speech_split[y] <- correction
              corrected_index <- c(corrected_index, y)
              indicator[y] <- +1
            }else{
              # bigram not found, check unigram and count
              unigram_cand_index <- which(!is.na(match(unigram_df$unigram, dictionary[index_dis_cutoff])))
              unigram_cand_count <- unigram_df$unigram_count[unigram_cand_index]
              names(unigram_cand_count) <- unigram_df$unigram[unigram_cand_index]
              correction <- names(sort(unigram_cand_count, decreasing = T)[1])
              print(paste(correction, "+un-"))
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
              print(paste(correction, "-bi+"))
              speech_split[y] <- correction
              corrected_index <- c(corrected_index, y) 
              indicator[y] <- +1
            }else{
              # bigram not found, check unigram and count
              unigram_cand_index <- which(!is.na(match(unigram_df$unigram, dictionary[index_dis_cutoff])))
              unigram_cand_count <- unigram_df$unigram_count[unigram_cand_index]
              names(unigram_cand_count) <- unigram_df$unigram[unigram_cand_index]
              correction <- names(sort(unigram_cand_count, decreasing = T)[1])
              print(paste(correction, "-un+"))
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
            print(paste(correction, "-un-"))
            speech_split[y] <- correction
            corrected_index <- c(corrected_index, y)
            indicator[y] <- +1
          }
        }
      }else{indicator[y] <- -1}
    }
  }
  
  corrected_speech <- c(corrected_speech,concatenate(speech_split))
  indicator_vec <- c(indicator_vec, indicator)
  
  ori_speech <- unlist(strsplit(i, split = " "))
  print(length(ori_speech))
  
  first_row <- speech_split
  print(length(first_row))
  
  second_row <- indicator
  print(length(second_row))
  
  matrix <- rbind(ori_speech, first_row, second_row)
  print(matrix)
  speech_list_method_4[[num_speech]] <- matrix
}
corrected_speech
indicator_vec
error

length(unlist(strsplit(data, split = " ")))
length(unlist(strsplit(corrected_speech, split = " ")))
length(indicator_vec)
which(nchar(indicator_vec)==0)

length(which(indicator_vec==0))
length(which(indicator_vec==1))
length(which(indicator_vec==-1))
length(which(indicator_vec==-2))

length(speech_list_method_4)

corrected_speech
