rm(list = ls())
rm(last_name)
library(stringdist)
#Load the master list from outside source
#load("C:\\Johannes Ledolter\\2019SageBook\\BookChapterMay2019\\NewOrderMay21\\FromLin\\last_name.RData")
load("last_name.RData")
load("meta2.RData")
last_name <- tolower(last_name)
last_name
meta <- meta2
last <- last_name
last <- tolower(last)

### name correction function: correct to the nearest name
name_correction <- function(uncleaned, name_dictionary, method = "lcs"){
  ## uncleaned: an input character vector that contains misspelled names and will be detected and corrected
  
  ## name_dictionary: an input character vector that contains correct names and is treated as comparable dictionary
  
  ## method: the method for distance calculation used in stringsim(). 
    # method can be chose from c("osa", "lv", "dl", "hamming", "lcs", "qgram","cosine", "jaccard", "jw", "soundex").
    # the default is "lcs".
  
  ## the function returns a data frame that contains input uncleaned names (uncleaned), nearest names, and corresponding similarity scores.
  
  # detect misspell names
  index_misspell <- which(is.na(match(uncleaned, name_dictionary)))
  
  # detect correct names
  index_corre_spell <- which(!is.na(match(uncleaned, name_dictionary)))
  
  # correct misspelled names
  nearest_name <- character(length(uncleaned))
  rates<- character(length(uncleaned))
  for (i in index_misspell) {
    rate <- stringsim(name_dictionary, uncleaned[i], method = method)
    names(rate)<- name_dictionary
    # get the highest similarity ratio
    name_rate <- sort(rate, decreasing = T)[1]
    print(name_rate)
    # determine the closest corrected name and enter it into the new vector "nearest_name"
    nearest_name[i]<- names(name_rate)
    # enter the highest similarity ratio into the new vector "rates"
    rates[i]<- name_rate
  }
  # insert the originially correct names in their position
  rates[index_corre_spell] <- "1"
  nearest_name[index_corre_spell] <- uncleaned[index_corre_spell]
  corrected_name_df<- data.frame(uncleaned, nearest_name, rates, stringsAsFactors = F)
  return(corrected_name_df)
}

name_correction_threshold <- function(corrected_name, similarity, cutoff, replacement = "UNKNOWN"){
  ## corrected_name: a input character vector that contains corrected names.
  
  ## similarity: a input vector that has corresponding similarity score of corrected_name compared to names in dictionary.
  
  ## cutoff: set up a threshold value.
  
  ## replacement: replace the names having similarity score below the cutoff value with what you want, default is "UNKNOWN". 
  
  ## this function returns a vector that the corrected_names having similarity score above the cutoff are kept 
  # and those having similarity score below the cutoff are marked as replacement.
  
  cleaned <- c()
  cleaned[which(similarity >= cutoff)] <- corrected_name[which(similarity>=cutoff)]
  cleaned[which(similarity < cutoff)] <- replacement
  return(cleaned)
}

# step 1: replace the misspelled name with its nearest name in the master list (name_dictionary)
corrected_name_df <- name_correction(meta2, last_name)
corrected_name_df
colnames(corrected_name_df)[which(colnames(corrected_name_df)=="uncleaned")] <- "meta2"
corrected_name_df

# step 2: determine a cutoff value
plot(corrected_name_df$rates)
hist(as.numeric(corrected_name_df$rates[which(corrected_name_df$rates<1)]))

table(corrected_name_df$rates[which(corrected_name_df$rates<=0.7&corrected_name_df$rates>=0.6)])
corrected_name_df[which(corrected_name_df$rates == 0.666666666666667), ]
corrected_name_df[which(corrected_name_df$rates == 0.6), ]
table(corrected_name_df$rates[which(corrected_name_df$rates<=0.6&corrected_name_df$rates>=0.55)])
corrected_name_df[which(corrected_name_df$rates == 0.571428571428571), ]

# step 3: apply cutoff on the corrections and generate a cleaned vector of names
meta2cleaned <- name_correction_threshold(corrected_name_df$nearest_name, corrected_name_df$rates, 0.7)
meta2cleaned
length(meta2cleaned)

# step 4: check
corrected_name_df$meta2cleaned <- meta2cleaned
corrected_name_df$meta1 <- meta1

# the number of incorrect (unrecognized) names left after cleaning
index_misspell_cleaned <- which(is.na(match(meta2cleaned, last_name)))
length(index_misspell_cleaned)

# the number of total correct names after cleaning
index_corre_spell_cleaned <- which(!is.na(match(meta2cleaned, last_name)))
length(index_corre_spell_cleaned)

# check if there is any empty name
which(nchar(meta2cleaned)==0)

# compare meta2 and meta2cleaned
length(meta1)
length(meta2)
length(meta2cleaned)
sort(table(meta2), decreasing = T)[1:10]
sort(table(meta2cleaned), decreasing = T)[1:10]

# meta2cleaned and meta2
# in both meta2cleaned and meta2
intersect(meta2cleaned, meta2)
# in meta2cleaned, but not in meta2
setdiff(meta2cleaned, meta2)

# meta2cleaned and last_name
# in both meta2cleaned and last_name
intersect(meta2cleaned, last_name)
# in meta2cleaned, but not in last_name
setdiff(meta2cleaned, last_name)
# in last_name, but not in meta2cleaned
setdiff(last_name, meta2cleaned)

#meta2 <-meta2cleaned #replaced meta2 with meta2cleaned

