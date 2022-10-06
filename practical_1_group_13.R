#=========================#
#= Practical 1, Group 13 =#
#=========================#
# Tongfei Li (s2328507),  Likang Xu (s2295871), Yifan Jin (s2434130)

# Team member contributions:
# Likang Xu: Contributing to the main structure of the whole task. Undertaking most of the coding, especially the conception of questions 8-10, and wrote the code comments. 
# Tongfei Li: Undertaking the code conception of the details of the topic, translating the topic into a formula, making constructive comments, advancing the process, and wrote the overview. 
# Yifan Jin: Participation in the discussion of the code framework and comments on the tenth question. 

# The whole structure would be displayed here first. Firstly, we use a function (split_punct) to help us split the punctuations automatically. After that, we use the unique() function to find out all the unique words in the whole bible and use the match() function to find out the index for the words in the original bible. With the help of tabulate(), we could find out the number of appearance times for each word and rank them using the order() function. Then we put the frequency and unique words into a dataframe which could be used to find the threshold. With the threshold (frequency is higher or equal than 162), we could obtain 501 words. 

# After removing the NA rows, we could then draw out the matrix T, A, and S. For the interpretation of each column of the matrix, take matrix A as an example. Columns 1-3 would be the index for the words. Column 4 for A would be the frequency of each combination. Finally, we could use the 3 matrices to draw out the sentences using the sample() function (the probabilities were calculated here). 

# To generate the 50-word sections, firstly, obtain the first words using matrix S. With the first word, we could draw out the second word using matrix A. After this, we need a loop to draw the remaining words. Based on the first and the second word, there are three situations when drawing the third words. We constructed a function find3() to find the third word given the two words above, the mechanism would be explained below. Using find3() we generated the 50-word sections, then we used S to simulate 50-word sections to compare. 

# For question 10, based on the original b (top 501 words in Bible), we counted the frequency of words with the capital letter(s) and ranked them, then selected the same proportion of them as we selected the m words from the unique words in Bible. Nest, we added them to the original b and got a modi_b, the following steps are the same except the b is changed into modi_b



#===========================#
#= Q3 Read the file into R =#
#===========================#
setwd('C:/Users/DELL/Desktop/Kangger/Postgraduate/Statistical Programming')
a <- scan("bible.txt",what="character",skip=104) # skip contents
n <- length(a)
a <- a[-((n-2886):n)] # strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] # strip out verse numbers
show(a) # check out what is in a



#======================================#
#= Q4 Create the split_punct function =#
#======================================#
# The split_punct function has two variables: punc, aa, and can search for each element in aa (text data) containing the punc (punctuation mark), and separate the element into a word and the punc, then returns a new aa.

split_punct <- function(punc,aa){
  results <- grep(punc, aa, value = F) # find which element of a have the punc
  newa <- rep("",length(results)+length(aa)) # vector used for storing the new data
  iis <- results+1:length(results) # count the index to place the separated punc
  # for insurance purpose, if the punc is not included in aa, newa = aa
  if(length(iis)==0){newa = aa} else{
    newa[-iis] <- aa # place the words in aa to the newa
    for (i in 1:length(results)) {
      newa[iis[i]] <- substr(aa[results[i]],nchar(aa[results[i]]),nchar(aa[results[i]])) # place the punc into the right place
      newa[iis[i]-1] <- substr(aa[results[i]],1,nchar(aa[results[i]])-1) # change the elements containing the punc into a single word
    }
  }
  return(newa)
}



#=====================================#
#= Q5 Separate the punctuation marks =#
#=====================================#
# Use the split_punct function to separate the punctuation marks in a
a <- split_punct('[.,:?!;]',a)



#=================================================#
#= Q6 Select the m most commonly occurring words =#
#=================================================#
unique_words <- unique(tolower(a)) # find the vector of unique words
matched = match(tolower(a),unique_words) # find the index of unique_words in a
count_words <- data.frame(count = tabulate(matched),unique_words) # dataframe containing the frequency of words (count), and words (unique_words)
rank <- count_words[order(count_words$count, decreasing= T), ] # sort words by frequency

# find the threshold so that m is near 500, and create b
for (i in 200:150) {
  b <- rank[which(rank$count>i),2]
  if (length(b)>=500){
    cat('The threshold is',i,'\n')
    cat(length(b),'words were selcted')
    break
  }
}



#==========================#
#= Q7 Make the T, A and S =#
#==========================#
a_match <- match(tolower(a),b)

# find the index of each top words on the bible
a_matched <- grep('[1-9]', a_match,value = F) # only interested in words in b

#=====#
#= T =#
#=====#
# find word triplets 
twords <- cbind(a_matched,a_match[a_matched],a_match[a_matched+1],a_match[a_matched+2])
twords <- twords[which(is.na(rowSums(twords))==F),]

# count the frequency of word triplets, and construct T (ttt)
twords <- cbind(twords,paste(twords[,2], twords[,3], twords[,4])) # paste the three index of each row, so we can use match() 
uniq <- unique(twords[,2:4])
uniq <- cbind(uniq,paste(uniq[,1], uniq[,2], uniq[,3])) # the same purpose as above
ttt <- cbind(uniq[,1:3],tabulate(match(twords[,5], uniq[,4])))
ttt <- apply(ttt,2,as.numeric)

# the first three columns in ttt implies the index of the word triplets in b, the forth column is the frequency of each triplet in a 

#=====#
#= A =#
#=====#
# the same procedure as T
awords <- cbind(a_matched,a_match[a_matched],a_match[a_matched+1])
awords <- awords[which(is.na(rowSums(awords))==F),]
awords <- cbind(awords,paste(awords[,2], awords[,3]))
uniq <- unique(awords[,2:3])
uniq <- cbind(uniq,paste(uniq[,1], uniq[,2]))
amatch <- match(awords[,4], uniq[,3])
aaa <- cbind(uniq[,1:2],tabulate(amatch))
aaa <- apply(aaa,2,as.numeric)

# the first two columns in aaa implies the index of the word doublets in b, the third column is the frequency of each doublet in a

#=====#
#= S =#
#=====#
# the same procedure as above (but easier)
swords <- cbind(a_matched,a_match[a_matched])
uniq <- unique(swords[,2])
smatch <- match(swords[,2], uniq)
sss <- cbind(uniq,tabulate(smatch))

# the first column in sss indicates the index of a word in b, and the second column is their frequency in a.



#================================#
#= Q8 Simulate 50-word sections =#
#================================#
# select the index of the first word using S
set.seed(10)
ii <- sample(sss[,1],size=1,replace=T,prob=sss[,2])
ii

# add a probability column in T and A for the following sampling procedure
aaa <- cbind(aaa,pro = aaa[,3]/sum(aaa[,3])) 
aaa <- as.data.frame(aaa)
ttt <- cbind(ttt,pro = ttt[,4]/sum(ttt[,4]))
ttt <- as.data.frame(ttt)

# select the second word using A
set.seed(11)
kk <- aaa[which(aaa[,1]==ii),]
kk <- sample(kk[,2],size=1,replace=T,prob=kk[,4])

# the function find3(ii,kk) can find the index of third word given the index of the indexes of first and second words, it firstly judges whether there are triplets in T given ii and kk, if the answer is yes, sample the third word using T; if the answer is no, judges whether there are doublets in A given kk, if the answer is yes, sample the third word using A; if the answer is still no, sample the third word using S
find3 <- function(ii,kk){
  third <- ttt[which(ttt[,1]==ii & ttt[,2]==kk),]
# if we can't find the third word with T, generate the word using A
if (nrow(third)==0) {
# detect whether we can find the third word using A
  kkk <- aaa[which(aaa[,1]==kk),]
# if we can't find the third word with A, generate the word using S
  if (nrow(kkk)==0){
    jj <- sample(sss[,1],size=1,replace=T,prob=sss[,2])
  } else {
    # in case there is only one doublet, can't use sample()
    if (nrow(kkk)==1){
      jj <- kkk[,2]
    } else {
    jj <- sample(kkk[,2],size=1,replace=T,prob=kkk[,4])
    }
  }
} else {
  # in case there is only one triplet, can't use sample()
  if (nrow(third)==1){
    jj <- third[,3]
  } else {
  jj <- sample(third[,3],size=1,replace=T,prob=third[,5])
  }
}
  return(jj)
}

# function used to remove the space before a punc after pasting the words into sentences
remove_space <- function(senten){
  for (i in c('.',',',':','?','!',';')){
    punc_space <- paste('',i)
    senten <- gsub(punc_space,i,senten,fixed=TRUE)
  }
  return(senten)
}


# select the following word
sent <- c(ii,kk) # setting the first two words selected before
set.seed(2)
for(i in 1:48){
  sent[i+2] <- find3(sent[i],sent[i+1])
}
sent <- paste(b[sent],collapse=" ") # paste the words into sentences
sent <- remove_space(sent) # remove the " " before punctuation
cat(sent)



#=================================#
#= Q9 Word sections taken from S =#
#=================================#
set.seed(6)
sect <- paste(b[sample(sss[,1],size=50,replace=T,prob=sss[,2])],collapse=" ")
sect <- remove_space(sect)
cat(sect)



#=========================================#
#= Q10 Take capital letters into account =#
#=========================================#
# based on the b (top 501 words in bible), we counted the frequency of words with capital letter(s) and ranked them, then selected the same proportion of them as we selected the m words from the unique words in bible. Nest, we added them to the original b and got a modi_b, the following steps are the same except the b is changed into modi_b

# for q10, gain the word with a capital letter
capital_words <- grep('[LETTERS]', a, value = T)
ucw <- unique(capital_words)
ucw <- data.frame(num = tabulate(match(a, ucw)),ucw)
rank_c <- ucw[order(ucw$num, decreasing= T),]
# we extract the same proportion of capital words as b (501/13106)
prop <- floor(nrow(rank_c)*length(b)/nrow(rank))
rank_c <- rank_c[1:prop,]
modi_b <- c(b,rank_c[,2])

# for q10, match the modified b (with capital words) with the bible
a_match <- match(a,modi_b)

# find the index of each top words on the bible
a_matched <- grep('[1-9]', a_match,value = F) # only interested in words in b

#=====#
#= T =#
#=====#
twords <- cbind(a_matched,a_match[a_matched],a_match[a_matched+1],a_match[a_matched+2])
twords <- twords[which(is.na(rowSums(twords))==F),]
twords <- cbind(twords,paste(twords[,2], twords[,3], twords[,4]))
uniq <- unique(twords[,2:4])
uniq <- cbind(uniq,paste(uniq[,1], uniq[,2], uniq[,3]))
ttt <- cbind(uniq[,1:3],tabulate(match(twords[,5], uniq[,4])))
ttt <- apply(ttt,2,as.numeric)
#=====#
#= A =#
#=====#
awords <- cbind(a_matched,a_match[a_matched],a_match[a_matched+1])
awords <- awords[which(is.na(rowSums(awords))==F),]
awords <- cbind(awords,paste(awords[,2], awords[,3]))
uniq <- unique(awords[,2:3])
uniq <- cbind(uniq,paste(uniq[,1], uniq[,2]))
amatch <- match(awords[,4], uniq[,3])
aaa <- cbind(uniq[,1:2],tabulate(amatch))
aaa <- apply(aaa,2,as.numeric)
#=====#
#= S =#
#=====#
swords <- cbind(a_matched,a_match[a_matched])
uniq <- unique(swords[,2])
smatch <- match(swords[,2], uniq)
sss <- cbind(uniq,tabulate(smatch))

# for q10, the first word should be selected form capital words
ii <- sample(rank_c[,2],size=1,replace=T,prob=rank_c[,1])
ii <- which(modi_b==ii)

# add a probability column in T and A for the following sampling procedure
aaa <- cbind(aaa,pro = aaa[,3]/sum(aaa[,3])) 
aaa <- as.data.frame(aaa)
ttt <- cbind(ttt,pro = ttt[,4]/sum(ttt[,4]))
ttt <- as.data.frame(ttt)

# select the second word using A
set.seed(11)
kk <- aaa[which(aaa[,1]==ii),]
kk <- sample(kk[,2],size=1,replace=T,prob=kk[,4])

# select the following word
sent_10 <- c(ii,kk) # setting the first two words selected before
set.seed(5)
for(i in 1:48){
  sent_10[i+2] <- find3(sent_10[i],sent_10[i+1])
}
sent_10 <- paste(modi_b[sent_10],collapse=" ")
sent_10 <- remove_space(sent_10)
cat(sent_10)









