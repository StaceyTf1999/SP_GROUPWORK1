# Practical 1

#match(c("tum","tee","tum","tee","tumpty","tum","wibble","wobble"),c("tum","tee"))


# setwd("put/your/local/repo/location/here")
setwd('C:/Users/DELL/Desktop/Kangger/Postgraduate/Statistical Programming')
a <- scan("bible.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers


# results <- gsub(".","",gsub(",","",b,fixed=TRUE),fixed=TRUE)
# results[42]

# substring("abcdef", 1:5, 5:6)


# pack as a function
b <- a

results <- grep('.', b,fixed = TRUE, value = F)
results # which elements of a have the prun
newa <- rep("",length(results)+length(b)) ## vector to store the punc
iis <- results+1:length(results)
newa[-iis] <- b
for (i in 1:length(results)) {
  newa[iis[i]] <- substr(b[results[i]],nchar(b[results[i]]),nchar(b[results[i]]))
  newa[iis[i]-1] <- substr(b[results[i]],1,nchar(b[results[i]])-1)
}

# q4

# for insurance, if a punctuation is not included, newa = b
split_punct <- function(prun,b){
  results <- grep(prun, b,fixed = TRUE, value = F)
  results # which elements of a have the punc
  newa <- rep("",length(results)+length(b)) ## vector to store the punc
  iis <- results+1:length(results)
  if(length(iis)==0){newa = b} else{
  newa[-iis] <- b
  for (i in 1:length(results)) {
    newa[iis[i]] <- substr(b[results[i]],nchar(b[results[i]]),nchar(b[results[i]]))
    newa[iis[i]-1] <- substr(b[results[i]],1,nchar(b[results[i]])-1)
  }
  }
  return(newa)
}

#split_punct('.',b)


# q5 ",", ".", ";", "!", ":" and "?"

for (i in c(",",".",";","!",":","?")) {
  b = split_punct(i,b)
}
#b

# q6

unique_words <- unique(tolower(b))
matched = match(tolower(b),unique_words)
t <- tabulate(matched)
# try to use cbind instead of dataframe!!!!!!!!!!!!!
twithwords <- data.frame(t,unique_words)
rank <- twithwords[order(twithwords$t, decreasing= T), ]
top500 <- rank[1:300,2]


# q7
lowerb <- tolower(b)
mmm <- match(lowerb,top500) # 21NA
# find the index of each top words on the bible
indexmm <- grep('[1-9]', mmm,value = F)

# to search for the following words, not necessary needed
t <- cbind(indexmm,indexmm+1,indexmm+1)
twords <- cbind(indexmm,mmm[indexmm],mmm[indexmm+1],mmm[indexmm+2])
twords <- twords[which(is.na(rowSums(twords))==F),]




# d


# rowSums(data.frame(uniq[1,]==twords[1,2:4]))
# 
# twords[2,2:4]==twords[1,2:4]
# 
# for (i in 1:nrow(twords)) {
# for (j in 1:nrow(twords)) {
#   if(twords[i,2:4]==twords[j,2:4]){
#     count <- 0
#     ttt[i,1:3] <- twords[i,2:4]
#     ttt[i,4] <- 1+count
#     count <- count+1
#   }
#     
# }
# }



# twords <- cbind(twords, rep(1,nrow(twords)))
uniq <- unique(twords[,2:4])
twords <- cbind(twords,paste(twords[,2], twords[,3], twords[,4]))
uniq <- cbind(uniq,paste(uniq[,1], uniq[,2], uniq[,3]))
# give ttt a column of 1s so that we don't need the count"
# for (i in 1:nrow(uniq)) {
#   count <- 0
#   for (j in 1:nrow(twords)) {
#   if (unique(twords[,2:4])[i,1] == twords[,2:4][j,1] &
#       unique(twords[,2:4])[i,2] == twords[,2:4][j,2] &
#       unique(twords[,2:4])[i,3] == twords[,2:4][j,3]){
#     ttt[i,1:3] <- uniq[i,1:3]
#     ttt[i,4] <- 1+count
#     count <- count+1
#   }
#   }
# }
freqt <- c()
for (i in 1:nrow(uniq)) {
  freqt[i] <- length(which(twords[,5]==uniq[i,4]))
}
ttt <- cbind(uniq[,1:3],freqt)



# e

# sample(ttt,size=20,replace=T,prob=c(0.8,0.2))

# q7 a, swords change to awords
swords <- cbind(indexmm,mmm[indexmm],mmm[indexmm+1])
swords <- swords[which(is.na(rowSums(swords))==F),]
swords <- cbind(swords,paste(swords[,2], swords[,3]))

uniq <- unique(swords[,2:3])
uniq <- cbind(uniq,paste(uniq[,1], uniq[,2]))

freqa <- c()
for (i in 1:nrow(uniq)) {
  freqa[i] <- length(which(swords[,4]==uniq[i,3]))
}
aaa <- cbind(uniq[,1:2],freqa)


# q7 s

swords <- cbind(indexmm,mmm[indexmm])
uniq <- unique(swords[,2])
# give ttt a column of 1s so that we don't need the count"
# for (i in 1:length(uniq)) {
#   count <- 0
#   for (j in 1:nrow(swords)) {
#     if (uniq[i] == swords[,2][j]){
#       sss[i,1] <- uniq[i]
#       sss[i,2] <- 1+count
#       count <- count+1
#     }
#   }
# }
freqs <- c()
for (i in 1:length(uniq)) {
  freqs[i] <- length(which(swords[,2]==uniq[i]))
}
sss <- cbind(uniq,freqs)




# q8
# set.seed(10)
ii <- sample(sss[,1],size=1,replace=T,prob=sss[,2])
ii

# add probability
aaa <- cbind(aaa,pro = freqa/sum(freqa)) # remember to remove the #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
aaa <- apply(aaa,2,as.numeric)
aaa <- as.data.frame(aaa)
# merge(aaa,freqa)
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ttt <- apply(ttt,2,as.numeric)
ttt <- cbind(ttt,pro = freqt/sum(freqt)) # remember to remove the #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ttt <- as.data.frame(ttt)
ttt
# select the second word
kk <- aaa[which(aaa[,1]==ii),]
kk <- sample(kk[,2],size=1,replace=T,prob=kk[,4])


# select the third word
find3 <- function(ii,kk){
  third <- ttt[which(ttt[,1]==ii & ttt[,2]==kk),]
if (nrow(third)==0) {
# detect whether we can find the third word using A
  kkk <- aaa[which(aaa[,1]==kk),]
# if we can't find the third word with A, generate the word using S
  if (nrow(kkk)==0){
    jj <- sample(sss[,1],size=1,replace=T,prob=sss[,2])
  } else {
    if (nrow(kkk)==1){
      jj <- kkk[,2]
    } else {
    jj <- sample(kkk[,2],size=1,replace=T,prob=kkk[,4])
    }
  }
} else {
  if (nrow(third)==1){
    jj <- third[,3]
  } else {
  jj <- sample(third[,3],size=1,replace=T,prob=third[,5])
  }
}
  return(jj)
}
# if we encounter the problem of incorrect number of probabilities, add if else to every jj

sent <- c(ii,kk)
sent
set.seed(5)
for(i in 1:48){
  sent[i+2] <- find3(sent[i],sent[i+1])
}

# show the sentence

print(top500[sent])

sent <- paste(top500[sent],collapse=" ")

sent <- gsub(" .",".",gsub(" ,",",",sent,fixed=TRUE),fixed=TRUE)
cat(sent)

# q9 word sections taken from S

sect <- paste(top500[sample(sss[,1],size=50,replace=T,prob=sss[,2])],collapse=" ")
sect <- gsub(" .",".",gsub(" ,",",",sect,fixed=TRUE),fixed=TRUE)
cat(sect)





