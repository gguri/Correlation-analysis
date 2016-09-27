
neg.words= c()
neg.use.word = c()

for (a in c(result.neg.aoa$number)){
aoa_content1 = subset(result.neg.aoa,number==a)
cps <- Corpus(VectorSource(aoa_content1$contents))  
tdm <- TermDocumentMatrix(cps,
                          control=list(tokenize=ko.words,
                                       removePunctuation=T,
                                       removeNumbers=T,
                                       wordLengths=c(2, 5),
                                       weighting=weightBin))
#사전을 올리고 갯수(59) 만큼 범위지정
  for (x in 1:59){
    neg = neg.word[x,1]
    for (y in 1:length(row.names(tdm))){
      neg.row.names = row.names(tdm)[y]
      if(neg == neg.row.names){
        neg.words = c(neg.words, neg.row.names)
        neg.use.word = c(neg.words)
      }
      else{
        neg.score = neg.score+0
      }
    }
  }
}

neg.use.word <- sort(neg.use.word, decreasing = F)
neg.use.word = data.frame(neg.use.word)
