
pos.words= c()
pos.use.word = c()
for (a in c(result.pos.aoa$number)){
  aoa_content2 = subset(result.pos.aoa,number==a)
  cps <- Corpus(VectorSource(aoa_content2$contents))  
  tdm <- TermDocumentMatrix(cps,
                            control=list(tokenize=ko.words,
                                         removePunctuation=T,
                                         removeNumbers=T,
                                         wordLengths=c(2, 5),
                                         weighting=weightBin))
  #사전을 올리고 갯수(59) 만큼 범위지정
  for (x in c(1:59)){
    pos = pos.word[x,1]
    for (y in 1:length(row.names(tdm))){
      pos.row.names = row.names(tdm)[y]
        if(pos == pos.row.names){
        pos.words = c(pos.words, pos.row.names)
        pos.use.word = c(pos.words)
      }
      else{
        pos.score = pos.score+0
      }
    }
  }
}
pos.use.word <- sort(pos.use.word, decreasing = F)
pos.use.word = data.frame(pos.use.word)
