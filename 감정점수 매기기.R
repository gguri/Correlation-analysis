library(rJava)
library(KoNLP)
library(tm)
library(stringr)
library(glmnet)

#사용할 data 불러오기
aoa <- read.csv('C:/Users/HA/Desktop/R project/뉴스 완성/aoa/aoa(1605).csv',stringsAsFactors = FALSE)
# 사용할 변수 초기값 및 벡터 생성
pos.score = 0
neg.score = 0
score=c()

# 긍정,부정 단어사전 불러오기
pos.word <- read.csv("C:/Users/HA/Desktop/R project/뉴스 완성/감정사전/dic_sen_pos.csv")
neg.word <- read.csv("C:/Users/HA/Desktop/R project/뉴스 완성/감정사전/dic_sen_neg.csv")

# 한글 형태소 분석 
ko.words = function(doc){
  d = as.character(doc)
  d = str_split(d, ' ')[[1]]  ## 띄어쓰기(' ')를 기준으로 한 문장을 여러 단어로 나눔 
  d = paste(d[nchar(d) <= 20], collapse = ' ') ## 각 단어들에서 200자 이하인 것만 선택하여 다시 문장으로 합침
  
  pos = paste(SimplePos09(d))
  extracted = str_match(pos, '([가-힣]+)/[NP]')
  #[가-힣]+ 한글 전체를 뽑아라.
  # /N 명사만 추출
  # /P 용언(형용사,동사) 추출
  keyword = extracted[,2]
  # ([가-힣]+)
  # 한글로된 부분만 여러개문자만 따로구분해서 2열에 해당
  keyword[!is.na(keyword)]  
}

# 긍정 점수 계산하기 
count.pos.word = function(){
  for (x in 1:59){
    pos = pos.word[x,1]
    for (y in 1:length(row.names(tdm))){
      pos.row.names = row.names(tdm)[y]
      if(pos == pos.row.names){
        pos.score = pos.score+1  
      }
      else{
      pos.score = pos.score+0
      }
    }
  }
  return(pos.score)
}

# 부정 점수 계산하기 
count.neg.word = function(){
  for (x in 1:59){
    neg = neg.word[x,1]
    for (y in 1:length(row.names(tdm))){
      neg.row.names = row.names(tdm)[y]
      if(neg == neg.row.names){
        neg.score = neg.score-1  
      }
      else{
        neg.score = neg.score+0
      }
    }
  }
  return(neg.score)
}


# 전체 기사 수만큼 반복
# tdm 을 만들어 준 후 각 기사 내용에 대한 긍정,부정 점수 계산 후 두개를 합산해서 총점을 매김
for(x in 1:length(aoa$contents)){
aoa_content1 = subset(aoa,number==x)
cps <- Corpus(VectorSource(aoa_content1$contents))
tdm <- TermDocumentMatrix(cps,
                          control=list(tokenize=ko.words,
                                       removePunctuation=T,
                                       removeNumbers=T,
                                       wordLengths=c(2, 5),
                                       weighting=weightBin))

total.score = count.pos.word()+count.neg.word()
score = c(score, total.score)
}

# 기존 데이터에 총점 데이터를 추가시킴
score = data.frame(score)
aoa = cbind(aoa,score)

# 0을 기준으로 긍정,중립,부정 3개로 구분 가능 
result.pos.aoa = subset(aoa, score>0)
result.neg.aoa = subset(aoa, score<0)
result.neu.aoa = subset(aoa, score==0)


