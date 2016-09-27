library(rJava)
library(KoNLP)
library(tm)
library(stringr)
library(glmnet)
mobile<- read.csv('C:/Users/HA/Desktop/R project/뉴스 완성/감정사전/sample_sen.csv',stringsAsFactors = FALSE)
options(mc.cores=1) 
# 한나눔 형태소분석기는 java로 구성
# tm의 경우 core가 2개가 같이 돌아갈 경우 충돌 위험이있음

ko.words = function(doc){
  d = as.character(doc)
  d = str_split(d, ' ')[[1]] ## 띄어쓰기(' ')를 기준으로 한 문장을 여러 단어로 나눔 
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

corpus <- Corpus(VectorSource(mobile$contents))
tdm <- TermDocumentMatrix(corpus,
                          #옵션을 list로 만들어서 한꺼번에 넘겨줌
                          control=list(tokenize=ko.words,
                                       # 문장을 쪼개서 기본 단위로 만듬 
                                       # 만들어진 ko.words로 사용
                                       wordLengths=c(2,6),
                                       removeNumbers=T,
                                       weighting = weightTfIdf))

dtm = as.DocumentTermMatrix(tdm)

X <- as.matrix(dtm)
Y <- mobile$sentiment

res.lasso <- cv.glmnet(X, Y, family = "binomial", alpha = 1,
                         nfolds = 4, type.measure="class")
options(scipen = 100)
coef.lasso <- coef(res.lasso, s="lambda.min")[,1]
pos.lasso <- coef.lasso[coef.lasso>0]
neg.lasso <- coef.lasso[coef.lasso<0]

pos.lasso <- sort(pos.lasso, decreasing=T)
neg.lasso <- sort(neg.lasso, decreasing=F)



res.elastic <- cv.glmnet(X, Y, family = "binomial", alpha = .5,
                         nfolds = 4, type.measure="class")

coef.elastic <- coef(res.elastic, s="lambda.min")[,1]
pos.elastic <- coef.elastic[coef.elastic>0]
neg.elastic <- coef.elastic[coef.elastic<0]

pos.elastic <- sort(pos.elastic, decreasing=T)
neg.elastic <- sort(neg.elastic, decreasing=F)

names(pos.lasso[1:121])
names(neg.lasso[1:76])
pos.elastic[1:224]
names(pos.elastic[1:224])
names(neg.elastic[1:76])


#감성점수 예측 polartiy 사용
library(tm.plugin.sentiment)
senti.lasso <- polarity(dtm,names(pos.lasso),names(neg.lasso))
sentti.elastic <- polarity(dtm,names(pos.elastic),names(neg.elastic))

senti.lasso.b <- ifelse(senti.lasso>0,1,0)
senti.elastic.b <- ifelse(sentti.elastic>0,1,0)

#기존 감성값과 정확도 확인.
library(caret)
confusionMatrix(senti.elastic.b, mobile$sentiment)
confusionMatrix(senti.lasso.b, mobile$sentiment)

