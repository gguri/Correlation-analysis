library(httr)
library(rvest)
library(stringr)

first_url = 'http://news.naver.com/main/list.nhn?sid1=106&mid=sec&mode=LSD&date= '
plus_page = 'page='

#데이터가 들어갈 벡터들 생성
titles = c()
contents = c()
dates = c()
writings = c()

#크롤링할 날짜범위 지정 as.date함수 사용
date_range=seq(from=as.Date('2016-02-01'), to=as.Date('2016-02-28'),by='days') 

#url의 경우 - 문자가 없는 20160301 형식이므로 gsub 사용해서 특정 문자 제거
date_range=gsub('-','', date_range)

# 첫번째 for : 날짜별 url 생성
for(i in date_range){
  second_url = modify_url(first_url, query=list(date=i))
  second_url=paste(second_url,plus_page,seq='&')
  # 조건문 각 일자별 페이지 지정 = 일별 2천개의 기사 수집이 목표
  if (i==20160205)
  {
    number = c(1:100)*3
  }
  else if(i==20160206)
  {
    number = c(1:70)*2
    number = c(number,141:170)
  }
  else{
    number = c(1:100)*4
  }
  
# 두번째 for : 날짜별 url 에 paste함수를 사용해서 page번호를 추가한 url 생성
  for(j in number){
    url=modify_url(second_url, query=list(page=j)) 
    h.list = read_html(url) 
    
    #네이버 뉴스 연예 속보는 headline과 일반 두분류로 구분
    title.links1 = html_nodes(h.list, '.type06_headline dt a')
    title.links2 = html_nodes(h.list, '.type06 dt a')
    
    #두 링크를 얻어서 하나의 벡터로 생성
    article.links1 = unique(html_attr(title.links1, 'href'))
    article.links2 = unique(html_attr(title.links2, 'href'))
    article.links = c(article.links1,article.links2)
    
    #headline 기사들에 해당하는 신문사 및 날짜
    article.writing1 = html_text(html_nodes(h.list, '.type06_headline dd span.writing'))
    article.date1 = html_text(html_nodes(h.list, '.type06_headline dd span.date'))
    
    #아래부분 기사들에 해당하는 신문사 및 날짜
    article.writing2 = html_text(html_nodes(h.list, '.type06 dd span.writing'))
    article.date2 = html_text(html_nodes(h.list, '.type06 dd span.date'))
    
    article.writing=c(article.writing1,article.writing2)
    article.date=c(article.date1,article.date2)
    
    dates = c(dates, article.date)
    writings = c (writings, article.writing)
    print(url)
    
    # 3번째 for : 수집한 url 링크들의 본문 게시글을 순차적으로 탐색
    for(link in article.links){
      h = read_html(link)  # 게시물을 가져옴
      
      # 제목
      title = html_text(html_nodes(h, 'p.end_tit'))
      title = str_trim(title)
      titles = c(titles, title)
    
      # 내용
      content = html_text(html_nodes(h, 'div#articeBody'))
      content = str_trim(content)
      contents = c(contents, content)
     
  
      print(link)
      Sys.sleep(0.5)
    }
  }
}기

#수집한 자료 data.frame객체에 저장
news = data.frame(titles,contents,writings,dates)

#data.frame csv파일로 내보내기
write.csv(news1,"C:/Users/HA/Desktop/R project/뉴스 완성/result(0227_146).csv")

