
# 설치된 모든 패키지 출력
search()

# 모든 식별자 삭제
rm(list=ls())

## DB 연동

# 패키지 설치
install.packages("rJava")
install.packages('RJDBC')
install.package("dplyr")

# 패키지 로드
library(rJava)
library(RJDBC)
library(dplyr)

# 한글깨짐 방지를 위한 인코딩 변경.
options(encoding="UTF-8")

# DB연결
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver"
                   , classPath="ojdbc6.jar")

conn <- dbConnect(jdbcDriver, 
                  "jdbc:oracle:thin:@localhost:1521/xe", "HR", "1234")

data <- dbGetQuery(conn, "SELECT * FROM seoultree")
data_new <- data

View(data)
View(data_new)

## 구글맵 - 가로수 위치 지도에 표시

# 패키지 로드
library(googleVis)

# 데이터 전처리
loc <- data_new %>% select(위도,경도, 가로명,수목명)
loc <- loc %>%  mutate(LATLON=paste(loc$위도,":",loc$경도, sep=""))
loc <- loc %>%  mutate(name=paste(loc$가로명,"-",loc$수목명))

# 결측치 제거
loc_nomiss <- na.omit(loc)

class(loc_nomiss)
str(loc_nomiss)
head(loc_nomiss)

print(loc_nomiss)

# 그래프 출력
# 데이터프레임,위도:경도, 요약정보
hoffice <- gvisMap(loc_nomiss, "LATLON" , "name"
                   ,options=list(showTip=TRUE
                                 ,showLine=TRUE
                                 ,enableScrollWheel=TRUE
                                 ,mapType='normal'
                                 ,useMapTypeControl=TRUE
                                 ,width=1000
                                 ,height=400))
plot(hoffice)


## 트리맵 - 어느 동에 가로수가 가장 많은지 트리맵으로 표현

# 패키지 설치
install.packages("dplyr")
install.packages("treemap")

# 패키지 로드
library(dplyr)
library(treemap)

# 데이터 전처리
dir()
tree <- select(data_new, 가로명)
class(tree)
head(tree)

# table() 함수를 이용해서 숫자 세기, 변수가 한개일때 도수분표표를 만들어줌
tree_count <- tree %>% table() %>% data.frame()
head(tree_count)
# 그래프 출력
# treemap(데이터, index=인덱스 표시 열 제목, vSize=크기를 이용할 열 제목, vColor=컬러, title=제목)
treemap(tree_count, index=".", vSize="Freq", title="서울특별시 가로수 가로명별 분포")
arrange(tree_count,desc(Freq)) %>% head()


## 구글 게이지 그래프
# 패키지 로드
library(googleVis)
library(dplyr)
#데이터 전처리
gauge <- select(data_new, 수목명)
gauge_cnt  <- gauge %>% table() %>% data.frame()
gauge_cnt <- gauge_cnt %>% arrange(desc(Freq))
#그래프 출력
g <-gvisGauge(gauge_cnt, options=list(min=0, max=2000, 
                                      greenFrom=1000, greenTo=2000, 
                                      yellowFrom=200, yellowTo=1000,
                                      redFrom=0, redTo=200, width=800, height=600))
plot(g)


## 워드클라우드 - 어떤 나무의 종류가 많은지 글자의 크기로 파악

# 패키지 설치
install.packages("wordcloud")

# 패키지 로드
library(wordcloud)

# 데이터 전처리
word <- select(data_new, 수목명)
word_cnt  <- word %>% table() %>% data.frame()
word_cnt <- word_cnt

# 그래프 출력
pal <- brewer.pal(8,"Dark2")
set.seed(1234)

wordcloud(words = word_cnt$.
          ,freq = word_cnt$Freq
          ,min.freq = 1
          ,max.words = 2000
          ,random.order = F
          ,rot.per = .1
          ,scale = c(5, 1)
          ,colors = pal)


## 구글 스캐터 차트 - 수관너비에 따른 평균 흉고지름

# 패키지 로드
library(googleVis)
library(dplyr)

# 데이터 전처리
scatter <- select(data_new, 수관너비, 흉고지름)
as.numeric(scatter$수관너비)
as.numeric(scatter$흉고지름)
mode(scatter$수관너비)<-"numeric"
mode(scatter$흉고지름)<-"numeric"
View(scatter)
class(scatter$흉고지름)

scatter_data <- scatter %>%
  group_by(수관너비) %>%
  summarize(평균_흉고지름 = mean(흉고지름))

View(scatter_data)

# 스캐터 그래프 출력
Scatter2 <- gvisScatterChart(scatter_data, 
                             options=list(legend="none",
                                          lineWidth=2, pointSize=2,
                                          title="tree", vAxis="{title:'평균_흉고지름 (lbs)'}",
                                          crosshair="{ trigger: 'both' }", 
                                          hAxis="{title:'수관너비 (in)'}", width=500, height=400))
plot(Scatter2)

## 구글 파이 차트 - 관리등급에 따른 평균 흉고지름
library(googleVis)
library(dplyr)

# 데이터 전처리
pie <- select(data_new, 관리등급, 흉고지름)
as.numeric(pie$흉고지름)
mode(pie$흉고지름) <-"numeric"
class(pie$흉고지름)
View(pie)

pie_data <- pie %>%
  group_by(관리등급) %>%
  summarize(평균_흉고지름 = mean(흉고지름))
# 그래프 출력
p <- gvisPieChart(pie_data,options=list(width=800, height=600)) 
plot(p)

## 인터랙티브 그래프 - 가로수의 종류와, 관리등급에 따른 나무의 개수 파악
# 패키지 로드
library(plotly)
library(ggplot2)
library(dplyr)

# 데이터 전처리
View(data_new)
grade <- select(data_new, 수목명, 관리등급)

# 그래프 출력
q <- ggplot(data = grade, aes(x = 수목명, fill = 관리등급)) + geom_bar(position = "dodge")
ggplotly(q)


## 피해정도에 따른 가로수의 수 파악.
# 패키지 로드
library(plotly)
library(ggplot2)
library(dplyr)

# 데이터 전처리
damage <- select(data_new, 수목명, 피해정도)
View(damage)

# 그래프 출력
damage_chart <- ggplot(data = damage, aes(x = 수목명, fill = 피해정도)) + geom_bar(position = "dodge")
ggplotly(damage_chart)

# 수관너비와 수고 관계 - 스캐터 그래프
# 패키지 로드
library(googleVis)
library(dplyr)

# 데이터 전처리
scatter <- select(data_new, 수관너비, 수고, 흉고지름)

as.numeric(scatter$수관너비)
as.numeric(scatter$수고)

mode(scatter$수관너비)<-"numeric"
mode(scatter$수고)<-"numeric"
View(scatter)

cor.test(scatter$수관너비,scatter$수고)

scatter_data <- scatter %>%
  group_by(수관너비) %>%
  summarize(평균_수고 = mean(수고))
View(scatter_data)

# 그래프 출력 
Scatter2 <- gvisScatterChart(scatter_data, 
                             options=list(legend="none",
                                          lineWidth=2, pointSize=2,
                                          title="tree", vAxis="{title:'평균_수고 (lbs)'}",
                                          crosshair="{ trigger: 'both' }", 
                                          hAxis="{title:'수관너비 (in)'}", width=500, height=400))
plot(Scatter2)
