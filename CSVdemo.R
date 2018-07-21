getwd()
#Database(Oracle,MySQL,MariaDB)
#Excel(xls)
#CSV (Comma separated Vector)
#외부 라이브러리는 타 개발자가 만들어 놓은 함수의 집합 

install.packages("dplyr") #한번만 해도 됨
library(dplyr)
read.csv("class_scores.csv")
score <- data.frame(read.csv("class_scores.csv"))
score
head(score)
tail(score)
summary(score)
view(score)
View(score)
# stu_ID, score, class, gender
# Math, English, Science, Marketing, Writing 
## select : 선택한 metadata(variable)에 해당하는 instance 를 출력 
## filter, distance, top_n, sample_n : 선택한 row_key value 에 해당하는 instance 를 출력 
## mutate, transmutate, mutate_each : data transform 
## group_by : group data analysis 
## summarise, summarise_each, count : summary
## arrange : data sorting
## inner_join. left_join, right_join, full_join : combination or join

head(score)
head(select(score,"Math"))
mean(score,"Math")
##NA,NULL
##NA : Not Avilable -> 결측값
## 값이 있기는 한데 정확히 몇인지는 모르는 상태.. 값이 있는것이 포인트
## 속성값이 없는 상태
## NULL 은 값이 없는 상태 . 0과는 다른 개념, 객체가 없는 상태

## 1 영어, 수학, 과학 도메인기(=컬럼)만 가져오기
score %>% 
  dplyr::select(Math, English, Science) %>%
  head

## 2 상위 10개 보기 top_n
score %>%
  dplyr::select(Math,English,Science) %>%
  slice(1:100)

## 3 성별 제외한 컬럼 보기
score %>%
  dplyr::select(-gender)%>%
  slice(1:12)

## 4 수학부터 작문까지 컬럼 보기
score %>%
  dplyr::select(Math:Writing) %>%
  slice(1:3)

## 5 모든 컬럼 조회 everything()
score %>%
  dplyr::select(everything())%>%
  head

## 6 E 로 시작하는 컬럼만 보기 starts_with('E')
score %>%
  dplyr::select(starts_with('E'))

## 7 e 로 끝나는 컬럼만 보기 ends_with('e')
score %>%
  dplyr::select(ends_with('e'))
## 8 e 가 들어가는 컬럼 다 가져오기 contains('e')
score %>%
  dplyr::select((contains('e')))
## 9.  1, 3, 5번째 컬럼만 가져오기 


## filter 예제
## 1. 1학년 학생들만 보기
score %>%
  filter(grade==1)%>%
  slice(1:3)
## 2. 1학년 남학생만 보기
score %>%
  filter(grade==1 & gender == 'M')%>%
  slice(1:3)
## 3. 1학년이 아닌 학생들만 보기 (!grade=1)
score %>%
  filter(!grade==1)%>%
  slice(1:3)
## 4. 1, 2학년 학생들만 보기 (grade==1 | grade==2)
score %>%
  filter(grade==1 | grade==2)%>%
  slice(1:50)
## 5. 수학점수가 80이상인 학생들만 보기 (Math>=80)
score %>%
  filter(Math>=80)%>%
  slice(1:50)
## 6. 수학점수가 80 이상이면서 영어점수가
##   70이상이 학생들만 보기 (Math>=80 | English>=70)
score %>%
  filter(Math>=80 & English>=70)%>%
  slice(1:50)
## 7. 학번이 10101 부터 10120인 학생들 
##  중에서 여학생이면서 영어가 80점 이상인
##  학생만 보기 (Stu_ID>=10101 & Stu_ID<=10120 & gender==F)
score %>%
  filter(Stu_ID>=10101 & Stu_ID<=10120 & gender=='F')%>%
  slice(1:50)
## 8. 학번이 홀수인 학생들 중 남자이면서 
##  수학과 과학이 모두 90점 이상인
##  학생들만 보기 (Stu_ID%%2==1 & gender==F & Math >= 90 & Science >= 90)
a <- score %>%
  filter(Stu_ID%%2==1 & gender=='M' & Math >= 90 & Science >= 90)
View(a)
## 9. 학생들 중 한 과목이라도 100점이 있는
##  학생만 보기 (contains(grade==100))
score %>%
  filter(Math==100 | English == 100 |Science==100|Marketing==100|Writing==100)
## 10. 학생들 중 한 과목이라도 0점이 있는
##   학생만 보 (contains(grade==0))
score %>%
  filter(Math==0 | English == 0 |Science==0|Marketing==0|Writing==0)

#mutate example
## 1. add 'Average' column 
score<-score %>% dplyr::mutate(Average=(Math+English+Science+Marketing+ Writing)/5)
View(score)
## 2. add a colunm 'Rank' based on average score
   ## dense_rank(desc(Average))
score<-score%>%dplyr::mutate(Rank=dense_rank(desc(Average)))
View(score)

##3. Arrange 를 기준으로 정렬하기
score %>%
  dplyr::arrange(Rank)%>%
  slice(1:3)

View(score)
## ifelse 예제
  ##mutate를 통해 eval 생성하기 (A~F)
  ##ifelse(Average>=90, 'A')
score %>% 
  dplyr::mutate(eval=ifelse(Average>=90,'A'),
                      ifelse(Average>=80 & Average<90,'B'),
                      ifelse(Average>=70 & Average<80,'C'),
                      ifelse(Average>=60 & Average<70,'D'),
                      ifelse(Average>=50 & Average<60,'E'),
                      ifelse(Average<50,'F'))
View(score)


#평균점수를 성별로 보기
temp <- score %>%
  mutate(temp_avg=(Math+English+Science+Marketing+Writing)/5) %>%
  group_by(gender)%>%
  summarise(성별평균점수=mean(temp_avg))
View(temp)

#히스토그램 
hist(temp$성별평균점수,
     xlab="남",
     col="yellow",
     border="blue")
#bar chart
barplot(temp$성별평균점수)
#pie chart
pie(
  c(temp$성별평균점수),
  c("남","여"),
  col=c("blue","red"))
#line chart
plot(c(temp$성별평균점수),type="o")

