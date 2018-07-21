install.packages("rJava")
install.packages("DBI")
install.packages("RJDBC") #R JAVA Database Connect
install.packages("data.table")
install.packages("dplyr") #dplyr은 프로젝트당 하나씩

library(rJAVA)     #객체(object) #페이지당 하나씩
library(DBI)
library(RJDBC)
library(data.table)
library(dplyr)

drv <- JDBC(
  "oracle.jdbc.driver.OracleDriver",
  "C:\\oraclexe\\app\\oracle\\product\\11.2.0\\server\\jdbc\\lib\\ojdbc6.jar"
)
conn <- dbConnect(drv,
                  "jdbc:oracle:thin:@localhost:1521:xe",
                  "hr",
                  "oracle")
tab <- dbGetQuery(conn, "SELECT * FROM TAB")
View(tab)

tname <- tab$TNAME
tname

# COUNTRIES   cnt
# DEPARTMENTS dep
# EMPLOYEES emp
# EMP_DETAILS_VIEW  empd
# JOBS job
# JOB_HISTORY jobh
# LOCATIONS loc
# REGIONS   reg     #ctrl+shift+c  

cnt <- data.frame(dbGetQuery(conn,"SELECT * FROM COUNTRIES"))
View(cnt)
dep <- data.frame(dbGetQuery(conn,"SELECT * FROM DEPARTMENTS"))
emp <- data.frame(dbGetQuery(conn,"SELECT * FROM EMPLOYEES"))
empd <- data.frame(dbGetQuery(conn,"SELECT * FROM EMP_DETAILS_VIEW"))
job <- data.frame(dbGetQuery(conn,"SELECT * FROM JOBS"))
jobh <- data.frame(dbGetQuery(conn,"SELECT * FROM JOB_HISTORY"))
loc <- data.frame(dbGetQuery(conn,"SELECT * FROM LOCATIONS"))
reg <- data.frame(dbGetQuery(conn,"SELECT * FROM REGIONS"))


View(emp)
##### EMPLOYEE 문제
##문제 1 . 사원의 First Name 과 Last name 을 붙여서 Name 으로 된 컬럼을 추가하시오. 예) James Dean
emp <- emp %>% dplyr::mutate(Name=paste(emp$FIRST_NAME,emp$LAST_NAME))



## 문제 2. (SALARY는 연봉) 경리부에서 매달 지급하는 월급여를 알려달라고 하여 MONTH_SAL이라는 컬럼을 추가시켜주세요.
emp <- emp %>% dplyr::mutate(MONTH_SAL=emp$SALARY/12)

##문제3. 경리부에서 급여가 20000불 이상인 사원의 목록을 NAME, EMPLOYEE_ID, SALARY 만 보여주세요
k<-emp %>% filter(SALARY>=20000)
k %>% dplyr::select(Name,EMPLOYEE_ID,SALARY)

#emp%>% 
#select(LAST_NAME,EMPLOYEE_ID,SALARY) %>%
#filter(SALARY >= 20000)

##문제 4. 경리부에서 급여가 7000불 이하인 사원에게 보너스로 급여의 10%를 더 지급하겠다고 합니다. 이번달에 보너스가 추가된 대상자의 목록을 NAME, EMPLOYEE_ID, SALARY 만 보여주세요

p<-emp %>% filter(SALARY<=7000)
p %>% dplyr::select(Name,EMPLOYEE_ID,SALARY)
View(p)

##문제 5. 직원중에서 성(LAST_NAME)에 e 또는 o 가 포함된 직원을 출력하시오.
emp %>% dplyr::select(emp$LAST_NAME, contains("e"))

##문제 6. 직원중에서 급여가 가장 높은 사람이 CEO라고 합니다. 이름이 무엇입니까?
##apply(object,direction,function to apply)
##적용방향 - 1:가로방향, 2:세로방향
ceo_sal <- apply(emp %>% select(SALARY),2,max)
q<-emp%>%filter(SALARY==ceo_sal)
q %>% dplyr::select(Name)







