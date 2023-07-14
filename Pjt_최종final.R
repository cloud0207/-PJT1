---
  title: "서울 특별시의 전기차 증가율 추이 분석 및 전기차 증가 방해 요인 분석"
author: "jangyelim"
date: "2023-05-16"
output: html_document
---
  
  
  ```{r }
setwd("/Users/jang-yelim/mini pjt/data")

# 1. 서울시의 전기차 증가 추이 분석 및 예측
# 1) 매년 전기차 수에 따른 회귀분석을 통한 2022년 전기차수 예측

# 독립변수(X):  19,20,21년도 월별 전기차대수
# 종속변수(Y):  22년도 월별 전기차대수
library(readxl)
mec22_c <- read_excel("../mini pjt/data/4개년월별전기차대수.xlsx")
names(mec22_c) <- c('c19','c20','c21','c22')


# 상관관계 분석 ####
# H0: 22년도 전기차대수는 19,20,21년도 전기차대수와 영향이 없다
# H1: 22년도 전기차대수는 19,20,21년도 전기차대수와 영향이 있다
cor(mec22_c)
# 22년도 전기차 대수는 19,20,21년도읭 전기차 대수와 매우 높은 양의 상관관계 있음을 알수있다


# 상관관계 시각화 그래프 ####
library(PerformanceAnalytics)
chart.Correlation(mec22_c)


# 회귀분석 ####
lm_mec <- lm(c22 ~., mec22_c)
# 설명력= 99% (Adjusted R-squared:  0.9944)
# H0기각, H1채택 (p-value: 6.537e-10)
# 각각의 변수에 대해서는 유의미 하지 않았지만, 조정된 결정의 계수로는 유의미한 결과가 나왔다.

library(car)
qqPlot(lm_mec, labels=row.names(mec22_c), id.method='identify')
outlierTest(lm_mec)
vif(lm_mec)
# 잔차(residuals) = 실제값과 예측값의 사이

lm_mec$residuals
# 잔차가 큰 순으로 재정렬
sort(lm_mec$residuals, decreasing = T)
# 잔차 및 이상치를 어떻게 할지 고민 필요! (평균값보다 중간값이 좋음)

lm_mec$coefficients
# 1.017584e+04 + (5.275860e-01 * x(c19)) = y(c22)
# 1.017584e+04 + (1.090375e+00 * x(c20)) = y(c22)
# 1.017584e+04 + (3.869881e-01 * x(c21)) = y(c22)


## plot() 그래프 설명
plot(lm_mec,1)
# 예측값(fitted)와 잔차(residuals) 비교 (수평도 확인)
# 빨간실선 = 잔차의 추세 (데이터가 중심의 아래쪽에 있다)
# 이상치 = 5,2,3로 관측됨

# 잔차 정규성 확인
plot(lm_mec,2)
# 이상치=5,2,3
shapiro.test(lm_mec$residuals)
# 잔차의 정규성 검사 실시
# p-value = 0.4624, 정규성에 따름

plot(lm_mec,3)
# 이상치=5,2,3
# 잔차의 등분산성(분산이 고르게 분포되어있다/분산이 같다)
# 빨간선의 위치 보고 판단 (일자여야됨)

plot(lm_mec,4)
# 잔차의 극단값을 나타내는 지표
# 11,2,3,5 극단값을 어떻게 처리해야할지 고민

install.packages("Mass")
library(MASS)
lm_mec2 <- stepAIC(lm_mec)

lm_mec3 <- mec22_c %>%select(c20,c21,c22)
# 설명력 높이기
lm_mec4 <- lm(c22 ~ c20 + c21, data=lm_mec3)
summary(lm_mec4)
# stepAIC() 없이 진행했을 때 모두가 유의미하지 않았지만, 전체적으로 유의미한 결과가 나왔다.
# 후, stepAIC() 진행하여 다중회귀를 진행했을때, 모형의 설명력이 99%로 모두 양의 유의미한 결과를 얻을 수 있다.

plot(lm_mec4,2)
shapiro.test(lm_mec4$residuals)

ncvTest(lm_mec4)
plot(lm_mec4,3)
plot(lm_mec4,1)
plot(lm_mec4,4)
# 12,11,2,3,5 이상치 제거 요망


# 이상치제거
library(tidyverse)
which.max(lm_mec4$residuals) 
#실제값과 예측값의 차이가 큰 잔차찾기

lm_mec3 <- lm_mec3[-c(5,3,2,11,4), ]
lm_mec4 <- lm(c22 ~ c20 + c21, data=lm_mec3)
ncvTest(lm_mec4)
plot(lm_mec4, 3)
plot(lm_mec4, 1)
shapiro.test(lm_mec4$residuals)
# 0.05보다 큰 값으로 정규성을 갖는다.
# 다중공정성 확인했을때 10보다 큰 값인 14.56246로 이상치를 다시 제거하였다

# 이상치 제거 후 재확인
lm_mec3 <- lm_mec3[-3, ]
lm_mec4 <- lm(c22 ~ c20 + c21, data=lm_mec3)


# 다중공정성 확인 ####
# 각각 모든 변수들간 산점도를 그려, 다중공정성 확인
# 1에 가까운 수가 있으므로 다중공선성이 의심
library(psych)
library(car)
vif(lm_mec4)
# 10보다 작기 때문, 다중공선성이 없음


# 평균차이에 대한 검정 ####
var.test(mec22_c$c22, mec22_c$c19)
# 두집단간 분산은 차이가 있다
t.test(mec22_c$c22,mec22_c$c19, var.equal  = F)
# p-value = 1.061e-10으로 두집단의 평균의 차이가 있다.

var.test(mec22_c$c22, mec22_c$c20)
# 두집단간 분산은 차이가 있다
t.test(mec22_c$c22,mec22_c$c20, var.equal  = F)
# p-value = 1.061e-10으로 두집단의 평균의 차이가 있다.

var.test(mec22_c$c22, mec22_c$c21)
# 두집단간 분산은 차이가 없다
t.test(mec22_c$c22,mec22_c$c21, var.equal  = T)
# 등분산 T-test의 결과 p-value = 7.089e-08으로 0.05보다 작아 두집단의 평균의 차가 있다.
# 즉, 모두 p < 0.05 이기 때문에 영향/차이가 있다(연구가설 채택)


# 결론 및 해석 ####
# 2022년 전기차증가대수와 19~21년도 전기차증가대수는 영향이 있다
# t.test검정 결과 영향이 있다라는 결론이 도출되었고,
# t.test(x1,y1,paired = T,alternative = 'greater') 각 p-value = 2.488e-06,3.045e-06,4.842e-06로
# 19,20,21년도 전기차대수와 22년도 전기차 대수를 측정했더니 영향이 있다고 밝혀졌다.
# 19~21년도 전기차대수가 22년도 전기차대수보다 크므로 (mean difference값) 차이로 22년도 전기차는 계속해서 꾸준히 증가하고 있음을 알 수 있다


# 추정과예측 ####
predict(lm_mec)
# 2022년 1~12월예측값들
# 실제치와 비교했을 때 굉장히 유사함을 확인

# 7~12월 값을 대입하여 2022년 7~12월 전기차대수 예측비교 
count <- data.frame(c19=c(11764,12050,12389,12711,13831,14952),
                    c20=c(20003,20997,22118,22676,23052,23393),
                    c21=c(29325,31220,33434,35067,38840,40564))

predict(lm_mec,count, interval = "confidence")
predict(lm_mec,count,interval = "prediction")
# 2022년 7~12월 전기차 대수 실제치 = 49590,51240,53798,55335,57351,59327
# 예측 범위내에 실제치가 있기 때문에 동일함을 알 수 있음
################################################################################ 
# 2. 전기차 증가를 방해하는 요인 분석
# 1) 높은 차량 구매금액 

library(readxl)
# 서울시 전기차 등록 대수 ###
ele_car <- read_excel("../jang-yelim/mini pjt/data/지역별 총 전기차 대수.xlsx")
ele_car$cty<- factor(ele_car$cty)
names(ele_car) <- c("cty","2018년","2019년","2020년","2021년")

ele_car <- as.data.frame(ele_car)

# 서울시 작년 대비 전기차 증가율 
library(dplyr)

ele_car_growth <- ele_car %>% 
  mutate(`rate_car_19` = (`2019년` - `2018년`) / `2018년` * 100,
         `rate_car_20` = (`2020년` - `2019년`) / `2019년` * 100,
         `rate_car_21` = (`2021년` - `2020년`) / `2020년` * 100) %>% 
  filter(cty =="서울특별시") %>% 
  select(cty,rate_car_19,rate_car_20,rate_car_21) %>%
  group_by(cty)

ele_car_growth <- as.data.frame((ele_car_growth))

rate_car_19 <- ele_car_growth$rate_car_19
rate_car_20 <- ele_car_growth$rate_car_20
rate_car_21 <- ele_car_growth$rate_car_21

# 서울시 보조금 ######
#  작년 대비 보조금액
ele_fee <- read_excel("../jang-yelim/mini pjt/data/3지역별 보조금 .xlsx")
ele_fee <- ele_fee %>% filter(cty =="서울특별시")
ele_fee <- as.data.frame(ele_fee)

ele_fee <- ele_fee[,-6]
# View(ele_fee)

ele_fee <- data.frame(year = c(2019,2020,2021),
                      fee = c(1350,1270,1000))


# 매년 전기차와 보조금 증감의 그래프 ####
# 매년 전기차 증감율 ####
library(reshape2)
rate_car_df <- melt(ele_car_growth  , id.vars = "cty", variable.name = "year", value.name = "rate_car")
rate_car_df$year <- as.numeric(gsub("rate_car_",20, rate_car_df$year))
# View(rate_car_df)

ggplot(rate_car_df, aes(x = year, y = rate_car, color = cty)) +
  geom_line() +
  labs(title = "연도별 전기차 증가율", x = "연도", y = "전기차 증가율")+
  scale_x_continuous(breaks = c(2019,2020,2021))


# 매년 보조금 그래프####  
library(ggplot2)
ggplot(ele_fee, aes(x = year, y =fee)) +
  geom_line() +
  labs(title = "연도별 보조금 ", x = "연도", y = "보조금 ") +
  scale_x_continuous(breaks = c(2019,2020,2021)) 

# 결론
## 두 그래프를 통해 매년 보조금이 하락함에 따라 전기차 증감율도 하락 추세임을 확인할 수 있다.


##############################################################################################################3 

#########################  각 행정구별 완속과 고속 충전기의 비율 #########################
# 각 행정구별 충전기,급속,완속 수
s_ch <- read_excel("../jang-yelim/mini pjt/data/2023년 서울 행정구별 전기차 충전소.xlsx")
s_ch <- na.omit(s_ch)
s_ch <- as.data.frame(s_ch)


#### 01_급속 충전기 비율 #### 
library(tidyverse)
s_ch_f <- s_ch %>% 
  mutate(f_rate = (s_ch[,3]/s_ch[,2])*100) %>% select(`행정구`,f_rate) %>%   group_by(`행정구`)

s_ch_f <- as.data.frame(s_ch_f)


#### 02_완속 충전기 비율 #### 

s_ch_s <- s_ch %>% 
  mutate(s_rate = (s_ch[,4]/s_ch[,2])*100 )%>% select(`행정구`,s_rate) %>%   group_by(`행정구`)

s_ch_s <- as.data.frame(s_ch_s)


#### 03_급속과 완속 비율만 나타내기 #####

s_ch_combine <- left_join(s_ch_f, s_ch_s, by = "행정구")

View(s_ch_combine)

#### 04_1_급속비율이 가장 낮은 행정구역 #####
which.min(s_ch_combine$f_rate)
# 결론
## 급속비율이 가장 낮은 행정구역인 `동작구`는 3.646973의 설치 비율을 보인다.

#### 04_2_급속비율이 가장 높은 행정구역 #####
which.max(s_ch_combine$f_rate)
# 결론
## 급속비율이 가장 높은 행정구역인 `관악구`는 16.47059의 설치 비율을 보인다.


############ 04_3_급속 비율이 가장 낮은 행정구역과 높은 행정구역의 차이 ############
s_ch_df1 <- max(s_ch_combine$f_rate) - min(s_ch_combine$f_rate)
# 결론
## 급속비율이 가장 높은 행정구역인 관악구`와 가장 낮은 `동작구`의 차이는 12.82362대이다.



#### 05_1_완속비율이 가장 낮은 행정구역 #####
which.min(s_ch_combine$s_rate)
# 결론
## 완속비율이 가장 낮은 행정구역인 `관악구`는 83.52941의 설치 비율을 보인다.

#### 05_2_완속비율이 가장 높은 행정구역 #####
which.max(s_ch_combine$s_rate)
# 결론
## 완속비율이 가장 높은 행정구역인 `동작구`는 96.35303의 설치 비율을 보인다.


############ 05_3_완속 비율이 가장 낮은 행정구역과 높은 행정구역의 차이 ############
s_ch_df2 <- max(s_ch_combine$s_rate) - min(s_ch_combine$s_rate)
# 결론
## 완속비율이 가장 높은 행정구역인 `동작구`와 가장 낮은 `관악구`의 차이는 12.82362대이다.



######################### 각 행정구별 충전기 1대당 충전 가능한 전기차수 #########################

#### 01_각 행정구별 전기차 수 #### 
s_c<- read_excel("../jang-yelim/mini pjt/data/2023년 서울 행정구별 전기차 대수.xlsx")
s_c <- as.data.frame(s_c)

####  02_각 행정구별 충전기 수 #### 
s_ch_count <- s_ch[,1:2]

#### 03_ 각 행정구별 충전기 1대당 충전 가능한 전기차수 #### 
s <- left_join(s_c, s_ch_count, by = "행정구")

one_ch_c <- s %>% 
  mutate(s_ch_rate = s[,2]/s[,3]) %>% select(`행정구`,s_ch_rate) %>% group_by(`행정구`)

one_ch_c <- as.data.frame(one_ch_c)

View(one_ch_c)
sort(one_ch_c$s_ch_rate,decreasing = T)
sort(s_ch_combine$f_rate,decreasing = F)
sort(s_c$전기차수,decreasing = F) 

# 결론_01
## 충전기 1대당 충전할 수 있는 전기차 수를 구해보았더니 다음과 같은 결과가 나타났다.
## 강남구 4.5479896, 강동구 1.4539052, 강북구 1.2500000, 강서구 1.2459800, 관악구 1.5941176, 광진구 1.4215426,
## 구로구 2.7792138, 도봉구 1.2037445, 서대문구 1.0853333, 서초구 1.7670877, 송파구 1.1492275, 양천구 1.4252223,
## 영등포구 1.3460827, 용산구 1.0273738, 종로구 1.0695755, 중구 1.7918401의 경우 1이 이상의 값이 나왔다.
## 이는 충전기 1대당 1대 이상을 충전해야 하는 상황으로 충전기의 부족을 의미한다. 
## 특히 `강남구`의 경우 충전기 1대당 4대 이상을 충전해야 하는 4.547989 값을 갖고 있지만,
## 급속충전기 비율을 내림차순으로 정렬시 7.619974로 서울의 25번째 행정구역에서 11번째로
## 상대적으로 낮아 급속 충전기 부족 문제가 심각하다고 볼 수 있다.
## 따라서 위와 같은 수치에 근거하여 강남구에 가장 먼저 고속 충전기를 추가로 설치해야 한다.



# 결론_02
## `구로구`는 충전기 1대당 충전 가능한 전기차수가 2.7792138로 강남구 다음으로 높은 행정구이다.
## 하지만 충전기 1대당 충전 가능한 전기차수가 1이상인 지역과 강남구를 제외하면 급속 충전기 설치 비율이 5.438880로 가장 낮다.
## 따라서 강남구 다음으로 `구로구`에 고속 충전기를 추가로 설치해야 한다.



# 결론_03
## 특이지역은 `관악구`이다.
## `관악구`는 충전기 1대당 충전 가능한 전기차수가 1.5941176로 5번째 높은 행정구이다.
## 하지만 2023년 기준 전기차 수를 내림차순으로 정렬시 1355로 서울시에서 10번째로 상대적으로 전기차수가 낮으며,
## 행정구별 고속 충전기 비율이 가장 높은 16.470588 값을 가진다.
## 이에 대해 인구유동이나, 주변 시설과 같은 다양한 변수와 관련이 있어 보이지만, 이번 프로젝트에서는 밝히지 못했다.




```

