setwd('C:/Users/82107.DESKTOP-NQ18D8E/Desktop/융합PJT프로젝트/# PJT1')
getwd()
# 독립변수(X):  19,20,21년도 월별 전기차대수
# 종속변수(Y):  22년도 월별 전기차대수
library(readxl)
mec22_c <- read_excel('../# PJT1/4개년월별전기차대수.xlsx')
names(mec22_c) <- c('c19','c20','c21','c22')
View(mec22_c)
mec22_c <- as.data.frame(mec22_c)
str(mec22_c)
summary(mec22_c)


# 상관관계 분석 ####
# H0: 22년도 전기차대수는 19,20,21년도 전기차대수와 영향이 없다
# H1: 22년도 전기차대수는 19,20,21년도 전기차대수와 영향이 있다
cor(mec22_c)
plot(mec22_c)
library(psych)
pairs.panels(mec22_c)  
# 매우 높은 양의 상관관계 있음을 알수있다


# 관계 시각화 그래프 ####
library(corrplot)
corrplot(cor(mec22_c),method='ellipse') 
library(PerformanceAnalytics)
chart.Correlation(mec22_c)
plot(c22 ~., mec22_c, type='l',main="22년 기준 매해 전기차대수 증가추세")
boxplot(mec22_c$c19,mec22_c$c20,mec22_c$c21,mec22_c$c22)


# 회귀분석 ####
lm_mec <- lm(c22 ~., mec22_c)
summary(lm_mec)
# 설명력= 99% (Adjusted R-squared:  0.9944)
# H0기각, H1채택 (p-value: 6.537e-10)
# 각 각의 변수에 대해서는 유의미 하지 않았지만, 조정된 결정의 계수로는 유의미한 결과가 나왔다.

plot(c22 ~ ., mec22_c)
plot(lm_mec)
qqnorm(lm_mec$residuals)
qqline(lm_mec$residuals,lty=1,col='red')
# 잔차의분포와 정규분포는 비슷함을 알 수 있음
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

# 위 4가지를 한꺼번에
par(mfrow=c(2,2))
plot(lm_mec)
# 마지막 그래프는 다름(cook's distance)
# 아웃라이어를 제시 및 전체적인 데이터/잔차가 편중되어있음을 보여줌

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
plot(lm_mec4, 2)
plot(lm_mec4, 4)
summary(lm_mec4)

# 잔차 내림차순 정렬
sort(lm_mec4$residuals, decreasing = T)
# 그 다음 큰 잔차 찾기
which.max(lm_mec4$residuals)

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
# [결론/해석] 2022년 전기차증가대수와 19~21년도 전기차증가대수는 영향이 있다
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
 
