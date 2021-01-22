rm(list = ls())
library(tidyverse)
#파일 불러오기
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/데이터")
load("dataset_set.rda")
load("dataset_set_outlier.rda")


#라벨인코딩
function_path = "C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/코드/"
source(file = paste0(function_path,"function.r"))
# vars <- c(3,4,5)
# for(i in vars){
#   trainset <- label_encoding(dataframe = trainset,column_num = i)
#   testset <- label_encoding(dataframe = testset,column_num = i)
# }
#입력변수 설정
colnames(trainset)
vars <- c("년도","분기","행정구역","대분류","중분류","매출비율_0614","매출건수비율_0614","매출총액")


trainset_lm <- trainset[,vars]
lapply(trainset_lm,function(x){sum(is.na(x))})
summary(trainset_lm)

#다중선형회귀분석 모델
#Multiple Linear Regression Model
#모델 적합
fit1_lm <- lm(formula = 매출총액~.,data = trainset_lm) #목표변수와 상관관계 있는 입력변수와 보류인 입력변수
null <- lm(formula = 매출총액~1.,data = trainset_lm)
full <- lm(formula = 매출총액~.,data = trainset_lm)
fit1_lm2 <- step(object = null,
                     scope = list(lower = null, upper = full),
                     direction = "both") #stepwise를 통한 단계적 변수 선택

trainset2_lm <- trainset_lm %>% select(-c(중분류))
fit2_lm <- lm(formula = 매출총액~.,data = trainset2_lm)


summary(fit1_lm)
#중분류 종합소매, 주점업 더미 NA 발생 => 특정 중분류 변수의 합으로 종합소매 및 주점업을 설명 가능
summary(fit1_lm2)
summary(fit2_lm)


#다중공산성 인자 확인
library(car)
vif(mod = fit1_lm2) # GVIF^(1/(2*Df)) > 2 => 매출_월화수목/금토일/0614/1421
vif(mod = fit2_lm) # GVIF^(1/(2*Df)) > 2 => 매출_월화수목/금토일/0614/1421


#다중공산성 인자 제거
vif_test <- function(dataset,stepwise_uages){
  library(car)
  name_list <- c()
  name_list_backup <- c()
  repeat{
    if(stepwise_uages == 0){
      model <- lm(formula = 매출총액~., data = dataset)
    } else {
      null <- lm(formula = 매출총액~1.,data = dataset)
      full <- lm(formula = 매출총액~.,data = dataset)
      model <- step(object = null,
                           scope = list(lower = null, upper = full),
                           direction = "both")
    }
    if(length(vif(mod = model))>4){
      vif_list <- vif(mod = model)[,3]
    } else {
      vif_list = vif(mod = model)
    }
    
    name <- names(which.max(vif_list[vif_list>2]))
    name_list <- c(name_list,name)
    if(length(name_list) == length(name_list_backup)) break
    name_list_backup <- name_list
    dataset <- dataset %>% select(-c(name))
  }
  data_list = list(model,name_list)
  return(data_list)
}
fit1_lm2_list <- vif_test(dataset = trainset_lm,stepwise_uages = 1)
fit2_lm_list <- vif_test(dataset = trainset2_lm,stepwise_uages = 0)

#다중공산성 인자 확인
fit1_lm2_list[2] #매출건수_0614 제거
fit2_lm_list[2] #매출건수_0614 제거

#다중공산성 컬럼을 제거한 후 적합한 모델
fit1_lm2_SP <- fit1_lm2_list[[1]]
fit2_lm_SP <- fit2_lm_list[[1]]

#결과재확인
summary(object = fit1_lm2_SP)
summary(object = fit2_lm_SP)

#다중공산성 문제 확인
vif(mod = fit1_lm2_SP)
vif(mod = fit2_lm_SP)

#변수 소거 전후 모델 비교평가
anova(fit1_lm2, fit1_lm2_SP) # p-value 0.05 이하로 변수소거 전후 성능 차이 있음을 확인
anova(fit2_lm, fit2_lm_SP) # p-value 0.05 이하로 변수소거 전후 성능 차이 있음을 확인


#잔차가정 검정 bonferroni p 0.05이하 제거
outliers <- function(model, dataset,stepwise){
  repeat{
    outliers <- outlierTest(model = model)
    outliers <- as.integer(names(outliers$bonf.p[outliers$bonf.p<0.05]))
    if(length(outliers)==0) break
    dataset <- dataset %>% slice(-outliers)
    if(stepwise == 0){
      model <- lm(formula = 매출총액~., data = dataset)
    } else {
      null <- lm(formula = 매출총액~1, data = dataset)
      full <- lm(formula = 매출총액~., data = dataset)
      model <- step(object = null,scope = list(lower = null, upper = full),direction = "both")
    }
  }
  return(model)
}
fit1_lm2_SP <- outliers(model = fit1_lm2_SP,dataset = trainset_lm, stepwise = 0)
fit2_lm_SP <- outliers(model = fit2_lm_SP,dataset = trainset2_lm, stepwise = 0)


#잔차 패턴 확인
windows()
par(mfrow = c(2,2))
plot(x = fit1_lm2_SP)
plot(x = fit2_lm_SP)
par(mfrow = c(1,1))

#잔차가정 검정
library(car)
ncvTest(model = fit1_lm2_SP)
durbinWatsonTest(model = fit2_lm_SP) 
crPlots(model = fit2_lm_SP)
influencePlot(model = fit2_lm_SP)
# fit_full_new_af - 애매하거나 확실한 입력변수 기준
# fit_selected_af - 확실한 입력변수 기준
# fit_stepwise_af - 전체 데이터(확정변수+비교용변수)에서 변수소거법 적용

# 다중공산성 문제를 일으키는 인자 제거
# 이상치 제거 => bonfferoni p 0.05 이하인 index 제거

# 최종 확인결과, 세 경우 모두 잔차의 목표변수가 정규성을 위배하여 이후 순서 진행불가

#######
library(olsrr)
olsrr::ols_plot_cooksd_bar(model = fit1_lm2_SP) #cook 거리 바플랏
#해당 관측값이 전체 최소제곱추정량에 미치는 영향력을 보여주는 지표
ols_plot_dfbetas(model = fit1_lm2_SP) #해당 관측치의 개별 베타 값에 대한 영향력 지표
ols_plot_dffits(model = fit1_lm2_SP) #베타값의 분상 공분상 행렬의 Cov(b^) 추정값에 대한 해당 관측치에 대한 영향력

#5000개 이상 데이터 정규성 확인 => 앤더슨 달링 테스트
library(nortest)
ad.test(fit1_lm2_SP$residuals)
ad.test(fit1_lm2_SP$residuals)
ad.test(fit1_lm2_SP$residuals)


#-------------------------------------------------------------------------------------------------------
#회귀나무로 모델 만들기
rm(list = ls())
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/데이터")
load("dataset_set.rda")
load("dataset_set_outlier.rda")

#라벨인코딩
function_path = "C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/코드/"
source(file = paste0(function_path,"function.r"))
# vars <- c(3,4,5)
# for(i in vars){
#   trainset <- label_encoding(dataframe = trainset,column_num = i)
#   testset <- label_encoding(dataframe = testset,column_num = i)
# }

library(tidyverse)
library(rpart)
library(rpart.plot)

#입력변수 설정
#최종 입력변수
#확정 : 매출_월화수목,매출_금토일,매출_0614,매출_1421,대분류
#비교 : 매출_2106, 중분류,년도, 분기,행정구역,년분기
vars <- c("년도","분기","행정구역","대분류","중분류","매출비율_0614","매출건수비율_0614","매출총액")
vars <- c("년도","분기","행정구역","대분류","중분류","매출비율_0614","매출건수비율_0614","매출총액","생존률_1년차","생존률_3년차","생존률_5년차")

trainset_lm <- trainset[,vars]

#회귀나무 정지요인 설정
ctrl <- rpart.control(minsplit = 5L,
                      cp = 0.001,
                      maxdepth = 30L)

#회귀나무모델 적합
set.seed(seed = 1234)
fit1 <- rpart(formula = 매출총액~.,
              data = trainset[,vars],
              control = ctrl)

#결과 확인 및 가지치기 여부 확인
summary(object = fit1)
plotcp(x = fit1)
nrow(fit1$cptable)
#가치치기 불필요 확인
which.min(fit1$cptable[,4]) #가지치기 필요

fit2 <- prune.rpart(tree = fit1,cp = 0.001)

#성능 분석
real <- testset$매출총액
pred1 <- predict(object = fit1, newdata = testset, type = "vector")
pred2 <- predict(object = fit2, newdata = testset, type = "vector")

testset$매출총액_pred1 <- pred1
testset$매출총액_pred2 <- pred2
result <- testset %>% select(행정구역,대분류,중분류,매출총액,매출총액_pred1,매출총액_pred2) %>% 
  group_by(행정구역,대분류) %>% 
  mutate(rank_real = row_number(desc(매출총액)),
         rank_pred1 = row_number(desc(매출총액_pred1)),
         rank_pred2 = row_number(desc(매출총액_pred2)),
         top_real = ifelse(rank_real <=3,"1","0"),
         top_pred1 = ifelse(rank_pred1 <=3,"1","0"),
         top_pred2 = ifelse(rank_pred2 <=3,"1","0"))

result[,10:12] <- map_df(.x = result[,10:12],.f = as.factor)
result %>% ggplot(aes(x = rank_real, y = rank_pred1, color = as.factor(rank_real)))+
  geom_point(position = position_jitter(),size = 2)
str(result)
table(result$top_pred1==result$top_real)
library(caret)
confusionMatrix(data = result$top_pred1, reference = result$top_real, positive = "1")
confusionMatrix(data = result$top_pred2, reference = result$top_real, positive = "1")
library(MLmetrics)
F1_Score(y_true = result$top_real, y_pred = result$top_pred1, positive = "1")
F1_Score(y_true = result$top_real, y_pred = result$top_pred2, positive = "1")

library(MLmetrics)
regMeasure(real = real, pred = pred1)
regMeasure(real = real, pred = pred2)
R2_Score(y_pred = pred1,y_true = real)
R2_Score(y_pred = pred2,y_true = real)

# saveRDS(object = fit1, file = "RegressionTree.rds")
# saveRDS(object = fit2, file = "RegressionTree_prune.rds")
saveRDS(object = fit1, file = "RegressionTree_outlier.rds") #아웃라이어 제거
saveRDS(object = fit2, file = "RegressionTree_prune_outlier.rds") #아웃라이어 제거
fit1 <- readRDS("RegressionTree_outlier.rds")
fit2 <- readRDS("RegressionTree_prune_outlier.rds")
#-------------------------------------------------------------------------------------------------------
rm(list = ls())
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/데이터")
# load("dataset_set.rda")
load("dataset_set_outlier.rda") #아웃라이어 제거

#라벨인코딩
function_path = "C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/코드/"
source(file = paste0(function_path,"function.r"))
# vars <- c(3,4,5)
# for(i in vars){
#   trainset <- label_encoding(dataframe = trainset,column_num = i)
#   testset <- label_encoding(dataframe = testset,column_num = i)
# }
library(tidyverse)
library(randomForest)

#입력변수 설정
#최종 입력변수
#확정 : 매출_월화수목,매출_금토일,매출_0614,매출_1421,대분류
#비교 : 매출_2106, 중분류,년도, 분기,행정구역,년분기
colnames(trainset)
vars <- c("년도","분기","행정구역","대분류","중분류","매출비율_0614","매출건수비율_0614","매출총액")


#반복문을 사용한 모형 튜닝
grid <- expand.grid(ntree = seq(from = 700, to = 1000, by = 100),
                    mtry = 7,
                    error = NA)
n = nrow(x = grid)
for(i in 1:n){
  disp <- str_glue('현재 {i}행 실행 중! [ntree: {grid$ntree[i]}, mtry: {grid$mtry[i]}]  {Sys.time()}')
  cat(disp,"\n")
  set.seed(seed = 1234)
  fit <- randomForest(formula = 매출총액~.,
                      data = trainset[,vars],
                      ntree = grid$ntree[i],
                      mtry = grid$mtry[i])
  grid$error[i] <- tail(x = fit$mse, n = 1)
}
windows()
#튜닝 결과 확인
plot(x = grid$error, type = 'b', pch = 19, col = 'gray30', main = 'Grid Search Result')
abline(h = min(x = grid$error), col = 'red', lty = 2)
loc <- which.min(x = grid$error)
print(x = loc)

#OOB 오차가 최소인 하이퍼파라미터 지정
bestPara <- grid[loc,] #ntree 1300, mtry 3, mse 0.0011497

#최적 하이퍼파라미터로 모형 적합
set.seed(seed = 1234)
fit1 <- randomForest(formula = 매출총액~.,
                     data = trainset[,vars],
                     ntree = bestPara$ntree,
                     mtry = bestPara$mtry,
                     # data = trainset[,vars],
                     # ntree = 1300,
                     # mtry = 7,
                     importance = T,
                     do.trace = 50,
                     keep.forest = T)

#결과 확인
plot(x = fit1, main = "best Fit")
importance(x = fit1)
varImpPlot(x = fit1, main = 'variable importance', type = 1 )

#시험셋으로 목표변수 추정값 생성
pred1 <- predict(object = fit1, newdata = testset[,vars], type = 'response')

#실제 관측치 벡터 생성
real <- testset$매출총액

#모형 성능 확인
regMeasure(real = real, pred = pred1)
R2_Score(y_true = real, y_pred = pred1)
plot(x = real, y = pred1, type = "p")
# options(scipen = 100)
#trainset에 rank_real과 rank_pred 생성
testset$매출총액_pred <- pred1
result <- testset %>% select(행정구역,대분류,중분류,매출총액,매출총액_pred) %>% 
  group_by(행정구역,대분류) %>% 
  mutate(rank_real = row_number(desc(매출총액)),
         rank_pred = row_number(desc(매출총액_pred)),
         top_real = ifelse(rank_real <=3,"1","0"),
         top_pred = ifelse(rank_pred <=3,"1","0")) %>% 
  arrange(행정구역,대분류,rank_real,rank_pred)
colnames(result)
result[,8:9] <- map_df(.x = result[,8:9],.f = as.factor)
  ggplot(aes(x = rank_real, y = rank_pred, color = as.factor(rank_real)))+
  geom_point(position = position_jitter(),size = 2)
str(result)
table(result$top_pred==result$top_real)
library(caret)
confusionMatrix(data = result$top_pred, reference = result$top_real, positive = "1")
F1_Score(y_true = result$top_real, y_pred = result$top_pred, positive = "1")
saveRDS(object = fit1, file = "randomforest.rds")