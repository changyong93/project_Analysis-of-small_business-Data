rm(list = ls())
library(tidyverse)
#파일 불러오기
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/2. 데이터")
load("dataset_set.rda")

#다중선형회귀분석 모델
#모델 적합
vars10_20 <- c(vars_10up[(vars_10up %in% vars_20up)==F])
loc10_20 <- which(colnames(trainset) %in% vars10_20)

#상관계수가 0.2미만 제거한 데이터셋 생성
trainset2 <- trainset[,-loc10_20] 

#모델 적합 1차
fit1 <- lm(formula = 매출총액~.,data = trainset) #모든 입력변수
fit2 <- lm(formula = 매출총액~.,data = trainset2) #상관계수가 0.2미만 제거
summary(fit1)
summary(fit2)

#1차 적합 시 회귀계수가 NA인 feature 제거
vars <- c("중분류")
loc1 <- which(colnames(trainset) %in% vars)
loc2 <- which(colnames(trainset2) %in% vars)

#P-value가 NA 컬럼 제거 후 재적합
trainset1_2 <- trainset[,-loc1] ; trainset2_2 <- trainset2[,-loc2]
fit1_2 <- lm(formula = 매출총액~., data = trainset1_2)
fit2_2 <- lm(formula = 매출총액~., data = trainset2_2)
summary(fit1_2)
summary(fit2_2)

#변수소거법을 통한 모델 적합
null <- lm(formula = 매출총액~1.,data = trainset)
full <- lm(formula = 매출총액~.,data = trainset)
fit11 <- step(object = null,
             scope = list(lower = null, upper = full),
             direction = "both") #stepwise를 통한 단계적 변수 선택

null <- lm(formula = 매출총액~1.,data = trainset2)
full <- lm(formula = 매출총액~.,data = trainset2)
fit22 <- step(object = null,
             scope = list(lower = null, upper = full),
             direction = "both") #stepwise를 통한 단계적 변수 선택
summary(fit11)
summary(fit22)

#다중공산성 및 아웃라이어 처리
vif_outlier_test <- function(dataset,stepwise_uages){
  library(car)
  dataset_backup <- dataset
 repeat{
   dataset <- dataset_backup
   name_list <- c()
   name_list_backup <- c()
   repeat{
     if(stepwise_uages ==0){
       model = lm(formula = 매출총액~., data = dataset)
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
     data_list = list(model,name_list)
     if(length(name_list) == length(name_list_backup)) break
     name_list_backup <- name_list
     loc <- which(colnames(dataset) %in% name_list)
     dataset <- dataset[-loc]
   }
   outliers <- outlierTest(model = model)
   outliers <- as.integer(names(outliers$bonf.p[outliers$bonf.p<0.05]))
   if(length(outliers)==0) break
   dataset_backup <- dataset_backup %>% slice(-outliers)
 }
  return(data_list)
}
fit1_2_list <- vif_outlier_test(dataset = trainset1_2,stepwise_uages = 0)
fit11_list <- vif_outlier_test(dataset = trainset,stepwise_uages = 1)
fit2_2_list <- vif_outlier_test(dataset = trainset2_2,stepwise_uages = 0)
fit22_list <- vif_outlier_test(dataset = trainset2,stepwise_uages = 1)

#다중공산성 변수 확인
fit1_2_list[2] ; fit11_list[2] 
fit2_2_list[2] ; fit22_list[2]

#다중공산성 컬럼을 제거한 후 적합한 모델
fit1_2 <- fit1_2_list[[1]] ; fit11 <- fit11_list[[1]]
fit2_2 <- fit2_2_list[[1]] ; fit22 <- fit22_list[[1]]

#결과재확인
summary(object = fit1_2)
summary(object = fit11)
summary(object = fit2_2)
summary(object = fit22)

#다중공산성 변수 재확인
vif(mod = fit1_2)
vif(mod = fit11)
vif(mod = fit2_2)
vif(mod = fit22)
 
#잔차 패턴 확인
# windows()
# par(mfrow = c(2,2))
# plot(x = fit1_2)
# plot(x = fit11)
# plot(x = fit2_2)
# plot(x = fit22)
# par(mfrow = c(1,1))

# #잔차가정 검정
# library(car)
# ncvTest(model = fit1_2)
# durbinWatsonTest(model = fit1_2) 
# crPlots(model = fit1_2)
# influencePlot(model = fit1_2)
# 
# ncvTest(model = fit2_2)
# durbinWatsonTest(model = fit2_2) 
# crPlots(model = fit2_2)
# influencePlot(model = fit2_2)
# 
# ncvTest(model = fit2_2)
# durbinWatsonTest(model = fit2_2) 
# crPlots(model = fit2_2)
# influencePlot(model = fit2_2)
# 
# ncvTest(model = fit2_2)
# durbinWatsonTest(model = fit2_2) 
# crPlots(model = fit2_2)
# influencePlot(model = fit2_2)

# 성능 확인
real <- testset$매출총액

performance <- function(model){
  #결과 확인
  pred <- predict(object = model, newdata = testset, type = "response")
  
  #연속형 결과 확인
  rmse <- MLmetrics::RMSE(y_pred = pred, y_true = real)
  r2 <- MLmetrics::R2_Score(y_pred = pred, y_true = real)
  
  #rank(범주형) 결과 확인
  testset$매출총액_pred <- pred
  dataset <- testset %>% group_by(행정구역,대분류) %>%
    mutate(rank_real = row_number(desc(매출총액)),
           rank_pred = row_number(desc(매출총액_pred)),
           top3_real = ifelse(rank_real %in% 1:3, 1,0),
           top3_pred = ifelse(rank_pred %in% 1:3, 1,0))
  f1 <- MLmetrics::F1_Score(y_true = dataset$top3_real, y_pred = dataset$top3_pred, positive = "1")
  
  data = list(pred = pred, rmse = rmse, r2 = r2, f1 = f1, model = model)
  print(data[2:4])
  return(data)
}

#모델 성능 확인
result1_2 <- performance(model = fit1_2)
result11 <- performance(model = fit11)
result2_2 <- performance(model = fit1_2)
result22 <- performance(model = fit22)

#성능이 가장 좋은 fit11 모델 사용 ==> NA를 제외한 모든 입력변수로 시작 후 다중공산성 변수 및 아웃라이어 제거한 모형
testset$매출총액_pred <- result11$pred
windows()
testset %>% group_by(행정구역,대분류) %>%
  mutate(rank_real = row_number(desc(매출총액)),
         rank_pred = row_number(desc(매출총액_pred)),
         top3_real = ifelse(rank_real %in% 1:3, 1,0),
         top3_rank = ifelse(rank_pred %in% 1:3, 1,0)) %>% 
  ggplot(aes(x = rank_real, y = rank_pred, color = as.factor(rank_real)))+geom_point(position = position_jitter())
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/2. 데이터")
save(result1_2,result11,result2_2,result22, file = "linearRegression.rda")
# 최종 확인결과, 세 경우 모두 잔차의 목표변수가 정규성을 위배하여 이후 순서 진행불가

#######
# library(olsrr)
# olsrr::ols_plot_cooksd_bar(model = fit2_2) #cook 거리 바플랏
# #해당 관측값이 전체 최소제곱추정량에 미치는 영향력을 보여주는 지표
# ols_plot_dfbetas(model = fit2_2) #해당 관측치의 개별 베타 값에 대한 영향력 지표
# ols_plot_dffits(model = fit2_2) #베타값의 분상 공분상 행렬의 Cov(b^) 추정값에 대한 해당 관측치에 대한 영향력
# 
# #5000개 이상 데이터 정규성 확인 => 앤더슨 달링 테스트
# library(nortest)
# ad.test(fit1_lm2_SP$residuals)
# ad.test(fit1_lm2_SP$residuals)
# ad.test(fit1_lm2_SP$residuals)


#-------------------------------------------------------------------------------------------------------
#회귀나무로 모델 만들기
library(tidyverse)
library(rpart)
library(rpart.plot)
library(MLmetrics)
rm(list = ls())
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/2. 데이터")
load("dataset_set.rda")
trainset_dummy <- trainset
testset_dummy <- testset

#1차
trainset <- trainset_dummy
testset <- testset_dummy
grid <- expand.grid(   
  minsplit = seq(from = 2, to = 20, by = 1),
  cp = seq(from = 0.0001, to = 0.001, length.out = 10),
  seed = 1234,
  RMSE = NA,
  F1 = NA,
  R2 = NA)
filename <-  "1차"
grid_filename <- "grid1"
pred_filename <- "pred1"

#2차 minsplit만 20~40
trainset <- trainset_dummy
testset <- testset_dummy

grid <- expand.grid(   
  minsplit = seq(from = 20, to = 40, by = 1),
  cp = 0.0001,
  seed = 1234,
  RMSE = NA,
  F1 = NA,
  R2 = NA)
filename = "2차"
grid_filename <- "grid2"
pred_filename <- "pred2"

#3차 = 변수중요도가 1000부근 및 미만인 변수 제거
vars <- c("행정구역",'매출비율_1724','생존율_3년차','생존율_1년차','소득분위','생존율_5년차','년도','분기')
loc <- which(colnames(trainset_dummy) %in% vars)
trainset <- trainset_dummy[,-loc]
testset <- testset_dummy[,-loc]

grid <- expand.grid(   
  minsplit = seq(from = 2, to = 20, by = 1),
  cp = seq(from = 0.0001, to = 0.001, length.out = 10),
  seed = 1234,
  RMSE = NA,
  F1 = NA,
  R2 = NA)
filename = "3차"
grid_filename <- "grid3"
pred_filename <- "pred3"

pred_list <- c()
#모델 튜닝 진행
for(i in 1:nrow(grid)){
  sentence <- str_glue('{i}번째 행 실행 중 [minsplit : {grid$minsplit[i]}, cp = {grid$cp[i]}')
  print(sentence,"\n")
  #정지규칙 설정
  ctrl <- rpart.control(minsplit = grid$minsplit[i],
                        cp = grid$cp[i],
                        maxdepth = 30L)
  
  #모델적합
  set.seed(seed = grid$seed[i])
    fit <- rpart(formula = 매출총액~.,
               data = trainset,
               control = ctrl)
    #가지치기 여부 확인 후 적합
    num1 <- nrow(fit$cptable)
    num2 <- which.min(fit$cptable[,4])
    if(num1 != num2){
      fit2 <- prune.rpart(tree = fit, cp = grid$cp[num2])
    } else {
      fit2 = fit
    }
    
    #변수중요도 확인
    setwd(paste0("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/4.모델 적합/회귀나무/",filename))
    png(filename = paste0("변수중요도_",i,".png"), width = 8000, height = 4000, res = 500)
    plot(x = fit$variable.importance, type = "b")
    text(x = fit$variable.importance+100, label = paste0(names(fit$variable.importance),"\n",round(fit$variable.importance,0)))
    dev.off()
    
    #성능 분석
    real <- testset$매출총액
    pred <- predict(object = fit2, newdata = testset, type = "vector")
    
    #RMSE 계산
    reg <- MLmetrics::RMSE(y_pred = pred, y_true = real)
    R2 <- MLmetrics::R2_Score(y_pred = pred, y_true = real)
    
    #실측값 및 예측값 Rank 
    result <- testset_dummy
    result$매출총액_pred <- pred
    result <- result %>% select(행정구역,대분류,중분류,매출총액,매출총액_pred) %>% 
      group_by(행정구역,대분류) %>% 
      mutate(rank_real = row_number(desc(매출총액)),
             rank_pred = row_number(desc(매출총액_pred)),
             top_real = ifelse(rank_real <=3,"1","0"),
             top_pred = ifelse(rank_pred <=3,"1","0"))
    
    #Rank Top3 산점도 그리기
    result %>% 
      ggplot(aes(x = rank_real, y = rank_pred, color = as.factor(대분류)))+
      geom_point(position = position_jitter(),size = 2, alpha = 0.7)+
      ggsave(filename = paste0("rank산점도_",i,"_(대분류).png"), width = 24, height = 12, units = "cm")
    result %>% 
      ggplot(aes(x = rank_real, y = rank_pred, color = as.factor(rank_real)))+
      geom_point(position = position_jitter(),size = 2, alpha = 0.7)+
      ggsave(filename = paste0("rank산점도_",i,"_(rank).png"), width = 24, height = 12, units = "cm")
    #Top3 예측 성능
    F1 <- F1_Score(y_true = result$top_real, y_pred = result$top_pred, positive = "1")
    
    #grid라는 dataframe에 RMSE 및 F1_Score, R2_score 저장
    grid$RMSE[i] <-reg
    grid$F1[i] <- F1
    grid$R2[i] <- R2
    pred_list <- cbind(pred_list,pred)
    write.csv(grid,file = paste0(grid_filename,".csv"))
    write.csv(pred_list,file = paste0(pred_filename,"_list.csv"))
    
    cat(str_glue('{round((i)*100/nrow(grid),2)}% 완료'))
}
# R2, F1, CP 선 그래프 그리기
windows()
text <- data.frame(x = rep(nrow(grid)+1,3),
                   y = as.numeric(grid[nrow(grid),(ncol(grid)-2):ncol(grid)]),
                   label = colnames(grid)[(ncol(grid)-2):ncol(grid)])

grid %>% mutate(order = row_number()) %>% 
  ggplot(aes(x = order, y = RMSE))+geom_line(col = "blue")+geom_point(col = "blue")+ylab("")+
  geom_vline(xintercept = which.min(grid$RMSE), col = "blue", lty = 1, lwd = 2, alpha = 0.7)+
  geom_line(aes(y = F1), col = "red")+geom_point(aes(y = F1),col = "red")+
  geom_vline(xintercept = which.max(grid$F1), col = "red", lty = 6, lwd = 1.75)+
  geom_line(aes(y = R2), col = "orange")+geom_point(aes(y = R2), col = "orange")+
  geom_vline(xintercept = which.max(grid$R2), col = "orange", lty = 2, lwd = 1.2)+
  scale_y_continuous(sec.axis = dup_axis(), breaks = seq(0,1,0.05))+
  geom_text(data = text, mapping = aes(x = text$x, y = text$y, label = text$label),col = c("blue","red","orange"), size = 10)+
  theme_classic()

#RMSE가 가장 낮은 경우, F1이 가장 높은 경우, R2가 가장 높은 경우 세 가지를 선택하고, random set.seed로 가장 성능 좋은 모형 찾기
# grDevices::colors()
RMSE <- which.min(grid$RMSE)
F1 <- which.max(grid$F1)
R2 <- which.max(grid$R2)
cat(RMSE,F1,R2)
#-------------------------------------------------------------------------------------------------------
rm(list = ls())
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/2. 데이터")
load("dataset_set.rda")

#입력변수를 변경하며 모델 생성을 위해 데이터셋 더미 만들어놓기기
trainset_dummy <- trainset
testset_dummy <- testset

library(tidyverse)
library(randomForest)

#반복문을 사용한 모형 튜닝
#1차 #최적 mtry 찾기
trainset <- trainset_dummy
testset <- testset_dummy
grid <- expand.grid(ntree = 200,
                    mtry = 3:16,
                    seed = 1234,
                    error = NA,
                    RMSE = NA,
                    F1 = NA,
                    R2 = NA) 
filename = "1차"
grid_filename <- "grid1"
pred_filename <- "pred1"


#2차 error & F1, R2, RMSE가 높았던 mtry 6,9,12,13에서 ntree 변경하여 튜닝
trainset <- trainset_dummy
testset <- testset_dummy
grid <- expand.grid(ntree = seq(from = 200, to = 500, by = 100),
                    mtry = c(9,12,14),
                    seed = 1234,
                    error = NA,
                    RMSE = NA,
                    F1 = NA,
                    R2 = NA) 
filename = "2차"
grid_filename <- "grid2"
pred_filename <- "pred2"

#3차 error & F1, R2, RMSE가 높았던 mtry 6,9,12,13에서 ntree 변경하여 튜닝
trainset <- trainset_dummy
testset <- testset_dummy
grid <- expand.grid(ntree = seq(from = 500, to = 1000, by = 100),
                    mtry = c(9),
                    seed = 1234,
                    error = NA,
                    RMSE = NA,
                    F1 = NA,
                    R2 = NA) 
filename = "3차"
grid_filename <- "grid3"
pred_filename <- "pred3"

#4차 변수중요도 상위 5개만 선택하여 튜닝
vars <- c("중분류","총매출건수","행정구역","매출비율_0611","매출비율_토104050대","매출총액")
loc <- which(colnames(trainset_dummy) %in% vars)
trainset <- trainset_dummy[,loc]
testset <- testset_dummy[,loc]
grid <- expand.grid(ntree = seq(200,700,100),
                    mtry = 2:5,
                    seed = 1234,
                    error = NA,
                    RMSE = NA,
                    F1 = NA,
                    R2 = NA) 
filename = "4차"
grid_filename <- "grid4"
pred_filename <- "pred4"

#4차 변수중요도 상위 5개만 선택하여 튜닝(최종 튜닝 조건으로 실행)
vars <- c("중분류","총매출건수","행정구역","매출비율_0611","매출비율_토104050대","매출총액")
loc <- which(colnames(trainset_dummy) %in% vars)
trainset <- trainset_dummy[,loc]
testset <- testset_dummy[,loc]
grid <- expand.grid(ntree = 700,
                    mtry = 5,
                    seed = 1234,
                    error = NA,
                    RMSE = NA,
                    F1 = NA,
                    R2 = NA) 
filename = "5차"
grid_filename <- "grid5"
pred_filename <- "pred5"


pred_list = c()
for(i in 1:nrow(grid)){
  disp <- str_glue('현재 {i}행 실행 중! [ntree: {grid$ntree[i]}, mtry: {grid$mtry[i]}]  {Sys.time()}')
  cat(disp,"\n")
  
  set.seed(seed = grid$seed)
  fit <- randomForest(formula = 매출총액~.,
                      data = trainset,
                      ntree = grid$ntree[i],
                      mtry = grid$mtry[i])
  grid$error[i] <- tail(x = fit$mse, n = 1)
  
  #변수중요도 플랏 저장
  setwd(paste0("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/4.모델 적합/랜덤포레스트/",filename))
  png(filename = paste0("변수중요도_",i,".png"), width = 8000, height = 4000, res = 500)
  varImpPlot(x = fit, main = 'variable importance')
  dev.off()
  
  #시험셋으로 목표변수 추정값 생성
  pred1 <- predict(object = fit, newdata = testset, type = 'response')
  pred_list <- cbind(pred_list,pred1)
  #실제 관측치 벡터 생성
  real <- testset$매출총액
  
  #실측값과 비교하기 위해 testset 조작
  results <- testset_dummy
  results$매출총액_pred <- pred1
  results <- results %>%
    group_by(행정구역,대분류) %>%
    mutate(rank_real = row_number(desc(매출총액)),
           rank_pred = row_number(desc(매출총액_pred)),
           top3_real = ifelse(rank_real <=3,"1","0"),
           top3_pred = ifelse(rank_pred <=3,"1","0"))
  
  #rank를 factor형으로 변경
  num <- ncol(results)
  results[,(num-3):num] <- map_df(.x = results[,(num-3):num],.f = as.factor)
  
  #real_rank와 pred_rank 산점도 그리기
  results %>% 
    ggplot(aes(x = rank_real, y = rank_pred, color = as.factor(rank_real)))+
    geom_point(position = position_jitter(),size = 2)+
    ggsave(filename = paste0("rank산점도rank_",i,"_",".png"), width = 24, height = 12, units = "cm")
  
  results %>% 
    ggplot(aes(x = rank_real, y = rank_pred, color = as.factor(rank_real)))+
    geom_point(position = position_jitter(),size = 2)+
    ggsave(filename = paste0("rank산점도대분류_",i,"_",".png"), width = 24, height = 12, units = "cm")
  
  
  #회귀값 예측 결과
  grid$RMSE[i] <- MLmetrics::RMSE(y_pred = pred1, y_true = real)
  
  #Top3 범주값 예측 결과
  grid$F1[i] <- MLmetrics::F1_Score(y_true = results$top3_real, y_pred = results$top3_pred, positive = "1")
  grid$R2[i] <- MLmetrics::R2_Score(y_true = real, y_pred = pred1)
  
  disp <- str_glue('현재 {i}행 완료! [{round((i)/nrow(grid),2)*100}% 완료]')
  write.csv(grid,file = paste0(grid_filename,".csv"), row.names = F)
  Sys.sleep(2)
  write.csv(pred_list,file = paste0(pred_filename,"_list.csv"),row.names = F)
  Sys.sleep(2)
  cat(disp, "\n")
}
#튜닝 결과 확인
windows()
plot(x = grid$error, type = 'b', pch = 19, col = 'gray30', main = 'Grid Search Result')
abline(v = which.min(x = grid$error), col = 'red', lty = 2)
loc <- which.min(x = grid$error)
print(x = loc)
grid[loc,]

#RMSE,F1,R2 플랏
text <- data.frame(x = rep(nrow(grid)+0.5,4),
                   y = as.numeric(grid[nrow(grid),4:7]),
                   label = colnames(grid)[4:7])
text[text$label=="error",2] <- text[text$label=="error",2]*10
# windows()
grid %>% mutate(order = row_number()) %>% 
  ggplot(aes(x = order, y = RMSE))+geom_line(col = "blue")+geom_point(col = "blue")+ylab("")+
  geom_vline(xintercept = which.min(grid$RMSE), col = "blue", lty = 1, lwd = 2, alpha = 0.7)+
  geom_line(aes(y = F1), col = "red")+geom_point(aes(y = F1),col = "red")+
  geom_vline(xintercept = which.max(grid$F1), col = "red", lty = 6, lwd = 1.75)+
  geom_line(aes(y = R2), col = "orange")+geom_point(aes(y = R2), col = "orange")+
  geom_vline(xintercept = which.max(grid$R2), col = "orange", lty = 2, lwd = 1.2)+
  geom_line(aes(y = error*10), col = "black")+geom_point(aes(y = error*10), col = "black")+
  geom_vline(xintercept = which.min(grid$error), col = "black", lty = 2, lwd = 1.2)+
  scale_y_continuous(name = "RMSE, F1, R2",sec.axis = dup_axis(~./10,name = "MSE Error"), limits = c(0,1), breaks = seq(0,1,0.1))+
  geom_text(data = text, mapping = aes(x = text$x, y = text$y, label = text$label),col = c("black","blue","red","orange"), size = 10)+
  theme_classic()

which.min(grid$RMSE)
which.max(grid$F1)
which.max(grid$R2)
which.min(grid$error)