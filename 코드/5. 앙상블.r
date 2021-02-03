rm(list = ls())
library(tidyverse)

#매출데이터 불러오기
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/2. 데이터")
load("dataset_set.rda")
results <- testset[,c("행정구역","대분류","중분류","매출총액")]

#선형회귀 결과 불러오기
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/2. 데이터")
load("linearRegression.rda")
rm(result1_2) ; rm(result2_2); rm(result22)

#회귀나무 결과 불러오기
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/4.모델 적합/회귀나무/1차")
pred_regTree <- read.csv("pred1_list.csv")

#랜덤포레스트 결과 불러오기
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/4.모델 적합/랜덤포레스트/5차")
pred_random <- read.csv("pred5_list.csv")

#결과 합치기
results$pred_lin <- result11$pred
results$pred_reg <- pred_regTree[,19]
results$pred_ran <- pred_random[,1]

#앙상블 작업을 위한 grid 생성
grids <- expand.grid(lin_ratio = seq(0,1,0.01),
                     reg_ratio = seq(0,1,0.01),
                     ran_ratio = seq(0,1,0.01),
                     rmse = NA,
                     F1 = NA,
                     R2 = NA)

#회귀모델, 회귀나무모델, 랜덤포레스트 모델 비율 합이 1인 인덱스만 선택
grids <- subset(grids,(lin_ratio+reg_ratio+ran_ratio)==1)

#가중치 적용을 통해 앙상블 최적 조건 찾기
library(MLmetrics)

for(i in 1:nrow(grids)){
  sen <- str_glue('{i}행 [lin_ratio = {grids$lin_ratio[i]}, reg_ratio = {grids$reg_ratio[i]}, ran_ratio = {grids$ran_ratio[i]}]')
  cat(paste0(sen,"\n"))
  results$매출_ensemble <- results$pred_lin*grids$lin_ratio[i] + results$pred_reg*grids$reg_ratio[i] + results$pred_ran*grids$ran_ratio[i]
  dataset <- results %>% 
    group_by(행정구역,대분류) %>%
    mutate(rank_real = row_number(desc(매출총액)),
           rank_ensemble = row_number(desc(매출_ensemble)),
           top3_real = ifelse(rank_real <= 3,"1", "0"),
           top3_ensem = ifelse(rank_ensemble <= 3, "1", "0"))

  grids$rmse[i] <- MLmetrics::RMSE(y_true = dataset$매출총액,y_pred = dataset$매출_ensemble)
  grids$F1[i] <- MLmetrics::F1_Score(y_true = dataset$top3_real,y_pred = dataset$top3_ensem, positive = "1")
  grids$R2[i] <- MLmetrics::R2_Score(y_true = dataset$매출총액,y_pred = dataset$매출_ensemble)
  # print(grids[i,])
  sen <- str_glue('{round(i*100/nrow(grids),2)} % 완료')
  cat(paste0(sen,"\n"))
}

#F1_Score가 가장 높은 조합 찾기 => 동일값이 많은 경우 RMSE 및 F2 비교
grids[which.max(x = grids$F1),]
grid_F1 <- grids[grids$F1>=0.913,]
grid_F1[which.min(x = grid_F1$rmse),]
grid_F1[which.max(x = grid_F1$R2),]

#F1 & R2가 가장 높으며, RMSE가 가장 낮은 경우는
#다중회귀 x 0.01 + 회귀나무 x 0.23 + 랜덤포레스트 x 0.76 => F1점수 0.009 상승
results$매출_ensemble <- results$pred_lin*0.01 + results$pred_reg*0.23 + results$pred_ran*0.76
results <- results %>% 
  group_by(행정구역,대분류) %>%
  mutate(rank_real = row_number(desc(매출총액)),
         rank_ran = row_number(desc(pred_ran)),
         rank_ensemble = row_number(desc(매출_ensemble)),
         top3_real = ifelse(rank_real <= 3,"1", "0"),
         top3_ran = ifelse(rank_ran <= 3,"1", "0"),
         top3_ensem = ifelse(rank_ensemble <= 3, "1", "0"))
#최적 조합으로 rank 산점도 그리기
windows()
results %>% 
  ggplot(aes(x = rank_real, y = rank_ensemble, color = as.factor(rank_real)))+geom_point(position = position_jitter())
#최적 조합으로 top3 누적 개수 구하기
results %>% 
  filter(rank_ensemble %in% 1:3) %>% 
  mutate(중분류 = factor(중분류, levels = c('스포츠','개인','오락관련서비스','숙박','교육','보건',
                                            '무점포','전자제품','기타상품전문','기타생활용품','오락및여가용품','음식료품및담배','종합소매',
                                            '주점업','기타음식점','비알콜음료점','일반음식점'))) %>%
  ungroup() %>% 
  count(대분류,중분류) %>% 
  ggplot(aes(x = 중분류, y = n, fill = 대분류))+geom_bar(stat = "identity")+
  geom_text(aes(y = n+1, label = paste0(중분류,"\n",n,"회")), fontface = "bold", size = 5)+theme_classic()
#최적 조합 rank 저장
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/4.모델 적합/앙상블")
write.csv(results,file = "ensemble.csv")