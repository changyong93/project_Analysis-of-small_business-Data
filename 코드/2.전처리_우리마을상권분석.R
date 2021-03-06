rm(list = ls())
library(tidyverse)

#폴더 지정
setwd('C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/데이터/원본데이터/우리마을 상권분석 서비스 데이터/1501_2009_2차')
getwd()

file_name <- list.files() #폴더 내 파일명 가져오기
file_name_list <- str_sub(file_name,1,nchar(file_name)-4) #파일명에서 .csv 제거
info_class_list <- c('신생기업 생존율'=2,'소득&가구수'=7,'임대시세'=8)

#엑셀파일 데이터 전처리 및 추출
modi_data <- function(filename,info_class,year){
  #파일 읽기
  test <- read.csv(filename)
  
  #분류 유형별 컬럼 선택
  col <- ifelse(info_class>5,2,2:3)
  
  #필요한 년도 데이터가 포함된 컬럼 선택
  col_num <- grep(pattern = year,x = names(test))
  test <- test[,c(col,as.integer(col_num))]
  
  col_num <- dim(test)[2]
  
  #생활밀접업종이라는 컬럼의 문자 전처리
  if(colnames(test)[2]=="생활밀접업종"){
    d1 <- c()
    d2 <- c()
    for(i in 1:dim(test)[1]){
      d1 <- c(d1,strsplit(test$생활밀접업종,"/")[[i]][1])
      if(length(strsplit(test$생활밀접업종,"/")[[i]])==2){
        d2 <- c(d2,strsplit(test$생활밀접업종,"/")[[i]][2])
      } else {
        name = paste0(strsplit(test$생활밀접업종,"/")[[i]][2],"&",strsplit(test$생활밀접업종,"/")[[i]][3])
        d2 <- c(d2,name)
      }
    }
    test$대분류 <- d1
    test$소분류 <- d2
  }
  #년도 및 분기 컬럼 생성
  year_quarter <- names(test)[3]
  test$년도 <- str_sub(year_quarter,2,5)
  test$분기 <- str_sub(year_quarter,8,8)
  
  #컬럼명 변경
  colnames(test)[1:col_num] <- test[1,1:col_num]
  
  #데이터셋 구성 최적화
  if(colnames(test)[2]=="생활밀접업종"){
    test <- test[-1,c((dim(test)[2]-1),dim(test)[2],(dim(test)[2]-3),(dim(test)[2]-2),1,3:(dim(test)[2]-4))]
  } else{
    test <- test[-1,c((dim(test)[2]-1),dim(test)[2],1:(dim(test)[2]-2))]
  }
}

#데이터를 정보분류별로 합치기 위한 변수 생성
new_Enter_data <- c()#신생기업 생존률
rent_data <- c() #임대시세
income_data <- c() #소득/가구수

#데이터 정보분류에 따라 합치기(merge)
for (i in c(1:(length(file_name)))){
  x=file_name[i]
  y=info_class_list[strsplit(file_name_list[i],'_')[[1]][3]]
  if(str_sub(file_name[i],1,4)==2017){
    data = rbind(modi_data(x,y,"2015"), modi_data(x,y,"2016"),modi_data(x,y,"2017"))
  }else if(str_sub(file_name[i],1,4)==2019){
    data = rbind(modi_data(x,y,"2018"), modi_data(x,y,"2019"))
  } else {
    data = rbind(modi_data(x,y,"2018"), modi_data(x,y,"2019"),modi_data(x,y,"2020"))
  }
  if (y==2){
    new_Enter_data = rbind(new_Enter_data,data)
  } else if (y==7){
    income_data = rbind(income_data, data)
  } else {
    rent_data = rbind(rent_data,data)
  }
  cat(round(x = i*100/(length(file_name)), digits = 2L),"% 완료","_",i,"_",x,"\n")
}

#불필요 행 및 열 제거
new_Enter_data <- new_Enter_data %>% filter(행정구역!="서울시 전체")
rent_data <- rent_data %>% filter(행정구역!="행정구역") %>% select(-c("환산 임대료.1","환산 임대료.2"))
income_data <- income_data %>% filter(행정구역!="서울시 전체") %>% select(-c("가구수"))

#임대료 컬럼 문자 정리
rent_data[,4] <- gsub(pattern = ",",replacement = "", x = rent_data[,4])

#소득분위 컬럼 값 변경
income_data <- income_data %>% mutate(소득분위 = str_sub(소득분위,1,1))

#정보분류 2-3번 합치기(대분류 & 소분류 포함 데이터)
# smallbz_data <- merge(x=new_Enter_data,y=store_num_data,by=c('년도','분기','대분류','소분류','행정구역'),all=T)
smallbz_data <- merge(x=new_Enter_data,y=rent_data,by=c('년도','분기','행정구역'),all=T)

#문자형 변경
vars <- 1:5
smallbz_data[,vars] <- map_df(.x = smallbz_data[,vars],.f = as.factor)
smallbz_data[,-vars] <- map_df(.x = smallbz_data[,-vars],.f = as.numeric)
income_data <- map_df(.x = income_data, .f = as.factor)
lapply(X = smallbz_data,FUN = function(x){sum(is.na(x))})

#rda file로 전처리 데이터 저장
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/데이터")
getwd()

save(smallbz_data,income_data,file = '우리마을상권분석_1501_2009_2차.rda')