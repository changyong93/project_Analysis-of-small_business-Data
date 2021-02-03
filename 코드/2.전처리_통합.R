rm(list = ls())
library(openxlsx)
library(tidyverse)

#상권코드 -행정동 코드 파일 읽기
library(XML)
loc1 <- xmlToDataFrame(doc = 'http://openapi.seoul.go.kr:8088/(인증키)/xml/TbgisTrdarRelm/1/1000/')
loc2 <- xmlToDataFrame(doc = 'http://openapi.seoul.go.kr:8088/(인증키)/xml/TbgisTrdarRelm/1001/1496/') 
var <- 1:2
loc1 <- loc1 %>% slice(-var)
loc2 <- loc2 %>% slice(-var)
sangkwon_loc <- rbind(loc1,loc2)
sangkwon_loc <- sangkwon_loc[,c(6,11)]
rm(loc1, loc2)
#행정동 코드-행정구 파일 읽기
setwd('C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/2. 데이터/원본데이터/')
guess_encoding(file = '행정동코드_매핑정보_20200325.xlsx')
sangkwon_gu <- read.xlsx(xlsxFile = '행정동코드_매핑정보_20200325.xlsx',sheet = 1)
sangkwon_gu <- sangkwon_gu %>% slice(-1)
sangkwon_gu <- sangkwon_gu[,c(2,4,5)]#행정동 추가

#EDA를 위한 초기 전처리 진행
#상권-추정매출 데이터 합치기
setwd('C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/2. 데이터/원본데이터/')
getwd()
#상권 추정매출 데이터 전처리
data2015 <- read.csv("서울시 우리마을가게 상권분석서비스(상권-추정매출)_2015.csv", fileEncoding = 'euc-kr')
data2016 <- read.csv("서울시 우리마을가게 상권분석서비스(상권-추정매출)_2016.csv", fileEncoding = 'euc-kr')
data2017 <- read.csv("서울시 우리마을가게 상권분석서비스(상권-추정매출)_2017.csv", fileEncoding = 'euc-kr')
data2018 <- read.csv("서울시 우리마을가게 상권분석서비스(상권-추정매출)_2018.csv", fileEncoding = 'euc-kr')
data2019 <- read.csv("서울시 우리마을가게 상권분석서비스(상권-추정매출)_2019.csv", fileEncoding = 'euc-kr')
data2020 <- read.csv("서울시 우리마을가게 상권분석서비스(상권-추정매출)_2020.csv", fileEncoding = 'euc-kr')

#상권코드명 인덱스 일치시키기
data2020[data2020$상권_코드_명=="종로?청계 관광특구",]$상권_코드_명 <- "종로·청계 관광특구"

#2015~2020년도 데이터셋 결합
smallbz_sales <- rbind(data2015,data2016,data2017,data2018,data2019,data2020)

#매출액이 마이너스인 데이터 제거
outlier_minus <- data.frame()
for(i in c(9:79)){
  outlier_minus <- rbind(outlier_minus, smallbz_sales[smallbz_sales[,i]<0,c(1,2,5,7)])
}
outlier_minus <- outlier_minus %>% distinct(상권_코드,서비스_업종_코드)
outlier_minus$사용여부 <- 1
smallbz_sales <- merge(x = smallbz_sales,y = outlier_minus, by = c("상권_코드","서비스_업종_코드"), all.x = T)
smallbz_sales <- smallbz_sales %>% filter(is.na(사용여부) == T)
smallbz_sales <- smallbz_sales[,-81]
rm(outlier_minus)

#필요한 컬럼만 선택
smallbz_sales <- smallbz_sales[,-c(11:33,57:79)]

#컬럼명 변경
colnames(smallbz_sales)[c(3,4,8,9,10,34)] <- c("년도","분기","소분류","매출총액","총매출건수","점포수_추정매출")

#매출데이터 행정구 추가
smallbz_total_1501_2009 <- merge(x = smallbz_sales, y = sangkwon_loc,by.x = '상권_코드', by.y = 'TRDAR_CD', all.x=T)
smallbz_total_1501_2009 <- merge(x = smallbz_total_1501_2009, y = sangkwon_gu,by.x = 'ADSTRD_CD', by.y = '행자부행정동코드', all.x=T)
smallbz_total_1501_2009 <- rename(smallbz_total_1501_2009,c('행정구역' = '시군구명'))

#우리마을 상권분석 데이터 불러오기
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/2. 데이터")
load('우리마을상권분석_1501_2009.rda')
colnames(smallbz_data)[6:9] <- c("생존률_1년차","생존률_3년차","생존률_5년차","임대료")

#문자열 변경
smallbz_data$소분류 <- str_replace_all(string = smallbz_data$소분류, pattern = '자전거및기타운송장비', replacement = '자전거 및 기타운송장비')
smallbz_total_1501_2009$소분류 <- str_replace_all(string = smallbz_total_1501_2009$소분류,pattern = "/",replacement = "&")

#행정동 컬럼명 변경
income_data <- rename(income_data,c('행정동명' = '행정구역'))

#통합
smallbz_total_1501_2009 <- merge(x = smallbz_total_1501_2009, y = smallbz_data, by = c('년도','분기','소분류','행정구역'), all.x=T)
smallbz_total_1501_2009 <- merge(x = smallbz_total_1501_2009, y = income_data, by = c('년도','분기','행정동명'),all.x =F)
rm(smallbz_data) ; rm(income_data)

#점포 개수 추가하기
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/2. 데이터/원본데이터")
data1 <- read.csv("서울시_우리마을가게_상권분석서비스(상권-점포)_2015년.csv")
data2 <- read.csv("서울시_우리마을가게_상권분석서비스(상권-점포)_2016년.csv")
data3 <- read.csv("서울시_우리마을가게_상권분석서비스(상권-점포)_2017년.csv")
data4 <- read.csv("서울시_우리마을가게_상권분석서비스(상권-점포)_2018년.csv")
data5 <- read.csv("서울시_우리마을가게_상권분석서비스(상권-점포)_2019년.csv")
data6 <- read.csv("서울시_우리마을가게_상권분석서비스(상권-점포)_2020년.csv")
jeompo <- rbind(data1,data2,data3,data4,data5,data6)

#행정동 및 행정구 컬럼 추가
jeompo <- merge(x = jeompo, y = sangkwon_loc,by.x = '상권_코드', by.y = 'TRDAR_CD', all.x=T)
jeompo <- merge(x = jeompo, y = sangkwon_gu,by.x = 'ADSTRD_CD', by.y = '행자부행정동코드', all.x=T)

#필요한 컬럼 선택 및 컬럼명 변경
vars <- c(2,3,4,17,9,10,11,13,15)
jeompo <- jeompo[,vars]
colnames(jeompo)[2:9] <- c("년도","분기","행정구역","소분류","점포수","점포수_유사업종","점포수_개업","점포수_폐업")

#소분류 특수문자 변경 (/ => &)
jeompo$소분류 <- str_replace_all(jeompo$소분류,"/","&")

#통합
smallbz_total_1501_2009 <- merge(x = smallbz_total_1501_2009, y= jeompo, by=c("상권_코드","년도","분기","행정구역","소분류"),all.x=T)

#점포수 처리, NA=>0
na_0 <- function(x){x <- ifelse(is.na(x)==T,0,x) ; return(x)}
vars <- c("점포수_추정매출","점포수","점포수_유사업종","점포수_개업","점포수_폐업")
for(i in vars){
  smallbz_total_1501_2009[,i] <- na_0(smallbz_total_1501_2009[,i])
}

#점포수는 점포수 추정 매출이 유사업종 수보다 큰 경우 점포수_추정매출을, 반대면 점포수_유사업종
#점포가 분기 중간에 폐업한 경우 점포수에서 제외됨
#매출액은 있는데 두 점포수 모두 없는 경우 1개 적용
smallbz_total_1501_2009 <- smallbz_total_1501_2009 %>%
  mutate(점포수 = ifelse(test = 점포수_추정매출 > 점포수_유사업종, yes = 점포수_추정매출, no = 점포수_유사업종))

#상권 및 업태별 점포수 개수가 5개 이상인 지역만 선택
jeom_num <- smallbz_total_1501_2009 %>% group_by(상권_코드,소분류)%>% filter(점포수 >= 5) %>% count() %>% filter(n == 23)

#점포수 개수가 5개 이상인 지역만 선택
smallbz_total_1501_2009 <- merge(x=smallbz_total_1501_2009,y=jeom_num, by = c('상권_코드','소분류'),all.y=T)
smallbz_total_1501_2009 <- smallbz_total_1501_2009[,-ncol(smallbz_total_1501_2009)]

#임대면적 데이터 불러오기
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/2. 데이터/원본데이터")
area_1501_1612 <- read.xlsx("상권별_집합_상가_기본정보_14011612.xlsx")
area_1701_1812 <- read.xlsx("상권별_집합_상가_기본정보_17011812.xlsx")
area_1901_1912 <- read.xlsx("상권별_집합_상가_기본정보_19011912.xlsx")
area_2001_2009 <- read.xlsx("상권별_집합_상가_기본정보_20012009.xlsx")

#데이터 전처리
area_modi <- function(data,type){
  if(type==1){data <- data[,2:ncol(data)]}
  data <- as.data.frame(t(data))
  colnames(data) <- data[1,]
  data$년도 <- str_sub(string = rownames(data),1,4)
  data$분기 <- str_sub(string = rownames(data),6,6)
  vars <- c(6,7,2,3,4,5)
  data <- data[,vars] %>% slice(-1)
  return(data)
}
area_1501_1612 <- area_modi(area_1501_1612,0)
area_1701_1812 <- area_modi(area_1701_1812,1)
area_1901_1912 <- area_modi(area_1901_1912,1)
area_2001_2009 <- area_modi(area_2001_2009,1)
colnames(area_1501_1612)[3:6] <- c("임대면적_도심지역","임대면적_강남지역","임대면적_신촌마포지역","임대면적_기타")
colnames(area_1701_1812)[3:6] <- c("임대면적_도심지역","임대면적_강남지역","임대면적_신촌마포지역","임대면적_기타")
colnames(area_1901_1912)[3:6] <- c("임대면적_도심지역","임대면적_강남지역","임대면적_신촌마포지역","임대면적_기타")
colnames(area_2001_2009)[3:6] <- c("임대면적_도심지역","임대면적_강남지역","임대면적_신촌마포지역","임대면적_기타")
area <- rbind(area_1501_1612, area_1701_1812, area_1901_1912, area_2001_2009)

#도심,강남,신촌마포,기타지역으로 데이터 구분
area_1 <- area[,c(1,2,3)] #서울시 도심지역
area_2 <- area[,c(1,2,4)] #서울시 강남지역
area_3 <- area[,c(1,2,5)] #서울시 신촌마포지역
area_4 <- area[,c(1,2,6)] #서울시 기타지역
area_list_1 <- c("중구","종로구")
area_list_2 <- c("강남구","서초구")
area_list_3 <- c("서대문구","마포구")
area_list_4 <- c("강동구","강북구","강서구","관악구","광진구","구로구","금천구","노원구","도봉구","동대문구",
                 "동작구","성동구","성북구","송파구","양천구","영등포구","용산구","은평구","중랑구")

#상권분석 데이터 임대료 데이터 추가
smallbz_total_1501_2009$임대면적 <- NA
for(i in unique(smallbz_total_1501_2009$년도)){
  for(j in unique(smallbz_total_1501_2009$분기)){
    for(k in unique(smallbz_total_1501_2009$행정구역)){
      if(k %in% area_list_1){
        smallbz_total_1501_2009[smallbz_total_1501_2009$년도==i & smallbz_total_1501_2009$분기==j,"임대면적"] <- area_1[area_1$년도==i & area_1$분기== j,3]
      } else if(k %in% area_list_2){
        smallbz_total_1501_2009[smallbz_total_1501_2009$년도==i & smallbz_total_1501_2009$분기==j,"임대면적"] <- area_2[area_2$년도==i & area_2$분기== j,3]
      } else if(k %in% area_list_3){
        smallbz_total_1501_2009[smallbz_total_1501_2009$년도==i & smallbz_total_1501_2009$분기==j,"임대면적"] <- area_3[area_3$년도==i & area_3$분기== j,3]
      } else {
        smallbz_total_1501_2009[smallbz_total_1501_2009$년도==i & smallbz_total_1501_2009$분기==j,"임대면적"] <- area_4[area_4$년도==i & area_4$분기== j,3]
      }
    }
  }
}
smallbz_total_1501_2009$임대면적 <- as.numeric(smallbz_total_1501_2009$임대면적)

#카드 결제 비율 계산
sales_card <- as.data.frame(t(read.xlsx("금융감독원_금융통계정보시스템_카드사.xlsx", colNames = F)))
sales_bank <- as.data.frame(t(read.xlsx("금융감독원_금융통계정보시스템_은행사.xlsx", colNames = F)))
colnames(sales_card) <- c("년월","우리카드","국민카드","롯데카드","비씨카드","삼성카드","신한카드","하나카드","현대카드")
colnames(sales_bank) <- c("년월","은행")
sales_card <- sales_card %>% slice(-1)
sales_card <- sales_card %>% mutate(년도 = str_sub(년월,1,4), 분기 = case_when(str_sub(년월,-2,-1)==".3" ~ "1",
                                                                         str_sub(년월,-2,-1)==".6" ~ "2",
                                                                         str_sub(년월,-2,-1)==".9" ~ "3",
                                                                         str_sub(년월,-2,-1)=="12" ~ "4")) %>% select(-년월)
sales_bank <- sales_bank %>% mutate(년도 = str_sub(년월,1,4), 분기 = case_when(str_sub(년월,6,7)=="03" ~ "1",
                                                                         str_sub(년월,6,7)=="06" ~ "2",
                                                                         str_sub(년월,6,7)=="09" ~ "3",
                                                                         str_sub(년월,6,7)=="12" ~ "4")) %>% select(-년월)
sales_merge <- merge(x = sales_bank, y = sales_card, by = c("년도","분기"))

#은행,국민카드,비씨카드, 신한카드 비율 계산
sales_merge[,3:ncol(sales_merge)] <- map_df(.x = sales_merge[,3:ncol(sales_merge)],.f = as.numeric)
sales_merge <- sales_merge %>% group_by(년도,분기) %>% summarise(카드비율_비씨신한국민 = (은행+비씨카드+신한카드+국민카드)/
                                                                          (은행+비씨카드+신한카드+국민카드+롯데카드+삼성카드+현대카드+하나카드+우리카드)) %>% as.data.frame()

sales_merge[,1:2] <- map_df(.x = sales_merge[,1:2],.f = as.factor)

#전체 결제 비율 중 카드결제 비율 데이터 생성
method <- data.frame(년도 = seq(2015,2020), 카드사용비율 = NA)
rate <- c((40.7+14.8)/100,(41.3+12.5)/100.6,(53.8+15.3)/100)
geom <- function(x,y){result = 2*(x*y)/(x+y); return(result)}
for(i in 1:6){
  if(i == 6){
    method[i,2] = (method[i-1,2]*method[i-2,2])/(2*method[i-2,2]-method[i-1,2])
  } else if(i %% 2 == 1){
    method[i,2] = rate[ceiling(i/2)]
  } else {
    method[i,2] = geom(rate[i/2],rate[i/2+1])
  }
}
#카드실적 및 카드결제 비율 데이터 merge
sales_merge <- merge(x = sales_merge,y = method, by="년도")

#임대료로 변환#분기별 전체 결제 방법 중 카드결제 비율 및 카드결제 비율 중 비씨,신한,국민카드 비율)
smallbz_total_1501_2009 <- merge(x = smallbz_total_1501_2009, y = sales_merge, by = c("년도","분기"))
smallbz_total_1501_2009 <- smallbz_total_1501_2009 %>%
  mutate(임대료 = 3/3.3*임대료*임대면적*카드비율_비씨신한국민*카드사용비율)%>%
  select(-c(임대면적,카드비율_비씨신한국민,카드사용비율))

#15.1분기 ~20.2분기의 22개의 분기 중 1번이라도 임대료보다 매출이 적은 경우 제외
remove_data <- smallbz_total_1501_2009 %>% mutate(년분기 = paste0(년도,"_",분기)) %>%
  filter(매출총액 < 임대료 & 년분기!="2020_3") %>%
  count(ADSTRD_CD,상권_코드,소분류)
smallbz_total_1501_2009 <- merge(x = smallbz_total_1501_2009,
                                 y = remove_data,
                                 by = c('ADSTRD_CD','상권_코드','소분류'),all.x=T)
smallbz_total_1501_2009 <- smallbz_total_1501_2009 %>% filter(is.na(n)==T) %>% select(-c(n,임대료))

#유동인구 데이터 가져오기
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/2. 데이터/원본데이터")
smallbz_pop <- read.csv("서울시 우리마을가게 상권분석서비스(상권-추정유동인구).csv")

#유동인구 데이터에 행정구 기준 추가
smallbz_pop <- merge(x = smallbz_pop, y = sangkwon_loc,by.x = '상권_코드', by.y = 'TRDAR_CD', all.x=T)
smallbz_pop <- merge(x = smallbz_pop, y = sangkwon_gu,by.x = 'ADSTRD_CD', by.y = '행자부행정동코드', all.x=T)

#조작할 컬럼만 선택
vars <- c(1:4,534,535,8) #년도,분기,시군구명,행정동명,총유동인구수
smallbz_pop <- smallbz_pop[,vars]

#컬럼명 및 컬럼 구성 변경
colnames(smallbz_pop)[3:7] <- c("년도","분기","행정구역","행정동명","총_유동인구수")

#데이터형 변경
smallbz_pop[,3:6] <- map_df(.x = smallbz_pop[,3:6],.f = as.factor)

#매출데이터와 유동인구 데이터 합치기
smallbz_total_1501_2009 <- merge(x = smallbz_total_1501_2009, y = smallbz_pop,
                                 by = c("ADSTRD_CD","상권_코드","년도","분기","행정구역","행정동명"), all.x=T)

#상권-숙박시설 데이터 합치기
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/2. 데이터/원본데이터")
smallbz_faci <- read.csv("서울시 우리마을가게 상권분석서비스(상권-집객시설).csv")

##NA를 0으로 변환
for(i in 7:26){
  smallbz_faci[,i] <- ifelse(is.na(smallbz_faci[,i])==T,0,smallbz_faci[,i])
}
##행정구 추가
smallbz_faci <- merge(x = smallbz_faci, y = sangkwon_loc,by.x = '상권_코드', by.y = 'TRDAR_CD', all.x=T)
smallbz_faci <- merge(x = smallbz_faci, y = sangkwon_gu,by.x = 'ADSTRD_CD', by.y = '행자부행정동코드', all.x=T)

##집객시설수 추가
vars <- c(1,2,3,4,28,29,8)
smallbz_faci <- smallbz_faci[,vars]
colnames(smallbz_faci)[3:6] <- c("년도","분기","행정구역","행정동명")

smallbz_total_1501_2009 <- merge(x = smallbz_total_1501_2009, y= smallbz_faci, by = c("년도","분기","행정구역","행정동명","상권_코드","ADSTRD_CD"),all.x =T)

#대분류(업종) 및 소분류(업태) 사이 중분류 추가
#참고 : 중소기업벤처부의 한국표준산업분류
MD_category <- list(오락관련서비스 = c("PC방","노래방","볼링장","전자게임장"),
                           개인및소비용품수리 = c("가전제품수리", "미용실","자동차수리","통신기기수리"),
                           숙박 = c("고시원","여관"),
                           스포츠 = c('골프연습장','당구장','스포츠클럽'),
                           개인 = c('네일숍','세탁소','자동차미용','피부관리실'),
                           교육 = c('스포츠 강습','예술학원','외국어학원','일반교습학원'),
                           보건 = c('일반의원','치과의원','한의원'),
                           부동산 = c("부동산중개업"),
                           기타상품전문 = c('시계및귀금속','안경','애완동물','의료기기','의약품','화장품','화초','예술품'),
                           기타생활용품 = c('가구','인테리어','조명용품','철물점','악기'),
                           무점포 = c('전자상거래업'),
                           오락및여가용품 = c('문구','서적','완구','운동&경기용품','자전거 및 기타운송장비'),
                           음식료품및담배 = c('미곡판매','반찬가게','수산물판매','슈퍼마켓','육류판매','청과상'),
                           의류 = c('가방','섬유제품','신발','일반의류','한복점','유아의류'),
                           전자제품 = c('가전제품','컴퓨터및주변장치판매','핸드폰'),
                           종합소매 = c('편의점'),
                           기타음식점 = c('분식전문점','제과점','치킨전문점','패스트푸드점'),
                           비알콜음료점 = c("커피-음료"),
                           일반음식점 = c('양식음식점','일식음식점','중식음식점','한식음식점'),
                           주점업 = c('호프-간이주점'))

smallbz_total_1501_2009$중분류 <- 0
for(i in 1:length(MD_category)){
  smallbz_total_1501_2009[smallbz_total_1501_2009$소분류 %in% MD_category[[i]],]$중분류 <- names(MD_category[i])
  cat(round(i/length(MD_category),digits=4L)*100,"% 완료\n")
}

#NA처리, NA=>0 / 집객시설_수, 총_유동인구수의 NA를 0으로 변경
na_0 <- function(x){ x <- ifelse(is.na(x)==T,0,x);return(x)}
smallbz_total_1501_2009[,c("집객시설_수","총_유동인구수")] <- map_df(.x = smallbz_total_1501_2009[,c("집객시설_수","총_유동인구수")],.f = na_0)

#생존률이 NA가 아닌 인덱스만 선택
smallbz_total_1501_2009 <- smallbz_total_1501_2009 %>% filter(is.na(생존률_1년차)==FALSE & is.na(생존률_3년차)==FALSE & is.na(생존률_5년차)==FALSE)

#15.1~20.3분기 데이터가 모두 있는 상권 및 업태만 선택
selected <- smallbz_total_1501_2009 %>%
  count(상권_코드,소분류) %>%
  filter(n == 23)
smallbz_total_1501_2009 <- merge(x = smallbz_total_1501_2009,
                                 y = selected,
                                 by = c('상권_코드','소분류'),all.x=T)

smallbz_total_1501_2009 <- smallbz_total_1501_2009 %>% filter(n == 23) %>% select(-n)

#불필요 컬럼 제거
vars <- c("ADSTRD_CD","서비스_업종_코드","상권_구분_코드","상권_구분_코드_명",
          "상권_코드_명","점포수_유사업종","점포수_개업","점포수_폐업","점포수_추정매출")
smallbz_total_1501_2009 <- smallbz_total_1501_2009 %>% select(-vars)

#범주형 및 연속형 데이터 정리
vars <- c("상권_코드","중분류","소분류","년도","분기","행정구역","행정동명","대분류","소득분위")
vars <- which(colnames(smallbz_total_1501_2009) %in% vars)
smallbz_total_1501_2009[,vars] <- map_df(.x = smallbz_total_1501_2009[,vars],.f = as.factor)
smallbz_total_1501_2009[,-vars] <- map_df(.x = smallbz_total_1501_2009[,-vars],.f = as.numeric)

#파일 저장
setwd("C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/2. 데이터")
save(smallbz_total_1501_2009,file = "dataset.rda")
