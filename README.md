# project_Analysis-of-Small-Business-Data

## Project 목표
---
> 소상공인 데이터 분석을 통한 행정구역 및 업종별 매출액 상위 3개 업태 제시

## project 배경
---
> 코로나 팬데믹과 같은 이슈 발생 시 다양한 측면에 부정적인 영향을 미치는데, 소상공인 매출도 그 중 하나이다. 정부가 팬데믹 상황을 타개하기 위해 다양한 정책을 시행함에도 근본적 이슈가 해결되지 않아 매출을 크게 감소한 상황이다. 그럼에도 신규 창업자는 지속적으로 존재하므로 이러한 창업자에게 팬데믹과 같은 이슈를 견뎌내고 매출을 지속적으로 발생시킬 수 있는 행정구역 및 업종별 매출액 상위 3개 업종을 제시하고자 한다.

## 데이터 출처
---
- 지급수단 이용형태
	1. 15년도 https://www.bok.or.kr/portal/bbs/B0000232/view.do?nttId=206659&menuNo=200706
	2. 17년도 http://www.bok.or.kr/portal/bbs/B0000232/view.do?nttId=236697&menuNo=200725
	3. 19년도 http://www.bok.or.kr/portal/bbs/B0000232/view.do?nttId=10056898&menuNo=200725
- 임대면적 : https://kosis.kr/statisticsList/statisticsListIndex.do?menuId=M_01_01&vwcd=MT_ZTITLE&parmTabId=M_01_01#SelectStatsBoxDiv
- 카드사별 실적 : http://fisis.fss.or.kr/fss/fsiview/indexw.html
- 은행별 실적 : http://fisis.fss.or.kr/fss/fsiview/indexw.html
- 임대시세 : https://golmok.seoul.go.kr/regionAreaAnalysis.do
- 소득분위 : https://golmok.seoul.go.kr/regionAreaAnalysis.do
- 신생기업 생존율 : https://golmok.seoul.go.kr/regionAreaAnalysis.do
- 행정동 코드 : http://data.seoul.go.kr/dataList/OA-15560/S/1/datasetView.do
- 행정구 코드 : https://data.seoul.go.kr/dataVisual/seoul/seoulLivingPopulation.do
- 유동인구수 : http://data.seoul.go.kr/dataList/OA-15568/S/1/datasetView.do
- 집객시설수 : http://data.seoul.go.kr/dataList/OA-15580/S/1/datasetView.do
- 점포수 : http://data.seoul.go.kr/dataList/OA-15577/S/1/datasetView.do
- 추정매출 : http://data.seoul.go.kr/dataList/OA-15572/S/1/datasetView.do

# 분석 방법
---
> 15.1~20.2분기 데이터를 분석하여 20.3분기의 매출액 상위 3개 업종의 매출총액을 예측하여 상위 3개 업종을 제시

# 분석 과정
---
![process](https://user-images.githubusercontent.com/74341192/106696709-9df50800-6620-11eb-9f69-ff4a0e58a94b.png)

 1. 데이터 수집(크롤링, openAPI, excel)
 2. 데이터 전처리 및 이상치 & 결측치 값 조정
 3. EDA
 4. 모델 적합(다중선형회귀, 회귀나무, 랜덤포레스트 + 앙상블)
 5. 결론

##### 과정별 특이사항
- 데이터 가공 :
	1. 점포수 : 목표 변수를 '매출총액/점포수'로 선정했기에, 점포수에 따른 bias를 최소화하기 위해 5개 이하인 점포 데이터는 삭제
	2. 매출액 : 15.1~20.2분기(22개 분기) 중 한 분기라도 매출액이 임대료보다 적은 경우 해당 데이터는 삭제
- EDA : 데이터 시각화 및 통계적 분석, feature engineering => 토요일 10,40,50대의 매출비율이 매출총액과 음의 상관관계를 갖는 것을 확인하여 피쳐로 채택
- 모델 적합 : 다중선형회귀, 회귀나무, 랜덤포레스트 모형을 적합하고 모델별 가중치를 적용을 하는 앙상블 과정을 통해 성능 향상

# 결론
---
예측 정확도 : 랜덤포레스트 모형의 경우 20.3분기 상위 3개 예측 결과가 89.3% 일치하였고, 앙상블 결과 91.3% 일치
한계점 : 
	1. 데이터 자체가 1차 가공이 된 상태로 사용한 것이므로, 이상치에 대한 판단 및 처리에 제한적
	2. 보정한 매출액이 모든 결제 수단을 대변 불가 ex)현금결제 비율이 높은 업종
	3. 기술 요구 조건에 대한 분석을 제외하여 창업 희망자에게 해당 분석 결과만으로 업태를 제시하는 것이 불가



		
