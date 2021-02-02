library(rvest)
library(RSelenium)
library(httr)
library(seleniumPipes)
library(stringr)

##selenium(셀레니움)을 켭니다
##cmd를 켠 상태에서
##1) cd C:\r-selenium
##2) java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445
##입력후 엔터로 실행합니다

##그리고 R에서 아래 명령어를 실행
remDr <- remoteDriver(remoteServerAddr="localhost",
                      port=4445L,  
                      browserName="chrome")

#cmd에서 상기 1),2)과정 후 cmd를 켜놔야 함
remDr$open()

#사이트 섭속
url_path <- 'https://golmok.seoul.go.kr/regionAreaAnalysis.do'
remDr$navigate(url_path)

#비회원 접속
element <- remDr$findElement(using="xpath",value='//*[@id="loginPop"]/div/button[1]')
element$clickElement()

ifelse(a == ifelse(b==2,1,0),'yes','no')
#crawling하여 csv파일로 저장하는 함수 생성
crawling <- function(x,y){
  table <- remDr$findElement(using=x,value=y)
  page_parse = remDr$getPageSource()[[1]]
  page_html = page_parse %>% read_html()
  Sys.setlocale('LC_ALL', 'English')
  table = page_html %>% html_table(fill = TRUE)
  Sys.setlocale('LC_ALL', 'Korean')
  data <- as.data.frame(table[4])
  if(b==8){
    data_final <- rbind(data[1:2,],data[str_sub(data$행정구역,-1)==ifelse(b==8,'동','구'),])
    file_name <- paste0(ele_year$getElementText(),"_",ele_quarter$getElementText(),"_",ele_info_class$getElementText(),".csv")
  } else{
    data_final <- rbind(data[1:2,],data[str_sub(data$행정구역,-1)=='구',])
    file_name <- paste0(ele_year$getElementText(),"_",ele_quarter$getElementText(),"_",
                        ele_info_class$getElementText(),"_",ele_sector$getElementText(),"_",ele_option$getElementText(),".csv")
  }
  if(str_detect(file_name, "/")==T){
    file_name <- str_replace_all(file_name,"/","&")
  }
  write.csv(data_final,file_name)
}

setwd('C:/Users/ChangYong/Desktop/나노디그리/1.정규강의 학습자료/1차 프로젝트/소상공인/데이터/원본데이터/우리마을 상권분석 서비스 데이터')
getwd()
# input 변수
year_quarter <- list(Set1=c(2017,1),Set2=c(2017,2),Set3=c(2017,3),Set4=c(2017,4),
                     Set5=c(2019,4),Set6=c(2020,1),Set7=c(2020,2),Set8=c(2020,3))

info_class <- c(3,8,9) 
info_class_name <- c(
  "신생기업 생존율 = 3",
  "소득/가구수" = 8,
  "임대시세" = 9
  )

sector <- c(2:4) #생활밀접업종
sector_name <- c("외식업" = 2, "서비스업" = 3, "소매업" = 4)

for(a in 1:length(year_quarter)){
  ele_year=remDr$findElement(using="xpath",value=paste0("//*[@id='selectYear']/option[@value='",year_quarter[[a]][1],"']"))
  ele_year$clickElement()
  Sys.sleep(time = 0.5)
  ele_quarter=remDr$findElement(using="xpath",value=paste0("//*[@id='selectQu']/option[@value='",year_quarter[[a]][2],"']"))
  ele_quarter$clickElement()
  Sys.sleep(time = 0.5)
  for(b in c(info_class)){
    ele_info_class=remDr$findElement(using="xpath",value=paste0("//*[@id='infoCategory']/option[",b,"]"))
    ele_info_class$clickElement()
    Sys.sleep(time = 0.5)
    if(b>6){
      remDr$findElement(using="xpath",value="//*[@id='presentSearch']")$clickElement()
      Sys.sleep(time = 2)
      crawling(x = "id",y = "table1")
      Sys.sleep(time = 0.5)
    } else{
      for(cc in c(sector)){
        ele_sector=remDr$findElement(using="xpath",value=paste0("//*[@id='induL']/option[",cc,"]"))
        ele_sector$clickElement()
        Sys.sleep(time = 0.5)
        webElem <- remDr$findElement('id',"induM")
        appHTML <- webElem$getElementAttribute("outerHTML")[[1]]
        induM_data <- strsplit(appHTML,"</option>")[[1]]
        for(d in seq(2,length(induM_data)-1,1)){
          ele_option=remDr$findElement(using="xpath",value=paste0("//*[@id='induM']/option[",d,"]"))
          ele_option$clickElement()
          Sys.sleep(time = 0.5)
          remDr$findElement(using="xpath",value="//*[@id='presentSearch']")$clickElement()
          Sys.sleep(time = 2.5)
          crawling("id","table1")
          Sys.sleep(time = 0.5)
          print(paste0(year_quarter[[a]][1],"_",year_quarter[[a]][2],"분기",
                names(info_class_name[b]),"_",names(sector_name[cc]),"_",round(d/length(induM_data),2),"%"))
        } 
      }
    }
  }
}