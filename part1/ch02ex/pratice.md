---
title: "practice"
output: html_document
---

1) GDP.rate에 경제성장률 값을 할당하고 출력하시오

```r
GDP.rate<-c(5.2,5.1,2.3,0.3,6.2,3.6)
GDP.rate
```

```
## [1] 5.2 5.1 2.3 0.3 6.2 3.6
```
2-1) Unem.rate에 연도별실업률 값을할당 후 GDP.rate와 동시에 출력하시오

```r
Unem.rate<-c(3.5,3.2,3.2,3.6,3.7,3.4)
Unem.rate;GDP.rate
```

```
## [1] 3.5 3.2 3.2 3.6 3.7 3.4
```

```
## [1] 5.2 5.1 2.3 0.3 6.2 3.6
```
2-2) Unem.rate의 길이, 유형, 클래스 확인

```r
length(Unem.rate)
```

```
## [1] 6
```

```r
mode(Unem.rate)
```

```
## [1] "numeric"
```

```r
class(Unem.rate)
```

```
## [1] "numeric"
```
3) 12개 관측값을 WDrate로 할당, 각 원소 이름할당, 출력

```r
WDrate<-c(1417.59,1285.47,1239.99,1168.81,1144.07, 1165.72,1184.63,1133.51,1119.81,1083.56,1083.91,1144.51)

names(WDrate)<-paste(rep(2009:2011,each=4),paste(1:4,4,sep="/"),sep=".")

WDrate
```

```
## 2009.1/4 2009.2/4 2009.3/4 2009.4/4 2010.1/4 2010.2/4 2010.3/4 2010.4/4 
##     1418     1285     1240     1169     1144     1166     1185     1134 
## 2011.1/4 2011.2/4 2011.3/4 2011.4/4 
##     1120     1084     1084     1145
```
4-1) 커피판매자료를 coffee에할당, 범주화자료로 변환(Amer,Latte,Cappu) 후coffee.fct에 새롭게 할당, 결과출력

```r
coffee<-c("c","b","c","b","c","c","a","b","a","b","a","b","b","a","b","c","a","b","b","a","a","b","b","a","b","c","b","a","a","b","b","c","b","a","b","c","a","a","c","a","a","c","b","b","b","a","c","b","b","b","b","b","a","c","c","b","a","c","a","b")
coffee.fct<-factor(coffee,levels=c("a","b","c"), labels=c("Amer","Latte","Cappu"))
coffee.fct
```

```
##  [1] Cappu Latte Cappu Latte Cappu Cappu Amer  Latte Amer  Latte Amer 
## [12] Latte Latte Amer  Latte Cappu Amer  Latte Latte Amer  Amer  Latte
## [23] Latte Amer  Latte Cappu Latte Amer  Amer  Latte Latte Cappu Latte
## [34] Amer  Latte Cappu Amer  Amer  Cappu Amer  Amer  Cappu Latte Latte
## [45] Latte Amer  Cappu Latte Latte Latte Latte Latte Amer  Cappu Cappu
## [56] Latte Amer  Cappu Amer  Latte
## Levels: Amer Latte Cappu
```
4-2) Latte의 판매비율

```r
Latte.vec<-coffee.fct[coffee.fct=="Latte"]
Latte.vec
```

```
##  [1] Latte Latte Latte Latte Latte Latte Latte Latte Latte Latte Latte
## [12] Latte Latte Latte Latte Latte Latte Latte Latte Latte Latte Latte
## [23] Latte Latte Latte Latte Latte
## Levels: Amer Latte Cappu
```

```r
length(Latte.vec)/length(coffee.fct)
```

```
## [1] 0.45
```
5-1) 모형계수행렬A를 정의하고 출력해라. 
변수는 Qd1, Qs1, P1, Qd2, Qs2, p2순서로 배열하고, 
행의 이름은 equ1~equ6 의 형태로, 열의 이름은 각각 변수이름으로 입력

```r
mat.data<-c(1,-1,0,0,0,0,1,0,2,0,0,-1,0,1,-3,0,0,0,0,0,0,1,-1,0,0,0,-1,1,0,1,0,0,0,0,1,-3)
A<-matrix(mat.data,ncol=6,byrow=T, dimnames=list(c(paste("equ", 1:6,sep=" ")),c("Qd1","Qs1","P1","Qd2", "Qs2", "P2")))
A
```

```
##       Qd1 Qs1 P1 Qd2 Qs2 P2
## equ 1   1  -1  0   0   0  0
## equ 2   1   0  2   0   0 -1
## equ 3   0   1 -3   0   0  0
## equ 4   0   0  0   1  -1  0
## equ 5   0   0 -1   1   0  1
## equ 6   0   0  0   0   1 -3
```
5-2) 행렬의 속성을 확인

```r
attributes(A)
```

```
## $dim
## [1] 6 6
## 
## $dimnames
## $dimnames[[1]]
## [1] "equ 1" "equ 2" "equ 3" "equ 4" "equ 5" "equ 6"
## 
## $dimnames[[2]]
## [1] "Qd1" "Qs1" "P1"  "Qd2" "Qs2" "P2"
```
6-1) 주어진 자료를 첫번째 차원은 국가, 두번째 차원은 변수, 세 번째 차원은 연도로 하는 3차원 형태로 배열

```r
BigMac<-array(c(3.5,3.89,4.93,910,1.7,1.18,-14,-4,21,3.19,3.82,4.43,882,1.69,1.2,-24,-9,6),dim=c(3,3,2),dimnames=list(c("KOR","UK","EURO"),c("Price","PPP","Value"),c("2011y","2012y")))

BigMac
```

```
## , , 2011y
## 
##      Price    PPP Value
## KOR   3.50 910.00   -14
## UK    3.89   1.70    -4
## EURO  4.93   1.18    21
## 
## , , 2012y
## 
##      Price    PPP Value
## KOR   3.19 882.00   -24
## UK    3.82   1.69    -9
## EURO  4.43   1.20     6
```
6-2) 11,12년도 한국, 영국, 유료지역 달러환산 빅맥가격 평균과 표준편차를 구하고 3개국가 빅맥의 현지 통화가격을 계신하여 새로운 배열을 구성

step1.빅맥가격의 평균과 표준편차

```r
mean(BigMac[,1,1]);sd(BigMac[,1,1])
```

```
## [1] 4.107
```

```
## [1] 0.7392
```

```r
mean(BigMac[,1,2]);sd(BigMac[,1,2])
```

```
## [1] 3.813
```

```
## [1] 0.62
```
step2. 3개국가 빅맥의 현지통화 가격을 계산

```r
localprice.11<-c((3.5/0.86)*910,(3.89/0.96)/1.7,(4.93/1.21)/1.18)
localprice.12<-c((3.19/0.76)*882,(3.82/0.91)/1.69,(4.43/1.06)/1.2)
localprice.11;localprice.12
```

```
## [1] 3703.488    2.384    3.453
```

```
## [1] 3702.079    2.484    3.483
```
step3. 새로운 배열 생성

```r
BicMac2011<-cbind(BigMac[,,1],localprice.11)
BicMac2012<-cbind(BigMac[,,2],localprice.12)
new.BicMac<-array(c(BicMac2011,BicMac2012),dim=c(3,4,2),dimnames=list(c("KOR","UK","EURO"),c("Price","PPP","Value","localprice"),c("2011y","2012y")))
new.BicMac
```

```
## , , 2011y
## 
##      Price    PPP Value localprice
## KOR   3.50 910.00   -14   3703.488
## UK    3.89   1.70    -4      2.384
## EURO  4.93   1.18    21      3.453
## 
## , , 2012y
## 
##      Price    PPP Value localprice
## KOR   3.19 882.00   -24   3702.079
## UK    3.82   1.69    -9      2.484
## EURO  4.43   1.20     6      3.483
```
7-1) 표자료를 데이터프레임으로 구성

```r
Mdot<-c(8.3,11.2,14.3,10.3,8.7,4.2)
Pdot<-c(5.2,5.1,2.3,0.3,6.3,3.6)
Ydot<-c(2.2,2.5,4.7,2.8,3,4)
MPY.data<-data.frame(Mdot,Pdot,Ydot,row.names=c(2006:2011))
MPY.data
```

```
##      Mdot Pdot Ydot
## 2006  8.3  5.2  2.2
## 2007 11.2  5.1  2.5
## 2008 14.3  2.3  4.7
## 2009 10.3  0.3  2.8
## 2010  8.7  6.3  3.0
## 2011  4.2  3.6  4.0
```
7-2) 연도별 물가상승률과 실질경제성장률 관측값을 합하여 새로운 벡터 할당후 데이터프레임의 네번째 변수로 추가

```r
PY<-MPY.data$Pdot+MPY.data$Ydot
MPY.data$sum.PY<-PY
MPY.data
```

```
##      Mdot Pdot Ydot sum.PY
## 2006  8.3  5.2  2.2    7.4
## 2007 11.2  5.1  2.5    7.6
## 2008 14.3  2.3  4.7    7.0
## 2009 10.3  0.3  2.8    3.1
## 2010  8.7  6.3  3.0    9.3
## 2011  4.2  3.6  4.0    7.6
```
7-3) 실질경제성장률의 6년간 기하평군을 구하고 물가상승요인이 작용한 해를 확인해라

```r
prod(MPY.data$Ydot)^(1/6)
```

```
## [1] 3.089
```

```r
MPY.data[MPY.data$Mdot>MPY.data$sum.PY,]
```

```
##      Mdot Pdot Ydot sum.PY
## 2006  8.3  5.2  2.2    7.4
## 2007 11.2  5.1  2.5    7.6
## 2008 14.3  2.3  4.7    7.0
## 2009 10.3  0.3  2.8    3.1
```
8) 평균50, 분산9 인 정규분포 난수 30개 생성 / 난수는 소수점 셋째자리에서 반올림, 언제나 동일한 난수를 생상하여 사용할수있도록 시드번호 1734입력.

```r
set.seed(1734)
normdata<-round(rnorm(n=30, mean=50, sd=3), digit=2)
normdata
```

```
##  [1] 46.58 50.18 49.02 51.82 50.24 48.74 47.95 45.69 48.97 51.72 52.84
## [12] 51.58 45.87 46.02 49.86 47.77 47.64 47.66 55.74 49.07 49.86 51.40
## [23] 48.30 45.74 47.95 47.02 53.46 49.37 53.77 51.45
```
8-1) 데이터 총합, 데이터의 범위

```r
sum(normdata)
```

```
## [1] 1483
```

```r
diff(range(normdata))
```

```
## [1] 10.05
```
8-2) 데이터 내림차순정렬, 평균값, 중앙값, 표준편차

```r
sort(normdata,decreasing=T)
```

```
##  [1] 55.74 53.77 53.46 52.84 51.82 51.72 51.58 51.45 51.40 50.24 50.18
## [12] 49.86 49.86 49.37 49.07 49.02 48.97 48.74 48.30 47.95 47.95 47.77
## [23] 47.66 47.64 47.02 46.58 46.02 45.87 45.74 45.69
```

```r
mean(normdata)
```

```
## [1] 49.44
```

```r
median(normdata)
```

```
## [1] 49.05
```

```r
sd(normdata)
```

```
## [1] 2.581
```
8-3) 데이터 20%, 40%, 60%, 80%분위수, 요약통계량

```r
quantile(normdata,prob=c(0.2,0.4,0.6,0.8))
```

```
##   20%   40%   60%   80% 
## 47.52 48.56 49.86 51.61
```

```r
summary(normdata)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    45.7    47.7    49.0    49.4    51.4    55.7
```
8-4) 종료시점 2013년 관측주기 1개월 시계열 자료생성, 시차지연함수 사용하여 1년 시차 지연효과를 적용해보라

```r
norm.ts<-ts(normdata,end=2013,frequency=12)
norm.ts
```

```
##        Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov
## 2010                                           46.58 50.18 49.02 51.82
## 2011 48.74 47.95 45.69 48.97 51.72 52.84 51.58 45.87 46.02 49.86 47.77
## 2012 47.66 55.74 49.07 49.86 51.40 48.30 45.74 47.95 47.02 53.46 49.37
## 2013 51.45                                                            
##        Dec
## 2010 50.24
## 2011 47.64
## 2012 53.77
## 2013
```
