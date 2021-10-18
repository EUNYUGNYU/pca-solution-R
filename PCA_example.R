# 패키지 불러오기
library(tidyverse)
library(FactoMineR)
library(factoextra)


# B. 미국 범죄통계

# 데이터 불러오기 및 데이터 정리
DF1<-read.csv('crime-kr.csv')
crime<-DF1[2:8]
row.names(crime)<-DF1$state
crime

# 상관행렬 R로 분석하기
Mprcomp_crime<-prcomp(crime, center=TRUE, scale=TRUE)
summary(Mprcomp_crime)

# 주성분계수 개수 정하기
vrexp<-data.frame(
  pc=1:length(Mprcomp_crime$sdev), #루트 람다
  eigval=Mprcomp_crime$sdev^2, # 람다. 아이겐값
  prop=Mprcomp_crime$sdev^2/sum(Mprcomp_crime$sdev^2), # 주성분 별 비율
  cuml=cumsum(Mprcomp_crime$sdev^2)/sum(Mprcomp_crime$sdev^2)) # 전체 누적 비율
vrexp

# 스크리 도표 그리기 (ggbiplot)
library(ggfortify)
ggplot(vrexp, aes(x=pc, y=eigval))+
  geom_col()+geom_point()+ geom_line()

# => 사회과학 쪽에서는 아이겐 값이 1이 넘는 것만 주성분으로 사용한다. 따라서 주성분 개수는 2개 이다.
# => 공학 쪽에서는 전체 정보량의 누적 설명량이 90% 이상되는 주성분을 사용한다. 이 경우 주성분 개수는 3개가 된다.
# => 여기서는 주성분 개수를 "2개"로 정하겠다.

# 주성분 해석하기
Mprcomp_crime$rotation

# PC1 해석하기
# => 모든 변수가 음수가 나왔다. 전반적으로 살인, 성범죄, 강도, 폭행, 빈집털이, 절도, 차량절도 7개의 범죄가 상관 관계가 있음을 알 수 있다. (하나의 범죄를 저지르면 다른 5개의 범죄를 저지를 가능성이 높다.)

#PC2 해석하기
# => 강도 - 빈집털이 - 절도 - 차량절도 끼리 상관관계가 있고, 살인 - 폭행 - 성범죄를 저지른 사람끼리 상관 관계가 있다. (엄밀히 말하면 성범죄는 0.16으로 비교적 다른 범죄들과 상관관계가 없는 편이다.
# => 강도 - 빈집털이 - 절도 - 차량절도 처럼 재산 및 금품 관련 범죄 중 1개를 저지른 사람은 다른 재산 및 금품 관련 저지를 가능성이 높지만 그것과 대비해서 살인 - 폭행 - 성범죄는 저지르지 않는 것을 알 수 있다.

# 주성분 점수 확인
Mprcomp_crime$x

# 주성분 1의 최고 점수를 받은 주
sort(Mprcomp_crime$x[,1], decreasing=TRUE)[1]

# 주성분 1의 최저 점수를 받은 주
sort(Mprcomp_crime$x[,1], decreasing=FALSE)[1]

# 주성분 2의 최고 점수를 받은 주
sort(Mprcomp_crime$x[,2], decreasing=TRUE)[1]

# 주성분 2의 최저 점수를 받은 주
sort(Mprcomp_crime$x[,2], decreasing=FALSE)[1]

# biplot 그리기
autoplot(Mprcomp_crime, data=crime,
         label=TRUE, label.size=3, label.vjust=1.5,
         loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=3,loadings.label.vjust=1.5)
