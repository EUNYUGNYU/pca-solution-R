# 패키지 불러오기
library(tidyverse)
library(FactoMineR)
library(factoextra)

# A. 모집단 공분산 행렬
S=matrix(c(10000,60,60,1), nrow=2)

# a. S에 대응하는 R 구하기
invDs<-diag(1/sqrt(diag(S)))
R<-invDs %*% S %*% invDs
R


# b. S를 이용한 주성분 분석
eig_S<-eigen(S)
# 아이겐 벡터
V_S<-eig_S$vectors
V_S
#아이겐 값
L_S<-diag(eig_S$values)
L_S


# c. R을 활용한 주성분 분석
eig_R<-eigen(R)
# 아이겐 벡터
V_R<-eig_R$vectors
V_R
#아이겐 값
L_R<-diag(eig_R$values)
L_R

# d. S를 이용한 주성분분석과 R을 이용한 주성분 분석 비교
eig_S
eig_R
# => R에 의한 주성분과 S에 의한 주성분이 설명하는 분산의 양은 다르며, 각 주성분의 계수도 다르다.
# => R은 척도가 변하지 않으므로, R에 의한 주성분도 변하지 않는다. 또한 R에 의한 주성분은 유일하지 않다.



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



# C. olivetti 얼굴 인식


# 데이터 불러오기
olivetti<-read.csv('olivetti_X.csv')
dim(olivetti) # 주성분 총 399개


# 공분산행렬 S로 분석하기
Mprcomp_olivetti<-prcomp(olivetti, center=TRUE, scale=FALSE)
summary(Mprcomp_olivetti) 


# 주성분계수 개수 정하기
vrexp2<-data.frame(
  pc=1:length(Mprcomp_olivetti$sdev), #루트 람다
  eigval=Mprcomp_olivetti$sdev^2, # 람다. 아이겐값
  prop=Mprcomp_olivetti$sdev^2/sum(Mprcomp_olivetti$sdev^2), # 주성분 별 비율
  cuml=cumsum(Mprcomp_olivetti$sdev^2)/sum(Mprcomp_olivetti$sdev^2)) # 전체 누적 비율
vrexp2


# 스크리 도표 그리기 (ggbiplot)
library(ggfortify)
ggplot(vrexp2, aes(x=pc, y=eigval))+
  geom_col()+geom_point()+ geom_line()

# => 아이겐 값이 1이 넘는 주성분만 사용할 때는 주성분 개수는 12개 이다.주성분 12개를 사용했을 때 전체의 68 %를 설명하고 있다. (이미지 압축 효과)
# => 누적 비율이 80% 가 넘는 주성분을 사용할 때는 주성분 개수가 27개, 90% 이상일 때는 주성분 개수가 66개 이다.
# 이미지를 주성분 분석했을 때에는 "이미지 압축" 효과가 있다. 즉, 원본 (주성분 분석 x) 에 비해 원본의 특징이 부각되지 않는 흐릿한 사진이 보이게 되는 것이다.


# biplot 그리기

autoplot(Mprcomp_olivetti, data=olivetti,
         loadings=TRUE, loadings.label=TRUE,
         loadings.label.size=1,loadings.label.vjust=1)

# => 변수 400개를 2차원으로 표현했기에 현재 난리(?)가 난 상태이다.
# => 이미지는 해석이 쉽지 않다. 

