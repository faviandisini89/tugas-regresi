# Pembacaan dataset
RB <- read.csv("D:/#kumpulan_dataset/data coba 2.csv", sep = ";", header = TRUE)

# Menyamakan objek data
RLB <- RB


X<-RB[,-1]
Y<-RB[,1]
Y<-as.matrix(Y)
n<-nrow(RB)
X1<-matrix(1,n,1)
Xgab<-cbind(X1,X)
Xgab<-as.matrix(Xgab)
betatopi<-solve(t(Xgab)%*%Xgab)%*%t(Xgab)%*%Y

## Uji Simultan/Uji F
JKR<-t(betatopi)%*%t(Xgab)%*%Y-n*mean(Y)^2
JKG<-t(Y)%*%Y-t(betatopi)%*%t(Xgab)%*%Y
JKT<-t(Y)%*%Y-n*mean(Y)^2

DBR<-ncol(X)            ## k = banyak variabel bebas (p - 1)
DBG<-n-ncol(X)-1        ## n - p
DBT<-n-1
KTR<-JKR/DBR
KTG<-JKG/DBG
Fhitung<-KTR/KTG
Ftab<-qf((1-0.05),DBR,DBG)

tab.anova<-data.frame(
  Sumber =c("Regresi","Galat","Total"),
  db=c(DBR,DBG,DBT),
  JK=c(JKR,JKG,JKT),
  KT=c(KTR,KTG,NA),
  Fhit=c(Fhitung,NA,NA),
  Ftab=c(Ftab,NA,NA)
)
if(Fhitung>Ftab){
  cat("Hasil Keputusan Uji F adalah Tolak H0","\n")
}else{
  cat("Hasil Keputusan Uji F adalah Terima H0","\n")
}

## Standard error koefisien
KTG<-as.numeric(KTG)
xx<-solve(t(Xgab)%*%Xgab)
rg<-sqrt(KTG*diag(xx))
Se<-rg

## Uji Parsial  ## i=iterasi
ttab<-qt((1-(0.05/2)),DBG)
ttab2<-qt(0.05/2,DBG)
nama<-c("Intercept",colnames(X))
th<-rep(0,nrow(betatopi))
for(i in 1:nrow(betatopi)){
  th[i]<-betatopi[i]/Se[i]
  if(th[i]>ttab || th[i]<ttab2){
    cat("Hasil Keputusan uji t",nama[i],"adalah Tolak H0 \n")
  }else{
    cat("Hasil Keputusan uji t",nama[i],"adalah Terima H0 \n")
  }
}

## Koefisien Determinasi
R2<-JKR/JKT
derb<-(n-1)/(n-ncol(X)-1)
R2a<-1-((1-R2)*derb)

## Lihat hasil
tab.anova
th
R2
R2a


library(leaps)
library(olsrr)
library(corrplot)
library(lmtest)

## EDA
boxplot(RLB)
korrlb <- cor(RLB)
corrplot::corrplot(korrlb, method="number")
summary(RLB)

MLB <- lm(Y ~ x1 + x2 + x3 + x4, data = RLB)
summary(MLB)

## Pilih Variabel
## Pemilihan Model menggunakan variabel terpilih, dapat dilakukan dengan beberapa cara, yaitu: R-Square, AIC, BIC, MSE  

## Backward
full <- lm(Y~., data = RLB)
BE=step(full)
summary(BE)

## Forward Elimination
null <- lm(Y~1, data = RLB)
FS <- step(null, scope = list(upper=full), direction = "forward")
summary(FS)

## Stepwise
SW <- step(null, scope = list(upper=full), direction = "both")
summary(SW)

## Uji Asumsi Klasik

# multikolinieritas
library("car")
vif(MLB)

## Uji Normalitas
shapiro.test(MLB$residuals)
qqnorm(MLB$residuals)
qqline(MLB$residuals,col="blue", lwd=2)

## Uji Heteroskedastisitas
plot(MLB$residuals, MLB$fitted.values)
bptest(MLB)

## Uji autokorelasi
dwtest(MLB)

##Best Subset (mencari model terbaik) dari semua kemungkinan yang dibangun berdasarkan jumlah variabel independen
olsrr::ols_step_best_subset(MLB)
olsrr::ols_step_all_possible(MLB)

## Setelah diketahui kemungkinan variabel dan model terbaik, maka cek apakah variabelnya signifikan
MLB1 <- lm(y~x1 + x2 + x3 +x4, data=RLB)

## Langkah- langkah RLB
#1. Uji Linearitas dan Uji Multikolinearitas
#2. Pemodelan (full)
#3. Pemilihan variabel dan model terbaik
#4. Menguji signifikan dari model terbaik
#5. Uji Asumsi Klasik