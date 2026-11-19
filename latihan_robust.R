## ----------  REGRESI ROBUST ----------

library(car)
library(MASS)
library(foreign)
library(lmtest)

Drop <- read.csv("D:/#kumpulan_dataset/latihan 1.csv", sep = ";", header = TRUE)
Drop <- Drop[,-1]

## EDA
summary(Drop)
boxplot(Drop)

## Deteksi Outlier
MRLB <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6, data=Drop)
cook <- cooks.distance(MRLB)
plot(cook, ylab="Cooks distances")
abline(h = 4/nrow(Drop), col = "red", lty = 2)
## Identifikasi observasi influential
pencilan <- which(cook > 4/nrow(Drop))
Drop[pencilan,]

## Pemodelan OLS dan Asumsi Klasik
MRLB <- lm(Y ~ X1 +X2 + X3 + X4 + X5 + X6, data=Drop)
summary(MRLB)
## Uji Normalitas
shapiro.test(MRLB$residuals)
qqnorm(MRLB$residuals)
qqline(MRLB$residuals)
## Uji Heteroskedastisitas
bptest(MRLB)
## Uji Multikolinieritas
vif(MRLB)
## Uji Autokorelasi
dwtest(MRLB)

## Pemodelan dengan Robust (Estimator Huber)
Rob_huber <- rlm(Y ~ X1 +X2 + X3 + X4 + X5 + X6, data=Drop, psi=psi.huber,maxit=1000, method="M")
summary(Rob_huber)
V_huber <- sandwich::vcovHC(Rob_huber, type = "HC0")

## Pemodelan dengan Robust (Estimator Bisquare)
Rob_bisquare <- rlm(Y ~ X1 +X2 + X3 + X4 + X5 + X6, data=Drop, psi=psi.bisquare,maxit=1000, method="M")
summary(Rob_bisquare)
V_bisquare <- sandwich::vcovHC(Rob_bisquare, type = "HC0")

## Uji Simultan (Estimator Huber)
car::linearHypothesis(
  Rob_huber,
  c("X1=0","X2=0","X3=0","X4=0","X5=0","X6=0"),
  vcov. = V_huber
)

## Uji Simultan (Estimator Bisquare)
car::linearHypothesis(
  Rob_bisquare,
  c("X1=0","X2=0","X3=0","X4=0","X5=0","X6=0"),
  vcov. = V_bisquare
)


## Fungsi Uji Parsial (Estimator Huber)
uji_parsial_huber <- function(model, V) {
  beta <- coef(model)
  se <- sqrt(diag(V))
  z <- beta / se
  p_value <- 2 * pnorm(abs(z), lower.tail = FALSE)
  
  hasil_huber <- data.frame(
    Koefisien = beta,
    Std_Error = se,
    Z_hitung = z,
    P_value = p_value
  )
  
  return(hasil_huber)
}
## Hasil Uji Parsial
uji_parsial_huber(Rob_huber, V_huber)


## Fungsi Uji Parsial (Estimator Bisquare)
uji_parsial_bisquare <- function(model, V) {
  beta <- coef(model)
  se <- sqrt(diag(V))
  z <- beta / se
  p_value <- 2 * pnorm(abs(z), lower.tail = FALSE)
  
  hasil_bisquare <- data.frame(
    Koefisien = beta,
    Std_Error = se,
    Z_hitung = z,
    P_value = p_value
  )
  
  return(hasil_bisquare)
}

## Hasil Uji Parsial
uji_parsial_bisquare(Rob_bisquare, V_bisquare)

#MSE Rob_Huber
ehuber<-Rob_huber$residuals
MSEhuber<-mean(ehuber^2)

ebi<-Rob_bisquare$residuals
MSEbi<-mean(ebi^2)

nilai.MSE<-data.frame(
  model=c("Huber", "Tukey-Biweight"),
  MSE=c(MSEhuber, MSEbi)
)
