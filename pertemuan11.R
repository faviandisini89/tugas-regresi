## ----------  REGRESI ROBUST ----------

library(car)
library(MASS)
library(foreign)
library(lmtest)

Drop <- read.csv("D:/#kumpulan_dataset/data robust.csv", sep = ";", header = TRUE)

## EDA
summary(Drop)
boxplot(Drop)

## Deteksi Outlier
MRLB <- lm(Y ~ X1 +X2 + X3 + X4 + X5, data=Drop)
cook <- cooks.distance(MRLB)
plot(cook, ylab="Cooks distances")
abline(h = 4/nrow(Drop), col = "red", lty = 2)
## Identifikasi observasi influential
pencilan <- which(cook > 4/nrow(Drop))
Drop[pencilan,]

MRLB <- lm(Y ~ X1 +X2 + X3 + X4 + X5, data=Drop)
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
Rob <- rlm(Y ~ X1 +X2 + X3 + X4 + X5, data=Drop, psi=psi.huber,maxit=1000, method="M")
summary(Rob)
V_huber <- sandwich::vcovHC(Rob, type = "HC0")

## Uji Simultan
car::linearHypothesis(
  Rob,
  c("X1=0","X2=0","X3=0","X4=0","X5=0"),
  vcov. = V_huber
)

## Fungsi Uji Parsial
uji_parsial <- function(model, V) {
  beta <- coef(model)
  se <- sqrt(diag(V))
  z <- beta / se
  p_value <- 2 * pnorm(abs(z), lower.tail = FALSE)
  
  hasil <- data.frame(
    Koefisien = beta,
    Std_Error = se,
    Z_hitung = z,
    P_value = p_value
  )
  
  return(hasil)
}

## Hasil Uji Parsial
uji_parsial(Rob, V_huber)