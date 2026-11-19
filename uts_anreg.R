nilai_akhir <- c(88, 89, 90, 75, 87, 67, 88, 97, 90, 88, 85, 87, 89, 77, 78)
Y <- nilai_akhir
x1 <- c(8, 7.5, 9, 7, 8.6, 5, 7, 7.5, 8, 7.5, 6, 7, 7.5, 6, 8)          ## rata2 jam per minggu (waktu belajar)
x2 <- c(100, 90, 95, 95, 95, 80, 100, 100, 100, 100, 100, 95, 95, 95, 95)  ## % (kehadiran)
x3 <- c(90, 90, 90, 93, 94, 96, 90, 90, 90, 94, 94, 95, 96, 97, 98)       ## nilai tugas
x4 <- c(6, 6, 6, 7, 7, 7.5, 7.5, 7, 5, 5, 5, 6, 6, 6, 5)                  ## jam (durasi tidur)
x5 <- c(4, 4, 4.5, 4.5, 5, 5, 5, 5, 5, 4, 4, 4.5, 5, 5, 5)                ## rata2 jam per minggu (screentime)

Y <- as.matrix(Y)
X <- as.data.frame(cbind(waktu_belajar = x1, kehadiran = x2, nilai_tugas = x3, durasi_tidur = x4, screentime = x5))
boxplot(cbind(X, Y = Y))

n <- nrow(X)
X1 <- matrix(1, n, 1)
Xgab <- cbind(X1, as.matrix(X))

betatopi <- solve(t(Xgab) %*% Xgab) %*% t(Xgab) %*% Y

## Uji Simultan/Uji F
JKR <- t(betatopi) %*% t(Xgab) %*% Y - n * mean(Y)^2
JKG <- t(Y) %*% Y - t(betatopi) %*% t(Xgab) %*% Y
JKT <- t(Y) %*% Y - n * mean(Y)^2

DBR <- ncol(X)
DBG <- n - ncol(X) - 1
DBT <- n - 1
KTR <- JKR / DBR
KTG <- JKG / DBG
Fhitung <- KTR / KTG
Ftab <- qf((1 - 0.05), DBR, DBG)

tab.anova <- data.frame(
  Sumber = c("Regresi", "Galat", "Total"),
  db = c(DBR, DBG, DBT),
  JK = c(as.numeric(JKR), as.numeric(JKG), as.numeric(JKT)),
  KT = c(as.numeric(KTR), as.numeric(KTG), NA),
  Fhit = c(as.numeric(Fhitung), NA, NA),
  Ftab = c(as.numeric(Ftab), NA, NA)
)

if (Fhitung > Ftab) {
  cat("Hasil Keputusan Uji F adalah Tolak H0", "\n")
} else {
  cat("Hasil Keputusan Uji F adalah Terima H0", "\n")
}

## Standard error koefisien
KTG <- as.numeric(KTG)
xx <- solve(t(Xgab) %*% Xgab)
rg <- sqrt(KTG * diag(xx))
Se <- rg

## Uji Parsial
ttab <- qt((1 - (0.05 / 2)), DBG)
ttab2 <- qt(0.05 / 2, DBG)
nama <- c("Intercept", colnames(X))
th <- rep(0, nrow(betatopi))
for (i in 1:nrow(betatopi)) {
  th[i] <- betatopi[i] / Se[i]
  if (th[i] > ttab || th[i] < ttab2) {
    cat("Hasil Keputusan uji t", nama[i], "adalah Tolak H0 \n")
  } else {
    cat("Hasil Keputusan uji t", nama[i], "adalah Terima H0 \n")
  }
}

## Koefisien Determinasi
R2 <- JKR / JKT
derb <- (n - 1) / (n - ncol(X) - 1)
R2a <- 1 - ((1 - R2) * derb)

## Lihat hasil
tab.anova
th
R2
R2a


library(leaps)
library(olsrr)
library(corrplot)
library(lmtest)
library(car)

## EDA
RLB <- data.frame(Y = Y, X)
boxplot(RLB)
korrlb <- cor(RLB)
corrplot::corrplot(korrlb, method = "number")
summary(RLB)

MLB <- lm(Y ~ waktu_belajar + kehadiran + nilai_tugas + durasi_tidur + screentime, data = RLB)
summary(MLB)

## Pilih Variabel
## Backward
full <- lm(Y ~ ., data = RLB)
BE <- step(full)
summary(BE)

## Forward Elimination
null <- lm(Y ~ 1, data = RLB)
FS <- step(null, scope = list(upper = full), direction = "forward")
summary(FS)

## Stepwise
SW <- step(null, scope = list(upper = full), direction = "both")
summary(SW)

## Uji Asumsi Klasik
# multikolinieritas
vif(MLB)

## Uji Normalitas
shapiro.test(MLB$residuals)
qqnorm(MLB$residuals)
qqline(MLB$residuals, col = "blue", lwd = 2)

## Uji Heteroskedastisitas
plot(MLB$fitted.values, MLB$residuals)
bptest(MLB)

## Uji autokorelasi
dwtest(MLB)