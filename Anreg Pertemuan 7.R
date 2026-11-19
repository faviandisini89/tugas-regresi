RB <- read.csv("C:/Users/INFINIX/Documents/analisis_regresi/kumpulan_dataset/kuis 2 reglin.csv", sep = ";")

X <- RB[, -1]
Y <- RB[, 1]
Y <- as.matrix(Y)
n <- nrow(RB)
X1 <- matrix(1, n, 1)
Xgab <- cbind(X1, X)
Xgab <- as.matrix(Xgab)
betatopi <- solve(t(Xgab) %*% Xgab) %*% t(Xgab) %*% Y

## Uji Simultan/Uji F
JKR <- t(betatopi) %*% t(Xgab) %*% Y - n * mean(Y)^2
JKG <- t(Y) %*% Y - t(betatopi) %*% t(Xgab) %*% Y
JKT <- t(Y) %*% Y - n * mean(Y)^2

DBR <- ncol(X) ## k = banyak variabel bebas (p - 1)
DBG <- n - ncol(X) - 1 ## n - p
DBT <- n - 1
KTR <- JKR / DBR
KTG <- JKG / DBG
Fhitung <- KTR / KTG
Ftab <- qf((1 - 0.05), DBR, DBG)

tab.anova <- data.frame(
  Sumber = c("Regresi", "Galat", "Total"),
  db = c(DBR, DBG, DBT),
  JK = c(JKR, JKG, JKT),
  KT = c(KTR, KTG, NA),
  Fhit = c(Fhitung, NA, NA),
  Ftab = c(Ftab, NA, NA)
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

## Uji Parsial  ## i=iterasi
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
