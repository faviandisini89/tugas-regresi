RB <- read.csv("C:/Users/INFINIX/Documents/analisis_regresi/kumpulan_dataset/data coba 2.csv", sep = ";")

X <- RB[, -1]
Y <- RB[, 1]
Y <- as.matrix(Y)
n <- nrow(RB)
X1 <- matrix(1, n, 1)
Xgab <- cbind(X1, X)
Xgab <- as.matrix(Xgab)
Btopi <- solve(t(Xgab) %*% Xgab) %*% t(Xgab) %*% Y

JKR <- t(Btopi) %*% t(Xgab) %*% Y - n %*% mean(Y)^2
JKG <- t(Y) %*% Y - t(Btopi) %*% t(Xgab) %*% Y
JKT <- t(Y) %*% Y - n %*% mean(Y)^2
DBR <- 4 - 1
DBG <- n - 4
DBT <- n - 1
KTR <- JKR / DBR
KTG <- JKG / DBG
Fhit <- KTR / KTG
Ftabel <- qf((1 - 0.05), DBR, DBG)

tab.anova <- data.frame(
  Sumber = c("Regresi", "Galat", "Total"),
  db = c(DBR, DBG, DBR),
  JK = c(JKR, JKG, JKT),
  KT = c(KTR, KTG, NA),
  Fhit = c(Fhit, NA, NA),
  Ftab = c(Ftabel, NA, NA)
)

if (Fhit > Ftabel) {
  cat("Hasil keputusan Uji F adalah tolak H0", "\n")
} else {
  cat("Hasil keputusan Uji F adalah terima H0", "\n")
}

KTG <- as.numeric(KTG)
xx <- solve(t(Xgab) %*% Xgab)
rg <- sqrt(KTG * as.matrix(xx))
Se <- diag(rg)

Ttabel <- qt(1 - (0.05 / 2), DBG)
Ttabel2 <- qt(0.05 / 2, DBG)
th <- rep(0, nrow(Btopi))
for (i in 1:nrow(Btopi)) {
  th <- Btopi[i] / Se
  if (th[i] > Ttabel || th[i] < Ttabel2) {
    cat("Hasil keputusan Uji T adalah tolak H0", "\n")
  } else {
    cat("Hasil keputusan Uji T adalah terima H0", "\n")
  }
}

R2 <- (JKR / JKT) * 100
