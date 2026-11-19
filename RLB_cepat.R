library(leaps)
library(olsrr)
library(corrplot)
library(lmtest)

RLB <- read.csv("C:/Users/INFINIX/Documents/analisis_regresi/kumpulan_dataset/data coba 2.csv", sep = ";", header = T)

##EDA
boxplot(RLB)
korrlb <- cor(RLB)
corrplot::corrplot(korrlb, method="number")
summary(RLB)

MLB <- lm(Y ~ x1 + x2 + x3 + x4, data = RLB)
summary(MLB)

## Pilih Variabel
## Pemilihan Model menggunakan variabel terpilih, dapat dilakukan dengan beberapa cara, yaitu: R-Square, AIC, BIC, MSE  

## Backward
full <- lm(y~., data = RLB)
BE=step(full)
summary(BE)

## Forward Elimination
null <- lm(y~1, data = RLB)
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
qqline(MLB$residuals,col="blue", Twd=2)

## Uji Heteroskedastisitas
plot(MLB$residuals, MLB$fitted.values)
bptest(MLB)

## Uji autokorelasi
dwtest(MLB)
