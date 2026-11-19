Data.X<-c(18, 23, 25, 15, 8, 20, 12) # Jumlah Panggilan
Data.Y<-c(10, 11, 15, 8, 3, 11, 7)  # Jumlah Penjualan
Dat.Gab<-as.data.frame(cbind(Data.X, Data.Y))
n<-length(Dat.Gab[,1])

x.bar<-mean(Dat.Gab[,1])
y.bar<-mean(Dat.Gab[,2])
xy<-Dat.Gab[,1] * Dat.Gab[,2]
x2<-Dat.Gab[,1]^2
y2<-Dat.Gab[,2]^2

Sxy<-sum(xy) - (n * x.bar * y.bar)
Sxx<-sum(x2) - (n * x.bar^2)
Syy<-sum(y2) - (n * y.bar^2)

# Menghitung koefisien korelasi Pearson
korxy<-Sxy / (sqrt(Sxx) * sqrt(Syy))
plot(Data.X, Data.Y)

# Menghitung koefisien regresi
b1<-Sxy / Sxx
b0<-y.bar - (b1 * x.bar)

# Menghitung komponen ANOVA
JKR<-b1 * Sxy
JKT<-Syy
JKG<-JKT - JKR
KTR<-JKR / 1
KTG<-JKG / (n - 2)

Fhitung<-KTR / KTG
Ftabel<-qf(0.95, 1, n - 2)

# Membuat tabel ANOVA (Uji F)
tab.anova<-data.frame(
  Sumber= c("Regresi", "Galat", "Total"),
  db= c(1, n - 2, n - 1),
  JK= c(JKR, JKG, JKT),
  KT= c(KTR, KTG, NA),
  Fhitung= c(Fhitung, NA, NA),
  Ftabel= c(Ftabel, NA, NA)
)
print(tab.anova)

if (Fhitung > Ftabel) {
  print("Tolak H0")
} else {
  print("Terima H0")
}

# Menghitung Uji T untuk koefisien regresi 
sb1<-sqrt(KTG / Sxx)
Thitung<-b1 / sb1
Ttabel<-qt(0.975, n - 2)

if(abs(Thitung) > Ttabel){
  print("Tolak H0")
} else {
  print("Terima H0")
}

# Menghitung Koefisien Determinasi 
R2<-(JKR / JKT) * 100

RLS<-lm(Data.Y~Data.X,data = Dat.Gab)
summary(RLS)
abline(RLS)