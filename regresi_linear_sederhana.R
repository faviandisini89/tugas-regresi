# Data X = Luas panen
Data.X<-c(676,456,134,789,768,567,668,690,700,678)

# Data Y = Jumlah produksi
Data.Y<-c(8900,7689,6745,9876,9871,5674,9076,8765,9874,9876)

# Menggabungkan Data X dan Data Y menjadi data frame
Dat.Gab<-as.data.frame(cbind(Data.X,Data.Y))

# Menampilkan data
Dat.Gab

# Menghitung jumlah observasi
n<-length(Dat.Gab[,1])

# Menghitung rata-rata X
x.bar<-mean(Dat.Gab[,1])

# Menghitung rata-rata Y
y.bar<-mean(Dat.Gab[,2])

# Menghitung X dikali Y
xy<-Dat.Gab[,1]*Dat.Gab[,2]

# Menghitung X kuadrat
x2<-Dat.Gab[,1]^2

# Menghitung Y kuadrat
y2<-Dat.Gab[,2]^2

# 4. MENGHITUNG Sxy, Sxx, DAN Syy
Sxy<-(sum(xy))-(n*x.bar*y.bar)
Sxx<-sum(x2)-(n*x.bar^2)
Syy<-sum(y2)-(n*y.bar^2)

# 5. MENGHITUNG KORELASI SECARA MANUAL
korxy<-Sxy/(sqrt(Sxx)*sqrt(Syy))
plot(Data.X, Data.Y)
b1<-Sxy/Sxx
b0<-y.bar-(b1*x.bar)

# 9. MENGHITUNG JUMLAH KUADRAT GALAT
JKG<-Syy-(b1*Sxy)
KTG<-JKG/(n-2)

# 10. MENGHITUNG KOMPONEN ANOVA
JKR<-b1*Sxy
JKT<-Syy
JKG<-JKT-JKR
KTR<-JKR/1
KTG<-JKG/(n-2)

# 11. MENGHITUNG F HITUNG DAN F TABEL
Fhitung<-KTR/KTG
ftabel<-qf(0.95,1,8)

# 12. TABEL ANOVA
tab.anova<-data.frame(
  Sumber=c("Regresi","Galat","Total"),
  db=c(1,n-2,n-1),
  JK=c(JKR,JKG,JKT),
  KT=c(KTR,KTG,NA),
  Fhit=c(Fhitung,NA,NA),
  Ftab=c(ftabel,NA,NA)
)

# 13. UJI F
if(Fhitung>ftabel)
{
  print("Tolak H0")
}else{
  print("Terima H0")
}

sb1<-sqrt(KTG/Sxx)
Thit<-b1/sb1
Ttab<-qt(0.975,8)


# 15. KEPUTUSAN UJI t
if(Thit>Ttab)
{
  print("Tolak H0")
}else{
  print("Terima H0")
}

# 16. MENGHITUNG KOEFISIEN DETERMINASI
R2<-(JKR/JKT)*100

# 17. MENGHITUNG R2 DARI KORELASI
R2.2<-korxy^2