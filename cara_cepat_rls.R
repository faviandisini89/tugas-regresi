# ============================================================
# ANALISIS REGRESI MENGGUNAKAN FUNGSI R
# ============================================================

# ------------------------------------------------------------
# 1. MEMASUKKAN DATA
# ------------------------------------------------------------

# Data X = Luas panen
Data.X<-c(676,456,134,789,768,567,668,690,700,678)

# Data Y = Jumlah produksi
Data.Y<-c(8900,7689,6745,9876,9871,5674,9076,8765,9874,9876)

# Menggabungkan Data X dan Data Y menjadi data frame
Dat.Gab<-as.data.frame(cbind(Data.X,Data.Y))

# Menampilkan data
Dat.Gab

# ------------------------------------------------------------
# 2. KORELASI
# ------------------------------------------------------------

# cor() digunakan untuk menghitung korelasi Pearson
#
# Hasilnya seharusnya sama dengan korelasi manual:
# korxy
korxy.2<-cor(Dat.Gab[,1],Dat.Gab[,2])

# Menampilkan hasil korelasi
korxy.2


# ------------------------------------------------------------
# 3. MEMBUAT MODEL REGRESI
# ------------------------------------------------------------

# lm() digunakan untuk membuat model regresi linear
#
# Data.Y = variabel Y
# Data.X = variabel X
#
# Bentuk model:
# Y = b0 + b1X
RLS<-lm(Data.Y~Data.X, data = Dat.Gab)


# ------------------------------------------------------------
# 4. MELIHAT HASIL ANALISIS REGRESI
# ------------------------------------------------------------

# summary() menampilkan hasil analisis regresi secara lengkap
summary(RLS)


# ------------------------------------------------------------
# 5. MELIHAT KOEFISIEN REGRESI
# ------------------------------------------------------------

# coef() digunakan untuk mengambil nilai:
# b0 = intercept
# b1 = koefisien regresi
coef(RLS)


# ------------------------------------------------------------
# 6. MELIHAT INTERVAL KEPERCAYAAN
# ------------------------------------------------------------

# confint() digunakan untuk menghitung
# interval kepercayaan koefisien regresi
confint(RLS)


# ------------------------------------------------------------
# 7. NILAI PREDIKSI
# ------------------------------------------------------------

# fitted() digunakan untuk mendapatkan nilai Y prediksi
fitted(RLS)


# ------------------------------------------------------------
# 8. RESIDUAL
# ------------------------------------------------------------

# resid() digunakan untuk mendapatkan residual
#
# Residual = Y aktual - Y prediksi
resid(RLS)


# ------------------------------------------------------------
# 9. TABEL ANOVA
# ------------------------------------------------------------

# anova() digunakan untuk menghasilkan tabel ANOVA
# berdasarkan model regresi
 


# ------------------------------------------------------------
# 10. SCATTER PLOT DAN GARIS REGRESI
# ------------------------------------------------------------

# Membuat scatter plot
plot(Data.X, Data.Y)

# Menambahkan garis regresi
abline(RLS)