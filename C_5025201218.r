# 1a. Carilah Standar Deviasi dari data selisih pasangan pengamatan tabel diatas
df.rats <- data.frame(x = c(78,75,67,77,70,72,78,74,77),
                      y = c(100,95,70,90,90,90,89,90,100))
mean.x <- with(df.rats, mean(x))
mean.y <- with(df.rats, mean(y, na.rm = T))
means.rats <- c(mean.x,mean.y)
df.rats2 <- rbind(df.rats, means.rats)
row.names(df.rats2)[dim(df.rats2)[1]] <- "means"
# =================
#Standard deviation
sd.x <- with(df.rats, sd(x))
sd.y <- with(df.rats, sd(y, na.rm = T))
sd.rats <- c(sd.x,sd.y)
df.rats2 <- rbind(df.rats2, sd.rats)
row.names(df.rats2)[dim(df.rats2)[1]] <- "SD"
df.rats2
#               x          y
# 1     78.000000 100.000000
# 2     75.000000  95.000000
# 3     67.000000  70.000000
# 4     77.000000  90.000000
# 5     70.000000  90.000000
# 6     72.000000  90.000000
# 7     78.000000  89.000000
# 8     74.000000  90.000000
# 9     77.000000 100.000000
# means 74.222222  90.444444
# SD     3.865805   8.833333
# ==================
df.rats3 <- data.frame(y-x)
df.rats3
#   y...x
# 1    22
# 2    20
# 3     3
# 4    13
# 5    20
# 6    18
# 7    11
# 8    16
# 9    23
# standard deviation
sdeviation <- sd.y-sd.x
# 4.967529
smean <- mean.y-mean.x
# 16.22222

# 1b. carilah nilai t (p-value)
x
# [1] 78 75 67 77 70 72 78 74 77
y
# [1] 100  95  70  90  90  90  89  90 100
z = c(y-x)
z
# [1] 22 20  3 13 20 18 11 16 23
t.test(z)
# 	One Sample t-test
# ======================================
# data:  z
# t = 7.6525, df = 8, p-value = 6.003e-05
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#  11.33381 21.11064
# sample estimates:
# mean of x 
#  16.22222 
# ====================================

# 1c. tentukanlah apakah terdapat pengaruh yang signifikan secara statistika
# dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan
# aktivitas 𝐴 jika diketahui tingkat signifikansi 𝛼 = 5% serta H0 : “tidak ada
# pengaruh yang signifikan secara statistika dalam hal kadar saturasi
# oksigen , sebelum dan sesudah melakukan aktivitas 𝐴”
# jawab
# dapat dapat dilihat bahwa nilai standar deviasi lebih kecil dari pada nilai rata – rata. Hal ini
# menunjukan bahwa variabel selisih y dan x  yang di gunakan oleh penelitian
# ini tidak bervariasi. Nilai rata-rata sebesar 16.22222 lebih
# mendekati kearah nilai maximum, sehingga nilai rata-rata struktur modal
# dalam sampel penelitian ini cukup tinggi. Tingginya nilai selisih variabel X dan Y menunjukkan
# tingkat pengaruh yang signifikan secara statistika terhadap kadar saturasi oksigen

# 2a. Apakah Anda setuju dengan klaim tersebut?
# hypothesized value 
mu0 = 20000
# sample mean 
xbar = 23500
# population standard deviation
sigma = 3900
# sample size 
n=100
# test statistic
z = (xbar-mu0)/(sigma/sqrt(n));
z
# [1] 8.974359

# asumsi significant level = 0.025
# .025 significance level
alpha = .05
# right tail critical value
z.alpha = qnorm(1-alpha)
# left tail critical value 
-z.alpha
# [1] -1.959964

# 2b. Jelaskan maksud dari output yang dihasilkan!
# the z-test statistics according to the information that we have from the statement.
# we use z-statistics because we know the mean μ and standard deviation σ, also we know that the sample size ≥30.
# Then, we calculate the left critical value.
# Now, we can conclude that the test statistic 8.974359 more than 
# the critical value of -1.959964. Consequently, at .05 significance level, 
# we accept the claim that mean a car is above 20000 km/year away.

# 2c. Buatlah kesimpulan berdasarkan P-Value yang dihasilkan!
pval = pnorm(z)   
# [1] 1
pval
# P value close to 1 suggests no difference between the groups other than due to chance.

# 3a. H0 dan H1
nbandung=19
meanbandung=3.64
sdbandung=1.67
nbali=27
meanbali=2.79
sdbali=1.32
alpha=0.05
# Null hypothesis and test statistic
# H0:μ1−μ2=0 HA:μ1−μ2≠0
delta_0 <- 0
sigma_sq_bd <- sdbandung
sigma_sq_bl <- sdbali
n_bd <- nbandung
n_bl <- nbali

# 3b. Hitung Sampel Statistik
z_stat <- (meanbandung - meanbali - delta_0) / 
  sqrt(sigma_sq_bd / n_bd + sigma_sq_bl / n_bl)
z_stat
# [1] 2.298274

# 3c. Lakukan Uji Statistik (df =2)
pval = 2*pt(z_stat, 2)
# [1] 1.851676
# Since it turns out to be greater than the .05 
# significance level, we do not reject the null hypothesis

# 3d. Nilai Kritikal
# .05 significance level
alpha = .05                                            
# per-one tail .025 significance level
# Two-Tailed 0.05 significance level
t.half.alpha = qt(1-alpha/2, df=2)                   
c(-t.half.alpha, t.half.alpha)  
# [1] -4.302653  4.302653

# 3e. Keputusan
# The test statistic 1.851676 lies between the critical values -4.302653 and 4.302653.
# Hence, at .05 significance level, we do not reject the null hypothesis 
# that the mean Bandung and Bali does not differ.

# 3f. Kesimpulan
# we calculate the z-test statistics according to the information that we have from the Example 2. 
# In this case, we use z-statistics because we know the mean μ and standard deviation σ
# z-test = 2.298274
# Then we calculate pval to compute two tail p-value df the statistics Instead of using the critical value
# assume df=2
# As it turns out to be greater than the .05 significance level, we do not reject the null hypothesis
# The test statistic 1.851676 lies between the critical values -4.302653 and 4.302653.
# Hence, at .05 significance level, we do not reject the null hypothesis 
# that the mean Bandung and Bali does not differ.

