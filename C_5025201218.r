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

# ================================================================================================================================
# ================================================================================================================================

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

# ================================================================================================================================
# ================================================================================================================================

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

# Dataset no 4
# kucing1 <- c(19, 18.6, 18.3, 18, 18.2, 18.6, 18.5, 18.2, 18.4, 18.9, 19.9, 18.5, 16.9, 18, 17.3, 17.8, 20, 19, 19.2, 18.9 ,17.5, 18.1, 18, 18.1, 17.4, 17.9, 17.4, 16.7, 19.7, 19.3, 19, 19.4, 19.8, 19.3, 18.5)
# kucing2 <- c(18.3, 17.9, 17.6, 17.3, 17.5, 17.9, 17.8, 17, 17.7, 18.2, 19.2, 17.8, 16.2, 17.3, 16.6, 17.1, 19.3, 18.3,18.5,18,16.8,17.2,17.3,17.4,16.7,17.2,16.7,16.2,19,18.6,18.3,18.7,19.1,18.6,17.8)
# kucing3 <- c(18, 18.6, 18.3, 18, 18.2, 18.2, 18.5, 18.2, 19.2, 18.5, 19.9, 18.5, 16.9, 18, 17, 17.2, 20, 19, 19.2, 18.9, 17.5, 18.1, 18, 18.1, 17.4, 17.9, 17.4, 16.5, 19.7, 19, 19, 19.7, 19.8, 19.3, 17)

# ================================================================================================================================
# ================================================================================================================================

# 4a. Buatlah masing masing jenis spesies menjadi 3 subjek "Grup" (grup 1,grup
# 2,grup 3). Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan
# lihat apakah ada outlier utama dalam homogenitas varians.
kucing1 <- c(19, 18.6, 18.3, 18, 18.2, 18.6, 18.5, 18.2, 18.4, 18.9, 19.9, 18.5, 16.9, 18, 17.3, 17.8, 20, 19, 19.2, 18.9 ,17.5, 18.1, 18, 18.1, 17.4, 17.9, 17.4, 16.7, 19.7, 19.3, 19, 19.4, 19.8, 19.3, 18.5)
kucing2 <- c(18.3, 17.9, 17.6, 17.3, 17.5, 17.9, 17.8, 17, 17.7, 18.2, 19.2, 17.8, 16.2, 17.3, 16.6, 17.1, 19.3, 18.3, 18.5, 18, 16.8, 17.2, 17.3, 17.4, 16.7, 17.2, 16.7, 16.2, 19,1 8.6, 18.3, 18.7, 19.1, 18.6, 17.8)
kucing3 <- c(18, 18.6, 18.3, 18, 18.2, 18.2, 18.5, 18.2, 19.2, 18.5, 19.9, 18.5, 16.9, 18, 17, 17.2, 20, 19, 19.2, 18.9, 17.5, 18.1, 18, 18.1, 17.4, 17.9, 17.4, 16.5, 19.7, 19, 19, 19.7, 19.8, 19.3, 17)
qqnorm(kucing1)
qqnorm(kucing2)
qqnorm(kucing3)
kucing <- data.frame(grup1 = kucing1, grup2 = kucing2, grup3 = kucing3)
my_data <- read.delim(file.choose())
my_data
#     Group Length
# 1       1   19.0
# 2       1   18.6
# 3       1   18.3
# 4       1   18.0
# 5       1   18.2
# 6       1   18.6
# 7       1   18.5
# 8       1   18.2
# 9       1   18.4
# 10      1   18.9
# 11      1   19.9
# 12      1   18.5
# 13      1   16.9
# 14      1   18.0
# 15      1   17.3
# 16      1   17.8
# 17      1   20.0
# 18      1   19.0
# 19      1   19.2
# 20      1   18.9
# 21      1   17.5
# 22      1   18.1
# 23      1   18.0
# 24      1   18.1
# 25      1   17.4
# 26      1   17.9
# 27      1   17.4
# 28      1   16.7
# 29      1   19.7
# 30      1   19.3
# 31      1   19.0
# 32      1   19.4
# 33      1   19.8
# 34      1   19.3
# 35      1   18.5
# 36      2   18.3
# 37      2   17.9
# 38      2   17.6
# 39      2   17.3
# 40      2   17.5
# 41      2   17.9
# 42      2   17.8
# 43      2   17.0
# 44      2   17.7
# 45      2   18.2
# 46      2   19.2
# 47      2   17.8
# 48      2   16.2
# 49      2   17.3
# 50      2   16.6
# 51      2   17.1
# 52      2   19.3
# 53      2   18.3
# 54      2   18.5
# 55      2   18.0
# 56      2   16.8
# 57      2   17.2
# 58      2   17.3
# 59      2   17.4
# 60      2   16.7
# 61      2   17.2
# 62      2   16.7
# 63      2   16.2
# 64      2   19.0
# 65      2   18.6
# 66      2   18.3
# 67      2   18.7
# 68      2   19.1
# 69      2   18.6
# 70      2   17.8
# 71      3   18.0
# 72      3   18.6
# 73      3   18.3
# 74      3   18.0
# 75      3   18.2
# 76      3   18.2
# 77      3   18.5
# 78      3   18.2
# 79      3   19.2
# 80      3   18.5
# 81      3   19.9
# 82      3   18.5
# 83      3   16.9
# 84      3   18.0
# 85      3   17.0
# 86      3   17.2
# 87      3   20.0
# 88      3   19.0
# 89      3   19.2
# 90      3   18.9
# 91      3   17.5
# 92      3   18.1
# 93      3   18.0
# 94      3   18.1
# 95      3   17.4
# 96      3   17.9
# 97      3   17.4
# 98      3   16.5
# 99      3   19.7
# 100     3   19.0
# 101     3   19.0
# 102     3   19.7
# 103     3   19.8
# 104     3   19.3
# 105     3   17.0
my_data$Group <- ordered(my_data$Group,
                         levels = c("1", "2", "3"))
levels(my_data$Group)
# [1] "1" "2" "3"
library(dplyr)
group_by(my_data, Group) %>%
    summarise(
       count = n(),
       mean = mean(Length, na.rm = TRUE),
       sd = sd(Length, na.rm = TRUE)
    )
# A tibble: 3 x 4
#   Group count  mean    sd
#   <ord> <int> <dbl> <dbl>
# 1 1        35  18.5 0.836
# 2 2        35  17.7 0.834
# 3 3        35  18.4 0.921
library("ggpubr")
ggboxplot(my_data, x = "Group", y = "Length", 
          color = "Group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("1", "2", "3"),
          ylab = "Length", xlab = "Kucing")
# There were no extreme outliers

# 4b. carilah atau periksalah Homogeneity of variances nya , Berapa nilai p yang
# didapatkan? , Apa hipotesis dan kesimpulan yang dapat diambil ?
leveneTest(Length ~ Group, data = my_data)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   2  0.1702 0.8438
#       102 
# From the output above we can see that the p-value is not less than the significance level of 0.05. 
# This means that there is no evidence to suggest that the variance across groups is statistically significantly different. 
# Therefore, we can assume there is no the homogeneity of variances in the different treatment groups.

# 4c. Untuk uji ANOVA (satu arah), buatlah model linier dengan Panjang versus
# Grup dan beri nama model tersebut model 1.
model  <- lm(Length ~ Group, data = my_data)
ggqqplot(residuals(model))

# 4d. Dari Hasil Poin C, Berapakah nilai-p ? , Apa yang dapat Anda simpulkan
# dari H0?
shapiro.test(residuals(model))
# 	Shapiro-Wilk normality test
# data:  residuals(model)
# W = 0.98017, p-value = 0.1176
# The conclusion above, is supported by the Shapiro-Wilk test on the ANOVA residuals (W = 0.98017, p = 0.1176) 
# which finds no indication that normality is violated.

# 4e. Verifikasilah jawaban model 1 dengan Post-hoc test Tukey HSD, dari nilai p
# yang didapatkan apakah satu jenis kucing lebih panjang dari yang lain? Jelaskan.
submodel <- aov(Length~Group, data=data)
summary(submodel)
#              Df Sum Sq Mean Sq F value Pr(>F)   
# Group         2  10.61   5.307   7.098 0.0013 **
# Residuals   102  76.27   0.748                  
# ---
# The observed p-value from the ANOVA table is less than 0.05, 
# indicating that there is enough evidence to conclude that the group means are not equal.

# 4f. Visualisasikan data dengan ggplot2
ggplot(my_data, aes(Group, Length, colour = Group)) + geom_point()

# ================================================================================================================================
# ================================================================================================================================

# 5a. Buatlah plot sederhana untuk visualisasi data
light_data <- read.csv(file.choose())
light_data
#    Glass Temp Light
# 1      A  100   580
# 2      A  100   568
# 3      A  100   570
# 4      B  100   550
# 5      B  100   530
# 6      B  100   579
# 7      C  100   546
# 8      C  100   575
# 9      C  100   599
# 10     A  125  1090
# 11     A  125  1087
# 12     A  125  1085
# 13     B  125  1070
# 14     B  125  1035
# 15     B  125  1000
# 16     C  125  1045
# 17     C  125  1053
# 18     C  125  1066
# 19     A  150  1392
# 20     A  150  1380
# 21     A  150  1386
# 22     B  150  1328
# 23     B  150  1312
# 24     B  150  1299
# 25     C  150   867
# 26     C  150   904
# 27     C  150   889
ggplot(data = light_data) +  geom_point(mapping = aes(x = Temp, y = Light))

# 5b. Lakukan uji ANOVA dua arah
res.aov2 <- aov(Light ~ Glass + Temp, data = light_data)
summary(res.aov2)
#             Df  Sum Sq Mean Sq F value   Pr(>F)    
# Glass        2  150865   75432   3.557   0.0451 *  
# Temp         1 1779756 1779756  83.932 3.89e-09 ***
# Residuals   23  487710   21205                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# From the ANOVA table we can conclude that both Glass and Light are statistically significant. 
# Light is the most significant factor variable. These results would lead us to believe that changing delivery methods (Glass) 
# or the Light of on the data, will impact significantly.

# 5c. Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk
# setiap perlakuan (kombinasi kaca pelat muka dan suhu operasi)
temp_data <- light_data
mean.glass <- with(temp_data, mean(Glass))
mean.temp <- with(temp_data, mean(Temp))
mean.light <- with(temp_data, mean(Light))
mean.all <- c(mean.glass, mean.temp, mean.light)
temp_data2 <- rbind(temp_data, mean.all)
row.names(temp_data2)[dim(temp_data2)[1]] <- "means"
# ------------------------------------------------------------
sd.glass <- with(temp_data, sd(Glass))
sd.temp <- with(temp_data, sd(Temp, na.rm = T))
sd.light <- with(temp_data, sd(Light, na.rm = T))
sd.temp_data <- c(sd.glass,sd.temp,sd.light)
temp_data2 <- rbind(temp_data2,sd.temp_data)
row.names(temp_data2)[dim(temp_data2)[1]] <- "SD"
temp_data2
#       Glass      Temp     Light
# 1         A 100.00000  580.0000
# 2         A 100.00000  568.0000
# 3         A 100.00000  570.0000
# 4         B 100.00000  550.0000
# 5         B 100.00000  530.0000
# 6         B 100.00000  579.0000
# 7         C 100.00000  546.0000
# 8         C 100.00000  575.0000
# 9         C 100.00000  599.0000
# 10        A 125.00000 1090.0000
# 11        A 125.00000 1087.0000
# 12        A 125.00000 1085.0000
# 13        B 125.00000 1070.0000
# 14        B 125.00000 1035.0000
# 15        B 125.00000 1000.0000
# 16        C 125.00000 1045.0000
# 17        C 125.00000 1053.0000
# 18        C 125.00000 1066.0000
# 19        A 150.00000 1392.0000
# 20        A 150.00000 1380.0000
# 21        A 150.00000 1386.0000
# 22        B 150.00000 1328.0000
# 23        B 150.00000 1312.0000
# 24        B 150.00000 1299.0000
# 25        C 150.00000  867.0000
# 26        C 150.00000  904.0000
# 27        C 150.00000  889.0000
# means  <NA> 125.00000  940.1852
# SD     <NA>  20.80126  304.9798