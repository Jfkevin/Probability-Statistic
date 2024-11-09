library(readxl)
library(moments)
Data <- read_excel("C:/Users/JFKevin/Downloads/ProjecProbstat/DataProbstat.xlsx")
orderedData <- Data[order(Data$garis_2022),]

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")

garis <- Data$'garis_2022'
index <- Data$'index_2022'
wilayah <- Data$`Nama Wilayah`

modus <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

expense <- function(idx, gk) {
  exp <- (1 - sqrt(idx)) * gk
  return (exp)
}

mean_garis <- mean(garis)
median(garis)
var(garis)
sd_garis <- sd(garis)
quantile(garis)
IQR(garis)

mean_index <- mean(index)
median(index)
modus(index)
var(index)
sd_index <- sd(index)
quantile(index)
IQR(index)

skewness(garis)
kurtosis(garis)
skewness(index)
kurtosis(index)

cor(garis, index)
cov(garis, index)
cor(garis, expense(garis, index))
cov(garis, expense(garis, index))



hist(Data$garis_2022, breaks = 20)
hist(Data$index_2022, breaks = 20)

plot(Data$garis_2022, Data$index_2022)

library(ggplot2)
colors <- c("Garis Kemiskinan" = 'red', "Keparahan Kemiskinan" = 'blue')
ggplot() +
  geom_point(data = orderedData, aes(reorder(`Nama Wilayah`, garis_2022), garis_2022, color = 'Garis Kemiskinan', group = 1)) +
  geom_point(data = orderedData, aes(reorder(`Nama Wilayah`, garis_2022), expense(index_2022, garis_2022), color = 'Keparahan Kemiskinan', group = 2)) +
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
  labs(title = "Garis Kemiskinan Tingkat Kabupaten",
    x = "Nama Wilayah",
    y = "Garis Kemiskinan/Tingkat Keparahan Kemiskinan"
    ) +
  scale_color_discrete(name="")


shapiro.test(Data$garis_2022)
shapiro.test(Data$index_2022)
shapiro.test(expense(index, garis))


# uji inferensi 1 populasi
# H0 = rata-rata Garis Kemiskinan >= 450000
t.test(Data$garis_2022, alternative = 'less', mu = 450000, conf.level = 0.95)

#Uji inferensi 2 populasi
# Ho = perbedaan rata-rata nilai garis kemiskinan dengan nilai keparahan kemiskinan kurang dari 250000
# Ho = garis_kemiskinan - keparahan_kemiskinan >= 250000
t.test(Data$garis_2022, expense(Data$index_2022, Data$garis_2022), paired = TRUE, 
       alternative = "less", mu=250000, conf.level = 0.95)


# regresi
x <- orderedData$garis_2022
y <- orderedData$index_2022
linear_model = lm(y~x)
print(linear_model)

plot(x, y)
abline(linear_model)
summary(linear_model)

dataEstimasi <- c(500000, 600000, 700000)
a <- data.frame(x=dataEstimasi)
hasilEstimasi <- predict(linear_model, a)
print(hasilEstimasi)

residu <- resid(linear_model)
print(residu)

garis_ranges <- cut(orderedData$garis_2022, breaks = c(300000, 400000, 500000, 600000, 700000, 800000, 900000), include.lowest = TRUE)
index_ranges <- cut(orderedData$index_2022, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2), include.lowest = TRUE)

frequency_table <- table(garis_ranges, index_ranges)
frequency_df <- as.data.frame.matrix(frequency_table)

print (frequency_df)

# H0 : garis kemiskinan dan index keparahan kemiskinan saling independent
# Ha : garis kemiskinan dan index keparahan kemiskinan saling dependent
chisq.test(frequency_df)
# critical chi square value 0.05 25 = 37.652
# chi square > critical chi square dan p-value < a, maka H0 ditolak
