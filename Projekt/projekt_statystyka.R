# Przygotowanie danych do analizy

library(ggplot2)
library(ggcorrplot)
library(moments)
library(nortest)
library(grid)
library(gridExtra)
library(hrbrthemes)


filePath <- "diamonds.csv"
data <- read.csv(filePath)
dataSet <- as.data.frame(cbind(data$carat, data$depth, data$table, data$price))
colnames(dataSet)[1] <- "carat"
colnames(dataSet)[2] <- "depth"
colnames(dataSet)[3] <- "table"
colnames(dataSet)[4] <- "price"
attach(dataSet)
write.csv(dataSet, "./dataSet.csv", row.names = TRUE)

# zmienna carat

histogram <- ggplot(dataSet, aes(x = carat), xlab="carat") + 
  geom_histogram(binwidth=0.15, color="black", fill="green") + 
  theme_ipsum(base_family = 'sans') + 
  labs(title="Histogram 'carat'")

boxplot <- ggplot(dataSet, aes(x = carat), xlab="carat") + 
  geom_boxplot(color="black", fill="green", alpha=0.5, outlier.size=5) + 
  theme_ipsum(base_family = 'sans') + 
  labs(title="Boxplot 'carat'")
grid.arrange(histogram, boxplot, nrow=1)

row1 <- data.frame(Minimum=min(carat), Maksimum=max(carat),
                   Srednia=mean(carat), Mediana=median(carat),
                   Wariancja=var(carat), Odch_Stn.=sd(carat), 
                   Skosnosc=skewness(carat), Kurtoza=kurtosis(carat))
print(row1, row.names = FALSE)

# zmienna depth

histogram <- ggplot(dataSet, aes(x = depth), xlab="depth") + 
  geom_histogram(binwidth=1.2, color="black", fill="red") + 
  theme_ipsum(base_family = 'sans') + 
  labs(title="Histogram 'depth'")

boxplot <- ggplot(dataSet, aes(x = depth), xlab="depth") + 
  geom_boxplot(color="black", fill="red", alpha=0.5, outlier.size=5) + 
  theme_ipsum(base_family = 'sans') + 
  labs(title="Boxplot 'depth'")
grid.arrange(histogram, boxplot, nrow=1)

row1 <- data.frame(Minimum=min(depth), Maksimum=max(depth),
                   Srednia=mean(depth), Mediana=median(depth),
                   Wariancja=var(depth), Odch_Stn.=sd(depth), 
                   Skosnosc=skewness(depth), Kurtoza=kurtosis(depth))
print(row1, row.names = FALSE)

# zmienna table

histogram <- ggplot(dataSet, aes(x = table), xlab="table") + 
  geom_histogram(binwidth=1.5, color="black", fill="blue") + 
  theme_ipsum(base_family = 'sans') + 
  labs(title="Histogram 'table'")

boxplot <- ggplot(dataSet, aes(x = table), xlab="table") + 
  geom_boxplot(color="black", fill="blue", alpha=0.5, outlier.size=5) + 
  theme_ipsum(base_family = 'sans') + 
  labs(title="Boxplot 'table'")
grid.arrange(histogram, boxplot, nrow=1)

row1 <- data.frame(Minimum=min(table), Maksimum=max(table),
                   Srednia=mean(table), Mediana=median(table),
                   Wariancja=var(table), Odch_Stn.=sd(table), 
                   Skosnosc=skewness(table), Kurtoza=kurtosis(table))
print(row1, row.names = FALSE)

# zmienna price

histogram <- ggplot(dataSet, aes(x = price), xlab="price") + 
  geom_histogram(binwidth=550, color="black", fill="yellow") + 
  theme_ipsum(base_family = 'sans') + 
  labs(title="Histogram 'price'")

boxplot <- ggplot(dataSet, aes(x = price), xlab="price") + 
  geom_boxplot(color="black", fill="yellow", alpha=0.5, outlier.size=5) + 
  theme_ipsum(base_family = 'sans') + 
  labs(title="Boxplot 'price'")
grid.arrange(histogram, boxplot, nrow=1)

row1 <- data.frame(Minimum=min(price), Maksimum=max(price),
                   Srednia=mean(price), Mediana=median(price),
                   Wariancja=var(price), Odch_Stn.=sd(price), 
                   Skosnosc=skewness(price), Kurtoza=kurtosis(price))
print(row1, row.names = FALSE)

# Macierze kowariancji i korelacji dla wszystkich zmiennych

cov(dataSet[,c(1,2,3,4)])
cor(dataSet[,c(1,2,3,4)])

ggcorrplot(cor(dataSet[,c(1,2,3,4)]))


# Zaleznosc miedzy price i carat

plotle <- ggplot(dataSet, aes(x = price, y = carat), xlab="price", 
                 ylab="carat") + geom_point() + 
  geom_smooth(formula = y ~ x, method = "lm") + 
  theme_ipsum(base_family = 'sans') + 
  labs(title="Zaleznosc 'price' od 'carat'")
plotle

reg <- lm(price ~ carat)
summary(reg)

# Zaleznosc miedzy price i carat

plotle <- ggplot(dataSet, aes(x = depth, y = table), xlab="depth", 
                 ylab="table") + geom_point() + 
  geom_smooth(formula = y ~ x, method = "lm") + 
  theme_ipsum(base_family = 'sans') + 
  labs(title="Zaleznosc 'depth' od 'table'")
plotle

# Zaleznosc miedzy price i carat

plotle <- ggplot(dataSet, aes(x = carat, y = table), xlab="carat", 
                 ylab="table") + geom_point() + 
  geom_smooth(formula = y ~ x, method = "lm") + 
  theme_ipsum(base_family = 'sans') + 
  labs(title="Zaleznosc 'carat' od 'table'")
plotle

# Zaleznosc miedzy price i carat

plotle <- ggplot(dataSet, aes(x = price, y = table), xlab="price", 
                 ylab="table") + geom_point() + 
  geom_smooth(formula = y ~ x, method = "lm") + 
  theme_ipsum(base_family = 'sans') + 
  labs(title="Zaleznosc 'price' od 'table'")
plotle
