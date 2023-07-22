install.packages('fpp2')
library(fpp2)
#задаємо часовий ряд 
a=ts(data = DAYTON_hourly$DAYTON_MW[1:4321], start=1,end=180, frequency = 24)
autoplot(a)
#Метод середніх
meanf(a,h=432)
autoplot(meanf(a,h=432))
#Наївний метод
naive(a,h=432)
autoplot(naive(a,h=432, PI=T))
#Сезонний наївний 
snaive(a,h=432)
autoplot(snaive(a,h=432))
#Дріфтовий метод
rwf(a,h=432,drift=TRUE)
autoplot(rwf(a,h=432,drift=TRUE))
#Метод двух крайніх точок
t <- ts(c(1:4726))
a1 <- (a[4297]-a[1])/(t[4297]-t[1])
a0 <- (a[1]-a1*t[1])
y <- (a0+a1*t)
y=ts(y,start=1,frequency = 24)
autoplot(a)+autolayer(y)
#Метод середніх групових точок
b1=(mean(a[2149:4297])-mean(a[1:2148]))/(mean(2149:4297)-mean(1:2148))
b0=mean(a[1:2148])-b1*mean(1:2148)
z2=b0+b1*t
z2 <- ts(z2,start = 1,end=197, frequency = 24)
autoplot(a)+autolayer(z2)
#Перетворення часового ряду методом Бокса-Кокса
lambda=-1.0
lambda=-0.7
lambda=-0.4
lambda=-0.1
lambda=0.2
lambda=0.5
lambda=0.8
lambda=1.1
lambda=1.4
lambda=1.7
lambda=2.0
(lambda <- BoxCox.lambda(a))
autoplot(BoxCox(a,lambda))
#Коригування заміщення 
fc <- rwf(a,drift = TRUE,lambda = lambda)
fc2 <- rwf(a,lambda = lambda,biasadj = TRUE)
autoplot(a)+
  autolayer(fc, series="Simple back transformation", PI=FALSE)+
  autolayer(fc2, series = "Biast adjusted",PI=FALSE)+ggtitle("Коригування")
#Похибки
res=residuals(naive(a))
gghistogram(res)
autoplot(res)
ggAcf(res)+ggtitle("Автокореляція")
#Тест Бокса-Пірса
Box.test(res,lag=10)

#Тест Люнга-Бокса
Box.test(res,lag=10, type="Lj")
checkresiduals(naive(a))








