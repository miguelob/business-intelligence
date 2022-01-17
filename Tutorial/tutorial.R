# Miguel Oleo Blanco

#1
a <- c(2002,2004,2006,2008)
a_2 <- seq(2002,2008,by=2)

#2
print(length(a))
print(length(a_2))

#3
print(a[1])
print(a[-1])
print(a[2:4])

#4
fdata <- read.csv('usedcars.csv')
print(fdata[1,])ç
print(fdata$year)
print(fdata[1])
print(fdata[[1]])

#5
str(fdata)
summary(fdata)

#6
View(fdata)
head(fdata)

#7
print(fdata$color[5:20])

#8
fdata_1 <- fdata[-10,][-99,]

#9
fdata_2  <- data.frame(fdata$year,fdata$price,fdata$mileage)

#10
summary(fdata_2)
lm(fdata.price ~ fdata.year, data=fdata)

#11
by(fdata$price,fdata$transmission,mean)
by(fdata$price,fdata$transmission,sd)

#12
out <- fdata[fdata$year == 2008, ]
#NO ESTA TERMINADO
