########
Egyszer� norm�lis eloszl�s s�r�s�gf�ggv�ny grafika be�getett adatokkal
########
x <- seq(-10, 10, by = .1)
y <- dnorm(x, mean = 2.5, sd = 0.5)
plot(x,y)







-----------------------------------
########
Eloszl�sf�ggv�ny
########
x <- seq(-10,10,by = .1)
y <- pnorm(x, mean = 2.5, sd = 0.5)
plot(x,y)







-----------------------------------
#######
Random gener�l�sa norm�lis eloszl�ssal
#######
y<-rnorm(50)
hist(y,main="Random sz�mok norm�l eloszl�ssal")









----------------------------------
###############
Mi kell ahhoz hogy norm�l eloszl�st tudjunk �br�zolni?
kell az �TLAG �s a SZ�R�S (�tlagt�l val� elt�r�s)

Az �TLAG-ot egyszer�en kapjuk meg: szum/darabsz�m
a SZ�R�S-t viszont t�bb l�p�sben kell kisz�molnunk
	1. az adatsor minden elem�b�l ki kell vonnunk az �tlagot, �s az
	   eredm�nyt n�gyzetre emelni
	2. Ezek �tlag�t kell kisz�molnunk, majd
	3. ezt gy�k al� kell vonnunk, �s megkapjuk az eredm�nyt
###############

calculateSd <- function(datas, mean) {
	sd<-0
	for(i in 1:length(datas)){
		sd<-sd+(datas[i]-mean)^2
	}
	sd<-sqrt(sd/length(datas))
}

calculateMean <- function(datas){
	mean<-0
	for(i in 1:length(datas)){
		mean <- mean+datas[i]
	}
	mean<-mean/length(datas)
	return(mean)
}

datas <- c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
atlag<-calculateMean(datas)
sdeviation<-calculateSd(datas,atlag)
y<-dnorm(datas,mean=atlag,sd=sdeviation)
#megl�v� adatsorral
plot(datas,y)
par(new=TRUE)
hist(y)









--------------------------
########
A hisztogramra nem fog illeszkedni a g�rbe, hiszen maga az adatsor az nem 
igazi adatsor, hiszen csak egy dimenzi�t tartalmaz. (nincsen a helyekhez �rt�k
hozz�rendelve)
########

data("PlantGrowth")
#print(PlantGrowth)
hist(PlantGrowth$weight, probability = TRUE)

plantsMean<-calculateMean(PlantGrowth$weight)
plantsSd<-calculateSd(PlantGrowth$weight,plantsMean)

interval<-3.5:6.5
plantsBellCurve <-  dnorm(interval, mean = plantsMean, sd = plantsSd)
lines(interval, plantsBellCurve, col = "blue")









--------------------------

data("USArrests")
hist(USArrests$Assault, probability = TRUE)

assaultMean<-calculateMean(USArrests$Assault)
assaultSd<-calculateSd(USArrests$Assault,assaultMean)

interval<-0:350
assaultBellCurve <-  dnorm(interval, mean = assaultMean, sd = assaultSd)
lines(interval, assaultBellCurve, col = "blue")

probability=0.3
probabilityPosition<-qnorm(probability,mean=assaultMean,sd=assaultSd)
probabilityPosition
probabilityInterval<-seq(0,probabilityPosition)
probabilityCurve<-dnorm(probabilityInterval, mean=assaultMean,sd=assaultSd)
#lines(probabilityInterval, probabilityCurve, col = "red")
polygon(c(0,probabilityInterval,probabilityPosition),c(0,probabilityCurve,0),col="gray")
text(probabilityPosition/2,y=0.0005,labels=probabilityPosition)

--
##K�veti-e a norm�l eloszl�st?
##Ha igen, akkor haszn�lhat�ak a qnormos egyenletek r�.
normal_comparsion <- rnorm(n = length(USArrests$Assault), mean = assaultMean, sd = assaultSd)
qqnorm(normal_comparsion, ylab = "Assaults", main = "Normal Q-Q plot")
qqline(normal_comparsion, col = "blue")





-----------------------------------

students <- read.csv("C:/GITHUB/matstat-beadando/R/students.csv")
males <- subset(students, gender=='Male')
hist(males$height, 
     breaks = 'Sturges',
     xlab = 'Height in cm',
     main = '',
     col = 3)

qqnorm(males$height, main = 'Q-Q plot for the height of male students')
qqline(males$height, col = 3, lwd = 2)