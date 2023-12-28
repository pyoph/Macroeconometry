#install.packages("usethis")
#install.packages("brms")
#install.packages("MCMCpack")
#install.packages("fastmatrix")
#install.packages("patchwork")
library(ggplot2)
library(gridExtra)
library(zoo)
library(brms)
library(MASS)
library(MCMCpack)
library(vars)
library(fastmatrix)
library(bvarsv)
library("patchwork")
#usethis::use_git_config(user.name="pyoph", user.email="paul.guillot2@gmail.com")

#Simulation of normal prior
Priornormal <- function(n_samples, mu, Sigma) {

  return(mvrnorm(n = n_samples, mu = mu, Sigma = Sigma))
}

#Simulation of inverse Wishart prior
invWishart <- function(A,df){
  return (riwish(df,A)) 
}


####Opening of data files####


fichierTL <- read.csv("TLAACBW027SBOG.csv")

fichierFFR <- read.csv("EFFR.csv")

fileReserves <- read.csv("WRESBAL.csv")

#Removing of NA's
fichierFFR <- na.omit(fichierFFR)
fichierFFR <- fichierFFR[fichierFFR$Spread != "NA ",]

fichierFFR$Spread <- as.double(fichierFFR$Spread)


fichierTL$DATE <- as.Date(fichierTL$DATE, format = "%Y-%m-%d")
fichierTL$DATE
names(fichierFFR)[names(fichierFFR) == "DATE"] <- "Date"

fichierFFR$Date <- as.Date(fichierFFR$Date)

dates_journalieres <- seq(as.Date("2009-01-07"), as.Date("2021-12-29"), by = "day")
dates_journalieres
fichierTL$TLAACBW027SBOG



#Interpolation of assets data in order to have daily data

TLAACts <- approx(fichierTL$DATE, fichierTL$TLAACBW027SBOG, xout = dates_journalieres)$y
fichierTL$TLAACBW027SBOG
TLAACts
TLAAj <- data.frame(Date = dates_journalieres, assets = TLAACts)
TLAAj

#Interpolation of reserves in order to have daily data
fileReserves$Date <- as.Date(fileReserves$DATE)

reserves <- approx(fileReserves$Date, fileReserves$WRESBAL, xout = dates_journalieres)$y
reservesj <- data.frame(Date = dates_journalieres, reserves = reserves)
reservesj





fichierFFR$Spread

#Fusion of datasets
mergedData <- merge(TLAAj,fichierFFR,by = "Date")
mergedData <- merge(reservesj,mergedData,by = "Date")

mergedData$assetsReserves <- 0

mergedData$assetsReserves <- (mergedData$reserves / mergedData$assets)*100

#Periods
mergedData$period <- cut(
  mergedData$Date,
  breaks = c(as.Date("2009-01-01"), as.Date("2015-01-01"), as.Date("2020-01-03"), as.Date("2021-12-29")),
  labels = c("Before 2015", "Between 2015 and 2020", "After march 2020")
)

###Representation of datas###

#Representation of reserves

plot1 <- ggplot(mergedData, aes(x = as.Date(Date, format = "%Y-%m-%d"))) + 
    geom_line(aes(y = as.numeric(reserves), color = "reserves",group = 1))  +
   ylab("Reserves")+
  xlab("Year") + ggtitle("Reserves")+
 scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
plot1
#plot1 <- ggplot(fichierTL, aes(x = as.Date(DATE, format = "%Y-%m-%d"))) + 
#  geom_line(aes(y = as.numeric(TLAACBW027SBOG), color = "reserves",group = 1))  +
 # ylab("Reserves billions $")+
#  xlab("Year") + ggtitle("Reserves billions")+
 # scale_x_date(date_breaks = "1 year", date_labels = "%Y")


#Representation of the federal fund rates - IORB
plot2 <- ggplot(fichierFFR, aes(x = as.Date(Date, format = "%Y-%m-%d"))) + 
  geom_line(aes(y = Spread, color = "spread federal fund rates - IORB",group = 1))  +
  ylab("Federal fund rates - IORB")+
  xlab("Year") + ggtitle("Federal fund rates - IORB")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  #scale_y_continuous(breaks = seq(-3, 3, by = 0.5)) +  
plot2
plot3 <- ggplot(mergedData, aes(x = assetsReserves, y = Spread , color = period)) +
  geom_point(size = 3) +
  labs(title = "Relationship between spread and normalized reserves",
       x = "Reserves / assets",
       y = "Federal Fund Rates - IORB") +
  scale_color_manual(values = c("Before 2015" = "gray",  "Between 2015 and 2020" = "blue", "After march 2020" = "red")) +
  theme_minimal()
plot3

#Put the graphs horizontally
plot <- (plot1 / plot2 / plot3) + plot_layout(guides = 'collect')


plot






###Variables###

#Data vector

y <- cbind(mergedData$reserves,mergedData$Spread)
colnames(y) <- c("reserves","spread")

##presample
mergedDataps <- mergedData[mergedData$Date <= "2010-01-19",]

typeof(mergedDataps$Spread)
as.double(mergedDataps$Spread)
mergedDataps$Spread
#Parameters
m = 10 #number of lags


l1 = 0.04
l2 = 0.2
l3 = 0.01



##Setting of B^, alpha^, log(sigma^), PsiB^ and Psialpha^ by OLS###

#Regression on datas from 01/07/2009 to 19/01/2010
mergedDataps <- na.omit(mergedDataps)


modelOLSps <- lm(as.double(Spread) ~ as.double(reserves), data = mergedDataps)
summary(modelOLSps)


#Extraction of alpha^

alphahat <- coef(modelOLSps)[1]
alphahat

#Extraction of B^


Bhat <- coef(modelOLSps)[2]
Bhat

Omega0 <- vcov(modelOLSps)
Omega0


#Extraction of PsiA and PsiB
PsiA <- Omega0[1,1]
PsiB <- Omega0[2,2]

#LDL décompostion of Sigma0*t(Sigma0)

Sigma0 <- ldl(Omega0)[2]

#Extraction of sigma10 and sigma20
str <- as.character(A0[1])
str
sigma10 <- as.double(substr(str[1],3,8))
sigma10 <- sigma10 * 10^(-5)
sigma20 <- as.double(substr(str[1],24,32))
sigma20 <- sigma20*10^(-12)

sigma10 <- sqrt(sigma10)
sigma20 <- sqrt(sigma20)

sigma10
sigma20

#Extration of a10
A0 <- ldl(Omega0)[1] 

A0

str <- as.character(A0[1])
str
a10 <- as.double(substr(str,5,12))
a10
M <- matrix(data=c(1,a10,0,1),nrow = 2)

invM <- solve

invM

a10 <- invM[2,1]
a10

#Settings of priors

set.seed(13579)                 

B0 <- rnorm(1,Bhat,PsiB)
B0

alpha0 <- rnorm(1,alphahat,4*PsiA)

logsigma <- matrix(data = c(log(sigma10),log(sigma20)),nrow = 2)
logsigma

logs0 <- t(Priornormal(1,logsigma,diag(2)))

logs0