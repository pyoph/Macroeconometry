#install.packages("usethis")
#install.packages("brms")
#install.packages("MCMCpack")
#install.packages("fastmatrix")
#install.packages("patchwork")
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)
#library(zoo)
library(brms)
library(MASS)
library(MCMCpack)
library(vars)
library(fastmatrix)
library(bvarsv)
library(patchwork)
#install.packages(c('R6', 'jsonlite'))
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

fichierIOER <- read.csv("IOER.csv")

fileReserves <- read.csv("WRESBAL.csv")


#Removing of NA's
fichierFFR <- na.omit(fichierFFR)

fichierTL$DATE <- as.Date(fichierTL$DATE, format = "%Y-%m-%d")
#fichierTL$DATE
names(fichierFFR)[names(fichierFFR) == "DATE"] <- "Date"

fichierFFR$Date <- as.Date(fichierFFR$Date)

names(fichierIOER)[names(fichierIOER) == "DATE"] <- "Date"
fichierIOER$Date <- as.Date(fichierIOER$Date)
names(fichierIOER)[names(fichierIOER) == "IOER"] <- "IOER_FRED"

dates_journalieres <- seq(as.Date("2009-05-01"), as.Date("2021-12-29"), by = "day")
#dates_journalieres
#fichierTL$TLAACBW027SBOG



#Interpolation of assets data in order to have daily data

TLAACts <- approx(fichierTL$DATE, fichierTL$TLAACBW027SBOG, xout = dates_journalieres)$y
#fichierTL$TLAACBW027SBOG

TLAAj <- data.frame(Date = dates_journalieres, assets = TLAACts)


#Interpolation of reserves in order to have daily data
fileReserves$Date <- as.Date(fileReserves$DATE)

reserves <- approx(fileReserves$Date, fileReserves$WRESBAL, xout = dates_journalieres)$y
reservesj <- data.frame(Date = dates_journalieres, reserves = reserves)
#reservesj

EFFR<- approx(fichierFFR$Date, fichierFFR$EFFR, xout = dates_journalieres)$y
fichierFFRj <- data.frame(Date = dates_journalieres, EFFR= EFFR)

#Fusion of datasets
mergedData <- left_join(TLAAj, fichierFFRj ,by = "Date")
mergedData <- left_join(mergedData,reservesj,by = "Date")
mergedData <- mergedData[6:nrow(mergedData),]


mergedData$assetsReserves <- 0

mergedData$assetsReserves <- (mergedData$reserves / mergedData$assets)*100

#Periods
mergedData$period <- cut(
  mergedData$Date,
  breaks = c(as.Date("2009-01-01"), as.Date("2015-01-01"), as.Date("2020-03-01"), as.Date("2021-12-29")),
  labels = c("Before 2015", "Between 2015 and 2020", "After march 2020")
)
  
#using FRED IOER measure
filteredData <- left_join(mergedData, fichierIOER,by = "Date")

#exclude one day windows around month ends

#install.packages("lubridate")
library(lubridate)

# Exclude one-day windows around month ends
filteredData <- filteredData %>%
  filter(!(day(Date) %in% c(1,2) | day(Date) >= (days_in_month(Date) - 1)))

filteredData <- filteredData %>%
  mutate(IOER_FRED = ifelse(is.na(IOER_FRED), 0.25, IOER_FRED))

filteredData <- filteredData %>% 
  mutate(Spread_FRED = as.numeric(EFFR)-as.numeric(IOER_FRED))


plot_data <- filteredData %>% 
  filter(Date >= as.Date("2010-01-01"))

# Explanation:
# - day(Date) extracts the day component of the date.
# - days_in_month(Date) returns the number of days in the month of the given date.
# - The filter condition excludes days 1 and 2 of each month, as well as the last day of each month.

# Now, filteredData contains the rows with dates excluding one-day windows around month ends.


###Representation of datas###
library(ggplot2)
#Representation of reserves/assets

plot1 <- ggplot(plot_data, aes(x = as.Date(Date, format = "%Y-%m-%d"),y = assetsReserves, color=period)) + 
    geom_line()+
   ylab("Reserves/assets")+
  xlab("Year") + ggtitle("Reserves")+
  scale_color_manual(values = c("Before 2015" = "blue",  "Between 2015 and 2020" = "gray", "After march 2020" = "red")) +
 scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
print(plot1)

#representation of the spread using the IOER from FRED
#nota bene on a shorter period of time

plot2 <- ggplot(plot_data, aes(x = as.Date(Date, format = "%Y-%m-%d"),y = Spread_FRED, color=period)) + 
  geom_line()+
  ylab("Federal Fund - IORB spread")+
  xlab("Year") + ggtitle("Spread")+
  scale_color_manual(values = c("Before 2015" = "blue",  "Between 2015 and 2020" = "gray", "After march 2020" = "red")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

print(plot2)


plot3 <- ggplot(plot_data, aes(x = assetsReserves, y = Spread_FRED , color = period)) +
  geom_point(size = 1) +
  labs(title = "Relationship between spread and normalized reserves",
       x = "Reserves / assets",
       y = "Federal Fund - IORB spread") +
  scale_color_manual(values = c("Before 2015" = "blue",  "Between 2015 and 2020" = "gray", "After march 2020" = "red")) +
  theme_minimal()
print(plot3)

library(patchwork)

# Arrange plots vertically using patchwork
combined_plot <- plot1 / plot2 / plot3
# Display the combined plot
print(combined_plot)

#print(plot)

library(bvarsv)

#Application à notre exemple


set.seed(1)
# Create an xts time series object
Y <-filteredData[,c( "assetsReserves", "Spread_FRED")]
Y <- ts(Y)
m <- 10
nburn.vignette <-500
nrep.vignette <- 5000

training_data <- filteredData %>% 
  filter(Date <= as.Date("2010-01-19"))
tau <- nrow(training_data)
tau
nf <- 5
Y <- na.omit(Y)
Y

#We differentiate the time serie in order to have stationnary time series and to avoid numerical errors
Yd <- diff(Y)
Yd
fit_bvar <- bvar.sv.tvp(Yd, p=m, nburn =nburn.vignette, nrep= nrep.vignette, tau = tau, nf = nf,pQ = NULL, pW = NULL, pS = NULL) 
#probleme je n'arrive pas à estimer un modèle avec un lag supérieur à 2
#chol(): given matrix is not symmetric
print(fit_bvar)


typeof(fit_bvar$Beta.draws)

Betaestimate <- fit_bvar$Beta.draws
options(max.print=2000)
head(Betaestimate)

save(list = ls(), file = "env_entier.RData") # sauvegarde de tout l'environnement sur le répertoire choisi
getwd()
#compare with classic var
library(vars)
fit.ols <- VAR(Y, p = 2)



#impulse response functions
par(mfrow = c(1, 1))
ira <- impulse.responses(fit_bvar, impulse.variable = 2, response.variable = 1)
# OLS impulse responses for comparison
ira.ols <- irf(fit.ols, n.ahead = 20)[[2]][[1]][-1, 1]
# Add to plot
lines(x = 1:20, y = -ira.ols, lwd = 3, lty = 5)




# Construct predictive density function for the second variable (inflation), one period ahead
f <- predictive.density(fit_bvar, v = 1, h = 1)
f
# Plot the density for a grid of values
grid <- seq(6, 20, by = 0.05)
# Cross-check: Extract MCMC sample for the same variable and horizon 1
smp <- predictive.draws(fit_bvar, v = 1, h = 1)

# Add density estimate to plot
lines(density(smp$y), col = "green")
plot(x = grid, y = f(grid), type = "l")





#Using the forecast error for reserves h days ago (uq,t−h) 
#as an instrument for reserves today (qt) in equation (5)

#draw the prediction for h=1 day of q_t
q_t_h <- predictive.draws(fit_bvar, v = 1, h = 5)
u_q_h <- 

#Our estimation of the forecasting model (6) gives us simultaneously the forecast errors u and
#their time-varying covariances with the observable variables q and p. These covariances are
#functions of the time-varying model parameters and can be obtained by drawing from their
#posterior distribution.



###Variables###

#Data vector

y <- cbind(filteredData$reserves,filteredData$Spread_FRED)
colnames(y) <- c("reserves","spread")

##presample
filteredDataps <- filteredData[filteredData$Date <= "2010-01-19",]

typeof(filteredDataps$Spread)
as.double(filteredDataps$Spread)
filteredDataps$Spread
#Parameters
m = 10 #number of lags


l1 = 0.04
l2 = 0.2
l3 = 0.01



##Setting of B^, alpha^, log(sigma^), PsiB^ and Psialpha^ by OLS###

#Regression on datas from 01/07/2009 to 19/01/2010
filteredDataps <- na.omit(filteredDataps)


modelOLSps <- lm(as.double(Spread) ~ as.double(reserves), data = filteredDataps)
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

invM <- solve(M)

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
