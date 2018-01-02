#A1 data prep code taken from Portal

## Data prep ##
nfl.raw <- read.csv("NFLdraft.csv", head= T, strip.white= T , stringsAsFactors = F)
# head(nfl.raw)
# tail(nfl.raw)
# str(nfl.raw)

nfl.raw$Pos[nfl.raw$Pos == "LS"] <- "C"

nfl <- within(nfl.raw, {
  Pos <- factor(Pos)
  PosGroup <- factor(ifelse(Pos %in% c("C", "DE", "DT", "OG", "OT", "TE"), "Linemen", 
                            ifelse(Pos %in% c("CB", "WR", "FS"), "Small Backs", "Big Backs")))
  
  ProTeam <- factor(matrix(unlist(strsplit(Drafted, " / ")), ncol= 4, byrow = T)[,1])
  Round   <- factor(matrix(unlist(strsplit(Drafted, " / ")), ncol= 4, byrow = T)[,2])
  Overall <- factor(matrix(unlist(strsplit(Drafted, " / ")), ncol= 4, byrow = T)[,3])
  Year    <- factor(matrix(unlist(strsplit(Drafted, " / ")), ncol= 4, byrow = T)[,4])
  
  Overall <- as.numeric(gsub("[^0-9]", "", Overall))
  
  HtFt <- as.numeric(sub("-.*", "", Ht))
  HtIn <- as.numeric(sub("*.-", "", Ht))
  Ht   <- 12*HtFt + HtIn
})
#nfl <- subset(nfl, select= -c(Link, Drafted, Year, HtFt, HtIn)) # Remove unused columns
head(nfl) # Check
str(nfl)
View(nfl)

#storing the original data frame in another variable for later use
origdf <- nfl

### Part A ###
#1. Removing all NAs
nfl <- nfl[!is.na(nfl$Vertical),]
nfl <- nfl[!is.na(nfl$Bench),]
nfl <- nfl[!is.na(nfl$Broad),]
nfl <- nfl[!is.na(nfl$Cone3),]
nfl <- nfl[!is.na(nfl$Shuttle),]
View(nfl)

par(family="Courier New")
#1. Fitting MLR model
fitFull <- lm(nfl$Cone3 ~ nfl$Ht + nfl$Wt + nfl$Yd40 + nfl$Vertical + nfl$Bench + nfl$Broad + nfl$Shuttle)
par(mfrow=c(1,2))
plot(fitFull,1); plot(fitFull, 2)

par(mfrow=c(1,1))

#2. 
summary(fitFull)
#Removing height
fitRed1 <- lm(nfl$Cone3 ~ nfl$Wt + nfl$Yd40 + nfl$Vertical + nfl$Bench + nfl$Broad + nfl$Shuttle)
summary(fitRed1)
#Removing bench press
fitRed2 <- lm(nfl$Cone3 ~ nfl$Wt + nfl$Yd40 + nfl$Vertical + nfl$Broad + nfl$Shuttle)
summary(fitRed2)
#Removing broad jump
fitRed3 <- lm(nfl$Cone3 ~ nfl$Wt + nfl$Yd40 + nfl$Vertical + nfl$Shuttle)
summary(fitRed3)
#Removing vertical 
fitRed4 <- lm(nfl$Cone3 ~ nfl$Wt + nfl$Yd40+ nfl$Shuttle)
summary(fitRed4)
#Removing weight
fitRed5 <- lm(nfl$Cone3 ~ nfl$Yd40+ nfl$Shuttle)
summary(fitRed5)
#fitRed5 is our reduced model

#3. Partial F-test
anova(fitRed5, fitFull)

#4.
anova(fitRed5)
anova(fitFull)

#5.
fitReduced <- lm(origdf$Cone3 ~ origdf$Yd40 + origdf$Shuttle)
summary(fitReduced)
#Adding broad jump
fitReduced1 <- lm(origdf$Cone3 ~ origdf$Broad + origdf$Yd40 + origdf$Shuttle)
summary(fitReduced1)
#Adding Bench Press
fitReduced2 <- lm(origdf$Cone3 ~ origdf$Bench + origdf$Yd40 + origdf$Shuttle)
summary(fitReduced2)
#Adding Vertical
fitReduced3 <- lm(origdf$Cone3 ~ origdf$Vertical + origdf$Yd40 + origdf$Shuttle)
summary(fitReduced3)
#Adding Wt
fitReduced4 <- lm(origdf$Cone3 ~ origdf$Wt + origdf$Yd40 + origdf$Shuttle)
summary(fitReduced4)
#all the predictors are significant
#Adding Ht
fitReduced5 <- lm(origdf$Cone3 ~ origdf$Ht + origdf$Yd40 + origdf$Shuttle)
summary(fitReduced5)


### PART B ###
#Reading in the Denver file
denver <- read.csv("Denver.csv", head= T, strip.white= T , stringsAsFactors = F)
head(denver)
tail(denver)
str(denver)
View(denver)

#1. Pairwise scatterplot matrix for all variables
pairs(denver)
a <- which(denver$pop_chg_per == max(denver$pop_chg_per))
b <- which(denver$crime_rate_per1000 == max(denver$crime_rate_per1000))
denver <- denver[-a, ]
denver <- denver[-b, ]
View(denver)

#2. Correlation matrix rounded to 3 dp
round(cor(denver), 3)

#3. Fitting an MLR model with crime rate change as response and the other six as predictor variables
fit.full <- lm(crime_chg_per ~ pop_1000 + pop_chg_per + child_per + lunch_part_per + house_inc_chg_per + crime_rate_per1000, data=denver)

install.packages("car")
library(car)

vifs <- vif(fit.full)
vifs
vifs[7] = NA
tab <- coefficients(summary(fit.full))
cbind(tab, vifs)

#4. Diagnostic plots
par(mfrow=c(1,2))
plot(fit.full,1); plot(fit.full, 2)
par(mfrow=c(1,1))

#5. Reducing the model
summary(fit.full)
#Removing pop_1000, pop_chg_per and house_inc_chg_per
fit.red <- lm(crime_chg_per ~ child_per + lunch_part_per + crime_rate_per1000, data=denver)
anova(fit.red, fit.full)
summary(fit.red)

#7. Fitting an interaction model and comparing it with the reduced model from 5
fit.int <- lm(crime_chg_per ~ child_per*lunch_part_per*crime_rate_per1000, data=denver)
anova(fit.red, fit.int)

#8. Predicting the change in crime rate for a community with population = 7, children = 25%, lunch program = 55%
crime <- mean(denver$crime_rate_per1000)
newData <- data.frame(child_per = 25, lunch_part_per = 55, crime_rate_per1000 = crime)
predict(fit.int, newData, interval="prediction") # PI


