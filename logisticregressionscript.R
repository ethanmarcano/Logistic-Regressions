#libraries
library(tidyverse)
library(caTools)
library(ROCR)




#Exercise 1
baseball <- read.csv("Baseball.csv")
dfbaseball <- as_tibble(baseball)

head(baseball)
tail(baseball)
summary(baseball)

filterball <- baseball %>%
  select(Team, Year, RankSeason, NumCompetitors, WonWorldSeries) %>%
  filter(WonWorldSeries == 1)
head(filterball)

simplemodel <- lm(WonWorldSeries ~ NumCompetitors, data = dfbaseball)

logisticyear <- glm(WonWorldSeries ~ Year, family = binomial, data = dfbaseball)
logisticRS <- glm(WonWorldSeries ~ RS, family = binomial, data = dfbaseball)
logisticRA <- glm(WonWorldSeries ~ RA, family = binomial, data = dfbaseball)
logisticW <- glm(WonWorldSeries ~ W, family = binomial, data = dfbaseball)
logisticOBP <- glm(WonWorldSeries ~ OBP, family = binomial, data = dfbaseball)
logisticSLG <- glm(WonWorldSeries ~ SLG, family = binomial, data = dfbaseball)
logisticBA <- glm(WonWorldSeries ~ BA, family = binomial, data = dfbaseball)
logisticrank <- glm(WonWorldSeries ~ RankSeason, family = binomial, data = dfbaseball)
logisticcomp <- glm(WonWorldSeries ~ NumCompetitors, family = binomial, data = dfbaseball)
logisticleague <- glm(WonWorldSeries ~ League, family = binomial, data = dfbaseball)

summary(logisticyear)
summary(logisticRS)
summary(logisticRA)
summary(logisticW)
summary(logisticOBP)
summary(logisticSLG)
summary(logisticBA)
summary(logisticrank)
summary(logisticcomp)
summary(logisticleague)

multimodel <- glm(WonWorldSeries ~ NumCompetitors + Year + RA + RankSeason, family='binomial', data = dfbaseball)
summary(multimodel)

bestmodel <- glm(WonWorldSeries ~ NumCompetitors + Year, family = 'binomial', data = dfbaseball)
summary(bestmodel)

#Exercise 2

parole <- read.csv("Parole.csv")
dfparole <- as_tibble(parole)

summary(dfparole)

count(dfparole)

violators <- dfparole %>%
  select(everything()) %>%
  filter(Violator == 1)
count(violators)

dfparole$State = as.factor(dfparole$State)
dfparole$Crime = as.factor(dfparole$Crime)
summary(dfparole$State)
summary(dfparole$Crime)

set.seed(88)
spl = sample.split(dfparole$Violator, SplitRatio = 0.7)
paroletrain = subset(dfparole, spl == TRUE)
paroletest = subset(dfparole, spl == FALSE)
nrow(paroletrain)/nrow(dfparole)
nrow(paroletest)/nrow(dfparole)

trainmodel <- glm(Violator ~., family = binomial, data = paroletrain)

summary(trainmodel)

Male = 1
RaceWhite = 1
Age = 50
StateOther = 1
StateLouisiana = 0
StateVirginia = 0
time.served = 3
max.sentence = 12
multiple.offenses = 0
CrimeDrugs = 0
CrimeLarceny = 1
CrimeOther = 0

logodds = -3.29370 + 0.65662*Male + -0.67930*RaceWhite + 0.01739*Age + 0.67688*StateOther + -0.17308*StateLouisiana + -3.38536*StateVirginia + -0.06809*time.served + 0.04536*max.sentence + 1.42426*multiple.offenses + -0.23931*CrimeDrugs + 0.99710*CrimeLarceny + 0.19106*CrimeOther
logodds

odds = exp(logodds)
odds

predictions = predict(trainmodel, newdata = paroletest, type = "response")
summary(predictions)
testmatrix <- table(paroletest$Violator, as.numeric(predictions >= 0.5))
testmatrix
sum(diag(testmatrix))/sum(testmatrix)
1/(1 + exp(-logodds))


simpletest <- table(paroletest$Violator)
simpletest
simpletest[1]/simpletest[2]

highthreshold <- table(paroletest$Violator, as.numeric(predictions >= 0.7))
lowthreshold <- table(paroletest$Violator, as.numeric(predictions >= 0.3))

highthreshold
lowthreshold

predrocr <- prediction(predictions, paroletest$Violator)
as.numeric(performance(predrocr, "auc")@y.values)

#Exercise 3

loans <- read.csv("Loans.csv")
dfloans <- as_tibble(loans)
summary(dfloans)

filterloans <- dfloans %>%
  select(everything())%>%
  filter(NotFullyPaid == 0)
count(filterloans)


set.seed(88)
spl = sample.split(dfloans$NotFullyPaid, SplitRatio = 0.7)

loanstrain = subset(dfloans, spl == TRUE)
loanstest = subset(dfloans, spl == FALSE)

nrow(loanstrain)/nrow(dfloans)
nrow(loanstest)/nrow(dfloans)

simpleloan <- table(loanstest$NotFullyPaid)
simpleloan
simpleloan[1]/sum(simpleloan)

loanmodel = glm(NotFullyPaid ~ ., data=loanstrain, family = "binomial")
summary(loanmodel)

#fico coefficient is -9.211e-03 and the score difference is 10

logoddiff = -9.211e-03 * -10
logoddiff

loanstest$PredictedRisk <- predict(loanmodel, newdata=loanstest, type="response")
loanmatrix = table(loanstest$NotFullyPaid, loanstest$PredictedRisk > 0.5)
loanmatrix
sum(diag(loanmatrix))/sum(loanmatrix)

pred = prediction(loanstest$PredictedRisk, loanstest$NotFullyPaid)
as.numeric(performance(pred, "auc")@y.values)

interestmodel <- glm(NotFullyPaid ~ IntRate, data = loanstrain, family = binomial)
summary(interestmodel)

dfloans$Purpose = as.numeric(dfloans$Purpose)
summary(dfloans$Purpose)
cor(dfloans)

interestpred = predict(interestmodel, newdata = loanstest, type = "response")
summary(interestpred)

interestpred = prediction(interestpred, loanstest$NotFullyPaid)
as.numeric(performance(interestpred, "auc")@y.values)

c = 10
r = 0.06
t = 3
c*exp(r*t)

#remember 3 year term
loanstest$profit = exp(loanstest$IntRate*3) - 1
loanstest$profit[loanstest$NotFullyPaid == 1] = -1
summary(loanstest$profit)

highinterest <- subset(loanstest, IntRate>= 0.15)
mean(highinterest$profit)
riskproportion = table(highinterest$NotFullyPaid)
riskproportion[2]/sum(riskproportion)


riskpoint = sort(highinterest$PredictedRisk, decreasing = FALSE)[100]
SelectedLoans = subset(highinterest, PredictedRisk <= riskpoint)
sum(SelectedLoans$profit)
selectloans = table(SelectedLoans$NotFullyPaid)
selectloans

31.24/20.94 * 100



