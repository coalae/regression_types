# AUFGABENBLATT 1 
# Cordula Eggerth (00750881)

# Verwendete Literaturquellen:
# - Folien und R-Codes zu den bisher vorgetragenen Kapiteln aus UK Erweiterungen des linearen Modells 
#   (Prof. Marcus Hudec).
# - Kernel Regression Examples Using np (Jeffrey Racine, McMaster University Ontario (Canada), 
#   https://socialsciences.mcmaster.ca/racinej/Gallery/Regression.html).
# - R Regression Diagnostics (Vik Paruchuri, DataQuest), 
#   http://www.vikparuchuri.com/blog/r-regression-diagnostics-part-1/).

rm(list=ls())

install.packages("car")
install.packages("np")
install.packages("UsingR")
install.packages("XLConnect")
install.packages("xlsx")
install.packages("MASS")

library(car)
library(np)
library(UsingR)
library(XLConnectJars)
library(XLConnect)
require(XLConnectJars)
require(XLConnect)
library(xlsxjars)
library(xlsx)
require(xlsxjars)
require(xlsx)
library(MASS)

path <- "C:/Users/Coala/Desktop/A1_ERWEIT"

#-----------------------------------------------------------------------------------------------
### AUFGABE 1 ### ------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# 1. Führen Sie mit dem Datensatz mtcars verschiedene sinnvolle regressionsanalytische 
#    Auswertungen und Visualisierungen durch.

data("mtcars")
mtcars

summary(mtcars) # deskriptive zusammenfassung der daten
n <- nrow(mtcars)
x.werte <- 1:n
scatterplotMatrix(mtcars) # diagonalelemente zeigen histogramm der jeweiligen 
                          # variable
sapply(mtcars,mean) # mittelwerte der variablen
sapply(mtcars,sd) # standardabweichung der variablen

# 1.a. LINEAR REGRESSION (univariat) 

# fall: erklaere mpg (miles per US gallon) durch wt (weight in 1000lbs)
ggplot(mtcars, aes(wt, mpg)) + geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  ylab("Miles per US Gallon") +
  xlab("Weight (in 1000lbs)") +
  ggtitle("Influence of Weight on Miles per Gallon")

# fall: erklaere mpg (miles per US gallon) durch qsec (1/4 mile time)
ggplot(mtcars, aes(qsec, mpg)) + geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  ylab("Miles per (US) Gallon") +
  xlab("Weight (in 1000lbs)") +
  ggtitle("Influence of Weight on Miles per Gallon")

# 1.b. LINEAR REGRESSION (multiple) 
#      (hier: mpg durch restliche variablen erklaeren als additives modell)
multiple_lm <- lm(mtcars$mpg ~ mtcars$cyl + mtcars$disp + mtcars$hp + 
                  mtcars$drat + mtcars$wt + mtcars$qsec + mtcars$vs + 
                  mtcars$am + mtcars$gear + mtcars$carb)
summary(multiple_lm)
influencePlot(multiple_lm, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance")

plot(multiple_lm$fitted.values) # plot fitted values vs. residuals

plot(multiple_lm$residuals) # qqplot


# 2.a. LOCAL REGRESSION  
    # LINEAR LOESS FIT (polynome 1. grades)
alpha <- c(0.1, 0.35, 0.75)
colors <- c("mediumblue","cadetblue","red2")

plot(x.werte, mtcars$mpg, pch=16, col="black", 
     xlab="Data points mpg", ylab="Miles per US Gallon", 
     main = "Different Linear Loess Fits") 

for(i in 1:length(alpha)){     
  localregression.model <- loess(mtcars$mpg ~ x.werte, span=alpha[i], degree=1)
  lines(x.werte, localregression.model$fitted, lwd=2, col=colors[i])
}

legend(3,33,c(expression(alpha == 0.10), expression(alpha == 0.35), 
              expression(alpha == 0.75)), lty=c(1,1,1), col=colors, lwd=c(2,2,2), 
       cex=0.8)

   # QUADRATIC LOESS FIT (polynome 2. grades)
alpha <- c(0.1, 0.35, 0.75)
colors <- c("mediumblue","cadetblue","red2")

plot(x.werte, mtcars$mpg, pch=16, col="black", 
     xlab="Data points mpg", ylab="Miles per US Gallon", 
     main = "Different Linear Loess Fits") 

for(i in 1:length(alpha)){     
  localregression.model <- loess(mtcars$mpg ~ x.werte, span=alpha[i], degree=2)
  lines(x.werte, localregression.model$fitted, lwd=2, col=colors[i])
}

legend(3,33,c(expression(alpha == 0.10), expression(alpha == 0.35), 
       expression(alpha == 0.75)), lty=c(1,1,1), col=colors, lwd=c(2,2,2), 
       cex=0.8)

  # linearitaet ueberpruefen:
ggplot(data=mtcars, aes(x=wt, y=mpg)) + 
  geom_point(size=2, shape=21, fill="blue") +
  stat_smooth(method=loess) 

  # lineare regression vs. loess 
ggplot(data=mtcars, aes(x=wt, y=mpg)) + 
  geom_point(size=2, shape=21, fill="blue") +
  stat_smooth(method=loess, col="red",se=FALSE) +
  stat_smooth(method=lm)

# 2.b. LOESS bzw. LOCAL POLYNOMIAL REGRESSION ("smoothing", univariat)

# fall: erklaere mpg (miles per US gallon) durch wt (weight in 1000lbs)
#       span=0.9 (glattere kurve, weil groessere anzahl an punkten in
#       umgebung in berechnung miteinbezogen)
ggplot(mtcars, aes(wt, mpg)) +
  stat_smooth(span=0.9) + geom_point() +
  ylab("Miles per US Gallon") +
  xlab ("Weight (in 1000lbs)") +
  ggtitle("Influence of Weight on Miles per Gallon")

#       span=0.3 (weniger glatte kurve, weil kleine anzahl von punkten
#       in umgebung in berechnung miteinbezogen)
ggplot(mtcars, aes(wt, mpg)) +
  stat_smooth(span=0.3) + geom_point() +
  ylab("Miles per US Gallon") +
  xlab ("Weight (in 1000lbs)") +
  ggtitle("Influence of Weight on Miles per Gallon")

# fall: erklaere mpg (miles per US gallon) durch qsec (1/4 mile time)
#       span=0.9
ggplot(mtcars, aes(qsec, mpg)) + geom_point() +
  stat_smooth(span=0.9) + geom_point() +
  ylab("Miles per (US) Gallon") +
  xlab("Weight (in 1000lbs)") +
  ggtitle("Influence of Weight on Miles per Gallon")

#       span=0.7
ggplot(mtcars, aes(qsec, mpg)) + geom_point() +
  stat_smooth(span=0.7) + geom_point() +
  ylab("Miles per (US) Gallon") +
  xlab("Weight (in 1000lbs)") +
  ggtitle("Influence of Weight on Miles per Gallon")


# 3.a. KERNSCHAETZUNG (KERNEL REGRESSION - nicht-parametrisch)
plot(x.werte, mtcars$mpg, pch=16, col="gray24", 
     xlab="Datenpunkte ", ylab="mpg", 
     main = "Kernel Regression Estimate") 

lines(ksmooth(x.werte,mtcars$mpg,"normal"),col="chocolate")
lines(ksmooth(x.werte,mtcars$mpg,"normal",bandwidth=3),
      col="orchid3", lwd=2) # bandwidth 3
lines(ksmooth(x.werte,mtcars$mpg,"normal",bandwidth=5),
      col="mediumseagreen", lwd=2) # bandwidth 5
lines(ksmooth(x.werte,mtcars$mpg,"normal",bandwidth=10),
      col="thistle", lwd=2) # bandwidth 10
legend("topleft", 
       c("bandwidth=0.5","bandwidth=3",
         "bandwidth=5","bandwidth=10"),
       lty=1, 
       col=c("chocolate","orchid3","mediumseagreen",
             "thistle"), 
       bty="n")

# 3.b. KERNEL REGRESSION (univariat)
# using library np from: 
# https://socialsciences.mcmaster.ca/racinej/Gallery/Regression.html
# local linear model 
local_linear_model <- npreg(mtcars$mpg~mtcars$wt,regtype="ll")
# local constant model
local_constant_model <- npreg(mtcars$mpg~mtcars$wt)
# plot
plot(mtcars$wt,mtcars$mpg,cex=0.25,col="black",pch=3)
lines(mtcars$wt,fitted(local_linear_model),col="chocolate",lty=2)
lines(mtcars$wt,fitted(local_constant_model),col="cadetblue",lty=1)
legend("topright", c("local linear model","local constant model"),
       lty=c(2,1), col=c("chocolate","cadetblue"), bty="n")


# 4. NAIVE REGRESSION 
plot(x.werte, mtcars$mpg, pch=20, col="gray24", cex=1.2,
     xlab="Data points of mpg", ylab="mpg", main = "Naive Regression")

x.breaks = cut(x.werte, breaks=20, labels=F)
x.naive <- by(x.werte, x.breaks, mean)
x.cut   <- by(x.werte, x.breaks, max)
abline(v=x.cut, col="lightgrey", lwd=0.7)
y.naive <- by(mtcars$mpg, x.breaks, mean)
lines(x.naive, y.naive , pch=19, cex=1.0, type="o", lwd=1, col=2)


# 5. PIECEWISE CONSTANT REGRESSION
const.reg <- function(x, y, interv=10, type=mean, ...){
  plot(x, y, pch=20, col="gray24", cex=1.2, ...)
  x.breaks = cut(x, breaks=interv, labels=F)
  x.naive <- by(x, x.breaks, type)
  x.cut   <- c(0, by(x, x.breaks, max))
  y.naive <- by(y, x.breaks, type)
  for (i in 1:(length(x.breaks)-1))
    lines(c(x.cut[i], x.cut[i+1]), c(y.naive[i],y.naive[i]), col=2)
  abline(v=x.cut, col="grey", lwd=0.8)
}

const.reg(x.werte, mtcars$mpg, xlab="Data Points of mpg", ylab="mpg", 
          main = "Piecewise Constant Regression")

# 6. BROKEN STICK REGRESSION mit lokalen funktionen 
#    (piecewise linear)
piecewise <- function(x, y, wo=2, vertical=T, ...)
{
  if(length(wo) == 1)
    wo <- quantile(x, probs = seq(0, 1, 1/wo))
  bruch <- (cut(x, wo,labels=F))
  bruch[is.na(bruch)] <- 1
  res <- vector("list", max(bruch))
  plot(x, y, ...)
  for(i in 1:length(res)) {   
    res[[i]] <- lm(y ~ x, subset = (bruch == i))
    xp <- wo[i:(i + 1)]
    yp <- xp * res[[i]]$coefficients[2] + res[[i]]$coefficients[1]
    lines(xp, yp, col=2)
  }
  if (vertical) abline(v=wo, lty=2)
  res
}

piecewise(x.werte, mtcars$mpg, 10, xlab="Data Points of mpg", ylab="mpg", 
          main = "Piecewise Linear Regression")


#-----------------------------------------------------------------------------------------------
### AUFGABE 2 ### ------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# 2. Führen Sie mit dem Datensatz decay verschiedene Modellierungen des 
#    offensichtlichnichtlinearen Zusammenhanges durch.

decay_data <- read.table(file=paste0(path,"/decay.txt"), header=TRUE)

plot(decay_data) # nicht-linearer zusammenhang zwischen x und y

cor(decay_data$x,decay_data$y)

# lineares modell lm() anschauen
lm_decay <- lm(decay_data$y ~ decay_data$x)
summary(lm_decay)
plot(lm_decay)
outlierTest(lm_decay) 
influencePlot(lm_decay, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" ) 
# component residual plots um nicht-linearitaet von komponenten und 
# residual plot zu analysieren
crPlots(lm_decay) 

# piecewise regression, um besser die sprungstellen zu sehen:
piecewise <- function(x, y, wo=2, vertical=T, ...){
  if(length(wo) == 1)
    wo <- quantile(x, probs = seq(0, 1, 1/wo))
  bruch <- (cut(x, wo,labels=F))
  bruch[is.na(bruch)] <- 1
  res <- vector("list", max(bruch))
  plot(x, y, ...)
  for(i in 1:length(res)) {   
    res[[i]] <- lm(y ~ x, subset = (bruch == i))
    xp <- wo[i:(i + 1)]
    yp <- xp * res[[i]]$coefficients[2] + res[[i]]$coefficients[1]
    lines(xp, yp, col=2)
  }
  if (vertical) abline(v=wo, lty=2)
  res
}

# 3 intervalle:
piecewise(decay_data$x, decay_data$y, 3, xlab="x", ylab="y", 
          main = "Piecewise Linear Regression")
# 5 intervalle:
piecewise(decay_data$x, decay_data$y, 5, xlab="x", ylab="y", 
          main = "Piecewise Linear Regression")


# ADDITIVES MODELL MIT 2 INDIKATOREN:
indikator_15 <- ifelse(decay_data$x>15, 1, 0)
indikator_5 <- ifelse(decay_data$x<5, 1, 0)
lm_decay_indikatoren <- lm(decay_data$y ~ decay_data$x + indikator_15 +
                             indikator_5)
summary(lm_decay_indikatoren)
plot(decay_data$y ~ decay_data$x, main="Additives Modell mit Indikator",
     pch=18, col="azure4")
lines(decay_data$x[decay_data$x > 15], 
      predict(lm_decay_indikatoren)[decay_data$x > 15], 
      col="chocolate", lwd=2)
lines(decay_data$x[decay_data$x < 5], 
      predict(lm_decay_indikatoren)[decay_data$x < 5], 
      col="blue2", lwd=2)
lines(decay_data$x[decay_data$x <= 15 & decay_data$x >= 5], 
      predict(lm_decay_indikatoren)[decay_data$x <= 15 & decay_data$x >= 5], 
      col="seagreen", lwd=2)

# MODELL MIT INTERAKTION:
lm_decay_interaktion <- lm(decay_data$y ~ decay_data$x*indikator_15 +
                                          decay_data$x*indikator_5)
summary(lm_decay_interaktion)
plot(decay_data$y ~ decay_data$x, main="Modell mit Interaktion",
     pch=18, col="azure4")
lines(decay_data$x[decay_data$x > 15], 
      predict(lm_decay_interaktion)[decay_data$x > 15], 
      col="chocolate", lwd=2)
lines(decay_data$x[decay_data$x < 5], 
      predict(lm_decay_interaktion)[decay_data$x < 5], 
      col="blue2", lwd=2)
lines(decay_data$x[decay_data$x <= 15 & decay_data$x >= 5], 
      predict(lm_decay_interaktion)[decay_data$x <= 15 & decay_data$x >= 5], 
      col="seagreen", lwd=2)

# MODELL MIT SPLINES interpolation:
spline_def <- ifelse(decay_data$x > 5, decay_data$x-5, 0)
model_spline <- lm(decay_data$y ~ decay_data$x + spline_def)
summary(model_spline)
plot(decay_data$y ~ decay_data$x, main="Modell mit Spline",
     pch=18, col="azure4")
lines(decay_data$x, fitted(model_spline),
      col="blue2", lwd=2)

# LOGARITHMIERTES MODELL:
log_x <- log(decay_data$x)
log_x <- log_x[2:length(log_x)] # weil log_x[1] ist -Inf
data_y <- decay_data$y[2:length(decay_data$y)]
intervalldaten <- data.frame(x = seq(from = min(log_x), 
                  to = max(log_x), by = 0.10))
model_log <- lm(data_y ~ log_x)
summary(model_log)
model_log2 <- lm(data_y ~ log_x)
summary(model_log2)
plot(x=log_x, y=data_y, main="Logarithmiertes Modell", 
     pch=18, col="azure4", xlab="log(x)", ylab="y")
abline(model_log, col="darkmagenta", lwd=2)

# TRANSFORMATION DURCH WURZEL: 
sqrt_x <- sqrt(decay_data$x)
model_sqrt <- lsfit(sqrt_x,decay_data$y)
ls.print(model_sqrt)
model_sqrt2 <- lm(decay_data$y ~ sqrt_x)
summary(model_sqrt2)
plot(sqrt_x,decay_data$y,main="Transformation mit Wurzel",
     xlab="sqrt(x)", ylab="y", pch=18, col="azure4")
abline(model_sqrt, lty=3, col="chocolate", lwd=2)

# POLYNOMIALES MODELL
x_zentriert  <- decay_data$x-mean(decay_data$x)
x_zentriert2 <- x_zentriert^2
x_zentriert3 <- x_zentriert^3
model_poly <- lm(decay_data$y ~ x_zentriert + x_zentriert2 +
                 x_zentriert3)                                            
summary(model_poly)

plot(decay_data$x, decay_data$y, pch=18,
     main="Polynomiales Modell",
     xlab="x",ylab="y", col="azure4")
lines(decay_data$x, fitted(model_poly), lty=1, 
      col="chocolate", lwd=2)


#-----------------------------------------------------------------------------------------------
### AUFGABE 3 ### ------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# Im Excel-Sheet "Some Datasets" finden Sie 5 kleine Datensätze. 
# Führen Sie für die einzelnen Datensätze regressionsanalytische 
# Auswertungen durch:

# a) WordRecall:      Check for Linearity

wordrecall_data <- read.xlsx(file=paste0(path,"/Some Datasets.xlsx"), 
                             sheetName=1, 
                             startRow=2, endRow=15,
                             colIndex=c(2:3),
                             colNames=TRUE, rowNames = FALSE)
wordrecall_data <- wordrecall_data[ ,-(3:4)]
wordrecall_data

plot(wordrecall_data$time, wordrecall_data$prop, pch=18,
     main="Word Recall", xlab="After Time in Minutes",
     ylab="% of Words Correctly Recalled")

cor(wordrecall_data$time, wordrecall_data$prop)

# lm
model_wordrecall <- lm(wordrecall_data$time ~ wordrecall_data$prop)
summary(model_wordrecall)
crPlots(model_wordrecall) 

par(mfrow=c(2,2))
plot(model_wordrecall)

# piecewise linear regression
piecewise(wordrecall_data$time, wordrecall_data$prop, 3, 
          xlab="% of Words Correctly Recalled", 
          ylab="After Time in Minutes", 
          main = "Piecewise Linear Regression")

# local regression (loess)
par(mfrow=c(1,1))
ggplot(wordrecall_data, aes(time, prop)) +
  stat_smooth(span=0.5,method=loess, col="chocolate") +
  geom_point() +
  ylab("prop") +
  xlab ("time") +
  ggtitle("Influence of time on prop - LM vs. LOESS") +
  stat_smooth(method=lm, se = TRUE)


# b) ShortLeaf: Check for Linearity and Influential Observations

shortleaf_data <- read.xlsx(file=paste0(path,"/Some Datasets.xlsx"), 
                            sheetName=2, 
                            startRow=1, endRow=71,
                            colIndex=c(1:2),
                            colNames=TRUE, rowNames = FALSE)
shortleaf_data <- shortleaf_data[ ,-(3:4)]
shortleaf_data

plot(shortleaf_data$Vol, shortleaf_data$Diam, pch=18,
     main="Shortleaf Pines", xlab="Diameter",
     ylab="Volume")

cor(shortleaf_data$Vol, shortleaf_data$Diam)

# lm
model_shortleaf <- lm(shortleaf_data$Vol ~ shortleaf_data$Diam)
summary(model_shortleaf)
crPlots(model_shortleaf) 


# piecewise linear regression
piecewise(shortleaf_data$Vol, shortleaf_data$Diam, 3, 
          xlab="Diameter", 
          ylab="Volume", 
          main = "Piecewise Linear Regression")

# local regression (loess)
ggplot(shortleaf_data, aes(Diam, Vol)) +
  stat_smooth(span=0.5,method=loess, col="chocolate") +
  geom_point() +
  ylab("Volume") +
  xlab ("Diameter") +
  ggtitle("Influence of Diam on Vol - LM vs. LOESS") +
  stat_smooth(method=lm, se = TRUE)

# REGRESSION DIAGNOSTICS
par(mfrow=c(2,2))
plot(model_shortleaf)
influence.measures(model_shortleaf)

par(mfrow=c(1,1))
influencePlot(model_shortleaf, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" ) 

# COOK'S DISTANCE MEASURE
plot(cooks.distance(model_shortleaf), type="h", 
     main="Cook's Distance")
abline(h=4/length(influences), col="indianred3", lty=5, lwd=2)

# DDFITS
influences <- lm.influence(model_shortleaf)$hat
plot(dffits(model_shortleaf), type = "h", main = "DFFITS")
abline(h=2*sqrt(length(model_shortleaf$coef)/length(influences)), 
       col="indianred3", lty=5, lwd=2)
abline(h=-2*sqrt(length(model_shortleaf$coef)/length(influences)), 
       col="indianred3", lty=5, lwd=2)


# c) BirthWeight: Use Indicator Variables

birthweight_data <- read.xlsx(file=paste0(path,"/Some Datasets.xlsx"), 
                              sheetName=3, 
                              startRow=1, endRow=33,
                              colIndex=c(1:3),
                              colNames=TRUE, rowNames = FALSE)
birthweight_data <- birthweight_data[ ,-(4:5)]
birthweight_data

# additives modell mit indikator fuer "smoke status":
I_smoke <- ifelse(birthweight_data$Smoke == "yes", 1, 0)
model_I_smoke <- lm(birthweight_data$Wgt ~ birthweight_data$Gest + I_smoke)
summary(model_I_smoke)

# modell mit interaktion zwischen weight und indikator
model_interaktion_I <- lm(birthweight_data$Wgt ~ birthweight_data$Gest*I_smoke)
summary(model_interaktion_I)

# overall LM: (ohne beachtung von smoke status)
model_birthweight <- lm(birthweight_data$Wgt ~ birthweight_data$Gest)
summary(model_birthweight)
par(mfrow=c(2,2))
plot(model_birthweight)

# mit indikatorvariablen (fuer smoke status) 
model_smokeYes <- lm(birthweight_data$Wgt ~ birthweight_data$Gest,
                     subset=birthweight_data$Smoke=="yes")
summary(model_smokeYes)
par(mfrow=c(2,2))
plot(model_smokeYes)

model_smokeNo <- lm(birthweight_data$Wgt ~ birthweight_data$Gest,
                    subset=birthweight_data$Smoke=="no")
summary(model_smokeNo)
par(mfrow=c(2,2))
plot(model_smokeNo)

par(mfrow=c(1,1))
plot(birthweight_data$Wgt ~ birthweight_data$Gest, 
     pch=18, col="azure4", 
     main="Einfluss von Smoke Status auf Weight",
     xlab="Gestation", ylab="Weight")
abline(model_birthweight, col="lightslateblue", lwd=3)
abline(model_smokeNo, col="chocolate", lwd=3)
abline(model_smokeYes, col="darkred", lwd=3)
legend("topleft", legend=c("Smoke: no", "Smoke: yes", "Overall"), 
       col=c("chocolate", "darkred", "lightslateblue"), lwd=3)


# d) Anti-Depressiva: Use Indicator Variables

antidepr_data <- read.xlsx(file=paste0(path,"/Some Datasets.xlsx"), 
                           sheetName=4, 
                           startRow=1, endRow=37,
                           colIndex=c(1:3),
                           colNames=TRUE, rowNames = FALSE)
antidepr_data <- antidepr_data[ ,-(4:5)]
antidepr_data

# overall LM: (ohne beachtung von smoke status)
model_antidepr <- lm(antidepr_data$y ~ antidepr_data$age)
summary(model_antidepr)
par(mfrow=c(2,2))
plot(model_antidepr)

# mit indikatorvariablen (fuer smoke status) 
model_antidepr_TRTA <- lm(antidepr_data$y ~ antidepr_data$age,
                          subset=antidepr_data$TRT=="A")
summary(model_antidepr_TRTA)
par(mfrow=c(2,2))
plot(model_antidepr_TRTA)

model_antidepr_TRTB <- lm(antidepr_data$y ~ antidepr_data$age,
                          subset=antidepr_data$TRT=="B")
summary(model_antidepr_TRTB)
par(mfrow=c(2,2))
plot(model_antidepr_TRTB)

model_antidepr_TRTC <- lm(antidepr_data$y ~ antidepr_data$age,
                          subset=antidepr_data$TRT=="C")
summary(model_antidepr_TRTC)
par(mfrow=c(2,2))
plot(model_antidepr_TRTC)


par(mfrow=c(1,1))
plot(antidepr_data$y ~ antidepr_data$age, 
     pch=18, col="azure4", 
     main="Einfluss von Treatment auf Antidepr.",
     xlab="Age", ylab="Antidepr.")
abline(model_antidepr_TRTA, col="lightslateblue", lwd=3)
abline(model_antidepr_TRTB, col="chocolate", lwd=3)
abline(model_antidepr_TRTC, col="darkred", lwd=3)
abline(model_antidepr, col="limegreen", lwd=3)
legend("topleft", legend=c("Overall Model", "TRT A", "TRT B", "TRT C"), 
       col=c("limegreen", "lightslateblue", "chocolate", "darkred"), 
       lwd=3)

# indikatorvariable im additiven modell
indikator_TRT <- as.numeric(antidepr_data$TRT) 
model_indik_TRT <- lm(antidepr_data$y ~ antidepr_data$age + indikator_TRT) 
summary(model_indik_TRT)

# indikatorvariable im modell mit interaktion
model_indik_interaktion <- lm(antidepr_data$y ~ antidepr_data$age*indikator_TRT) 
summary(model_indik_interaktion)

#-----------------------------------------------------------------------------------------------











