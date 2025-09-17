---
title: "Predicting PSA Levels from Prognostic Clinical Measurements in Men with Advanced Prostate Cancer"
author: "Lindsey Hornberger"
date: "2023-07-24"
output:
html_document:
df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
setwd("/Users/lindseyhornberger/Desktop/STAT 823/FINAL")
library(tidyverse)
library(modelr)
library(broom)
library(readxl)
library(psych)
library(car)
library(leaps)
library(lmtest)
pcancer <- read_excel("pcancer.xlsx")
pcancer <- pcancer %>% rename("CV" = "cancerv", "Age" = "age", "PW" = "weight", "BPH" = "hyperplasia", "SVI"= seminal, "CP" = "capsular", "GS" = "score")
```

# Boxplots of Predictors 
```{r}
# Boxplot of Cancer Volume
ggplot(pcancer, aes(x=CV)) +
  labs(x ="(a) Boxplot of Cancer Volume") +
  xlim(0,50) +
  ylim(-1,1) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot()

# Boxplot of Weight
ggplot(pcancer, aes(x=PW)) +
  labs(x ="(b) Boxplot of Weight") +
  xlim(0,500) +
  ylim(-1,1) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot()

# Boxplot of Age
ggplot(pcancer, aes(x=Age)) +
  labs(x ="(c) Boxplot of Age") +
  xlim(35,90) +
  ylim(-1,1) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot()

# Boxplot of Hyperplasia
ggplot(pcancer, aes(x=BPH)) +
  labs(x ="(d) Boxplot of Hyperplasia") +
  xlim(0,12) +
  ylim(-1,1) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot()

# Boxplot of Seminal vesicle invasion
ggplot(pcancer, aes(x=SVI)) +
  labs(x ="(e) Boxplot of Seminal vesicle invasion") +
  xlim(-1,3) +
  ylim(-1,1) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot()
# Boxplot of Capsular penetration
ggplot(pcancer, aes(x=CP)) +
  labs(x ="(f) Boxplot of Capsular penetration") +
  xlim(0,20) +
  ylim(-1,1) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot()

# Bloxplot of Gleason Score
ggplot(pcancer, aes(x=GS)) +
  labs(x ="(g) Boxplot of Gleason Scores") +
  xlim(5,10) +
  ylim(-1,1) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot()

```
# Analysis of Prostate-Specific Antigen Level (PSA)
```{r}
# Boxplot of Response Variable (PSA)
ggplot(pcancer, aes(x=psa)) +
  labs(x ="(a) Boxplot of Prostate-Specific Antigen Level (PSA)") +
  xlim(0,275) +
  ylim(-1,1) +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot()

# plot of PSA 
ggplot(pcancer, aes(x=psa)) +
  labs(x ="(b) Prostate-Specific Antigen Level (PSA)", y="Frequency") +
  theme_bw() + 
  geom_histogram(binwidth = 15)
```



# PSA vs. Cancer Volume 
```{r}
plot(psa~CV, pcancer, xlim=c(0,50), ylim=c(0,300), xlab="Cancer Volume in cc (CV)", ylab = "Prostate Specific Antigen (PSA)")
abline(lm(psa~CV, pcancer))

pcancer %>% 
  ggplot(aes(x=CV, y=psa)) +
  labs(x ="Cancer Volume in cc (CV)", y = "Prostate Specific Antigen (PSA)")+ 
  geom_point(alpha=0.1)+
  geom_smooth(method=lm)

mod_v <- pcancer %>% lm(psa ~ CV, data = .) 
mod_v %>% tidy()
mod_v %>% glance() %>% select(r.squared, adj.r.squared)
modelcv <- lm(psa ~ CV, pcancer)
summary(modelcv)
```

## PSA vs. Weight
```{r}
plot(psa~PW, pcancer, xlim=c(0,500), ylim=c(0,300), xlab="Prostate Weight in kg (PW)", ylab = "Prostate Specific Antigen (PSA)")
abline(lm(psa~PW, pcancer))

pcancer %>% 
  ggplot(aes(x=PW, y=psa)) +
  labs(x ="Prostate Weight in kg (PW)", y = "Prostate Specific Antigen (PSA)")+ 
  geom_point(alpha=0.1)+
  geom_smooth(method=lm)

mod_w <- pcancer %>% lm(psa ~ PW, data = .) 
mod_w %>% tidy()
mod_w %>% glance() %>% select(r.squared, adj.r.squared)
modelweight <- lm(psa ~ PW, pcancer)
summary(modelweight)
```

# PSA vs. Age 
```{r}
plot(psa~Age, pcancer, xlim=c(40,85), ylim=c(0,300), xlab="Patient Age (PA)", ylab = "Prostate Specific Antigen (PSA)")
abline(lm(psa~Age, pcancer))

pcancer %>% 
  ggplot(aes(x=Age, y=psa)) +
  labs(x ="Patient Age (PA)", y = "Prostate Specific Antigen (PSA)")+ 
  geom_point(alpha=0.1)+
  geom_smooth(method=lm)

mod_a <- pcancer %>% lm(psa ~ Age, data = .) 
mod_a %>% tidy()
mod_a %>% glance() %>% select(r.squared, adj.r.squared)
modelage <- lm(psa ~ Age, pcancer)
summary(modelage)
```

# PSA vs. Hyperplasia
```{r}
plot(psa~BPH, pcancer, xlim=c(0,11), ylim=c(0,300), xlab="Hyperplasia in cm^2 (BPH)", ylab = "Prostate Specific Antigen (PSA)")
abline(lm(psa~BPH, pcancer))

pcancer %>% 
  ggplot(aes(x=BPH, y=psa)) +
  labs(x ="Hyperplasia in cm^2 (BPH)", y = "Prostate Specific Antigen (PSA)")+ 
  geom_point(alpha=0.1)+
  geom_smooth(method=lm)

mod_hyper <- pcancer %>% lm(psa ~ BPH, data = .) 
mod_hyper %>% tidy()
mod_hyper %>% glance() %>% select(r.squared, adj.r.squared)
model <- lm(psa ~ BPH, pcancer)
summary(model)
```

# PSA vs. Seminal 
```{r}
plot(psa~SVI, pcancer, xlim=c(0,1), ylim=c(0,300), xlab="Seminal Vesicle Invasion (SVI) (1 if yes, 0 if other)", ylab = "Prostate Specific Antigen (PSA)")
abline(lm(psa~SVI, pcancer))

pcancer %>% 
  ggplot(aes(x=SVI, y=psa)) +
  labs(x ="Seminal Vesicle Invasion (SVI) (1 if yes, 0 if other)", y = "Prostate Specific Antigen (PSA)")+ 
  geom_point(alpha=0.1)+
  geom_smooth(method=lm)

mod_sem <- pcancer %>% lm(psa ~ SVI, data = .) 
mod_sem %>% tidy()
mod_sem %>% glance() %>% select(r.squared, adj.r.squared)
model <- lm(psa ~ SVI, pcancer)
summary(model)
```

# PSA vs. Capsular
```{r}
plot(psa~CP, pcancer, xlim=c(0,20), ylim=c(0,300), xlab="Capsular Penetratrion in cm (CP)", ylab = "Prostate Specific Antigen (PSA)")
abline(lm(psa~CP, pcancer))

pcancer %>% 
  ggplot(aes(x=CP, y=psa)) +
  labs(x ="Capsular Penetratrion in cm (CP)", y = "Prostate Specific Antigen (PSA)")+ 
  geom_point(alpha=0.1)+
  geom_smooth(method=lm)

mod_cap <- pcancer %>% lm(psa ~ CP, data = .) 
mod_cap %>% tidy()
model <- lm(psa ~ CP, pcancer)
summary(model)
```

# PSA vs. Gleason Score 
```{r}
plot(psa~GS, pcancer, xlim=c(5,9), ylim=c(0,300), xlab="Gleason Score (GS)", ylab = "Prostate Specific Antigen (PSA)")
abline(lm(psa~GS, pcancer))
model <- lm(psa~GS, pcancer)
summary(model)
pcancer %>% 
  ggplot(aes(x=GS, y=psa)) +
  labs(x ="Gleason Score (GS)", y = "Prostate Specific Antigen (PSA)")+ 
  geom_point(alpha=0.1)+
  geom_smooth(method=lm)

mod_score <- pcancer %>% lm(psa ~ GS, data = .) 
mod_score %>% tidy()
model <- lm(psa~GS, pcancer)
summary(model)
```

# Faucets ??
```{r}
#mod_faucet <- pcancer %>% lm(psa ~ CV + PW + Age + BPH + SVI + CP + GS, data = pcancer, data = .) 
#pcancer %>% 
  #add_predictions(mod_faucet) %>%  
  #ggplot(mapping=aes(x=psa, y=pred,color=GS)) +geom_line() + facet_wrap(~Age)
```

# Multicolinearity between predictors 
```{r}
ppcancer <- pcancer %>% select(-idnum)
pairs.panels(ppcancer, ellipses = FALSE,
density = FALSE)
cor(pcancer)

psa.vif <- lm(psa~.,data=pcancer)
vif(psa.vif)

```

# Model Selection
```{r}
avsm <- regsubsets(psa ~ CV + PW + Age + BPH + SVI + CP + GS, data = pcancer)
sum <- summary(avsm)
sum$which
sum$cp
sum$bic
sum$adjr2
```

# Plotting Residuals 
```{r}
my.lm <- lm(psa ~ CV + PW + Age + BPH + SVI + CP + GS, data = pcancer)

crPlots(my.lm,col = "pink", layout = NA, ylab = "βx + Residuals", col.lines = c("darkred",
"blue"))
```

# Detect presence of outliers 
```{r}
plot(fitted(my.lm), rstudent(my.lm),
col = "darkseagreen",
xlab = expression(hat(Y)),
ylab = "Studentized residuals",
main = "(a)",
cex.lab = 0.7, cex = 0.5)
abline(h = 0, lty = 2, lwd = 3,
col = "gray36")
abline(h = 3, lty = 3, lwd = 3,
col = "gray36")
abline(h = -3, lty = 3, lwd = 3,
col = "gray36")

# cooks distance 
cooksd <- cooks.distance(my.lm)
plot(cooksd, pch = "*",
main = "(b) Influential Obs by Cooks distance",
ylab = "Cooks distance", cex.lab = 0.7)
text(x = 1:length(cooksd) + 1,
y = cooksd, labels = ifelse(cooksd >
0.01, names(cooksd), ""),
col = "red")
#halfnorm(cooksd)
```

# Dataset without outliers 
```{r}
no.outs <- pcancer[-c(39, 47, 55, 62, 64, 74, 75, 76, 79, 86, 94, 95, 96, 97),]
new.with.outs.model <- lm(psa ~ CV + SVI + GS, pcancer)
no.outs.model <- lm(psa ~ CV + SVI + GS, no.outs)
summary(new.with.outs.model)
summary(no.outs.model)
```

# Residuals Diagnostics 
```{r}
plot(fitted(no.outs.model), residuals(no.outs.model),
col = "skyblue2", main = "(a)",
xlab = expression("Fitted values" *
hat(Y)), ylab = "Residuals")
abline(h = 0, col = "gray26", lwd = 2,
lty = 2)

#Normality 
qqnorm(residuals(no.outs.model), col = "lightblue",
main = "(b)")
qqline(residuals(no.outs.model), col = "darkgray",
lwd = 2)
hist(residuals(no.outs.model), col = "lightblue", main = "(c)",
xlab = "Residuals")

# Independence of error terms
plot(residuals(no.outs.model), type = "l",
col = "skyblue3", main = "(d)",
ylab = "Residuals")
```
# R squared values and t tests 
```{r}
library(lmtest)
t <- qt(0.95, (97 - 2))

summary(lm(psa~CV, no.outs))
summary(lm(psa~SVI, no.outs))
summary(lm(psa~GS, no.outs))
```

# Validating Model Predictions
```{r}
no.outs.model <- lm(psa ~ CV + SVI + GS, no.outs)

#1 
predict(no.outs.model, data.frame(CV = 5.6407, SVI=1, GS=7), level = 0.95, interval = "prediction")

#2
predict(no.outs.model, data.frame(CV = 2.6379,SVI=0, GS=7), level = 0.95, interval = "prediction")
#3
predict(no.outs.model, data.frame(CV = 16.6099, SVI=1, GS=8), level = 0.95, interval = "prediction")
#4
predict(no.outs.model, data.frame(CV = 0.2592, SVI=0, GS=6), level = 0.95, interval = "prediction")

#5
predict(no.outs.model, data.frame(CV = 5.7546, SVI=0, GS=6), level = 0.95, interval = "prediction")
```
# ANOVA
```{r}
anova(no.outs.model)
model.summary <- summary(no.outs.model)
mean(model.summary$residuals^2)

```


