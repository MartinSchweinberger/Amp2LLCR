##################################################################
# Titel:      How Corpus-Based Methods Can Support Language Learning 
#             and Teaching: An Analysis of Amplifier Use by L1-Speakers 
#             and Learners of English - Part 5
# R version:  3.5.1 (2018-07-02) -- "Feather Spray"
# Autor:      Martin Schweinberger
# Date:       2019-02-02
# Contact:    martin.schweinberger.hh@gmail.com
# Disclaimer: If you have questions,suggestions or you found errors
#             or in case you would to provide feedback, questions
#             write an email to martin.schweinberger.hh@gmail.com.
# Citation:   If you use this script or results thereof, please cite it as:
#             Schweinberger, Martin. 2018. "How Corpus-Based Methods 
#             Can Support Language Learning and Teaching: An Analysis 
#             of Amplifier Use by L1-Speakers and Learners of English - Part 5",
#             unpublished R script, The University of Queensland.
###############################################################
rm(list=ls(all=T))                                      # clean current workspace
setwd("D:\\Uni\\Projekte\\02-Intensification\\Amp2L")   # set wd
# load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggplot2)       # activate ggplot2 library for plotting
library(cowplot)       # activate cowplot library for modifying ggplot2
library(randomForest)  # activate randomForest library for random forests
# set options
options(stringsAsFactors = F)
options(scipen = 999)
options(max.print=10000)
# define image directory
imageDirectory<-"images"
###############################################################
# load data
ampicle <- read.table("ampicle_statz06.txt", sep = "\t", header = T)
# inspect data
nrow(ampicle); str(ampicle); head(ampicle)

# simplify Variant
frqvrnt <- names(table(ampicle$Variant))[which(table(ampicle$Variant) > 200)]
ampicle$Variant <- ifelse(ampicle$Variant %in% frqvrnt, ampicle$Variant, "other")
# factorize variables
clfct <- c("Variant", "Language", "File", "Adjective", "Function", "Priming", 
           "SemanticCategory", "Gradability", "Emotionality")
ampicle[clfct] <- lapply(ampicle[clfct], factor)

# extract native-speaker data
ampicle$very <- NULL
ampicle$really <- NULL
ampicle$other <- NULL
ampicle$completely <- NULL
ampicle$File <- NULL
PolyFrequency <- poly(ampicle$Frequency, degree = 2, simple = T)
ampicle$PolyFrequency <- PolyFrequency[,2]
# inspect data
nrow(ampicle); str(ampicle); head(ampicle)

###
nsd <- ampicle[ampicle$Language == "English",]
nsd$Language <- NULL
nsd$Proficiency <- NULL
#nsd$Frequency  <- as.vector(unlist(scale(nsd$Frequency)))
nsd$Frequency <- nsd$PolyFrequency
nsd$PolyFrequency <- NULL
head(nsd); str(nsd)

###########################################################################
#           RANDOM FOREST: NATIVE-SPEAKRES
# set seed
set.seed(201905101)
nsrf <- randomForest(Variant ~ ., data=nsd, ntree=3000, proximity=TRUE)
nsrf 

# set seed
set.seed(201905102)
nsrf <- randomForest(Variant ~ ., data=nsd, ntree=5000, proximity=TRUE, mtry = 2)
nsrf 

# plot new precision/error rate
oob.error.data <- data.frame(
  Trees=rep(1:nrow(nsrf$err.rate), times=7),
  Type=rep(c("OOB", "completely", "extremely", "other", "really", "so", "very"), each=nrow(nsrf$err.rate)),
  Error=c(nsrf$err.rate[,"OOB"],
          nsrf$err.rate[,"completely"],
          nsrf$err.rate[,"extremely"],
          nsrf$err.rate[,"other"],
          nsrf$err.rate[,"really"],
          nsrf$err.rate[,"so"],
          nsrf$err.rate[,"very"]))
ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

# determine accuracy by prediction
# install package
#source("https://bioconductor.org/biocLite.R"); biocLite(); library(Biobase)
#install.packages("Biobase", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com", 
#                                      "http://cran.rstudio.com/", dependencies=TRUE))
#install.packages("dimRed", dependencies = TRUE)
#install.packages('caret', dependencies = TRUE)
# load caret library
library(caret) # because initially caret did not work, the libraries above had to be installed
id <- sample(2, nrow(nsd), replace = T, prob = c(.7, .3))
train <- nsd[id == 1, ]
test <- nsd[id == 2,]

ptrain <- predict(nsrf, train)       # extract prediction for training data
head(ptrain); head(train$Variant)         # inspect predictions

confusionMatrix(ptrain, train$Variant)

ptest <- predict(nsrf, test)
confusionMatrix(ptest, test$Variant)

png("images/VarImpRF1.png",  width = 680, height = 480) # save plot
varImpPlot(nsrf, main = "", pch = 20, cex = 2) 
dev.off()

pnsrf <- predict(nsrf, nsd)
confusionMatrix(pnsrf, nsd$Variant)

0.703/0.5743

###########################################################################
#           RANDOM FOREST: NON-NATIVE-SPEAKERS
nnsd <- ampicle[ampicle$Language != "English",]
nnsd$Frequency <- nnsd$PolyFrequency
#nnsd$PolyFrequency <- NULL
nnsd$Proficiency <- as.vector(unlist(scale(nnsd$Proficiency)))
head(nnsd); str(nnsd)

pnns <- predict(nsrf, nnsd)       # extract prediction for training data
head(pnns); head(nnsd$Variant)       # inspect predictions
confusionMatrix(pnns, nnsd$Variant)

0.5374/0.5596

# add native choice prediction to data
nnsd$NativeChoice <- as.vector(pnns)
nnsd$NativeChoice <- as.factor(nnsd$NativeChoice)
# code if choice of nns is nativelike or not
nnsd$Variant <- as.character(nnsd$Variant)
nnsd$NativeChoice <- as.character(nnsd$NativeChoice)
nnsd$NonNativeLike <- ifelse(nnsd$Variant == nnsd$NativeChoice, 0, 1)
# inspect new data
head(nnsd)

###########################################################################
# load library
library(rms)
# set options
options(contrasts  =c("contr.treatment", "contr.poly"))
nnsd.dist <- datadist(nnsd)
options(datadist = "nnsd.dist")
# generate initial minimal regression model 
m0.glm = glm(NonNativeLike ~ 1, family = binomial, data = nnsd) # baseline model glm
m0.lrm = lrm(NonNativeLike ~ 1, data = nnsd, x = T, y = T) # baseline model lrm
# inspect results
summary(m0.glm)

m0.lrm

# load library
library(lme4)
library(car)
# create model with a random intercept for token
m0.lmer <- lmer(NonNativeLike ~ (1|Adjective), data = nnsd, family = binomial)
# results of the lmer object
print(m0.lmer, corr = F)

# Baayen (2008:278-284) uses the function above but this function is no longer
# up-to-date because the "family" parameter is deprecated
# we switch to glmer (suggested by R) and also create a lmer object of 
# the final minimal adequate model as some functions do not (yet) work on glmer
m0a.glmer = glmer(NonNativeLike ~ (1|Adjective), data = nnsd, family = binomial)
m0b.glmer = glmer(NonNativeLike ~ (1|Language), data = nnsd, family = binomial)
m0c.glmer = glmer(NonNativeLike ~ (1|Adjective)+(1|Language), data = nnsd, family = binomial)
m0d.glmer = glmer(NonNativeLike ~ (1|Language/Adjective), data = nnsd, family = binomial) # singular fit!
# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.m0a.glmer <- AIC(logLik(m0a.glmer))
aic.m0b.glmer <- AIC(logLik(m0b.glmer)) 
aic.m0c.glmer <- AIC(logLik(m0c.glmer)) 
aic.m0d.glmer <- AIC(logLik(m0d.glmer)) 
aic.glm <- AIC(logLik(m0.glm))
aic.m0a.glmer; aic.m0b.glmer; aic.m0c.glmer; aic.m0d.glmer; aic.glm

# the aic of  glmer object c ((1|Adjective)+(1|Language)) is consitently smaller 
# than the aic of both the glm object and the other glmer objects.
# this means that including random intercepts is justified
m0.glmer = glmer(NonNativeLike ~ (1|Adjective)+(1|Language), data = nnsd, family = binomial)

# test random effects
null.id = -2 * logLik(m0.glm) + 2 * logLik(m0.glmer)
pchisq(as.numeric(null.id), df=1, lower.tail=F) # sig m0.glmer better than m0.glm

# inspect results
summary(m0.glm)

summary(m0.glmer)

###########################################################################
# model fitting
# fit the model to find the "best" model, i.e. the minimal adequate model
# we will use a step-wise step up procedure
# we need to add "control = glmerControl(optimizer = "bobyqa")" 
# because otherwise R fails to converge
#	manual modelfitting
m0.glmer <- glmer(NonNativeLike ~ 1 + (1|Adjective)+(1|Language), family = binomial, 
                  data = nnsd, control=glmerControl(optimizer="bobyqa"))
# add Function
ifelse(min(ftable(nnsd$Function, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m1.glm <- update(m0.glm, .~.+Function)
m1.glmer <- update(m0.glmer, .~.+Function)
anova(m1.glmer, m0.glmer, test = "Chi") # SIG! (p<0.00000000000000022 ***)

# add Priming
ifelse(min(ftable(nnsd$Priming, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m2.glm <- update(m1.glm, .~.+Priming)
ifelse(max(vif(m2.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m2.glmer <- update(m1.glmer, .~.+Priming)
anova(m2.glmer, m1.glmer, test = "Chi") # SIG! (p=0.00000000000000022 ***)
Anova(m2.glmer, type = "III", test = "Chi")

# add Proficiency
m3.glm <- update(m2.glm, .~.+Proficiency)
ifelse(max(vif(m3.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m3.glmer <- update(m2.glmer, .~.+Proficiency)
anova(m3.glmer, m2.glmer, test = "Chi") # SIG! (p=0.0157*)
Anova(m3.glmer, type = "III", test = "Chi")

# add Frequency
m4.glm <- update(m3.glm, .~.+Frequency)
ifelse(max(vif(m4.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m4.glmer <- update(m3.glmer, .~.+Frequency)
anova(m4.glmer, m3.glmer, test = "Chi") # SIG! (p=0.0000000001483 ***)
Anova(m4.glmer, type = "III", test = "Chi")

# add Gradability
ifelse(min(ftable(nnsd$Gradability, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m5.glm <- update(m4.glm, .~.+Gradability)
ifelse(max(vif(m5.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m5.glmer <- update(m4.glmer, .~.+Gradability)
anova(m5.glmer, m4.glmer, test = "Chi") # SIG! (p=0.00000000000001719***)
Anova(m5.glmer, type = "III", test = "Chi")

# add SemanticCategory
ifelse(min(ftable(nnsd$SemanticCategory, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m6.glm <- update(m5.glm, .~.+SemanticCategory)
ifelse(max(vif(m6.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Emotionality
ifelse(min(ftable(nnsd$Emotionality, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m7.glm <- update(m5.glm, .~.+Emotionality)
ifelse(max(vif(m7.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m7.glmer <- update(m5.glmer, .~.+Emotionality)
anova(m7.glmer, m5.glmer, test = "Chi") # SIG! (p=0.000000000000001977***)
Anova(m7.glmer, type = "III", test = "Chi")

###########################################################################
# find all 2-way interactions
library(utils)
colnames(nnsd)
# define variables included in interactions
vars <- c("Function", "Priming", "Proficiency", "Frequency", "Gradability",
          "SemanticCategory", "Emotionality")
intac <- t(combn(vars, 2))
intac

# add Function*Priming
ifelse(min(ftable(nnsd$Function, nnsd$Priming, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m10.glm <- update(m7.glm, .~.+Function*Priming)
ifelse(max(vif(m10.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Function*Proficiency
m11.glm <- update(m7.glm, .~.+Function*Proficiency)
ifelse(max(vif(m11.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m11.glmer <- update(m7.glmer, .~.+Function*Proficiency)
anova(m11.glmer, m7.glmer, test = "Chi") # not sig (p=0.4293) 

# add Function*Frequency
m12.glm <- update(m7.glm, .~.+Function*Frequency)
ifelse(max(vif(m12.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m12.glmer <- update(m7.glmer, .~.+Function*Frequency)
anova(m12.glmer, m7.glmer, test = "Chi") # SIG! (p=0.03612 *)
Anova(m12.glmer, type = "III", test = "Chi")

# add Function*Gradability
ifelse(min(ftable(nnsd$Function, nnsd$Gradability, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m13.glm <- update(m12.glm, .~.+Function*Gradability)
ifelse(max(vif(m13.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Function*SemanticCategory
ifelse(min(ftable(nnsd$Function, nnsd$SemanticCategory, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m14.glm <- update(m12.glm, .~.+Function*SemanticCategory)
ifelse(max(vif(m14.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Function*Emotionality
ifelse(min(ftable(nnsd$Function, nnsd$Emotionality, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m15.glm <- update(m12.glm, .~.+Function*Emotionality)
ifelse(max(vif(m15.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Priming*Proficiency
m16.glm <- update(m12.glm, .~.+Priming*Proficiency)
ifelse(max(vif(m16.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m16.glmer <- update(m12.glmer, .~.+Priming*Proficiency)
anova(m16.glmer, m12.glmer, test = "Chi") # not sig (p=0.5157)

# add Priming*Frequency
m17.glm <- update(m12.glm, .~.+Priming*Frequency)
ifelse(max(vif(m17.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m17.glmer <- update(m12.glmer, .~.+Priming*Frequency)
anova(m17.glmer, m12.glmer, test = "Chi") # not sig (p=0.1498)

# add Priming*Gradability
ifelse(min(ftable(nnsd$Priming, nnsd$Gradability, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m18.glm <- update(m12.glm, .~.+Priming*Gradability)
ifelse(max(vif(m18.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Priming*SemanticCategory
ifelse(min(ftable(nnsd$Priming, nnsd$SemanticCategory, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m19.glm <- update(m12.glm, .~.+Priming*SemanticCategory)
ifelse(max(vif(m19.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Priming*Emotionality
ifelse(min(ftable(nnsd$Priming, nnsd$Emotionality, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m20.glm <- update(m12.glm, .~.+Priming*Emotionality)
ifelse(max(vif(m20.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Proficiency*Frequency
m21.glm <- update(m12.glm, .~.+Proficiency*Frequency)
ifelse(max(vif(m20.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Proficiency*Gradability
m22.glm <- update(m12.glm, .~.+Proficiency*Gradability)
ifelse(max(vif(m22.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Proficiency*SemanticCategory
m23.glm <- update(m12.glm, .~.+Proficiency*SemanticCategory)
ifelse(max(vif(m23.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Proficiency*Emotionality
m24.glm <- update(m12.glm, .~.+Proficiency*Emotionality)
ifelse(max(vif(m24.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Frequency*Gradability
m25.glm <- update(m12.glm, .~.+Frequency*Gradability)
ifelse(max(vif(m25.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Frequency*SemanticCategory
m26.glm <- update(m12.glm, .~.+Frequency*SemanticCategory)
ifelse(max(vif(m26.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Gradability*SemanticCategory
ifelse(min(ftable(nnsd$Gradability, nnsd$SemanticCategory, nnsd$NonNativeLike)) == 0, "not possible", "possible")

# add Gradability*Emotionality
ifelse(min(ftable(nnsd$Gradability, nnsd$Emotionality, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m27.glm <- update(m12.glm, .~.+Gradability*Emotionality)
ifelse(max(vif(m27.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add SemanticCategory*Emotionality
ifelse(min(ftable(nnsd$SemanticCategory, nnsd$Emotionality, nnsd$NonNativeLike)) == 0, "not possible", "possible")

#########################################
# load function for regression table summary
source("D:\\R/meblr.summary.tworandom.R")
# set up summary table
meblrm_nnsd <- meblrm.summary(m0.glm, m12.glm, m0.glmer, m12.glmer, nnsd$NonNativeLike) #
meblrm_nnsd

# save results to disc
write.table(meblrm_nnsd, "meblrm_nnsd.txt", sep="\t")

# load function
library(car)
meblrm_nnsd_Anova <- Anova(m12.glmer, type = "III", test = "Chi")
meblrm_nnsd_Anova

# save results to disc
write.table(meblrm_nnsd_Anova, "meblrm_nnsd_Anova.txt", sep="\t")

effectfunction <- anova(m0.glmer, m1.glmer, test = "Chi")

effectpriming <- anova(m2.glmer, m1.glmer, test = "Chi")

effectproficiency <- anova(m3.glmer, m2.glmer, test = "Chi")

effectfrequency <- anova(m4.glmer, m3.glmer, test = "Chi")

effectgradability <- anova(m5.glmer, m4.glmer, test = "Chi")

effectemotionality <- anova(m7.glmer, m5.glmer, test = "Chi")

effectfunction_frequency <- anova(m12.glmer, m7.glmer, test = "Chi")

# use customized model comparison function
# create comparisons
m1.m0 <- anova(m1.glmer, m0.glmer, test = "Chi") # SIG! (p<0.00000000000000022 ***)
m2.m1 <- anova(m2.glmer, m1.glmer, test = "Chi") # SIG! (p=0.00000000000000022 ***)
m3.m2 <- anova(m3.glmer, m2.glmer, test = "Chi") # SIG! (p=0.0157*)
m4.m3 <- anova(m4.glmer, m3.glmer, test = "Chi") # SIG! (p=0.0000000001483 ***)
m5.m4 <- anova(m5.glmer, m4.glmer, test = "Chi") # SIG! (p=0.00000000000001719***)
m7.m5 <- anova(m7.glmer, m5.glmer, test = "Chi") # SIG! (p=0.000000000000001977***)
m11.m7 <- anova(m11.glmer, m7.glmer, test = "Chi") # not sig (p=0.4293) 
m12.m7 <- anova(m12.glmer, m7.glmer, test = "Chi") # SIG! (p=0.03612 *)
m16.m12 <- anova(m16.glmer, m12.glmer, test = "Chi") # not sig (p=0.5157)
m17.m12 <- anova(m17.glmer, m12.glmer, test = "Chi") # not sig (p=0.1498)
# create a list of the model compariNonNativeLikens
mdlcmp <- list(m1.m0, m2.m1, m3.m2, m4.m3, m5.m4, m7.m5, m11.m7, m12.m7, m16.m12, m17.m12)
# load function
source("D:\\R/ModelFittingSummarySWSU.R") # for Mixed Effects Model fitting (step-wise step-up): Binary Logistic Mixed Effects Models
# apply function
mdl.cmp.glmersc.swsu.dm <- mdl.fttng.swsu(mdlcmp)
# inspect output
mdl.cmp.glmersc.swsu.dm

write.table(mdl.cmp.glmersc.swsu.dm, "mdl_cmp_glmersc_swsu_nnsd.txt", sep="\t")
###############################################################
# predict probs of nativelike for effects
nnsd$Prediction <- predict(m12.glmer, nnsd, type="response")
summary(nnsd$Prediction)

###############################################################
pd <- nnsd
pd$Emotionality <- ifelse(pd$Emotionality == "PositiveEmotional", "Positive",
                          ifelse(pd$Emotionality == "NegativeEmotional", "Negative", "Non-emotional"))
p1 <- ggplot(pd, aes(Function, Prediction)) +
  #  facet_wrap(vars(Language)) +
  stat_summary(fun.y = mean, geom = "point", size = 1) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_set(theme_bw(base_size = 12)) +
  theme(legend.position="none") +
  labs(x = "Syntactic function", y = "Predicted probability of \nnon-native-like choices") +
  scale_color_manual(values = c("grey30", "grey30"))
ggsave(file = paste(imageDirectory,"PredFunction.png",sep="/"), 
       height = 5,  width = 3, dpi = 320)
# activate (remove #) to show
p1

p2 <- ggplot(pd, aes(Priming, Prediction)) +
  #  facet_wrap(vars(Language)) +
  stat_summary(fun.y = mean, geom = "point", size = 1) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_set(theme_bw(base_size = 12)) +
  theme(legend.position="none") +
  labs(x = "Priming", y = "Predicted probability of \nnon-native-like choices") +
  scale_color_manual(values = c("grey30", "grey30"))
ggsave(file = paste(imageDirectory,"PredPriming.png",sep="/"), 
       height = 5,  width = 3, dpi = 320)
# activate (remove #) to show
p2

# start plot: all with zero
p3 <- ggplot(pd, aes(x = Frequency, y = Prediction)) +
  #  facet_wrap(vars(Language)) +
  geom_smooth(aes(y = Prediction), color ="darkgrey",  size=.5, se = T) +
  theme_set(theme_bw(base_size = 12)) +
  theme(legend.position="none") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Frequency of adj. type", y = "Predicted probability of \nnon-native-like choices") +
  guides(size = FALSE)+
  guides(alpha = FALSE)
ggsave(file = paste(imageDirectory,"PredFrequency.png",sep="/"), 
       height = 5,  width = 3, dpi = 320)
p3

# start plot: all with zero
p4 <- ggplot(pd, aes(Emotionality, Prediction)) +
  #  facet_wrap(vars(Language)) +
  stat_summary(fun.y = mean, geom = "point", size = 1) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_bw(base_size = 15)) +
  theme(legend.position="none") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Emotionality of Adjective", y = "Predicted probability of \nnon-native-like choices") +
  guides(size = FALSE)+
  guides(alpha = FALSE)
ggsave(file = paste(imageDirectory,"PredEmotionality.png",sep="/"), 
       height = 6,  width = 4, dpi = 320)
p4

# effectfunction_frequency
p5 <- ggplot(pd, aes(x = Frequency, y = Prediction)) +
  geom_smooth(aes(y = Prediction, x = Frequency, color = Function), size=1, se = T) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_set(theme_light(base_size = 15)) +
  #  theme(legend.position="none") +
  labs(x = "Frequency of adj. type", y = "Predicted probability of \nnon-native-like choices") +
  scale_color_manual(values = c("grey30",  "grey50"))   
ggsave(file = paste(imageDirectory,"PredFrequencyFunction.png",sep="/"), 
       height = 5,  width = 5,  dpi = 320)
p5

randomtb <- ranef(m4.glmer)
rndmlng <- as.vector(unlist(randomtb$`Language`))
lng <- c("Bulgarian", "Czech", "Dutch", "Finnish", "Flemish", "French", 
         "German", "Italian", "Polish", "Russian", "Spanish", "Swedish")
rndmlngtb <- data.frame(lng, rndmlng)
colnames(rndmlngtb) <- c("Language", "Intercept")
rndmlngtb <- rndmlngtb[order(rndmlngtb$Intercept, decreasing = T),]

p6 <- ggplot(rndmlngtb, aes(Language, Intercept)) +
  geom_point(aes(reorder(Language, -Intercept, fun = Intercept), y=Intercept)) +
  coord_cartesian(ylim = c(-1.5, 1.5)) +
  theme_set(theme_bw(base_size = 15)) +
  theme(legend.position="none", axis.text.x = element_text(size=15, angle=90)) +
  labs(x = "Language", y = "Adjustment to Intercept")
ggsave(file = paste(imageDirectory,"RanLanguage.png",sep="/"), 
       height = 5,  width = 7,  dpi = 320)
# activate (remove #) to show
p6

rndmadj <- as.vector(unlist(randomtb$`Adjective`))
adj <- c("different", "difficult", "good", "hard", "important", "other")
rndmadjtb <- data.frame(adj, rndmadj)
colnames(rndmadjtb) <- c("Adjective", "Intercept")
rndmadjtb <- rndmadjtb[order(rndmadjtb$Intercept),]
rndmadjtb

p7 <- ggplot(rndmadjtb, aes(Adjective, Intercept)) +
  geom_point(aes(reorder(Adjective, -Intercept, fun = Intercept), y=Intercept)) +
  coord_cartesian(ylim = c(-1.5, 1.5)) +
  theme_set(theme_bw(base_size = 15)) +
  theme(legend.position="none", axis.text.x = element_text(size=15, angle=90)) +
  labs(x = "Adjective type", y = "Adjustment to Intercept")
ggsave(file = paste(imageDirectory,"RanAdjective.png",sep="/"), 
       height = 5,  width = 5,  dpi = 320)
# activate (remove #) to show
p7

# effectfunction_frequency
pd$RelativeFrequency <- sqrt(pd$Frequency)
p8 <- ggplot(pd, aes(x = Frequency, y = Prediction)) +
  geom_smooth(aes(y = Prediction, x = Frequency, color = Function), size=1, se = T) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_set(theme_light(base_size = 15)) +
  #  theme(legend.position="none") +
  labs(x = "Frequency of adj. type", y = "Predicted probability of \nnon-native-like choices") +
  scale_color_manual(values = c("grey30",  "grey50"))   
ggsave(file = paste(imageDirectory,"PredRelFrequencyFunction.png",sep="/"), 
       height = 5,  width = 5,  dpi = 320)
p8

# summary tables
tb1 <- ampicle %>% 
  dplyr::group_by(Adjective) %>%
  dplyr::summarise(Freq = n())
tb2 <- ampicle %>% 
  dplyr::group_by(Adjective, Variant) %>%
  dplyr::summarise(FreqV = n()) %>%
  dplyr::filter(Variant == 1)
tb3 <-data.frame(tb1, tb2)
tb3 <- tb3 %>%
  dplyr::select(-Variant)%>%
  dplyr::mutate(S=Freq) %>%
  dplyr::mutate(P=round(FreqV/(Freq+FreqV)*100, 1))
tb3

###############################################################
# evalute random forests
library(languageR)
#library(Design)
library(party)
# set controls
data.controls <- cforest_unbiased(ntree=1000, mtry=2)
# NSD
# set seed
set.seed(201905103)
# create random forest
data.cforest <- cforest(Variant ~ ., data = nsd, controls=data.controls)
# extract importance
data.cforest.varimp <- varimp(data.cforest, conditional = TRUE)
# inspect importance
data.cforest.varimp
# plot importance
dotplot(sort(data.cforest.varimp))
# new plot
dotplot(sort(data.cforest.varimp), 
        xlab="Variable Importance in the native speaker data\n(predictors to right of dotted vertical line are significant)", 
        panel = function(x,y){ panel.dotplot(x, y, col="black", pch=20, cex=1.1) 
          panel.abline(v=abs(min(data.cforest.varimp)), col="grey", lty="dotted", lwd=2) } )
data.trp <- treeresponse(data.cforest)
data.predforest <- lapply(data.trp, function(x) {
  x <- as.vector(unlist(x))
  x <- ifelse(max(x) == x[1], "completely",
              ifelse(max(x) == x[2], "extremely", 
                     ifelse(max(x) == x[3], "other", 
                            ifelse(max(x) == x[4], "really", 
                                   ifelse(max(x) == x[5], "so", "very")))))
  })
ampred <- as.vector(unlist(data.predforest))

test <- as.character(nsd$Variant) 
somers2(ampred, test) 
#          C         Dxy           n     Missing 
#  0.2222890  -0.5554219 303.0000000   0.0000000 
###############################################################
###              END PART 5
###############################################################
