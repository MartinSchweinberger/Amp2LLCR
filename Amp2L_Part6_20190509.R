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
# set seed
set.seed(20190507)
# define image directory
imageDirectory<-"images"
###############################################################
# load data
ampicle <- read.table("ampicle_statz05.txt", sep = "\t", header = T)
# inspect data
nrow(ampicle); str(ampicle); head(ampicle)

colnames(ampicle) <- ifelse(colnames(ampicle) == "Gradabilty", "Gradability",
                            ifelse(colnames(ampicle) == "Freq", "Frequency",colnames(ampicle)))
# tabulating
tb5 <- table(ampicle$Language, ampicle$Adjective)
tb5

tb6 <- apply(tb5, 2, function(x){
  ifelse(x < 3, NA, x)
})
tb6 <- as.data.frame(t(tb6))
tb6 <- tb6[complete.cases(tb6),]
ampicle$Adjective <- ifelse(ampicle$Adjective %in% rownames(tb6), ampicle$Adjective, "other")
tb7 <- table(ampicle$Language, ampicle$Adjective)
tb7

# factorize variables
clfct <- c("completely", "Language", "File", "Adjective", "Function", "Priming", 
           "SemanticCategory", "Gradability", "Emotionality")
ampicle[clfct] <- lapply(ampicle[clfct], factor)

# extract native-speaker data
ampicle$other <- NULL
ampicle$very <- NULL
ampicle$File <- NULL
PolyFrequency <- poly(ampicle$Frequency, degree = 2, simple = T)
ampicle$PolyFrequency <- PolyFrequency[,2]


###
nsd <- ampicle[ampicle$Language == "English",]
nsd$Language <- NULL
nsd$Proficiency <- NULL
nsd$Frequency <- nsd$PolyFrequency
nsd$PolyFrequency <- NULL
head(nsd); str(nsd)

###########################################################################
#           RANDOM FOREST: NATIVE-SPEAKRES
nsrf <- randomForest(completely ~ ., data=nsd, ntree=3000, proximity=TRUE)
nsrf 

# plot precision/error rate
oob.error.data <- data.frame(
  Trees=rep(1:nrow(nsrf$err.rate), times=3),
  Type=rep(c("OOB", "1", "0"), each=nrow(nsrf$err.rate)),
  Error=c(nsrf$err.rate[,"OOB"],
          nsrf$err.rate[,"1"],
          nsrf$err.rate[,"0"]))
ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))
# determine mtry
oob.values <- vector(length=7)
for(i in 1:7) {
  temp.nsrf <- randomForest(completely ~ ., data=nsd, mtry=i, ntree=3000)
  oob.values[i] <- temp.nsrf$err.rate[nrow(temp.nsrf$err.rate),1]
}
oob.values # mtry 2 has lowest value
nsrf <- randomForest(completely ~ ., data=nsd, ntree=3000, proximity=TRUE, mtry = 2)
nsrf 

# plot new precision/error rate
oob.error.data <- data.frame(
  Trees=rep(1:nrow(nsrf$err.rate), times=3),
  Type=rep(c("OOB", "1", "0"), each=nrow(nsrf$err.rate)),
  Error=c(nsrf$err.rate[,"OOB"],
          nsrf$err.rate[,"1"],
          nsrf$err.rate[,"0"]))
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
head(ptrain); head(train$completely)         # inspect predictions

confusionMatrix(ptrain, train$completely)

ptest <- predict(nsrf, test)
confusionMatrix(ptest, test$completely)

png("images/completely_VarImpRF1.png",  width = 680, height = 480) # save plot
varImpPlot(nsrf, main = "", pch = 20, cex = 2) 
dev.off()

pnsrf <- predict(nsrf, nsd)
confusionMatrix(pnsrf, nsd$completely)

0.7723/0.5742

###########################################################################
#           RANDOM FOREST: NON-NATIVE-SPEAKERS
nnsd <- ampicle[ampicle$Language != "English",]
nnsd$Frequency <- nnsd$PolyFrequency
nnsd$PolyFrequency <- NULL
nnsd$Proficiency <- as.vector(unlist(scale(nnsd$Proficiency)))
head(nnsd); str(nnsd)

pnns <- predict(nsrf, nnsd)       # extract prediction for training data
head(pnns); head(nnsd$completely)       # inspect predictions
confusionMatrix(pnns, nnsd$completely)

3607/(2839 + 3607)

0.6205/0.5595718

# add native choice prediction to data
nnsd$NativeChoice <- as.vector(pnns)
nnsd$NativeChoice <- factor(nnsd$NativeChoice, levels = c(0,1))
# code if choice of nns is nativelike or not
nnsd$NativeLike <- ifelse(nnsd$completely == nnsd$NativeChoice, 1, 0)
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
m0.glm = glm(NativeLike ~ 1, family = binomial, data = nnsd) # baseline model glm
m0.lrm = lrm(NativeLike ~ 1, data = nnsd, x = T, y = T) # baseline model lrm
# inspect results
summary(m0.glm)

m0.lrm

# load library
library(lme4)
# create model with a random intercept for token
m0.lmer <- lmer(NativeLike ~ (1|Adjective), data = nnsd, family = binomial)
# results of the lmer object
print(m0.lmer, corr = F)

# Baayen (2008:278-284) uses the call above but the this call is now longer
# up-to-date because the "family" parameter is deprecated
# we switch to glmer (suggested by R) instead but we will also
# create a lmer object of the final minimal adequate model as some functions
# will not (yet) work on glmer
m0a.glmer = glmer(NativeLike ~ (1|Adjective), data = nnsd, family = binomial)
m0b.glmer = glmer(NativeLike ~ (1|Language), data = nnsd, family = binomial)
m0c.glmer = glmer(NativeLike ~ (1|Adjective)+(1|Language), data = nnsd, family = binomial)
m0d.glmer = glmer(NativeLike ~ (1|Language/Adjective), data = nnsd, family = binomial) # singular fit!
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
m0.glmer = glmer(NativeLike ~ (1|Adjective)+(1|Language), data = nnsd, family = binomial)

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
m0.glmer <- glmer(NativeLike ~ 1+ (1|Adjective)+(1|Language), family = binomial, data = nnsd, 
                  control=glmerControl(optimizer="bobyqa"))
# add Function
ifelse(min(ftable(nnsd$Function, nnsd$NativeLike)) == 0, "not possible", "possible")
m1.glm <- update(m0.glm, .~.+Function)
m1.glmer <- update(m0.glmer, .~.+Function)
anova(m1.glmer, m0.glmer, test = "Chi") # SIG! (p<0.01031 ***)

# add Priming
ifelse(min(ftable(nnsd$Priming, nnsd$NativeLike)) == 0, "not possible", "possible")
m2.glm <- update(m1.glm, .~.+Priming)
ifelse(max(vif(m2.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m2.glmer <- update(m1.glmer, .~.+Priming)
anova(m2.glmer, m1.glmer, test = "Chi") # SIG! (p=0.000004205 ***)

# add Proficiency
m3.glm <- update(m2.glm, .~.+Proficiency)
ifelse(max(vif(m3.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m3.glmer <- update(m2.glmer, .~.+Proficiency)
anova(m3.glmer, m2.glmer, test = "Chi") # not sig (p=0.3417)

# add Frequency
m4.glm <- update(m2.glm, .~.+Frequency)
ifelse(max(vif(m4.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m4.glmer <- update(m2.glmer, .~.+Frequency)
anova(m4.glmer, m2.glmer, test = "Chi") # SIG! (p=0.8642)

# add Gradability
ifelse(min(ftable(nnsd$Gradability, nnsd$NativeLike)) == 0, "not possible", "possible")
m5.glm <- update(m2.glm, .~.+Gradability)
ifelse(max(vif(m5.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add SemanticCategory
ifelse(min(ftable(nnsd$SemanticCategory, nnsd$NativeLike)) == 0, "not possible", "possible")
m6.glm <- update(m2.glm, .~.+SemanticCategory)
ifelse(max(vif(m6.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Emotionality
ifelse(min(ftable(nnsd$Emotionality, nnsd$NativeLike)) == 0, "not possible", "possible")
m7.glm <- update(m2.glm, .~.+Emotionality)
ifelse(max(vif(m7.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m7.glmer <- update(m2.glmer, .~.+Emotionality)
anova(m7.glmer, m2.glmer, test = "Chi") # SIG! (p=0.00000000007075 ***)

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
ifelse(min(ftable(nnsd$Function, nnsd$Priming, nnsd$NativeLike)) == 0, "not possible", "possible")
m10.glm <- update(m7.glm, .~.+Function*Priming)
ifelse(max(vif(m10.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Function*Proficiency
m11.glm <- update(m7.glm, .~.+Function*Proficiency)
ifelse(max(vif(m11.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m11.glmer <- update(m7.glmer, .~.+Function*Proficiency)
anova(m11.glmer, m7.glmer, test = "Chi") # not sig! (p=0.3577)

# add Function*Frequency
m12.glm <- update(m7.glm, .~.+Function*Frequency)
ifelse(max(vif(m12.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m12.glmer <- update(m7.glmer, .~.+Function*Frequency)
anova(m12.glmer, m7.glmer, test = "Chi") # SIG! (p=0.3343 *)

# add Function*Gradability
ifelse(min(ftable(nnsd$Function, nnsd$Gradability, nnsd$NativeLike)) == 0, "not possible", "possible")
m13.glm <- update(m7.glm, .~.+Function*Gradability)
ifelse(max(vif(m13.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Function*SemanticCategory
ifelse(min(ftable(nnsd$Function, nnsd$SemanticCategory, nnsd$NativeLike)) == 0, "not possible", "possible")
m14.glm <- update(m7.glm, .~.+Function*SemanticCategory)
ifelse(max(vif(m14.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Function*Emotionality
ifelse(min(ftable(nnsd$Function, nnsd$Emotionality, nnsd$NativeLike)) == 0, "not possible", "possible")
m15.glm <- update(m7.glm, .~.+Function*Emotionality)
ifelse(max(vif(m15.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Priming*Proficiency
m16.glm <- update(m7.glm, .~.+Priming*Proficiency)
ifelse(max(vif(m16.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m16.glmer <- update(m7.glmer, .~.+Priming*Proficiency)
anova(m16.glmer, m7.glmer, test = "Chi") # not sig (p=0.6925)

# add Priming*Frequency
m17.glm <- update(m7.glm, .~.+Priming*Frequency)
ifelse(max(vif(m17.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m17.glmer <- update(m7.glmer, .~.+Priming*Frequency)
anova(m17.glmer, m7.glmer, test = "Chi") # not sig (p=0.5434)

# add Priming*Gradability
ifelse(min(ftable(nnsd$Priming, nnsd$Gradability, nnsd$NativeLike)) == 0, "not possible", "possible")
m18.glm <- update(m7.glm, .~.+Priming*Gradability)
ifelse(max(vif(m18.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Priming*SemanticCategory
ifelse(min(ftable(nnsd$Priming, nnsd$SemanticCategory, nnsd$NativeLike)) == 0, "not possible", "possible")
m19.glm <- update(m7.glm, .~.+Priming*SemanticCategory)
ifelse(max(vif(m19.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Priming*Emotionality
ifelse(min(ftable(nnsd$Priming, nnsd$Emotionality, nnsd$NativeLike)) == 0, "not possible", "possible")
m20.glm <- update(m7.glm, .~.+Priming*Emotionality)
ifelse(max(vif(m20.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable
m20.glmer <- update(m7.glmer, .~.+Priming*Emotionality)
anova(m20.glmer, m7.glmer, test = "Chi") # not sig (p=0.7635)

# add Proficiency*Frequency
m21.glm <- update(m7.glm, .~.+Proficiency*Frequency)
ifelse(max(vif(m20.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable
m21.glmer <- update(m7.glmer, .~.+Proficiency*Frequency)
anova(m21.glmer, m7.glmer, test = "Chi") # not sig (p=0.7144)

# add Proficiency*Gradability
m22.glm <- update(m7.glm, .~.+Proficiency*Gradability)
ifelse(max(vif(m22.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Proficiency*SemanticCategory
m23.glm <- update(m7.glm, .~.+Proficiency*SemanticCategory)
ifelse(max(vif(m23.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Proficiency*Emotionality
m24.glm <- update(m7.glm, .~.+Proficiency*Emotionality)
ifelse(max(vif(m24.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Frequency*Gradability
m25.glm <- update(m7.glm, .~.+Frequency*Gradability)
ifelse(max(vif(m25.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Frequency*SemanticCategory
m26.glm <- update(m7.glm, .~.+Frequency*SemanticCategory)
ifelse(max(vif(m26.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Gradability*SemanticCategory
ifelse(min(ftable(nnsd$Gradability, nnsd$SemanticCategory, nnsd$NativeLike)) == 0, "not possible", "possible")

# add Gradability*Emotionality
ifelse(min(ftable(nnsd$Gradability, nnsd$Emotionality, nnsd$NativeLike)) == 0, "not possible", "possible")
m27.glm <- update(m7.glm, .~.+Gradability*Emotionality)
ifelse(max(vif(m27.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add SemanticCategory*Emotionality
ifelse(min(ftable(nnsd$SemanticCategory, nnsd$Emotionality, nnsd$NativeLike)) == 0, "not possible", "possible")

#########################################
# load function for regression table summary
source("D:\\R/meblr.summary.tworandom.R")
# set up summary table
completely_meblrm_nnsd <- meblrm.summary(m0.glm, m7.glm, m0.glmer, m7.glmer, nnsd$NativeLike) #
completely_meblrm_nnsd

# save results to disc
write.table(meblrm_nnsd, "completely_meblrm_nnsd.txt", sep="\t")

# load function
library(car)
completely_meblrm_nnsd_Anova <- Anova(m7.glmer, type = "III", test = "Chi")
completely_meblrm_nnsd_Anova

# save results to disc
write.table(meblrm_nnsd_Anova, "completely_meblrm_nnsd_Anova.txt", sep="\t")

effectfunction <- anova(m0.glmer, m1.glmer, test = "Chi")

effectpriming <- anova(m2.glmer, m1.glmer, test = "Chi")

effectemotionality <- anova(m2.glmer, m7.glmer, test = "Chi")

# use customized model comparison function
# create compariNativeLikens
m1.m0 <- anova(m1.glmer, m0.glmer, test = "Chi") # SIG! (p<0.01031 ***)
m2.m1 <- anova(m2.glmer, m1.glmer, test = "Chi") # SIG! (p=0.000004205 ***)
m3.m2 <- anova(m3.glmer, m2.glmer, test = "Chi") # not sig (p=0.3417)
m4.m2 <- anova(m4.glmer, m2.glmer, test = "Chi") # SIG! (p=0.8642)
m7.m2 <- anova(m7.glmer, m2.glmer, test = "Chi") # SIG! (p=0.00000000007075 ***)
m11.m7 <- anova(m11.glmer, m7.glmer, test = "Chi") # not sig! (p=0.3577)
m12.m7 <- anova(m12.glmer, m7.glmer, test = "Chi") # SIG! (p=0.3343 *)
m16.m7 <- anova(m16.glmer, m7.glmer, test = "Chi") # not sig (p=0.6925)
m17.m7 <- anova(m17.glmer, m7.glmer, test = "Chi") # not sig (p=0.5434)
m20.m7 <- anova(m20.glmer, m7.glmer, test = "Chi") # not sig (p=0.7635)
m21.m7 <- anova(m21.glmer, m7.glmer, test = "Chi") # not sig (p=0.7144)

# create a list of the model compariNativeLikens
mdlcmp <- list(m1.m0, m2.m1, m3.m2, m4.m2, m7.m2, m11.m7, m12.m7, m16.m7, m17.m7, m20.m7, m21.m7)
# load function
source("D:\\R/ModelFittingSummarySWSU.R") # for Mixed Effects Model fitting (step-wise step-up): Binary Logistic Mixed Effects Models
# apply function
completely_mdl.cmp.glmersc.swsu.dm <- mdl.fttng.swsu(mdlcmp)
# inspect output
completely_mdl.cmp.glmersc.swsu.dm

write.table(completely_mdl.cmp.glmersc.swsu.dm, "completely_mdl_cmp_glmersc_swsu_nnsd.txt", sep="\t")
################################################################
#                 IMPORTANT OBJECTS
################################################################
# inspect NativeLike important objects
head(nnsd)

# glmer
effectfunction

effectpriming

effectemotionality

completely_meblrm_nnsd

completely_meblrm_nnsd_Anova

###############################################################
# predict probs of nativelike for effects
nnsd$Prediction <- predict(m7.glmer, nnsd, type="response")
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
  labs(x = "Syntactic function", y = "Predicted probability of nativelike choices") +
  scale_color_manual(values = c("grey30", "grey30"))
ggsave(file = paste(imageDirectory,"completely_PredFunction.png",sep="/"), 
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
  labs(x = "Priming", y = "Predicted probability of nativelike choices") +
  scale_color_manual(values = c("grey30", "grey30"))
ggsave(file = paste(imageDirectory,"completely_PredPriming.png",sep="/"), 
       height = 5,  width = 3, dpi = 320)
# activate (remove #) to show
p2

# start plot: all with zero
p4 <- ggplot(pd, aes(Emotionality, Prediction)) +
  #  facet_wrap(vars(Language)) +
  stat_summary(fun.y = mean, geom = "point", size = 1) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_bw(base_size = 15)) +
  theme(legend.position="none") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Emotionality of Adjective", y = "Predicted probability of nativelike choices") +
  guides(size = FALSE)+
  guides(alpha = FALSE)
ggsave(file = paste(imageDirectory,"completely_PredEmotionality.png",sep="/"), 
       height = 6,  width = 4, dpi = 320)
p4

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
ggsave(file = paste(imageDirectory,"completely_RanLanguage.png",sep="/"), 
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
  geom_point(aes(reorder(Adjective, Intercept, fun = -Intercept), y=Intercept)) +
  coord_cartesian(ylim = c(-1.5, 1.5)) +
  theme_set(theme_bw(base_size = 15)) +
  theme(legend.position="none", axis.text.x = element_text(size=15, angle=90)) +
  labs(x = "Adjective type", y = "Adjustment to Intercept")
ggsave(file = paste(imageDirectory,"completely_RanAdjective.png",sep="/"), 
       height = 5,  width = 5,  dpi = 320)
# activate (remove #) to show
p7

# summary tables
tb11 <- ampicle %>% 
  dplyr::group_by(Priming, completely) %>%
  dplyr::summarise(Freq = n())
tb11completely <- tb11 %>%
  dplyr::filter(completely == 1) %>%
  dplyr::select(Language, Adjective, Freq)
tb11nocompletely <- tb11 %>%
  dplyr::filter(very == 0) %>%
  dplyr::select(Language, Adjective, Freq)
tb11wide <- data.frame(tb11completely, tb11nocompletely)
tb11wide

###############################################################
# evalute random forests
library(languageR)
#library(Design)
library(party)
# set controls
data.controls <- cforest_unbiased(ntree=1000, mtry=2)
# NSD
# create random forest
data.cforest <- cforest(completely ~ ., data = nsd, controls=data.controls)
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
data.predforest <- sapply(data.trp, FUN = function(v) return(v[1]))
test <- as.character(nsd$completely) 
somers2(data.predforest, test) 
#       C        Dxy          n    Missing 
#0.221398  -0.557204 303.000000   0.000000 
###############################################################
###              END PART 6
###############################################################



