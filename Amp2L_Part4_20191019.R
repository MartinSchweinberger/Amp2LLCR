##################################################################
# Titel:      How Corpus-Based Methods Can Support Language Learning 
#             and Teaching: An Analysis of Amplifier Use by L1-Speakers 
#             and Learners of English - Part 4
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
#             of Amplifier Use by L1-Speakers and Learners of English - Part 4",
#             unpublished R script, The University of Queensland.
###############################################################
rm(list=ls(all=T))                                      # clean current workspace
setwd("D:\\Uni\\Projekte\\02-Intensification\\Amp2L")   # set wd
# load libraries
library(dplyr)         # activate dplyr library for data processing
library(tidyr)         # activate tidyr library for data processing
library(ggplot2)       # activate ggplot2 library for plotting
library(cowplot)       # activate cowplot library for modifying ggplot2
library(randomForest)  # activate randomForest library for random forests
# set options
options(stringsAsFactors = F)
options(scipen = 999)
options(max.print=10000)
# define image directory
imageDirectory<-"IJLCR\\images"
###############################################################
# load data
ampicle <- read.table("ampicle_statz05.txt", sep = "\t", header = T)
# inspect data
nrow(ampicle); str(ampicle); head(ampicle)

# reduce number of levels for Adjective
adjtb <- ampicle %>%
  dplyr::select(Adjective) %>%
  dplyr::group_by(Adjective) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::arrange(desc(Frequency))
adjtb[1:20,]

ntfrqadj <- names(table(ampicle$Adjective))[which(table(ampicle$Adjective) <= 50)]
ampicle$Adjective <- ifelse(ampicle$Adjective %in% ntfrqadj, "other", ampicle$Adjective)
# reduce number of levels for Variant
vnttb <- ampicle %>%
  dplyr::select(Variant) %>%
  dplyr::group_by(Variant) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::arrange(desc(Frequency))
vnttb[1:20,]

ntfrqvnt <- names(table(ampicle$Variant))[which(table(ampicle$Variant) <= 5)]
ampicle$Variant <- ifelse(ampicle$Variant %in% ntfrqvnt, "other", ampicle$Variant)

# clean colnames
ampicle <- ampicle %>%
  dplyr::rename(Frequency = Freq, Gradability = Gradabilty) %>%
  dplyr::select(-File, - Variant, -AdjectiveRedux, -so, -really, -extremely, -completely, -other)
# factorize variables
clfct <- c("Language", "Adjective", "Function", 
           "Priming", "Gradability", "SemanticCategory", 
           "Emotionality", "very")
ampicle[clfct] <- lapply(ampicle[clfct], factor)

# inspect data
str(ampicle)

###############################################################
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
set.seed(20191019)
nsrf <- randomForest(very ~ ., data=nsd, ntree=3000, proximity=TRUE)
nsrf 

plot(nsrf) # indicates that we do  not need more than 1000 trees

# set seed
set.seed(20191019)
nsrf <- randomForest(very ~ ., data=nsd, ntree=1000, proximity=TRUE)
nsrf 

# plot new precision/error rate
oob.error.data <- data.frame(
  Trees=rep(1:nrow(nsrf$err.rate), times=2),
  Type=rep(c("OOB", "very"), each=nrow(nsrf$err.rate)),
  Error=c(nsrf$err.rate[,"OOB"],
          nsrf$err.rate[,"1"]))
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
head(ptrain); head(train$very)         # inspect predictions

confusionMatrix(ptrain, train$very)

ptest <- predict(nsrf, test)
confusionMatrix(ptest, test$very)

png("IJLCR\\images/VarImpRF1.png",  width = 680, height = 480) # save plot
varImpPlot(nsrf, main = "", pch = 20, cex = 2) 
dev.off()

pnsrf <- predict(nsrf, nsd)
confusionMatrix(pnsrf, nsd$very)

0.7263/0.5263 # 1.380011


###########################################################################
#           RANDOM FOREST: NON-NATIVE-SPEAKERS
nnsd <- ampicle[ampicle$Language != "English",]
head(nnsd); str(nnsd)

pnns <- predict(nsrf, nnsd)       # extract prediction for training data
head(pnns); head(nnsd$very)       # inspect predictions
confusionMatrix(pnns, nnsd$very)

0.6213/0.555 # 1.119459

# add native choice prediction to data
nnsd$NativeChoice <- as.vector(pnns)
nnsd$NativeChoice <- as.factor(nnsd$NativeChoice)
# code if choice of nns is nativelike or not
nnsd$very <- as.character(nnsd$very)
nnsd$NativeChoice <- as.character(nnsd$NativeChoice)
nnsd$NonNativeLike <- ifelse(nnsd$very == nnsd$NativeChoice, 0, 1)
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
m0d.glmer = glmer(NonNativeLike ~ (1|Language/Adjective), data = nnsd, family = binomial) 
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
anova(m1.glmer, m0.glmer, test = "Chi") # not sig! (p=0.5795)

# add Priming
ifelse(min(ftable(nnsd$Priming, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m2.glm <- update(m0.glm, .~.+Priming)
m2.glmer <- update(m0.glmer, .~.+Priming)
anova(m2.glmer, m0.glmer, test = "Chi") # SIG! (p=0.00000000000000022 ***)
Anova(m2.glmer, type = "III", test = "Chi")

# add Frequency
m3.glm <- update(m2.glm, .~.+Frequency)
ifelse(max(vif(m3.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m3.glmer <- update(m2.glmer, .~.+Frequency)
anova(m3.glmer, m2.glmer, test = "Chi") # SIG! (p=0.0000232 ***)
Anova(m3.glmer, type = "III", test = "Chi")

# add Gradability
ifelse(min(ftable(nnsd$Gradability, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m4.glm <- update(m3.glm, .~.+Gradability)
ifelse(max(vif(m4.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m4.glmer <- update(m3.glmer, .~.+Gradability)
anova(m4.glmer, m3.glmer, test = "Chi") # SIG! (p=0.03963*)
Anova(m4.glmer, type = "III", test = "Chi")

# add SemanticCategory
ifelse(min(ftable(nnsd$SemanticCategory, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m5.glm <- update(m4.glm, .~.+SemanticCategory)
ifelse(max(vif(m5.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Emotionality
ifelse(min(ftable(nnsd$Emotionality, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m6.glm <- update(m4.glm, .~.+Emotionality)
ifelse(max(vif(m6.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m6.glmer <- update(m4.glmer, .~.+Emotionality)
anova(m6.glmer, m4.glmer, test = "Chi") # SIG! (p=0.00009482***)
Anova(m6.glmer, type = "III", test = "Chi")

###########################################################################
# find all 2-way interactions
library(utils)
colnames(nnsd)
# define variables included in interactions
vars <- c("Function", "Priming", "Frequency", "Gradability",
          "SemanticCategory", "Emotionality")
intac <- t(combn(vars, 2))
intac

# add Function*Priming
ifelse(min(ftable(nnsd$Function, nnsd$Priming, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m7.glm <- update(m6.glm, .~.+Function*Priming)
ifelse(max(vif(m7.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Function*Frequency
m8.glm <- update(m6.glm, .~.+Function*Frequency)
ifelse(max(vif(m8.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m8.glmer <- update(m6.glmer, .~.+Function*Frequency)
anova(m8.glmer, m6.glmer, test = "Chi") # not sig (p=0.8559)

# add Function*Gradability
ifelse(min(ftable(nnsd$Function, nnsd$Gradability, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m9.glm <- update(m6.glm, .~.+Function*Gradability)
ifelse(max(vif(m9.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Function*SemanticCategory
ifelse(min(ftable(nnsd$Function, nnsd$SemanticCategory, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m10.glm <- update(m6.glm, .~.+Function*SemanticCategory)
ifelse(max(vif(m10.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Function*Emotionality
ifelse(min(ftable(nnsd$Function, nnsd$Emotionality, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m11.glm <- update(m6.glm, .~.+Function*Emotionality)
ifelse(max(vif(m11.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Priming*Frequency
m12.glm <- update(m6.glm, .~.+Priming*Frequency)
ifelse(max(vif(m12.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs ok
m12.glmer <- update(m6.glmer, .~.+Priming*Frequency)
anova(m12.glmer, m6.glmer, test = "Chi") # not sig (p=0.8158)

# add Priming*Gradability
ifelse(min(ftable(nnsd$Priming, nnsd$Gradability, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m13.glm <- update(m6.glm, .~.+Priming*Gradability)
ifelse(max(vif(m13.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Priming*SemanticCategory
ifelse(min(ftable(nnsd$Priming, nnsd$SemanticCategory, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m14.glm <- update(m6.glm, .~.+Priming*SemanticCategory)
ifelse(max(vif(m14.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Priming*Emotionality
ifelse(min(ftable(nnsd$Priming, nnsd$Emotionality, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m15.glm <- update(m6.glm, .~.+Priming*Emotionality)
ifelse(max(vif(m15.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Frequency*Gradability
m16.glm <- update(m6.glm, .~.+Frequency*Gradability)
ifelse(max(vif(m16.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Frequency*SemanticCategory
m17.glm <- update(m6.glm, .~.+Frequency*SemanticCategory)
ifelse(max(vif(m17.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Frequency*Emotionality
m18.glm <- update(m6.glm, .~.+Frequency*Emotionality)
ifelse(max(vif(m18.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add Gradability*SemanticCategory
ifelse(min(ftable(nnsd$Gradability, nnsd$SemanticCategory, nnsd$NonNativeLike)) == 0, "not possible", "possible")

# add Gradability*Emotionality
ifelse(min(ftable(nnsd$Gradability, nnsd$Emotionality, nnsd$NonNativeLike)) == 0, "not possible", "possible")
m19.glm <- update(m6.glm, .~.+Gradability*Emotionality)
ifelse(max(vif(m19.glm)) <= 3,  "vifs ok", "vifs unacceptable") # VIFs unacceptable

# add SemanticCategory*Emotionality
ifelse(min(ftable(nnsd$SemanticCategory, nnsd$Emotionality, nnsd$NonNativeLike)) == 0, "not possible", "possible")

#########################################
# load function for regression table summary
source("D:\\R/meblr.summary.tworandom.R")
# set up summary table
meblrm_nnsd <- meblrm.summary(m0.glm, m6.glm, m0.glmer, m6.glmer, nnsd$NonNativeLike) #
meblrm_nnsd

# save results to disc
write.table(meblrm_nnsd, "IJLRC\\meblrm_nnsd.txt", sep="\t")

# load function
library(car)
meblrm_nnsd_Anova <- Anova(m6.glmer, type = "III", test = "Chi")
meblrm_nnsd_Anova

# save results to disc
write.table(meblrm_nnsd_Anova, "IJLRC\\meblrm_nnsd_Anova.txt", sep="\t")

effectpriming <- anova(m2.glmer, m0.glmer, test = "Chi")

effectfrequency <- anova(m3.glmer, m2.glmer, test = "Chi")

effectgradability <- anova(m4.glmer, m3.glmer, test = "Chi")

effectemotionality <- anova(m6.glmer, m4.glmer, test = "Chi")

# use customized model comparison function
# create comparisons
m1.m0 <- anova(m1.glmer, m0.glmer, test = "Chi") # not sig! (p=0.5795)
m2.m0 <- anova(m2.glmer, m0.glmer, test = "Chi") # SIG! (p=0.00000000000000022 ***)
m3.m2 <- anova(m3.glmer, m2.glmer, test = "Chi") # SIG! (p=0.0000232 ***)
m4.m3 <- anova(m4.glmer, m3.glmer, test = "Chi") # SIG! (p=0.03963*)
m6.m4 <- anova(m6.glmer, m4.glmer, test = "Chi") # SIG! (p=0.00009482***)
m8.m6 <- anova(m8.glmer, m6.glmer, test = "Chi") # not sig (p=0.8559)
m12.m6 <- anova(m12.glmer, m6.glmer, test = "Chi") # not sig (p=0.8158)

# create a list of the model compariNonNativeLikens
mdlcmp <- list(m1.m0, m2.m0, m3.m2, m4.m3, m6.m4, m8.m6, m12.m6)
# load function
source("D:\\R/ModelFittingSummarySWSU.R") # for Mixed Effects Model fitting (step-wise step-up): Binary Logistic Mixed Effects Models
# apply function
mdl.cmp.glmersc.swsu.dm <- mdl.fttng.swsu(mdlcmp)
# inspect output
mdl.cmp.glmersc.swsu.dm

write.table(mdl.cmp.glmersc.swsu.dm, "IJLRC\\mdl_cmp_glmersc_swsu_nnsd.txt", sep="\t")
###############################################################
# predict probs of nativelike for effects
nnsd$Prediction <- predict(m6.glmer, nnsd, type="response")
summary(nnsd$Prediction)

###############################################################
pd <- nnsd
pd$Emotionality <- ifelse(pd$Emotionality == "PositiveEmotional", "Positive",
                          ifelse(pd$Emotionality == "NegativeEmotional", "Negative", "Non-emotional"))

p1 <- ggplot(pd, aes(Priming, Prediction)) +
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
p1

# start plot: all with zero
p2 <- ggplot(pd, aes(x = Frequency, y = Prediction)) +
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
p2

# start plot: all with zero
p3 <- ggplot(pd, aes(Gradability, Prediction)) +
  #  facet_wrap(vars(Language)) +
  stat_summary(fun.y = mean, geom = "point", size = 1) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1) +
  theme_set(theme_bw(base_size = 15)) +
  theme(legend.position="none") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Gradability of Adjective", y = "Predicted probability of \nnon-native-like choices") +
  guides(size = FALSE)+
  guides(alpha = FALSE)
ggsave(file = paste(imageDirectory,"PredGradability.png",sep="/"), 
       height = 6,  width = 4, dpi = 320)
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

randomtb <- ranef(m6.glmer)
rndmlng <- as.vector(unlist(randomtb$`Language`))
lng <- c("Bulgarian", "Czech", "Dutch", "Finnish", "Flemish", "French", 
         "German", "Italian", "Polish", "Russian", "Spanish", "Swedish")
rndmlngtb <- data.frame(lng, rndmlng)
colnames(rndmlngtb) <- c("Language", "Intercept")
rndmlngtb <- rndmlngtb[order(rndmlngtb$Intercept, decreasing = T),]

p5 <- ggplot(rndmlngtb, aes(Language, Intercept)) +
  geom_point(aes(reorder(Language, -Intercept, fun = Intercept), y=Intercept)) +
  coord_cartesian(ylim = c(-1.5, 1.5)) +
  theme_set(theme_bw(base_size = 15)) +
  theme(legend.position="none", axis.text.x = element_text(size=15, angle=90)) +
  labs(x = "Language", y = "Adjustment to Intercept")
ggsave(file = paste(imageDirectory,"RanLanguage.png",sep="/"), 
       height = 5,  width = 7,  dpi = 320)
# activate (remove #) to show
p5

rndmadj <- as.vector(unlist(randomtb$`Adjective`))
adj <- c("bad", "common", "dangerous", "different", "difficult", "easy", "expensive", "few", "good", "hard", "high", "important", "interesting", "likely", "little", "long", "necessary", "new", "other", "popular", "serious", "simple", "small", "strong", "true", "useful")
rndmadjtb <- data.frame(adj, rndmadj)
colnames(rndmadjtb) <- c("Adjective", "Intercept")
rndmadjtb <- rndmadjtb[order(rndmadjtb$Intercept),]
rndmadjtb

p6 <- ggplot(rndmadjtb, aes(Adjective, Intercept)) +
  geom_point(aes(reorder(Adjective, -Intercept, fun = Intercept), y=Intercept)) +
  coord_cartesian(ylim = c(-1.5, 1.5)) +
  theme_set(theme_bw(base_size = 15)) +
  theme(legend.position="none", axis.text.x = element_text(size=15, angle=90)) +
  labs(x = "Adjective type", y = "Adjustment to Intercept")
ggsave(file = paste(imageDirectory,"RanAdjective.png",sep="/"), 
       height = 5,  width = 5,  dpi = 320)
# activate (remove #) to show
p6

# summary tables
tb1 <- ampicle %>% 
  dplyr::group_by(Adjective) %>%
  dplyr::summarise(Freq = n())
tb2 <- ampicle %>% 
  dplyr::group_by(Adjective, very) %>%
  dplyr::summarise(FreqV = n()) %>%
  dplyr::filter(very == 1)
tb3 <-data.frame(tb1, tb2)
tb3 <- tb3 %>%
  dplyr::select(-very)%>%
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
set.seed(20191019)
# create random forest
data.cforest <- cforest(very ~ ., data = nsd, controls=data.controls)
# extract importance
data.cforest.varimp <- varimp(data.cforest, conditional = TRUE)
# inspect importance
data.cforest.varimp
# plot importance
dotplot(sort(data.cforest.varimp))
# new plot
dotplot(sort(data.cforest.varimp), 
        xlab="Variable importance in the NS data\n(predictors to right of dotted vertical line are significant)", 
        panel = function(x,y){ panel.dotplot(x, y, col="black", pch=20, cex=1.1) 
          panel.abline(v=abs(min(data.cforest.varimp)), col="grey", lty="dotted", lwd=2) } )
data.trp <- treeresponse(data.cforest)
data.predforest <- lapply(data.trp, function(x) {
  x <- as.vector(unlist(x))
  x <- ifelse(x[1] > x[2], 0, 1)
})
ampred <- as.vector(unlist(data.predforest))

test <- as.character(nsd$very) 
somers2(ampred, test) 
#      C     Dxy   n    Missing 
#    0.702 0.404  475     0 
###############################################################
###              END PART 4
###############################################################
