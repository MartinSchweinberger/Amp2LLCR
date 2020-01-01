##################################################################
# Titel:      How Corpus-Based Methods Can Support Language Learning 
#             and Teaching: An Analysis of Amplifier Use by L1-Speakers 
#             and Learners of English - Part 3
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
#             of Amplifier Use by L1-Speakers and Learners of English - Part 3",
#             unpublished R script, The University of Queensland.
###############################################################
rm(list=ls(all=T))                                      # clean current workspace
setwd("D:\\Uni\\Projekte\\02-Intensification\\Amp2L")   # set wd
library(plyr)                                           # load packages
library(Rling)                                          # load packages
options(stringsAsFactors = F)                           # set options
options(scipen = 999)                                   # set options
options(max.print=10000)                                # set options
imageDirectory<-"images"                                # define image directory
###############################################################
# read in data
ampicle <- read.table("ampicle_clean04.txt", sep = "\t", header=TRUE)
# remove superfluous columns
ampicle$Subfile <- NULL
ampicle$Speaker <- NULL
head(ampicle)  # inspect data

amplocness <- read.table("amplocness_clean04.txt", sep = "\t", header=TRUE, skipNul = T)
head(amplocness)  # inspect data

colnames(ampicle); colnames(amplocness)

# combine data sets
ampicle <- rbind(ampicle, amplocness)
head(ampicle)  # inspect data
str(ampicle)

# recode proficiency to be more iconic (reversed)
ampicle$Proficiency <- max(ampicle$Proficiency)- ampicle$Proficiency

# remove flemish (too many languages)
#nrow(ampicle)
#ampicle <- ampicle[ampicle$Language != "Flemish",]
#nrow(ampicle)
###############################################################
# factorize variables
clfct <- c("Function", "Priming", "Gradabilty", "SemanticCategory", 
           "Emotionality")
ampicle[clfct] <- lapply(ampicle[clfct], factor)
###############################################################
#                 WARNING
#             DATA REDUCTION
# exclude amplifiers that are very dissimilar to main group of amplifiers
nrow(ampicle)

ampicle <- ampicle[!ampicle$Variant == "", ]
ampicle <- ampicle[complete.cases(ampicle),]
nrow(ampicle)

table(ampicle$Variant)[order(table(ampicle$Variant), decreasing = T)]

###############################################################
# prepare data for plotting
# create data frame with relevant variables
pd <- ampicle %>%
  dplyr::select(Language, File, Function, Adjective, Amplified, Variant)
# convert Age column
LanguageLbs <- names(table(pd$Language))
# multiply Amplified * 100 to get percent for Variant
pd$Amplified <- ifelse(pd$Amplified == 1, 100, 0)
# convert Amplified Amplifiedo a numeric variables
pd$Amplified <- as.numeric(pd$Amplified)
famps <- names(table(pd$Variant))[which(table(pd$Variant) > 250)]
# reclassify Adjectives - infreq. Adjectives are collapsed Amplifiedo category other
pd$Variant <- ifelse(pd$Variant  %in% famps, pd$Variant , "other")
# create variables 
pd$other <- ifelse(pd$Variant == "other", 100, 0)
pd$completely <- ifelse(pd$Variant == "completely", 100, 0)
pd$extremely <- ifelse(pd$Variant == "extremely", 100, 0)
pd$really <- ifelse(pd$Variant == "really", 100, 0)
pd$so <- ifelse(pd$Variant == "so", 100, 0)
pd$very <- ifelse(pd$Variant == "very", 100, 0)
pd$zero <- ifelse(pd$Variant == "0", 100, 0)
pd <- pd[complete.cases(pd),]
pd$Language <- factor(pd$Language, levels=c("English", "Bulgarian", "Czech",
                                            "Dutch", "Finnish", "Flemish", "French", 
                                            "German", "Italian", "Polish", "Russian", 
                                            "Spanish", "Swedish"))
# load library
library(ggplot2)
###############################################################
# p0
p0d <- pd
# start plot: Amplified
p0 <- ggplot(p0d, aes(Language, Amplified)) +
  geom_point(aes(reorder(Language, Amplified, function(Amplified) -mean(Amplified)), y=Amplified), size = NA) +
  stat_summary(fun.y = mean, geom = "point", size = 1) +
  stat_summary(fun.y = mean, geom = "line", size = .5) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = .5) +
  theme(legend.position="none", axis.text.x = element_text(size=15, angle=90)) +
  theme_set(theme_bw(base_size = 15)) +
  coord_cartesian(ylim = c(0, 15)) +
  labs(x = "Language", y = "Percent (Amplified Adjectives)") +
  scale_color_manual(values = c("grey30", "grey30"))
ggsave(file = paste(imageDirectory,"AmplifiedLanguage.png",sep="/"),
       height = 5,  width = 7,  dpi = 320)
p0

###############################################################
# p1
p1d <- pd
# start plot: Amplified
p1 <- ggplot(p1d, aes(Language, Amplified)) +
  facet_grid(vars(Function)) +
  geom_point(aes(reorder(Language, Amplified, function(Amplified) -mean(Amplified)), y=Amplified), size = NA) +
  stat_summary(fun.y = mean, geom = "point", size = 1) +
  stat_summary(fun.y = mean, geom = "line", size = .5) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = .5) +
  theme_set(theme_bw(base_size = 10)) +
  coord_cartesian(ylim = c(0, 20)) +
  theme(legend.position="none", axis.text.x = element_text(size=8, angle=90)) +
  labs(x = "Language", y = "Percent (Amplified Adjectives)") +
  scale_color_manual(values = c("grey30", "grey30"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"AmplifiedLanguageFunction.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p1

###############################################################
library(dplyr)
# p2
p2d <- pd
# remove non-amplified instances
p2d <- p2d[p2d$Amplified != 0,]
p2d <- p2d %>%
  dplyr::group_by(Function, Language) %>%
  dplyr::summarise(mean_very = mean(very),
            mean_really = mean(really),
            mean_so = mean(so),
            mean_extremely = mean(extremely),
            mean_completely = mean(completely),
            mean_other = mean(other))
Variant <- c(
  rep("very", nrow(p2d)),
  rep("really", nrow(p2d)),
  rep("so", nrow(p2d)),
  rep("extremely", nrow(p2d)),
  rep("completely", nrow(p2d)),
  rep("other", nrow(p2d))
)
Function <- rep(p2d$Function, 6)
Language <- rep(p2d$Language, 6)
Percentage <- c(p2d$mean_very, p2d$mean_really, p2d$mean_so, p2d$mean_extremely, p2d$mean_completely, p2d$mean_other)
p2d <- data.frame(Function, Language, Variant, Percentage)

# install pacake for extra shapes
library(ggpubr)
# start plot: all
p2 <- ggplot(p2d, aes(Language, Percentage, group=Variant)) +
  facet_grid(vars(Function)) +
  geom_point(aes(y = Percentage, shape=Variant), size=3) +
  guides(shape=guide_legend(override.aes=list(fill=NA))) +
  scale_shape_manual(values=c("c","e","o","r","s","v"))+
  theme_set(theme_bw(base_size = 15)) +
  theme(legend.position="top", axis.text.x = element_text(size=10, angle=90)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Language", y = "Percent of Amplification") +
#  guides(shape = FALSE)+
  guides(alpha = FALSE)
ggsave(file = paste(imageDirectory,"VariantLanguageFunction.png",sep="/"),
       height = 5, width = 7, dpi = 320)
p2

###############################################################
# p2
p3d <- pd %>%
  dplyr::filter(Amplified != 0) %>%
  dplyr::group_by(Language) %>%
  dplyr::summarise(mean_very = mean(very),
                   mean_really = mean(really),
                   mean_so = mean(so),
                   mean_extremely = mean(extremely),
                   mean_completely = mean(completely),
                   mean_other = mean(other))
Variant <- c(
  rep("very", nrow(p3d)),
  rep("really", nrow(p3d)),
  rep("so", nrow(p3d)),
  rep("extremely", nrow(p3d)),
  rep("completely", nrow(p3d)),
  rep("other", nrow(p3d))
)
Language <- rep(p3d$Language, 6)
Percentage <- c(p3d$mean_very, p3d$mean_really, p3d$mean_so, p3d$mean_extremely, p3d$mean_completely, p3d$mean_other)
p3d <- data.frame(Language, Variant, Percentage)


# start plot: all
p3 <- ggplot(p3d, aes(Language, Percentage, group=Variant)) +
  geom_point(aes(y = Percentage, shape=Variant), size=3) +
  guides(shape=guide_legend(override.aes=list(fill=NA))) +
  scale_shape_manual(values=c("c","e","o","r","s","v"))+
  theme_set(theme_bw(base_size = 15)) +
  theme(legend.position="top", axis.text.x = element_text(size=10, angle=90)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Language", y = "Percent of Amplification") +
  #  guides(shape = FALSE)+
  guides(alpha = FALSE)
ggsave(file = paste(imageDirectory,"VariantLanguage.png",sep="/"),
       height = 5, width = 7, dpi = 320)
p3


###############################################################
#            WARNING: DATA REDUCTION
###############################################################
tbd <- ampicle
# recode adjectives
ntfrqadj <- names(table(tbd$Adjective))[which(table(tbd$Adjective) <= 2000)]
tbd$AdjectiveRedux <- ifelse(tbd$Adjective %in% ntfrqadj, "other", tbd$Adjective)
###############################################################
###             TABULARIZATION
###############################################################
# tb1
tb1 <- tbd %>%
  dplyr::group_by(Variant) %>%
  dplyr::summarize(
    Tokens = n()
    ) %>%
  mutate(Percent = round(Tokens/sum(Tokens)*100, 2)) %>%
  mutate(PercentAmplifiers = c("", round(Tokens[2:length(Tokens)]/sum(Tokens[2:length(Tokens)])*100, 2)))
tb1 <- tb1[order(tb1$Percent, decreasing = T),]
tb1

# save data to disc
write.table(tb1, "Table1.txt", sep = "\t", row.names = F)
###############################################################
# tb2
tb2 <- select(tbd, Variant, Language)
frqamp <- names(table(tb2$Variant))[which(table(tb2$Variant) > 200)]
tb2$Variant <- ifelse(tb2$Variant %in% frqamp, tb2$Variant, "other")
#tb2 <- ftable(tb2$Variant, tb2$Language)
tb2 <- tb2 %>%
  dplyr::group_by(Language, Variant) %>%
  dplyr::summarize(Tokens = n()) %>%
  mutate(Percent = round(Tokens/sum(Tokens)*100, 2))
head(tb2)

# save data to disc
write.table(tb2, "Table2.txt", sep = "\t", row.names = F)
###############################################################
# tb3
tb3 <- tbd %>%
  dplyr::filter(Variant != "0") %>%
  dplyr::group_by(AdjectiveRedux, Language) %>%
  dplyr::mutate(
    completely = Variant == "completely",
    extremely = Variant == "extremely",
    other = Variant == "other",    
    really = Variant == "really",
    so = Variant == "so",
    very = Variant == "very") %>%
  dplyr::summarise(completely = sum(completely),
            extremely = sum(extremely),
            other = sum(other),
            really = sum(really),
            so = sum(so),
            very = sum(very))
tb3

# save data to disc
write.table(tb3, "Table3.txt", sep = "\t", row.names = T)
###############################################################
# tb4
tb4 <- tbd %>%
  dplyr::filter(Variant != "0") %>%
  dplyr::group_by(AdjectiveRedux, Language) %>%
  dplyr::mutate(
    completely = Variant == "completely",
    extremely = Variant == "extremely",
    other = Variant == "other",    
    really = Variant == "really",
    so = Variant == "so",
    very = Variant == "very") %>%
  dplyr::summarise(completely = sum(completely),
            extremely = sum(extremely),
            other = sum(other),
            really = sum(really),
            so = sum(so),
            very = sum(very))
tb4$RowTotal <- rowSums(tb4[,c(3:ncol(tb4))])
tb4 <- tb4 %>%
  dplyr::mutate(
    completelypercent = round(completely/RowTotal*100, 2),    
    extremelypercent = round(extremely/RowTotal*100, 2),
    otherpercent = round(other/RowTotal*100, 2),
    reallypercent = round(really/RowTotal*100, 2),
    sopercent = round(so/RowTotal*100, 2),
    verypercent = round(very/RowTotal*100, 2))
tb4 <- tb4 %>%
  dplyr::select(AdjectiveRedux, Language, completelypercent, extremelypercent, 
         otherpercent, reallypercent, sopercent, verypercent)
tb4

# save data to disc
write.table(tb4, "Table4.txt",  sep = "\t", row.names = T)
###############################################################
tb5 <- tbd[tbd$Variant != "0",]
tb5 <- select(tb5, Variant, Language)
frqamp <- names(table(tb5$Variant))[which(table(tb5$Variant) > 200)]
tb5$Variant <- ifelse(tb5$Variant %in% frqamp, tb5$Variant, "other")
tb5 <- ftable(tb5)
tb5

png("images/mosaic.png",  width = 400, height = 800) 
mosaicplot(tb5)
dev.off()
png("images/assocplot.png",  width = 500, height = 2000) 
assocplot(as.matrix(tb5))
dev.off()

###############################################################
p6d <- ampicle %>%
  select(Language, Adjective, Variant)
frqamp <- names(table(p6d$Variant))[which(table(p6d$Variant) > 200)]
p6d$Variant <- ifelse(p6d$Variant %in% frqamp, p6d$Variant, "other")

p6d_extb <- p6d %>%
  filter(Language == "Flemish" | Language == "Finnish" | Language == "English") %>%
  filter(Variant == "very") %>%
  dplyr::group_by(Language) %>%
  dplyr::summarize(
    VariantFrequency = n(),
    AdjectiveFrequency = length(names(table(Adjective))),
    LexicalDiversity = round(AdjectiveFrequency/VariantFrequency, 2)
  )
head(p6d_extb)

###############################################################
# recode variant 
tb11d <- ampicle %>%
  select(Language, Adjective, Variant)
vld <- c("completely", "really", "so", "very", "0")
tb11d$Variant <- ifelse(tb11d$Variant %in% vld, tb11d$Variant, "other")
# tabulate
tb11 <- tb11d %>%
  dplyr::group_by(Language, Variant) %>%
  dplyr::summarise(
    VariantFrequency = n(),
    AdjectiveFrequency = length(names(table(Adjective))),
    LexicalDiversity = round(AdjectiveFrequency/VariantFrequency, 2)
  )
tb11

###########################################################################
#                  CHANGES IN Adjective FREQ
library(tidyr)
tb12 <- ampicle[ampicle$Variant != "0",] 
frqadj <- names(table(tb12$Adjective))[which(table(tb12$Adjective) > 100)]
tb12$Adjective <- ifelse(tb12$Adjective %in% frqadj, tb12$Adjective, "other")
tb12$Language <- factor(tb12$Language, 
                        levels=c("English", "Bulgarian", "Czech", "Dutch", "Finnish", 
                                 "Flemish", "French", "German", "Italian", "Polish", 
                                 "Russian", "Spanish", "Swedish"))
tb12 <- tb12%>%
  dplyr::select(Language, Adjective, Variant) %>%
  dplyr::filter(Variant != "0") %>%
  dplyr::group_by(Language) %>%
  dplyr::count(Adjective) %>%
  tidyr::spread(Language, n, fill = 0, convert = FALSE) %>%
  dplyr::select(Adjective, everything())
tb12 <- as.data.frame(tb12)
rownames(tb12) <- tb12$Adjective
tb12$Adjective <- NULL
tb12 <- apply(tb12, 2, function(x) {x <- round(x/sum(x)*100, 1)})
tb12
  
# save data to disc
write.table(tb12, "Table5.txt", sep = "\t", row.names = F)
###########################################################################
# start plot: Adjective
p11d <- as.data.frame(t(tb12))
p11d$Language <- rownames(p11d)
p11d$Language <- factor(p11d$Language, levels=c("English", "Bulgarian", "Czech",
                                                  "Dutch", "Finnish", "Flemish", "French", 
                                                  "German", "Italian", "Polish", "Russian", 
                                                  "Spanish", "Swedish"))
p11 <- ggplot(p11d, aes(x = Language, y = other), size = 8) +
  geom_point(aes(y = other, color = "other"), size=1) +
  geom_point(aes(y = good, color = "good"), size=1) +
  geom_point(aes(y = different, color = "different"), size=1) +
  geom_point(aes(y = difficult, color = "difficult"), size=1) +
  geom_point(aes(y = hard, color = "hard"), size=1) +
  geom_point(aes(y = important, color = "important"), size=1) +
  geom_point(aes(y = little, color = "little"), size=1) +
  geom_point(aes(y = strong, color = "strong"), size=1) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
scale_colour_manual(values=c("grey30", "grey60", "goldenrod2",  "indianred4", "grey30", "goldenrod2", "grey60", "blue"),
                      name="", 
                      breaks=c("other", "good", "different", "difficult", "hard", "important", "little", "strong"), 
                      labels = c("other", "good", "different", "difficult", "hard", "important", "little", "strong")) +
  theme_set(theme_light(base_size = 8)) +
  theme(legend.position="top", axis.text.x = element_text(size=8, angle=90)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Language", y = "Percent of Adjectives") +
ggsave(file = paste(imageDirectory,"AdjectivefreqLanguage.png", sep="/"), width = 15,  height = 7.5, units = c("cm"),  dpi = 320)
p11

###############################################################
# tb6
# reorder data
tb6 <- ampicle[ampicle$Variant != "0",]
frqamp <- names(table(tb6$Variant))[which(table(tb6$Variant) > 200)]
tb6$Variant <- ifelse(tb6$Variant %in% frqamp, tb6$Variant, "other")
tb6 <- tb6 %>%
  dplyr::group_by(Language) %>%
  dplyr::mutate(
    very = Variant == "very",
    so = Variant == "so",
    really = Variant == "really",
    completely = Variant == "completely",
    extremely = Variant == "extremely",
    other = Variant == "other") %>%
  dplyr::summarise(very = sum(very),
            really = sum(really),
            so = sum(so),
            extremely = sum(extremely),
            completely = sum(completely),
            other = sum(other))
tb6$RowTotal <- rowSums(tb6[,c(2:ncol(tb6))])
tb6 <- tb6 %>%
  dplyr::mutate(
    verypercent = round(very/RowTotal*100, 1),
    sopercent = round(so/RowTotal*100, 1),
    reallypercent = round(really/RowTotal*100, 1),
    completelypercent = round(completely/RowTotal*100, 1),
    extremelypercent = round(extremely/RowTotal*100, 1),
    otherpercent = round(other/RowTotal*100, 1))
tb6 <- tb6 %>%
  select(Language, very, verypercent, so, sopercent, really, reallypercent, 
         extremely, extremelypercent, completely, completelypercent, other, otherpercent)
tb6

# save data to disc
write.table(tb6, "Table6.txt",  sep = "\t", row.names = T)
###########################################################################
#                  REGRESSION DATA SET
###########################################################################
# only amplified instances
veryicle <- ampicle[ampicle$Amplified == 1,]
# inspect data
str(veryicle); colnames(veryicle)

# remove superfluous columns
veryicle$ID <- NULL
veryicle$PreContext <- NULL
veryicle$PostContext <- NULL
veryicle$PreContextLong <- NULL
veryicle$Amplified <- NULL
###############################################################
#            WARNING: DATA REDUCTION
###############################################################
# recode adjectives
ntfrqadj <- names(table(veryicle$Adjective))[which(table(veryicle$Adjective) <= 100)]
veryicle$AdjectiveRedux <- ifelse(veryicle$Adjective %in% ntfrqadj, "other", veryicle$Adjective)
###############################################################
#veryicle$completely <- NULL
#veryicle$Variant <- NULL 
# define vector for data inspection
cltb <- c("Language", "File", "AdjectiveRedux", "Emotionality", "Function", "Priming", 
          "very", "very", "Gradabilty", "SemanticCategory")
# tabulate data
lapply(veryicle[cltb], table)

# check data
#veryicle[which(veryicle$SemanticCategory == "NoSemType"),]

# recategorzie Priming
veryicle$Priming <- ifelse(veryicle$Priming == "noprime", "NoPrime",
                           ifelse(veryicle$Priming == "prime", "Prime", veryicle$Priming))
# recategorzie SemanticCategory
veryicle$SemanticCategory <- as.character(veryicle$SemanticCategory)
table(veryicle$SemanticCategory)

veryicle$SemanticCategory <- ifelse(veryicle$SemanticCategory == "Age", "NoSemType",
                                    ifelse(veryicle$SemanticCategory == "Color", "NoSemType", veryicle$SemanticCategory))
# define vector for data inspection
cltb <- c("Language", "Adjective", "Emotionality",  "Function", "Priming",  
          "very", "Gradabilty", "SemanticCategory")
# tabulate data
lapply(veryicle[cltb], table)

# inspect data
nrow(veryicle); str(veryicle); colnames(veryicle)

###############################################################
write.table(veryicle, "ampicle_statz05.txt", row.names= F, sep = "\t")
###############################################################
#                   END PART 3
###############################################################
