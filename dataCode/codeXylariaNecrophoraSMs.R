#!/usr/bin/env Rscript
#Jose E. Solorzano
# R version 4.1.0 
#https://cran.r-project.org/bin/windows/base/ #update R if needed

#packages
#install.packages("agricolae")
library("agricolae") #analysis
library("car")  #analysis
library("plyr") #data management
library("dplyr") #data management
library("tidyverse") #data management
library("broom")
#install.packages("multcompView") #to see tukey
library("multcompView")
library("stringr") #to get words from vector
library("ggplot2")#plots
library("ggpubr")
#install.packages("ggprism") 
library("ggprism") 
#install.packages("ggpmisc")
library("ggpmisc")
library("cowplot")
library("xlsx") #write .xlsxfiles
library("psych") #to describe by
#install.packages("rcompanion") trasnformation
library("rcompanion")
#install.packages('olsrr') #to check normality
library("olsrr")


#beginning of code
#data
#general working directory
#setwd("~/Google Drive/My Drive/lsuResearch")

#Data and figures
#Infiltration assay
# 4 mm inside (white) 6 mm outside in white background
infiltration.pronase <- read.csv("rcodeandDataMS/dataforR/pronaseheatedinfiltrationassaydonut.csv")
str(infiltration.pronase)
summary(infiltration.pronase)

#Check normality of the data.
#chl means chlorophyll
plotNormalHistogram(infiltration.pronase$chl)
hist(infiltration.pronase$chl)
shapiro.test(infiltration.pronase$chl) #test of normality 
#For additional analyses the infiltration data will be transformed
#Find more information: https://rdrr.io/cran/rcompanion/man/transformTukey.html
#Transformation of the data was not necessary because the chl itself follows a normal distribution

#stats
#to conduct tukey HSD we need to conduct a lm first
#infiltration.pronase.transformed for transformed
chl.pronase.lm <- lm(chl ~ Treatment + as.character(isoRepetition) + plantRep + techrep, data = infiltration.pronase)
summary(chl.pronase.lm)

#ANOVA
chl.pronase.aov <- aov(chl.pronase.lm)
summary(chl.pronase.aov)

chl.pronase.aov.residuals <- residuals(object = chl.pronase.aov)
hist(x = chl.pronase.aov.residuals)
ks.test(chl.pronase.aov.residuals, "pnorm", mean(chl.pronase.aov.residuals), sd(chl.pronase.aov.residuals))
#Post test
#If the p-value is greater than 0.05, it suggests that the error terms are normally distributed. Hence each group was sampled from normally distributed population. 

#Tukey test to obtain the differences (values) among groups - for visual assessment of the differences between groups
chl.pronase.posthoc <- TukeyHSD(chl.pronase.aov)
chl.pronase.posthoc
#cv.chl.pronase.posthoc <- as.data.frame(chl.pronase.posthoc$Treatment) #Make and output a csv file
#posthoc.pronase.df.data.for.summary <- "#SelectDestinationPath/NameFile.csv"
#write.csv(cv.chl.pronase.posthoc, posthoc.pronase.df.data.for.summary)

#Get significance letters
tukey.chl.pronase <- HSD.test(chl.pronase.aov, trt = 'Treatment')
tukey.chl.pronase
#dput(as.character(tukey.chl.pronase.extracts$groups[[2]])) #use this line to get significance letters
#chl.pronase.df.tukey <- as.data.frame(tukey.chl.pronase$groups)
#chl.pronase.df.tukey
#ch.pronase.df.data.for.letters <- "SelectDestinationPath/NameFile1.csv"
#write.csv(chl.pronase.df.tukey, ch.pronase.df.data.for.letters)
#chl.pronase.df.tukeydata <- read.csv("#SelectOriginPath/NameFile1.csv") #read the file containing the significance letters
#as.data.frame(chl.pronase.df.tukeydata)

#chl.pronase.df.tukeydata <- chl.pronase.df.tukeydata[order(chl.pronase.df.tukeydata$X),] #organizing letters
#chl.pronase.df.tukeydata
#dput(as.character(chl.pronase.df.tukeydata$groups)) #now get significance letters as a list

#summary chl content in pronase raw
#plots
head(infiltration.pronase) #see data again for plotting
infiltration.pronase.summary <- infiltration.pronase %>% # the names of the new data frame and the data frame to be summarized
  group_by(Treatment, Legend) %>%   # the grouping variable
  summarise(mean_pronase = mean(chl), sd_pronase = sd(chl), var = var(chl), 
            n_pronase = n(), SE_pronase = sd(chl)/sqrt(n())); infiltration.pronase.summary #SE calculates the standard error of each group

#Make a csv file
#cv.pronase <- as.data.frame(infiltration.pronase.summary)
#pronase.df.data.for.summary <- "#SelectDestinationPath/pronaseSummary.csv" #For future reference
#write.csv(cv.pronase, pronase.df.data.for.summary)

#Plot relationship between sd and mean
#plot1 <- ggplot(infiltration.pronase.summary, aes(x = sd_pronase, y = mean_pronase, color= Treatment)) +
#  geom_point(shape = 15)+
#  theme_classic();plot1

#Barplot
barPlot_pronase <- ggplot(infiltration.pronase.summary, aes(x = reorder(Treatment, -mean_pronase), y = mean_pronase, fill=mean_pronase)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = mean_pronase - SE_pronase, ymax = mean_pronase + SE_pronase), width=0.2) +
  geom_text(label = c("ab", "a", "bc", "abc", "c", "abc", "c"), #letters extracted from HSD.test
            aes(y = mean_pronase+SE_pronase+1, x = reorder(Treatment, -mean_pronase)),vjust = -0.5, 
            size = 5) +
  scale_fill_gradient2(low='white', mid = 'yellow', high='darkolivegreen') +
  labs(fill = bquote("Chlorophyll" ~(ng/mm^2)))
barPlotCustomized_pronase <- barPlot_pronase + labs(title="Chlorophyll Content per Treatment", 
                                          x="Treatments", y = "Chlorophyll content in " ~(ng/mm^2)) +
  scale_y_continuous(guide = "prism_minor",
                     limits = c(0, 125),
                     expand = c(0, 0),
                     minor_breaks = seq(0, 125, 5)) + #this helps to land the bars on the x axis
  theme_classic() + theme_prism (border = F,
                                 axis_text_angle = 45) + 
  theme(legend.title = element_text(size = 12)); barPlotCustomized_pronase

##
#figure number 2 (A and B) fresh root weight and chlorophyll from root assay
root2 <- read.csv("rootweight2and1assayMarchandApril2021.csv")
str(root2)
head(root2)


#check normality
plotNormalHistogram(root2$weight)
hist(root2$qchl1)

PDB <- root2[which(root2$Treatment %in% c("PDB broth")),]
XCF <- root2[which(root2$Treatment %in% c("X. necrophora CFs")),]

shapiro.test(XCF$weight)
hist(XCF$weight)
shapiro.test(PDB$weight)
hist(PDB$weight)
#Both are normally distributed

#var.test 
var.test(weight ~ Treatment, root2)
#here the p value was lower than 0.05, so variances are not equal

#Two sided, independent t-test when variances are not equal
t.test(weight ~ Treatment, root2, var.equal=F)
#pooled t-test based if the variance are equal
#t.test(root2$, root2$, paired = TRUE)
#summary root weight
#tail(root2)
root2n <- root2 %>% # the names of the new data frame and the data frame to be summarized
  dplyr::group_by(Treatment) %>%   # the grouping variable
  dplyr::summarise(mean = mean(weight, na.rm=TRUE), sd = sd(weight, na.rm=TRUE), n = n(), SEex = sd(weight, na.rm=TRUE)/sqrt(n())) # calculates the standard error of each group

root2n

#make a csv file
cv.rootweight <- as.data.frame(root2n)

rootweight.df.data.for.summary <- "#SelectDestinationPath/rootweight.summary.csv"
#write.csv(cv.rootweight, rootweight.df.data.for.summary)




#more descriptive 
describeBy(root2$weight, root2$Treatment, na.rm = T)


#plot
rootplots2 <- ggplot(root2n, aes(x = reorder(Treatment, -mean), y = mean, fill=Treatment)) + 
  geom_col(width = 0.5) +  
  geom_errorbar(aes(ymin = mean - SEex, ymax = mean + SEex), width=0.2) +
  geom_text(label = c("a", "b"), 
            aes(y = mean+SEex+0.03, x = reorder(Treatment, -mean)),vjust = -0.5, 
            size = 1) +
  scale_fill_manual(values=c('gray', 'gray42'))


rootplot <- rootplots2 + labs(title="10 days after application", 
                              x="Treatments", y = "Fresh root biomass (g)") +
  scale_y_continuous(limits = c(0, 1),
                     expand = c(0, 0),
                     guide = guide_prism_minor()) + #this helps to land the bars on the x axis
  theme_classic() + theme_prism (border = F,
                                 axis_text_angle = 45) + 
  theme(legend.title = element_text(size = 12)); rootplot


#chemical chl quantification
#chlorophyll

#check normality
hist(root2$qchl1)
PDB.qchl <- root2[which(root2$Treatment %in% c("PDB broth")),]
XCF.qchl <- root2[which(root2$Treatment %in% c("X. necrophora CFs")),]

shapiro.test(PDB.qchl$qchl2)
shapiro.test(XCF.qchl$qchl2)

#transformation
qch.t <- transformTukey(root2$qchl1)

#add transformed column to df
qinfiltration.pronase.dataframe <- as.data.frame(root2)
qch.t.withtransformed <- cbind(qinfiltration.pronase.dataframe, qch.t)

#check normality
plotNormalHistogram(qch.t.withtransformed$qch.t)
PDB.qch.t <- qch.t.withtransformed[which(qch.t.withtransformed$Treatment %in% c("PDB broth")),]
XCF.qch.t <- qch.t.withtransformed[which(qch.t.withtransformed$Treatment %in% c("X. necrophora CFs")),]

shapiro.test(PDB.qch.t$qch.t)
shapiro.test(XCF.qch.t$qch.t)


head(root2)
#var.test 
var.test(qchl1 ~ Treatment, qch.t.withtransformed)
#here variances ARE equal

#Two sided, pooled t-test based if the variances are equal
#two sided, independent t-test when variances are not equal
t.test(qchl1 ~ Treatment, qch.t.withtransformed, var.equal=T)

#box plot using normalized data
boxplot(qchl1 ~ Treatment, data=root2)


#normalized data for analysis only
#summary root assay qchl
root2.qch <- root2 %>% # the names of the new data frame and the data frame to be summarized
  dplyr::group_by(Treatment) %>%   # the grouping variable
  dplyr::summarise(m = mean(qchl1, na.rm=TRUE), sd = sd(qchl1, na.rm=TRUE), n = n(), SE = sd(qchl1, na.rm=TRUE)/sqrt(n())) # calculates the standard error of each group

root2.qch

#make a csv file
cv.qch.rootweight <- as.data.frame(root2.qch)

qch.rootweight.df.data.for.summary <- "#SelectDestinationPath/qch.summary.csv"
#write.csv(cv.qch.rootweight, qch.rootweight.df.data.for.summary)




#plot
root2qch <- ggplot(root2.qch, aes(x = reorder(Treatment, -m), y = m, fill=m)) + 
  geom_col(width = 0.5) +  
  geom_errorbar(aes(ymin = m - SE, ymax = m + SE), width=0.2) +
  geom_text(label = c("a", "b"), 
            aes(y = m+SE+0.5, x = reorder(Treatment, -m)),vjust = -0.5, 
            size = 5) +
  scale_fill_gradient2(low='white', mid = 'yellow', high='darkolivegreen') +
  labs(fill = bquote("Chlorophyll" ~(ng/mm^2)))

root2qchplot <- root2qch + labs(title="Chlorophyll content 10 days after application", 
                              x="Treatments", y = "Chlorophyll" ~(ng/mm^2)) +
  scale_y_continuous(limits = c(0, 10),
                     expand = c(0, 0),
                     guide = guide_prism_minor()) + #this helps to land the bars on the x axis
  theme_classic() + theme_prism (border = F,
                                 axis_text_angle = 45) + 
  theme(legend.title = element_text(size = 12)); root2qchplot




#root length
root.length <- read.csv("rootlengthmay.csv")
str(root.length)

#check normality
hist(root.length$Length)
PDB.length <- root.length[which(root.length$treatment %in% c("c")),]
XCF.length <- root.length[which(root.length$treatment %in% c("x")),]

shapiro.test(PDB.length$Length)
hist(PDB.length$Length)
shapiro.test(XCF.length$Length)
hist(XCF.length$Length)

#length.root2 <- shapiro.test(root.length$Length)
#length.root2

#transformation
length.root2.t <- transformTukey(root.length$Length)

#add transformed column to df
lengthroot2.dataframe <- as.data.frame(root.length)
lengthroot2.withtransformed <- cbind(lengthroot2.dataframe, length.root2.t)
#head(lengthroot2.withtransformed)
#check normality
plotNormalHistogram(lengthroot2.withtransformed$length.root2.t)
PDB.length.t <- lengthroot2.withtransformed[which(lengthroot2.withtransformed$treatment %in% c("c")),]
XCF.length.t <- lengthroot2.withtransformed[which(lengthroot2.withtransformed$treatment %in% c("x")),]

shapiro.test(PDB.length.t$length.root2.t)
shapiro.test(XCF.length.t$length.root2.t)

head(lengthroot2.withtransformed)
#var.test 
var.test(Length ~ treatment, root.length)
#here variances not equal

#Two sided, pooled t-test based if the variances are equal
#two sided, independent t-test when variances are not equal
t.test(Length ~ treatment, root.length, var.equal=F)

#box plot using normalized data
boxplot(Length ~ treatment, data=lengthroot2.withtransformed)

#summary
root.length.sm <- root.length %>% # the names of the new data frame and the data frame to be summarized
  dplyr::group_by(treatment) %>%   # the grouping variable
  dplyr::summarise(mean = mean(Length, na.rm=TRUE), sd = sd(Length, na.rm=TRUE), n = n(), SEex = sd(Length, na.rm=TRUE)/sqrt(n())) # calculates the standard error of each group

root.length.sm

#make a csv file
cv.root.length <- as.data.frame(root.length.sm)

root.length.df.data.for.summary <- "#SelectDestinationPath/root.length.summary.csv"
#write.csv(cv.root.length, root.length.df.data.for.summary)



#plot
root.length.plot <- ggplot(root.length.sm, aes(x = reorder(treatment, -mean), y = mean, fill=treatment)) + 
  geom_col(width = 0.5) +  
  geom_errorbar(aes(ymin = mean - SEex, ymax = mean + SEex), width=0.2)+
  scale_fill_manual(values=c('gray', 'gray42'))

root.length.plot

root.length.plot.2 <- root.length.plot + labs(title="10 days after application", 
                                              x="Treatments", y = "Root length (mm)") +
  scale_y_continuous(limits = c(0, 12),
                     expand = c(0, 0),
                     guide = guide_prism_minor()) + #this helps to land the bars on the x axis
  theme_classic() + theme_prism (border = F,
                                 axis_text_angle = 0) + 
  theme(legend.title = element_text(size = 12)); root.length.plot.2





#chlorophyll
chlroot <- read.csv("chlgroup2mayandmarchdigital.csv")
str(chlroot)


#chlroot2 <- na.omit(chlroot)


#check normality
hist(chlroot$chl)
PDB.chl <- chlroot[which(chlroot$Treatment %in% c("PDB broth")),]
XCF.chl <- chlroot[which(chlroot$Treatment %in% c("X. necrophora CFs")),]

shapiro.test(PDB.chl$chl)
hist(PDB.chl$chl)
shapiro.test(XCF.chl$chl)
hist(XCF.chl$chl)

#transformation
chl.root2.t <- transformTukey(chlroot$chl)

#add transformed column to df
chlroot2.dataframe <- as.data.frame(chlroot)
chlroot2.withtransformed.chl <- cbind(chlroot2.dataframe, chl.root2.t)
head(chlroot2.withtransformed.chl)

#var.test 
var.test(chl ~ Treatment, chlroot2.withtransformed.chl)

#Two sided, pooled t-test based if the variances are not equal
#independent t test if they are equal
t.test(chl ~ Treatment, chlroot2.withtransformed.chl, var.equal=F)

#box plot using normalized data
#boxplot(chl ~ Treatment, data = chlroot2)

#normalized data for analysis only
#summary root assay chl
rootassay.chl.summary <- chlroot %>% # the names of the new data frame and the data frame to be summarized
  dplyr::group_by(Treatment) %>%   # the grouping variable
  dplyr::summarise(meanrch = mean(chl, na.rm=TRUE), sdrch = sd(chl, na.rm=TRUE), n = n(), SErch = sd(chl, na.rm=TRUE)/sqrt(n())) # calculates the standard error of each group

rootassay.chl.summary

#make a csv file
cv.dch <- as.data.frame(rootassay.chl.summary)

cv.dch.df.data.for.summary <- "#SelectDestinationPath/cv.dch.summary.csv"
#write.csv(cv.dch, cv.dch.df.data.for.summary)






#plot
root2chld <- ggplot(rootassay.chl.summary, aes(x = reorder(Treatment, -meanrch), y = meanrch, fill=meanrch)) + 
  geom_col(width = 0.7) +  
  geom_errorbar(aes(ymin = meanrch - SErch, ymax = meanrch + SErch), width=0.2) +
  scale_fill_gradient2(low='white', mid = 'yellow', high='darkolivegreen') +
  labs(fill = bquote("Chlorophyll" ~(ng/mm^2)))


rootplotd <- root2chld + labs(title="Chlorophyll content 10 days after application", 
                              x="Treatments", y = "Chlorophyll" ~(ng/mm^2)) +
  scale_y_continuous(limits = c(0, 200),
                     expand = c(0, 0),
                     guide = guide_prism_minor()) + #this helps to land the bars on the x axis
  theme_classic() + theme_prism (border = F,
                                 axis_text_angle = 45) + 
  theme(legend.title = element_text(size = 12)); rootplotd







#figure 3 chl from extracts assay
disk.july.extract <- read.csv("diskextractassayJuly21021.csv")
str(disk.july.extract)
summary(disk.july.extract)

#do this
disk.july.extracts <- disk.july.extract
disk.july.extracts[is.nan(disk.july.extracts$chl)] <- NA
str(disk.july.extracts)

disk.july.extracts.clean <- na.omit(disk.july.extract)

#stats
shapiro.test(disk.july.extracts$chl)
ggqqplot(disk.july.extract$chl)
hist(disk.july.extract$chl)

#transformation
disck.July21.chl <- transformTukey(disk.july.extract$chl)

#add transformed column to df
disk.july.extracts.dataframe <- as.data.frame(disk.july.extract)
disk.july.withtransformed.chl <- cbind(disk.july.extracts.dataframe, disck.July21.chl)
head(disk.july.withtransformed.chl)


#stats
#to conduct tukey HSD we need to conduct a lm first
disk.july.extracts.lm <- lm(chl ~ treatment + rep + techrep + plate + disk, data = disk.july.withtransformed.chl)
summary(disk.july.extracts.lm)


#ANOVA
disk.july.extracts.av <- aov(disk.july.extracts.lm)
summary(disk.july.extracts.av)

#redisuals
disk.july.extracts.av.residuals <- residuals(object = disk.july.extracts.av)
hist( x = disk.july.extracts.av.residuals)
ks.test(disk.july.extracts.av.residuals, "pnorm", mean(disk.july.extracts.av.residuals), sd(disk.july.extracts.av.residuals))
#if p-value is greater than 0.05, it suggests that the error terms are normally distributed
#Hence each group was sampled from normally distributed population.
#so no need for transformation

#tukey #to check the differences
disk.july.extracts.posthoc <- TukeyHSD(disk.july.extracts.av)
disk.july.extracts.posthoc
#plot(TukeyHSD(disk.june.extracts.av, conf.level = 0.95),las=3, col = "red")

#make a csv file
cv.ch.disk.posthoc <- as.data.frame(disk.july.extracts.posthoc$treatment)

posthoc.disk.df.data.for.summary <- "#SelectDestinationPath/posthoc.disk.summary.csv"
#write.csv(cv.ch.disk.posthoc, posthoc.disk.df.data.for.summary)


#for groups
tukey.disk.july.extracts <- HSD.test(disk.july.extracts.av, trt = 'treatment')
tukey.disk.july.extracts
#dput(as.character(tukey.chl.pronase.extracts$groups[[2]])) #get significance letters

disk.july.extracts.df <- as.data.frame(tukey.disk.july.extracts$groups)
disk.july.extracts.df

disk.july.extracts.tukey.data.for.letters <- "#SelectDestinationPath/disk.july.extracts.tukey.csv"
#write.csv(disk.july.extracts.df, disk.july.extracts.tukey.data.for.letters)

disk.july.extracts.tukeydata <- read.csv("#SelectDestinationPath/disk.july.extracts.tukey.csv")
as.data.frame(disk.july.extracts.tukeydata)

df.disk.july.extracts.tukeydata <- disk.july.extracts.tukeydata[order(disk.july.extracts.tukeydata$X),]
df.disk.july.extracts.tukeydata

#now get significance letters
dput(as.character(df.disk.july.extracts.tukeydata$groups))
#compare with direct control


#summary disk extracts
disk2.july.extracts <- disk.july.extracts %>% # the names of the new data frame and the data frame to be summarized
  dplyr::group_by(treatment) %>%   # the grouping variable
  dplyr::summarise(m = mean(chl, na.rm=TRUE), sdd = sd(chl, na.rm=TRUE), nn = n(), SEE = sd(chl, na.rm=TRUE)/sqrt(n())) # calculates the standard error of each group

disk2.july.extracts

#make a csv file
disk.cv.dch <- as.data.frame(disk2.july.extracts)

disk.cv.dch.df.data.for.summary <- "#SelectDestinationPath/disk.dch.summary.csv"
#write.csv(disk.cv.dch, disk.cv.dch.df.data.for.summary)


#plot
disk.july.extracts.plot <- ggplot(disk2.july.extracts, aes(x = reorder(treatment, -m), y = m, fill=m)) + 
  geom_col() + 
  geom_text(label = c("ab", "bc", "ab", "abc", "ab", "a", "d", "abc", "e", "d", "c", 
                      "bc", "cd"), aes(y = m+SEE+0.5, x = reorder(treatment, -m)),vjust = -0.5, 
            size = 5) +
  geom_errorbar(aes(ymin = m - SEE, ymax = m + SEE), width=0.2)+
  scale_fill_gradient2(low='white', mid = 'yellow', high='darkolivegreen') +
  labs(fill = bquote("Chlorophyll" ~(ng/mm^2)))

disk.july.extracts.plot

disk.july.extracts.plot.d2 <- disk.july.extracts.plot + labs(title="Chlorophyll Content per Treatment/Extracts on leaf disks July 2021 (after 48 of exposure)", 
                                                             x="Treatments", y = "Chlorophyll content in " ~(ng/mm^2)) +
  scale_y_continuous(guide = "prism_minor",
                     limits = c(0, 160),
                     expand = c(0, 0),
                     minor_breaks = seq(0, 160, 5)) + #this helps to land the bars on the x axis
  theme_classic() + theme_prism (border = F,
                                 axis_text_angle = 45) + 
  theme(legend.title = element_text(size = 12)); disk.july.extracts.plot.d2








#fungal competition
#figure 2 dilution 10 and 50 all 15 fungi
#change to cf1 if not testing only day5
cf0 <- read.csv("cfassay2020octfnl.csv")
str(cf0)
summary(cf0)

#test only 5 days
cf1 <-cf0[which(cf0$day %in% c('5')),]
str(cf1)

#this is long
#calculate the mean of the measurements (each side of the plate)
mcf <-cf1 %>%
  rowwise() %>%
  mutate(Mean = mean(c(side1,side2,side3,side4), na.rm=TRUE))
mcf
mf <-as.data.frame(mcf)
mf

#get inhibition of all fungi

#get data #macrophomina
macro<-mf[which(mf$fungi %in% c('Macrophomina')),]
dim(macro)

mp <-mf[which(macro$treatment %in% c('CCF')),]
dim(mp)
mc <- mp[order(-mp$concentration),]
mc

mp2 <-mf[which(macro$treatment %in% c('XCF')),]
dim(mp2)
mc2 <- mp2[order(-mp2$concentration),]
mc2



Mch <-data.frame(fungus= mc$fungi,
                 rep=mc$repetition, 
                 trep=mc$techrep, 
                 pnum=mc$plate_number,
                 pnumx=mc2$plate_number,
                 concentrationcont=mc$concentration,
                 concentrationxy=mc2$concentration,
                 codeccf=mc$code,
                 codexcf=mc2$code,
                 CCF=mc$Mean, 
                 XCF=mc2$Mean)
Mch

#calculate inhibition
Mch1 <-Mch %>%
  mutate(Inhibition = (CCF-XCF)/CCF * 100)
MchX <-as.data.frame(Mch1)
MchX
#check names
#cf1$fungi

#get data for Rhizoctonia
macro1<-mf[which(mf$fungi %in% c('Rhizoctonia')),]
dim(macro1)

rt <-macro1[which(macro1$treatment %in% c('CCF')),]
dim(rt)
rc <- rt[order(-rt$concentration),]
rc

rt2 <-macro1[which(macro1$treatment %in% c('XCF')),]
dim(rt2)
rc2 <- rt2[order(-rt2$concentration),]
rc2


Rch <-data.frame(fungus= rc$fungi,
                 rep=rc$repetition, 
                 trep=rc$techrep, 
                 pnum=rc$plate_number,
                 pnumx=rc2$plate_number,
                 concentrationcont=rc$concentration,
                 concentrationxy=rc2$concentration,
                 codeccf=rc$code,
                 codexcf=rc2$code,
                 CCF=rc$Mean, 
                 XCF=rc2$Mean)
dim(Rch)
Rch
#calculate inhibition
Rh1 <-Rch %>%
  mutate(Inhibition = (CCF-XCF)/CCF * 100)
RhX <-as.data.frame(Rh1)
RhX$Inhibition[RhX$Inhibition < 0] <- 0
RhX

#cf1$fungi


#get data for "Sclerotium"
scle<-mf[which(mf$fungi %in% c("Sclerotium")),]
dim(scle)

sce <- scle[which(scle$treatment %in% c('CCF')),]
dim(sce)
sc <- sce[order(-sce$concentration),]
sc

sce2 <- scle[which(scle$treatment %in% c('XCF')),]
dim(sce2)
sc2 <- sce2[order(-sce2$concentration),]
dim(sc2)


scl <-data.frame(fungus= sc$fungi,
                 rep=sc$repetition, 
                 trep=sc$techrep, 
                 pnum=sc$plate_number,
                 pnumx=sc2$plate_number,
                 concentrationcont=sc$concentration,
                 concentrationxy=sc2$concentration,
                 codeccf=sc$code,
                 codexcf=sc2$code,
                 CCF=sc$Mean, 
                 XCF=sc2$Mean)
dim(scl)
scl

#calculate inhibition
scl1 <-scl %>%
  mutate(Inhibition = (CCF-XCF)/CCF * 100)
sclx <-as.data.frame(scl1)
sclx$Inhibition[sclx$Inhibition < 0] <- 0
sclx

#cf1$fungi
#"Ceratocystis"  

#get data for "Ceratocystis"
cer<-mf[which(mf$fungi %in% c("Ceratocystis")),]
dim(cer)

crk <- cer[which(cer$treatment %in% c('CCF')),]
dim(crk)
cr <- crk[order(-crk$concentration),]
cr

crk2 <- cer[which(cer$treatment %in% c('XCF')),]
dim(crk2)
cr2 <- crk2[order(-crk2$concentration),]
cr2

crt <-data.frame(fungus= cr$fungi,
                 rep=cr$repetition, 
                 trep=cr$techrep, 
                 pnum=cr$plate_number,
                 pnumx=cr2$plate_number,
                 concentrationcont=cr$concentration,
                 concentrationxy=cr2$concentration,
                 codeccf=cr$code,
                 codexcf=cr2$code,
                 CCF=cr$Mean, 
                 XCF=cr2$Mean)
dim(crt)
crt

#calculate inhibition
cera <-crt %>%
  mutate(Inhibition = (CCF-XCF)/CCF * 100)
cerax <-as.data.frame(cera)
cerax$Inhibition[cerax$Inhibition < 0] <- 0
cerax

#cf1$fungi
#"Rhizopus" 
rz<-mf[which(mf$fungi %in% c("Rhizopus")),]
dim(rz)
rz

rk <- rz[which(rz$treatment %in% c('CCF')),]
dim(rk)
r <- rk[order(-rk$concentration),]
r

rk2 <- rz[which(rz$treatment %in% c('XCF')),]
dim(rk2)
r2 <- rk2[order(-rk2$concentration),]
r2



rzt <-data.frame(fungus= r$fungi,
                 rep=r$repetition, 
                 trep=r$techrep, 
                 pnum=r$plate_number,
                 pnumx=r2$plate_number,
                 concentrationcont=r$concentration,
                 concentrationxy=r2$concentration,
                 codeccf=r$code,
                 codexcf=r2$code,
                 CCF=r$Mean, 
                 XCF=r2$Mean)
dim(rzt)
rzt

#calculate inhibition
rza <-rzt %>%
  mutate(Inhibition = (CCF-XCF)/CCF * 100)
rzax <-as.data.frame(rza)
rzax



#cf1$fungi

sp<-mf[which(mf$fungi %in% c("Aspergillus")),]
dim(sp)

rr1 <- sp[which(sp$treatment %in% c('CCF')),]
dim(rr1)
pr <- rr1[order(-rr1$concentration),]
pr

rr2 <- sp[which(sp$treatment %in% c('XCF')),]
dim(rr2)
pr2 <- rr2[order(-rr2$concentration),]
pr2

przt <-data.frame(fungus= pr$fungi,
                  rep=pr$repetition, 
                  trep=pr$techrep, 
                  pnum=pr$plate_number,
                  pnumx=pr2$plate_number,
                  concentrationcont=pr$concentration,
                  concentrationxy=pr2$concentration,
                  codeccf=pr$code,
                  codexcf=pr2$code,
                  CCF=pr$Mean, 
                  XCF=pr2$Mean)
dim(przt)

#calculate inhibition
prza <-przt %>%
  mutate(Inhibition = (CCF-XCF)/CCF * 100)
przax <-as.data.frame(prza)
przax

#(cf1$fungi)
#"cercospora_z"

zsp<-mf[which(mf$fungi %in% c("cercospora_z")),]
dim(zsp)

zr <- zsp[which(zsp$treatment %in% c('CCF')),]
dim(zr)
zpr <- zr[order(-zr$concentration),]
zpr

zr2 <- zsp[which(zsp$treatment %in% c('XCF')),]
dim(zr2)
zpr2 <- zr2[order(-zr2$concentration),]
zpr2

zprzt <-data.frame(fungus= zpr$fungi,
                   rep=zpr$repetition, 
                   trep=zpr$techrep, 
                   pnum=zpr$plate_number,
                   pnumx=zpr2$plate_number,
                   concentrationcont=zpr$concentration,
                   concentrationxy=zpr2$concentration,
                   codeccf=zpr$code,
                   codexcf=zpr2$code,
                   CCF=zpr$Mean, 
                   XCF=zpr2$Mean)
dim(zprzt)

#calculate inhibition
zprza <-zprzt %>%
  mutate(Inhibition = (CCF-XCF)/CCF * 100)
zprzax <-as.data.frame(zprza)
zprzax

#cf1$fungi[1500]
#"Exserohilum"

xx<-mf[which(mf$fungi %in% c("Exserohilum")),]
dim(xx)

x1 <- xx[which(xx$treatment %in% c('CCF')),]
dim(x1)
xr <- x1[order(-x1$concentration),]
xr

x2 <- xx[which(xx$treatment %in% c('XCF')),]
dim(x2)
xr2 <- x2[order(-x2$concentration),]
xr2


zx <-data.frame(fungus= xr$fungi,
                rep=xr$repetition, 
                trep=xr$techrep, 
                pnum=xr$plate_number,
                pnumx=xr2$plate_number,
                concentrationcont=xr$concentration,
                concentrationxy=xr2$concentration,
                codeccf=xr$code,
                codexcf=xr2$code,
                CCF=xr$Mean, 
                XCF=xr2$Mean)
dim(zx)

#calculate inhibition
zd <- zx %>%
  mutate(Inhibition = (CCF-XCF)/CCF * 100)
zxx <-as.data.frame(zd)
zxx

#cf1$fungi[1900]
#"curvularia"

cr<-mf[which(mf$fungi %in% c("curvularia")),]
dim(cr)

cv1 <- cr[which(cr$treatment %in% c('CCF')),]
dim(cv1)
cxr <- cv1[order(-cv1$concentration),]
cxr

cv2 <- cr[which(cr$treatment %in% c('XCF')),]
dim(cv2)
cxr2 <- cv2[order(-cv2$concentration),]
cxr2


uzx <-data.frame(fungus= cxr$fungi,
                 rep=cxr$repetition, 
                 trep=cxr$techrep, 
                 pnum=cxr$plate_number,
                 pnumx=cxr2$plate_number,
                 concentrationcont=cxr$concentration,
                 concentrationxy=cxr2$concentration,
                 codeccf=cxr$code,
                 codexcf=cxr2$code,
                 CCF=cxr$Mean, 
                 XCF=cxr2$Mean)
dim(uzx)

#calculate inhibition
uzd <- uzx %>%
  mutate(Inhibition = (CCF-XCF)/CCF * 100)
uxx <-as.data.frame(uzd)
uxx


#cf1$fungi[2100]
#"cercospora_s"
scr<-mf[which(mf$fungi %in% c("cercospora_s")),]
dim(scr)

s1 <- scr[which(scr$treatment %in% c('CCF')),]
dim(s1)
scxr <- s1[order(-s1$concentration),]
scxr

s2 <- scr[which(scr$treatment %in% c('XCF')),]
dim(s2)
scxr2 <- s2[order(-s2$concentration),]
scxr2


suzx <-data.frame(fungus= scxr$fungi,
                  rep=scxr$repetition, 
                  trep=scxr$techrep, 
                  pnum=scxr$plate_number,
                  pnumx=scxr2$plate_number,
                  concentrationcont=scxr$concentration,
                  concentrationxy=scxr2$concentration,
                  codeccf=scxr$code,
                  codexcf=scxr2$code,
                  CCF=scxr$Mean, 
                  XCF=scxr2$Mean)
dim(suzx)

#calculate inhibition
suzd <- suzx %>%
  mutate(Inhibition = (CCF-XCF)/CCF * 100)
suxx <-as.data.frame(suzd)
suxx

#cf1$fungi[2400]
#"monilochaetes"

mn<-mf[which(mf$fungi %in% c("monilochaetes")),]
dim(mn)


n1 <- mn[which(mn$treatment %in% c('CCF')),]
dim(n1)
mnr <- n1[order(-n1$concentration),]
mnr

n2 <- mn[which(mn$treatment %in% c('XCF')),]
dim(n2)
mnr2 <- n2[order(-n2$concentration),]
mnr2


mnx <-data.frame(fungus= mnr$fungi,
                 rep=mnr$repetition, 
                 trep=mnr$techrep, 
                 pnum=mnr$plate_number,
                 pnumx=mnr2$plate_number,
                 concentrationcont=mnr$concentration,
                 concentrationxy=mnr2$concentration,
                 codeccf=mnr$code,
                 codexcf=mnr2$code,
                 CCF=mnr$Mean, 
                 XCF=mnr2$Mean)
dim(mnx)

#calculate inhibition
mnxx <- mnx %>%
  mutate(Inhibition = (CCF-XCF)/CCF * 100)
mnxq <-as.data.frame(mnxx)
mnxq$Inhibition[mnxq$Inhibition < 0] <- 0
mnxq



#cf1$fungi[1400]
"Colletotrichum"
cll<-mf[which(mf$fungi %in% c("Colletotrichum")),]
dim(cll)

l1 <- cll[which(cll$treatment %in% c('CCF')),]
dim(l1)
l1
lr <- l1[order(-l1$concentration),]
lr

l2 <- cll[which(cll$treatment %in% c('XCF')),]
dim(l2)
l2
lr2 <- l2[order(-l2$concentration),]
lr2


lx <-data.frame(fungus= lr$fungi,
                rep=lr$repetition, 
                trep=lr$techrep, 
                pnum=lr$plate_number,
                pnumx=lr2$plate_number,
                concentrationcont=lr$concentration,
                concentrationxy=lr2$concentration,
                codeccf=lr$code,
                codexcf=lr2$code,
                CCF=lr$Mean, 
                XCF=lr2$Mean)
dim(lx)

#calculate inhibition
lxx <- lx %>%
  mutate(Inhibition = (CCF-XCF)/CCF * 100)
llx <-as.data.frame(lxx)
llx


#cf1$fungi[1600]
#Maganaporthe

mg<-mf[which(mf$fungi %in% c("Maganaporthe")),]
dim(mg)

m1 <- mg[which(mg$treatment %in% c('CCF')),]
dim(m1)
mgr <- m1[order(-m1$concentration),]
mgr

m2 <- mg[which(mg$treatment %in% c('XCF')),]
dim(m2)
mgr2 <- m2[order(-m2$concentration),]
mgr2


mgx <-data.frame(fungus= mgr$fungi,
                 rep=mgr$repetition, 
                 trep=mgr$techrep, 
                 pnum=mgr$plate_number,
                 pnumx=mgr2$plate_number,
                 concentrationcont=mgr$concentration,
                 concentrationxy=mgr2$concentration,
                 codeccf=mgr$code,
                 codexcf=mgr2$code,
                 CCF=mgr$Mean, 
                 XCF=mgr2$Mean)
dim(mgx)

#calculate inhibition
mgxx <- mgx %>%
  mutate(Inhibition = (CCF-XCF)/CCF * 100)
mmgx <-as.data.frame(mgxx)
mmgx

#cer_f

fmg<-mf[which(mf$fungi %in% c("cercospora_f")),]
dim(fmg)

cf1 <- fmg[which(fmg$treatment %in% c('CCF')),]
dim(cf1)
fmgr <- cf1[order(-cf1$concentration),]
fmgr

cf2 <- fmg[which(fmg$treatment %in% c('XCF')),]
dim(cf2)
fmgr2 <- cf2[order(-cf2$concentration),]


fmgx <-data.frame(fungus= fmgr$fungi,
                  rep=fmgr$repetition, 
                  trep=fmgr$techrep, 
                  pnum=fmgr$plate_number,
                  pnumx=fmgr2$plate_number,
                  concentrationcont=fmgr$concentration,
                  concentrationxy=fmgr2$concentration,
                  codeccf=fmgr$code,
                  codexcf=fmgr2$code,
                  CCF=fmgr$Mean, 
                  XCF=fmgr2$Mean)
dim(fmgx)

#calculate inhibition
fmgxx <- fmgx %>%
  mutate(Inhibition = (CCF-XCF)/CCF * 100)
fmmgx <-as.data.frame(fmgxx)
fmmgx$Inhibition[fmmgx$Inhibition < 0] <- 0
fmmgx


##
fmj<-mf[which(mf$fungi %in% c("cercospora_j")),]
dim(fmj)

c1 <- fmj[which(fmj$treatment %in% c('CCF')),]
dim(c1)

fmjr <- c1[order(-c1$concentration),]
fmjr

c2 <- fmj[which(fmj$treatment %in% c('XCF')),]
dim(c2)
fmjr2 <- c2[order(-c2$concentration),]
fmjr2


fmjx <-data.frame(fungus= fmjr$fungi,
                  rep=fmjr$repetition, 
                  trep=fmjr$techrep, 
                  pnum=fmjr$plate_number,
                  pnumx=fmjr2$plate_number,
                  concentrationcont=fmjr$concentration,
                  concentrationxy=fmjr2$concentration,
                  codeccf=fmjr$code,
                  codexcf=fmjr2$code,
                  CCF=fmjr$Mean, 
                  XCF=fmjr2$Mean)
dim(fmjx)


#calculate inhibition
fmjxx <- fmjx %>%
  mutate(Inhibition = (CCF-XCF)/CCF * 100)
fmmjx <-as.data.frame(fmjxx)
dim(fmmjx)
fmmjx$Inhibition[fmmjx$Inhibition < 0] <- 0
fmmjx


#Join all dfs | all the data
total15 <- rbind(MchX, RhX, sclx, cerax, rzax, przax, zprzax, zxx, uxx, suxx, mnxq, llx, mmgx, fmmgx, fmmjx)
total15

#new part
#Converting NaN into NA
#Here we have generated NaN values due to NA present during the calculation of inhibition rates or by (0-0/0) division. Therefore, we are going to convert those NaNs into NAs.

#function to remove nan
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
totl15 <- total15
totl15[is.nan(totl15)] <- NA
totl15


#Summary
#head(cf1)
#mean by concentration
#(for plotall15)
meanbycontc<-totl15%>%
  dplyr::group_by(fungus, concentrationcont)%>%
  dplyr::summarise(Inhibition=mean(Inhibition, na.rm = T))
tr<-data.frame(meanbycontc)
trptosave<-tr %>% 
  dplyr::mutate(across(where(is.numeric), round, 0))
tr

#change view of data, make it wider
#use tidyverse
tes <- trptosave %>%
  pivot_wider(c("fungus"), names_from = concentrationcont, values_from = Inhibition)
tes1 <- data.frame(tes); tes1

#rename columns (for plotall15)
inb2 <- dplyr::rename(tes1, c10=X10, c50=X50)
inb2
inb2.df_5days <- data.frame(inb2)
#getwd()
#export data, must have java installed on computer 
#write.xlsx(total15, "all15fungi_inhibition_5days.xlsx")
#write.xlsx(trptosave, "inhibitionbyconcentration_5days.xlsx")
#write.csv(inb2.df_5days, "#SelectDestinationPath/dataofinhibitionbyconcentration_5days.csv")

#summary by dilution
inh <- totl15 %>% # the names of the new data frame and the data frame to be summarized
  dplyr::group_by(concentrationcont) %>%   # the grouping variable
  dplyr::summarise(m = mean(Inhibition, na.rm=TRUE), sdd = sd(Inhibition, na.rm=TRUE), nn = n(),
                   SEE = sd(Inhibition, na.rm=TRUE)/sqrt(n())) # calculates the standard error of each group
inh

#typeof(barinh)
as.data.frame(inh)
#MAKE concentrationcont character value
inh$concentrationcont <- as.factor(inh$concentrationcont)
typeof(inh$concentrationcont)
class(inh$concentrationcont)

#produce a data frame of general mean of inhibition including both dilutions
#generl inhibition
meangen<-totl15%>%
  dplyr::group_by(fungus)%>%
  dplyr::summarise(Inhibition = mean(Inhibition, na.rm = T))
data.frame(meangen)

head(totl15)
#summary all15plot
inhall <- totl15 %>% # the names of the new data frame and the data frame to be summarized
  dplyr::group_by(fungus, concentrationcont) %>%   # the grouping variable
  dplyr::summarise(m = mean(Inhibition, na.rm=TRUE), sdd = sd(Inhibition, na.rm=TRUE), nn = n(),
                   SEE = sd(Inhibition, na.rm=TRUE)/sqrt(n())) # calculates the standard error of each group
inhall.df <- as.data.frame(inhall)
inhall.df

totl15n <- totl15 %>% # the names of the new data frame and the data frame to be summarized
  dplyr::group_by(fungus, concentrationxy) %>%   # the grouping variable
  dplyr::summarise(mean = mean(Inhibition, na.rm=TRUE), sd = sd(Inhibition, na.rm=TRUE), n = n(), SEex = sd(Inhibition, na.rm=TRUE)/sqrt(n())) # calculates the standard error of each group

as.data.frame(totl15n)

#make a csv file
cf.cv <- as.data.frame(inhall)

cf.cv.df.data.for.summary <- "#SelectDestinationPath/cf.summary.csv"
#write.csv(cf.cv, cf.cv.df.data.for.summary)


#inhall and totl15n must yield same results

#Statistics
#stats
#Group by fungus and dilution
totl15.50 <- totl15[which(totl15$concentrationcont %in% c('50')),]
totl15.10 <- totl15[which(totl15$concentrationcont %in% c('10')),]

#t-test to compare differences of each Fungal inhibition by concentration
totl15.50a <- tibble::as_tibble(totl15.50)
r16 <- totl15.50a %>% group_by(fungus) %>%
  dplyr::do(tpvals = t.test(.$Inhibition, totl15.10$Inhibition, paired = F, var.equal = FALSE)) %>%
  dplyr::summarize(fungus, pvals = tpvals$p.value)
r16


#do this
totl15.clean <- na.omit(totl15)
totl15.clean
#now conduct stats for anova
shapiro.test(totl15.clean$Inhibition)
ggqqplot(totl15.clean$Inhibition)
hist(totl15.clean$Inhibition)
#head(total15.clean)

#transformation
plotNormalHistogram(totl15.clean$Inhibition)  #need rcompanion package to use this function
#tInhibition <- transformTukey(totl15.clean$Inhibition) #transformation not used because it does not change anything 
#plotNormalHistogram(tInhibition)
#proceed with caution

#stats
#to conduct tukey HSD we need to conduct a lm first
str(totl15.clean)
totl15.Inhibition.lm <- lm(Inhibition ~ fungus + as.factor(concentrationxy) + as.factor(rep) + as.factor(trep), data = totl15)
summary(totl15.Inhibition.lm)

#ANOVA
totl15.Inhibition.av <- aov(totl15.Inhibition.lm)
summary(totl15.Inhibition.av)


#redisuals
totl15.Inhibition.av.residuals <- residuals( object = totl15.Inhibition.av )
hist( x = totl15.Inhibition.av.residuals)
ks.test(totl15.Inhibition.av.residuals, "pnorm", mean(totl15.Inhibition.av.residuals), sd(totl15.Inhibition.av.residuals))
#if p-value is greater than 0.05, it suggests that the error terms are normally distributed
#Hence each group was sampled from normally distributed population.
#so no need for transformation

#tukey #to check the differences
totl15.Inhibition.posthoc <- TukeyHSD(totl15.Inhibition.av)
totl15.Inhibition.posthoc
#df.comparison.fungus <- as.data.frame(totl15.Inhibition.posthoc$fungus)
#plot(TukeyHSD(disk.june.extracts.av, conf.level = 0.95),las=3, col = "red")

#make a csv file
cv.cf.15fungi.posthoc <- as.data.frame(totl15.Inhibition.posthoc$fungus)
cv.cf.concent.15fungi.posthoc <- as.data.frame(totl15.Inhibition.posthoc$`as.factor(concentrationxy)`)

posthoc.cf15.df.data.for.summary <- "#SelectDestinationPath/posthoc.cf.15fungi.summary.csv"
#write.csv(cv.cf.15fungi.posthoc, posthoc.cf15.df.data.for.summary)
posthoc.cf15.cont.df.data.for.summary <- "#SelectDestinationPath/posthoc.cf.15fungi.concent.summary.csv"
#write.csv(cv.cf.concent.15fungi.posthoc, posthoc.cf15.cont.df.data.for.summary)

#for groups
#among fungi
tukey.totl15.Inhibition <- HSD.test(totl15.Inhibition.av, trt = 'fungus')
tukey.totl15.Inhibition

#between groups
tukey.totl15.Inhibition.dilution <- HSD.test(totl15.Inhibition.av, trt = 'as.factor(concentrationxy)')
tukey.totl15.Inhibition.dilution
#dput(as.character(tukey.totl15.Inhibition$groups[[2]])) #get significance letters

tukey.totl15.Inhibition.df <- as.data.frame(tukey.totl15.Inhibition$groups)
tukey.totl15.Inhibition.df
getwd()
tukey.totl15.Inhibition.df.tukey.data.for.letters <- "#SelectDestinationPath/tukeygroupalltotl15July.Inhibition.csv"
#write.csv(tukey.totl15.Inhibition.df, tukey.totl15.Inhibition.df.tukey.data.for.letters)

totl15.Inhibition.tukeydata <- read.csv("#SelectDestinationPath/tukeygroupalltotl15July.Inhibition.csv")
as.data.frame(totl15.Inhibition.tukeydata)

df.totl15.Inhibition.tukeydata <- totl15.Inhibition.tukeydata[order(totl15.Inhibition.tukeydata$X),]
df.totl15.Inhibition.tukeydata

#now get significance letters
dput(as.character(df.totl15.Inhibition.tukeydata$groups))
#compare with direct control


#plots
#dput(as.character(inb2.df_5days$fungus))
names <- c("A. flavus", "A. flavus", "C. fimbriata", "C. fimbriata", "C. flagellaris", "C. flagellaris", "C. janseana", "C. janseana", "C. sojina", "C. sojina", "C. zeaemaydis", "C. zeaemaydis", "G. cingulata", "G. cingulata", "C. lunata", "C. lunata", "E. tursicum", "E. tursicum", "M. phaseolina", "M. phaseolina", "M. oryzae", "M. oryzae", "M. infuscans", "M. infuscans", "R. solani", "R. solani", "R. stolonifer", "R. stolonifer", "S. rolfsii", "S. rolfsii")

#order.name <- c("C. zeaemaydis", "C. zeaemaydis", "C. fimbriata", "C. fimbriata", "E. tursicum", "E. tursicum",  "C. flagellaris", "C. flagellaris", "C. sojina", "C. sojina", "C. janseana", "C. janseana",  "G. cingulata", "G. cingulata",  "C. lunata", "C. lunata", "M. oryzae",  
#           "M. oryzae", "A. flavus", "A. flavus", "R. stolonifer", "R. stolonifer", "M. phaseolina", "M. phaseolina", "M. infuscans",  "M. infuscans", "S. rolfsii", "S. rolfsii", "R. solani", "R. solani")


#add names to data frame
inhall$fungi <- names
inhall$fungi

#hardcoding like nobodyelse

#new
ordered <-inhall %>%
  arrange(m) %>%
  mutate(fungi = factor(fungi, levels = c("C. zeaemaydis", "C. fimbriata", "E. tursicum","C. flagellaris", "C. sojina", "C. janseana",
                                          "G. cingulata", "C. lunata", "M. oryzae",  "A. flavus", "R. stolonifer", "M. phaseolina",
                                          "M. infuscans", "S. rolfsii", "R. solani"))) %>%
  ggplot( aes(x=fungi, y=m, fill=factor(concentrationcont))) + 
  geom_bar(stat = "identity", position = "dodge") +  
  geom_errorbar(aes(ymin = m - SEE, ymax = m + SEE), width=0.8, size=0.5, position = position_dodge(0.9))



inhplt.odered <- ordered + labs(title="inhibition % july 2021", 
                            x="Fungi", y = "Inhibition %") +
  scale_fill_manual(values=c('gray', "gray42"))+
  scale_y_continuous(guide = "prism_minor",
                     limits = c(0, 100),
                     expand = c(0, 0),
                     minor_breaks = seq(0, 100, 5)) + #this helps to land the bars on the x axis
  theme_classic() + theme_prism (border = F,
                                 axis_text_angle = 45) + 
  theme(legend.title = element_text(size = 12));inhplt.odered

#great it worked


#old
#(inhall, aes(x = reorder(fungi, -m), y = m, fill=factor(concentrationcont)
inhallplot <- ggplot(inhall, aes(x = reorder(fungi, -m), y = m, fill=factor(concentrationcont))) + 
  geom_bar(stat = "identity", position = "dodge") +  
  geom_errorbar(aes(ymin = m - SEE, ymax = m + SEE), width=0.8, size=0.5, position = position_dodge(0.9))
inhallplot

inhplt <- inhallplot + labs(title="inhibition % may 2021", 
                            x="Fungi", y = "Inhibition %") +
  scale_fill_manual(values=c('gray', "gray42"))+
  scale_y_continuous(guide = "prism_minor",
                     limits = c(0, 100),
                     expand = c(0, 0),
                     minor_breaks = seq(0, 100, 5)) + #this helps to land the bars on the x axis
  theme_classic() + theme_prism (border = F,
                                 axis_text_angle = 45) + 
  theme(legend.title = element_text(size = 12));inhplt


#add label manually
#use this 
#totl15.Inhibition.tukeydata #or the ordered one
#df.totl15.Inhibition.tukeydata

#geom_text(label = c("efg", "ab", "abcd", "cdef", "bcde", "a", "cdef", "defg", "abc", 
#                    "gh", "efg", "hi", "i", "fgh", "i"), aes(y = m+SEE+0.5, x = reorder(fungus, -m)),vjust = -0.5, 
#          size = 5) +


#plot by dilution
inhibition.plot.forthesis <- ggplot(inh, aes(x = reorder(concentrationcont, -m), y = m, fill=concentrationcont)) + 
  geom_col(width = 0.5) +  #, position = position_dodge(width = 0.7)
  geom_errorbar(aes(ymin = m - SEE, ymax = m + SEE), width=0.2); inhibition.plot.forthesis



inhibition.plot.forthesis.2bars <- inhibition.plot.forthesis + labs(title="inhibition % by dilution of CFs", 
                                                                    x = "Dilutions (v/v)", y = "Inhibition % ") +
  scale_fill_manual(values=c('gray', "gray42"))+
  scale_y_continuous(guide = "prism_minor",
                     limits = c(0, 80),
                     expand = c(0, 0),
                     minor_breaks = seq(0, 80, 5)) + #this helps to land the bars on the x axis
  theme_classic() + theme_prism (border = F,
                                 axis_text_angle = 0) + 
  theme(legend.title = element_text(size = 12)); inhibition.plot.forthesis.2bars












#PDA growth assay figure
grFungi <- read.csv("growthallassaypda.csv")
str(grFungi)
head(grFungi)

plotNormalHistogram(grFungi$growth)


pdafungi <- grFungi[which(grFungi$days %in% c('day5')),]
xylaria <- grFungi[which(grFungi$Fungi %in% c("X. necrophora")),]

#ok lets make a function, hard codded
getPDAdata <- function(x, y, d){
  data <- x[which(x$Fungi %in% c(y) & x$days %in% c(d)),]
  return(data)
}

#usage
xday5 <- getPDAdata(grFungi, c("X. necrophora"), c("day5"))

#pdafungi
plotNormalHistogram(pdafungi$growth)
shapiro.test(pdafungi$growth)


#transformation
pdasqrt <- sqrt(pdafungi$growth)
plotNormalHistogram(pdasqrt) #root squared for right skewed data
#shapiro.test(pdasqrt) it did not work
#logpda <- log(na.omit(pdafungi$growth)) #it did not work
#hist(logpda)
#shapiro.test(logpda)
pdatukey.t <- transformTukey(pdafungi$growth) #it did not work either

#add transformed column to df
pdafungi.dataframe <- as.data.frame(pdafungi)
pdafungi.df.t <- cbind(pdafungi.dataframe, pdasqrt)
head(pdafungi.df.t)

#proceed with caution, later test normality of residuals
str(pdafungi)
pdafungi.lm <- lm(pdasqrt ~ Fungi + rep + trep, data = pdafungi.df.t)
summary(pdafungi.lm)

remodel <- residuals(pdafungi.lm)
plotNormalHistogram(remodel)

#ANOVA
pdafungi.av <- aov(pdafungi.lm)
summary(pdafungi.av)


#redisuals
pdafungi.av.residuals <- residuals( object = pdafungi.av )
hist( x = pdafungi.av.residuals)
ks.test(pdafungi.av.residuals, "pnorm", mean(pdafungi.av.residuals), sd(pdafungi.av.residuals))
#if p-value is greater than 0.05, it suggests that the error terms are normally distributed
#Hence each group was sampled from normally distributed population.
#so no need for transformation

#summary
PDA.summary2 <- grFungi %>% # the names of the new data frame and the data frame to be summarized
  dplyr::group_by(Fungi, days) %>%   # the grouping variable
  dplyr::summarise(mean = mean(growth, na.rm=TRUE), sd = sd(growth, na.rm=TRUE), n = n(), SE = sd(growth, na.rm=TRUE)/sqrt(n())) # calculates the standard error of each group

PDA.summary2.df <- as.data.frame(PDA.summary2)
PDA.summary2.df

PDA.summary3 <- pdafungi %>% # the names of the new data frame and the data frame to be summarized
  dplyr::group_by(Fungi) %>%   # the grouping variable
  dplyr::summarise(mean = mean(growth, na.rm=TRUE), sd = sd(growth, na.rm=TRUE), n = n(), SE = sd(growth, na.rm=TRUE)/sqrt(n())) # calculates the standard error of each group

PDA.summary3.df <- as.data.frame(PDA.summary3)
PDA.summary3.df

#make a csv file
pda.cv <- as.data.frame(PDA.summary3)

pda.cv.df.data.for.summary <- "#SelectDestinationPath/pda.summary.csv"
#write.csv(pda.cv, pda.cv.df.data.for.summary)



#plot
PDA.summary2.plot <- ggplot(data = PDA.summary2.df, mapping = aes(x = days,
                                               y = mean,
                                               group = Fungi, shape = Fungi)) + 
  scale_shape_manual(values=c(1:16)) +
  labs(title = "Growth comparison on PDA") +
  geom_line(size = 0.5) + 
  geom_line(data = PDA.summary2.df[which(PDA.summary2.df$Fungi == "X. necrophora"),], col = "#ff5a32", size = 0.5) +
  geom_point(col = "black", size=3)+
  ylim(0,NA) +
  labs(y = "Mycelial growth (mm) \nmean of radial measurements\n",
       x = "Time (Days)") +
  geom_hline(yintercept = 1, linetype = "dashed")+
  theme_cowplot() + 
  theme(legend.text = element_text(face = "italic"),
        legend.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5)); PDA.summary2.plot


#geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), width=0.2) +


#dual assay figure
dualassay.second <- read.csv("dualtwojune2021.csv")

str(dualassay.second)

plotNormalHistogram(dualassay.second$inhibition)


#erase negative
dual.nonegative <- dualassay.second %>% 
  filter(inhibition >= 0)
dual.nonegative


#get days
day10 <-dual.nonegative[which(dual.nonegative$day %in% c('day10')),]
#twodays <- dual.nonegative[which(dual.nonegative$day %in% c('day5', 'day10')),]

#stats
plotNormalHistogram(twodays$inhibition)
shapiro.test(twodays$inhibition)

#tra
dual.t <- transformTukey(day10$inhibition)

#add transformed column to df
dual.nonegative.dataframe <- as.data.frame(day10)
day10.t <- cbind(dual.nonegative.dataframe, dual.t)
head(day10.t)
str(day10.t)

#stats+ rep + tech_rep
#to conduct tukey HSD we need to conduct a lm first
day10.t.lm <- lm(inhibition ~ fungus, data = day10.t)
summary(day10.t.lm)

#test normality
ks.test(day10.t.lm$residuals, 'pnorm', mean(day10.t.lm$residuals), sd(day10.t.lm$residuals))

hist(day10.t.lm$residuals,
     xlab='Residuals',
     main='Histogram of Residuals with Overlaying Normal Curve',
     col="grey",
     prob=TRUE)
curve(dnorm(x, mean=mean(day10.t.lm$residuals),
            sd=sd(day10.t.lm$residuals)), col='blue', add=TRUE)


ols_plot_resid_qq(day10.t.lm)
ols_test_normality(days2.t.lm)
ols_plot_resid_fit(day10.t.lm)
ols_plot_resid_hist(day10.t.lm)

#levene
with(day10.t, leveneTest(y=inhibition, group=fungus, center = mean))


#ANOVA
day10.t.av <- aov(day10.t.lm)
summary(day10.t.lm)


#redisuals
day10.t.av.residuals <- residuals(object = day10.t.av)
hist( x = day10.t.av.residuals)
ks.test(day10.t.av.residuals, "pnorm", mean(day10.t.av.residuals), sd(day10.t.av.residuals))
#if p-value is greater than 0.05, it suggests that the error terms are normally distributed
#Hence each group was sampled from normally distributed population.


#tukey #to check the differences
day10.t.posthoc <- TukeyHSD(day10.t.av)
day10.t.posthoc
#plot(TukeyHSD(disk.june.extracts.av, conf.level = 0.95),las=3, col = "red")


#make a csv file
cv.dual.posthoc <- as.data.frame(day10.t.posthoc$fungus)

posthoc.dual.df.data.for.summary <- "#SelectDestinationPath/10.posthoc.dual.summary.csv"
#write.csv(cv.dual.posthoc, posthoc.dual.df.data.for.summary)


#for groups
tukey.day10.t <- HSD.test(day10.t.av, trt = 'fungus')
tukey.day10.t
#dput(as.character(tukey.chl.pronase.extracts$groups[[2]])) #get significance letters
tukey.day.day10.t <- HSD.test(day10.t.av, trt = 'day')
tukey.day.day10.t


tukey.day10.t.df <- as.data.frame(tukey.day10.t$groups)
tukey.day10.t.df

tukey.day.day10.t.df <- as.data.frame(tukey.day.day10.t$groups)
tukey.day.day10.t.df

tukey.day10.t.data.for.letters <- "#SelectDestinationPath/dualassay.tukey.day10.onewaydays.t.tukey.csv"
#write.csv(tukey.day10.t.df, tukey.day10.t.data.for.letters)

tukey.days.t.data.for.letters <- "#SelectDestinationPath/dualassay.tukey.threedays.only.t.tukey.csv"
#write.csv(tukey.day.day10.t.df, tukey.days.t.data.for.letters)

tukey.day10.t.data.for.letters.tukeydata <- read.csv("#SelectDestinationPath/dualassay.tukey.threedays.t.tukey.csv")
as.data.frame(tukey.day10.t.data.for.letters.tukeydata)

df.tukey.day10.t.tukeydata <- tukey.day10.t.data.for.letters.tukeydata[order(tukey.day10.t.data.for.letters.tukeydata$X),]
df.tukey.day10.t.tukeydata

#now get significance letters
dput(as.character(df.tukey.day10.t.tukeydata$groups))
#compare with direct control


#summary day 10
#sumary
dual101 <- day10 %>% # the names of the new data frame and the data frame to be summarized
  dplyr::group_by(fungus) %>%   # the grouping variable
  dplyr::summarise(mean = mean(inhibition, na.rm=TRUE), sd = sd(inhibition, na.rm=TRUE), n = n(), SE = sd(inhibition, na.rm=TRUE)/sqrt(n())) # calculates the standard error of each group
dual101
dual101.df <- as.data.frame(dual101)
head(dual101.df)

#make a csv file
dual.cv <- as.data.frame(dual101)

dual.cv.df.data.for.summary <- "#SelectDestinationPath/dual.threedays.summary.csv"
#write.csv(dual.cv, dual.cv.df.data.for.summary)



#barplot dual assay
#dev.off()
dualplot10 <- ggplot(dual101, aes(x = reorder(fungus, -mean), y = mean, fill=fungus)) + 
  geom_col() + geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), width=0.2) +
  geom_text(label = c("fg", "ab", "def", "abc", "abcd", "efg", "cdef", "a", "fg", 
                       "cdef", "a", "bcde", "efg", "g", "abcd"), 
  aes(y = mean+SE+0.5, x = reorder(fungus, -mean)),vjust = -0.5, 
size = 5); dualplot10



alldual10 <- dualplot10 + labs(title="Growth Inhibition caused by X. necrophora", 
                              x="Fungi", y = "% Inhibition") +
  scale_fill_manual(values=c("#999999", "#999999","#999999","#999999","#999999","#999999","#999999",
                             "#999999","#999999","#999999","#999999","#999999","#999999","#999999",
                             "#999999"))+
  scale_y_continuous(guide = "prism_minor",
                     limits = c(0, 80),
                     expand = c(0, 0),
                     minor_breaks = seq(0, 80, 5)) + #this helps to land the bars on the x axis
  theme_classic() + theme_prism (border = F,
                                 axis_text_angle = 45) + 
  theme(legend.position = "none"); alldual10









#extracts assay figure found dcm
#Extracts <-read.csv("radiaL_growthbydayE1.csv")
#str(Extracts)

dcm <- read.csv("DCMExtractsfile.csv")
str(dcm)

#stats
plotNormalHistogram(dcm$inh)
shapiro.test(dcm$inh)

#tra
dcm.t <- transformTukey(dcm$inh)

#add transformed column to df
dcm.dataframe <- as.data.frame(dcm)
dcm.df.t <- cbind(dcm.dataframe, dcm.t)
head(dcm.df.t)

#stats
#to conduct tukey HSD we need to conduct a lm first
dcm.df.t.lm <- lm(inh ~ Extract + rep, data = dcm.df.t)
summary(dcm.df.t.lm)


#ANOVA
dcm.df.t.av <- aov(dcm.df.t.lm)
summary(dcm.df.t.av)


#residuals
dcm.df.t.av.residuals <- residuals(object = dcm.df.t.av)
hist( x = dcm.df.t.av.residuals)
ks.test(dcm.df.t.av.residuals, "pnorm", mean(dcm.df.t.av.residuals), sd(dcm.df.t.av.residuals))
#if p-value is greater than 0.05, it suggests that the error terms are normally distributed
#Hence each group was sampled from normally distributed population.


#tukey #to check the differences
dcm.df.t.posthoc <- TukeyHSD(dcm.df.t.av)
dcm.df.t.posthoc

#make a csv file
cv.dcm.posthoc <- as.data.frame(dcm.df.t.posthoc$Extract)

posthoc.dcm.df.data.for.summary <- "#SelectDestinationPath/posthoc.dcm.summary.csv"
#write.csv(cv.dcm.posthoc, posthoc.dcm.df.data.for.summary)


#for groups
tukey.dcm.df.t <- HSD.test(dcm.df.t.av, trt = 'Extract')
tukey.dcm.df.t

tukey.dcm <- as.data.frame(tukey.dcm.df.t$groups)
tukey.dcm

tukey.dcm.for.letters <- "#SelectDestinationPath/dcm.tukey.t.tukey.csv"
write.csv(tukey.dcm, tukey.dcm.for.letters)

tukey.dcm.tukeydata <- read.csv("#SelectDestinationPath/dcm.tukey.t.tukey.csv")
as.data.frame(tukey.dcm.tukeydata)

df.tukey.dcm.tukeydata <- tukey.dcm.tukeydata[order(tukey.dcm.tukeydata$X),]
df.tukey.dcm.tukeydata

#now get significance letters
dput(as.character(df.tukey.dcm.tukeydata$groups))

#summary
ext <- dcm %>% # the names of the new data frame and the data frame to be summarized
  dplyr::group_by(Extract, Treatment) %>%   # the grouping variable
  dplyr::summarise(meanext = mean(inh, na.rm=TRUE), sdext = sd(inh, na.rm=TRUE), n = n(), SEext = sd(inh, na.rm=TRUE)/sqrt(n())) # calculates the standard error of each group

ext

#make a csv file
dcm.cv <- as.data.frame(ext)

dcm.cv.df.data.for.summary <- "#SelectDestinationPath/dcm.summary.csv"
#write.csv(dcm.cv, dcm.cv.df.data.for.summary)

#plot
ext1 <- ggplot(ext, aes(x = reorder(Extract, -meanext), y = meanext, fill=Treatment)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = meanext - SEext, ymax = meanext + SEext), width=0.2)+
  geom_text(label = c("b", "b", "b", "b", "b", "a", "b", "b", "b", "b", "b", "b"), 
            aes(y = meanext+SEext+0.5, x = reorder(Extract, -meanext)),vjust = -0.5, 
            size = 5)

ext_dcm <- ext1 + labs(title="",x="Treatments", y = "Mycelial growth inhibition %") +
  scale_fill_manual(values=c('gray', 'gray42'))+
  scale_y_continuous(guide = "prism_minor",
                     limits = c(0, 40),
                     expand = c(0, 0),
                     minor_breaks = seq(0, 40, 5)) + #this helps to land the bars on the x axis
  theme_classic() + theme_prism (border = F,
                                 axis_text_angle = 45) + 
  theme(legend.title = element_text(size = 12)); ext_dcm





#protoplast figure
tran <- read.csv("transformation.csv")
str(tran)
summary(tran)
describe(tran)
describeBy(tran$amount, tran$hours, na.rm = T)
head(tran)


#summary
tran.total.per.hour <- tran %>% # the names of the new data frame and the data frame to be summarized
  dplyr::group_by(hours) %>%   # the grouping variable
  dplyr::summarise(total=sum(formula))
tran.total.per.hour


tran.df <-data.frame(tran.total.per.hour)
tran.df

#make a csv file
proto.cv <- as.data.frame(tran.total.per.hour)

proto.cv.df.data.for.summary <- "#SelectDestinationPath/protoplast.transformation.summary.csv"
#write.csv(proto.cv, proto.cv.df.data.for.summary)

#plot
#barplot
tran.plot.1 <- ggplot(tran.total.per.hour, aes(x = reorder(hours, -total), y= total, fill=hours)) + 
  geom_col(width = 0.5) + scale_y_continuous(limits = c(0, 60000000), expand = c(0, 0)); tran.plot.1


tran.plot.2 <- tran.plot.1 + labs(title="Protoplasm production at two hours of incubation", 
                                  x="Digestion time (h)", y = "Protoplasm count." ~mL^-1) +
  scale_fill_manual(values=c('gray', "gray42"))+
  theme_classic() + theme_prism (border = F,
                                 axis_text_angle = 0) + 
  theme(legend.title = element_text(size = 12)); tran.plot.2

#End of code















