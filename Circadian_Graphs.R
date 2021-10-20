######################################################
### Graphs for Circadian Sleep-Wake Rhythms Across Ages, DS
######################################################
### aklovos@email.arizona.edu | last updated 10/10/2021 ###

# *** NOTE TO USER: Column numbers in the code 
# throughout document have not yet been
# adjusted for the reduced, de-identified data set. If using 
# before the author makes adjustment, please adjust for
# yourself the column numbers in the code you wish to use. ***


# Graphing Cog outcomes
setwd("")
cogs <- read.table("Circadian_Data_Share_9.10.21.xlsx", header=TRUE, sep=",")
cogs <- cogs[c(1:101),c(1:73)]
library(psych)
library(stats)
library(dplyr)
library(ggplot2)
  theme_set(theme_classic()) +
              theme(legend.position = "top")

# Exclude all TD data
cogsds <- subset(cogs, DS.TD=="DS")
# Exclude also all non-AMAP/BRIEF psych data
cogent <- cogsds[,c(1,5,6,8,19,24,29,48:52,55,68)]

#############################################################################
########## Figure: Phase Timing (Offsets) and Total Sleep Time (TST) #######################################
#############################################################################
setwd("/Users/lisle/Desktop/Actigraphy/data-sheets")
cogs <- read.csv("All_Rhythms_Compiled_6.30.21_AKL.csv", header=TRUE, sep=",")
cogs <- cogs[c(1:101),c(1:73)]
# subset the data
phase <- cogs[,c(2,6,7,13)]
# Set up for grouped plots 
library(ggplot2)
theme_set(theme_classic()) +
  theme(legend.position = "right")
# Make plots for Offsets.
offs_boxplot <- ggplot(phase, aes(x=DS.TD, y=Offsets_avg, fill=Agex3)) +
  geom_boxplot() # Use this!
offs_boxplot # Call plot this way
# replicate above for total sleep time (TST)
tst <- cogs[,c(2,6,7,23)]
tst_boxplot <- ggplot(tst, aes(x=DS.TD, y=AvgSlpTime, fill=Agex3)) +
  geom_boxplot()
tst_boxplot

# Exclude all DS data to create boxplots for TDs:
cogstd <-  subset(cogs, DS.TD=="TD")
tst <- cogstd[,c(2,6,23)]
tst_boxplot <- ggplot(tst, aes(x=Agex3, y=AvgSlpTime)) +
  geom_boxplot()
tst_boxplot

#######################################################################
########## Figure: IS, IV by ages for each group ########################
#######################################################################
setwd("/Users/lisle/Desktop/Actigraphy/data-sheets")
cogs <- read.csv("All_Rhythms_Compiled_6.30.21_AKL.csv", header=TRUE, sep=",")
cogs <- cogs[c(1:101),c(1:73)]
cogsds <- subset(cogs, DS.TD=="DS")
cogsds$Agex3 <- factor(cogsds$Agex3, levels = c("Child", "Teens", "Twenties"))
# IS, DS:
dsIS <- ggplot(data=cogsds, mapping = aes(x=Age_Acti, y = IS)) 
dsIS <- dsIS + geom_point(aes(shape=Agex3, color=Agex3)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(y="IS", x="Age", title="IS, Participants with DS") +
  theme_classic() #    # IS signif at p=.00707; mult R2=.4065 (31 obs); NEGATIVE correlation w amap???
dsIS
# IS for 3 TD age Groups:
cogstd <- subset(cogs, DS.TD=="TD")
cogstd$Agex3 <- factor(cogstd$Agex3, levels = c("Child", "Teens", "Twenties"))
tdIS <- ggplot(data=cogstd, mapping = aes(x=Age_Acti, y = IS)) 
tdIS <- tdIS + geom_point(aes(shape=Agex3, color=Agex3)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(y="IS", x="Age", title="IS, Participants with TD") +
  theme_classic() #    
tdIS
# IV, DS:
cogsds$Agex3 <- factor(cogsds$Agex3, levels = c("Child", "Teens", "Twenties"))
dsIV <- ggplot(data=cogsds, mapping = aes(x=Age_Acti, y = IV)) 
dsIV <- dsIV + geom_point(aes(shape=Agex3, color=Agex3)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(y="IV", x="Age", title="IV, Participants with DS") +
  theme_classic() 
dsIV
# IV, TD use this
cogstd$Agex3 <- factor(cogstd$Agex3, levels = c("Child", "Teens", "Twenties"))
tdIV <- ggplot(data=cogstd, mapping = aes(x=Age_Acti, y = IV)) 
tdIV <- tdIV + geom_point(aes(shape=Agex3, color=Agex3)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(y="IV", x="Age", title="IV, Participants with TD") +
  theme_classic() 
tdIV

################################################################
######### Figure: RxnTm by IS, all ages ########################
################################################################
# Exclude also all non-AMAP/BRIEF psych data and incl the Agex3 for graphing
cogentgr <- cogsds[,c(1,5,6,8,19,24,29,48:52,55,68)]

######### Figure: RxnTm by IS, all ages ########################
cog1g <- cogentgr[,c(2:6,14)] 
cog1g <- subset(cog1g, Efalt_Avg_Time<5) # USE THIS. It nicely shows pos corr for kids/teens & neg cor for adults  
cog1g <- na.omit(cog1g)
rxn <- ggplot(data=cog1g, mapping = aes(x=IS, y = Efalt_Avg_Time)) 
rxn <- rxn + geom_point(aes(shape=Agex3, color=Agex3)) +
       scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
       labs(y="Reaction Time", x="IS", title="Reaction Time by Stability") +
       theme_classic() # THis shows that the adults group may have a different pattern + 1 outlier in kids group rm that:
rxn
######### Figure: Verbal Recall AMAP 07 by IS  ##########################################   
cog3 <- cogentgr[,c(2:6,10)]
cog3$log07 <- log(cog3$AMAP_07_LLCorrect)
cog3[cog3 == "-Inf"] <- 0 # worked
cog3 <- na.omit(cog3) # 33 obs
ll07 <- ggplot(data=cog3, mapping = aes(x=IS, y = AMAP_07_LLCorrect)) 
ll07 <-  ll07 + geom_point(aes(shape=Agex3, color=Agex3)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(y="Verbal Recall", x="IS", title="Immediate Verbal Recall by Stability") +
  theme_classic() #    This is good USE THIS MAYBE; shows the neg corr BUT no adult data for this phase
ll07
######### Figure: AMAP 09 Immediate Scene Recall by IS  ####################    
cog7g <- cogentgr[,c(2:6,12)]
cog7g <- na.omit(cog7g)
a09 <- ggplot(data=cog7g, mapping = aes(x=IS, y = AMAP_09_SceneRecScore)) 
a09 <- a09 + geom_point(aes(shape=Agex3, color=Agex3)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(y="Scene Recall", x="IS", title="Immediate Scene Recall by Stability") +
  theme_classic() #    # IS signif at p=.00707; mult R2=.4065 (31 obs); NEGATIVE correlation w amap???

a09

#######################################################################
########## Figure: L5 by 4 ages #######################################
# following what was done in my grouped boxplots for offsets
setwd("/Users/lisle/Desktop/Actigraphy/data-sheets")
cogs <- read.csv("All_Rhythms_Compiled_6.30.21_AKL.csv", header=TRUE, sep=",")
cogs <- cogs[c(1:101),c(1:73)]
# subset the data
amp <- cogs[,c(2,6,7,16,17,25)]
# Set up for grouped plots 
library(ggplot2)
theme_set(theme_classic()) +
  theme(legend.position = "right")
# Exclude TD teens outlier from graph
amp <- subset(amp, L5<60)
# Make plots for Offsets.
L5_boxplot <- ggplot(amp, aes(x=DS.TD, y=L5, fill=Agex3)) +
  geom_boxplot() # Use this!
L5_boxplot # Call plot this way
############################################
# replicate above for M10 and then WASO
amp <- cogs[,c(2,6,7,16,17,25)]
M10_boxplot <- ggplot(amp, aes(x=DS.TD, y=M10, fill=Agex3)) +
  geom_boxplot()
M10_boxplot
############################################
# replicate above for WASO
amp <- cogs[,c(2,6,7,16,17,25)]
WASO_boxplot <- ggplot(amp, aes(x=DS.TD, y=WASOavgmin, fill=Agex3)) +
  geom_boxplot()
WASO_boxplot
