library(lsmeans)

dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
rm(dir)

lap.inc <- read.csv("LapIncWidth.csv")

attach(lap.inc)

lap.interaction.temp <- lm(Growthmm ~ AveIncLap*Temp)
anova(lap.interaction.temp)

lap.interaction.ration <- lm(lap.inc$Growthmm ~ lap.inc$AveIncLap*lap.inc$Ration)
anova(lap.interaction.ration)


sag.inc <- read.csv("SagIncWidth.csv")

sag.interaction.temp <- lm(sag.inc$Growthmm ~ sag.inc$AveSagInc*sag.inc$Temp)
anova(sag.interaction.temp)

sag.interaction.ration <- lm(sag.inc$Growthmm ~ sag.inc$AveSagInc*sag.inc$Ration)
anova(sag.interaction.ration)


lap.interaction.temp$coefficients

lap <-lm(Growthmm ~ AveIncLap)
anova(lap)

sag <- lm(sag.inc$Growthmm ~ sag.inc$AveSagInc)
anova(sag)
