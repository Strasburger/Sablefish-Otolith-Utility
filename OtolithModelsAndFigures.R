dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
rm(dir)

library(tidyverse)
library(cowplot)
library(viridis)
library(visreg)
library(MuMIn)
library(extrafont)
loadfonts(device = "win")

###Age composition linear model and figure

agecomp <- read.csv("TotalAgeComp.csv")

attach(agecomp)

lm1 <- lm(SagittaAge~LapillusAge-1)
summary(lm1)

lm2 <- lm(SagittaAge~LapillusAge)
summary(lm2)

AICc(lm1, lm2)

detach(agecomp)

p1 <- agecomp %>% ggplot(aes(x=LapillusAge, y=SagittaAge))+
  geom_smooth(method = 'lm', formula = y~x-1, color = "dark gray", lwd=1.5)+
  geom_point(size = 1.5)+
  coord_cartesian(xlim = c(100, 160), ylim = c(100, 160))+
  scale_y_continuous(name="Sagitta age (days)", breaks=seq(100,160,10))+
  scale_x_continuous(name="Lapillus age (days)", breaks=seq(100,160,10))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), text = element_text(size=15))


p2 <- p1+ annotate("text", x = 108, y = 152,
                   label = expression("SDA =" ~ 1.002*"*LPA" + epsilon))

p3 <- p2+annotate("text", x=124, y=152, label = expression(r^2==0.99))

p3

ggsave("AgeComp.tiff", width = 6.5, height = 4, units = "in", dpi = 600);dev.off()

###Lapillus diameter vs. fork length models and figure

lap <- read.csv("LapLength.csv")

lm3 <-lm(ForkLength ~ D, data=lap)
summary(lm3)

lm4 <- glm(ForkLength ~ D, data = lap, family = gaussian(link = log))
summary(lm4)

AICc(lm3, lm4)

p4 <- ggplot(lap, aes(x=D, y=ForkLength))+
  geom_point()+
  geom_smooth(method = 'lm', formula = y~x, color = "dark gray", se = TRUE) +
  geom_point(size = 1.5)+
  ylab("")+
  xlab("")+
  scale_y_continuous(breaks=seq(50,310,40))+
  scale_x_continuous(breaks=seq(300,900,50))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), text = element_text(size=15))

##Sagitta height vs. fork length model and figure

sag <- read.csv("SagittaLength.csv")

lm5 <-lm(ForkLength ~ H, data=sag)
summary(lm5)

p5 <- ggplot(sag, aes(x=H, y=ForkLength))+
  geom_point()+
  geom_smooth(method = 'lm', formula = y~x, color = "dark gray", se = TRUE)+
  geom_point(size = 1.5)+
  scale_y_continuous(breaks=seq(40,460,80))+
  ylab ("")+
  xlab("")+
  scale_x_continuous(breaks=seq(400,2700,200))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), text = element_text(size=15))

plot <- plot_grid(p4, p5, labels = c('Lapillus diameter', 'Sagitta height'), ncol = 1, label_x = c(0.6,0.63), label_y = c(0.41,0.41))

temp1 <- plot_grid(NULL, plot, ncol = 2, rel_widths = c(0.5,6))
fbase <- plot_grid(temp1, NULL, ncol = 1, rel_heights = c(9,1))

ggdraw(fbase) + draw_label("Otolith measurement (μm)", fontfamily = "Times New Roman", x = 0.6, y = 0.13, size = 15)+
  draw_label("Fork length (mm)", fontfamily = "Times New Roman", angle = 90, x = 0.1, y = 0.6, size = 15)+draw_label(expression("FL =" ~ 3.131 + 0.003*"*LapDiam" + epsilon), x = 0.35, y = 0.91, fontfamily = "Times New Roman", size = 10)+
  draw_label(expression(r^2==0.97), x = 0.58, y = 0.915, fontfamily = "Times New Roman", size = 10)+
  draw_label(expression("FL =" ~ -38.89 + 0.16*"*SagHeight" + epsilon), x = 0.35, y = 0.48, fontfamily = "Times New Roman", size = 10)+
  draw_label(expression(r^2==0.97), x = 0.58, y = 0.485, fontfamily = "Times New Roman", size = 10)

ggsave("OtoLength.tiff", width = 6.5, height = 4, units = "in", dpi = 600)

###Temperature/ration models and figures

lap.inc <- read.csv("LapIncWidth.csv")

fit1 <- lm(AveIncLap~poly(Temp, 2):Ration+Ration, data=lap.inc)
summary(fit1)

p6 <- visreg(fit1, "Temp", "Ration", gg=TRUE, ylab = "Increment Width µm", overlay = TRUE)+
  geom_point(aes(color = Ration))+
  scale_color_viridis_d() +
  scale_fill_viridis_d(alpha = 0.5)+
  scale_x_continuous(breaks = c(6.5, 12.7, 16.8, 19.5))+
  scale_y_continuous(breaks = c(2, 3, 4, 5))+
  xlab("")+
  ylab("")+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), text = element_text(size=15),
        legend.position = "none")


sag.inc <- read.csv("SagIncWidth.csv")

fit2 <- lm(AveSagInc~poly(Temp,2):Ration+Ration, data = sag.inc)
summary(fit2)

p7 <- visreg(fit2, "Temp", "Ration", gg=TRUE, ylab = "Increment Width µm", overlay = TRUE)+
  geom_point(aes(color = Ration))+
  scale_color_viridis_d() +
  scale_fill_viridis_d(alpha = 0.5)+
  scale_x_continuous(breaks = c(6.5, 12.7, 16.8, 19.5))+
  ylab("")+
  xlab("")+
  ylim(2,7.5)+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), text = element_text(size=15),
        legend.position = c(0.1, 0.855))

plota <- plot_grid(p6, p7, labels = c('Lapillus', 'Sagitta'), ncol = 1, label_x = 0.6)

temp2 <- plot_grid(NULL, plota, ncol = 2, rel_widths = c(0.5,6))
fbase1 <- plot_grid(temp2, NULL, ncol = 1, rel_heights = c(10,0.6))

ggdraw(fbase1) + draw_label(expression('Temperature ('*~degree*C*')'), fontfamily = "Times New Roman", x = 0.56, y = 0.07, size = 15)+
  draw_label("Otolith increment width (μm)", fontfamily = "Times New Roman", angle = 90, x = 0.1, y = 0.6, size = 15)+
  draw_label(expression(r^2==0.90), x = 0.35, y = 0.965, fontfamily = "Times New Roman", size = 10)+
  draw_label(expression(r^2==0.60), x = 0.35, y = 0.525, fontfamily = "Times New Roman", size = 10)


ggsave("IncWidthTempRation.tiff", width = 6, height = 7, units = "in", dpi = 600)

###Increment vs Growth using slope test results to group temperature and ration
lap.inc1 <- read.csv("LapIncWidth1.csv")

HighTempLap <- filter(lap.inc1, Temp == 19.5)

lm6 <-lm(Growthmm ~ AveIncLap, data=HighTempLap)
summary(lm6)

AllOtherLap <- filter(lap.inc1, Temp != 19.5)

lm7 <- lm(Growthmm ~ AveIncLap, data=AllOtherLap)
summary(lm7)

p8 <- lap.inc1 %>% ggplot(aes(x=AveIncLap, y=Growthmm, color = Temp))+
  geom_point(size = 1.5)+
  geom_smooth(method = 'lm', formula = y~x, se = TRUE, aes(fill = Temp))+
  scale_color_viridis_d() +
  scale_fill_viridis_d(alpha = 0.5)+
  ylab ("")+
  xlab("")+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), text = element_text(size=15))

ggdraw(p8) + draw_label('Mean Lapillus Increment Width (μm)', x = 0.5, y = 0.05, size = 15)+
  draw_label("Mean Daily Growth (FL mm)", angle = 90, x = 0.02, y = 0.6, size = 15)+
  draw_label(expression("19.5 °C Growth mm =" ~ -1.21 + 0.59*"*LapInc" + epsilon), x = 0.46, y = 0.245, fontfamily = "Times New Roman", size = 10)+
  draw_label(expression(r^2==0.76), x = 0.38, y = 0.21, fontfamily = "Times New Roman", size = 10)+
  draw_label(expression("Growth mm =" ~ -0.18 + 0.26*"*LapInc" + epsilon), x = 0.34, y = 0.81, fontfamily = "Times New Roman", size = 10)+
  draw_label(expression(r^2==0.75), x = 0.25, y = 0.775, fontfamily = "Times New Roman", size = 10)


ggsave("TempGrowth.tiff", width = 6, height = 4, units = "in", dpi = 600)

lm8 <-lm(Growthmm ~ AveIncLap, data=lap.inc)
summary(lm8)

p9 <- lap.inc %>% ggplot(aes(x=AveIncLap, y=Growthmm))+
  geom_point(size = 1.5)+
  geom_smooth(method = 'lm', formula = y~x, se = TRUE) +
  scale_color_viridis_d() +
  scale_fill_viridis_d(alpha = 0.5)+
  ylab ("")+
  xlab("")+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), text = element_text(size=15), legend.position = "none")


sag.inc$Temp <- as.factor(sag.inc$Temp)
sag.inc$Ration <- as.factor(sag.inc$Ration)

lm9 <- lm(Growthmm ~ AveSagInc, data = sag.inc)
summary(lm9)

p10 <- sag.inc %>% ggplot(aes(x=AveSagInc, y=Growthmm))+
  geom_point(size = 1.5)+
  geom_smooth(method = 'lm', formula = y~x, se = TRUE) +
  scale_color_viridis_d() +
  scale_fill_viridis_d(alpha = 0.5)+
  ylab ("")+
  xlab("")+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), text = element_text(size=15), legend.position = c(0.5,1.25))

plotb <- plot_grid(p9, p10, labels = c('Lapillus', 'Sagitta'), ncol = 1, label_x = 0.1)

temp4 <- plot_grid(NULL, plotb, ncol = 2, rel_widths = c(0.5,6))
fbase2 <- plot_grid(temp4, NULL, ncol = 1, rel_heights = c(10,0.6))

ggdraw(fbase2) + draw_label('Mean Increment Width (μm)', x = 0.56, y = 0.1, size = 15)+
  draw_label("Mean Daily Growth (FL mm)", angle = 90, x = 0.1, y = 0.6, size = 15)+
  draw_label(expression("Daily Growth =" ~ -0.44 + 0.33*"*IncWidth" + epsilon), x = 0.415, y = 0.91, fontfamily = "Times New Roman", size = 10)+
  draw_label(expression(r^2==0.76), x = 0.265, y = 0.865, fontfamily = "Times New Roman", size = 10)+
  draw_label(expression("Daily Growth =" ~ -0.25 + 0.22*"*IncWidth" + epsilon), x = 0.41, y = 0.44, fontfamily = "Times New Roman", size = 10)+
  draw_label(expression(r^2==0.63), x = 0.26, y = 0.4, fontfamily = "Times New Roman", size = 10)

ggsave("IncWidthGrowth.tiff", width = 6.5, height = 4, units = "in", dpi = 600)

save.image(file="OtolithModelsAndFigures.Rdata")
