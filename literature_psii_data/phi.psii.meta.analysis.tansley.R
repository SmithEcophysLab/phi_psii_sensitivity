# load libraries
library(tidyverse)
library(rockchalk)
library(lme4)
library(lmerTest)
library(emmeans)
library(broom)
library(Hmisc)
library(corrplot)
library(ggpubr)
library(ggsci)
library(Rmisc)
library(agricolae)
library(arsenal)
library(ggpmisc)

#all paths assume 'phi_psii_sensitivity/literature_psii_data' repository is root directory
extracted.psii.data <- read.csv("extracted_psii_t_response_data_v2.csv")
psii.data.nostems.noalgae <- subset(extracted.psii.data, omitted_fig2 == 'no')


#subset data by inidivudal papers
bernacchi.psii.data <- extracted.psii.data[which(extracted.psii.data$paper == "bernacchi"),]
june.psii.data <- extracted.psii.data[which(extracted.psii.data$paper == "june"),]
salvucci.psii.data <- extracted.psii.data[which(extracted.psii.data$paper == "salvucci"),]
dwyer.psii.data <- extracted.psii.data[which(extracted.psii.data$paper == "dwyer"),]
wp.psii.data <- extracted.psii.data[which(extracted.psii.data$paper == "wittman.pfanz"),]
dongsansuk.psii.data <- extracted.psii.data[which(extracted.psii.data$paper == "dongsansuk"),]
janka.psii.data <- extracted.psii.data[which(extracted.psii.data$paper == "janka"),]
wittman.psii.data <- extracted.psii.data[which(extracted.psii.data$paper == "wittman"),]
xu.psii.data <- extracted.psii.data[which(extracted.psii.data$paper == "xu"),]
greer.psii.data <- extracted.psii.data[which(extracted.psii.data$paper == "greer"),]
dongsansuk.neuner.psii.data <- extracted.psii.data[which(extracted.psii.data$paper == "dongsansuk.neuner"),]


#set formula for fitting polynomial curves
formula <- y ~ poly(x, 2, raw = TRUE)


##### supplementary figures #####

#plots from Janka
ggplot(janka.psii.data, aes(meas.temp, phi.psii)) +
  facet_wrap(~id) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  stat_regline_equation(label.y = 0.3, size = 5.5) +
  stat_cor(label.y = 0.25, size = 5.5) +
  labs(x = "Temperature (°C)", y = "\u03A6 PSII") +
  theme_classic(base_size = 26) +
  border() +
  scale_x_continuous(breaks=seq(-10,60,5),
                     minor_breaks=NULL)

#plots from dwyer
ggplot(dwyer.psii.data, aes(meas.temp, phi.psii)) +
  facet_wrap(~id) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula, se = FALSE) +
  stat_poly_eq(aes(label =  paste("atop(",..eq.label.., ",", ..adj.rr.label.., ")")),
               formula = formula, parse = TRUE, size = 5, label.y = "bottom", label.x = "right") +
  labs(x = "Temperature (°C)", y = "\u03A6 PSII") +
  theme_classic(base_size = 24) +
  border() +
  scale_x_continuous(breaks=seq(-10,60,5),
                     minor_breaks=NULL)

#plots from wittman and pfanz
ggplot(wp.psii.data, aes(meas.temp, phi.psii)) +
  facet_wrap(~id) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula, se = FALSE) +
  stat_poly_eq(aes(label =  paste("atop(",..eq.label.., ",", ..adj.rr.label.., ")")),
               formula = formula, parse = TRUE, size = 5, label.y = "bottom", label.x = "right") +
  labs(x = "Temperature (°C)", y = "\u03A6 PSII") +
  theme_classic(base_size = 24) +
  border() +
  scale_x_continuous(breaks=seq(-10,60,5),
                     minor_breaks=NULL)

#plots from dongsansuk
ggplot(dongsansuk.psii.data, aes(meas.temp, phi.psii)) +
  facet_wrap(~id) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula, se = FALSE) +
  stat_poly_eq(aes(label =  paste("atop(",..eq.label.., ",", ..adj.rr.label.., ")")),
               formula = formula, parse = TRUE, size = 5, label.y = "bottom", label.x = "middle") +
  labs(x = "Temperature (°C)", y = "\u03A6 PSII") +
  theme_classic(base_size = 24) +
  border() +
  scale_x_continuous(breaks=seq(-10,60,5),
                     minor_breaks=NULL)

#plots from bernacchi
ggplot(bernacchi.psii.data, aes(meas.temp, phi.psii)) +
  facet_wrap(~id) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula, se = FALSE) +
  stat_poly_eq(aes(label =  paste("atop(",..eq.label.., ",", ..adj.rr.label.., ")")),
               formula = formula, parse = TRUE, size = 5, label.y = "bottom", label.x = "right") +
  labs(x = "Temperature (°C)", y = "\u03A6 PSII") +
  theme_classic(base_size = 24) +
  border() +
  scale_x_continuous(breaks=seq(-10,60,5),
                     minor_breaks=NULL)

#plots from june
ggplot(june.psii.data, aes(meas.temp, phi.psii)) +
  facet_wrap(~id) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula, se = FALSE) +
  stat_poly_eq(aes(label =  paste("atop(",..eq.label.., ",", ..adj.rr.label.., ")")),
               formula = formula, parse = TRUE, size = 5, label.y = "bottom", label.x = "right") +
  labs(x = "Temperature (°C)", y = "\u03A6 PSII") +
  theme_classic(base_size = 24) +
  border() +
  scale_x_continuous(breaks=seq(-10,60,5),
                     minor_breaks=NULL)

#plots from salvucci
ggplot(salvucci.psii.data, aes(meas.temp, phi.psii)) +
  facet_wrap(~id) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula, se = FALSE) +
  stat_poly_eq(aes(label =  paste("atop(",..eq.label.., ",", ..adj.rr.label.., ")")),
               formula = formula, parse = TRUE, size = 5, label.y = "bottom", label.x = "left") +
  labs(x = "Temperature (°C)", y = "\u03A6 PSII") +
  theme_classic(base_size = 24) +
  border() +
  scale_x_continuous(breaks=seq(-10,60,5),
                     minor_breaks=NULL)

#plots from wittman
ggplot(wittman.psii.data, aes(meas.temp, phi.psii)) +
  facet_wrap(~id) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula, se = FALSE) +
  stat_poly_eq(aes(label =  paste("atop(",..eq.label.., ",", ..adj.rr.label.., ")")),
               formula = formula, parse = TRUE, size = 5, label.y = "bottom", label.x = "left") +
  labs(x = "Temperature (°C)", y = "\u03A6 PSII") +
  theme_classic(base_size = 24) +
  border() +
  scale_x_continuous(breaks=seq(-10,60,5),
                     minor_breaks=NULL)

#plots from xu
ggplot(xu.psii.data, aes(meas.temp, phi.psii)) +
  facet_wrap(~id) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula, se = FALSE) +
  stat_poly_eq(aes(label =  paste("atop(",..eq.label.., ",", ..adj.rr.label.., ")")),
               formula = formula, parse = TRUE, size = 5, label.y = "bottom", label.x = "left") +
  labs(x = "Temperature (°C)", y = "\u03A6 PSII") +
  theme_classic(base_size = 24) +
  border() +
  scale_x_continuous(breaks=seq(-10,60,5),
                     minor_breaks=NULL)

#plots from greer
ggplot(greer.psii.data, aes(meas.temp, phi.psii)) +
  facet_wrap(~id) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula, se = FALSE) +
  stat_poly_eq(aes(label =  paste("atop(",..eq.label.., ",", ..adj.rr.label.., ")")),
               formula = formula, parse = TRUE, size = 5, label.y = "top", label.x = "left") +
  labs(x = "Temperature (°C)", y = "\u03A6 PSII") +
  theme_classic(base_size = 24) +
  border() +
  scale_x_continuous(breaks=seq(-10,60,5),
                     minor_breaks=NULL)

#plots from dongsansuk & neüner
ggplot(dongsansuk.neuner.psii.data, aes(meas.temp, phi.psii)) +
  facet_wrap(~id) +
  geom_point() +
  geom_smooth(method = "lm", formula = formula, se = FALSE) +
  stat_poly_eq(aes(label =  paste("atop(",..eq.label.., ",", ..adj.rr.label.., ")")),
               formula = formula, parse = TRUE, size = 5, label.y = "bottom", label.x = "middle") +
  labs(x = "Temperature (°C)", y = "\u03A6 PSII") +
  theme_classic(base_size = 24) +
  border() +
  scale_x_continuous(breaks=seq(-10,60,5),
                     minor_breaks=NULL)


###### figure 2 #######

#phi PSII relative to Topt scatter plot
ggplot(psii.data.nostems.noalgae, aes(meas.temp, topt.normalised, colour = growth.temp, shape = id, group = id)) +
  geom_point(alpha = 0.8, size = 3) +
  geom_smooth(method = "lm", aes(group = 1), 
              formula = formula, se = TRUE, 
              colour = 'black') +
  geom_line(stat = 'smooth', method = "lm", 
            formula = formula, se = FALSE, 
            alpha = 0.5, linetype = 2) +
  scale_colour_gradient(low = 'dodgerblue1', high = 'firebrick2') +
  scale_shape_manual(values = c(0:18)) +
  labs(x = "Temperature (°C)", 
       y = "\u03A6 PSII (normalised to max value)", 
       colour = "Growth temperature (°C)", 
       shape = "Curve") +
  theme_classic(base_size = 22) +
  theme(legend.text = element_text(size = 12),
        legend.background = element_rect(fill = "transparent")) +
  border() +
  scale_x_continuous(breaks=seq(-10,60,5),
                     minor_breaks=NULL)
