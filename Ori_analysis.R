library(tidyverse)
library(svglite)
library(investr)
library(deming)

#import data

CO2_data <- read.csv('MBogdos.csv')

#this is to just the equation displayed on the plot

lr_CO2_all <- function(CO2_data){
  lr_CO2 <- lm(data = CO2_data,RFU~percet)

  eq <- substitute(italic(y) == b ~italic(x) + ~~a*","~~italic(r)^2~"="~r2, 
                 list(a = format(unname(coef(lr_CO2)[1]), digits = 4),
                      b = format(unname(coef(lr_CO2)[2]), digits = 4),
                      r2 = format(summary(lr_CO2)$r.squared, digits = 4)))
  as.character(as.expression(eq));
}
#make graph pretty

themething_big_labs <- theme(axis.line = element_line(size=1), text = element_text(size=15), panel.background=element_rect(fill="white"), axis.ticks = element_line(size=1), axis.text = element_text(size = rel(1.5)), axis.title = element_text(size = rel(1.5)),plot.title = element_text(size=rel(1.75),face="bold"),plot.title.position = "panel")

#dummy data to get fit and predition intervals

CO2_dummy <- data.frame(percet=seq(0,55,by=1))
interval <- as_tibble(predFit(lr_CO2, newdata = CO2_dummy, interval = "confidence",level = 0.997)) %>%
  mutate(percet = CO2_dummy$percet)

CO2_dummy_pred <- data.frame(percet=seq(0,55,by=1))
pred.interv <- as_tibble(predFit(lr_CO2, newdata = CO2_dummy_pred, interval = "prediction",level = 0.997)) %>%
  mutate(percet = CO2_dummy_pred$percet)

#plot simple

CO2_lr <- ggplot(data = CO2_data,mapping = aes(x=percet,y=RFU)) +
  geom_point(size=2) +
  geom_smooth(method = lm,size=1) + 
  geom_errorbar(aes(ymin=RFU-stdev_RFU,ymax=RFU+stdev_RFU),width=0.2,position = position_dodge(0.05)) +
  geom_errorbar(aes(xmin=percet-stdev_percet,xmax=percet-stdev_percet),width=0.2,position = position_dodge(0.05)) +
  geom_text(x = 25,y = 200, label = lr_CO2_all(CO2_data), parse = TRUE) + 
  labs(x=expression(paste(CO[2],"in air (%)")),y="RFU") +
  themething_big_labs

#plot with both CIs

CO2_lr_pred <- ggplot(data = CO2_data,mapping = aes(x=percet,y=RFU)) +
  geom_point(size=2) +
  geom_function(fun = ~predict(lr_CO2,data.frame(percet = .x))) +
  geom_errorbar(aes(ymin=RFU-stdev,ymax=RFU+stdev),width=0.2,position = position_dodge(0.05)) +
  geom_ribbon(data = interval, aes(x= percet, ymin=lwr, ymax=upr), alpha = 0.5, inherit.aes = FALSE, fill="blue") + 
  geom_ribbon(data = pred.interv, aes(x=percet,ymin=lwr, ymax=upr), alpha = 0.25, inherit.aes = FALSE, fill = "green") +
  geom_text(x = 25,y = 200, label = lr_CO2_all(CO2_data), parse = TRUE) + 
  labs(x=expression(paste(CO[2],"in air (%)")),y="RFU") +
  themething_big_labs

#only 4 data points

CO2_4pt <- read.csv("MBogdos2.csv")

lr_CO2_4 <- lm(data = CO2_4pt,RFU ~ percet)
  
CO2_dummy_4 <- data.frame(percet=seq(0,7,by=0.01))
interval <- as_tibble(predFit(lr_CO2_4, newdata = CO2_dummy_4, interval = "confidence",level = 0.95)) %>%
  mutate(percet = CO2_dummy_4$percet)

CO2_dummy_pred_4 <- data.frame(percet=seq(0,7,by=0.01))
pred.interv <- as_tibble(predFit(lr_CO2_4, newdata = CO2_dummy_pred_4, interval = "prediction",level = 0.95)) %>%
  mutate(percet = CO2_dummy_pred_4$percet)

CO2_lr_pred_4 <- ggplot(data = CO2_4pt,mapping = aes(x=percet,y=RFU)) +
  geom_point(size=2) +
  geom_function(fun = ~predict(lr_CO2_4,data.frame(percet = .x))) +
  geom_errorbar(aes(ymin=RFU-stdev,ymax=RFU+stdev),width=0.2,position = position_dodge(0.05)) +
  geom_ribbon(data = interval, aes(x= percet, ymin=lwr, ymax=upr), alpha = 0.5, inherit.aes = FALSE, fill="blue") + 
  geom_ribbon(data = pred.interv, aes(x=percet,ymin=lwr, ymax=upr), alpha = 0.25, inherit.aes = FALSE, fill = "green") +
  labs(x=expression(paste(CO[2]," in air (%)")),y="RFU") +
  themething_big_labs

#Deming regression

CO2_vr <- CO2_data %>%
  mutate(var_ratio = stdev_RFU/stdev_percet)

CO2_dem_par <- deming(RFU ~ percet, data = CO2_vr, xstd = stdev_percet, ystd = stdev_RFU)

CO2_dem_coef <- coef(CO2_dem_par)

CO2_lr_coef <- coef(lr_CO2)

CO2_dummy_deming <- data.frame(percet=seq(0,100,by=1))
interval <- as_tibble(predFit(CO2_dem_par, newdata = CO2_dummy_deming, interval = "confidence",level = 0.95)) %>%
  mutate(percet = CO2_dummy_deming$percet)

CO2_dummy_pred_deming <- data.frame(percet=seq(0,100,by=1))
pred.interv <- as_tibble(predFit(CO2_dem_par, newdata = CO2_dummy_pred_deming, interval = "prediction",level = 0.95)) %>%
  mutate(percet = CO2_dummy_pred_deming$percet)

CO2_deming_test <- ggplot(data = CO2_data) +
  geom_point(mapping = aes(x=percet,y=RFU)) +
  geom_abline(intercept = CO2_dem_coef[1], slope = CO2_dem_coef[2], colour = 'blue') +
  geom_abline(intercept = CO2_lr_coef[1], slope = CO2_lr_coef[2])

#use all individual observations

CO2_all_obs <- read.csv('MBogdos_all.csv')

all_obs_lr <- lm(RFU ~ CO2_percent, data=CO2_all_obs)

CO2_lr_all <- ggplot(data = CO2_all_obs,mapping = aes(x=CO2_percent,y=RFU)) +
  geom_point(size=2) +
  geom_smooth(method = lm,size=1) + 
  labs(x=expression(paste(CO[2],"in air (%)")),y="RFU") +
  themething_big_labs

# get for PhMe2

PhMe2_data <- read.csv('PPhMe2_avg.csv')

PhMe2_lr_CO2 <- lm(data = PhMe2_data,RFU~CO2_pct)

PhMe2_lr_CO2_eq <- function(PhMe2_data){
  
  eq <- substitute(italic(y) == b ~italic(x) + ~~a*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(PhMe2_lr_CO2)[1]), digits = 4),
                        b = format(unname(coef(PhMe2_lr_CO2)[2]), digits = 4),
                        r2 = format(summary(PhMe2_lr_CO2)$r.squared, digits = 4)))
  as.character(as.expression(eq));
}

PhMe2_CO2_dummy <- data.frame(CO2_pct=seq(0,100,by=1))
PhMe2_interval <- as_tibble(predFit(PhMe2_lr_CO2, newdata = PhMe2_CO2_dummy, interval = "confidence",level = 0.95)) %>%
  mutate(CO2_pct = PhMe2_CO2_dummy$CO2_pct)

PhMe2_CO2_dummy_pred <- data.frame(CO2_pct=seq(0,100,by=1))
PhMe2_pred.interv <- as_tibble(predFit(PhMe2_lr_CO2, newdata = PhMe2_CO2_dummy_pred, interval = "prediction",level = 0.95)) %>%
  mutate(CO2_pct = PhMe2_CO2_dummy_pred$CO2_pct)

PhMe2_fig <- ggplot(data = PhMe2_data,mapping = aes(x=CO2_pct,y=RFU)) +
  geom_errorbar(aes(ymin=RFU-RFU_STDEV,ymax=RFU+RFU_STDEV),width=0.2,position = position_dodge(0.05)) +
  geom_ribbon(data = PhMe2_pred.interv, aes(x=CO2_pct,ymin=lwr, ymax=upr), alpha = 0.15, inherit.aes = FALSE, fill = "blue") +
  geom_ribbon(data = PhMe2_interval, aes(x= CO2_pct, ymin=lwr, ymax=upr), alpha = 0.3, inherit.aes = FALSE, fill="blue") + 
  geom_function(fun = ~predict(PhMe2_lr_CO2,data.frame(CO2_pct = .x)), size=1) +
  geom_text(x = 25,y = 600, label = PhMe2_lr_CO2_eq(PhMe2_data), parse = TRUE) + 
  geom_point(size=7)
  
#for figure PPh3

theme_OG_figs <- theme(axis.line = element_line(size=1), panel.background=element_rect(fill="white"), axis.ticks = element_line(size=3), axis.text = element_text(size = 35, face = "bold"), axis.title = element_blank())

CO2_CI_pred_fit_fig <- ggplot(data = CO2_data,mapping = aes(x=percet,y=RFU)) +
  geom_errorbar(aes(ymin=RFU-stdev_RFU,ymax=RFU+stdev_RFU),width=1.25,position = position_dodge(0.05),size=1.25) +
  geom_ribbon(data = pred.interv, aes(x=percet,ymin=lwr, ymax=upr), alpha = 0.15, inherit.aes = FALSE, fill = "blue") +
  geom_ribbon(data = interval, aes(x= percet, ymin=lwr, ymax=upr), alpha = 0.3, inherit.aes = FALSE, fill="blue") + 
  geom_function(fun = ~predict(lr_CO2,data.frame(percet = .x)), size=1) +
  geom_point(size=7) +
  theme_OG_figs

PPh3_fig <- ggplot(data = CO2_data,mapping = aes(x=percet,y=RFU)) +
  geom_errorbar(aes(ymin=RFU-stdev_RFU,ymax=RFU+stdev_RFU),width=1.25,position = position_dodge(0.05),size=1.25) +
  geom_ribbon(data = pred.interv, aes(x=percet,ymin=lwr, ymax=upr), alpha = 0.15, inherit.aes = FALSE, fill = "blue") +
  geom_ribbon(data = interval, aes(x= percet, ymin=lwr, ymax=upr), alpha = 0.3, inherit.aes = FALSE, fill="blue") + 
  geom_function(fun = ~predict(lr_CO2,data.frame(percet = .x)), size=1.25) +
  scale_x_continuous(breaks = c(0,10,20,30,40,50),limits = c(0,55)) +
  scale_y_continuous(breaks = c(30,60,90,120,150),limits = c(0,160)) +
  geom_point(size=7, colour = "#0F26BB") +
  theme_OG_figs

#for figure PPhMe2

PhMe2_fig_bothCI <- ggplot(data = PhMe2_data,mapping = aes(x=CO2_pct,y=RFU)) +
  geom_errorbar(aes(ymin=RFU-RFU_STDEV,ymax=RFU+RFU_STDEV),width=1.25,position = position_dodge(0.05),size=1.25) +
  geom_ribbon(data = PhMe2_pred.interv, aes(x=CO2_pct,ymin=lwr, ymax=upr), alpha = 0.15, inherit.aes = FALSE, fill = "blue") +
  geom_ribbon(data = PhMe2_interval, aes(x= CO2_pct, ymin=lwr, ymax=upr), alpha = 0.3, inherit.aes = FALSE, fill="blue") + 
  geom_function(fun = ~predict(PhMe2_lr_CO2,data.frame(CO2_pct = .x)), size=1) +
  geom_point(size=7) +
  theme_OG_figs

#BODIPY data

BODIPY_data <- read.csv('BODIPY_data.csv')

BODIPY_lr_CO2 <- lm(data = BODIPY_data,RFU~Air_pct)

BODIPY_lr_CO2_eq <- function(BODIPY_data){
  
  eq <- substitute(italic(y) == b ~italic(x) + ~~a*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(BODIPY_lr_CO2)[1]), digits = 4),
                        b = format(unname(coef(BODIPY_lr_CO2)[2]), digits = 4),
                        r2 = format(summary(BODIPY_lr_CO2)$r.squared, digits = 4)))
  as.character(as.expression(eq));
}

BODIPY_dummy <- data.frame(Air_pct=seq(0,55,by=1))
BODIPY_interval <- as_tibble(predFit(BODIPY_lr_CO2, newdata = BODIPY_dummy, interval = "confidence",level = 0.95)) %>%
  mutate(Air_pct = BODIPY_dummy$Air_pct)

BODIPY_dummy_pred <- data.frame(Air_pct=seq(0,55,by=1))
BODIPY_pred.interv <- as_tibble(predFit(BODIPY_lr_CO2, newdata = BODIPY_dummy_pred, interval = "prediction",level = 0.95)) %>%
  mutate(Air_pct = BODIPY_dummy_pred$Air_pct)

BODIPY_fig <- ggplot(data = BODIPY_data,mapping = aes(x=Air_pct,y=RFU)) +
  geom_errorbar(aes(ymin=RFU-RFU_stdev,ymax=RFU+RFU_stdev),width=0.2,position = position_dodge(0.05)) +
  geom_ribbon(data = BODIPY_pred.interv, aes(x=Air_pct,ymin=lwr, ymax=upr), alpha = 0.15, inherit.aes = FALSE, fill = "blue") +
  geom_ribbon(data = BODIPY_interval, aes(x= Air_pct, ymin=lwr, ymax=upr), alpha = 0.3, inherit.aes = FALSE, fill="blue") + 
  geom_function(fun = ~predict(BODIPY_lr_CO2,data.frame(Air_pct = .x)), size=1) +
  geom_text(x = 25,y = 1000, label = BODIPY_lr_CO2_eq(BODIPY_data), parse = TRUE) + 
  geom_point(size=7)

BODIPY_pretty_fig <-ggplot(data = BODIPY_data,mapping = aes(x=Air_pct,y=RFU)) +
  geom_errorbar(aes(ymin=RFU-RFU_stdev,ymax=RFU+RFU_stdev),width=1.25,position = position_dodge(0.05), size = 1.25) +
  geom_ribbon(data = BODIPY_pred.interv, aes(x=Air_pct,ymin=lwr, ymax=upr), alpha = 0.3, inherit.aes = FALSE, fill = "#057B2C") +
  geom_ribbon(data = BODIPY_interval, aes(x= Air_pct, ymin=lwr, ymax=upr), alpha = 0.5, inherit.aes = FALSE, fill="#057B2C") + 
  geom_function(fun = ~predict(BODIPY_lr_CO2,data.frame(Air_pct = .x)), size=1.25) +
  geom_point(size=7,colour="#01EC50") + 
  scale_x_continuous(breaks = c(0,10,20,30,40,50),limits = c(0,55)) +
  scale_y_continuous(breaks = c(5000,10000,15000,20000,25000,30000),limits = c(0,30000)) +
  theme_OG_figs
