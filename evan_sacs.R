library(tidyverse)
library(ggplot2)
library(readr)
library(ggpubr)
library(emmeans)
library(lme4)




# Check your y variable. A linear model assumes
# normal distribution of y
hist(metaplats_ids.diam$norm.work.len.measure) # that's pretty far off. try taking log y
metaplats_ids.diam$log_norm.work.len.measure <- log(metaplats_ids.diam$norm.work.len.measure) 
hist(metaplats_ids.diam$log_norm.work.len.measure) # not great, but much better. Of course, now you have many -Inf. 
# Not sure if log(normalized work) is the correct way forward. Proceed with regular y!


mod1 <- lm(mJ_per_length ~ plat.pH.mean, metaplats_ids.diam)
mod2 <- lm(mJ_per_length ~ plat.pH.mean + species, metaplats_ids.diam)
mod3 <- lm(mJ_per_length ~ plat.pH.mean * species, metaplats_ids.diam)
mod4 <- lm(mJ_per_length ~ (plat.pH.mean + I(plat.pH.mean^2)) * species, metaplats_ids.diam)
mod5 <- lm(mJ_per_length ~ (plat.pH.mean + I(plat.pH.mean^2) + I(plat.pH.mean^3)) * species, metaplats_ids.diam)
# modX - try sigmoid model here?

AIC(mod1, mod2, mod3, mod4, mod5)
# Given your data, models 4 and 5 are exactly the same



ggplot(data.frame(x = c(6, 6.45)), aes(x = x)) + 
  #stat_function(fun = quad.func.am, linewidth = 2) +
  stat_function(fun = quad.func.am.manual, linewidth = 2) +
  #geom_line(data = newdata, aes(x = plat.pH.mean, y = fit)) +
  #stat_function(fun = quad.func.am.manual_upr, linewidth = 0.5) 
  #stat_function(fun = quad.func.triv, linewidth = 2) +
  geom_hline(yintercept = 7.121400e-02) + 
  geom_hline(yintercept = 1.059670e-01)

ggplot(data = newdata, aes(x = plat.pH.mean, color = species)) +
  geom_line(y = fit) +  # predicted line
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species), alpha = 0.3, color = NA) +  # confidence band
  theme_minimal() +
  labs(
    y = "mJ per length",
    x = "pH",
    title = "Quadratic fit with 95% confidence bands"
  )


# You can use either mod4 or 5, doesn't really matter
# apply the confidence intervals to the main df
metaplats_ids.diam.mod4 <- cbind(metaplats_ids.diam, predict(mod4, metaplats_ids.diam, interval = 'confidence'))

# for pH error bars use metaplats_ids.diam$plat.pH.sd


mod4.mix <- lmer(mJ_per_length ~ (plat.pH.mean + I(plat.pH.mean^2)) * species + (1|indvd), metaplats_ids.diam)



#####################     try log_norm.work    ##################
ggplot(data = metaplats_ids.diam,
       aes(x = plat.pH.mean,
           y = mJ_per_length,
           #y = mJ,
           #y = norm.work,
           shape = species,
           linetype = species)) +
  scale_shape_manual(values=c(1, 6)) +
  geom_point(size = 3.5) +
  geom_smooth(method = "lm", 
              formula = y ~ (x + I(x^2)),
              color = '#555555') +
  geom_hline(yintercept = 0.071) +
  geom_hline(yintercept = 0.1055) +
  annotate("rect", xmin = metaplats_ids.diam$plat.pH.mean - metaplats_ids.diam$plat.pH.sd,
           xmax = metaplats_ids.diam$plat.pH.mean + metaplats_ids.diam$plat.pH.sd,
           ymin = -0.01, ymax = -0.008, fill = "black") +
  ylim(-0.01, 0.17) +
  xlim(5.98, 6.5) +
  labs(y = "mJ / starting air-sac length", x = "pH") +
  theme_classic() +
  theme(axis.ticks.length=unit(-0.1, "cm")) +
  theme(legend.position = c(0.2, 0.8))




#setwd("C:/Users/evanm/Documents/student_documents/UBC/Research/pH_stat/4mM buffer, for final pH step analysis")
#setwd("C:/Users/evanm/Documents/student_documents/UBC/Research/pH_stat")

ggsave("pH-stat mJ per starting air-sac length.png", 
       units = c("cm"), 
       width = 14, height = 14,
       path = "~/student_documents\\UBC\\Research\\Writing, talks, notes\\chapters\\whole, chapter files\\Chapter 4\\whole chapter versions")
       #path = "~/student_documents\\UBC\\Research\\Writing, talks, notes\\chapters\\whole, chapter files\\Chapter 4")



##
## playing with select and filter to get simple experimental design stuff ##
##

get.n.triv <- metaplats_ids.diam %>%
  select(species, indvd, sac) %>%
  filter(species == "trivittatus") %>%
  unique()



# stats

anova(mod4)
summary(mod4)
emtrends(mod4, ~ species, var="plat.pH.mean")
sjPlot::plot_model(mod4)

emmeans(mod4, "plat.pH.mean", by = "species", data = metaplats_ids.diam) #maybe makes sense?
emmeans(mod4, list(pairwise ~ species), adjust = "tukey") #I think this is a comparison over the whole pH range...

##  This works!
str(metaplats_ids.diam)
mod4.aov <- aov(mJ_per_length ~ (as.factor(plat.pH.mean) + I(plat.pH.mean^2)) * species, metaplats_ids.diam)
summary(mod4.aov)
TukeyHSD(mod4.aov)


str
aov.mod <- aov(norm.work ~ (as.factor(pH) + I(pH^2)) * species, sac)
TukeyHSD(aov.mod) #this works with the old stuff...

my_equation.am <- function(x) ({x*-0.40293 + (x^2)*0.03443}) +1.178
ggplot(data.frame(x = c(6, 6.45)), aes(x = x)) + 
  stat_function(fun = my_equation.am, linewidth = 2)

###########  model brought in from edulis R for plotting  ########### 

# equation for a given x here in the edulis plot, y scale = range for pH stat and also hlines in edulis work plot
#(x*(-2.635*(10^-6)) + (x^2)*(-4.974*(10^-7)) + 4.59*(10^-5)
#define equation. abs value for positive work done
my_equation <- function(x) ((abs({x*-2.724e-06 + (x^2)*-5.155e-07 + 5.500e-05}))*1000) -0.6835475
my_equation.upr <- function(x)((abs({x*(-2.724e-06 + 5.040e-06) + (x^2)*(-5.155e-07 + 5.728e-08) + (5.500e-05 + 1.261e-04)}))*1000) -0.6835475 # +se
 ##  * 1000 for mJ, -0.6835475 so set interval start as zero (work done starting from pH-stat end point)


#plot equation
ggplot(data.frame(x = c(35.30079, 52.0624)), aes(x = x)) + 
  stat_function(fun = my_equation, linewidth = 2) +
  stat_function(fun = my_equation.upr, linewidth = 1, linetype = 2) +
  stat_function(fun = my_equation.lwr, linewidth = 1, linetype = 2)

# Define original and new x ranges
x_old_min <- 35.30079
x_old_max <- 52.0624
x_new_min <- 6.00
x_new_max <- 6.45

# Compute scaling coefficients
b <- (x_new_max - x_new_min) / (x_old_max - x_old_min)
a <- x_new_min - b * x_old_min

# Print values to check
a
b

# modified function to keep the same y values but show on the pH stat x range
ggplot(data.frame(x = c(6.00, 6.45)), aes(x = x)) + 
  stat_function(fun = function(x) my_equation((x - a)/ b), linewidth = 2) +
  stat_function(fun = function(x) my_equation.upr((x - a)/ b), linewidth = 1, linetype = 2) +
  stat_function(fun = function(x) my_equation.lwr((x - a)/ b), linewidth = 1, linetype = 2)



###################################################################################################

# ranges modified for showing with edulis data ((for exporting to illustrator))
ggplot(data = metaplats_ids.diam,
       aes(x = plat.pH.mean,
           y = mJ_per_length,
           #y = mJ,
           #y = norm.work,
           shape = species,
           linetype = species)) +
  scale_shape_manual(values=c(1, 6)) +
  geom_point(size = 3.5) +
  geom_smooth(method = "lm", 
              formula = y ~ (x + I(x^2)),
              color = '#555555') +
  stat_function(fun = function(x) my_equation((x - a)/ b), ## edulis work, SE below
                linewidth = 2, colour = "#619CFF") + 
  stat_function(fun = function(x) my_equation.upr((x - a)/ b), 
                linewidth = 1, linetype = 2, colour = "#619CFF") +
  stat_function(fun = function(x) my_equation.lwr((x - a)/ b), 
                linewidth = 1, linetype = 2, colour = "#619CFF") +
  labs(y = "mJ / starting air-sac length", x = "pH") +
  #geom_hline(yintercept = 0.8005299) + # upper bound for edulis (difference between lower and upper bounds on edulis plot restricted to pH stat scale)
  # xlim(6.0, 6.45) +
  theme_classic() +
                                                                                                                                                                                                                                                                         
  theme(legend.position = c(0.2, 0.8))

#setwd("C:/Users/evanm/Documents/student_documents/UBC/Research/pH_stat/4mM buffer, for final pH step analysis")
#setwd("C:/Users/evanm/Documents/student_documents/UBC/Research/pH_stat")

ggsave("pH-stat mJ per starting air-sac length, edulis y-scale.pdf", 
       units = c("cm"), 
       width = 14, height = 14,
       #path = "~/student_documents\\UBC\\Research\\Writing, talks, notes\\beer talk\\2024, March")
       path = "~/student_documents\\UBC\\Research\\Writing, talks, notes\\chapters\\whole, chapter files\\Chapter 4/whole chapter versions")
