library(tidyverse)
library(ggplot2)
library(readr)
library(ggpubr)

###############              ###############

smaller.data.frame <- metaplats_ids.diam %>%
  ungroup() %>%
  select(species, pH.fct, sac.work) %>%
  rename(pH = pH.fct) %>%
  rename(y = sac.work)


print(smaller.data.frame, n= 75)    


write_csv(smaller.data.frame,
          "~/student_documents/UBC/Research/pH_stat/smaller.data.csv")


#############               #################


sac <- read.csv("C:\\Users\\evanm\\Documents\\student_documents\\UBC\\Research\\pH_stat\\smaller.data.csv")
sac$species <- as.factor(sac$species)
sac$pH <- as.numeric(sac$pH)

str(sac)

###################################################


# Check your y variable. A linear model assumes
# normal distribution of y
hist(metaplats_ids.diam$norm.work.len.measure) # that's pretty far off. try taking log y
metaplats_ids.diam$log_norm.work.len.measure <- log(metaplats_ids.diam$norm.work.len.measure) 
hist(metaplats_ids.diam$log_norm.work.len.measure) # not great, but much better. Of course, now you have many -Inf. 
# Not sure if log(normalized work) is the correct way forward. Proceed with regular y!


# # normalized work  by starting sac area (taken from main big code doc)
# sac$norm.work <- metaplats_ids.diam$norm.work
# 
# # throws off histogram normal distb test to have all these zeros...
# sac <- sac %>%
#   mutate(log_norm.work = replace(log_norm.work,
#                                   log_norm.work == "-Inf",
#                                   0))
# print(sac)


# filter for only subsequent pHs where work has been done
# sac_after0 <- sac %>%
#   filter(log_norm.work > 0)
# 
# print(sac_after0)
# hist(sac_after0$log_norm.work) #histogram is the same as log_y
# 
# # get histograms for data at the two subsequent pHs separately
# sac_1st_alk <- sac_after0 %>%
#   filter(pH < 6.4)
# print(sac_1st_alk)
# hist(sac_1st_alk$y)
# 
# sac_2nd_alk <- sac_after0 %>%
#   filter(pH > 6.3)
# print(sac_2nd_alk)
# hist(sac_2nd_alk$log_norm.work)
# # log is better for 2nd alk, non log is better for 1s alk... in terms of norm distribution


mod1 <- lm(norm.work.len.measure ~ plat.pH.mean, metaplats_ids.diam)
mod2 <- lm(norm.work.len.measure ~ plat.pH.mean + species, metaplats_ids.diam)
mod3 <- lm(norm.work.len.measure ~ plat.pH.mean * species, metaplats_ids.diam)
mod4 <- lm(norm.work.len.measure ~ (plat.pH.mean + I(plat.pH.mean^2)) * species, metaplats_ids.diam)
mod5 <- lm(norm.work.len.measure ~ (plat.pH.mean + I(plat.pH.mean^2) + I(plat.pH.mean^3)) * species, metaplats_ids.diam)
# modX - try sigmoid model here?

AIC(mod1, mod2, mod3, mod4, mod5)
# Given your data, models 4 and 5 are exactly the same 

###################     try logging these at some point     #####################
# modX - try sigmoid model here?


# You can use either mod4 or 5, doesn't really matter
# apply the confidence intervals to the main df
metaplats_ids.diam <- cbind(metaplats_ids.diam, predict(mod4, metaplats_ids.diam, interval = 'confidence'))

# for pH error bars use metaplats_ids.diam$plat.pH.sd


#####################     try log_norm.work    ##################
ggplot(data = metaplats_ids.diam,
       aes(x = plat.pH.mean,
           y = plat_frac.length.change,
           #y = norm.work,
           shape = species)) +
  scale_shape_manual(values=c(1, 6)) +
  geom_jitter(size = 3.5, width = 0.01) +
  geom_smooth(method = "lm", 
              formula = y ~ (x + I(x^2)),
              color = '#555555') +
  #geom_line(aes(x = pH, y = fit), show.legend = FALSE) +
  #geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3, show.legend = FALSE) +
  # annotate("rect",
  #          xmin = sac$pH - sac$pH.sd,
  #          xmax = sac$pH + sac$pH.sd,
  #          ymin = -30, ymax = -25, fill = "black") +
  #ylab("work (J) per starting sac length") +
  #ylab("Change in length (mm)") +
  ylab("Fractional length change (L/L0)") +
  #labs(title = "using measured lenngths for plateau 1") +
  coord_flip() + # for stress strain, with length change instead of work
  #labs(title = "using linearized area") +
  theme_classic() +
  theme(axis.ticks.length=unit(-0.1, "cm")) +
  theme(legend.position = c(0.8, 0.2))

#setwd("C:/Users/evanm/Documents/student_documents/UBC/Research/pH_stat/4mM buffer, for final pH step analysis")
setwd("C:/Users/evanm/Documents/student_documents/UBC/Research/pH_stat")

ggsave("work with measured lengths for plateau 1.pdf", 
       units = c("cm"), 
       width = 12, height = 12) 

ggsave("pH-stat for combined stress-strain graph.pdf", 
       units = c("cm"), 
       width = 12, height = 12) 



# stats
mod4nw <- lm(norm.work ~ (pH + I(pH^2)) * species, sac)
modxnw <- lm(norm.work.len.measure ~ (pH + I(pH^2)) * species, sac)

anova(modxnw)
summary(mod4)
sjPlot::plot_model(mod4nw)

aov.mod <- aov(norm.work ~ (as.factor(pH) + I(pH^2)) * species, sac)
TukeyHSD(aov.mod)



# If we want to mess around with random effects....
# here's the old code and jk we probably don't want to anyway

library(lmerTest)


# predict function for bootstrapping
predfn <- function(.) {
  predict(., newdata=new, re.form=NULL)
}

# summarise output of bootstrapping
sumBoot <- function(merBoot) {
  return(
    data.frame(fit = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.5, na.rm=TRUE))),
               lwr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE))),
               upr = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
    )
  )
}

# Bootstrapped
new <- sac # the bootMer function uses the df "new" for data
boot <- lme4::bootMer(mod2, predfn, nsim=250, use.u=TRUE, type="parametric")

sac <- cbind(sac, dplyr::bind_cols(sumBoot(boot)))