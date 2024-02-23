library(tidyverse)
library(ggplot2)
library(readr)
library(ggpubr)


sac <- read.csv("C:\\Users\\evanm\\Documents\\student_documents\\UBC\\Research\\pH_stat\\smaller.data.csv")
sac$species <- as.factor(sac$species)
sac$pH <- as.numeric(sac$pH)

# Check your y variable. A linear model assumes
# normal distribution of y
hist(sac$norm.work) # that's pretty far off. try taking log y
sac$log_norm.work <- log(sac$norm.work) 
hist(sac$log_norm.work) # not great, but much better. Of course, now you have many -Inf. 
# Not sure if log(y) is the correct way forward. Proceed with regular y!

# normalized work  by starting sac area (taken from main big code doc)
sac$norm.work <- metaplats_ids.diam$norm.work

# throws off histogram normal distb test to have all these zeros...
sac <- sac %>%
  mutate(log_norm.work = replace(log_norm.work,
                                  log_norm.work == "-Inf",
                                  0))

print(sac)

# filter for only subsequent pHs where work has been done
sac_after0 <- sac %>%
  filter(log_norm.work > 0)

print(sac_after0)
hist(sac_after0$log_norm.work) #histogram is the same as log_y

# get histograms for data at the two subsequent pHs separately
sac_1st_alk <- sac_after0 %>%
  filter(pH < 6.4)
print(sac_1st_alk)
hist(sac_1st_alk$y)

sac_2nd_alk <- sac_after0 %>%
  filter(pH > 6.3)
print(sac_2nd_alk)
hist(sac_2nd_alk$log_norm.work)
# log is better for 2nd alk, non log is better for 1s alk... in terms of norm distribution


mod1 <- lm(y ~ pH, sac)
mod2 <- lm(y ~ pH + species, sac)
mod3 <- lm(y ~ pH * species, sac)
mod4 <- lm(y ~ (pH + I(pH^2)) * species, sac)
mod5 <- lm(y ~ (pH + I(pH^2) + I(pH^3)) * species, sac)
# modX - try sigmoid model here?

AIC(mod1, mod2, mod3, mod4, mod5)
# Given your data, models 4 and 5 are exactly the same 

###################     Should change "y" to "norm work"     #####################
mod1nw <- lm(norm.work ~ pH, sac)
mod2nw <- lm(norm.work ~ pH + species, sac)
mod3nw <- lm(norm.work ~ pH * species, sac)

mod4nw <- lm(norm.work ~ (pH + I(pH^2)) * species, sac)
mod4log_nw <- lm(log_norm.work ~ (pH + I(pH^2)) * species, sac)

mod5nw <- lm(norm.work ~ (pH + I(pH^2) + I(pH^3)) * species, sac)
# modX - try sigmoid model here?

AIC(mod1nw, mod2nw, mod3nw, mod4nw, mod5nw, mod4log_nw)
# Given your data, models 4 and 5 are exactly the same 

# You can use either mod4 or 5, doesn't really matter
sac <- cbind(sac, predict(mod4, sac, interval = 'confidence'))

# for pH error bars
sac$pH.sd <- metaplats_ids.diam$plat.pH.sd

# stick in linearized area % change (strain)
sac$lin_area.pct.change <- metaplats_ids.diam$lin_area.pct.change

# stick in linearized area % change (strain)
sac$plat.areapct.mean <- metaplats_ids.diam$plat.areapct.mean

#####################     try log_norm.work    ##################
ggplot(data = sac,
       aes(x = pH,
           y = norm.work,
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
  theme_classic() +
  theme(axis.ticks.length=unit(-0.1, "cm"))

# stats
anova(mod4log_nw)
summary(mod4log_nw)
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