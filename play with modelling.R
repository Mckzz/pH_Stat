

#################################################################################
############                         ############
############        modeling         ############
#################################################################################

hist(metaplats_ids.diam$sac.work)




# with just straight lines and all data assumed to be linear
ggplot(metaplats_ids.diam, 
       aes(y = mJ_per_length, x = plat.pH.mean, 
           group = species, shape = species))  + 
  #geom_smooth(data = metaplats_ids.diam, method = "lm", formula = sac.work ~ plat.pH.mean + (sac.work | species)) +
  geom_smooth(method = "lm", se = T, span = 0.8) +
  #geom_line() + 
  geom_point()

#try logging y


###############              ###############

smaller.data.frame <- metaplats_ids.diam %>%
  ungroup() %>%
  select(species, plat.pH.mean, sac.work, norm.work.len.measure) %>%
  rename(pH = plat.pH.mean) %>%
  rename(y = sac.work)


print(smaller.data.frame, n= 75)    


write_csv(smaller.data.frame,
          "~/student_documents/UBC/Research/pH_stat/smaller.data.csv")


#############               #################


sac <- read.csv("C:\\Users\\evanm\\Documents\\student_documents\\UBC\\Research\\pH_stat\\smaller.data.csv")
sac$species <- as.factor(sac$species)
sac$pH <- as.numeric(sac$pH)

str(sac)


library(lmerTest)
mod1 <- lm(y ~ pH, sac)
mod2 <- lmer(y ~ pH + (species | pH), sac)

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

# add pH sd column from metaplats_ids.diam
sac$pH.sd <- metaplats_ids.diam$plat.pH.sd

ggplot(data = sac,
       aes(x = pH,
           y = y,
           shape = species)) +
  scale_shape_manual(values=c(1, 6)) +
  geom_jitter(size = 4, width = 0.01) +
  #geom_abline(slope = fit_lm$coefficients[2], intercept = fit_lm$coefficients[1]) +
  #geom_abline(slope = ranef(mod2)$pH[[2]], intercept = ranef(mod2)$pH[[1]]) +
  geom_line(aes(x = pH, y = fit), show.legend = FALSE) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3, show.legend = FALSE) +
  annotate("rect",
           xmin = sac$pH - sac$pH.sd,
           xmax = sac$pH + sac$pH.sd,
           ymin = -40, ymax = -35, fill = "black") +
  theme_classic() +
  theme(axis.ticks.length=unit(-0.1, "cm"))


# stats
# anova(mod2)
# summary(mod2)

# compare the two species as well
# mod_am <- lm(y ~ pH, sac[sac$species == "americanus", ])
# mod_tr <- lm(y ~ pH, sac[sac$species == "trivittatus", ])

#  make pH a factor for anova
sac <- sac %>%
  mutate(pH = as.factor(pH))

mod3 <- aov(y ~ pH * species, sac)
predict(mod3)
confint(mod3)

summary(mod3)
TukeyHSD(mod3)


## for t test, split up species then do in excel lol
sac.trivt <- sac %>% 
  filter(species == "trivittatus")

write_csv(sac.trivt, 
          "~/student_documents/UBC/Research/pH_stat/sac.trivit.csv")

sac.am <- sac %>% 
  filter(species == "americanus")

write_csv(sac.am, 
          "~/student_documents/UBC/Research/pH_stat/sac.am.csv")

## bootstrapoped 95 CI from a linear mixed effect
## large variance for species indicates that random effect for that is appropriate



## quadratic as evidenced by the AIC shit Sarah P did
ggplot(data = sac,
       aes(x = as.numeric(pH),
           y = y,
           shape = species)) +
  geom_point() +
  geom_smooth(method = "lm", 
              formula = y ~ (x + I(x^2)),
              color = '#555555') +
  #geom_line(aes(x = pH, y = fit), show.legend = FALSE) +
  #geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3, show.legend = FALSE) +
  theme_minimal()

