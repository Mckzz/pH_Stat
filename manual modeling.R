

# # Create a grid of pH values and species combinations for conf intervals
# newdata <- expand.grid(
#   plat.pH.mean = seq(min(metaplats_ids.diam$plat.pH.mean), max(metaplats_ids.diam$plat.pH.mean), length.out = 200),
#   species = levels(metaplats_ids.diam$species)) %>%
#   as.data.frame() %>%
#   print()
# 
# 
# preds <- predict(mod4, newdata = newdata, interval = "confidence")
# 
# predic.data <- cbind(newdata, preds) %>%
#   print()

###################
# Create a grid of pH values and species combinations for conf intervals
# continuous new data so you don't just get broken sticks
newdata <- expand.grid(
  plat.pH.mean = seq(min(metaplats_ids.diam$plat.pH.mean), max(metaplats_ids.diam$plat.pH.mean), length.out = 200),
  species = unique(metaplats_ids.diam$species))

# Add the squared term manually for consistency with the model
newdata$`I(x^2)` <- newdata$plat.pH.mean^2

preds <- predict(mod4, newdata = newdata, interval = "confidence")
preds_df <- cbind(newdata, preds)

ggplot(preds_df, aes(x = plat.pH.mean, y = fit, color = species, fill = species)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +
  theme(legend.position = c(0.2, 0.8)) #+
  # ylim(-0.01, 0.17) +
  # xlim(5.98, 6.5) 

# add modelled data with conf intervals from edulis
# see pH-steps-pressure project (combined) for how this was made
newdata.ed.pHstat.match <- 
  read_csv("~/student_documents/UBC/Research/Malawi/data/sac pressure, pH series (6, 7, 8)/newdata.ed.pHstat.match.csv") %>%
  mutate(species = "edulis") %>%
  # mutate(se_from.upr = pos.upr.zeroed + pos.fit.zeroed) %>% # addition / subtraction reversed, I think because of reflection in orig project
  # mutate(se_from.lwr = pos.lwr.zeroed - pos.fit.zeroed) # both just to check
  mutate(se_calc = (pos.lwr.zeroed - pos.upr.zeroed) /2) 
  

####  plot all together  ####
ggplot() +
  geom_line(data = preds_df, aes(x = plat.pH.mean, y = fit, color = species), size = 1) +
  geom_ribbon(data = preds_df, aes(x = plat.pH.mean, y = fit, ymin = lwr, ymax = upr, 
                  color = species, fill = species), 
              alpha = 0.2, colour = NA) +
  geom_line(data = newdata.ed.pHstat.match, 
            aes(x = match.depth, y = pos.fit.zeroed,
                colour = species),
            size = 1) + # edulis central line brought in
  geom_ribbon(data = newdata.ed.pHstat.match, 
              aes(x = match.depth, y = pos.fit.zeroed, 
                  ymin = pos.lwr.zeroed, ymax = pos.upr.zeroed,
                  colour = species, fill = species), 
              colour = NA, alpha = 0.2) +
  theme_classic() +                                     
  theme(legend.position = c(0.2, 0.8)) +                      
  theme(axis.ticks.length=unit(-0.1, "cm")) +
  labs(x = "pH", y = "mJ / starting air-sac length (m)")

# ggsave("plot all together, model predic lines, pH x.pdf", 
#        units = c("cm"), 
#        width = 14, height = 14,
#        #path = "~/student_documents\\UBC\\Research\\Writing, talks, notes\\beer talk\\2024, March")
#        path = "~/student_documents\\UBC\\Research\\Writing, talks, notes\\chapters\\whole, chapter files\\Chapter 4/whole chapter versions")

# get data for work done over pH-stat interval by each species
# model comparison for species
# df with work done over pH (for edulis, depth transposed to pH)                                                                                                                                                                                        
# lm and anova?

## problem is that there is only one edulis measurement over the shared range. 
## Not sure I can do a direct model comparison, even with a transposed x axis

ed.larvae.df <- 
  read_csv("~/student_documents/UBC/Research/Malawi/data/sac pressure, pH series (6, 7, 8)/ed.larvae.df.csv") %>%
  mutate(species = "edulis", .before = larva) %>%
  mutate(mJ_per_length = -(norm.work * 1000)) %>% # reflect for positive and *1000 for mJ
  mutate(mean.mJ_per_length = -(mean.norm.work * 1000)) %>% # for checking 
  print()

# making rbind() compatible dfs for the three species
am.triv.work <- metaplats_ids.diam %>%
  select(species, indvd, sac, plat.pH.mean, mJ_per_length) %>%
  rename("pH" = "plat.pH.mean") %>%
  print()

ed.work <- ed.larvae.df %>%
  select(species, larva, sac, depth_m, mJ_per_length) %>%
  rename("indvd" = "larva") %>%
  # mutate(match.depth = 
  #          scales::rescale(depth_m, to = c(6.00, 6.45))) %>%
  print()

####  other way to use functions for coefs  ####
summary(mod4)                                                                                                                                                                                                                                              
coef_table <- summary(mod4)$coefficients %>%
  as.data.frame() %>%
  rename("std.error" = "Std. Error") %>%
  print()

quad.func.triv <- function(x) {
  (coefs["(Intercept)"] + coefs["speciestrivittatus"]) +
    (coefs["plat.pH.mean"] + coefs["plat.pH.mean:speciestrivittatus"]) * x +
    (coefs["I(plat.pH.mean^2)"] + coefs["I(plat.pH.mean^2):speciestrivittatus"]) * x^2
}
