install.packages("tidyverse")

library(tidyverse)
library(ggplot2)
library(readr)
library(ggpubr)

options(pillar.sigfig = 4)

setwd("~/student_documents/UBC/Research/pH_stat/4mM buffer, for final pH step analysis\\2022-12-21")

############     a column made of values at plateau 1 repeated to the length of the following df    ################

plat1 <- plateaus.mean.sd %>%
  select(Area_mean) %>%
  filter(plateau == "plateau1") %>%
  rename(plat1.norm = Area_mean) %>%
  as.data.frame() %>%
  mutate(plateau = NULL) %>%
  mutate(med_pH = NULL) %>%
  slice(rep(1:n(), times = (length(jpg_files))))

print(plat1)

image.info$plat1.norm <- plat1$plat1.norm

image.info <- image.info %>%
  group_by(sac) %>%
  mutate(area.pct.change = ##  normalize to start of 1st plateau (not Area[1])
           ((Area - plat1.norm) / plat1.norm) * 100)
      
head(image.info, n = 12)

#####################        ASCII OCPs        ########################
setwd("./pH")

list_of_OCP.ascii <- # list of file names
  list.files(pattern = "\\.txt$",
             full.names = F) #leave out folder path
head(list_of_OCP.ascii, n = 10)

# make a data frame of all the OCPs with file times, calculated pH,
# and a column for time of pH measure relative to start of image files (start of experiment)
OCPs.df <- read_delim(list_of_OCP.ascii,
                      delim = "\t",
                      id = "OCP.files") %>% #file ID column
  mutate(OCP.file.time = file.info(OCP.files)$ctime) %>% # date and time of OCP ascii file creation
  mutate(OCP.file.seconds = as.numeric(OCP.file.time)) %>% #POSIX seconds of above
  rename("seconds" = "Time (s)") %>%
  rename(V = "WE(1).Potential (V)") %>%
  mutate(minutes_from_OCP_start = (seconds - seconds[1])/60) %>%
  mutate(OCP.files = substr(OCP.files, 1, nchar(OCP.files)-4)) %>% #drop file extension so that OCPs can relate to image file times
  group_by(OCP.files) %>%
  mutate(median_V = median(V)) %>%
  mutate(pH = (median_V + 0.3579)/0.054) %>% # Volts to pH 2022-12-21
  mutate(exp_start = (image.info$seconds[1])) %>% #1st image POSIX time
  mutate(minutes_from_start =        # time point of pH measurement relative to image file times
           ((OCP.file.seconds ## the end of an OCP run
             - median(seconds)) ## the middle of an OCP run (to be subtracted from end time for 1/2 time)
            - exp_start) ## put in relation to POSIX experiment start time (1st image)
         /60) ## minutes

head(OCPs.df, n = 10)

# simplified pH data frame for integration with sac area/ image data frame, with OCP measures placed in correct order
pH.info <- OCPs.df %>%
  select(minutes_from_start, pH, OCP.files) %>%
  unique() %>%
  arrange(minutes_from_start)

head(pH.info, n= 20)

combined <- merge(image.info, pH.info, all = TRUE) #%>%
  #filter(rep.minutes_from_start > 85, rep.minutes_from_start < 1222)

head(combined, n = 12)


########################    for current cycle ascii files    #######################

setwd("../current")

current.files <- # list of file names
  list.files(pattern = "\\.txt$",
             full.names = F) #leave out folder path

head(current.files, n= 10)

#make a data frame with current ascii files related to the time each set of cycles finished (the file creation times)
current_times <- data.frame(current.files,
                            as.numeric(file.info(current.files)$ctime)) %>%
  rename(current.file.time = "as.numeric.file.info.current.files..ctime.")

head(current_times)

# make data frame of current experimental parameters
current_cycles.df <- read_delim(current.files,
                                delim = "\t",
                                id = "current.files")

head(current_cycles.df, n = 20)

# current time calcs
current_cycles <- left_join(current_cycles.df, current_times) %>%
  select(current.files, `Time (s)`, current.file.time, `WE(1).Current (A)`) %>%
  mutate(exp_start = median(OCPs.df$exp_start)) %>%
  mutate(current.file.time = current.file.time - exp_start) %>%
  group_by(current.files) %>%
  mutate(current.start.time = (current.file.time - max(`Time (s)`) + min(`Time (s)`))) %>%
  mutate(current.file.time = current.file.time / 60) %>%
  mutate(current.start.time = current.start.time / 60)

head(current_cycles)

# table for use in plot to mark current times
current.times.table <- current_cycles %>%
  select(current.files, current.start.time, current.file.time) %>%
  unique()

head(current.times.table)



########################     plotting    ########################

ggplot(data = combined, 
       aes(x= minutes_from_start, 
           group = sac, 
           colour= sac)) +
  geom_point(aes(y= area.pct.change)) +
  geom_line(aes(y= area.pct.change)) +
  scale_colour_discrete(na.translate = F) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 15)) +
  geom_point(aes(y= (pH *22) -131.278444 -8), # magnify relative to main axis, translate so first point = zero, translate for fit -> see scale_y_continuous
             size = 4.4,
             colour = "orangered3") +
 # scale_y_continuous(sec.axis = sec_axis(~./22 + 5.967202 + (8/22), # / by magnify factor, + start pH, + inverse (+-) (vert translation/ mag factor)
                                        #name = "pH", 
                                         #breaks = c(6.0, 6.25, 6.5, 6.75, 7, 7.25))) +
  annotate("rect", xmin= current.times.table$current.start.time[1], 
           xmax= current.times.table$current.file.time[1], 
           ymin=-10, ymax=6, fill = "green3", alpha=0.2) +
  annotate("rect", xmin= current.times.table$current.start.time[2], 
           xmax= current.times.table$current.file.time[2], 
           ymin=-10, ymax=6, fill = "green3", alpha=0.2) +
  annotate("rect", xmin= current.times.table$current.start.time[3], 
           xmax= current.times.table$current.file.time[3], 
           ymin=-10, ymax=6, fill = "green3", alpha=0.2) +
  #geom_vline(xintercept = 603.4333) +
  labs(x = "Minutes", 
       y = "% Change in Air-sac Area") +
  theme_classic() +
  theme(axis.ticks.length = unit(-1, "mm")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 40)) +
  theme(legend.position = c(0.08, 0.2))


###############################   extract plateaus   #################################### (needed on both 1st and 2nd pass)

# make the 4 plateaus and get a median pH column with no NAs 2022-12-21
plateau.1 <- combined %>%
  filter(minutes_from_start > 300, minutes_from_start < 360) %>%
  mutate(plateau = "plateau1") %>%
  mutate(med_pH = median(pH, na.rm = T)) %>%
  as_tibble()
plateau.2 <- combined %>%
  filter(minutes_from_start > 380, minutes_from_start < 440) %>%
  mutate(plateau = "plateau2") %>%
  mutate(med_pH = median(pH, na.rm = T)) %>%
  as_tibble()
plateau.3 <- combined %>%
  filter(minutes_from_start > 460, minutes_from_start < 530) %>%
  mutate(plateau = "plateau3") %>%
  mutate(med_pH = median(pH, na.rm = T)) %>%
  as_tibble()




### make med_pH for plateau.1 the med starting pH, even though there isnt an OCP during the plateau it's self (it's very stable though)    ########
##############   FOR WHEN THERE ISN'T A pH MEASURE RIGHT DURING THE FIRST PLATEAU  (use previous measure)  #############
#############          OTHERWISE, COMMENT THIS OUT      #########      for 2022-11-01   (needed on both 1st and 2nd pass)  #############
# start.pH <- combined %>%
#   select(pH) %>%
#   na.omit() %>%
#   mutate(start_pH = median(pH[1:6]))
# 
# print(start.pH)
# 
# plateau.1 <- plateau.1 %>%
#   mutate(med_pH = start.pH$start_pH[1])
# 
# head(plateau.1)



# combine the plateaus and remove NAs from the other columns (needed on both 1st and 2nd pass)
plateaus <- rbind(plateau.1, plateau.2, plateau.3) %>%
  select(plateau, med_pH, minutes_from_start, sac, Area, area.pct.change) %>%
  na.omit() %>%
  group_by(sac) %>%
  mutate(med_pH = as_factor(med_pH)) %>%
  mutate(sac = as_factor(sac)) %>%
  mutate(day = as.character(combined$file.time[1])) %>% #COLUMN TO ID WHICH DAY'S EXPERIMENT THIS IS. to later allow appending of the other experiment days
  mutate(day = substr(day, 1, nchar(day)-9)) %>% # drop h/min/sec
  as_tibble()

print(plateaus, n= 40)


#trace(ggpubr:::.stat_lm, edit = TRUE) #line 14 specifies the significant digits


###########################       making combined data set      ###########################

########  write/ lengthen .csv for replicate experiments. Original write using 2022-08-24. Subsequently, append to bottom 
# write_csv(plateaus,
# "~/student_documents/UBC/Research/pH_stat/4mM buffer, for final pH step analysis\\meta.plateaus.csv")



##############       open previously built .csv and append current data set to the bottom    ONLY FOR SUBSEQUENT EXPERIMENTS
meta.plateaus <- read_csv("~/student_documents/UBC/Research/pH_stat/4mM buffer, for final pH step analysis/meta.plateaus.csv")

print(meta.plateaus)

meta.plateaus <- rbind(meta.plateaus, plateaus)

## use combined data set to replace original .csv
write_csv(meta.plateaus,
          "~/student_documents/UBC/Research/pH_stat/4mM buffer, for final pH step analysis\\meta.plateaus.csv")


###################                      remove mismade rows
# meta.plateaus <- meta.plateaus %>%
#   filter(day != "2022-11-01")




#####################################################################################################
##  for all together
#####################################################################################################

## remove troublesome bits from the end
# meta.plateaus <- meta.plateaus %>%
#   filter(day != "2022-12-05")



# For once all data is in, add a larva ID column using this sheet in the next code block
ids <- read_csv("../../ids.csv")

# get indvd and IDs, and get means for a meta plot. 
# collapse day and retain only means between days (so as not to inflate n# for smoothed plot)
metaplats_ids <- merge(meta.plateaus, ids, by = c('day','sac')) %>%
  arrange(day, plateau, minutes_from_start) %>%
  group_by(plateau) %>%
  mutate(med_pH = as.numeric(med_pH)) %>%
  mutate(plat.pH.mean = mean(med_pH)) %>% # pH for each plateau averaged over days. Factor so the loess behaves
  mutate(plat.pH.sd = sd(med_pH)) %>%
  group_by(plateau, species, indvd, sac) %>%
  mutate(plat.areapct.mean = mean(area.pct.change)) %>% # area % change for each plateau averaged over days
  mutate(plat.areapct.sd = sd(area.pct.change)) %>%
  mutate(plat.area.mean = mean(Area)) %>% # ABS area for each plateau averaged over days
  mutate(plat.area.sd = sd(Area)) %>%
  #mutate(day = NULL) %>%
  mutate(minutes_from_start = NULL) %>%
  mutate(Area = NULL) %>%
  mutate(area.pct.change = NULL) %>%
  unique() %>%
  mutate(indvd = as.factor(indvd)) %>%
  mutate(med_hydrogen = 10^(-(as.numeric(plat.pH.mean)))) %>%
  mutate(med_plat_hydrogen = 10^(-(as.numeric(med_pH)))) %>%
  group_by(plateau) %>%
  mutate(med_hyd_sd = sd(med_plat_hydrogen))
  
print(metaplats_ids)



# indvds sampled as meany times per plateau pH as there are images for that plateau

ggplot(metaplats_ids, 
       aes(y = plat.areapct.mean, x = plat.pH.mean, 
           group = species, colour = species, shape = indvd), 
       lab.nb.digits = 4) +
  geom_jitter(size = 3, width = 0.02) +
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11)) +
  geom_smooth(method = "loess", se = T, span = 0.8) + # 95CI from se = T by default
  scale_color_manual(values=c("#D55E00", "#009e73")) +
  theme_classic() +
  #ylim(-1, 20) +
  theme(legend.position = c(0.2, 0.7))






# [H+] as x axis
ggplot(metaplats_ids, 
       aes(y = plat.areapct.mean, x = med_hydrogen, 
           group = species, colour = species, ), 
       lab.nb.digits = 4) +
  geom_point(size = 3) +
  #geom_jitter(size = 3, width = 0.02) +
  #scale_shape_manual(values=c(0, 1, 2, 6, 7, 9)) +
  geom_smooth(method = "loess", se = T) + # 95CI from se = T by default
  scale_color_manual(values=c("#D55E00", "#009e73")) +
  scale_x_reverse() + # so alkalinization goes to the right, as with pH
  # geom_errorbar(position = position_dodge(width = 0.1), 
  #               mapping = aes(y = plat.areapct.mean[-1], 
  #                             xmin = med_hydrogen - med_hyd_sd, 
  #                             xmax = med_hydrogen + med_hyd_sd), 
  #               width = 0.1,
  #               size = 0.5)
  theme_classic() +
  theme(legend.position = c(0.2, 0.7))



z <- lm(plat.areapct.mean ~ plat.pH.mean * species, data = metaplats_ids)
summary(z)
anova(z)

install.packages("lmerTest")
library(lmerTest)

y <- lmer()




#############     getting a value for airsac width averaged over the 1st plateau    ##############
#########   for use in creating a linearized index based on area

setwd("~/student_documents/UBC/Research/pH_stat/4mM buffer, for final pH step analysis\\2022-08-24")

# initially create this data frame using the first experiment
plat1_widths_raw_2022.08.24 <- read_csv("./Results_plat1.width.csv") %>%
  mutate(...1 = NULL) %>%
  mutate(Area = NULL) %>%
  mutate(Angle = NULL) %>%
  rename(width = Length) %>%
  mutate(sac = rep(c(1:4), times = 4)) %>% # of sacs in an image, times = # of images
  mutate(plat1_image = rep(c(1:4), each = 4)) %>% # of images, times = # of sacs in an image
  group_by(sac) %>%
  mutate(mean_width = mean(width)) %>%
  mutate(day = as.Date(basename(getwd())))

print(plat1_widths_raw_2022.08.24)

# next expt
setwd("~/student_documents/UBC/Research/pH_stat/4mM buffer, for final pH step analysis\\2022-08-30")

plat1_widths_raw_2022.08.30 <- read_csv("./Results_plat1.width.csv") %>%
  mutate(...1 = NULL) %>%
  mutate(Area = NULL) %>%
  mutate(Angle = NULL) %>%
  rename(width = Length) %>%
  mutate(sac = rep(c(1:4), times = 3)) %>% # of sacs in an image, times = # of images
  mutate(plat1_image = rep(c(1:3), each = 4)) %>% # of images, times = # of sacs in an image
  group_by(sac) %>%
  mutate(mean_width = mean(width)) %>%
  mutate(day = as.Date(basename(getwd())))

print(plat1_widths_raw_2022.08.30)

# next expt
setwd("~/student_documents/UBC/Research/pH_stat/4mM buffer, for final pH step analysis\\2022-10-29")

plat1_widths_raw_2022.10.29 <- read_csv("./Results_plat1.width.csv") %>%
  mutate(...1 = NULL) %>%
  mutate(Area = NULL) %>%
  mutate(Angle = NULL) %>%
  rename(width = Length) %>%
  mutate(sac = rep(c(1:4), times = 9)) %>% # of sacs in an image, times = # of images
  mutate(plat1_image = rep(c(1:9), each = 4)) %>% # of images, times = # of sacs in an image
  group_by(sac) %>%
  mutate(mean_width = mean(width)) %>%
  mutate(day = as.Date(basename(getwd())))

print(plat1_widths_raw_2022.10.29)

# next expt
setwd("~/student_documents/UBC/Research/pH_stat/4mM buffer, for final pH step analysis\\2022-11-01")

plat1_widths_raw_2022.11.01 <- read_csv("./Results_plat1.width.csv") %>%
  mutate(...1 = NULL) %>%
  mutate(Area = NULL) %>%
  mutate(Angle = NULL) %>%
  rename(width = Length) %>%
  mutate(sac = rep(c(1:4), times = 5)) %>% # of sacs in an image, times = # of images
  mutate(plat1_image = rep(c(1:5), each = 4)) %>% # of images, times = # of sacs in an image
  group_by(sac) %>%
  mutate(mean_width = mean(width)) %>%
  mutate(day = as.Date(basename(getwd())))

print(plat1_widths_raw_2022.11.01)


# next expt
setwd("~/student_documents/UBC/Research/pH_stat/4mM buffer, for final pH step analysis\\2022-12-05")

plat1_widths_raw_2022.12.05 <- read_csv("./Results_plat1.width.csv") %>%
  mutate(...1 = NULL) %>%
  mutate(Area = NULL) %>%
  mutate(Angle = NULL) %>%
  rename(width = Length) %>%
  mutate(sac = rep(c(1, 3), times = 4)) %>% # of sacs in an image, times = # of images
  mutate(plat1_image = rep(c(1:4), each = 2)) %>% # of images, times = # of sacs in an image
  group_by(sac) %>%
  mutate(mean_width = mean(width)) %>%
  mutate(day = as.Date(basename(getwd())))

print(plat1_widths_raw_2022.12.05)

# next expt
setwd("~/student_documents/UBC/Research/pH_stat/4mM buffer, for final pH step analysis\\2022-12-20")

plat1_widths_raw_2022.12.20 <- read_csv("./Results_plat1.width.csv") %>%
  mutate(...1 = NULL) %>%
  mutate(Area = NULL) %>%
  mutate(Angle = NULL) %>%
  rename(width = Length) %>%
  mutate(sac = rep(c(2, 3, 4), times = 7)) %>% # of sacs in an image, times = # of images
  mutate(plat1_image = rep(c(1:7), each = 3)) %>% # of images, times = # of sacs in an image
  group_by(sac) %>%
  mutate(mean_width = mean(width)) %>%
  mutate(day = as.Date(basename(getwd())))

print(plat1_widths_raw_2022.12.20)

# next expt
setwd("~/student_documents/UBC/Research/pH_stat/4mM buffer, for final pH step analysis\\2022-12-21")

plat1_widths_raw_2022.12.21 <- read_csv("./Results_plat1.width.csv") %>%
  mutate(...1 = NULL) %>%
  mutate(Area = NULL) %>%
  mutate(Angle = NULL) %>%
  rename(width = Length) %>%
  mutate(sac = rep(c(1:4), times = 6)) %>% # of sacs in an image, times = # of images
  mutate(plat1_image = rep(c(1:6), each = 4)) %>% # of images, times = # of sacs in an image
  group_by(sac) %>%
  mutate(mean_width = mean(width)) %>%
  mutate(day = as.Date(basename(getwd())))

print(plat1_widths_raw_2022.12.21)



# make lengthened plat1_widths_raw with all the experiments
plat1_width_means <- rbind(plat1_widths_raw_2022.08.24,
                           plat1_widths_raw_2022.08.30,
                           plat1_widths_raw_2022.10.29,
                           plat1_widths_raw_2022.11.01,
                           plat1_widths_raw_2022.12.05,
                           plat1_widths_raw_2022.12.20,
                           plat1_widths_raw_2022.12.21)%>%
  mutate(width = NULL) %>%
  mutate(plat1_image = NULL) %>%
  mutate(sac = as.character(sac)) %>%
  unique()

print(plat1_width_means, n = 25)



# plat.area.mean is an absolute area. you want change in area without going to %ages. 
# Linearize first? (plat.area.mean / mean_width)
# 
# create linearized measure column using widths
metaplats_ids.diam <- left_join(metaplats_ids, 
                                plat1_width_means, 
                                by = c("day", 
                                       "sac")) %>%
  mutate(lin_area = plat.area.mean / mean_width) %>% # aspect is a linearized area
  group_by(day, sac) %>%
  mutate(lin_area.pct.change = (((lin_area - lin_area[1]) / lin_area[1]) * 100)) %>% # % change in aspect from 1st plateau
  mutate(area_change = (plat.area.mean - plat.area.mean[1])) %>% # Just the change in area
  mutate(lin_area_change = (lin_area - lin_area[1])) %>% # Just the change in linearized area
  mutate(sac.force = ((mean_width / 2)^2) * pi * 41368.5) %>% # 41368.5 N/m sqrd = 6 psi
  mutate(sac.work = sac.force * lin_area_change) # F * D

print(metaplats_ids.diam)





# indvds sampled as meany times per plateau pH as there are images for that plateau
ggplot(metaplats_ids.diam, 
       aes(y = aspect.pct.change, x = plat.pH.mean, 
           group = species, colour = species, shape = indvd), 
       lab.nb.digits = 4) +
  geom_jitter(size = 3, width = 0.02) +
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13)) +
  geom_smooth(method = "loess", se = T, span = 0.8) + # 95CI from se = T by default
  scale_color_manual(values=c("#D55E00", "#009e73")) +
  theme_classic() +
  #ylim(-1, 20) +
  theme(legend.position = c(0.2, 0.7))


# indvds sampled as meany times per plateau pH as there are images for that plateau
ggplot(metaplats_ids.diam, 
       aes(y = sac.work, x = plat.pH.mean, 
           group = species, colour = species, shape = indvd), 
       lab.nb.digits = 4) +
  geom_jitter(size = 3, width = 0.02) +
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13)) +
  geom_smooth(method = "loess", se = T, span = 0.8) + # 95CI from se = T by default
  scale_color_manual(values=c("#D55E00", "#009e73")) +
  theme_classic() +
  #ylim(-1, 20) +
  theme(legend.position = c(0.2, 0.6))


