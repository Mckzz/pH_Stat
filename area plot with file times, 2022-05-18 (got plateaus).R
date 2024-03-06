library(tidyverse)
library(ggplot2)
library(readr)
library(ggpubr)


options(pillar.sigfig = 4)

##################   OCP pH data (file times from nova (nox), and .csvs with voltage values)   ##################
setwd("C:/Users/evanm/Documents/My Procedures 1.11/Nova")

# get a list of when nova made the calib and galv files: gets all .nox files
nox_files <- list.files(pattern ="*.nox",
                        ignore.case= T)
#print(nox_files)

# create a df with the POSIx seconds for when all the .nox files were created (the end of each nova command) 
nox.times <- data.frame(file.info(nox_files)$ctime) %>%
  mutate(nox_files) %>%
  rename(nox.file.time = file.info.nox_files..ctime) %>%
  arrange(nox.file.time) %>%
  mutate(nox.seconds = as.numeric(nox.file.time)) #seconds from arbitrary date (POSIX)

head(nox.times, n = 24)

# get all the OCP .csv data files together into a df 
list_of_OCP.CSVs <- 
  list.files(path = "~/student_documents/UBC/Research/pH_stat/with_a_sac_(prelim)/2022-05-18/OCPs",
             recursive = TRUE,
             pattern = "\\.csv$",
             full.names = TRUE)

OCPs.df <- readr::read_csv(list_of_OCP.CSVs, id = "OCP.files") %>%
  mutate(...3 = NULL)
head(OCPs.df, n = 10)

######################     set working directory to folder with image files     ######################
# sac size data image file times
setwd("~/student_documents/UBC/Research/pH_stat/with_a_sac_(prelim)/2022-05-18/2022_05_18")


######################     import csv w/ measurements (airsac sizes)    ######################
data <- 
  read_csv("~/student_documents/UBC/Research/pH_stat/with_a_sac_(prelim)/2022-05-18/2022-05-18, Areas.csv") %>%
  mutate(...1 = NULL) %>%
  mutate(Mean = NULL)
print(data)

# get a list of all the image files to extract the times for
jpg_files <- list.files(pattern ="*.jpg",
                        ignore.case= T)
print(jpg_files)

#make a data frame with image file creation times (time axis for data)
exp.times <- data.frame(file.info(jpg_files)$ctime) %>%
  rename(file.time = file.info.jpg_files..ctime) %>%
  mutate(jpg_files) %>% #file names from list.files
  arrange(file.time) %>% #arrange by time of file creation
  mutate(seconds = as.numeric(file.time)) %>% #seconds from arbitrary date (POSIX)
  mutate(minutes_from_start = (seconds - seconds[1])/60)

head(exp.times, n = 12)

# create a df with the V and time points of the OCP determinations, then calculate 
# the middle time point and median V for each for the combined plot (must recalc substring rename positions)
OCPs_with_calcs <- OCPs.df %>%
  mutate(minutes_from_OCP_start = (seconds - seconds[1])/60) %>% 
  group_by(OCP.files) %>% # can be off by the fraction of a second diff between OCP seconds start time
  mutate(median_V = median(V)) %>%
  mutate(exp_start = (exp.times$seconds[1])) %>%
  mutate(substring(OCP.files, 101)) %>% #new column with ocp.files dropping the path before the file name (here position 101)
  mutate(substring(substring(OCP.files, 101), 1, 18)) %>% ## drop file extension (works as long as the OCP # has three digits)
  rename(OCP = 'substring(substring(OCP.files, 101), 1, 18)') %>% # fix stupid column name
  mutate(OCP = paste(OCP, "nox", sep = ".")) %>% ## now OCP should = the nox.times$nox_files so they can be linked later
  mutate('substring(OCP.files, 101)' = NULL) %>%
  ungroup() %>%
  group_by(OCP) %>%
  mutate(OCP.files = NULL) %>%
  mutate(exp.time.point = 
           ((nox.times$nox.seconds[
             nox.times$nox_files == OCP] ## the end of an OCP run
             - median(seconds)) ## the middle of an OCP run (to be subtracted from end time for 1/2 time)
            - exp_start) ## put in relation to POSIX experiment start time
         /60) ## minutes

head(OCPs_with_calcs)

#make a data frame with just median_V and exp.time.point from OCPs_with_calcs
OCP_V_time <- OCPs_with_calcs %>%
  ungroup() %>%
  mutate(pH = (median_V + 0.332)/0.054) %>% #old calibration
  mutate(seconds = NULL) %>%
  mutate(V = NULL) %>%
  mutate(minutes_from_OCP_start = NULL) %>%
  mutate(exp_start = NULL) %>%
  mutate(OCP = NULL) %>%
  na.omit() %>%
  unique() %>%
  rename(rep.minutes_from_start = exp.time.point)

head(OCP_V_time, n = 12)

# repeat each of the file info values for how many sacs are in each image (i.e. in each file)
rep.file.time <- rep(exp.times$file.time, each = 4)
rep.jpg_files <- rep(exp.times$jpg_files, each = 4)
rep.seconds <- rep(exp.times$seconds, each = 4)
rep.minutes_from_start <- rep(exp.times$minutes_from_start, each = 4)

# make a data frame of the above repeated info and add the area data
file.reps <- data.frame(rep.file.time) %>%
  mutate(rep.jpg_files) %>%
  mutate(rep.seconds) %>%
  mutate(rep.minutes_from_start) %>%
  mutate(data, Area) %>% #get areas from imported .csv
  mutate(sac = 
           rep(c("1","2","3","4"), times = nrow(exp.times))) %>% #sac ID column
  group_by(sac) %>%
  mutate(
    area.pct.change = ((Area - Area[1]) / Area[1])
    *100) %>%
  ungroup()

file.reps <- merge(file.reps, OCP_V_time, all = TRUE) #%>% # insert rows for the pH data points
  #filter(rep.minutes_from_start >= 0)

head(file.reps, n = 50)


########################     plotting and stats    ########################

ggplot(data = file.reps, 
       aes(x= rep.minutes_from_start, 
           group = sac, 
           colour= sac)) +
  geom_point(aes(y= area.pct.change)) +
  geom_line(aes(y= area.pct.change)) +
  scale_colour_discrete(na.translate = F) +
  geom_point(aes(y= (pH *30) - 184.2054 - 14), # magnify relative to main axis, translate points so first point = zero, translate down for fit -> see scale_y_continuous
             size = 4.4,
             colour = "orangered3") +
  scale_y_continuous(sec.axis = 
                       sec_axis(~./30 + 6.14018 + (14/30), # / by magnify factor, + start pH, + (vert translation/ mag factor)
                                name = "pH", 
                                breaks = c(6.25, 6.5, 6.75, 7, 7.25))) +  
  geom_vline(xintercept = 49.9, colour = "black") + 
  geom_vline(xintercept = 57.7, colour = "grey") +
  geom_vline(xintercept = 64.9, colour = "grey") +
  geom_vline(xintercept = 364.9, colour = "black") + 
  annotate("rect", xmin=126.45, xmax=126.45 + 13.578, ymin=-16, ymax=22, fill = "green3", alpha=0.2) +
  annotate("rect", xmin=145.6333, xmax=145.6333 + 13.626, ymin=-16, ymax=22, fill = "green3", alpha=0.2) +
  annotate("rect", xmin=165.55, xmax=165.55  + 9.79125, ymin=-16, ymax=22, fill = "green3", alpha=0.2) +
  annotate("rect", xmin=179.55, xmax=179.55  + 1.03657, ymin=-16, ymax=22, fill = "green3", alpha=0.2) +
  annotate("rect", xmin=186.783, xmax=186.783  + 1.2828, ymin=-16, ymax=22, fill = "green3", alpha=0.2) +
  annotate("rect", xmin=250.65, xmax=250.65  + 13.63425, ymin=-16, ymax=22, fill = "green3", alpha=0.2) +
  annotate("rect", xmin=272.8, xmax=272.8  + 13.6398, ymin=-16, ymax=22, fill = "green3", alpha=0.2) +
  annotate("rect", xmin=291.16, xmax=291.16  + 13.661, ymin=-16, ymax=22, fill = "green3", alpha=0.2) +
  annotate("rect", xmin=305.08, xmax=305.08  + 9.6026, ymin=-16, ymax=22, fill = "green3", alpha=0.2) +
  labs(x = "Minutes", 
       y = "Sac area % change") +
  theme_classic() +
  theme(axis.ticks.length = unit(-1, "mm")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 24)) +
  theme(legend.position = c(0.07, 0.8))


#subset of experiment times (could be plateau or linear increase region)
time.subset <- file.reps %>%
  filter(rep.minutes_from_start > 255, rep.minutes_from_start < 320) #%>% 
  #mutate(area.pct.change = replace(area.pct.change, sac == 4, NA)) # remove damaged/ unresponsive sac from analysis

print(time.subset)

#selected linear region plot
ggplot(data = time.subset, 
       aes(x= rep.minutes_from_start, 
           group = sac, 
           colour= sac)) +
  geom_point(aes(y= area.pct.change)) +
  geom_line(aes(y= area.pct.change)) +
  scale_colour_discrete(na.translate = F) +
  labs(x = "Minutes", 
       y = "% change") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  #theme(legend.position = "none") +
  ggtitle("selected linear region")

#regression plot of the one sample selected linear region with linear formula and r2 using ggpubr package
ggplot(time.subset, aes(y = area.pct.change, x = rep.minutes_from_start)) +
  geom_point(size = 2, col = "red") +
  geom_smooth(method = "lm", se = FALSE) +
  stat_regline_equation(label.x.npc = 0, 
                        label.y.npc = 1, 
                        aes(label = ..eq.label..)) +
  stat_regline_equation(label.x.npc = 0, 
                        label.y.npc = 0.9, 
                        aes(label = ..rr.label..)) +
  theme_classic()

# model for slope of area of interest (the above subset)
q <- lm(area.pct.change ~ rep.minutes_from_start, data = time.subset)
summary(q)
#summary(n)

######################      for plateaus against pH     ######################

# make the three plateaus and get a median pH column with no NAs
plateau.1 <- file.reps %>%
  filter(rep.minutes_from_start > 70, rep.minutes_from_start < 130) %>%
  mutate(plateau = "plateau1") %>%
  mutate(med_pH = median(pH, na.rm = T)) %>%
  as_tibble()
plateau.2 <- file.reps %>%
  filter(rep.minutes_from_start > 190, rep.minutes_from_start < 250) %>%
  mutate(plateau = "plateau2") %>%
  mutate(med_pH = median(pH, na.rm = T)) %>%
  as_tibble()
plateau.3 <- file.reps %>%
  filter(rep.minutes_from_start > 320, rep.minutes_from_start < 365) %>%
  mutate(plateau = "plateau3") %>%
  mutate(med_pH = median(pH, na.rm = T)) %>%
  as_tibble()

head(plateau.3)

# combine the plateaus and remove NAs from the other columns
plateaus <- rbind(plateau.1, plateau.2, plateau.3) %>%
  select(plateau, med_pH, rep.minutes_from_start, sac, Area, area.pct.change) %>%
  na.omit() %>%
  group_by(sac) %>%
  #mutate(med_pH = as_factor(med_pH)) %>%
  as_tibble()

print(plateaus, n= 40)

# mean and sd data frame for absolute areas and % changes as they change over the time within a plateau
plateaus.mean.sd <-
  plateaus %>%
  select(-rep.minutes_from_start) %>% 
  group_by(plateau, med_pH, sac) %>%
  summarize(across(everything(), na.rm= T,
                   tibble::lst(mean = mean, sd = sd)))

print(plateaus.mean.sd, n= 40)

# error bars show degree of sac size change within the time of the given plateau
ggplot(data = plateaus.mean.sd, 
       aes(x= med_pH, 
           group = sac, 
           colour= sac)) +
  geom_point(position = position_dodge(width = 0.1),
             aes(y= area.pct.change_mean)) +
  geom_errorbar(position = position_dodge(width = 0.1), 
                mapping = aes(x = med_pH,
                              ymin = area.pct.change_mean - area.pct.change_sd,
                              ymax = area.pct.change_mean + area.pct.change_sd), 
                width = 0.1,
                size = 0.5)

# modeled slopes of sac size (% or abs) with pH
ggplot(plateaus, aes(y = area.pct.change, x = med_pH, 
                     group = sac, 
                     colour = sac), 
       lab.nb.digits = 6) +
  geom_point(size = 2, col = "red") +
  geom_smooth(method = "lm", se = T) +
  stat_regline_equation(label.x.npc = 0, 
                        label.y.npc = 1, 
                        aes(label = ..eq.label..)) +
  stat_regline_equation(label.x.npc = 0, 
                        label.y.npc = 0.8, 
                        aes(label = ..rr.label..)) +
  theme_classic()

w <- lm(area.pct.change ~ med_pH, data = plateaus)
summary(w)


#################   getting coeffs for the two sections (pH1 -- pH2, pH2 -- pH3)     ###############

# make model to get the coefficients for the first interval of plateaus model plot
plateaus.1.2 <- rbind(plateau.1, plateau.2) %>%
  select(plateau, med_pH, rep.minutes_from_start, sac, Area, area.pct.change) %>%
  na.omit() %>%
  group_by(sac) %>%
  #mutate(med_pH = as_factor(med_pH)) %>%
  as_tibble()

first.half <- lm(area.pct.change ~ med_pH, data = plateaus.1.2)
summary(first.half)

# make model to get the coefficients for the second interval of plateaus model plot
plateaus.2.3 <- rbind(plateau.2, plateau.3) %>%
  select(plateau, med_pH, rep.minutes_from_start, sac, Area, area.pct.change) %>%
  na.omit() %>%
  group_by(sac) %>%
  #mutate(med_pH = as_factor(med_pH)) %>%
  as_tibble()

second.half <- lm(area.pct.change ~ med_pH, data = plateaus.2.3)
summary(second.half)

#plot for the two intervals modeled separately
plateaus.diffslopes <- rbind(plateau.1, plateau.2, plateau.3) %>%
  select(plateau, med_pH, rep.minutes_from_start, sac, Area, area.pct.change) %>%
  na.omit() %>%
  group_by(sac) %>%
  mutate(med_pH = as_factor(med_pH)) %>%
  as_tibble()

ggplot(plateaus.diffslopes, aes(y = area.pct.change, x = med_pH, group = sac, colour = sac), 
       lab.nb.digits = 4) +
  geom_point(size = 2, col = "red") +
  geom_smooth(method = "loess", se = T) +
  theme_classic()

#trace(ggpubr:::.stat_lm, edit = TRUE) #line 14 specifies the significant digits
