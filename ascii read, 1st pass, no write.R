library(tidyverse)
library(ggplot2)
library(readr)
library(ggpubr)

options(pillar.sigfig = 4)

#setwd("~/student_documents/UBC/Research/pH_stat/reverse (no e-)\\2023-05-31, pilot with two trivits, one NEMed")

setwd("~/student_documents/UBC/Research/pH_stat/4mM buffer, for final pH step analysis\\2022-12-21")

#################   sac areas .csv and image files with file times   #################
areas <-
  read_csv("./results.csv") %>%
  mutate(...1 = NULL) %>%
  mutate(Mean = NULL)
print(areas)

# set working directory to folder with image files
setwd("./images")

# get a list of all the image files to extract the times for
jpg_files <- list.files(pattern ="*.jpg",
                        ignore.case= T) #%>%
#as.data.frame()

print(jpg_files)

#make a data frame with image file creation times (time axis for data)
image.info <- data.frame(file.info(jpg_files)$mtime) %>%
  rename(file.time = file.info.jpg_files..mtime) %>%
  mutate(jpg_files) %>% #file names from list.files
  arrange(file.time) %>% #arrange by time of file creation
  mutate(seconds = as.numeric(file.time)) %>% #seconds from arbitrary date (POSIX)
  mutate(minutes_from_start = (seconds - seconds[1])/60) %>%
  slice(rep(1:n(), each = 4)) %>% # repeat rows for the number of sacs in each image ####### ####### ######## ########
  mutate(sac = rep(c("1","2","3","4"), # sac ID column, sacs/image repeated for #of images ####### ####### ######## ########
                   times = (length(jpg_files) /4 ))) %>% 
  mutate(areas) %>%
  group_by(sac) %>%
  mutate(area.pct.change =
           ((Area - Area[1]) / Area[1]) * 100) #%>%
  #filter(sac == "1" | sac == "3") # %>% # remove sacs 2 and 3 (FOR UNANALYZABLE SACs, 2022-12-05)
  #filter(sac != "1")  # remove sac 1 (FOR UNANALYZABLE SAC, 2022-12-20)
  

head(image.info, n = 12)

#####################        ASCII OCPs        ########################
setwd("../pH")

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

combined <- merge(image.info, pH.info, all = TRUE) %>%
  as_tibble() #%>% 
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

ggplot(data = image.info, 
       aes(x= minutes_from_start, 
           group = sac, 
           colour= sac)) +
  geom_point(aes(y= Area)) +
  geom_line(aes(y= Area)) +
  scale_colour_discrete(na.translate = F) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 15)) +
  # geom_point(aes(y= (pH *22) -135 -0), # magnify relative to main axis, translate so first point = zero, translate for fit -> see scale_y_continuous
  #            size = 4.4,
  #            colour = "orangered3") +
  # scale_y_continuous(sec.axis = sec_axis(~./22 + 5.967202 + (8/22), # / by magnify factor, + start pH, + inverse (+-) (vert translation/ mag factor)
  #name = "pH", 
  #breaks = c(6.0, 6.25, 6.5, 6.75, 7, 7.25))) +
  # annotate("rect", xmin= current.times.table$current.start.time[1],
  #          xmax= current.times.table$current.file.time[1],
  #          ymin=-16, ymax=0, fill = "green3", alpha=0.2) +
  # annotate("rect", xmin= current.times.table$current.start.time[2],
  #          xmax= current.times.table$current.file.time[2],
  #          ymin=-16, ymax=0, fill = "green3", alpha=0.2) +
  # annotate("rect", xmin= current.times.table$current.start.time[3],
  #          xmax= current.times.table$current.file.time[3],
  #          ymin=-16, ymax=0, fill = "green3", alpha=0.2) +
  # geom_vline(xintercept = 603.4333) +
  labs(x = "Minutes", 
       y = "% Change in Air-sac Area") +
  theme_classic() +
  theme(axis.ticks.length = unit(-1, "mm")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 40)) +
  theme(legend.position = c(0.5, 0.3))


###############################   extract plateaus   ####################################

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
#############          OTHERWISE, COMMENT THIS OUT      #########      for 2022-11-01     #############
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


# combine the plateaus and remove NAs from the other columns
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

#####    plot where error bars show degree of sac size change within the time of a given plateau    #####
# mean and sd data frame for sac size change (abs or %) over the time within a plateau
plateaus.mean.sd <-
  plateaus %>%
  select(-minutes_from_start, -day) %>% 
  group_by(plateau, med_pH, sac) %>%
  summarize(across(everything(), na.rm= T,
                   tibble::lst(mean = mean, sd = sd)))

print(plateaus.mean.sd, n= 40)

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


# modeled slopes of sac size (% or abs) with pH, assumed linear
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


#################   getting coeffs for individual intervals eg. pH1 -- pH2, pH2 -- pH3... etc.     ###############

# make model to get the coefficients for the first interval of plateaus model plot
# plateaus.1.2 <- rbind(plateau.1, plateau.2) %>%
#   select(plateau, 
#          med_pH, 
#          minutes_from_start, 
#          sac, 
#          Area, 
#          area.pct.change) %>%
#   na.omit() %>%
#   group_by(sac) %>%
#   #mutate(med_pH = as_factor(med_pH)) %>%
#   as_tibble()


# 
# #plot for the intervals modeled separately
# ggplot(plateaus, 
#        aes(y = area.pct.change, x = med_pH, 
#            group = sac, colour = sac), 
#        lab.nb.digits = 4) +
#   geom_point(size = 2) +
#   geom_smooth(method = "loess", se = T) +
#   theme_classic()


