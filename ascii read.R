library(tidyverse)
library(ggplot2)
library(readr)
library(ggpubr)

options(pillar.sigfig = 4)

##################   sac areas .csv and image files with file times   #################
areas <- 
  read_csv("~/student_documents/UBC/Research/pH_stat/2022-07-21 Strong and weak buffs gas control/results.csv") %>%
  mutate(...1 = NULL) %>%
  mutate(Mean = NULL)
print(areas)

# set working directory to folder with image files
setwd("~/student_documents/UBC/Research/pH_stat/2022-07-21 Strong and weak buffs gas control/images")

# get a list of all the image files to extract the times for
jpg_files <- list.files(pattern ="*.jpg",
                        ignore.case= T) #%>%
  #as.data.frame()

print(jpg_files)

#make a data frame with image file creation times (time axis for data)
image.info <- data.frame(file.info(jpg_files)$ctime) %>%
  rename(file.time = file.info.jpg_files..ctime) %>%
  mutate(jpg_files) %>% #file names from list.files
  arrange(file.time) %>% #arrange by time of file creation
  mutate(seconds = as.numeric(file.time)) %>% #seconds from arbitrary date (POSIX)
  mutate(minutes_from_start = (seconds - seconds[1])/60) %>%
  slice(rep(1:n(), each = 3)) %>% # repeat rows for the number of sacs in each image
  mutate(sac = rep(c("1","2","3"), # sac ID column, sacs/image repeated for #of images
                   times = (length(jpg_files) /3 ))) %>% 
  mutate(areas) %>%
  group_by(sac) %>%
  mutate(area.pct.change = 
           ((Area - Area[1]) / Area[1]) * 100)
  

head(image.info, n = 12)

#####################        ASCII OCPs        ########################
setwd("~/student_documents/UBC/Research/pH_stat/2022-07-21 Strong and weak buffs gas control/pH")

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
  mutate(pH = (median_V + 0.3502)/0.054) %>% # Volts to pH
  mutate(exp_start = (image.info$seconds[1])) %>% #1st image POSIX time
  mutate(minutes_from_start =        # time point of pH measurement relative to image file times
           ((OCP.file.seconds ## the end of an OCP run
             - median(seconds)) ## the middle of an OCP run (to be subtracted from end time for 1/2 time)
            - exp_start) ## put in relation to POSIX experiment start time (1st image)
         /60) ## minutes

head(OCPs.df, n = 10)

# simplified pH data frame for integration with sac area/ image data frame
pH.info <- OCPs.df %>%
  select(minutes_from_start, pH, OCP.files) %>%
  unique() %>%
  arrange(minutes_from_start)

head(pH.info, n= 11)

combined <- merge(image.info, pH.info, all = TRUE) #%>% 
  #filter(rep.minutes_from_start > 85, rep.minutes_from_start < 1222)

head(combined, n = 12)


########################    for current cycle ascii files    #######################

setwd("~/student_documents/UBC/Research/pH_stat/2022-07-21 Strong and weak buffs gas control/current")

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
  #geom_point(aes(y= (pH *3) - 18.027843 + 0.5), # magnify relative to main axis, translate so first point = zero, translate for fit -> see scale_y_continuous
            # size = 4.4,
            # colour = "orangered3") +
 # scale_y_continuous(sec.axis = sec_axis(~./3 + 6.009281 - (0.5/3), # / by magnify factor, + start pH, + (vert translation/ mag factor)
                                        # name = "pH", 
                                        # breaks = c(6.0, 6.25, 6.5, 6.75, 7, 7.25))) +
  annotate("rect", xmin= current.times.table$current.start.time[1], 
           xmax= current.times.table$current.file.time[1], 
           ymin=-2, ymax=14, fill = "green3", alpha=0.2) +
  annotate("rect", xmin= current.times.table$current.start.time[2], 
           xmax= current.times.table$current.file.time[2], 
           ymin=-2, ymax=14, fill = "green3", alpha=0.2) +
  annotate("rect", xmin= current.times.table$current.start.time[3], 
           xmax= current.times.table$current.file.time[3], 
           ymin=-2, ymax=14, fill = "green3", alpha=0.2) +
  annotate("rect", xmin= current.times.table$current.start.time[4], 
           xmax= current.times.table$current.file.time[4], 
           ymin=-2, ymax=14, fill = "green3", alpha=0.2) +
  annotate("rect", xmin= current.times.table$current.start.time[5], 
           xmax= current.times.table$current.file.time[5], 
           ymin=-2, ymax=14, fill = "green3", alpha=0.2) +
  labs(x = "Minutes", 
       y = "area % change") +
  theme_classic() +
  theme(axis.ticks.length = unit(-1, "mm")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 25)) +
  theme(legend.position = c(0.07, 0.8))

