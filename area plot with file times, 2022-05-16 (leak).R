install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(readr)

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
  list.files(path = "~/student_documents/UBC/Research/pH_stat/with_a_sac_(prelim)/2022-05-16/OCPs",
             recursive = TRUE,
             pattern = "\\.csv$",
             full.names = TRUE)

OCPs.df <- readr::read_csv(list_of_OCP.CSVs, id = "OCP.files") %>%
  mutate(...3 = NULL)
head(OCPs.df, n = 10)

######################     set working directory to folder with image files     ######################
# sac size data image file times
setwd("~/student_documents/UBC/Research/pH_stat/with_a_sac_(prelim)/2022-05-16/2022_05_16")


######################     import csv w/ measurements (airsac sizes)    ######################
data <- 
  read_csv("~/student_documents/UBC/Research/pH_stat/with_a_sac_(prelim)/2022-05-16/2022-05-16, Areas.csv") %>%
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
  mutate(pH = (median_V + 0.35)/0.054) %>% #old calibration
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
           rep(c(1,2,3,4), times = nrow(exp.times))) %>% #sac ID column
  group_by(sac) %>%
  mutate(
    area.pct.change = ((Area - Area[1]) / Area[1])
    *100) %>%
  ungroup()

file.reps <- merge(file.reps, OCP_V_time, all = TRUE) %>% # insert rows for the pH data points
  filter(rep.minutes_from_start >= 0)

head(file.reps, n = 50)


########################     plotting and stats    ########################

ggplot(data = file.reps, 
       aes(x= rep.minutes_from_start, 
           group = sac, 
           colour= sac)) +
  geom_point(aes(y= area.pct.change)) +
  geom_line(aes(y= area.pct.change)) +
  geom_point(aes(y= (pH *30) - 181.0353 - 4), # magnify relative to main axis, 
  #translate pH points so final point = zero -> see scale_y_continuous
            size = 4.4,
           colour = "orangered3") +
   scale_y_continuous(sec.axis = sec_axis(~./30 + 6.034511 + (4/30), name = "pH")) + # /20 to undo magnify, 
  # +8.021611 to translate final pH point back to its proper value on the 2nd axis
  #geom_vline(xintercept = 195, colour = "green3") + 
  #geom_vline(xintercept = 197.8, colour = "green3") + 
  #geom_vline(xintercept = 260.1, colour = "green3") + 
  #geom_vline(xintercept = 262.89, colour = "green3") + 
  labs(x = "Minutes", 
       y = "Sac area % change") +
  theme_classic() +
  theme(axis.ticks.length = unit(-1, "mm")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 24)) 
#theme(legend.position = "none")



#subset of experiment times
time.subset <- file.reps %>%
  filter(rep.minutes_from_start > 200) #%>%  
#mutate(area.pct.change = replace(area.pct.change, sac == 4, NA)) # remove unresponsive sac from analysis

print(time.subset)

ggplot(data = time.subset, 
       aes(x= rep.minutes_from_start, 
           group = sac, 
           colour= sac)) +
  geom_point(aes(y= area.pct.change)) +
  geom_line(aes(y= area.pct.change)) +
  labs(x = "Minutes", 
       y = "Sac area % change") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 24)) +
  #theme(legend.position = "none") +
  ggtitle("selected linear region")

# model for slope of area of interest (the above subset)
l <- lm(area.pct.change ~ rep.minutes_from_start, data = time.subset)
summary(l)
#summary(n)

ggplot(time.subset, aes(y = area.pct.change, x = rep.minutes_from_start)) +
  geom_point(size = 2, col = "red") +
  geom_smooth(method = "lm", se = FALSE) +
  theme(aspect.ratio = 0.80) +
  theme_classic()
