library(tidyverse)
library(ggplot2)
library(readr)
library(ggpubr)


##################   sac areas .csv and image files with file times   #################
areas <- 
  read_csv("~/student_documents/UBC/Research/pH_stat/with_a_sac_(prelim)/2022-05-25, strong buff H2 control/2022-05-25, Areas-trimmed.csv") %>%
  mutate(...1 = NULL) %>%
  mutate(Mean = NULL)
print(areas)

# set working directory to folder with image files
setwd("~/student_documents/UBC/Research/pH_stat/with_a_sac_(prelim)/2022-05-25, strong buff H2 control/2022_05_25")

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
  slice(rep(1:n(), each = 3)) %>% # repeat for the number of sacs in each image
  mutate(sac = rep(c("1","2","3"), # sac ID column, sacs/image repeated for #of images
                   times = (length(jpg_files) /3 ))) %>% 
  mutate(areas) %>%
  group_by(sac) %>%
  mutate(area.pct.change = 
           ((Area - Area[1]) / Area[1]) * 100)

head(image.info, n = 12)

#####################        ASCII OCPs        ########################
setwd("~/student_documents/UBC/Research/pH_stat/OCP ascii save test")

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
  unique() 

head(pH.info)

combined <- merge(image.info, pH.info, all = TRUE) #%>% 
  #filter(rep.minutes_from_start > 85, rep.minutes_from_start < 1222)

head(combined, n = 12)



# for current cycle ascii files
current.cycles <- read.table("~/student_documents/UBC/Research/pH_stat/with_a_sac_(prelim)/2022-05-25, strong buff H2 control/file save test.txt",  # Read TXT file
                             sep = "\t", header = TRUE)
current.cycles 



