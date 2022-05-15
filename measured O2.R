install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(readr)


#import csv w/ measurements
O2 <- read_csv("~/student_documents/UBC/Research/pH_stat/O2_probe.csv")

#View(O2)
options(pillar.sigfig = 4)

O2 <- O2 %>%
  mutate(O2_calib = O2_sat - 7.7)  # set O2 sat at 100% for air-equilibrated water
print(O2)

ggplot(data = O2, 
       aes(x= minutes, 
           y= O2_calib)) + 
  geom_line() + 
  geom_point() +
  theme_classic() +
  theme(axis.ticks.length = unit(-1, "mm")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 24))
