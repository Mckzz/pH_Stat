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
  annotate("rect", 
           xmin = 30, 
           xmax = 60, 
           ymin = min(O2$O2_calib), 
           ymax = max(O2$O2_calib), 
           fill = "darkolivegreen3", alpha=0.5) +
  annotate("rect", 
           xmin = 0, 
           xmax = 30, 
           ymin = min(O2$O2_calib), 
           ymax = max(O2$O2_calib),  
           fill = "skyblue2", alpha=0.2) +
  annotate("rect", 
           xmin = 60, 
           xmax = 105, 
           ymin = min(O2$O2_calib), 
           ymax = max(O2$O2_calib), 
           fill = "skyblue2", alpha=0.2) +
  annotate("text", x = 45, y = 95, label = "current cycle on") +
  annotate("text", x = 15, y = 85, label = "current cycle off") +
  annotate("text", x = 82.5, y = 85, label = "current cycle off") +
  geom_line() + 
  geom_point() +
  theme_classic() +
  theme(axis.ticks.length = unit(-1, "mm")) +
  labs(y = "Oxygen saturation (%)", x = "Min") +
  theme(axis.title=element_text(size=14)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 24))


ggsave("pH-stat O2 sat.png", 
       units = c("cm"), 
       width = 18, height = 12,
       path = "~/student_documents\\UBC\\Research\\Writing, talks, notes\\chapters\\whole, chapter files\\Chapter 4\\whole chapter versions")
#path = "~/student_documents\\UBC\\Research\\Writing, talks, notes\\chapters\\whole, chapter files\\Chapter 4")