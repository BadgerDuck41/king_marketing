# call libraries needed for analysis
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyverse)

#####LIBERAL ESTIMATION########

#read csv into R
liberal_est_df <- read.csv("/Users/jferrero/Desktop/Personal/king marketing/Referral Program - 5.2020/Data/liberal_estimate_df.csv")

#sort data into format wanted
liberal_est_df <- liberal_est_df %>%
           arrange (desc(step))

#create new variables for graph positioning
liberal_est_df <- liberal_est_df %>%
  filter (step >=1) %>%
  arrange(step) %>%
  group_by(step) %>%
  mutate(pos = cumsum(number) - 0.5*number) %>%
  ungroup()

#linebreak for funnel level label and base size
levels(liberal_est_df$lbl) <- gsub("!", "\n", levels(liberal_est_df$lbl))

# creating custom palette (with 'white' color for dummies (blue - current subscribers; green - referred prospects)
cols <- c("#4D6BC6", "#9EC063", "#258635", "#2F466C","#232347")

#create plot and save to object called "liberal_est_plot"
liberal_est_plot <-ggplot(liberal_est_df,aes(x=step,y=number,fill=content)) +
  geom_bar(stat="identity", width=1) +
  geom_text(aes(label = rate,hjust = -.35),fontface = "bold.italic") +
  scale_fill_manual(values=cols) +
  coord_flip() +
  scale_y_log10() +
  ylab("") +
  xlab("") +
  ggtitle("referral program growth funnel",subtitle = "(estimation)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.415)) +
  theme(plot.title = element_text(lineheight=.8, size = 12,face="bold")) +
  theme(plot.subtitle = element_text(size = 10,face="italic",color = "#A61A2E")) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data=liberal_est_df, aes(x=step, y=pos, label=lbl), family = "sans",size=3.75, hjust = .9, color='white') +
  annotate("text",x=3.95, y=.5, label = "current subscriber",angle=90,color="#4D6BC6",fontface = "bold") +
  annotate("text",x=1.5, y=.5, label = "referred prospect",angle=90,color="#258635",fontface="bold")

liberal_est_plot 

#export to photo (.png)
ggsave("/Users/jferrero/Desktop/liberal_est_plot.png",liberal_est_plot)


