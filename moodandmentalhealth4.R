

#First of all, clearing the existing environment

rm(list=ls());

#Loading the required packages

library(openxlsx)
library(ggplot2)
library(stringr)
library(gridExtra)
library(knitr)
library(dplyr)
library(EnvStats)
library(stats)
library(ggpubr)
library(grid)
library(gtable)

#Downloading the Excel spreadhseet from the SSC website

ssc2019 <- read.xlsx("https://slatestarcodex.com/Stuff/ssc2019_public.xlsx");

#Making sure column names are unique

colnames(ssc2019) <- make.unique(names(ssc2019));

#Filtering null values

ssc2019 <- ssc2019 %>% 
  filter(Depression != "NA")  %>%
  filter(Anxiety != "NA")  %>%
  filter(ADHD != "NA")  %>%
  filter(Autism != "NA")

#General well-being variables: Mood.Scale, Anxiety.1, Life.Satisfaction, Financial.Situation, Romantic.Life

#Wrapping text (for the legends)

ssc2019$Depression <- paste(str_wrap(ssc2019$Depression, 25), "\n");
ssc2019$Anxiety <- paste(str_wrap(ssc2019$Anxiety, 25), "\n");
ssc2019$Autism <- paste(str_wrap(ssc2019$Autism, 25), "\n");
ssc2019$ADHD <- paste(str_wrap(ssc2019$ADHD, 25), "\n");

#Creating summary variables for each mental disorder in the survey

summaryDepression <- ssc2019 %>% 
  group_by(Depression)  %>% 
  summarise(
    count = n(), 
    percentage = (n()/length(ssc2019$Depression))*100
  );

summaryAnxiety <- ssc2019 %>% 
  group_by(Anxiety)  %>% 
  summarise(
    count = n(), 
    percentage = (n()/length(ssc2019$Anxiety))*100
  );

summaryAutism <- ssc2019 %>% 
  group_by(Autism)  %>% 
  summarise(
    count = n(), 
    percentage = (n()/length(ssc2019$Autism))*100
  );

summaryADHD <- ssc2019 %>% 
  group_by(ADHD)  %>% 
  summarise(
    count = n(), 
    percentage = (n()/length(ssc2019$ADHD))*100
  );


#Defining the plot theme

plottheme <- theme(
  text = element_text(family = "Charter", color = "#333333", size = (5)),
  plot.background = element_rect(fill = "#ffffff"),
  plot.title = element_text(family = "Charter", color = "#333333", size = (20), hjust=0.5, face = "italic"),
  plot.subtitle = element_text(family = "Charter", color = "#333333", size = (10), face = "italic"),
  plot.caption = element_text(family = "Charter", color = "#333333", size = (10), face = "italic", hjust=0.85),
  panel.background = element_rect(fill = alpha("#f5f5f5", 1)),
  panel.grid.major.y = element_line(color = alpha("#ffffff", 1)),
  panel.grid.minor.y = element_line(color = alpha("#ffffff", 0.5)),
  panel.grid.major.x = element_line(color = alpha("#ffffff", 1)),
  panel.grid.minor.x = element_line(color = alpha("#ffffff", 0.5)),
  legend.title = element_text(family = "Charter", color = "#333333", size = (12), face = "bold"),
  legend.text = element_text(family = "Charter", color = "#333333", size = (10)),
  legend.background = element_rect(fill = "#f9f9f9"),
  legend.key = element_rect(fill = "#f9f9f9", color = "#f9f9f9"),
  axis.text = element_text(family = "Charter", color = "#333333", size = (10)),
  axis.ticks.x = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(family = "Charter", color = "#333333", size = (10)),
  axis.title = element_text(family = "Charter", color = "#333333", size = (15), face = "italic")
);

#Defining each box plot

d <- ggplot(ssc2019, aes(x = Depression, y = Mood.Scale, color = Depression)) +
  stat_compare_means(
    label = "p.signif", 
    method = "t.test", 
    ref.group = ssc2019$Depression[3],
    label.y = 10.5
  ) +
  geom_jitter(alpha = 0.02) +
  geom_boxplot(notch=TRUE, na.rm = TRUE, alpha = 0.75, outlier.alpha = 0) + 
  plottheme + 
  scale_color_discrete(name = "Legend") + 
  scale_x_discrete(position = "top") +
  guides(color = guide_legend(override.aes = list(shape = 22,
                                                  size = 1))) +
  scale_y_continuous(breaks = round(seq(min(0), max(10), by = 1),1)) +
  geom_text(
    data=summaryDepression, 
    aes(x = Depression, y = 1, label=paste0(sprintf("%0.1f",percentage),"%","\n(","N = ", count,")")), 
    color="black", 
    size=2.5,
    family="Charter",
    fontface="bold"
  );

an <- d;
an$mapping$x <- ssc2019$Anxiety;
an$mapping$colour <- ssc2019$Anxiety;




an <- ggplot(ssc2019, aes(x = Anxiety, y = Mood.Scale, color = Anxiety)) +
  stat_compare_means(
    label = "p.signif", 
    method = "t.test", 
    ref.group = ssc2019$Depression[3],
    label.y = 10.5
  ) +
  geom_jitter(alpha = 0.02) +
  geom_boxplot(notch=TRUE, na.rm = TRUE, alpha = 0.75, outlier.alpha = 0) + 
  scale_x_discrete(position = "top") +
  guides(color = guide_legend(override.aes = list(shape = 22,
                                                  size = 1))) +
  plottheme + 
  scale_color_discrete(name = "") + 
  scale_y_continuous(breaks = round(seq(min(0), max(10), by = 1),1)) +
  geom_text(
    data=summaryAnxiety, 
    aes(x = Anxiety, y = 1, label=paste0(sprintf("%0.1f",percentage),"%","\n(","N = ", count,")")), 
    color="black", 
    size=2.5,
    family="Charter",
    fontface="bold"
  );

au <- ggplot(ssc2019, aes(x = Autism, y = Mood.Scale, color = Autism)) +
  stat_compare_means(
    label = "p.signif", 
    method = "t.test", 
    ref.group = ssc2019$Depression[3],
    label.y = 10.5
  ) +
  geom_jitter(alpha = 0.02) +
  geom_boxplot(notch=TRUE, na.rm = TRUE, alpha = 0.75, outlier.alpha = 0) + 
  scale_x_discrete(position = "top") +
  guides(color = guide_legend(override.aes = list(shape = 22,
                                                  size = 1))) +
  plottheme + 
  scale_color_discrete(name = "") + 
  scale_y_continuous(breaks = round(seq(min(0), max(10), by = 1),1)) +
  geom_text(
    data=summaryAutism, 
    aes(x = Autism, y = 1, label=paste0(sprintf("%0.1f",percentage),"%","\n(","N = ", count,")")), 
    color="black", 
    size=2.5,
    family="Charter",
    fontface="bold"
  );

adhd <- ggplot(ssc2019, aes(x = ADHD, y = Mood.Scale, color = ADHD)) +
  stat_compare_means(
    label = "p.signif", 
    method = "t.test", 
    ref.group = ssc2019$Depression[3],
    label.y = 10.5
  ) +
  geom_jitter(alpha = 0.02) +
  geom_boxplot(notch=TRUE, na.rm = TRUE, alpha = 0.75, outlier.alpha = 0) + 
  scale_x_discrete(position = "top") +
  guides(color = guide_legend(override.aes = list(shape = 22,
                                                  size = 1))) +
  plottheme + 
  scale_color_discrete(name = "") + 
  scale_y_continuous(breaks = round(seq(min(0), max(10), by = 1),1)) +
  geom_text(
    data=summaryADHD, 
    aes(x = ADHD, y = 1, label=paste0(sprintf("%0.1f",percentage),"%","\n(","N = ", count,")")), 
    color="black", 
    size=2.5,
    family="Charter",
    fontface="bold"
  );

#Creating a legend

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

mylegend<-g_legend(d);

#Organizing the plots and the legend in a grid

png(file="png-files/moodandmentalhealth4.png", width = 1750, height = 550, type = "quartz", res = 120);

grid <- grid.arrange(
  arrangeGrob(
    d + theme(legend.position="none", axis.title.y = element_blank()),
    an + theme(legend.position="none", axis.title.y = element_blank()),
    adhd + theme(legend.position="none", axis.title.y = element_blank()),
    au + theme(legend.position="none", axis.title.y = element_blank()),
    mylegend,
    nrow = 1
  ),
  top = grid.text(
    "\n Answers to “How would you rate your usual mood?” \n based on the most common mental disorders \n",
    gp = gpar(
      fontsize = 17, 
      col = "#333333",
      fontfamily="Charter"
    )
  ), 
  left = grid.text(
    "\n Mood rating \n", 
    gp = gpar(
      fontsize = 16,
      col = "#000000",
      fontfamily = "Charter",
      fontface = "italic"
    ),
    rot = 90
  ),
  bottom = grid.text(
    "ns: p > 0.05, *: p < 0.05, **: p < 0.01, ***: p < 0.001, ****: p < 0.0001.\n All groups were compared with the diagnostic group (green).\n Source: SlateStarCodex’s 2019 survey \n", 
    gp = gpar(
      fontsize = 9, 
      col = "#111111", 
      fontfamily = "Charter", 
      fontface = "italic"
    )
  )
);

dev.off()
