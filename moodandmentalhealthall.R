#First of all, clearing the existing environment

rm(list=ls());

#Defining the requred packages

requiredPackages <- c("ggplot2","openxlsx","stringr","gridExtra","knitr","dplyr","EnvStats","stats","ggpubr","grid","gtable","reshape2");

#Installing the unavailable ones
unavailablePackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])];
if(length(unavailablePackages)) install.packages(unavailablePackages);

#Loading all of the requred packages into the environment
lapply(requiredPackages, library, character.only = TRUE);

#Downloading the Excel spreadhseet from the SSC website
ssc2019 <- read.xlsx("https://slatestarcodex.com/Stuff/ssc2019_public.xlsx");

#Making sure column names are unique
colnames(ssc2019) <- make.unique(names(ssc2019));

#Replacing periods for whitespace
#colnames(ssc2019) <- gsub(".", " ", colnames(ssc2019), fixed = TRUE)

#Filtering null values. Listwise for now, because I am lazy. Will change for pairwise later.
ssc2019 <- ssc2019 %>% 
  filter(Depression != "NA")  %>%
  filter(Anxiety != "NA")  %>%
  filter(ADHD != "NA")  %>%
  filter(OCD != "NA")  %>%
  filter(Eating.disorder != "NA")  %>%
  filter(Alcoholism != "NA")  %>%
  filter(Drug.addiction != "NA")  %>%
  filter(Borderline != "NA")  %>%
  filter(Bipolar != "NA")  %>%
  filter(Schizophrenia != "NA")  %>%
  filter(Autism != "NA")

#General well-being variables: Mood.Scale, Anxiety.1, Life.Satisfaction, Financial.Situation, Romantic.Life

#Wrapping text (for the legends)
ssc2019$Depression <- paste(str_wrap(ssc2019$Depression, 25), "\n");

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

summaryOCD <- ssc2019 %>% 
  group_by(OCD)  %>% 
  summarise(
    count = n(), 
    percentage = (n()/length(ssc2019$OCD))*100
  );

summaryEating.disorder <- ssc2019 %>% 
  group_by(Eating.disorder)  %>% 
  summarise(
    count = n(), 
    percentage = (n()/length(ssc2019$Eating.disorder))*100
  );

summaryAlcoholism <- ssc2019 %>% 
  group_by(Alcoholism)  %>% 
  summarise(
    count = n(), 
    percentage = (n()/length(ssc2019$Alcoholism))*100
  );

summaryDrug.addiction <- ssc2019 %>% 
  group_by(Drug.addiction)  %>% 
  summarise(
    count = n(), 
    percentage = (n()/length(ssc2019$Drug.addiction))*100
  );

summaryBipolar <- ssc2019 %>% 
  group_by(Bipolar)  %>% 
  summarise(
    count = n(), 
    percentage = (n()/length(ssc2019$Bipolar))*100
  );

summarySchizophrenia <- ssc2019 %>% 
  group_by(Schizophrenia)  %>% 
  summarise(
    count = n(), 
    percentage = (n()/length(ssc2019$Schizophrenia))*100
  );

summaryBorderline <- ssc2019 %>% 
  group_by(Borderline)  %>% 
  summarise(
    count = n(), 
    percentage = (n()/length(ssc2019$Borderline))*100
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

an <- ggplot(ssc2019, aes(x = Anxiety, y = Mood.Scale, color = Anxiety)) +
  stat_compare_means(
    label = "p.signif", 
    method = "t.test", 
    ref.group = ssc2019$Anxiety[3],
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
    ref.group = ssc2019$Anxiety[3],
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
    ref.group = ssc2019$Anxiety[3],
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

bipolar <- ggplot(ssc2019, aes(x = Bipolar, y = Mood.Scale, color = Bipolar)) +
  stat_compare_means(
    label = "p.signif", 
    method = "t.test", 
    ref.group = ssc2019$Anxiety[3],
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
    data=summaryBipolar, 
    aes(x = Bipolar, y = 1, label=paste0(sprintf("%0.1f",percentage),"%","\n(","N = ", count,")")), 
    color="black", 
    size=2.5,
    family="Charter",
    fontface="bold"
  );


sch <- ggplot(ssc2019, aes(x = Schizophrenia, y = Mood.Scale, color = Schizophrenia)) +
  stat_compare_means(
    label = "p.signif", 
    method = "t.test", 
    ref.group = ssc2019$Anxiety[3],
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
    data=summarySchizophrenia, 
    aes(x = Schizophrenia, y = 1, label=paste0(sprintf("%0.1f",percentage),"%","\n(","N = ", count,")")), 
    color="black", 
    size=2.5,
    family="Charter",
    fontface="bold"
  );


da <- ggplot(ssc2019, aes(x = Drug.addiction, y = Mood.Scale, color = Drug.addiction)) +
  stat_compare_means(
    label = "p.signif", 
    method = "t.test", 
    ref.group = ssc2019$Anxiety[3],
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
    data=summaryDrug.addiction, 
    aes(x = Drug.addiction, y = 1, label=paste0(sprintf("%0.1f",percentage),"%","\n(","N = ", count,")")), 
    color="black", 
    size=2.5,
    family="Charter",
    fontface="bold"
  );


ed <- ggplot(ssc2019, aes(x = Eating.disorder, y = Mood.Scale, color = Eating.disorder)) +
  stat_compare_means(
    label = "p.signif", 
    method = "t.test", 
    ref.group = ssc2019$Anxiety[3],
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
    data=summaryEating.disorder, 
    aes(x = Eating.disorder, y = 1, label=paste0(sprintf("%0.1f",percentage),"%","\n(","N = ", count,")")), 
    color="black", 
    size=2.5,
    family="Charter",
    fontface="bold"
  );


ocd <- ggplot(ssc2019, aes(x = OCD, y = Mood.Scale, color = OCD)) +
  stat_compare_means(
    label = "p.signif", 
    method = "t.test", 
    ref.group = ssc2019$Anxiety[3],
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
    data=summaryOCD, 
    aes(x = OCD, y = 1, label=paste0(sprintf("%0.1f",percentage),"%","\n(","N = ", count,")")), 
    color="black", 
    size=2.5,
    family="Charter",
    fontface="bold"
  );

alc <- ggplot(ssc2019, aes(x = Alcoholism, y = Mood.Scale, color = Alcoholism)) +
  stat_compare_means(
    label = "p.signif", 
    method = "t.test", 
    ref.group = ssc2019$Anxiety[3],
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
    data=summaryAlcoholism, 
    aes(x = Alcoholism, y = 1, label=paste0(sprintf("%0.1f",percentage),"%","\n(","N = ", count,")")), 
    color="black", 
    size=2.5,
    family="Charter",
    fontface="bold"
  );

bpd <- ggplot(ssc2019, aes(x = Borderline, y = Mood.Scale, color = Borderline)) +
  stat_compare_means(
    label = "p.signif", 
    method = "t.test", 
    ref.group = ssc2019$Anxiety[3],
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
    data=summaryBorderline, 
    aes(x = Borderline, y = 1, label=paste0(sprintf("%0.1f",percentage),"%","\n(","N = ", count,")")), 
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

sigTable <- matrix(c("ns:","p > 0.05","*:","p < 0.05","**:","p < 0.01","***:","p < 0.001","****:","p < 0.0001"),ncol=2,byrow=TRUE);
colnames(sigTable) <- c("","");
rownames(sigTable) <- c("","","","","");
sigTable <- as.table(sigTable);

#Organizing the plots and the legend in a grid

png(file="png-files/moodandmentalhealthall.png", width = 2600, height = 1000, type = "quartz", res = 150);

grid <- grid.arrange(
  arrangeGrob(
    d + theme(legend.position="none", axis.title.y = element_blank()),
    an + theme(legend.position="none", axis.title.y = element_blank()),
    adhd + theme(legend.position="none", axis.title.y = element_blank()),
    au + theme(legend.position="none", axis.title.y = element_blank()),
    mylegend,
    nrow = 1
  ),
  arrangeGrob(
    ocd + theme(legend.position="none", axis.title.y = element_blank()),
    bipolar + theme(legend.position="none", axis.title.y = element_blank()),
    ed + theme(legend.position="none", axis.title.y = element_blank()),
    da + theme(legend.position="none", axis.title.y = element_blank()),
    sch + theme(legend.position="none", axis.title.y = element_blank()),    
    bpd + theme(legend.position="none", axis.title.y = element_blank()),
    alc + theme(legend.position="none", axis.title.y = element_blank()),
    nrow = 1
  ),
  top = grid.text(
    substitute(paste("\n\n Answers to ",italic("“How would you rate your usual mood?”"), " based on mental disorders")),
    gp = gpar(
      fontsize = 22, 
      col = "#333333",
      fontfamily="Charter"
    )
  ), 
  left = grid.text(
    "\n Mood rating \n", 
    gp = gpar(
      fontsize = 17,
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
