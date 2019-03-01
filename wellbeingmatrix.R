#First of all, clearing the existing environment
rm(list=ls());

#Unloading currently loaded packages
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)

#Defining the required packages, installing the unavailable ones, and loading them into the environment
requiredPackages <- c("ggplot2","openxlsx","stringr","gridExtra","knitr","dplyr","ggpubr","grid","gtable","reshape2","PerformanceAnalytics");
unavailablePackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])];
if(length(unavailablePackages)) install.packages(unavailablePackages);
lapply(requiredPackages, library, character.only = TRUE);

#Downloading our data, the Excel spreadsheet with survey responses hosted in SSC's website
ssc2019 <- read.xlsx("https://slatestarcodex.com/Stuff/ssc2019_public.xlsx");

#Making sure column names are unique
colnames(ssc2019) <- make.unique(names(ssc2019));

#Filtering entries (listwise because I am lazy)
ssc2019 <- ssc2019 %>% 
  filter(Mood.Scale != "NA")  %>%
  filter(Life.Satisfaction != "NA")  %>%
  filter(Anxiety.1 != "NA")  %>%
  filter(Ambition != "NA")  %>%
  filter(Status != "NA")  %>%
  filter(Financial.Situation != "NA")  %>%
  filter(Social.Skills != "NA")  %>%
  filter(Romantic.Life != "NA");
  
#Reverse-coding the anxiety variable, so that higher values indicate lower anxiety
ssc2019$Anxiety.Reversed <- 11 - ssc2019$Anxiety.1
  
#Defining the plot theme
plottheme <- theme(
  plot.background = element_rect(fill = "#ffffff"),
  plot.title = element_text(family = "Charter", color = "#333333", size = (40), face = "bold"),
  plot.subtitle = element_blank(),
  plot.caption = element_blank(),
  panel.background = element_rect(fill = alpha("#f5f5f5", 1)),
  panel.grid.major.y = element_line(color = alpha("#ffffff", 1)),
  panel.grid.minor.y = element_line(color = alpha("#ffffff", 0.5)),
  panel.grid.major.x = element_line(color = alpha("#ffffff", 1)),
  panel.grid.minor.x = element_line(color = alpha("#ffffff", 0.5)),
  legend.title = element_blank(),
  legend.text = element_blank(),
  legend.background = element_rect(fill = "#f9f9f9"),
  legend.key = element_blank(),
  axis.text = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.title = element_text(family = "Charter", color = "#333333", size = (40), face = "bold"),
);

#Getting the well-being related columns from the original spreadsheet
subset <- select(ssc2019, Mood.Scale, Life.Satisfaction, Anxiety.Reversed, Ambition, Status, Social.Skills, Financial.Situation, Romantic.Life);

#
length <- length(subset);

#Making an empty list which will be populated with the charts to be plotted in the following for-loop
plots <- list();

#Using a for-loop to make a correlation matrix
for (i in 0:(length*length - 1))
 local({
 
 i <- i;
 row <- floor(i/length);
 
 if (i %% (length + 1) == 0) {
    plots[[i + 1]] <<- ggplot(subset, aes(subset[[row + 1]])) +
                        geom_bar() +
                        plottheme +
                        scale_x_continuous(breaks = round(seq(min(0), max(10), by = 1),1)) +
                        labs(y="",x="",title="");
   } else {
    if (i %% length <= row) {
      plots[[i + 1]] <<- ggplot(subset, aes(x=subset[[row + 1]], y=subset[[i %% length + 1]])) +
                          geom_density2d() +
                          plottheme +
                          scale_x_continuous(breaks = round(seq(min(0), max(10), by = 1),1)) +
                          scale_y_continuous(breaks = round(seq(min(0), max(10), by = 1),1)) +
                          labs(y="",x="",title="");
    } else {
      correlation <- cor.test(subset[[row + 1]], subset[[i %% length + 1]], method = "pearson", conf.level = 0.95);
      plots[[i + 1]] <<- ggplot(subset, aes(x = subset[[row + 1]], y = subset[[i %% length + 1]])) + 
                          geom_text(aes(x=1,y=2000), label = paste("R = ",round(correlation$estimate, digits = 2),"\n p = ", correlation$p.value), size=15, family="Charter",fontface="italic") +
                          plottheme +
                          scale_x_continuous(breaks = round(seq(min(0), max(10), by = 1),1)) +
                          scale_y_continuous(breaks = round(seq(min(0), max(10), by = 1),1)) +
                          labs(y="",x="",title="");
    }
   }
   if (row==0) {
    plots[[i+1]] <<- plots[[i+1]] + labs(title = paste("\n",gsub("."," ", names(subset[i%%length+1]), fixed = TRUE),"\n"));
   }
   if(i %% length == 0) {
    plots[[i+1]] <<- plots[[i+1]] + labs(y=paste("\n",gsub("."," ",names(subset[row + 1]), fixed = TRUE),"\n"));
   }
})

#Organizing the plots into a proper matrix and saving the resulting graphic to png-files/wellbeingmatrix.png
png(file="png-files/wellbeingmatrix.png", width = 7000, height = 7000, type = "quartz", res = 150);

grid <- grid.arrange(
  grobs = plots, 
  ncol=length, 
  nrow=length, 
  top = grid.text(
    "\n Well-being variables in the SSC survey \n", 
    gp = gpar(fontsize = 80, col = "#333333", fontfamily="Charter")), 
  bottom = grid.text(
    "\n Source: SlateStarCodex 2019 Survey \n", 
    gp=gpar(fontsize=40, col="#111111",fontfamily="Charter",fontface="italic")
  )
);

dev.off()
