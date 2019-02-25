#This R script creates a correlation matrix between the well-being/happiness related variables in the 2019 SlateStarCodex survey and saves it in a .png image. 
#The diagonals in the matrix (where 


#First of all, clearing the existing environment

rm(list=ls());

#Defining the requred packages, installing the unavailable ones, and loading them into the environment

requiredPackages <- c("ggplot2","openxlsx","stringr","gridExtra","knitr","dplyr","EnvStats","stats","ggpubr","grid","gtable","reshape2","GGally");
unavailablePackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])];
if(length(unavailablePackages)) install.packages(unavailablePackages);
lapply(requiredPackages, library, character.only = TRUE);

#Downloading the Excel spreadsheet from the SSC website

ssc2019 <- read.xlsx("https://slatestarcodex.com/Stuff/ssc2019_public.xlsx");

#Making sure column names are unique

colnames(ssc2019) <- make.unique(names(ssc2019));

#Filtering null values
ssc2019 <- ssc2019 %>% 
  filter(Mood.Scale != "NA")  %>%
  filter(Life.Satisfaction != "NA")  %>%
  filter(Anxiety.1 != "NA")  %>%
  filter(Ambition != "NA")  %>%
  filter(Status != "NA")  %>%
  filter(Financial.Situation != "NA")  %>%
  filter(Social.Skills != "NA")  %>%
  filter(Romantic.Life != "NA");
  
#Reverse-coding the anxiety variables, so that higher values indicate lower anxiety
ssc2019$Anxiety.Reversed <- 11 - ssc2019$Anxiety.1;

#Defining the plot theme
plottheme <- theme(
  plot.background = element_rect(fill = "#ffffff"),
  plot.title = element_text(family = "Charter", color = "#333333", size = (20), face = "bold"),
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
  axis.title = element_text(family = "Charter", color = "#333333", size = (20), face = "bold"),
);
  
#Subsetting the survey spreadsheet to only the desired variables
subset <- select(ssc2019, Mood.Scale, Life.Satisfaction, Anxiety.Reversed, Ambition, Status, Social.Skills, Financial.Situation, Romantic.Life);

length <- length(subset);

#Making a list with all of the plots to be graphed
plots <- list();

for (i in 0:(length*length - 1))
 local({
 
 i <- i;
 row <- floor(i/length);
 xValue <- row + 1;
 
 if (i %% (length + 1) == 0) {
    plots[[i + 1]] <<- ggplot(subset, aes(subset[[xValue]])) +
                        geom_bar() +
                        plottheme +
                        scale_x_continuous(breaks = round(seq(min(0), max(10), by = 1),1)) +
                        labs(y="",x="",title="");
   } else {
    if (i %% length <= row) {
      plots[[i + 1]] <<- ggplot(subset, aes(x=subset[[xValue]], y=subset[[i %% length + 1]])) +
                          geom_density2d() +
                          plottheme +
                          scale_x_continuous(breaks = round(seq(min(0), max(10), by = 1),1)) +
                          scale_y_continuous(breaks = round(seq(min(0), max(10), by = 1),1)) +
                          labs(y="",x="",title="");
    } else {
      plots[[i + 1]] <<- ggplot(subset, aes(x = subset[[xValue]], y = subset[[i %% length + 1]])) + 
                          geom_jitter(alpha=0.01, size=5, color="black") +
                          stat_cor(method = "pearson", color="black", size=5, family="Charter", fontface="bold") +
                          plottheme +
                          scale_x_continuous(breaks = round(seq(min(0), max(10), by = 1),1)) +
                          scale_y_continuous(breaks = round(seq(min(0), max(10), by = 1),1)) +
                          labs(y="",x="",title="");
    }
   }
   if (row==0) {
    plots[[i+1]] <<- plots[[i+1]] + labs(title=names(subset[i%%length+1]));
   }
   if(i %% length == 0) {
    plots[[i+1]] <<- plots[[i+1]] + labs(y=names(subset[xValue]));
   }
})

#Finally, graphing the plots in a grid and saving the result to png-files/wellbeingmatrix.png.

png(file="png-files/wellbeingmatrix.png", width = 3200, height = 3200, type = "quartz", res = 150);

grid <- grid.arrange(
  grobs = plots, 
  ncol=length, 
  nrow=length, 
  top = grid.text(
    "\n Well-being variables in the SSC survey \n", 
    gp = gpar(fontsize = 37, col = "#333333", fontfamily="Charter")), 
  bottom = grid.text(
    "\n Source: SlateStarCodex 2019 Survey \n", 
    gp=gpar(fontsize=20, col="#111111",fontfamily="Charter",fontface="italic")
  )
);

dev.off()
