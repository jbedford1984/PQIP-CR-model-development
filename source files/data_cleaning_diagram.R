## Create consort flow diagram for prediction modelling paper

## create plot grid 100 x 100
Continuous_data_cleaning_diagram <- tibble(x= 1:100, y= 1:100) %>% 
  ggplot(aes(x, y)) +
  scale_x_continuous(minor_breaks = seq(5, 100, 1), breaks=seq(5, 100, 5)) +
  scale_y_continuous(minor_breaks = seq(5, 100, 1)) +
  theme_linedraw() +
  ## add first text box to diagram
  geom_rect(xmin = 32, xmax=68, ymin=94, ymax=100, color='black',
            fill='white', size=0.25, size=0.25) +
  annotate('text', x= 50, y=97,label = expression(paste("Remove body mass index values <12 and >60kg","",m^{2}^{-1})), size=2.5) +
  # add subsequent boxes
  geom_rect(xmin = 36, xmax=64, ymin=82, ymax=88, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=85,label= expression(paste("Remove sodium values >160", "",mmolL^{-1})), size=2.5) +
  ## add arrows
  geom_segment(
    x=50, xend=50, y=94, yend=88.3, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=69.7, y=92, yend=92, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  # add information regarding removal of non locked cases
  geom_rect(xmin = 70, xmax=97, ymin=89, ymax=95, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 83.5, y=92,label= paste0('9 values set to NA'), size=2.5) +
  ## number of cases operated on or before 30th march 2020
  geom_rect(xmin = 36, xmax=64, ymin=70, ymax=76, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=73,label= expression(paste("Remove potassium values >6.5", "",mmolL^{-1})), size=2.5) +
  ## add arrows
  geom_segment(
    x=50, xend=50, y=82, yend=76.3, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=69.7, y=79, yend=79, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  # add information regarding removal of Na+ values
  geom_rect(xmin = 70, xmax=97, ymin=76, ymax=82, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 83.5, y=79,label= paste0('1 value set to NA'), size=2.5) +
  ## number of colorectal cases in dataset
  geom_rect(xmin = 36, xmax=64, ymin=58, ymax=64, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=61,label= expression(paste("Remove urea values >40", "",mmolL^{-1})), size=2.5) +
  ## add arrow
  geom_segment(
    x=50, xend=50, y=70, yend=64.3, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=69.7, y=67, yend=67, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  # add information regarding removal of K+ >6.5mmol/L cases
  geom_rect(xmin = 70, xmax=97, ymin=64, ymax=70, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 83.5, y=67,label= paste0('2 values set to NA'), size=2.5) +
  ## cases included in final analysis
  ## add arrow
  geom_segment(
    x=50, xend=50, y=58, yend=52.3, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=69.7, y=55, yend=55, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) + 
  # add information regarding removal of urea >40mmol/L cases
  geom_rect(xmin = 70, xmax=97, ymin=52, ymax=58, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 83.5, y=55,label= paste0('13 values set to NA'), size=2.5) +
  ## number of WCC values set to NA
  geom_rect(xmin = 36, xmax=64, ymin=46, ymax=52, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=49,label= expression(paste("Remove white cell count values >40", "",x10^{-9}, L^{-1})), size=2.5) +
  ## add arrow
  geom_segment(
    x=50, xend=50, y=46, yend=40.3, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=69.7, y=43, yend=43, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) + 
  # add information regarding removal of urea >40mmol/L cases
  geom_rect(xmin = 70, xmax=97, ymin=40, ymax=46, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 83.5, y=43,label= paste0('16 values set to NA'), size=2.5) +
  ## number of Hb values set to NA
  geom_rect(xmin = 36, xmax=64, ymin=34, ymax=40, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=37,label= expression(paste("Remove haemoglobin values <70", "",gL^{-1})), size=2.5) +
  ## add arrow
  geom_segment(
    x=50, xend=50, y=34, yend=28.3, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=69.7, y=31, yend=31, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) + 
  # add information regarding removal of haemoglobin <70g/L cases
  geom_rect(xmin = 70, xmax=97, ymin=28, ymax=34, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 83.5, y=31,label= paste0('15 values set to NA'), size=2.5) +
  ## number of pulse rate values set to NA
  geom_rect(xmin = 36, xmax=64, ymin=22, ymax=28, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=25,label= paste("Remove heart rate values >150bpm"), size=2.5) +
  ## add arrow
  geom_segment(
    x=50, xend=50, y=22, yend=16.3, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=69.7, y=19, yend=19, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) + 
  # add information regarding removal of BP cases
  geom_rect(xmin = 70, xmax=97, ymin= 16, ymax=22, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 83.5, y=19,label= paste0('4 values set to NA'), size=2.5) +
  # number of BP values set to NA
  geom_rect(xmin = 36, xmax=64, ymin=10, ymax=16, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=13,label= paste("Remove systolic blood pressure values\n>250mmHg or < 70mmHg"), size=2.5) +
  ## add arrow
  geom_segment(
    x=50, xend=50, y=10, yend=4.3, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=69.7, y=7, yend=7, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) + 
  # add information regarding removal of haemoglobin <70g/L cases
  geom_rect(xmin = 70, xmax=97, ymin= 4, ymax=10, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 83.5, y=7,label= paste0('12 values set to NA'), size=2.5) +
  # number of O2 sats values set to NA
  geom_rect(xmin = 36, xmax=64, ymin=-2, ymax=4, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=1,label= paste("Remove oxygen saturation levels <85%"), size=2.5) +
  ## add arrow
  geom_segment(
    x=50, xend=50, y=-2, yend=-7.7, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=69.7, y=-5, yend=-5, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) + 
  # add information regarding removal of haemoglobin <70g/L cases
  geom_rect(xmin = 70, xmax=97, ymin= -2, ymax=-8, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 83.5, y=-5,label= paste0('12 values set to NA'), size=2.5) +
  # number of HbA1c values >20 set to NA
  geom_rect(xmin = 36, xmax=64, ymin=-14, ymax=-8, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=-11,label= paste("Remove HbA1c values >20%"), size=2.5) +
  ## add arrow
  geom_segment(
    x=50, xend=50, y=-14, yend=-19.7, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=69.7, y=-17, yend=-17, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) + 
  # add information regarding removal of haemoglobin <70g/L cases
  geom_rect(xmin = 70, xmax=97, ymin= -20, ymax=-14, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 83.5, y=-17,label= paste0('2 values set to NA'), size=2.5) +
  # number of HbA1c values >20 set to NA
  geom_rect(xmin = 36, xmax=64, ymin=-26, ymax=-20, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=-23,label= paste("End of removal of implausible values"), size=2.5) +
  theme_void()

#save plot
ggsave(filename = "./figures/Continuous_data_cleaning_diagram.eps", plot = Continuous_data_cleaning_diagram, 
       dpi = 500, width = 200, height = 200, units = "mm", device = cairo_ps)
