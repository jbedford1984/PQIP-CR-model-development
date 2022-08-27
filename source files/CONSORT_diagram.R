## Create consort flow diagram for prediction modelling paper

## create plot grid 100 x 100
CONSORT_diagram <- tibble(x= 1:100, y= 1:100) %>% 
  ggplot(aes(x, y)) +
  theme(element_text(family = "Calibri")) +
  scale_x_continuous(minor_breaks = seq(5, 100, 1), breaks=seq(5, 100, 5)) +
  scale_y_continuous(minor_breaks = seq(5, 100, 1)) +
  theme_linedraw() +
  ## add first text box to diagram
  geom_rect(xmin = 36, xmax=64, ymin=94, ymax=100, color='black',
            fill='white', size=0.25, size=0.25) +
  annotate('text', x= 50, y=97,label= paste0('Cases exported from study database\nn=', comma(n_total_export)), size=2.5) +
  # add subsequent boxes
  geom_rect(xmin = 36, xmax=64, ymin=76, ymax=82, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=79,label= paste0('Locked cases\nn=', comma(n_locked)), size=2.5) +
  ## add arrows
  geom_segment(
    x=50, xend=50, y=94, yend=82.3, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=69.7, y=89, yend=89, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  # add information regarding removal of non locked cases
  geom_rect(xmin = 70, xmax=97, ymin=85, ymax=93, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 83.5, y=89,label= paste0('Unlocked case records excluded\nn=', comma(n_total_export-n_locked)), size=2.5) +
  ## number of cases operated on or before 30th march 2020
  geom_rect(xmin = 32, xmax=68, ymin=58, ymax=64, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=61,label= paste0('Cases undergoing surgery on or before 31st March 2020\nn=', comma(n_eligble_date)), size=2.5) +
  ## add arrows
  geom_segment(
    x=50, xend=50, y=76, yend=64.3, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=69.7, y=70, yend=70, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  # add information regarding removal of surgery > 30th March 2020
  geom_rect(xmin = 70, xmax=97, ymin=66, ymax=74, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 83.5, y=70,label= paste0('Cases excluded due to undergoing\nsurgery after 31st March 2020\nn=', comma(n_locked-n_eligble_date)), size=2.5) +
  ## number of colorectal cases in dataset
  geom_rect(xmin = 35, xmax=65, ymin=40, ymax=46, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=43,label= paste0('Cases undergoing colorectal surgery\nn=', comma(n_colorectal_eligible)), size=2.5) +
  ## add arrow
  geom_segment(
    x=50, xend=50, y=58, yend=46.3, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=69.7, y=52, yend=52, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  # add information regarding removal of non-colorectal cases
  geom_rect(xmin = 70, xmax=97, ymin=49, ymax=55, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 83.5, y=52,label= paste0('Non-colorectal cases excluded\nn=', comma(n_eligble_date-n_colorectal_eligible)), size=2.5) +
  ## cases included in final analysis
  geom_rect(xmin = 35, xmax=65, ymin=22, ymax=28, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=25,label= paste0('Cases included in final analysis\nn=', comma(n_final_analysis)), size=2.5) +
  ## add arrow
  geom_segment(
    x=50, xend=50, y=40, yend=28.3, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=68.7, y=34, yend=34, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  # add information regarding missing outcome data
  geom_rect(xmin = 69, xmax=98, ymin=28, ymax=40, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 83.5, y=34,label= paste0('Cases missing outcome measure\nn=', comma(n_colorectal_eligible-n_final_analysis),
                                               "\n",
                                               "\n",
                                               n.withdrawl," cases withdrew before postoperative day 7\n",
                                               length(LOS.cleaning) + length(missing.outcome), " missing outcome"), size=2.5) +
  theme_void()

#save plot
library(grDevices)
ggsave(filename = "./figures/CONSORT.eps", plot = CONSORT_diagram, 
         dpi = 500, width = 220, height = 220, units = "mm", device = cairo_ps)


