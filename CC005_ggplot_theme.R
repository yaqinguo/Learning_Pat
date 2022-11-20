library(tidyverse)

candy_data <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/candy-power-ranking/candy-data.csv",
                       col_types="clllllllllddd")

pluribus_data <- candy_data %>%
  filter(pluribus & (chocolate | fruity)) %>%
  pivot_longer(cols=c(chocolate, fruity), names_to="type", values_to="answer") %>%
  filter(answer)
p <- pluribus_data %>%
  ggplot(aes(x=type, y=winpercent, color=type)) +
  geom_jitter(width=0.1) +
  scale_x_discrete(breaks=c("chocolate", "fruity"), labels=c("Chocolate", "Fruity")) +
  coord_cartesian(ylim=c(0,100)) +
  labs(y="Contests won (%)",
       x="Type of candy",
       title="Bite-sized candies containing chocolate are preferred to candy without",
       subtitle="Data collected by FiveThirtyEight.com")

p + theme_gray()
p + theme_classic()
p + theme_bw()
p + theme_dark()
p + theme_light()
p + theme_void()

p + theme_excel()

p + theme_classic(base_size=20,base_family="mono",base_line_size=2)

p + theme(plot.title = element_text(size=rel(2),face = "bold",color = "red"),
          panel.background = element_rect(fill = "hotpink"),
          axis.title=element_text(size=rel(0.8),family = "mono"),
          panel.grid.major = element_line(color = "black"))
p + theme(axis.line = element_line(lineend = "round"),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          legend.key = element_blank()
          )
?theme
p +theme_dark()
p + theme(panel.background = element_rect(fill = "gray50"),
          legend.key = element_rect(fill = "gray50"),
          panel.grid = element_line(color = "gray40"))
p + theme_fivethirtyeight()
p + theme(plot.background = element_rect(fill = "gray95"),
          axis.line = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "gray40"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          legend.key = element_blank(),
          legend.background = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size = rel(2),face="bold")
          )
