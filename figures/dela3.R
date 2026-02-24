library(ggplot2)
library(dplyr)

raw <- read.csv('velocity.csv',header=TRUE)
data <- tibble(raw)

fig6a <- ggplot(data,aes(x=t_s,y=v_ms,color=type))+
     geom_point()
