library(ggplot2)
library(dplyr)

# make 4.1 and 4.2 together
raw41 <- read.csv('massAll.csv',header=TRUE)
data41 <- tibble(raw41)
data41 <- filter(data41,t_s>0.2)
data41 <- mutate(data41,t_s=t_s-0.2,y_m=y_m/2)
fig41 <- ggplot(data41,aes(x=t_s,y=y_m,color=type))+
      geom_hline(yintercept=0,color="gray70")+
      geom_point()+
      geom_smooth(method="lm",formula=y~I(x^2),se=FALSE)+
      xlab('$t$, \\unit{\\second}')+
      ylab('$y$, \\unit{\\meter}')+
      theme_bw(base_size=8)+
      theme(legend.position="inside",
	legend.position.inside=c(0.95,0.95),
	legend.justification.inside=c("right","top"),
	legend.key.size=unit(4,"pt"),
	legend.title=element_blank())
ggsave('fig41.svg',plot=fig41,width=3.4167,height=2,units="in")


# do stats
model1 <- lm(y_m~I(t_s^2),data41)
model2 <- lm(y_m~I(t_s^2):type,data41)

