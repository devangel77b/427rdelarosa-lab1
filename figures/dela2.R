library(ggplot2)
library(dplyr)

raw <- read.csv('times.csv',header=TRUE)
data <- tibble(raw)
data <- mutate(data,
     a = 2*5.0/t_s^2)
grouped <- dplyr::group_by(data,type)
summary_data <- dplyr::summarise(grouped,
	     m = mean(t_s),
	     sd = sd(t_s),
	     ma = mean(a),
	     sda = sd(a))
	     
fig <- ggplot(summary_data) +
    geom_hline(yintercept=0,color="gray70") + 
    geom_bar( aes(x=type, y=m), stat="identity", fill="gray70",width=0.5) +
    geom_errorbar( aes(x=type, ymin=m-sd, ymax=m+sd), width=0.3) +
    theme_bw(base_size=8) +
    theme(axis.title.x = element_blank()) +
    #xlab("ball type") + 
    ylab("$t$, \\unit{\\second}") +
    theme(axis.title.y=element_text(margin=margin(t=0,r=3,b=0,l=0,unit="pt")))

# save the figure
ggsave("fig5a.svg", plot=fig, width=1.708, height=2, units="in")

# give me the t-test results 
print(t.test(t_s ~ type, data))




figB <- ggplot(summary_data) +
    geom_hline(yintercept=0,color="gray70") + 
    geom_bar( aes(x=type, y=ma), stat="identity", fill="gray70",width=0.5) +
    geom_errorbar( aes(x=type, ymin=ma-sda, ymax=ma+sda), width=0.3) +
    theme_bw(base_size=8) +
    theme(axis.title.x = element_blank()) +
    #xlab("ball type") + 
    ylab("$a$, \\unit{\\meter\\per\\second\\squared}") +
    theme(axis.title.y=element_text(margin=margin(t=0,r=3,b=0,l=0,unit="pt")))

# save the figure
ggsave("fig5b.svg", plot=figB, width=1.708, height=2, units="in")

# give me the t-test results 
print(t.test(a ~ type, data))
