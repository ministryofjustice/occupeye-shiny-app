library(ggplot2)
library(scales)

prop_daily_usage_chart <- function(df_sum) {
  
  ggplot(df_sum,
         aes(x=date,y=utilisation,fill=util_cat)) +
    geom_bar(stat="identity", position='fill') +
    ggtitle("Desk Utilisation By Day") +
    labs(y="Desk Utilisation",fill="") +
    labs(x=NULL,fill="") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_date(breaks = pretty_breaks(30)) +
    theme(legend.position="right") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("Effective utilisation"="coral2","Under utilised"="thistle3","Unused"="powderblue")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size=10))
  
}

