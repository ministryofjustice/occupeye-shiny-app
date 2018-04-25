library(ggplot2)
library(scales)
library(dplyr)

prop_daily_usage_chart <- function(df_sum) {
  
  prop_usage <- df_sum %>%
    count(date,util_cat) %>%
    mutate(prop = n/n_distinct(df_sum$surveydeviceid))
  
  ggplot(prop_usage,
    aes(x=date,y=prop,fill=util_cat)) +
    geom_bar(stat="identity", position='fill') +
    ggtitle("Desk Utilisation By Date") +
    labs(y="Desk Utilisation",fill="") +
    labs(x=NULL,fill="") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_date(breaks = pretty_breaks(30)) +
    theme(legend.position="right") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("Effective utilisation"="coral2","Under utilised"="thistle3","Unused"="powderblue")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size=10))
  
}

prop_weekday_usage_chart <- function(df_sum) {
  
  prop_usage_day <- prop.table(table(weekdays(df_sum$date),df_sum$util_cat),1)
  
  prop_usage_day <- as.data.frame(prop_usage_day) %>%
    rename(day=Var1,util_cat=Var2,prop=Freq)
  
  weekday<-c("Monday","Tuesday","Wednesday","Thursday","Friday")
  
  ggplot(prop_usage_day,
    aes(x=day,y=prop,fill=util_cat)) +
    geom_bar(stat="identity", position='fill') +
    ggtitle("Desk Utilisation By Day") +
    labs(y="Desk Utilisation",fill="") +
    labs(x=NULL,fill="") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(limits = weekday) +
    theme(legend.position="right") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("Effective utilisation"="coral2","Under utilised"="thistle3","Unused"="powderblue")) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=10))
  
  
  
}


prop_desk_usage_chart <- function(df_sum) {
  
  prop_usage_type <- prop.table(table(df_sum$devicetype,df_sum$util_cat),1)
  
  prop_usage_type <- as.data.frame(prop_usage_type) %>%
    rename(desk_type=Var1,util_cat=Var2,prop=Freq)
  

  
  ggplot(prop_usage_type,
         aes(x=desk_type,y=prop,fill=util_cat)) +
    geom_bar(stat="identity", position='fill') +
    ggtitle("Desk Utilisation By desk type") +
    labs(y="Desk Utilisation",fill="") +
    labs(x=NULL,fill="") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position="right") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("Effective utilisation"="coral2","Under utilised"="thistle3","Unused"="powderblue")) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=10))
  
  
  
}


allocation_strategy_table <- function(df_sum) {
  
  current_allocation <- n_distinct(df_sum$surveydeviceid)
  
  prop_usage <- prop.table(table(df_sum$date,df_sum$util_cat),1)
  prop_usage_day <- prop.table(table(weekdays(df_sum$date),df_sum$util_cat),1)
  
  min_unused <- min(prop_usage[,"Unused"])
  
  av.unutilise <- mean(prop_usage_day[,"Unused"])
  full_smoothing_factor <- 1-av.unutilise
  
  daily_utilisation <- 1 - prop_usage_day[,"Unused"]
  
  smoothing <- (daily_utilisation + full_smoothing_factor)/2
  
  partial_smoothing_factor <- max(smoothing)
  
  mean_underutlised <- mean(prop_usage[,"Under utilised"])
  
  hotdesk_ratio <- 0.7 # assumption of hotdesk allocation. Could shift to the user input?
  
  
  dark_blue <- current_allocation * (1-min_unused)
  green <- current_allocation * partial_smoothing_factor
  second_green <- current_allocation * full_smoothing_factor
  yellow <- second_green - ((1 - hotdesk_ratio)*current_allocation*mean_underutlised)
  orange <- daily_utilisation["Friday"] * current_allocation
  
  recommendation_list <- c("Current Allocation","Unused Desks","Partial Smoothing","Full Smoothing","Hot Desking","Friday Figure")
  desks_in_scope <- c(current_allocation,round(dark_blue),round(green),round(second_green),round(yellow),round(orange))
  percent_current_allocation <- paste(round((desks_in_scope/current_allocation)*100),"%",sep="")
  
  out <- data.frame("recommendation" = recommendation_list,
                    "desks in scope" = desks_in_scope,
                    "percent current allocation" = percent_current_allocation)
  
  
}

