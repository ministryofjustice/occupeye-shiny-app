library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)

get_prop_usage <- function(df_sum) {

  df_sum %>%
    count(date,util_cat) %>%
    group_by(date) %>%
    mutate(prop = n/sum(n)) %>%
    ungroup(date)
}

get_prop_usage_day <- function(df_sum) {

  df_sum %>%
    mutate(day = weekdays(date)) %>%
    count(day,util_cat) %>%
    group_by(day) %>%
    mutate(prop = n/sum(n)) %>%
    ungroup(day)
  
}

get_prop_usage_type <- function(df_sum) {
  
  df_sum %>%
    count(devicetype,util_cat) %>%
    group_by(devicetype) %>%
    mutate(prop = n/sum(n)) %>%
    ungroup(devicetype)
}

get_prop_usage_team <- function(df_sum) {
  df_sum %>%
    count(category_1,category_2,category_3,util_cat) %>%
    group_by(category_1,category_2,category_3) %>%
    mutate(prop = n/sum(n)) %>%
    ungroup(category_1,category_2,category_3)
  
  
}

prop_team_usage_chart <- function(df_sum) {
  
  prop_usage_team <- get_prop_usage_team(df_sum)
  
  ggplot(prop_usage_team,
         aes(x=category_3,y=prop,fill=util_cat)) +
    geom_bar(stat="identity", position='fill') +
    ggtitle("Desk Utilisation By Date") +
    labs(y="Desk Utilisation",fill="") +
    labs(x=NULL,fill="") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position="right") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("Effective utilisation"="coral2","Under utilised"="thistle3","Unused"="powderblue")) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=10))
  
  
}


prop_daily_usage_chart <- function(df_sum) {
  
  prop_usage <- get_prop_usage(df_sum)
  
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
  
  prop_usage_day <- get_prop_usage_day(df_sum)
  
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
  
  prop_usage_type <- get_prop_usage_type(df_sum)
  

  ggplot(prop_usage_type,
         aes(x=devicetype,y=prop,fill=util_cat)) +
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

smoothing_chart <- function(df_sum, smoothing_factor) {

  weekdays <-c("Monday","Tuesday","Wednesday","Thursday","Friday")
  
  cat_order <- c("Full Smoothing","Current Utilisation","Partial Smoothing")

  smoothing <- get_prop_usage_day(df_sum) %>%
    filter(util_cat == "Unused") %>%
    mutate(current_utilisation = 1- prop) %>%
    mutate(full_smoothing = mean(current_utilisation)) %>%
    mutate(partial_smoothing = (full_smoothing * smoothing_factor) + (current_utilisation * (1-smoothing_factor))) %>%
    select(day,"Full Smoothing" = full_smoothing,"Current Utilisation" = current_utilisation, "Partial Smoothing" = partial_smoothing) %>%
    melt("day") %>%
    mutate(variable = factor(variable,cat_order))
  

  
  ggplot(smoothing,
         aes(x=day,y=value,fill=variable)) + 
    geom_bar(stat="identity",position="dodge") + 
    ggtitle("Required Desk Allocation - Smoothing Assumptions") + 
    scale_x_discrete(limits = weekdays) + 
    theme(legend.position="top") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    expand_limits(y=0)+scale_y_continuous(expand = c(0, 0),labels = percent) +
    coord_cartesian(ylim=c(0,1))+labs(y="Desk Utilisation",fill="") + 
    scale_fill_brewer(palette="Accent")
  
  
  
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

desks_by_desk_type <- function(df_sum) {
  df_sum %>%
    group_by(devicetype) %>%
    summarise(sensors = n_distinct(surveydeviceid)) %>%
    rename("Desk Type" = devicetype)
  
}

