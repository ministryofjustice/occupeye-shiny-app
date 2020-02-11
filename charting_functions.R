# Library declarations ----------------------------------------------------

library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)
library(glue)
library(tidyr)
library(waffle)

get_prop_usage <- function(df_sum) {
  
  df_sum %>%
    count(date, util_cat, .drop = F) %>%
    group_by(date) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup(date)
}

get_max_capacity_days <- function(df_sum) {
  
  get_prop_usage(df_sum) %>% 
    dplyr::filter(util_cat != "Unused") %>% 
    group_by(date) %>% 
    summarise(prop = sum(prop)) %>% 
    dplyr::filter(prop == 1) %>%
    .$date
}

prop_daily_usage_chart <- function(df_sum) {
  
  prop_usage <- get_prop_usage(df_sum) %>%
    mutate(prop_label = replace(round(prop * 100, digits = 0),
                                prop <= 0.05,
                                ""))
  
  ggplot(prop_usage,
         aes(x = date, y = prop, fill = util_cat)) +
    geom_bar(stat = "identity", position = "fill") +
    ggtitle("Desk Utilisation By Date") +
    labs(y = "Desk Utilisation", fill = "") +
    labs(x = NULL, fill = "") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_date(breaks = pretty_breaks(30)) +
    theme(legend.position = "right") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values = c("Effective utilisation" = "coral2",
                                 "Under utilised" = "thistle3",
                                 "Unused" = "powderblue")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10)) +
    geom_text(aes(label = prop_label),
              position = position_fill(),
              vjust = 2,
              size = 3)
  
}

daily_usage_chart_narrative <- function(df_sum) {
  
  if (nrow(df_sum) == 0) {
    narrative <- "There are no sensors in this filter. This may be an error."
  } else {
    
    daily_unused <- get_prop_usage(df_sum) %>% dplyr::filter(util_cat == "Unused")
    
    if (nrow(daily_unused) == 0) {
      av_unused <- 0
    } else {
      av_unused <- mean(daily_unused$prop)
    }
    
    max_capacity_day_narrative <- ""
    max_capacity_days <- get_max_capacity_days(df_sum)
    max_capacity_days_concatenated <- paste(max_capacity_days, collapse=", ")
    
    if (length(max_capacity_days) > 0) {
      max_capacity_day_narrative <- glue("<li> Full capacity was reached on {length(max_capacity_days)} days in the sample period: {max_capacity_days_concatenated}</li>")
    }
    
    narrative <- paste0(glue("<ul><li>On average, desks were unused {percent(av_unused)} of the time during the sample period.</li>", max_capacity_day_narrative, "</ul>"))
  }
  return(narrative)
}


get_prop_usage_day <- function(df_sum) {
  
  df_sum %>%
    mutate(day = weekdays(date)) %>%
    group_by(day, util_cat) %>%
    summarise(n = n()) %>%
    tidyr::complete(day, util_cat, fill = list(n = 0)) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup(day)
  
}

weekday_usage_narrative <- function(df_sum) {
  prop_usage_day <- get_prop_usage_day(df_sum)
  
  daily_util <- prop_usage_day %>%
    dplyr::filter(util_cat %in% c("Effective utilisation", "Under utilised")) %>%
    group_by(day) %>%
    summarise(prop = sum(prop))
  
  top_day <- daily_util %>% dplyr::filter(prop == max(prop)) %>% .$day
  
  friday_util <- daily_util %>% dplyr::filter(day == "Friday") %>% .$prop
  
  glue("The rate of unused desks varies over the working week with {top_day} being the busiest day on average. On average {percent(1-friday_util)} of desks are unused on Fridays.")
  
}

prop_weekday_usage_chart <- function(df_sum) {
  
  prop_usage_day <- get_prop_usage_day(df_sum)
  
  weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  
  ggplot(prop_usage_day,
         aes(x = day, y = prop, fill = util_cat)) +
    geom_bar(stat = "identity", position = "fill") +
    ggtitle("Desk Utilisation By Day") +
    labs(y = "Desk Utilisation", fill = "") +
    labs(x = NULL, fill = "") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(limits = weekday) +
    theme(legend.position = "right") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values = c("Effective utilisation" = "coral2",
                                 "Under utilised" = "thistle3",
                                 "Unused" = "powderblue")) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10)) +
    geom_text(aes(label = scales::percent(prop, accuracy = 1)), position = position_fill(), vjust = 2)
  
  
}


get_prop_usage_type <- function(df_sum) {
  
  df_sum %>%
    count(devicetype, util_cat, .drop = F) %>%
    group_by(devicetype) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup(devicetype)
}

prop_desk_usage_chart <- function(df_sum) {
  
  prop_usage_type <- get_prop_usage_type(df_sum)
  
  ggplot(prop_usage_type,
         aes(x = devicetype, y = prop, fill = util_cat)) +
    geom_bar(stat = "identity", position = "fill") +
    ggtitle("Desk Utilisation By desk type") +
    labs(y = "Desk Utilisation", fill = "") +
    labs(x = NULL, fill = "") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "right") +
    theme(plot.title = element_text(hjust = 0.5)) +
    
    scale_fill_manual(values = c("Effective utilisation" = "coral2",
                                 "Under utilised" = "thistle3",
                                 "Unused" = "powderblue")) +
    theme(axis.text.x = element_text(angle = 25, hjust = 1, size = 10)) +
    geom_text(aes(label = scales::percent(prop, accuracy = 1)), position = position_fill(), vjust = 2)
  
}

get_prop_usage_team <- function(df_sum) {
  df_sum %>%
    count(category_1, category_2, category_3, util_cat, .drop = F) %>%
    group_by(category_1, category_2, category_3) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup(category_1, category_2, category_3)
  
}

prop_team_usage_chart <- function(df_sum) {
  
  prop_usage_team <- get_prop_usage_team(df_sum)
  
  ggplot(prop_usage_team,
         aes(x = category_3, y = prop, fill = util_cat)) +
    geom_bar(stat = "identity", position = "fill") +
    ggtitle("Desk Utilisation By Team") +
    labs(y = "Desk Utilisation", fill = "") +
    labs(x = NULL, fill = "") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "right") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values = c("Effective utilisation" = "coral2",
                                 "Under utilised" = "thistle3",
                                 "Unused" = "powderblue")) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10)) +
    geom_text(aes(label = scales::percent(prop, accuracy = 1)), position = position_fill(), vjust = 2)
}


get_prop_usage_floor <- function(df_sum) {
  
  df_sum %>%
    count(floor, util_cat) %>%
    group_by(floor) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup(floor)
}

prop_floor_usage_chart <- function(df_sum) {
  
  prop_usage_floor <- get_prop_usage_floor(df_sum)
  
  ggplot(prop_usage_floor,
         aes(x = factor(floor), y = prop, fill = util_cat)) +
    geom_bar(stat = "identity", position = "fill") +
    ggtitle("Desk Utilisation By Floor") +
    labs(y = "Desk Utilisation", fill = "") +
    labs(x = NULL, fill = "") +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "right") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values = c("Effective utilisation" = "coral2",
                                 "Under utilised" = "thistle3",
                                 "Unused" = "powderblue")) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10)) +
    geom_text(aes(label = scales::percent(prop, accuracy = 1)), position = position_fill(), vjust = 2)
  
  
}

get_smoothing_table <- function(df_sum, smoothing_factor) {
  
  weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  
  cat_order <- c("Full Smoothing", "Current Utilisation", "Partial Smoothing")
  
  smoothing <- get_prop_usage_day(df_sum) %>%
    dplyr::filter(util_cat == "Unused") %>%
    mutate(current_utilisation = 1 - prop) %>%
    mutate(full_smoothing = mean(current_utilisation)) %>%
    mutate(partial_smoothing = (full_smoothing * smoothing_factor) + (current_utilisation * (1 - smoothing_factor))) %>%
    select(day,
           "Full Smoothing" = full_smoothing, 
           "Current Utilisation" = current_utilisation,
           "Partial Smoothing" = partial_smoothing) %>%
    melt("day") %>%
    mutate(variable = factor(variable, cat_order))
}

get_smoothing_narrative <- function(df_sum, smoothing_factor) {
  smoothing_table <- get_smoothing_table(df_sum, smoothing_factor)
  
  smoothing_table_wide <- smoothing_table %>% 
    dcast(day ~ variable) %>%
    mutate(diff = `Full Smoothing` - `Current Utilisation`)
  
  top_diff <- smoothing_table_wide %>% dplyr::filter(diff == max(diff)) %>% .$day
  bottom_diff <- smoothing_table_wide %>% dplyr::filter(diff == min(diff)) %>% .$day
  
  glue("Partial or full smoothing would have most impact on {bottom_diff}s, where fewer people would work in the office, and {top_diff}s, where more people would work in the office")
  
}

smoothing_chart <- function(df_sum, smoothing_factor) {
  
  
  weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  
  cat_order <- c("Full Smoothing", "Current Utilisation", "Partial Smoothing")
  
  smoothing <- get_smoothing_table(df_sum,smoothing_factor)
  
  
  ggplot(smoothing,
         aes(x = day, y = value, fill = variable)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    ggtitle("Required Desk Allocation - Smoothing Assumptions") + 
    scale_x_discrete(limits = weekdays) + 
    theme(legend.position = "top") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    expand_limits(y = 0) + 
    scale_y_continuous(expand = c(0, 0),labels = percent) +
    coord_cartesian(ylim = c(0, 1)) + 
    labs(y="Desk Utilisation",fill="") + 
    scale_fill_brewer(palette = "Accent") +
    geom_text(aes(label = scales::percent(value, accuracy = 1)), position = position_dodge(width = 0.9), vjust = 2)
  
}


allocation_strategy_table <- function(df_sum) {
  
  current_allocation <- n_distinct(df_sum$survey_device_id)
  
  prop_usage <- prop.table(table(df_sum$date, df_sum$util_cat), 1)
  prop_usage_day <- prop.table(table(weekdays(df_sum$date), df_sum$util_cat), 1)
  
  min_unused <- min(prop_usage[,"Unused"])
  
  av.unutilise <- mean(prop_usage_day[,"Unused"])
  full_smoothing_factor <- 1 - av.unutilise
  
  daily_utilisation <- 1 - prop_usage_day[,"Unused"]
  
  smoothing <- (daily_utilisation + full_smoothing_factor) / 2
  
  partial_smoothing_factor <- max(smoothing)
  
  mean_underutilised <- mean(prop_usage[,"Under utilised"])
  
  hotdesk_ratio <- 0.6 # assumption of hotdesk allocation. Could shift to the user input?
  
  
  dark_blue <- current_allocation * (1-min_unused)
  green <- current_allocation * partial_smoothing_factor
  second_green <- current_allocation * full_smoothing_factor
  yellow <- second_green - ((1 - hotdesk_ratio) * current_allocation * mean_underutilised)
  orange <- daily_utilisation["Friday"] * current_allocation
  
  recommendation_list <- c("Current Allocation",
                           glue("Given current working patterns, the selected region could have had {round(current_allocation - dark_blue)} fewer desks over the sample period without experiencing any overcrowding issues."),
                           glue("If you were to partially smooth working patterns over the week, you could save a further {round(dark_blue - green)} desks"),
                           glue("If you were to fully smooth working patterns over the week, you could save a further {round(green - second_green)} desks"),
                           glue("On average {round(mean_underutilised * 100)}% of desks were used inefficiently. By embedding a culture of hotdesking (assuming a desk-to-person ratio of 0.6) it would be possible to replace {round(current_allocation * mean_underutilised)} desks with {round(current_allocation * mean_underutilised * hotdesk_ratio)} desks."),
                           "The average amount of desks effectively utilised and under utilised on Friday in the survey period.")
  desks_in_scope <- c(current_allocation,
                      round(dark_blue),
                      round(green),
                      round(second_green),
                      round(yellow),
                      round(orange))
  scenario_saving <- c(0,
                       round(current_allocation - dark_blue),
                       round(current_allocation - green),
                       round(current_allocation - second_green),
                       round(current_allocation - yellow),
                       round(current_allocation - orange))
  percent_current_allocation <- paste(round((desks_in_scope / current_allocation)*100), "%", sep="")
  
  out <- data.frame("recommendation" = recommendation_list,
                    "desks in scope" = desks_in_scope,
                    "total saving" = scenario_saving,
                    "% of current allocation" = percent_current_allocation,
                    check.names = FALSE)
  
  
}

desks_by_desk_type <- function(df_sum) {
  df_sum %>%
    group_by(devicetype) %>%
    summarise(sensors = n_distinct(survey_device_id)) %>%
    rename("Desk Type" = devicetype)
  
}

desks_by_team <- function(df_sum) {
  df_sum %>%
    group_by(category_1, category_2, category_3) %>%
    summarise(sensors = n_distinct(survey_device_id)) %>%
    rename("Directorate" = category_1, "Department" = category_2, "Team" = category_3)
}

desks_by_desk_type_and_team <- function(df_sum) {
  df_sum %>%
    group_by(category_3, devicetype) %>%
    summarise(sensors = n_distinct(survey_device_id)) %>%
    rename("Desk Type" = devicetype, "team" = category_3)
}

get_peak_occupancy <- function(df_sum) {
  prop_usage <- get_prop_usage(df_sum)
  
  prop_usage %>% 
    dplyr::filter(util_cat != "Unused") %>%
    group_by(date) %>%
    summarise(prop = sum(prop)) %>%
    mutate(date = as.character(date), utilisation = percent(prop, accuracy = 1)) %>% 
    arrange(desc(prop)) %>%
    select(date, utilisation) %>%
    head(10)
}

ranked_desk_chart <- function(df_sum) {
  
  transformed_df_sum <- df_sum %>%
    count(location, util_cat) %>%
    mutate(location = forcats::fct_reorder2(location, util_cat, n, .desc = TRUE))
  
  ggplot(data = transformed_df_sum, mapping = aes(x = location, y = n, fill = util_cat)) +
    geom_bar(position = "fill", stat = "identity")
  
}


# NPS charts --------------------------------------------------------------

weekday_usage_chart <- function(df) {
  # Just shows average utilisation per weekday
  weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  
  weekday_average <- df %>%
    mutate(day = weekdays(obs_datetime)) %>%
    group_by(day) %>%
    summarise(average_utilisation = mean(sensor_value, na.rm = T))
  
  ggplot(weekday_average,
         aes(x = day, y = average_utilisation)) +
    geom_bar(stat = "identity", fill = "coral2") +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0,1)) +
    scale_x_discrete(limits = weekday) +
    ylab("Average Occupancy") +
    theme_minimal() +
    ggtitle("Average occupancy by weekday")
  
}


nps_donut_narrative <- function(room_df, target_occupancy) {
  concurrent_rooms <- concurrent_room_table(room_df)
  
  top_usage <- concurrent_rooms %>%
    dplyr::filter(rooms_occupied == max(rooms_occupied))
  
  weekday_average <- room_df %>%
    mutate(day = weekdays(obs_datetime)) %>%
    group_by(day) %>%
    summarise(average_utilisation = mean(sensor_value))
  
  mean_rooms <- room_df %>% 
    group_by(obs_datetime) %>%
    summarise(rooms_occupied = sum(sensor_value)) %>%
    pull(rooms_occupied) %>%
    mean() %>%
    ceiling()
  
  top_weekday <- weekday_average %>%
    dplyr::filter(average_utilisation == max(average_utilisation))
  
  glue("<li>At peak {top_usage$rooms_occupied} rooms were in use for a combined period of {top_usage$n * 10} minutes</li>
       <li>{top_weekday$day} is the busiest day for selected room types ({paste(unique(room_df$devicetype), collapse = 's; ')})</li>
       <li>On average {mean_rooms} rooms are in use at any one time")
}

concurrent_room_table <- function(room_df) {
  room_df %>% 
    group_by(obs_datetime) %>%
    summarise(rooms_occupied = sum(sensor_value)) %>%
    count(rooms_occupied) %>%
    mutate(prop = prop.table(n))
}

concurrent_room_usage_chart <- function(room_df) {
  room_count <- concurrent_room_table(room_df)
  
  ggplot(room_count, aes(x = rooms_occupied, y = prop)) +
    geom_bar(stat = "identity", fill = "coral2") +
    scale_x_continuous(breaks = room_count$rooms_occupied) +
    xlab("Number of rooms occupied concurrently") +
    ylab("proportion of time in sample") +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    theme_minimal() +
    ggtitle("Concurrent use distribution")
  
}

time_of_day_bar <- function(room_df) {
  weekdays <- c("Monday",
                "Tuesday",
                "Wednesday",
                "Thursday",
                "Friday")
  
  data <- room_df %>%
    mutate(time_of_day = strftime(obs_datetime, format="%H:%M"),
           weekday = weekdays(obs_datetime)) %>%
    group_by(weekday, time_of_day) %>%
    summarise(occupied = mean(sensor_value))
  
  ggplot(data,
         aes(x = hm(time_of_day),
             y = occupied)) +
    geom_step(stat = "identity",
              position = "identity", alpha = 0.5) +
    facet_wrap(~factor(weekday, levels = weekdays), nrow = 5) +
    geom_hline(aes(yintercept = mean(occupied))) +
    geom_hline(aes(yintercept = max(occupied))) +
    scale_x_time() +
    labs(title = "Occupancy by weekday and time",
         x = "Time of Day") +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank())
  
}

make_gauge_data <- function(room_df,target) {
  mean_occupancy <- mean(room_df$sensor_value)
  
  total_rooms <- n_distinct(room_df$survey_device_id)
  
  tribble(
    ~variable, ~occupancy, ~label, ~title,
   "Actual occupancy", mean_occupancy, scales::percent(mean_occupancy, accuracy = 1), "Average \n occupancy"
  )
}

vertical_gauge_chart <- function(room_df,
                                 target,
                                 scaling_factor = 1) {
  # code adapted from https://pomvlad.blog/2018/05/03/gauges-ggplot2/
  # so some details are a bit redundant
  
  df <- make_gauge_data(room_df, target)
  
  
  ggplot(df) +
    geom_rect(aes(ymax=1,
                  ymin=0,
                  xmax=2,
                  xmin=1),
              fill ="#f0f0f0",
              colour = "#000000") +
    geom_rect(aes(ymax = occupancy,
                  ymin = 0,
                  xmax = 2,
                  xmin = 1.5),
              colour = "#000000",
              fill = "coral2") +
    geom_rect(aes(ymax = target,
                  ymin = 0,
                  xmax = 1.5,
                  xmin = 1),
              colour = "#000000",
              fill = "sandybrown") +
    geom_text(aes(x = 1.75, y = 0.1,
                  label = label),
              colour = "black",
              size = 4.5 * scaling_factor,
              family = "Poppins SemiBold") +
    geom_text(aes(x = 1.25, y = 0.1,
                  label = scales::percent(target,
                                          accuracy = 1)),
              colour = "black",
              size = 4.5 * scaling_factor,
              family = "Poppins SemiBold") +
    geom_text(aes(x = 2.5,
                  y = occupancy,
                  label = title),
              size = 4.2 * scaling_factor) +
    geom_text(aes(x = 0.5,
                  y = target),
              label = "Target \n occupancy",
              size = 4.2 * scaling_factor) +
    theme_void() +
    scale_x_continuous(limits = c(0,4)) +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank()) +
    guides(fill = FALSE) +
    guides(colour=FALSE)
}

make_waffle_data <- function(room_df, target) {
  total_rooms <- n_distinct(room_df$survey_device_id)
  
  mean_rooms <- room_df %>% 
    group_by(obs_datetime) %>%
    summarise(rooms_occupied = sum(sensor_value)) %>%
    pull(rooms_occupied) %>%
    mean() %>%
    ceiling()
  
  peak_rooms <- room_df %>% 
    group_by(obs_datetime) %>%
    summarise(rooms_occupied = sum(sensor_value)) %>%
    pull(rooms_occupied) %>%
    max()
  
  mean_occupancy <- mean(room_df$sensor_value)
  recommended_rooms <- ceiling(total_rooms * (mean_occupancy/target))
  
  background_dummy <- max(total_rooms, recommended_rooms)
  
  tribble(
    ~label, ~figure, ~dummy,
    "Total number of rooms", total_rooms, background_dummy - total_rooms,
    "Peak occupancy", peak_rooms, background_dummy - peak_rooms,
    "Average occupancy", mean_rooms, background_dummy - mean_rooms,
    "Recommended number of rooms", recommended_rooms, background_dummy - recommended_rooms
  ) %>%
    mutate(label = factor(label,
                          levels = label)) %>%
    gather(key = variable,
           value = value,
           -label)
  
  
}

room_waffle_chart <- function(room_df, target, scaling_factor = 1) {
  df <- make_waffle_data(room_df, target)
  
  total_rooms <- n_distinct(room_df$survey_device_id)
  
  ggplot(df,
         aes(fill = variable,
             values = value)) +
    geom_waffle(color = "white", flip = T, n_rows = 5, size = 1) +
    geom_text(aes(label = case_when(variable == "figure" ~as.character(value),
                                    T ~""),
                  x = min(3, ceiling(total_rooms / 2)),
                  y = -0.1),
              vjust = "inward",
              size = 5 * scaling_factor) +
    facet_wrap(~label,
               strip.position = "top",
               labeller = label_wrap_gen(width = 12),
               nrow = 1) +
    theme_minimal() +
    theme_enhance_waffle() +
    labs(x = NULL,
         y = NULL) +
    scale_fill_manual(values = c("#f0f0f0", "coral2")) +
    coord_equal() +
    theme(panel.grid = element_blank(),
          legend.position = "none",
          strip.text = element_text(size = 11 * scaling_factor))
  
}


get_room_resource_requirements <- function(df,
                                           group_room_target,
                                           interview_room_target,
                                           room_footage_hot) {
  df %>%
    group_by(devicetype) %>%
    summarise("count" = n_distinct(survey_device_id),
              "occupancy" = mean(sensor_value, na.rm = T)) %>%
    dplyr::filter(devicetype %in% c("Group Room",
                                    "Interview Room")) %>%
    mutate(target = case_when(devicetype == "Group Room" ~group_room_target,
                                        devicetype == "Interview Room" ~interview_room_target),
           recommended_rooms = ceiling(count * (occupancy / target))) %>%
    left_join(room_footage_hot, by = c("devicetype" = "resource_name")) %>%
    mutate(space_required = make_numeric(space_required),
           total_space = space_required * recommended_rooms)
  
}

get_fte_resource_requirements <- function(fte, space_per_fte) {
  data.frame(resource_name = "Staff total space",
             FTE = fte,
             space_required = space_per_fte,
             total_space = space_per_fte * fte)
}

get_staff_accommodation_requirements <- function(resource_hot, fte) {
  resource_hot %>%
    mutate(resource_per_fte_ratio = paste(resource, per_fte, sep = ":"),
           qty = ceiling(fte * make_numeric(resource) / make_numeric(per_fte))) %>%
    select(-resource, -per_fte)
}

get_total_space_table <- function(filtered_room_df,
                                  group_room_target,
                                  interview_room_target,
                                  room_footage_hot,
                                  fte,
                                  space_per_fte,
                                  ancillary_space_hot) {
  
  room_resources <- get_room_resource_requirements(filtered_room_df,
                                                   group_room_target,
                                                   interview_room_target,
                                                   room_footage_hot) %>%
    select(resource_name = devicetype,
           qty = recommended_rooms,
           space_required,
           total_space) %>%
    mutate_all(as.character)
  
  fte_resources <- get_fte_resource_requirements(fte,
                                                 space_per_fte) %>%
    mutate_all(as.character) %>%
    rename(qty = FTE)
  
  ancillary_resources <- ancillary_space_hot %>%
    mutate(total_space = make_numeric(qty) * make_numeric(space_required)) %>%
    mutate_all(as.character)
  
  bind_rows(room_resources, fte_resources, ancillary_resources) %>%
    mutate(total_space = as.numeric(total_space)) %>%
    janitor::adorn_totals() %>%
    convert_fields_to_sentence_case()
}

error_chart <- function(message) {
  
  ggplot() + 
    annotate("text", x = 4, y = 25, size=8, label = message) +
    theme_void()
}

# probably should shift this to an s3 file rather than hard-coding.
get_resource_df <- function() {
  tribble(
    ~resource_name, ~resource, ~per_fte,
    "Long Stay Desks", 8, 10,
    "Touchdown", 1, 20,
    "Quiet/phone room", 1, 20,
    "Open Meeting", 1, 30,
    "Breakout", 1, 40,
    "Tea point", 1, 50,
    "Print & copy", 1, 100,
    "Lockers", 1, 1,
    "File Storage", 0.5, 1
  )
}
  