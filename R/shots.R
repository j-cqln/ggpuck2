library(dplyr)

library(ggplot2)
library(ggstar)
library(gridExtra)

library(pubtheme)

# Get playoff teams for a season
# Used for plot.(...)2 functions
# Requires season (year), season_type
get.playoff.teams <- function(shots, year) {
  return(unique(shots$team[shots$season == year & shots$season_type == 'playoffs']))
}

# Grouped stacked bar plot with team shots, goals in regular season, playoffs
# 1 plot
# Requires season (year), season_type, team, goal
plot.shots.goals.teams <- function(shots, year) {
  background_color <- '#ffffff'
  
  s <- shots %>%
    filter(season == year,
           on_goal == 1) %>%
    group_by(team, season_type) %>%
    summarise(shots = n(),
              goals = sum(goal)) %>%
    arrange(desc(shots)) %>%
    select(team, season_type, shots, goals)
  
  s <- melt(s, id = c('team', 'season_type'))
  
  temp <- s %>%
    group_by(team, variable) %>%
    summarise(value = sum(value)) %>%
    ungroup()
  
  coeff <- 2 * max(temp$value[temp$variable == 'goals']) /
    max(temp$value[temp$variable == 'shots'])
  
  s <- s %>%
    mutate(value = ifelse(variable == 'goals', value / coeff, value))
  
  ggplot(data = s,
         aes(x = variable,
             y = value,
             fill = interaction(season_type, variable, sep = ' '),
             alpha = 0.8)) +
    geom_col(width = 1,
             position = position_stack(reverse = FALSE)) +
    scale_alpha_continuous(guide = FALSE) +
    scale_fill_manual(values = c('#c82f2f', '#ef9ba3', '#0074a7', '#73c3e8')) +
    scale_y_continuous(name = 'Shots',
                       sec.axis = sec_axis(~.*coeff, name = 'Goals')) +
    labs(title = paste0('Shots and goals per team, ', year),
         subtitle = 'Regular season and playoffs (if qualified)',
         caption = 'Created by github.com/j-cqln',
         x = 'Event (shots or goals)',
         fill = 'Season type',
         alpha = '') +
    theme_pub(type = 'line', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color),
          panel.spacing.x = unit(0, units = "in"),
          axis.ticks.x.bottom = element_blank(),
          axis.text.x.bottom = element_blank(),
          strip.text = element_text(colour = pubtextgray,
                                    size = 36/3,
                                    angle = 90)) +
    facet_wrap(vars(team), nrow = 1)
}

# Map of shots by goal proportion
# 3 plots: all teams, for team, against team
# Requires season (year), team (team_name), x, y, goal 
plot.shots.map <- function(shots, year, team_name) {
  background_color <- '#ffffff'
  gradient_low_color <- '#bedceb'
  gradient_high_color <- '#004b71'
  gradient_low_color3 <- '#eee2d2'
  gradient_high_color3 <- '#d57700'
  
  title <- paste0('Shots by % goal, ',
                  year, ' ')
  title3 <- paste0('Shots against by % goal, ',
                   year, ' ')
  
  s <- shots %>% filter(season == year,
                        on_goal == 1)
  
  s_hexbin <- hexbin::hexbin(s$x, s$y, xbins = 21, IDs = TRUE)
  s_hexbin_df <- data.frame(hexbin::hcell2xy(s_hexbin),
                            cell = s_hexbin@cell,
                            count = s_hexbin@count)
  s$cell <- s_hexbin@cID
  
  s2 <- s %>%
    filter(team == team_name) %>%
    group_by(cell) %>%
    summarise(shot_count = n(),
              avg_g = mean(goal)) %>%
    ungroup() %>%
    right_join(s_hexbin_df, by = 'cell') %>%
    select(cell, x, y, count, shot_count, avg_g)
  
  s3 <- s %>%
    filter(opposing_team == team_name) %>%
    group_by(cell) %>%
    summarise(shot_count = n(),
              avg_g = mean(goal)) %>%
    ungroup() %>%
    right_join(s_hexbin_df, by = 'cell') %>%
    select(cell, x, y, count, shot_count, avg_g)
  
  s <- s %>%
    group_by(cell) %>%
    summarise(shot_count = n(),
              avg_g = mean(goal)) %>%
    ungroup() %>%
    right_join(s_hexbin_df, by = 'cell') %>%
    select(cell, x, y, count, shot_count, avg_g)
  
  p <- rink +
    geom_star(data = s,
              aes(x = x, y = y,
                  fill = avg_g,
                  size = shot_count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient_low_color,
                        high = gradient_high_color,
                        na.value = NA,
                        limits = c(0.0, 0.2),
                        breaks = c(0.0, 0.2),
                        labels = c('0%', '20%+'),
                        oob = squish) +
    scale_size_area(limits = c(0, 1000),
                    breaks = c(100, 500, 1000),
                    labels = c('100', '500', '1000+'),
                    oob = squish) +
    labs(title = paste0(title, 'all'),
         subtitle = 'Percent of shots becoming goals by region',
         caption = 'Created by github.com/j-cqln',
         fill = '% goal',
         size = 'Unblocked shot attempts') +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color),
          legend.position = 'top',
          legend.box = 'horizontal',
          legend.box.just = 'left',
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
          legend.direction = 'horizontal',
          legend.justification = c(0, 0),
          legend.margin = margin(t = 0, r = 10, b = 0, l = 0, unit = 'pt')) +
    guides(size = guide_legend(order = 1,
                               nrow = 1,
                               title.position = 'top',
                               title.hjust = 0,
                               override.aes = list(fill = pubdarkgray)), 
           fill = guide_colorbar(order = 2,
                                 title.position = 'top',
                                 title.hjust = 0))
  
  p2 <- rink +
    geom_star(data = s2,
              aes(x = x, y = y,
                  fill = avg_g,
                  size = shot_count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient_low_color,
                        high = gradient_high_color,
                        na.value = NA,
                        limits = c(0.0, 0.2),
                        breaks = c(0.0, 0.2),
                        labels = c('0%', '20%+'),
                        oob = squish) +
    scale_size_area(limits = c(0, 80),
                    breaks = c(5, 40, 80),
                    labels = c('5', '40', '80+'),
                    oob = squish) +
    labs(title = paste0(title, team_name),
         subtitle = 'Percent of shots becoming goals by region',
         caption = 'Created by github.com/j-cqln',
         fill = '% goal',
         size = 'Unblocked shot attempts') +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color),
          legend.position = 'top',
          legend.box = 'horizontal',
          legend.box.just = 'left',
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
          legend.direction = 'horizontal',
          legend.justification = c(0, 0),
          legend.margin = margin(t = 0, r = 10, b = 0, l = 0, unit = 'pt')) +
    guides(size = guide_legend(order = 1,
                               nrow = 1,
                               title.position = 'top',
                               title.hjust = 0,
                               override.aes = list(fill = pubdarkgray)), 
           fill = guide_colorbar(order = 2,
                                 title.position = 'top',
                                 title.hjust = 0))
  
  p3 <- rink +
    geom_star(data = s3,
              aes(x = x, y = y,
                  fill = avg_g,
                  size = shot_count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient_low_color3,
                        high = gradient_high_color3,
                        na.value = NA,
                        limits = c(0.0, 0.2),
                        breaks = c(0.0, 0.2),
                        labels = c('0%', '20%+'),
                        oob = squish) +
    scale_size_area(limits = c(0, 80),
                    breaks = c(5, 40, 80),
                    labels = c('5', '40', '80+'),
                    oob = squish) +
    labs(title = paste0(title3, team_name),
         subtitle = 'Percent of shots becoming goals by region',
         caption = 'Created by github.com/j-cqln',
         fill = '% goal',
         size = 'Unblocked shot attempts') +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color),
          legend.position = 'top',
          legend.box = 'horizontal',
          legend.box.just = 'left',
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
          legend.direction = 'horizontal',
          legend.justification = c(0, 0),
          legend.margin = margin(t = 0, r = 10, b = 0, l = 0, unit = 'pt')) +
    guides(size = guide_legend(order = 1,
                               nrow = 1,
                               title.position = 'top',
                               title.hjust = 0,
                               override.aes = list(fill = pubdarkgray)), 
           fill = guide_colorbar(order = 2,
                                 title.position = 'top',
                                 title.hjust = 0))
  
  grid.arrange(p, p2, p3, ncol = 3)
}

# Distribution of shots by distance in regular season, playoffs
# 3 plots: all teams, for team, against team
# Requires season (year), season_type, team (team_name), distance
plot.shots.distance <- function(shots, year, team_name) {
  background_color <- '#ffffff'
  
  title <- paste0('Shots by distance, ',
                  year, ' ')
  title3 <- paste0('Shots against by distance, ',
                   year, ' ')
  
  s <- shots %>%
    filter(season == year,
           on_goal == 1) %>%
    mutate(season_type = factor(season_type,
                                levels = c('regular', 'playoffs'),
                                labels = c('regular', 'playoffs')))
  
  s2 <- s %>% filter(team == team_name)
  s3 <- s %>% filter(opposing_team == team_name)
  
  p <- ggplot(data = s,
              aes(distance,
                  fill = season_type,
                  color = season_type)) +
    geom_density(alpha = 0.1) +
    labs(title = paste0(title, 'all'),
         subtitle = 'Regular season and playoffs comparison',
         caption = 'Created by github.com/j-cqln',
         x = 'Distance from net',
         y = 'Shot density',
         fill = 'Season type',
         color = 'Season type') +
    theme_pub(type = 'line', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color))
  
  p2 <- ggplot(data = s2,
               aes(distance,
                   fill = season_type,
                   color = season_type)) +
    geom_density(alpha = 0.1) +
    labs(title = paste0(title, team_name),
         subtitle = 'Regular season and playoffs comparison',
         caption = 'Created by github.com/j-cqln',
         x = 'Distance from net',
         y = 'Shot density',
         fill = 'Season type',
         color = 'Season type') +
    theme_pub(type = 'line', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color))
  
  p3 <- ggplot(data = s3,
               aes(distance,
                   fill = season_type,
                   color = season_type)) +
    geom_density(alpha = 0.1) +
    labs(title = paste0(title3, team_name),
         subtitle = 'Regular season and playoffs comparison',
         caption = 'Created by github.com/j-cqln',
         x = 'Distance from net',
         y = 'Shot against density',
         fill = 'Season type',
         color = 'Season type') +
    theme_pub(type = 'line', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color))
  
  grid.arrange(p, p2, p3, ncol = 3)
}

# Distribution of shots by angle in regular season, playoffs
# 3 plots: all teams, for team, against team
# Requires season (year), season_type, team (team_name), distance
plot.shots.angle <- function(shots, year, team_name) {
  background_color <- '#ffffff'
  
  title <- paste0('Shots by angle, ',
                  year, ' ')
  
  title3 <- paste0('Shots against by angle, ',
                   year, ' ')
  
  s <- shots %>%
    filter(season == year,
           on_goal == 1) %>%
    mutate(season_type = factor(season_type,
                                levels = c('regular', 'playoffs'),
                                labels = c('regular', 'playoffs')))
  
  s2 <- s %>% filter(team == team_name)
  
  s3 <- s %>% filter(opposing_team == team_name)
  
  p <- ggplot(data = s,
              aes(angle,
                  fill = season_type,
                  color = season_type)) +
    geom_density(alpha = 0.1) +
    labs(title = paste0(title, 'all'),
         subtitle = 'Regular season and playoffs comparison',
         caption = 'Created by github.com/j-cqln',
         x = 'Shot angle relative to net',
         y = 'Shot density',
         fill = 'Season type',
         color = 'Season type') +
    theme_pub(type = 'line', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color))
  
  p2 <- ggplot(data = s2,
               aes(angle,
                   fill = season_type,
                   color = season_type)) +
    geom_density(alpha = 0.1) +
    labs(title = paste0(title, team_name),
         subtitle = 'Regular season and playoffs comparison',
         caption = 'Created by github.com/j-cqln',
         x = 'Shot angle relative to net',
         y = 'Shot density',
         fill = 'Season type',
         color = 'Season type') +
    theme_pub(type = 'line', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color))
  
  p3 <- ggplot(data = s3,
               aes(angle,
                   fill = season_type,
                   color = season_type)) +
    geom_density(alpha = 0.1) +
    labs(title = paste0(title3, team_name),
         subtitle = 'Regular season and playoffs comparison',
         caption = 'Created by github.com/j-cqln',
         x = 'Shot against angle relative to net',
         y = 'Shot against density',
         fill = 'Season type',
         color = 'Season type') +
    theme_pub(type = 'line', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color))
  
  grid.arrange(p, p2, p3, ncol = 3)
}

# Like plot.shots.map, except only shots for, all teams
# 1 plot
# Requires season (year), x, y, goal 
plot.shots.map2 <- function(shots, year) {
  background_color <- '#ffffff'
  gradient_low_color <- '#bedceb'
  gradient_high_color <- '#004b71'
  
  title <- paste0('Shots by % goal, ',
                  year, ' ')
  
  s <- shots %>% filter(season == year,
                        on_goal == 1)
  
  s_hexbin <- hexbin::hexbin(s$x, s$y, xbins = 21, IDs = TRUE)
  s_hexbin_df <- data.frame(hexbin::hcell2xy(s_hexbin),
                            cell = s_hexbin@cell,
                            count = s_hexbin@count)
  s$cell <- s_hexbin@cID
  
  s <- s %>%
    group_by(cell) %>%
    summarise(shot_count = n(),
              avg_g = mean(goal)) %>%
    ungroup() %>%
    right_join(s_hexbin_df, by = 'cell') %>%
    select(cell, x, y, count, shot_count, avg_g)
  
  p <- rink +
    geom_star(data = s,
              aes(x = x, y = y,
                  fill = avg_g,
                  size = shot_count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient_low_color,
                        high = gradient_high_color,
                        na.value = NA,
                        limits = c(0.0, 0.2),
                        breaks = c(0.0, 0.2),
                        labels = c('0%', '20%+'),
                        oob = squish) +
    scale_size_area(limits = c(0, 1000),
                    breaks = c(100, 500, 1000),
                    labels = c('100', '500', '1000+'),
                    oob = squish) +
    labs(title = paste0(title, 'all'),
         subtitle = 'Percent of shots becoming goals by region',
         caption = 'Created by github.com/j-cqln',
         fill = '% goal',
         size = 'Unblocked shot attempts') +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color),
          legend.position = 'top',
          legend.box = 'horizontal',
          legend.box.just = 'left',
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
          legend.direction = 'horizontal',
          legend.justification = c(0, 0),
          legend.margin = margin(t = 0, r = 10, b = 0, l = 0, unit = 'pt')) +
    guides(size = guide_legend(order = 1,
                               nrow = 1,
                               title.position = 'top',
                               title.hjust = 0,
                               override.aes = list(fill = pubdarkgray)), 
           fill = guide_colorbar(order = 2,
                                 title.position = 'top',
                                 title.hjust = 0))
  
  return(p)
}

# Like plot.shots.distance, except only shots for, all teams or one team option, all shots or goals only option, all teams or playoff teams only option
# 1 plot
# Requires season (year), season_type, team (team_name), distance, goal
plot.shots.distance2 <- function(shots, year, team_name = NULL, goals_only = FALSE, playoffs_only = FALSE) {
  background_color <- '#ffffff'
  
  s <- shots %>% filter(season == year,
                        on_goal == 1)
  
  # Only plot teams that made playoffs
  if (playoffs_only) {
    s <- s %>% filter(team %in% get.playoff.teams(shots, year))
    playoffs <- ' playoff '
  } else {
    playoffs <- ' '
  }
  
  # Just goals or all shots?
  if (goals_only) {
    s <- s %>% filter(goal == 1)
    title <- paste0('Goal density by distance from net, ', shots$season, ', ')
  } else {
    title <- paste0('Shot density by distance from net, ', shots$season, ', ')
  }
  
  # Specific team or all teams?
  if (!is.null(team_name)) {
    s <- s %>% filter(team == team_name)
    title <- paste0(title, team_name)
  } else {
    title <- paste0(title, 'all', playoffs, 'teams')
  }
  
  # Plot
  p <- ggplot(s,
              aes(distance,
                  fill = season_type,
                  colour = season_type)) +
    geom_density(alpha = 0.1) +
    labs(title = title,
         subtitle = 'Regular season and playoffs comparison',
         caption = 'Created by github.com/j-cqln',
         x = 'Distance from net', 
         y = 'Shot density',
         fill = 'Season type',
         colour = 'Season type') +
    theme_pub(type = 'line', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color))
  
  # Formatting all teams
  if (is.null(team_name)) {
    p <- p + facet_wrap(vars(team))
  }
  
  return(p)
}

# Like plot.shots.angle, except only shots for, all teams or one team option, all shots or goals only option, all teams or playoff teams only option
# 1 plot
# Requires season (year), season_type, team (team_name), angle, goal
plot.shots.angle2 <- function(shots, year, team_name = NULL, goals_only = FALSE, playoffs_only = FALSE) {
  background_color <- '#ffffff'
  
  s <- shots %>% filter(season == year,
                        on_goal == 1)
  
  # Only plot teams that made playoffs
  if (playoffs_only) {
    s <- s %>% filter(team %in% get.playoff.teams(shots, year))
    playoffs <- ' playoff '
  } else {
    playoffs <- ' '
  }
  
  # Just goals or all shots?
  if (goals_only) {
    s <- s %>% filter(goal == 1)
    title <- paste0('Goal angles relative to net, ', shots$season, ', ')
  } else {
    title <- paste0('Shot angles relative to net, ', shots$season, ', ')
  }
  
  # Specific team or all teams?
  if (!is.null(team_name)) {
    s <- s %>% filter(team == team_name)
    title <- paste0(title, team_name)
  } else {
    title <- paste0(title, 'all', playoffs, 'teams')
  }
  
  # Plot
  p <- ggplot(s,
              aes(angle,
                  fill = season_type,
                  colour = season_type)) +
    geom_density(alpha = 0.1) +
    labs(title = title,
         subtitle = 'Regular season and playoffs comparison',
         caption = 'Created by github.com/j-cqln',
         x = 'Angle relative to net', 
         y = 'Number of shots',
         fill = 'Season type',
         colour = 'Season type') +
    theme_pub(type = 'hist', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color))
  
  # Formatting all teams
  if (is.null(team_name)) {
    p <- p + facet_wrap(vars(team))
  }
  
  return(p)
}

# Like plot.shots.map2, but no rink background
# 1 plot
# Requires season (year), x, y, goal 
plot.shots.map3 <- function(shots, year) {
  background_color <- '#ffffff'
  gradient_low_color <- '#bedceb'
  gradient_high_color <- '#004b71'
  
  title <- paste0('Shots by % goal, ',
                  year, ' ')
  
  s <- shots %>% filter(season == year,
                        on_goal == 1)
  
  s_hexbin <- hexbin::hexbin(s$x, s$y, xbins = 21, IDs = TRUE)
  s_hexbin_df <- data.frame(hexbin::hcell2xy(s_hexbin),
                            cell = s_hexbin@cell,
                            count = s_hexbin@count)
  s$cell <- s_hexbin@cID
  
  s <- s %>%
    group_by(cell) %>%
    summarise(shot_count = n(),
              avg_g = mean(goal)) %>%
    ungroup() %>%
    right_join(s_hexbin_df, by = 'cell') %>%
    select(cell, x, y, count, shot_count, avg_g)
  
  p <- ggplot() +
    geom_star(data = s,
              aes(x = x, y = y,
                  fill = avg_g,
                  size = shot_count),
              color = NA,
              starshape = 'hexagon',
              show.legend = TRUE) +
    scale_fill_gradient(low = gradient_low_color,
                        high = gradient_high_color,
                        na.value = NA,
                        limits = c(0.0, 0.2),
                        breaks = c(0.0, 0.2),
                        labels = c('0%', '20%+'),
                        oob = squish) +
    scale_size_area(limits = c(0, 1000),
                    breaks = c(100, 500, 1000),
                    labels = c('100', '500', '1000+'),
                    oob = squish) +
    labs(title = paste0(title, 'all'),
         subtitle = 'Percent of shots becoming goals by region',
         caption = 'Created by github.com/j-cqln',
         fill = '% goal',
         size = 'Unblocked shot attempts') +
    ylim(c(-100.1, -24)) +
    xlim(c(-42.6, 42.6)) +
    theme_pub(type = 'map', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color),
          legend.position = 'top',
          legend.box = 'horizontal',
          legend.box.just = 'left',
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt'),
          legend.direction = 'horizontal',
          legend.justification = c(0, 0),
          legend.margin = margin(t = 0, r = 10, b = 0, l = 0, unit = 'pt')) +
    guides(size = guide_legend(order = 1,
                               nrow = 1,
                               title.position = 'top',
                               title.hjust = 0,
                               override.aes = list(fill = pubdarkgray)), 
           fill = guide_colorbar(order = 2,
                                 title.position = 'top',
                                 title.hjust = 0))
  
  return(p)
}

# Like plot.shots.distance2, except as a boxplot
# 1 plot
# Requires season (year), season_type, distance, goal
plot.shots.distance.boxplot <- function(shots, year, goals_only = FALSE, playoffs_only = FALSE) {
  background_color <- '#ffffff'
  
  s <- shots %>% filter(season == year,
                        on_goal == 1)
  
  # Just goals or all shots?
  if (goals_only) {
    s <- s %>% filter(goal == 1)
    title <- paste0('Goal density by distance from net, ', shots$season, ', ')
  } else {
    title <- paste0('Shot density by distance from net, ', shots$season, ', ')
  }
  
  # Only plot teams that made playoffs
  if (playoffs_only) {
    s <- s %>% filter(team %in% get.playoff.teams(shots, year))
    playoffs <- ' playoff '
  } else {
    playoffs <- ' '
  }
  
  title <- paste0(title, 'all', playoffs, 'teams')
  
  # Plot
  p <- ggplot(s,
              aes(season_type, distance,
                  fill = season_type)) +
    geom_boxplot() +
    labs(title = title,
         subtitle = 'Regular season and playoffs comparison',
         caption = 'Created by github.com/j-cqln',
         x = 'Season type', 
         y = 'Shot distance',
         fill = 'Season type') +
    theme_pub(type = 'hist', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color)) +
    facet_wrap(vars(team))
  
  return(p)
}

# Like plot.shots.angle2, except as a boxplot, playoff teams only
# 1 plot
# Requires season (year), season_type, abs_angle, goal
plot.shots.angle.boxplot <- function(shots, year, goals_only = FALSE, playoffs_only = FALSE) {
  background_color <- '#ffffff'
  
  s <- shots %>% filter(season == year,
                        on_goal == 1)
  
  # Just goals or all shots?
  if (goals_only) {
    s <- s %>% filter(goal == 1)
    title <- paste0('Goal angles relative to net, ', shots$season, ', ')
  } else {
    title <- paste0('Shot angles relative to net, ', shots$season, ', ')
  }
  
  # Only plot teams that made playoffs
  if (playoffs_only) {
    s <- s %>% filter(team %in% get.playoff.teams(shots, year))
    playoffs <- ' playoff '
  } else {
    playoffs <- ' '
  }
  
  title <- paste0(title, 'all', playoffs, 'teams')
  
  # Plot
  p <- ggplot(s,
              aes(season_type, abs_angle,
                  fill = season_type)) +
    geom_boxplot() +
    labs(title = title,
         subtitle = 'Regular season and playoffs comparison',
         caption = 'Created by github.com/j-cqln',
         x = 'Season type', 
         y = 'Shot angle',
         fill = 'Season type') +
    theme_pub(type = 'hist', base_size = 36/3) +
    theme(plot.background = element_rect(fill = background_color),
          panel.background = element_rect(fill = background_color),
          legend.background = element_rect(fill = background_color)) +
    facet_wrap(vars(team))
  
  return(p)
}