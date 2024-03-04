## Reaper
Reaper <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Reaper')
Reaper <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Reaper' & (OWL_stats_2023$stat_name=='Death Blossom Kills' | OWL_stats_2023$stat_name=='Self Healing' | OWL_stats_2023$stat_name=='Critical Hits' | OWL_stats_2023$stat_name=='Shots Fired' | OWL_stats_2023$stat_name=='Shots Hit'  | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Final Blows' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Reaper <- aggregate(amount ~ player_name + stat_name, Reaper, sum)
library(tidyr)

Reaper <- Reaper %>%
  spread(stat_name, amount)

Reaper[is.na(Reaper)] <- 0
Reaper <- subset(Reaper,Reaper$`Time Played`>=300)
Reaper[, c(2,3,4,5,6,7,8,11,12,13)] <- Reaper[, c(2,3,4,5,6,7,8,11,12,13)] / Reaper[, 14]
Reaper[,'Games Won'] <- Reaper[,"Games Won"]/Reaper[,"Games Played"]
# removing 
Reaper <- subset(Reaper, select = -`Games Played`)

z <- scale(Reaper[,c(2,3,4,5,7,8,9,10,11,12)])
score <- apply(z, 1, mean)
Reaper <- cbind(Reaper, score)
Reaper <- data.frame(lapply(Reaper, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Reaper[,c(2:14)])

## Whos playing too much Reaper

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Reaper)
summary(fit)
# Generate predictions for the line
Reaper$predicted <- predict(fit, Reaper)

# Create interactive scatterplot
library(plotly)
Reaper_plot <- plot_ly(
  Reaper,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Reaper"
  )
# Add the regression line
Reaper_plot <- add_trace(
  Reaper_plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Reaper_plot
Reaper_plot


z2 <- scale(Reaper[,c(2,3,4,7,8,9)])
score2 <- apply(z2, 1, mean)
Reaper <- cbind(Reaper, score2)
Reaper <- data.frame(lapply(Reaper, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Reaper)
summary(fit2)
# Generate predictions for the line
Reaper$predicted2 <- predict(fit2, Reaper)

# Create interactive scatterplot
library(plotly)
Reaper_plot2 <- plot_ly(
  Reaper,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Reaper is being carried by team"
  )
Reaper_plot2
# Add the regression line
Reaper_plot2 <- add_trace(
  Reaper_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Reaper_plot
Reaper_plot2

