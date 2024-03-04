## Soldier_76
Soldier_76 <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Soldier: 76')
Soldier_76 <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Soldier: 76' & (OWL_stats_2023$stat_name=='Tactical Visor Kills' | OWL_stats_2023$stat_name=='Helix Rocket Kills' | OWL_stats_2023$stat_name=='Self Healing' | OWL_stats_2023$stat_name=='Critical Hits' | OWL_stats_2023$stat_name=='Shots Fired' | OWL_stats_2023$stat_name=='Shots Hit'  | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Final Blows' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Soldier_76 <- aggregate(amount ~ player_name + stat_name, Soldier_76, sum)
library(tidyr)

Soldier_76 <- Soldier_76 %>%
  spread(stat_name, amount)

Soldier_76[is.na(Soldier_76)] <- 0
Soldier_76 <- subset(Soldier_76,Soldier_76$`Time Played`>=300)
Soldier_76[, c(2,3,4,5,6,7,10,11,12,13,14)] <- Soldier_76[, c(2,3,4,5,6,7,10,11,12,13,14)] / Soldier_76[, 15]
Soldier_76[,'Games Won'] <- Soldier_76[,"Games Won"]/Soldier_76[,"Games Played"]
# removing 
Soldier_76 <- subset(Soldier_76, select = -`Games Played`)

z <- scale(Soldier_76[,c(2,3,4,6,7,8,9,10,11,12,13)])
score <- apply(z, 1, mean)
Soldier_76 <- cbind(Soldier_76, score)
Soldier_76 <- data.frame(lapply(Soldier_76, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Soldier_76[,c(2:15)])

## Whos playing too much Soldier_76

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Soldier_76)
summary(fit)
# Generate predictions for the line
Soldier_76$predicted <- predict(fit, Soldier_76)

# Create interactive scatterplot
library(plotly)
Soldier_76_plot <- plot_ly(
  Soldier_76,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Soldier_76"
  )
# Add the regression line
Soldier_76_plot <- add_trace(
  Soldier_76_plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Soldier_76_plot
Soldier_76_plot


z2 <- scale(Soldier_76[,c(2,3,4,7,8,9)])
score2 <- apply(z2, 1, mean)
Soldier_76 <- cbind(Soldier_76, score2)
Soldier_76 <- data.frame(lapply(Soldier_76, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Soldier_76)
summary(fit2)
# Generate predictions for the line
Soldier_76$predicted2 <- predict(fit2, Soldier_76)

# Create interactive scatterplot
library(plotly)
Soldier_76_plot2 <- plot_ly(
  Soldier_76,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Soldier_76 is being carried by team"
  )
Soldier_76_plot2
# Add the regression line
Soldier_76_plot2 <- add_trace(
  Soldier_76_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Soldier_76_plot
Soldier_76_plot2

