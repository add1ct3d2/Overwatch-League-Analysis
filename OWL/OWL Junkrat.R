## Junkrat
Junkrat <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Junkrat')
Junkrat <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Junkrat' & (OWL_stats_2023$stat_name=='Dragonstrike Kills' | OWL_stats_2023$stat_name=='Recon Assists' | OWL_stats_2023$stat_name=='Storm Arrow Kills' | OWL_stats_2023$stat_name=='Critical Hits' | OWL_stats_2023$stat_name=='Shots Fired' | OWL_stats_2023$stat_name=='Shots Hit'  | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Final Blows' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Junkrat <- aggregate(amount ~ player_name + stat_name, Junkrat, sum)
library(tidyr)

Junkrat <- Junkrat %>%
  spread(stat_name, amount)

Junkrat[is.na(Junkrat)] <- 0
Junkrat <- subset(Junkrat,Junkrat$`Time Played`>=300)
Junkrat[, c(2,3,4,5,6,7,8,11,12,13,14)] <- Junkrat[, c(2,3,4,5,6,7,8,11,12,13,14)] / Junkrat[, 15]
Junkrat[,'Games Won'] <- Junkrat[,"Games Won"]/Junkrat[,"Games Played"]
# removing 
Junkrat <- subset(Junkrat, select = -`Games Played`)

z <- scale(Junkrat[,c(2,3,4,6,7,8,9,10,11,12,13)])
score <- apply(z, 1, mean)
Junkrat <- cbind(Junkrat, score)
Junkrat <- data.frame(lapply(Junkrat, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Junkrat[,c(2:15)])

## Whos playing too much Junkrat

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Junkrat)
summary(fit)
# Generate predictions for the line
Junkrat$predicted <- predict(fit, Junkrat)

# Create interactive scatterplot
library(plotly)
Junkrat_plot <- plot_ly(
  Junkrat,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Junkrat"
  )
# Add the regression line
Junkrat_plot <- add_trace(
  Junkrat_plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Junkrat_plot
Junkrat_plot


z2 <- scale(Junkrat[,c(2,3,4,7,8,9)])
score2 <- apply(z2, 1, mean)
Junkrat <- cbind(Junkrat, score2)
Junkrat <- data.frame(lapply(Junkrat, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Junkrat)
summary(fit2)
# Generate predictions for the line
Junkrat$predicted2 <- predict(fit2, Junkrat)

# Create interactive scatterplot
library(plotly)
Junkrat_plot2 <- plot_ly(
  Junkrat,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Junkrat is being carried by team"
  )
Junkrat_plot2
# Add the regression line
Junkrat_plot2 <- add_trace(
  Junkrat_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Junkrat_plot
Junkrat_plot2

