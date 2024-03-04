## Genji
Genji <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Genji')
Genji <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Genji' & (OWL_stats_2023$stat_name=='Dragonblade Kills' | OWL_stats_2023$stat_name=='Swift Strike Resets' | OWL_stats_2023$stat_name=='Deflection Kills' | OWL_stats_2023$stat_name=='Critical Hits' | OWL_stats_2023$stat_name=='Shots Fired' | OWL_stats_2023$stat_name=='Shots Hit'  | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Final Blows' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Genji <- aggregate(amount ~ player_name + stat_name, Genji, sum)
library(tidyr)

Genji <- Genji %>%
  spread(stat_name, amount)

Genji[is.na(Genji)] <- 0
Genji <- subset(Genji,Genji$`Time Played`>=300)
Genji[, c(2,3,4,5,6,7,8,9,12,13,14)] <- Genji[, c(2,3,4,5,6,7,8,9,12,13,14)] / Genji[, 15]
Genji[,'Games Won'] <- Genji[,"Games Won"]/Genji[,"Games Played"]
# removing 
Genji <- subset(Genji, select = -`Games Played`)

z <- scale(Genji[,c(2,3,4,6,7,8,9,10,11,12,13)])
score <- apply(z, 1, mean)
Genji <- cbind(Genji, score)
Genji <- data.frame(lapply(Genji, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Genji[,c(2:15)])

## Whos playing too much Genji

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Genji)
summary(fit)
# Generate predictions for the line
Genji$predicted <- predict(fit, Genji)

# Create interactive scatterplot
library(plotly)
Genji_plot <- plot_ly(
  Genji,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Genji"
  )
# Add the regression line
Genji_plot <- add_trace(
  Genji_plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Genji_plot
Genji_plot


z2 <- scale(Genji[,c(2,3,4,7,8,9)])
score2 <- apply(z2, 1, mean)
Genji <- cbind(Genji, score2)
Genji <- data.frame(lapply(Genji, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Genji)
summary(fit2)
# Generate predictions for the line
Genji$predicted2 <- predict(fit2, Genji)

# Create interactive scatterplot
library(plotly)
Genji_plot2 <- plot_ly(
  Genji,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Genji is being carried by team"
  )
Genji_plot2
# Add the regression line
Genji_plot2 <- add_trace(
  Genji_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Genji_plot
Genji_plot2

