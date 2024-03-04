## Sojourn
Sojourn <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Sojourn')
Sojourn <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Sojourn' & (OWL_stats_2023$stat_name=='Railgun Kills' | OWL_stats_2023$stat_name=='Disruptor Shot Kills' | OWL_stats_2023$stat_name=='Railgun Critical Hits' | OWL_stats_2023$stat_name=='Critical Hits' | OWL_stats_2023$stat_name=='Shots Fired' | OWL_stats_2023$stat_name=='Shots Hit'  | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Final Blows' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Sojourn <- aggregate(amount ~ player_name + stat_name, Sojourn, sum)
library(tidyr)

Sojourn <- Sojourn %>%
  spread(stat_name, amount)

Sojourn[is.na(Sojourn)] <- 0
Sojourn <- subset(Sojourn,Sojourn$`Time Played`>=300)
Sojourn[, c(2,3,4,5,6,7,10,11,12,13)] <- Sojourn[, c(2,3,4,5,6,7,10,11,12,13)] / Sojourn[, 14]
Sojourn[,'Games Won'] <- Sojourn[,"Games Won"]/Sojourn[,"Games Played"]
# removing 
Sojourn <- subset(Sojourn, select = -`Games Played`)

z <- scale(Sojourn[,c(2,3,5,6,7,8,9,10,11,12)])
score <- apply(z, 1, mean)
Sojourn <- cbind(Sojourn, score)
Sojourn <- data.frame(lapply(Sojourn, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Sojourn[,c(2:14)])

## Whos playing too much Sojourn

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Sojourn)
summary(fit)
# Generate predictions for the line
Sojourn$predicted <- predict(fit, Sojourn)

# Create interactive scatterplot
library(plotly)
Sojourn_plot <- plot_ly(
  Sojourn,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Sojourn"
  )
# Add the regression line
Sojourn_plot <- add_trace(
  Sojourn_plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Sojourn_plot
Sojourn_plot


z2 <- scale(Sojourn[,c(2,3,4,7,8,9)])
score2 <- apply(z2, 1, mean)
Sojourn <- cbind(Sojourn, score2)
Sojourn <- data.frame(lapply(Sojourn, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Sojourn)
summary(fit2)
# Generate predictions for the line
Sojourn$predicted2 <- predict(fit2, Sojourn)

# Create interactive scatterplot
library(plotly)
Sojourn_plot2 <- plot_ly(
  Sojourn,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Sojourn is being carried by team"
  )
Sojourn_plot2
# Add the regression line
Sojourn_plot2 <- add_trace(
  Sojourn_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Sojourn_plot
Sojourn_plot2

