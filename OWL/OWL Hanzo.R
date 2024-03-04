## Hanzo
Hanzo <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Hanzo')
Hanzo <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Hanzo' & (OWL_stats_2023$stat_name=='Dragonstrike Kills' | OWL_stats_2023$stat_name=='Recon Assists' | OWL_stats_2023$stat_name=='Storm Arrow Kills' | OWL_stats_2023$stat_name=='Critical Hits' | OWL_stats_2023$stat_name=='Shots Fired' | OWL_stats_2023$stat_name=='Shots Hit'  | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Final Blows' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Hanzo <- aggregate(amount ~ player_name + stat_name, Hanzo, sum)
library(tidyr)

Hanzo <- Hanzo %>%
  spread(stat_name, amount)

Hanzo[is.na(Hanzo)] <- 0
Hanzo <- subset(Hanzo,Hanzo$`Time Played`>=300)
Hanzo[, c(2,3,4,5,6,7,8,11,12,13,14)] <- Hanzo[, c(2,3,4,5,6,7,8,11,12,13,14)] / Hanzo[, 15]
Hanzo[,'Games Won'] <- Hanzo[,"Games Won"]/Hanzo[,"Games Played"]
# removing 
Hanzo <- subset(Hanzo, select = -`Games Played`)

z <- scale(Hanzo[,c(2,3,4,6,7,8,9,10,11,12,13)])
score <- apply(z, 1, mean)
Hanzo <- cbind(Hanzo, score)
Hanzo <- data.frame(lapply(Hanzo, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Hanzo[,c(2:15)])

## Whos playing too much Hanzo

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Hanzo)
summary(fit)
# Generate predictions for the line
Hanzo$predicted <- predict(fit, Hanzo)

# Create interactive scatterplot
library(plotly)
Hanzo_plot <- plot_ly(
  Hanzo,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Hanzo"
  )
# Add the regression line
Hanzo_plot <- add_trace(
  Hanzo_plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Hanzo_plot
Hanzo_plot


z2 <- scale(Hanzo[,c(2,3,4,7,8,9)])
score2 <- apply(z2, 1, mean)
Hanzo <- cbind(Hanzo, score2)
Hanzo <- data.frame(lapply(Hanzo, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Hanzo)
summary(fit2)
# Generate predictions for the line
Hanzo$predicted2 <- predict(fit2, Hanzo)

# Create interactive scatterplot
library(plotly)
Hanzo_plot2 <- plot_ly(
  Hanzo,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Hanzo is being carried by team"
  )
Hanzo_plot2
# Add the regression line
Hanzo_plot2 <- add_trace(
  Hanzo_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Hanzo_plot
Hanzo_plot2

