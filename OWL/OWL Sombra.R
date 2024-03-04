## Sombra
Sombra <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Sombra')
Sombra <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Sombra' & (OWL_stats_2023$stat_name=='EMP Kills' | OWL_stats_2023$stat_name=='Enemies Hacked' | OWL_stats_2023$stat_name=='Low Health Teleports' | OWL_stats_2023$stat_name=='Critical Hits' | OWL_stats_2023$stat_name=='Shots Fired' | OWL_stats_2023$stat_name=='Shots Hit'  | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Final Blows' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Sombra <- aggregate(amount ~ player_name + stat_name, Sombra, sum)
library(tidyr)

Sombra <- Sombra %>%
  spread(stat_name, amount)

Sombra[is.na(Sombra)] <- 0
Sombra <- subset(Sombra,Sombra$`Time Played`>=300)
Sombra[, c(2,3,4,5,6,7,8,9,12,13,14)] <- Sombra[, c(2,3,4,5,6,7,8,9,12,13,14)] / Sombra[, 15]
Sombra[,'Games Won'] <- Sombra[,"Games Won"]/Sombra[,"Games Played"]
# removing 
Sombra <- subset(Sombra, select = -`Games Played`)

z <- scale(Sombra[,c(2,3,4,6,7,8,9,10,11,12,13)])
score <- apply(z, 1, mean)
Sombra <- cbind(Sombra, score)
Sombra <- data.frame(lapply(Sombra, function(x) 
  if(is.numeric(x)) round(x, 5) else x))

z2 <- scale(Sombra[,c(2,3,4,6,7,8,9,11,12,13)])
score2 <- apply(z2, 1, mean)
Sombra <- cbind(Sombra, score2)
Sombra <- data.frame(lapply(Sombra, function(x) 
  if(is.numeric(x)) round(x, 5) else x))

library(psych)
corr.test(Sombra[,c(2:16)])

## Whos playing too much Sombra

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Sombra)
summary(fit)
# Generate predictions for the line
Sombra$predicted <- predict(fit, Sombra)

# Create interactive scatterplot
library(plotly)
Sombra_plot <- plot_ly(
  Sombra,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Sombra"
  )
# Add the regression line
Sombra_plot <- add_trace(
  Sombra_plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Sombra_plot
Sombra_plot

# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Sombra)
summary(fit2)
# Generate predictions for the line
Sombra$predicted2 <- predict(fit2, Sombra)

# Create interactive scatterplot
library(plotly)
Sombra_plot2 <- plot_ly(
  Sombra,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Sombra is being carried by team"
  )
Sombra_plot2
# Add the regression line
Sombra_plot2 <- add_trace(
  Sombra_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Sombra_plot
Sombra_plot2

