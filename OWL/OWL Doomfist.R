## Doomfist
Doomfist <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Doomfist')
Doomfist <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Doomfist' & (OWL_stats_2023$stat_name=='Shields Created' | OWL_stats_2023$stat_name=='Solo Kills' | OWL_stats_2023$stat_name=='Critical Hits' | OWL_stats_2023$stat_name=='Shots Fired' | OWL_stats_2023$stat_name=='Shots Hit'  | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Final Blows' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Doomfist <- aggregate(amount ~ player_name + stat_name, Doomfist, sum)
library(tidyr)

Doomfist <- Doomfist %>%
  spread(stat_name, amount)

Doomfist[is.na(Doomfist)] <- 0
Doomfist <- subset(Doomfist,Doomfist$`Time Played`>=300)
Doomfist$KD <- Doomfist$Eliminations/Doomfist$Deaths
Doomfist[, c(2,3,4,5,6,7,10,11,12,13)] <- Doomfist[, c(2,3,4,5,6,7,10,11,12,13)] / Doomfist[, 14]
Doomfist[,'Games Won'] <- Doomfist[,"Games Won"]/Doomfist[,"Games Played"]
# removing 
Doomfist <- subset(Doomfist, select = -`Games Played`)

z <- scale(Doomfist[,c(2,3,4,6,7,8,9,10,11,12,14)])
score <- apply(z, 1, mean)
Doomfist <- cbind(Doomfist, score)
Doomfist <- data.frame(lapply(Doomfist, function(x) 
  if(is.numeric(x)) round(x, 5) else x))

z2 <- scale(Doomfist[,c(2,3,4,6,7,9,10,11,12,14)])
score2 <- apply(z2, 1, mean)
Doomfist <- cbind(Doomfist, score2)
Doomfist <- data.frame(lapply(Doomfist, function(x) 
  if(is.numeric(x)) round(x, 5) else x))

library(psych)
corr.test(Doomfist[,c(2:16)])

## Whos playing too much Doomfist

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Doomfist)
summary(fit)
# Generate predictions for the line
Doomfist$predicted <- predict(fit, Doomfist)

# Create interactive scatterplot
library(plotly)
Doomfist_plot <- plot_ly(
  Doomfist,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Doomfist"
  )
# Add the regression line
Doomfist_plot <- add_trace(
  Doomfist_plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Doomfist_plot
Doomfist_plot

# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Doomfist)
summary(fit2)
# Generate predictions for the line
Doomfist$predicted2 <- predict(fit2, Doomfist)

# Create interactive scatterplot
library(plotly)
Doomfist_plot2 <- plot_ly(
  Doomfist,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Doomfist is being carried by team"
  )
Doomfist_plot2
# Add the regression line
Doomfist_plot2 <- add_trace(
  Doomfist_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Doomfist_plot
Doomfist_plot2

