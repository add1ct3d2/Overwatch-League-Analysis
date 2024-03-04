## Junker_Queen
Junker_Queen <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Junker Queen')
Junker_Queen <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Junker Queen' & (OWL_stats_2023$stat_name=='Self Healing' | OWL_stats_2023$stat_name=='Solo Kills' | OWL_stats_2023$stat_name=='Critical Hits' | OWL_stats_2023$stat_name=='Shots Fired' | OWL_stats_2023$stat_name=='Shots Hit'  | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Final Blows' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Junker_Queen <- aggregate(amount ~ player_name + stat_name, Junker_Queen, sum)
library(tidyr)

Junker_Queen <- Junker_Queen %>%
  spread(stat_name, amount)

Junker_Queen[is.na(Junker_Queen)] <- 0
Junker_Queen <- subset(Junker_Queen,Junker_Queen$`Time Played`>=300)
Junker_Queen$KD <- Junker_Queen$Eliminations/Junker_Queen$Deaths
Junker_Queen[, c(2,3,4,5,6,9,10,11,12)] <- Junker_Queen[, c(2,3,4,5,6,9,10,11,12)] / Junker_Queen[, 13]
Junker_Queen[,'Games Won'] <- Junker_Queen[,"Games Won"]/Junker_Queen[,"Games Played"]
# removing 
Junker_Queen <- subset(Junker_Queen, select = -`Games Played`)

z <- scale(Junker_Queen[,c(2,3,5,6,7,8,9,10,11,13)])
score <- apply(z, 1, mean)
Junker_Queen <- cbind(Junker_Queen, score)
Junker_Queen <- data.frame(lapply(Junker_Queen, function(x) 
  if(is.numeric(x)) round(x, 5) else x))

z2 <- scale(Junker_Queen[,c(2,3,5,6,8,9,10,11,13)])
score2 <- apply(z2, 1, mean)
Junker_Queen <- cbind(Junker_Queen, score2)
Junker_Queen <- data.frame(lapply(Junker_Queen, function(x) 
  if(is.numeric(x)) round(x, 5) else x))

library(psych)
corr.test(Junker_Queen[,c(2:15)])

## Whos playing too much Junker_Queen

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Junker_Queen)
summary(fit)
# Generate predictions for the line
Junker_Queen$predicted <- predict(fit, Junker_Queen)

# Create interactive scatterplot
library(plotly)
Junker_Queen_plot <- plot_ly(
  Junker_Queen,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Junker_Queen"
  )
# Add the regression line
Junker_Queen_plot <- add_trace(
  Junker_Queen_plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Junker_Queen_plot
Junker_Queen_plot

# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Junker_Queen)
summary(fit2)
# Generate predictions for the line
Junker_Queen$predicted2 <- predict(fit2, Junker_Queen)

# Create interactive scatterplot
library(plotly)
Junker_Queen_plot2 <- plot_ly(
  Junker_Queen,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Junker_Queen is being carried by team"
  )
Junker_Queen_plot2
# Add the regression line
Junker_Queen_plot2 <- add_trace(
  Junker_Queen_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Junker_Queen_plot
Junker_Queen_plot2

