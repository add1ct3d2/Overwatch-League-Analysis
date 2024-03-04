## Kiriko
Kiriko <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Kiriko')
Kiriko <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Kiriko' & (OWL_stats_2023$stat_name=='Kitsune Rush Assists' |OWL_stats_2023$stat_name=='Negative Effects Cleansed'| OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Offensive Assists' | OWL_stats_2023$stat_name=='Defensive Assists' | OWL_stats_2023$stat_name=='Damage Prevented' | OWL_stats_2023$stat_name=='Players Saved' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Healing Done' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Kiriko <- aggregate(amount ~ player_name + stat_name, Kiriko, sum)
library(tidyr)

Kiriko <- Kiriko %>%
  spread(stat_name, amount)

Kiriko[is.na(Kiriko)] <- 0
Kiriko <- subset(Kiriko,Kiriko$`Time Played`>=300)
Kiriko[, c(2,3,4,5,6,9,10,11,12,13)] <- Kiriko[, c(2,3,4,5,6,9,10,11,12,13)] / Kiriko$`Time Played`
Kiriko[,8] <- Kiriko[,8]/Kiriko[,7]
# removing 
Kiriko <- Kiriko[,-c(7)]

z <- scale(Kiriko[,c(2,3,5,6,7,8,9,10,11,12)])
score <- apply(z, 1, mean)
Kiriko <- cbind(Kiriko, score)
Kiriko <- data.frame(lapply(Kiriko, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Kiriko[,c(2:14)])

## Whos playing too much Kiriko

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Kiriko)
summary(fit)
# Generate predictions for the line
Kiriko$predicted <- predict(fit, Kiriko)

# Create interactive scatterplot
library(plotly)
Kiriko_plot1 <- plot_ly(
  Kiriko,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Kiriko"
  )
# Add the regression line
Kiriko_plot1 <- add_trace(
  Kiriko_plot1,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the plot
Kiriko_plot1


z2 <- scale(Kiriko[,c(2,3,4,6,8,9,10)])
score2 <- apply(z2, 1, mean)
Kiriko <- cbind(Kiriko, score2)
Kiriko <- data.frame(lapply(Kiriko, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Kiriko)
summary(fit2)
# Generate predictions for the line
Kiriko$predicted2 <- predict(fit2, Kiriko)

# Create interactive scatterplot
library(plotly)
Kiriko_plot2 <- plot_ly(
  Kiriko,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Kiriko is being carried by team"
  )
Kiriko_plot2
# Add the regression line
Kiriko_plot2 <- add_trace(
  Kiriko_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the plot
Kiriko_plot2

