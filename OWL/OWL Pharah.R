## Pharah
Pharah <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Pharah')
Pharah <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Pharah' & (OWL_stats_2023$stat_name=='Rocket Direct Hits' | OWL_stats_2023$stat_name=='Airtime' | OWL_stats_2023$stat_name=='Barrage Kills' | OWL_stats_2023$stat_name=='Critical Hits' | OWL_stats_2023$stat_name=='Shots Fired' | OWL_stats_2023$stat_name=='Shots Hit'  | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Final Blows' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Pharah <- aggregate(amount ~ player_name + stat_name, Pharah, sum)
library(tidyr)

Pharah <- Pharah %>%
  spread(stat_name, amount)

Pharah[is.na(Pharah)] <- 0
Pharah <- subset(Pharah,Pharah$`Time Played`>=300)
Pharah[, c(2,3,4,5,6,7,8,11,12,13)] <- Pharah[, c(2,3,4,5,6,7,8,11,12,13)] / Pharah[, 14]
Pharah[,'Games Won'] <- Pharah[,"Games Won"]/Pharah[,"Games Played"]
# removing 
Pharah <- subset(Pharah, select = -`Games Played`)

z <- scale(Pharah[,c(2,3,4,5,7,8,9,10,11,12)])
score <- apply(z, 1, mean)
Pharah <- cbind(Pharah, score)
Pharah <- data.frame(lapply(Pharah, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Pharah[,c(2:14)])

## Whos playing too much Pharah

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Pharah)
summary(fit)
# Generate predictions for the line
Pharah$predicted <- predict(fit, Pharah)

# Create interactive scatterplot
library(plotly)
Pharah_plot <- plot_ly(
  Pharah,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Pharah"
  )
# Add the regression line
Pharah_plot <- add_trace(
  Pharah_plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Pharah_plot
Pharah_plot


z2 <- scale(Pharah[,c(2,3,4,7,8,9)])
score2 <- apply(z2, 1, mean)
Pharah <- cbind(Pharah, score2)
Pharah <- data.frame(lapply(Pharah, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Pharah)
summary(fit2)
# Generate predictions for the line
Pharah$predicted2 <- predict(fit2, Pharah)

# Create interactive scatterplot
library(plotly)
Pharah_plot2 <- plot_ly(
  Pharah,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Pharah is being carried by team"
  )
Pharah_plot2
# Add the regression line
Pharah_plot2 <- add_trace(
  Pharah_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Pharah_plot
Pharah_plot2

