## Mercy
Mercy <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Mercy')
Mercy <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Mercy' & (OWL_stats_2023$stat_name=='Players Resurrected' | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Healing Amplified' | OWL_stats_2023$stat_name=='Damage Amplified' | OWL_stats_2023$stat_name=='Offensive Assists' | OWL_stats_2023$stat_name=='Defensive Assists' | OWL_stats_2023$stat_name=='Players Saved' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Healing Done' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Mercy <- aggregate(amount ~ player_name + stat_name, Mercy, sum)
library(tidyr)

Mercy <- Mercy %>%
  spread(stat_name, amount)

Mercy[is.na(Mercy)] <- 0
Mercy <- subset(Mercy,Mercy$`Time Played`>=300)
Mercy[, c(2,3,4,5,6,7,10,11,12,13)] <- Mercy[, c(2,3,4,5,6,7,10,11,12,13)] / Mercy$`Time Played`
Mercy[,9] <- Mercy[,9]/Mercy[,8]
# removing 
Mercy <- Mercy[,-c(8)]

z <- scale(Mercy[,c(2,3,4,5,6,8,9,10,11,12,13)])
score <- apply(z, 1, mean)
Mercy <- cbind(Mercy, score)
Mercy <- data.frame(lapply(Mercy, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Mercy[,c(2:14)])

## Whos playing too much Mercy

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Mercy)
summary(fit)
# Generate predictions for the line
Mercy$predicted <- predict(fit, Mercy)

# Create interactive scatterplot
library(plotly)
Mercy_plot1 <- plot_ly(
  Mercy,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Mercy"
  )
# Add the regression line
Mercy_plot1 <- add_trace(
  Mercy_plot1,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the plot
Mercy_plot1


z2 <- scale(Mercy[,c(2,3,4,6,8,9,10)])
score2 <- apply(z2, 1, mean)
Mercy <- cbind(Mercy, score2)
Mercy <- data.frame(lapply(Mercy, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Mercy)
summary(fit2)
# Generate predictions for the line
Mercy$predicted2 <- predict(fit2, Mercy)

# Create interactive scatterplot
library(plotly)
Mercy_plot2 <- plot_ly(
  Mercy,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Mercy is being carried by team"
  )
Mercy_plot2
# Add the regression line
Mercy_plot2 <- add_trace(
  Mercy_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the plot
Mercy_plot2

