## Baptiste
Baptiste <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Baptiste')
Baptiste <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Baptiste' & (OWL_stats_2023$stat_name=='Amplification Matrix Assists' | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Healing Amplified' | OWL_stats_2023$stat_name=='Damage Amplified' | OWL_stats_2023$stat_name=='Damage Prevented' | OWL_stats_2023$stat_name=='Immortality Field Deaths Prevented' | OWL_stats_2023$stat_name=='Players Saved' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Healing Done' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Baptiste <- aggregate(amount ~ player_name + stat_name, Baptiste, sum)
library(tidyr)

Baptiste <- Baptiste %>%
  spread(stat_name, amount)

Baptiste[is.na(Baptiste)] <- 0
Baptiste <- subset(Baptiste,Baptiste$`Time Played`>=300)
Baptiste[, c(2,3,4,5,6,7,8,11,12,13,14)] <- Baptiste[, c(2,3,4,5,6,7,8,11,12,13,14)] / Baptiste$`Time Played`
Baptiste[,10] <- Baptiste[,10]/Baptiste[,9]
# removing 
Baptiste <- Baptiste[,-c(9)]

z <- scale(Baptiste[,c(2,3,4,5,6,8,9,10,11,12,13)])
score <- apply(z, 1, mean)
Baptiste <- cbind(Baptiste, score)
Baptiste <- data.frame(lapply(Baptiste, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Baptiste[,c(2:15)])

## Whos playing too much Baptiste

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Baptiste)
summary(fit)
# Generate predictions for the line
Baptiste$predicted <- predict(fit, Baptiste)

# Create interactive scatterplot
library(plotly)
Baptiste_plot1 <- plot_ly(
  Baptiste,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Baptiste"
  )
# Add the regression line
Baptiste_plot1 <- add_trace(
  Baptiste_plot1,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the plot
Baptiste_plot1


z2 <- scale(Baptiste[,c(2,3,4,6,8,9,10)])
score2 <- apply(z2, 1, mean)
Baptiste <- cbind(Baptiste, score2)
Baptiste <- data.frame(lapply(Baptiste, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Baptiste)
summary(fit2)
# Generate predictions for the line
Baptiste$predicted2 <- predict(fit2, Baptiste)

# Create interactive scatterplot
library(plotly)
Baptiste_plot2 <- plot_ly(
  Baptiste,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Baptiste is being carried by team"
  )
Baptiste_plot2
# Add the regression line
Baptiste_plot2 <- add_trace(
  Baptiste_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the plot
Baptiste_plot2

