## Echo
Echo <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Echo')
Echo <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Echo' & (OWL_stats_2023$stat_name=='Sticky Bombs Kills' | OWL_stats_2023$stat_name=='Focusing Beam Kills' | OWL_stats_2023$stat_name=='Critical Hits' | OWL_stats_2023$stat_name=='Shots Fired' | OWL_stats_2023$stat_name=='Shots Hit'  | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Final Blows' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Echo <- aggregate(amount ~ player_name + stat_name, Echo, sum)
library(tidyr)

Echo <- Echo %>%
  spread(stat_name, amount)

Echo[is.na(Echo)] <- 0
Echo <- subset(Echo,Echo$`Time Played`>=300)
Echo[, c(2,3,4,5,6,7,8,11,12,13)] <- Echo[, c(2,3,4,5,6,7,8,11,12,13)] / Echo[, 14]
Echo[,'Games Won'] <- Echo[,"Games Won"]/Echo[,"Games Played"]
# removing 
Echo <- subset(Echo, select = -`Games Played`)

z <- scale(Echo[,c(2,3,4,6,7,8,9,10,11,12)])
score <- apply(z, 1, mean)
Echo <- cbind(Echo, score)
Echo <- data.frame(lapply(Echo, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Echo[,c(2:14)])

## Whos playing too much Echo

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Echo)
summary(fit)
# Generate predictions for the line
Echo$predicted <- predict(fit, Echo)

# Create interactive scatterplot
library(plotly)
Echo_plot <- plot_ly(
  Echo,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Echo"
  )
# Add the regression line
Echo_plot <- add_trace(
  Echo_plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Echo_plot
Echo_plot


z2 <- scale(Echo[,c(2,3,4,7,8,9)])
score2 <- apply(z2, 1, mean)
Echo <- cbind(Echo, score2)
Echo <- data.frame(lapply(Echo, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Echo)
summary(fit2)
# Generate predictions for the line
Echo$predicted2 <- predict(fit2, Echo)

# Create interactive scatterplot
library(plotly)
Echo_plot2 <- plot_ly(
  Echo,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Echo is being carried by team"
  )
Echo_plot2
# Add the regression line
Echo_plot2 <- add_trace(
  Echo_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Echo_plot
Echo_plot2

