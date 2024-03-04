## Ana
Ana <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Ana')
Ana <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Ana' & (OWL_stats_2023$stat_name=='Enemies Slept' | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Healing Amplified' |OWL_stats_2023$stat_name=='Healing Prevented' | OWL_stats_2023$stat_name=='Nano Boost Assists' | OWL_stats_2023$stat_name=='Players Saved' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Healing Done' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Ana <- aggregate(amount ~ player_name + stat_name, Ana, sum)
library(tidyr)

Ana <- Ana %>%
  spread(stat_name, amount)

Ana[is.na(Ana)] <- 0
Ana <- subset(Ana,Ana$`Time Played`>=300)
Ana[, c(2,3,4,5,6,9,10,11,12,13)] <- Ana[, c(2,3,4,5,6,9,10,11,12,13)] / Ana$`Time Played`
Ana[,8] <- Ana[,8]/Ana[,7]
# removing 
Ana <- Ana[,-c(7)]

z <- scale(Ana[,c(2,3,5,6,7,8,9,10,11,12)])
score <- apply(z, 1, mean)
Ana <- cbind(Ana, score)
Ana <- data.frame(lapply(Ana, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Ana[,c(2:14)])

## Whos playing too much Ana

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Ana)
summary(fit)
# Generate predictions for the line
Ana$predicted <- predict(fit, Ana)

# Create interactive scatterplot
library(plotly)
plot <- plot_ly(
  Ana,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Ana"
  )
# Add the regression line
plot <- add_trace(
  plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the plot
plot


z2 <- scale(Ana[,c(2,3,4,6,8,9,10)])
score2 <- apply(z2, 1, mean)
Ana <- cbind(Ana, score2)
Ana <- data.frame(lapply(Ana, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Ana)
summary(fit2)
# Generate predictions for the line
Ana$predicted2 <- predict(fit2, Ana)

# Create interactive scatterplot
library(plotly)
plot2 <- plot_ly(
  Ana,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Ana is being carried by team"
  )
plot2
# Add the regression line
plot2 <- add_trace(
  plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the plot
plot2

