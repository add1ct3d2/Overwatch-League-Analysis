## Brigitte
Brigitte <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Brigitte')
Brigitte <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Brigitte' & (OWL_stats_2023$stat_name=='Armor Provided' | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Inspire Uptime' | OWL_stats_2023$stat_name=='Players Saved' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Healing Done' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Brigitte <- aggregate(amount ~ player_name + stat_name, Brigitte, sum)
library(tidyr)

Brigitte <- Brigitte %>%
  spread(stat_name, amount)

Brigitte[is.na(Brigitte)] <- 0
Brigitte <- subset(Brigitte,Brigitte$`Time Played`>=300)
Brigitte[, c(2,3,4,5,6,9,10,11)] <- Brigitte[, c(2,3,4,5,6,9,10,11)] / Brigitte$`Time Played`
Brigitte[,8] <- Brigitte[,8]/Brigitte[,7]
# removing 
Brigitte <- Brigitte[,-c(7)]

z <- scale(Brigitte[,c(2,3,4,6,7,8,9,10)])
score <- apply(z, 1, mean)
Brigitte <- cbind(Brigitte, score)
Brigitte <- data.frame(lapply(Brigitte, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Brigitte[,c(2:12)])

## Whos playing too much Brigitte

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Brigitte)
summary(fit)
# Generate predictions for the line
Brigitte$predicted <- predict(fit, Brigitte)

# Create interactive scatterplot
library(plotly)
plot <- plot_ly(
  Brigitte,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Brigitte"
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


z2 <- scale(Brigitte[,c(2,3,4,6,8,9,10)])
score2 <- apply(z2, 1, mean)
Brigitte <- cbind(Brigitte, score2)
Brigitte <- data.frame(lapply(Brigitte, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Brigitte)
summary(fit2)
# Generate predictions for the line
Brigitte$predicted2 <- predict(fit2, Brigitte)

# Create interactive scatterplot
library(plotly)
plot2 <- plot_ly(
  Brigitte,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Brigitte is being carried"
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
