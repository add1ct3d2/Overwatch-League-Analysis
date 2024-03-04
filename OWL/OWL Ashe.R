## Ashe
Ashe <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Ashe')
Ashe <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Ashe' & (OWL_stats_2023$stat_name=='Damage - Dynamite' | OWL_stats_2023$stat_name=='Bob Gun Damage' | OWL_stats_2023$stat_name=='Critical Hits' | OWL_stats_2023$stat_name=='Scoped Shots' | OWL_stats_2023$stat_name=='Scoped Hits'  | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Final Blows' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Ashe <- aggregate(amount ~ player_name + stat_name, Ashe, sum)
library(tidyr)

Ashe <- Ashe %>%
  spread(stat_name, amount)

Ashe[is.na(Ashe)] <- 0
Ashe <- subset(Ashe,Ashe$`Time Played`>=300)
Ashe[, c(2,3,4,5,6,7,8,9,12,13)] <- Ashe[, c(2,3,4,5,6,7,8,9,12,13)] / Ashe[, 14]
Ashe[,11] <- Ashe[,11]/Ashe[,10]
# removing 
Ashe <- Ashe[,-c(10)]

z <- scale(Ashe[,c(2,3,4,5,6,8,9,10,11,12,13)])
score <- apply(z, 1, mean)
Ashe <- cbind(Ashe, score)
Ashe <- data.frame(lapply(Ashe, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Ashe[,c(2:14)])

## Whos playing too much Ashe

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Ashe)
summary(fit)
# Generate predictions for the line
Ashe$predicted <- predict(fit, Ashe)

# Create interactive scatterplot
library(plotly)
Ashe_plot <- plot_ly(
  Ashe,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Ashe"
  )
# Add the regression line
Ashe_plot <- add_trace(
  Ashe_plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Ashe_plot
Ashe_plot


z2 <- scale(Ashe[,c(2,3,4,7,8,9)])
score2 <- apply(z2, 1, mean)
Ashe <- cbind(Ashe, score2)
Ashe <- data.frame(lapply(Ashe, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Ashe)
summary(fit2)
# Generate predictions for the line
Ashe$predicted2 <- predict(fit2, Ashe)

# Create interactive scatterplot
library(plotly)
Ashe_plot2 <- plot_ly(
  Ashe,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Ashe is being carried by team"
  )
Ashe_plot2
# Add the regression line
Ashe_plot2 <- add_trace(
  Ashe_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Ashe_plot
Ashe_plot2

