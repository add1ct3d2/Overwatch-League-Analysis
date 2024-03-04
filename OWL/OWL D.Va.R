## D.Va
D.Va <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='D.Va')
D.Va <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='D.Va' & (OWL_stats_2023$stat_name=='Damage Blocked' | OWL_stats_2023$stat_name=='Solo Kills' | OWL_stats_2023$stat_name=='Critical Hits' | OWL_stats_2023$stat_name=='Shots Fired' | OWL_stats_2023$stat_name=='Shots Hit'  | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Final Blows' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
D.Va <- aggregate(amount ~ player_name + stat_name, D.Va, sum)
library(tidyr)

D.Va <- D.Va %>%
  spread(stat_name, amount)

D.Va[is.na(D.Va)] <- 0
D.Va <- subset(D.Va,D.Va$`Time Played`>=300)
D.Va$KD <- D.Va$Eliminations/D.Va$Deaths
D.Va[, c(2,3,4,5,6,7,8,11,12,13)] <- D.Va[, c(2,3,4,5,6,7,8,11,12,13)] / D.Va[, 14]
D.Va[,'Games Won'] <- D.Va[,"Games Won"]/D.Va[,"Games Played"]
# removing 
D.Va <- subset(D.Va, select = -`Games Played`)

z <- scale(D.Va[,c(2,3,4,5,6,7,8,9,10,11,12,14)])
score <- apply(z, 1, mean)
D.Va <- cbind(D.Va, score)
D.Va <- data.frame(lapply(D.Va, function(x) 
  if(is.numeric(x)) round(x, 5) else x))

z2 <- scale(D.Va[,c(2,3,4,5,6,7,8,10,11,12,14)])
score2 <- apply(z2, 1, mean)
D.Va <- cbind(D.Va, score2)
D.Va <- data.frame(lapply(D.Va, function(x) 
  if(is.numeric(x)) round(x, 5) else x))

library(psych)
corr.test(D.Va[,c(2:16)])

## Whos playing too much D.Va

# Fit the linear model
fit <- lm(score ~ Time.Played, data = D.Va)
summary(fit)
# Generate predictions for the line
D.Va$predicted <- predict(fit, D.Va)

# Create interactive scatterplot
library(plotly)
D.Va_plot <- plot_ly(
  D.Va,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much D.Va"
  )
# Add the regression line
D.Va_plot <- add_trace(
  D.Va_plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the D.Va_plot
D.Va_plot

# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = D.Va)
summary(fit2)
# Generate predictions for the line
D.Va$predicted2 <- predict(fit2, D.Va)

# Create interactive scatterplot
library(plotly)
D.Va_plot2 <- plot_ly(
  D.Va,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos D.Va is being carried by team"
  )
D.Va_plot2
# Add the regression line
D.Va_plot2 <- add_trace(
  D.Va_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the D.Va_plot
D.Va_plot2

