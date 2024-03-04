## Mei
Mei <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Mei')
Mei <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Mei' & (OWL_stats_2023$stat_name=='Blizzard Kills' | OWL_stats_2023$stat_name=='Self Healing' | OWL_stats_2023$stat_name=='Damage Blocked' | OWL_stats_2023$stat_name=='Critical Hits' | OWL_stats_2023$stat_name=='Shots Fired' | OWL_stats_2023$stat_name=='Shots Hit'  | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Final Blows' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Mei <- aggregate(amount ~ player_name + stat_name, Mei, sum)
library(tidyr)

Mei <- Mei %>%
  spread(stat_name, amount)

Mei[is.na(Mei)] <- 0
Mei <- subset(Mei,Mei$`Time Played`>=300)
Mei[, c(2,3,4,5,6,7,8,11,12,13)] <- Mei[, c(2,3,4,5,6,7,8,11,12,13)] / Mei[, 14]
Mei[,'Games Won'] <- Mei[,"Games Won"]/Mei[,"Games Played"]
# removing 
Mei <- subset(Mei, select = -`Games Played`)

z <- scale(Mei[,c(2,3,4,5,7,8,9,10,11,12)])
score <- apply(z, 1, mean)
Mei <- cbind(Mei, score)
Mei <- data.frame(lapply(Mei, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Mei[,c(2:14)])

## Whos playing too much Mei

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Mei)
summary(fit)
# Generate predictions for the line
Mei$predicted <- predict(fit, Mei)

# Create interactive scatterplot
library(plotly)
Mei_plot <- plot_ly(
  Mei,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Mei"
  )
# Add the regression line
Mei_plot <- add_trace(
  Mei_plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Mei_plot
Mei_plot


z2 <- scale(Mei[,c(2,3,4,7,8,9)])
score2 <- apply(z2, 1, mean)
Mei <- cbind(Mei, score2)
Mei <- data.frame(lapply(Mei, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Mei)
summary(fit2)
# Generate predictions for the line
Mei$predicted2 <- predict(fit2, Mei)

# Create interactive scatterplot
library(plotly)
Mei_plot2 <- plot_ly(
  Mei,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Mei is being carried by team"
  )
Mei_plot2
# Add the regression line
Mei_plot2 <- add_trace(
  Mei_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Mei_plot
Mei_plot2

