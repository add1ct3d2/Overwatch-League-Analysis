## Cassidy
Cassidy <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Cassidy')
Cassidy <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Cassidy' & (OWL_stats_2023$stat_name=='Magnetic Grenade Kills' | OWL_stats_2023$stat_name=='Barrier Damage Done' | OWL_stats_2023$stat_name=='Critical Hits' | OWL_stats_2023$stat_name=='Scoped Shots' | OWL_stats_2023$stat_name=='Scoped Hits'  | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Final Blows' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Cassidy <- aggregate(amount ~ player_name + stat_name, Cassidy, sum)
library(tidyr)

Cassidy <- Cassidy %>%
  spread(stat_name, amount)

Cassidy[is.na(Cassidy)] <- 0
Cassidy <- subset(Cassidy,Cassidy$`Time Played`>=300)
Cassidy[, c(2,3,4,5,6,7,8,11)] <- Cassidy[, c(2,3,4,5,6,7,8,11)] / Cassidy[, 12]
Cassidy[,'Games Won'] <- Cassidy[,"Games Won"]/Cassidy[,"Games Played"]
# removing 
Cassidy <- subset(Cassidy, select = -`Games Played`)

z <- scale(Cassidy[,c(2,3,4,5,7,8,9,10,11)])
score <- apply(z, 1, mean)
Cassidy <- cbind(Cassidy, score)
Cassidy <- data.frame(lapply(Cassidy, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Cassidy[,c(2:12)])

## Whos playing too much Cassidy

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Cassidy)
summary(fit)
# Generate predictions for the line
Cassidy$predicted <- predict(fit, Cassidy)

# Create interactive scatterplot
library(plotly)
Cassidy_plot <- plot_ly(
  Cassidy,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Cassidy"
  )
# Add the regression line
Cassidy_plot <- add_trace(
  Cassidy_plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Cassidy_plot
Cassidy_plot


z2 <- scale(Cassidy[,c(2,3,4,7,8,9)])
score2 <- apply(z2, 1, mean)
Cassidy <- cbind(Cassidy, score2)
Cassidy <- data.frame(lapply(Cassidy, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Cassidy)
summary(fit2)
# Generate predictions for the line
Cassidy$predicted2 <- predict(fit2, Cassidy)

# Create interactive scatterplot
library(plotly)
Cassidy_plot2 <- plot_ly(
  Cassidy,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Cassidy is being carried by team"
  )
Cassidy_plot2
# Add the regression line
Cassidy_plot2 <- add_trace(
  Cassidy_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Cassidy_plot
Cassidy_plot2

