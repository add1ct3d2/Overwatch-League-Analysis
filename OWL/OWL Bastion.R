## Bastion
Bastion <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Bastion')
Bastion <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Bastion' & (OWL_stats_2023$stat_name=='Artillery Eliminations' | OWL_stats_2023$stat_name=='Barrier Damage Done' | OWL_stats_2023$stat_name=='Critical Hits' | OWL_stats_2023$stat_name=='Scoped Shots' | OWL_stats_2023$stat_name=='Scoped Hits'  | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Final Blows' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Bastion <- aggregate(amount ~ player_name + stat_name, Bastion, sum)
library(tidyr)

Bastion <- Bastion %>%
  spread(stat_name, amount)

Bastion[is.na(Bastion)] <- 0
Bastion <- subset(Bastion,Bastion$`Time Played`>=300)
Bastion[, c(2,3,4,5,6,7,8)] <- Bastion[, c(2,3,4,5,6,7,8)] / Bastion[, 11]
Bastion[,10] <- Bastion[,10]/Bastion[,9]
# removing 
Bastion <- Bastion[,-c(9)]

z <- scale(Bastion[,c(2,3,4,5,7,8,9)])
score <- apply(z, 1, mean)
Bastion <- cbind(Bastion, score)
Bastion <- data.frame(lapply(Bastion, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Bastion[,c(2:11)])

## Whos playing too much Bastion

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Bastion)
summary(fit)
# Generate predictions for the line
Bastion$predicted <- predict(fit, Bastion)

# Create interactive scatterplot
library(plotly)
Bastion_plot <- plot_ly(
  Bastion,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Bastion"
  )
# Add the regression line
Bastion_plot <- add_trace(
  Bastion_plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Bastion_plot
Bastion_plot


z2 <- scale(Bastion[,c(2,3,4,5,7,8)])
score2 <- apply(z2, 1, mean)
Bastion <- cbind(Bastion, score2)
Bastion <- data.frame(lapply(Bastion, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Bastion)
summary(fit2)
# Generate predictions for the line
Bastion$predicted2 <- predict(fit2, Bastion)

# Create interactive scatterplot
library(plotly)
Bastion_plot2 <- plot_ly(
  Bastion,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Bastion is being carried by team"
  )
Bastion_plot2
# Add the regression line
Bastion_plot2 <- add_trace(
  Bastion_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Bastion_plot
Bastion_plot2

