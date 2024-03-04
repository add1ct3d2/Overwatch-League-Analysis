## Orisa
Orisa <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Orisa')
Orisa <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Orisa' & (OWL_stats_2023$stat_name=='Damage Absorbed' | OWL_stats_2023$stat_name=='Solo Kills' | OWL_stats_2023$stat_name=='Critical Hits' | OWL_stats_2023$stat_name=='Shots Fired' | OWL_stats_2023$stat_name=='Shots Hit'  | OWL_stats_2023$stat_name=='All Damage Done' | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Final Blows' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Orisa <- aggregate(amount ~ player_name + stat_name, Orisa, sum)
library(tidyr)

Orisa <- Orisa %>%
  spread(stat_name, amount)

Orisa[is.na(Orisa)] <- 0
Orisa <- subset(Orisa,Orisa$`Time Played`>=300)
Orisa$KD <- Orisa$Eliminations/Orisa$Deaths
Orisa[, c(2,3,4,5,6,7,8,11,12,13)] <- Orisa[, c(2,3,4,5,6,7,8,11,12,13)] / Orisa[, 14]
Orisa[,'Games Won'] <- Orisa[,"Games Won"]/Orisa[,"Games Played"]
# removing 
Orisa <- subset(Orisa, select = -`Games Played`)

z <- scale(Orisa[,c(2,3,4,5,6,7,8,9,10,11,12,14)])
score <- apply(z, 1, mean)
Orisa <- cbind(Orisa, score)
Orisa <- data.frame(lapply(Orisa, function(x) 
  if(is.numeric(x)) round(x, 5) else x))

z2 <- scale(Orisa[,c(2,3,4,5,6,7,8,10,11,12,14)])
score2 <- apply(z2, 1, mean)
Orisa <- cbind(Orisa, score2)
Orisa <- data.frame(lapply(Orisa, function(x) 
  if(is.numeric(x)) round(x, 5) else x))

library(psych)
corr.test(Orisa[,c(2:16)])

## Whos playing too much Orisa

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Orisa)
summary(fit)
# Generate predictions for the line
Orisa$predicted <- predict(fit, Orisa)

# Create interactive scatterplot
library(plotly)
Orisa_plot <- plot_ly(
  Orisa,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Orisa"
  )
# Add the regression line
Orisa_plot <- add_trace(
  Orisa_plot,
  x = ~Time.Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Orisa_plot
Orisa_plot

# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Orisa)
summary(fit2)
# Generate predictions for the line
Orisa$predicted2 <- predict(fit2, Orisa)

# Create interactive scatterplot
library(plotly)
Orisa_plot2 <- plot_ly(
  Orisa,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Whos Orisa is being carried by team"
  )
Orisa_plot2
# Add the regression line
Orisa_plot2 <- add_trace(
  Orisa_plot2,
  x = ~Games.Won,
  y = ~predicted2,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the Orisa_plot
Orisa_plot2

