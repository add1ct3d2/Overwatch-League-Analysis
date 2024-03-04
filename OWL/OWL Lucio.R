## Lucio

Lucio <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Lucio' & (OWL_stats_2023$stat_name=='Amped Heal Activations' | OWL_stats_2023$stat_name=='Amped Speed Activations'  | OWL_stats_2023$stat_name=='Assists'| OWL_stats_2023$stat_name=='Eliminations' | OWL_stats_2023$stat_name=='Healing Done' | OWL_stats_2023$stat_name=='Deaths' | OWL_stats_2023$stat_name=='Games Won' | OWL_stats_2023$stat_name=='Time Played' | OWL_stats_2023$stat_name=='Games Played')) 
Lucio <- aggregate(amount ~ player_name + stat_name, Lucio, sum)
library(tidyr)

Lucio <- Lucio %>%
  spread(stat_name, amount)

Lucio[is.na(Lucio)] <- 0
Lucio <- subset(Lucio,Lucio$`Time Played`>=300)
Lucio[, c(2,3,4,5,6,9)] <- Lucio[, c(2,3,4,5,6,9)] / Lucio[, 10]
Lucio[,8] <- Lucio[,8]/Lucio[,7]
# removing 
Lucio <- Lucio[,-c(7)]

z <- scale(Lucio[,c(2,3,4,6,7,8)])
score <- apply(z, 1, mean)
Lucio <- cbind(Lucio, score)
Lucio <- data.frame(lapply(Lucio, function(x) 
  if(is.numeric(x)) round(x, 5) else x))


library(psych)
corr.test(Lucio[,c(2:8,10)])

## Whos playing too much Lucio

# Fit the linear model
fit <- lm(score ~ Time.Played, data = Lucio)
summary(fit)
# Generate predictions for the line
Lucio$predicted <- predict(fit, Lucio)

# Create interactive scatterplot
library(plotly)
plot <- plot_ly(
  Lucio,
  x = ~Time.Played,
  y = ~score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Playing Too Much Lucio"
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


z2 <- scale(Lucio[,c(2,3,4,6,8)])
score2 <- apply(z2, 1, mean)
Lucio <- cbind(Lucio, score2)
Lucio <- data.frame(lapply(Lucio, function(x) 
  if(is.numeric(x)) round(x, 5) else x))
# Fit the linear model
fit2 <- lm(score2 ~ Games.Won, data = Lucio)
summary(fit2)
# Generate predictions for the line
Lucio$predicted2 <- predict(fit2, Lucio)

# Create interactive scatterplot
library(plotly)
plot2 <- plot_ly(
  Lucio,
  x = ~Games.Won,
  y = ~score2,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "Who's Lucio is being carried"
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













##Lucio Eliminations

Lucio_Eliminations <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Lucio' & (OWL_stats_2023$stat_name=="Eliminations"|OWL_stats_2023$stat_name=='Time Played'))

Lucio_Eliminations <- aggregate(amount ~ player_name + stat_name, Lucio_Eliminations, sum)

library(tidyr)

Lucio_Eliminations <- Lucio_Eliminations %>%
  spread(stat_name, amount)

Lucio_Eliminations[is.na(Lucio_Eliminations)] <- 0

# Fit the linear model
fit <- lm(Eliminations ~ `Time Played`, data = Lucio_Eliminations)
summary(fit)
# Generate predictions for the line
Lucio_Eliminations$predicted <- predict(fit, Lucio_Eliminations)

# Create interactive scatterplot
library(plotly)
plot<- plot_ly(
  Lucio_Eliminations,
  x = ~`Time Played`,
  y = ~Eliminations,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
)
# Add the regression line
plot <- add_trace(
  plot,
  x = ~`Time Played`,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the plot
plot

##Lucio Healing

Lucio_Healing <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Lucio' & (OWL_stats_2023$stat_name=="Healing Done"|OWL_stats_2023$stat_name=='Time Played'))

Lucio_Healing <- aggregate(amount ~ player_name + stat_name, Lucio_Healing, sum)

library(tidyr)

Lucio_Healing <- Lucio_Healing %>%
  spread(stat_name, amount)

Lucio_Healing[is.na(Lucio_Healing)] <- 0

# Fit the linear model
fit <- lm(`Healing Done`~ `Time Played`, data = Lucio_Healing)
summary(fit)
# Generate predictions for the line
Lucio_Healing$predicted <- predict(fit, Lucio_Healing)

# Create interactive scatterplot
library(plotly)
plot2<- plot_ly(
  Lucio_Healing,
  x = ~`Time Played`,
  y = ~`Healing Done`,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
)
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

plot(Lucio_Eliminations$Eliminations,Lucio_Healing$`Healing Done`)


##Lucio Eliminations

Lucio_Eliminations <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Lucio' & (OWL_stats_2023$stat_name=="Eliminations"|OWL_stats_2023$stat_name=='Time Played'))

Lucio_Eliminations <- aggregate(amount ~ player_name + stat_name, Lucio_Eliminations, sum)

library(tidyr)

Lucio_Eliminations <- Lucio_Eliminations %>%
  spread(stat_name, amount)

Lucio_Eliminations[is.na(Lucio_Eliminations)] <- 0

# Fit the linear model
fit <- lm(Eliminations ~ `Time Played`, data = Lucio_Eliminations)
summary(fit)
# Generate predictions for the line
Lucio_Eliminations$predicted <- predict(fit, Lucio_Eliminations)

# Create interactive scatterplot
library(plotly)
plot<- plot_ly(
  Lucio_Eliminations,
  x = ~`Time Played`,
  y = ~Eliminations,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
)
# Add the regression line
plot <- add_trace(
  plot,
  x = ~`Time Played`,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the plot
plot

##Lucio Healing

Lucio_Healing <- subset(OWL_stats_2023,OWL_stats_2023$hero_name=='Lucio' & (OWL_stats_2023$stat_name=="Healing Done"|OWL_stats_2023$stat_name=='Time Played'))

Lucio_Healing <- aggregate(amount ~ player_name + stat_name, Lucio_Healing, sum)

library(tidyr)

Lucio_Healing <- Lucio_Healing %>%
  spread(stat_name, amount)

Lucio_Healing[is.na(Lucio_Healing)] <- 0

# Fit the linear model
fit <- lm(`Healing Done`~ `Time Played`, data = Lucio_Healing)
summary(fit)
# Generate predictions for the line
Lucio_Healing$predicted <- predict(fit, Lucio_Healing)

# Create interactive scatterplot
library(plotly)
plot2<- plot_ly(
  Lucio_Healing,
  x = ~`Time Played`,
  y = ~`Healing Done`,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
)
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

plot(Lucio_Eliminations$Eliminations,Lucio_Healing$`Healing Done`)