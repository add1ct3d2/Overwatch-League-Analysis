# Damage time played
Damage_Time_Played <- subset(OWL_stats_2023, 
                              (OWL_stats_2023$hero_name %in% c('Ashe', 'Bastion', 'Cassidy', 'Echo', 'Genji', 
                                                               'Hanzo', 'Junkrat', 'Mei', 'Pharah', 'Reaper', 
                                                               'Sojourn', 'Soldier_76', 'Sombra', 'Symmetra', 
                                                               'Torbjorn', 'Tracer', 'Widowmaker')) &
                                (OWL_stats_2023$stat_name=='Time Played'))

Damage_Time_Played <- aggregate(amount ~ player_name + hero_name + stat_name, Damage_Time_Played, sum)
library(tidyr)
Damage_Time_Played <- Damage_Time_Played %>%
  spread(stat_name, amount)
Damage_Time_Played <- Damage_Time_Played %>%
  spread(hero_name, `Time Played`)

Damage_Time_Played[is.na(Damage_Time_Played)] <- 0
Damage_Time_Played <- data.frame(lapply(Damage_Time_Played, function(x) 
  if(is.numeric(x)) round(x, 2) else x))

# Note that we are summing over 17 heroes now instead of 7
Damage_Time_Played$Total <- rowSums(Damage_Time_Played[,2:17])

#Scores
all_players <- unique(c(Ashe$player_name, Bastion$player_name, Cassidy$player_name, Echo$player_name, Genji$player_name, 
                        Hanzo$player_name, Junkrat$player_name, Mei$player_name, Pharah$player_name, Reaper$player_name, 
                        Sojourn$player_name, Soldier_76$player_name, Sombra$player_name, Symmetra$player_name, 
                        Torbjorn$player_name, Tracer$player_name, Widowmaker$player_name))

players_df <- data.frame(player_name = all_players)

ashe_stats <- merge(players_df, Ashe[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
bastion_stats <- merge(players_df, Bastion[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
cassidy_stats <- merge(players_df, Cassidy[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
echo_stats <- merge(players_df, Echo[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
genji_stats <- merge(players_df, Genji[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
hanzo_stats <- merge(players_df, Hanzo[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
junkrat_stats <- merge(players_df, Junkrat[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
mei_stats <- merge(players_df, Mei[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
pharah_stats <- merge(players_df, Pharah[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
reaper_stats <- merge(players_df, Reaper[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
sojourn_stats <- merge(players_df, Sojourn[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
soldier_76_stats <- merge(players_df, Soldier_76[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
sombra_stats <- merge(players_df, Sombra[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
symmetra_stats <- merge(players_df, Symmetra[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
torbjorn_stats <- merge(players_df, Torbjorn[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
tracer_stats <- merge(players_df, Tracer[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
widowmaker_stats <- merge(players_df, Widowmaker[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)

# Start with players_df
Damage_Stats <- players_df

# Merge Ashe stats
Damage_Stats <- merge(Damage_Stats, Ashe[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Damage_Stats)[names(Damage_Stats) %in% c("score", "Time.Played")] <- c("Ashe_Score", "Ashe_Time.Played")

# Merge Bastion stats
Damage_Stats <- merge(Damage_Stats, Bastion[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Damage_Stats)[names(Damage_Stats) %in% c("score", "Time.Played")] <- c("Bastion_Score", "Bastion_Time.Played")

# Merge Cassidy stats
Damage_Stats <- merge(Damage_Stats, Cassidy[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Damage_Stats)[names(Damage_Stats) %in% c("score", "Time.Played")] <- c("Cassidy_Score", "Cassidy_Time.Played")

# Merge Echo stats
Damage_Stats <- merge(Damage_Stats, Echo[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Damage_Stats)[names(Damage_Stats) %in% c("score", "Time.Played")] <- c("Echo_Score", "Echo_Time.Played")

# Merge Genji stats
Damage_Stats <- merge(Damage_Stats, Genji[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Damage_Stats)[names(Damage_Stats) %in% c("score", "Time.Played")] <- c("Genji_Score", "Genji_Time.Played")

# Merge Hanzo stats
Damage_Stats <- merge(Damage_Stats, Hanzo[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Damage_Stats)[names(Damage_Stats) %in% c("score", "Time.Played")] <- c("Hanzo_Score", "Hanzo_Time.Played")

# Merge Junkrat stats
Damage_Stats <- merge(Damage_Stats, Junkrat[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Damage_Stats)[names(Damage_Stats) %in% c("score", "Time.Played")] <- c("Junkrat_Score", "Junkrat_Time.Played")

# Merge Mei stats
Damage_Stats <- merge(Damage_Stats, Mei[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Damage_Stats)[names(Damage_Stats) %in% c("score", "Time.Played")] <- c("Mei_Score", "Mei_Time.Played")

# Merge Pharah stats
Damage_Stats <- merge(Damage_Stats, Pharah[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Damage_Stats)[names(Damage_Stats) %in% c("score", "Time.Played")] <- c("Pharah_Score", "Pharah_Time.Played")

# Merge Reaper stats
Damage_Stats <- merge(Damage_Stats, Reaper[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Damage_Stats)[names(Damage_Stats) %in% c("score", "Time.Played")] <- c("Reaper_Score", "Reaper_Time.Played")

# Merge Soldier76 stats
Damage_Stats <- merge(Damage_Stats, Soldier_76[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Damage_Stats)[names(Damage_Stats) %in% c("score", "Time.Played")] <- c("Soldier76_Score", "Soldier76_Time.Played")

# Merge Sombra stats
Damage_Stats <- merge(Damage_Stats, Sombra[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Damage_Stats)[names(Damage_Stats) %in% c("score", "Time.Played")] <- c("Sombra_Score", "Sombra_Time.Played")

# Merge Symmetra stats
Damage_Stats <- merge(Damage_Stats, Symmetra[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Damage_Stats)[names(Damage_Stats) %in% c("score", "Time.Played")] <- c("Symmetra_Score", "Symmetra_Time.Played")

# Merge Torbjorn stats
Damage_Stats <- merge(Damage_Stats, Torbjorn[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Damage_Stats)[names(Damage_Stats) %in% c("score", "Time.Played")] <- c("Torbjorn_Score", "Torbjorn_Time.Played")

# Merge Tracer stats
Damage_Stats <- merge(Damage_Stats, Tracer[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Damage_Stats)[names(Damage_Stats) %in% c("score", "Time.Played")] <- c("Tracer_Score", "Tracer_Time.Played")

# Merge Widowmaker stats
Damage_Stats <- merge(Damage_Stats, Widowmaker[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Damage_Stats)[names(Damage_Stats) %in% c("score", "Time.Played")] <- c("Widowmaker_Score", "Widowmaker_Time.Played")

# View the final data frame
View(Damage_Stats)

# After merging all, Add total score
Damage_Stats$Total_Score <- rowSums(Damage_Stats[grep("_Score", names(Damage_Stats))], na.rm = TRUE)

# Add total time played
Damage_Stats$Total_Time_Played <- rowSums(Damage_Stats[grep("_Time.Played", names(Damage_Stats))], na.rm = TRUE)

# Multiply each hero's score by time played and then sum up to create a new column for the weighted sum of hero scores
Damage_Stats$Weighted_Sum_Score <- rowSums(
  Damage_Stats[grep("_Score", names(Damage_Stats))] *
    Damage_Stats[grep("_Time.Played", names(Damage_Stats))], 
  na.rm = TRUE
)

Damage_Stats$True_Score <- Damage_Stats$Weighted_Sum_Score / Damage_Stats$Total_Time_Played

plot(Damage_Stats$Total_Time_Played, Damage_Stats$True_Score)

# Fit the linear model
fit <- lm(True_Score ~ Total_Time_Played, data = Damage_Stats)
summary(fit)
# Generate predictions for the line
Damage_Stats$predicted <- predict(fit, Damage_Stats)

# Create interactive scatterplot
library(plotly)
plot <- plot_ly(
  Damage_Stats,
  x = ~Total_Time_Played,
  y = ~True_Score,
  type = "scatter",
  mode = "markers",
  text = ~paste("Player:", player_name),
  hoverinfo = "text"
) %>%
  layout(
    title = "True Score v Time Played"
  )
# Add the regression line
plot <- add_trace(
  plot,
  x = ~Total_Time_Played,
  y = ~predicted,
  type = "scatter",
  mode = "lines",
  line = list(color = 'red')
)

# Show the plot
plot
