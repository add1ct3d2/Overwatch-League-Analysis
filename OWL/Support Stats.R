## support time played

Support_Time_Played <- subset(OWL_stats_2023,(OWL_stats_2023$hero_name=='Ana'|OWL_stats_2023$hero_name=='Baptiste'|OWL_stats_2023$hero_name=='Brigitte'|OWL_stats_2023$hero_name=='Lucio'|OWL_stats_2023$hero_name=='Mercy'|OWL_stats_2023$hero_name=='Kiriko'|OWL_stats_2023$hero_name=='Zenyatta') & (OWL_stats_2023$stat_name=='Time Played'))
Support_Time_Played <- aggregate(amount ~ player_name + hero_name + stat_name, Support_Time_Played, sum)
library(tidyr)
Support_Time_Played <- Support_Time_Played %>%
  spread(stat_name, amount)
Support_Time_Played <- Support_Time_Played %>%
  spread(hero_name, `Time Played`)

Support_Time_Played[is.na(Support_Time_Played)] <- 0
Support_Time_Played <- data.frame(lapply(Support_Time_Played, function(x) 
  if(is.numeric(x)) round(x, 2) else x))

Support_Time_Played$Total <- rowSums(Support_Time_Played[,2:8])

#Scores

all_players <- unique(c(Ana$player_name, Baptiste$player_name, Brigitte$player_name, Mercy$player_name, Zenyatta$player_name, Kiriko$player_name, Lucio$player_name))

players_df <- data.frame(player_name = all_players)

ana_stats <- merge(players_df, Ana[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
baptiste_stats <- merge(players_df, Baptiste[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
brigitte_stats <- merge(players_df, Brigitte[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
mercy_stats <- merge(players_df, Mercy[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
zenyatta_stats <- merge(players_df, Zenyatta[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
kiriko_stats <- merge(players_df, Kiriko[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)
lucio_stats <- merge(players_df, Lucio[, c("player_name", "score", "Time.Played")], by = "player_name", all.x = TRUE)

# Start with players_df
Support_Stats <- players_df

# Merge Ana stats
Support_Stats <- merge(Support_Stats, Ana[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Support_Stats)[names(Support_Stats) %in% c("score", "Time.Played")] <- c("Ana_Score", "Ana_Time.Played")

# Merge Baptiste stats
Support_Stats <- merge(Support_Stats, Baptiste[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Support_Stats)[names(Support_Stats) %in% c("score", "Time.Played")] <- c("Baptiste_Score", "Baptiste_Time.Played")

# Merge Brigitte stats
Support_Stats <- merge(Support_Stats, Brigitte[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Support_Stats)[names(Support_Stats) %in% c("score", "Time.Played")] <- c("Brigitte_Score", "Brigitte_Time.Played")

# Merge Mercy stats
Support_Stats <- merge(Support_Stats, Mercy[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Support_Stats)[names(Support_Stats) %in% c("score", "Time.Played")] <- c("Mercy_Score", "Mercy_Time.Played")

# Merge Zenyatta stats
Support_Stats <- merge(Support_Stats, Zenyatta[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Support_Stats)[names(Support_Stats) %in% c("score", "Time.Played")] <- c("Zenyatta_Score", "Zenyatta_Time.Played")

# Merge Kiriko stats
Support_Stats <- merge(Support_Stats, Kiriko[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Support_Stats)[names(Support_Stats) %in% c("score", "Time.Played")] <- c("Kiriko_Score", "Kiriko_Time.Played")

#Merge Lucio stats
Support_Stats <- merge(Support_Stats, Lucio[, c("player_name", "score", "Time.Played")], by = "player_name", all = TRUE)
names(Support_Stats)[names(Support_Stats) %in% c("score", "Time.Played")] <- c("Lucio_Score", "Lucio_Time.Played")

# View the final data frame
View(Support_Stats)

# Add total score
Support_Stats$Total_Score <- rowSums(Support_Stats[c("Ana_Score", "Baptiste_Score", "Brigitte_Score", "Mercy_Score", "Zenyatta_Score", "Kiriko_Score", 'Lucio_Score')], na.rm = TRUE)

# Add total time played
Support_Stats$Total_Time_Played <- rowSums(Support_Stats[c("Ana_Time.Played", "Baptiste_Time.Played", "Brigitte_Time.Played", "Mercy_Time.Played", "Zenyatta_Time.Played", "Kiriko_Time.Played", "Lucio_Time.Played")], na.rm = TRUE)

# Multiply each hero's score by time played and then sum up to create a new column for the weighted sum of hero scores
Support_Stats$Weighted_Sum_Score <- rowSums(
  Support_Stats[, c("Ana_Score", "Baptiste_Score", "Brigitte_Score", "Mercy_Score", "Zenyatta_Score", "Kiriko_Score", "Lucio_Score")] *
    Support_Stats[, c("Ana_Time.Played", "Baptiste_Time.Played", "Brigitte_Time.Played", "Mercy_Time.Played", "Zenyatta_Time.Played", "Kiriko_Time.Played", "Lucio_Time.Played")], 
  na.rm = TRUE
)

Support_Stats$True_Score <- Support_Stats$Weighted_Sum_Score / Support_Stats$Total_Time_Played