#
# Author: Jeet Ganatra
# Purpose: Homework 5
#

fpl <- read.csv(file.choose(), sep = ",", header = T, stringsAsFactors = F)
str(fpl)
View(fpl)

#Barplot to display the total number of players present in each team
par(bty = "n")
fpl <- data.frame(fpl)

#Barplot to display the the sum of goals scored according to each position
col <- c("chartreuse", "chartreuse2", "chartreuse3", "chartreuse4")
position_points <- tapply(fpl$Points, list(fpl$Position), sum)
position_points
text(x = barplot(position_points, col = col,
     main = "Total points for each position", xlab = "Positions", ylab = "Goal count"),
     y = position_points, labels = position_points, pos = 3, cex = 0.7, col = "white", xpd = T)
legend("topleft", legend = rownames(position_points), bty = "n", pch = 15, ncol = 4,
       col = col)

#Barplot to display the sum of player costs according to each position
position_type_cost <- tapply(fpl$Cost, list(fpl$Position), sum)
text(x = barplot(position_type_cost, col = c("skyblue3", "springgreen3", "tomato3", "goldenrod1"),
                 main = "Total cost of each position", xlab = "Positions", ylab = "Total cost"),
     y = position_type_cost, labels = position_type_cost, pos = 3, cex = 0.7, col = "sienna3", xpd = T)
legend("topleft", legend = rownames(position_type_cost), bty = "n", pch = 15, ncol = 4,
       col = c("skyblue3", "springgreen3", "tomato3", "goldenrod1"))

#Density plot to display the distribution of player costs
d <- (density(fpl$Points))
plot(d, main = "Density distribution of Total cost of players", 
     xlab = "Total cost", ylab = "Frequency")
polygon(d, col = "steelblue3")

#Boxplot to display the numerical distribution of points recorded by all players
boxplot(fpl$Points, main = "Distribution of points recorded by all players", col = "steelblue3", 
        xlab = "Numerical distribution of Points", ylab = "Frequency")

par(mar=c(8, 4.1, 4.1, 2.1))
team_subset <- fpl[fpl$Team == "MCI",]
team_subset

MCI_goal_scorers <- tapply(team_subset$Minutes, list(team_subset$Team, team_subset$Name), sum)
MCI_goal_scorers

#Barplot to display the number of minutes played by each player for Manchester City team
text(x = barplot(MCI_goal_scorers,
                 beside = T, main = "Minutes played by each player for Manchester City", xlab = "",
                 ylab = "Minutes played", las = 2), y = MCI_goal_scorers, label = MCI_goal_scorers, xpd = T,
     pos = 3, cex = 0.7)
mtext(text = "Player names", side = 1, line = 6.5)

#Scatterplot showing the distribution of Points vs Cost for all playing positions
fpl %>%
  filter(Points != 0) %>%
  group_by(Points, Position, Cost) %>%
  summarise(NumPlayers = n()) %>%
  ungroup() %>%
  ggplot(aes(Cost, Points)) +
  geom_point(aes(size = NumPlayers, color = Position), position = "jitter") +
  ggtitle("Points vs Cost Scatterplot with Positions")

#Scatterplot showing the distribution of Points vs Cost for all playing positions with grid lines
fpl %>%
  filter(Points != 0) %>%
  group_by(Points, ICT, Cost) %>%
  summarise(NumPlayers = n()) %>%
  ungroup() %>%
  ggplot(aes(Cost, Points)) +
  geom_point(aes(size = NumPlayers, color = ICT), position = "jitter") +
  geom_hline(yintercept = c(100, 75, 50, 25)) +
  geom_vline(xintercept = c(55,70)) +
  ggtitle("Points vs Cost Scatterplot with Grid Lines")

#Scatterplot showing the distribution of Points vs Minutes Played
fpl %>%
  filter(Points != 0) %>%
  ggplot(aes(Minutes, Points)) +
  geom_point(aes(color = Position), position = "jitter") +
  geom_smooth(method = lm, se = FALSE) +
  xlim(0, 1200)
  ggtitle("Points vs Minutes Played Scatterplot")
  
str(fpl)

#Tremap showing the Players scoring more than 40 points for their teams in their respective positions
fpl %>%
  filter(Points > 40) %>%
  treemap(fpl,
          index = c("Position", "Team", "Name"),
          vSize = "Points",
          type = "index",
          palette = "Reds",
          title = "Points in the premier league",
          fontsize.title = 14)
