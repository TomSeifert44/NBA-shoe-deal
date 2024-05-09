load("Desktop/UCLA/BSA/nbafollowers.R")
head(followers)

library(ggplot2)
library(dplyr)

save(followers, file = "Desktop/UCLA/BSA/nbafollowers.R")

with(followers, boxplot(insta_followers ~ shoe_deal, ylim = c(0, max(insta_followers))))

##############################

pdf("nbafollowers.pdf")

color_vec <- c("grey", "dodgerblue")
with(followers[1:75,], barplot(insta_followers/1000 ~ reorder(player_name, -insta_followers), 
                        ylim = c(0, max(insta_followers/1000)),
                        col = color_vec[shoe_deal + 1], xlab = "", ylab = "",
                        cex.names = 0.45, las= 2, yaxt = "n", xaxt = "n",
                        main = "Instagram Followers by Player"))
legend("topright",legend = c("Shoe Deal", "No Shoe Deal"), fill = c(rev(color_vec)), lty = c(2, 2), col = rev(color_vec) )
title(ylab = "Instagram Followers (Thousands)", line = 2)

axis(side = 2,line = -0.5)
axis(side = 1, labels = followers$player_name[1:75], at = seq(0.75, 89.5, length = 75), line = .01, cex.axis = 0.4, las = 2)

text(50, 100000, "Not Shown: Kevin Durant, Derrick Rose")

abline(h = mean(followers[followers$shoe_deal, ]$insta_followers)/1000, lty = 2, col = "dodgerblue")
abline(h = mean(followers[!(followers$shoe_deal), ]$insta_followers)/1000, lty = 2, col = "grey")

mean(followers[followers$shoe_deal, ]$insta_followers) - mean(followers[!(followers$shoe_deal), ]$insta_followers)

dev.off()

###############################

with(followers[followers$shoe_deal, ], boxplot(Instagram_Followers ~ Brand, ylim = c(0, 50000000)))

with(followers, tapply(Instagram_Followers, Brand, mean))

followers[followers$shoe_deal, ]

mean(followers[!(followers$shoe_deal), ]$insta_followers)

names(followers)

save(followers, file = "Desktop/UCLA/BSA/nbafollowers.R")

setwd("/Desktop/UCLA/BSA")
load("/Desktop/UCLA/BSA/nbashoescareerstats.R")

nrow(followers[followers$shoe_deal, ])
followers[followers$player_name == "LaMelo Ball",]$shoe_deal <- TRUE

plot(followers)

with(followers, table(player_name))

library(rvest)
library(tidyverse)

############

markets <- read_html("https://www.sportsmediawatch.com/nba-market-size-nfl-mlb-nhl-nielsen-ratings/")

markets <- markets %>% html_node("#olytable") %>% html_table()

head(markets)

markets <- rbind(markets[1:2, ], markets)

markets <- markets %>% arrange(Market)
markets <- markets[markets$NBA != "no team", ]

print(markets, n = 30)

markets$NBA <- c("ATL","BOS", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "HOU", "IND","LAC","LAL","MEM","MIA","MIL","MIN","NOP","NYK","BKN","OKC","ORL","PHI","PHX","POR","SAC","UTA","SAS","GSW","WAS")

load("/Desktop/UCLA/BSA/nbafollowers.R")

# Add LaMelo

library(rvest)

lamelo <- read_html("https://www.basketball-reference.com/players/b/ballla01.html")

lamelo <- lamelo %>% html_node("#per_game") %>% html_table()

lamelo <- with(lamelo, lamelo[str_detect(Season, "^2[0-9]{3}-[0-9]{2}"), ])

lamelo$Season <- vapply(lamelo$Season, substr, character(1), start = 1, stop = 4)

lamelo$Season <- as.numeric(lamelo$Season)

lamelo$name <- "LaMelo Ball"

lamelo <- lamelo %>% select(name, names(lamelo)[-31])

names(org_full_df) <- names(lamelo)

org_full_df <- rbind(org_full_df, lamelo)

write_csv(org_full_df, "Desktop/UCLA/BSA/nbacareerstats.csv")

######

tail(org_full_df)

full_followers$shoe_deal <- TRUE

org_full_df[org_full_df$Tm == "CHA", ]

unique(org_full_df$Tm)
markets



org_full_df <- read.csv("Desktop/UCLA/BSA/nbacareerstats.csv")
full_followers <- left_join(org_full_df, followers, by = "name")


full_all <- with(full_followers, full_join(full_followers[Season == year_of_deal, ], markets, by = "Tm"))

full_all <- full_all %>% select(Tm, names(full_all))
full_all

library(ggplot2)
library(tidyverse)

names(full_all)

full_all$Tm

markets <- markets %>% arrange(Tm)

markets <- read_csv("Desktop/UCLA/BSA/nbamarkets.csv")

markets[markets$Tm == "LAL",]$team_shoe <- TRUE

#######

jpeg("nbamarkets")

with(markets, barplot(homes_reached ~ Tm, 
                      col = ifelse(team_shoe, "dodgerblue", "gray")
                      , las = 3, xlab = "Team",
                      ylab = "Homes Reached (Thousands)",
                      ylim = (c(0, 8000)),
                      main = "Market Size for Each NBA City"))
with(markets, abline(lty = 2, h = mean(homes_reached[team_shoe]), col = "dodgerblue"))
with(markets, abline(lty = 2, h = mean(homes_reached[!team_shoe]), col = "gray"))
with(markets, legend(5,8000,legend = c("Shoe Deal", "No Shoe Deal"), 
                     fill = c("dodgerblue", "gray"), lty = c(2, 2), col = c("dodgerblue", "gray")))

dev.off()

#####

markets$Tm %in% unique(full_followers[full_followers$year_of_deal == full_followers$Season,]$Tm)

markets$team_shoe <- markets$Tm %in% unique(full_followers[full_followers$year_of_deal == full_followers$Season,]$Tm)

unique(full_followers[full_followers$year_of_deal == full_followers$Season,]$Tm)

reorder(Tm, team_shoe)

markets$Tm 

full_followers[full_followers$year_of_deal == full_followers$Season,]$Tm


###########


#########
 # All Star Scraping

allstar_years <- as.character(2016:2022)
allstar_years

as_roster_df <- data.frame()

for (year in allstar_years) {
  roster <- read_html(paste("https://www.basketball-reference.com/allstar/NBA_", year,".html", sep = ""))
  roster <- roster %>% html_nodes(".stats_table") %>% html_table()
  roster <- rbind(roster[[2]], roster[[3]])
  as_roster_df <- rbind(as_roster_df, roster)
}

allstar23 <- read_html("https://www.basketball-reference.com/allstar/NBA_2023.html")
allstar23 <- allstar23 %>% html_nodes(".stats_table")  %>% html_table()
allstar23 <- rbind(allstar23[[1]], allstar23[[2]])
allstar23

team <- read_html("https://www.basketball-reference.com/allstar/NBA_2018.html")
team <- team %>% html_nodes("h2") %>% html_text()
team

as_names <- unique(as_roster_df[[1]])
as_names <- as_names[!(as_names %in% c("Starters", "", "Reserves", "Team Totals"))]
as_names

full_stats <- read.csv("Desktop/UCLA/BSA/nbacareerstats.csv")
load("Desktop/UCLA/BSA/nbafollowers.R")

library(tidyverse)

names(followers)[2] <- "name"

stats_followers <- left_join(full_stats, followers, by = "name")
unique(stats_followers$name)
as_names <- as_names[!(as_names %in% unique(stats_followers$name))]

as_names

split_names <- str_split(as_names, " ")

url_name2 <- vapply(split_names, function(name) {
  first <- substr(name[1], start = 1, stop = 2)
  last <- substr(name[2], start = 1, stop = 5)
  tolower(paste(last, first, sep = ""))
}, character(1))

url_name2 <- chartr("č", "c", url_name2)
url_name2 <- chartr("ć", "c", url_name2)
url_name2

as_full_df <- data.frame()

for (i in (31:length(url_name2))) {
  Sys.sleep(15)
  career_stats <- read_html(paste("https://www.basketball-reference.com/players/", substr(url_name2[i], start = 1, stop = 1), "/", url_name2[i],"01.html", sep = ""))
  as_full_df <- rbind(as_full_df, career_stats %>% html_node("#per_game") %>% html_table())
  message(i)
}

as_full_df[as_full_df$Season == "1996-97",]
seasons_as_full_df <- unique(as_full_df)
print(u_as_full_df[c(TRUE, diff(u_as_full_df$Age) < 0), ], n = 50)
u_as_full_df$Season
diff(u_as_full_df$Age)

seasons_as_full_df <- with(seasons_as_full_df, seasons_as_full_df[str_detect(Season, "^[1-2][0-9]{3}-[0-9]{2}"), ])

u_as_full_df

as_names <- as_names[c(-1, -2, -3)]

plyr_indx <- with(seasons_as_full_df, which(diff(Age) < 0))


seasons_as_full_df$name <- rep(as_names, diff(c(0, plyr_indx, nrow(seasons_as_full_df))))

u_as_full_df

seasons_as_full_df <- seasons_as_full_df %>% select(name, names(seasons_as_full_df))

#add AD

ad <- read_html("https://www.basketball-reference.com/players/d/davisan02.html")
ad <- ad %>% html_node("#per_game") %>% html_table()

ad <- ad[1:11,]
ad

u_as_full_df[u_as_full_df$Season == "1990-91",]

kemba <- read_html("https://www.basketball-reference.com/players/w/walkeke02.html")
kemba <- kemba %>% html_node("#per_game") %>% html_table()

it <- read_html("https://www.basketball-reference.com/players/t/thomais02.html")
it <- it %>% html_node("#per_game") %>% html_table()

it <- it[1:18,]
kemba <- kemba[1:12,]

u_as_full_df <- u_as_full_df[!(u_as_full_df$name %in% c("Anthony Davis", "Kemba Walker", "Isaiah Thomas")),]

print(u_as_full_df[c(TRUE, diff(u_as_full_df$Age) < 0), ], n = 50)

kemba$name <- "Kemba Walker"
ad$name <- "Anthony Davis"
it$name <- "Isaiah Thomas"


seasons_as_full_df <- rbind(ad, kemba, it, seasons_as_full_df)

print(u_as_full_df, n = 50)

as_names <- c("Anthony Davis", "Kemba Walker", "Isaiah Thomas", as_names[c(-13, -4, -22)])
as_names

no_shoe_as <- followers[followers$name %in% u_as_full_df$name,]
no_shoe_as
followers[followers$name %in% u_as_full_df$name,][35,]$insta_followers <- NA

as_stats_followers <- left_join(seasons_as_full_df, no_shoe_as, by = "name")

seasons_as_full_df$Season <- vapply(seasons_as_full_df$Season, substr, character(1), start = 1, stop = 4)

seasons_as_full_df$Season <- as.numeric(seasons_as_full_df$Season)

seasons_as_full_df <- seasons_as_full_df[-(c(45:64, 201:213, 346:354)),]
seasons_as_full_df$Season

as_stats_followers <- as_stats_followers %>% select(name, names(as_stats_followers))
seasons_as_full_df$Season
test <- as_stats_followers
test <- as_stats_followers[,-c(1,4,5,6)]
as_stats_followers[,-c(1,4,5,6,32:36)] <- vapply(names(as_stats_followers)[-c(1, 4, 5, 6, 32:36) ], function(x) {
  as_stats_followers[[x]] <- as.numeric(as_stats_followers[[x]])
}, numeric(623))
names(stats_followers)

all_stats_followers <- rbind(as_stats_followers, stats_followers)

stats_followers$shoe_deal <- TRUE

# to do: add first year all stars from this year

allstar23_players <- allstar23$Player[c(-14, -29)]
first_year_as <- allstar23_players[!(allstar23_players %in% unique(all_stats_followers$name))]
first_year_as[6] <- "DeAaron Fox"
first_year_as

# https://www.basketball-reference.com/players/j/jacksja02.html
# https://www.basketball-reference.com/players/m/markkla01.html
# https://www.basketball-reference.com/players/f/foxde01.html
# https://www.basketball-reference.com/players/g/gilgesh01.html
# https://www.basketball-reference.com/players/h/holidjr01.html
# https://www.basketball-reference.com/players/e/edwaran01.html
# https://www.basketball-reference.com/players/h/halibty01.html

library(rvest)

jjj <- read_html("https://www.basketball-reference.com/players/j/jacksja02.html")
jjj <- jjj %>% html_node("#per_game") %>% html_table()

lauri <- read_html("https://www.basketball-reference.com/players/m/markkla01.html")
lauri <- lauri %>% html_node("#per_game") %>% html_table()

fox <- read_html("https://www.basketball-reference.com/players/f/foxde01.html")
fox <- fox %>% html_node("#per_game") %>% html_table()

shai <- read_html("https://www.basketball-reference.com/players/g/gilgesh01.html")
shai <- shai %>% html_node("#per_game") %>% html_table()

jrue <- read_html("https://www.basketball-reference.com/players/h/holidjr01.html")
jrue <- jrue %>% html_node("#per_game") %>% html_table()

ant <- read_html("https://www.basketball-reference.com/players/e/edwaran01.html")
ant <- ant %>% html_node("#per_game") %>% html_table()

tyrese <- read_html("https://www.basketball-reference.com/players/h/halibty01.html")
tyrese <- tyrese %>% html_node("#per_game") %>% html_table()


fox <- fox[1:6,]
tyrese <- tyrese[1:5,]
shai <- shai[1:5,]
ant <- ant[1:3,]
jrue <- jrue[1:14,]
lauri <- lauri[1:6,]
jjj <- jjj[1:5,]

new_as_followers <- followers[followers$name %in% first_year_as,]

save(first_year_as, file = "firstyear_as.R")

write_csv(all_stats_followers, file = "Desktop/UCLA/BSA/all_stats_followers.csv")

all_stats_followers <- read.csv("Desktop/UCLA/BSA/all_stats_followers.csv")

first_year_df <- rbind(lauri, shai, jrue, ant, tyrese,fox, jjj)

first_year_df$name <- c(rep(first_year_as, c(nrow(lauri),nrow(shai),nrow(jrue),nrow(ant),nrow(tyrese),nrow(fox),nrow(jjj))))

library(tidyverse)

first_year_df <- first_year_df %>% select(name, names(first_year_df))
nrow(first_year_df)

names(followers)[2] <- "name"

first_year_followers <- left_join(first_year_df, followers, by = "name")

first_year_df$Season <- vapply(first_year_df$Season, substr, character(1), start = 1, stop = 4)

first_year_df$Season <- as.numeric(first_year_df$Season)

load("Desktop/UCLA/BSA/nbafollowers.R")

all_stats_followers

names(all_stats_followers) <- names(first_year_followers)

as_2016_23 <- rbind(all_stats_followers, first_year_followers)

unique(as_2016_23$name)[!(unique(as_2016_23$name) %in% followers[followers$shoe_deal, ]$name)]

unique(as_2016_23[as_2016_23$shoe_deal, ]$name)

as_2016_23[as_2016_23$name == "LaMelo Ball",]$year_of_deal <- 2020

as_2016_23$name[799:833]

write_csv(as_2016_23, file = "Desktop/UCLA/BSA/nbaallstarstats.csv")

as_2016_23[as_2016_23$shoe_deal,] %>% select(brand, year_of_deal)

unique(as_2016_23$Tm)

as_2016_23$Tm <- test_as_2016_23$Tm

as_2016_23[as_2016_23$Tm == "CHO",]$Tm <- "CHA"
"NOH" <- "NOP"
"PHO" <- "PHX"
"NOK" <- "NOP"
"BRK" <- "BKN"

# TO DO:
  # Graph stats vs. time for players w/ shoes
    # indicate when shoe deal was received
    # which stats seem to matter?
    # experiment with time intervals - compare stats of all seasons before deal, season before and after deal
    # are they all above a certain threshold in the season before the deal, all seasons leading up to the deal, etc?
    # want to find any pattern to establish some sort of criteria
  # Logistic regression w/ all-star players and stats - find which stats are most indicative
  # Use markets/followers/age/stat trends/thresholds/results of regression to predict!!

as_stats <- read.csv("Desktop/UCLA/BSA/nbaallstarstats.csv")

######## Season/age frequency bar plot

png("nbaseasonnum.png")

as_stats_season_num <- as_stats[as_stats$shoe_deal & as_stats$Season == as_stats$year_of_deal,]


with(as_stats_season_num, plot(season_num ~ Season, pch = 16, 
                               ylab = "Years of Experience",
                               col = rainbow(length(name)),
                               main = "Years of Experience Before Signing Shoe Deal"))
with(as_stats_season_num, legend(2005.5, 8.28, legend = name, fill = rainbow(length(name)), cex = 0.5))

mean(as_stats_season_num$season_num, na.rm = TRUE)

dev.off()

with(as_stats_season_num, text(season_num ~ Season, data = as_stats_season_num, name))
########

plot
as_stats_season_num <- rbindas_stats_season_num

as_stats[as_stats$name == "Al Horford",]$season_num

seq_len(table(as_stats$name)[["Al Horford"]])

as_stats[c("Season","season_num","name")]

for (nm in names(table(as_stats$name))) {
  as_stats[as_stats$name == nm,]$season_num <- seq_len(table(as_stats$name)[[nm]])
}
  
library(tidyverse)

write_csv(as_stats, file = "Desktop/UCLA/BSA/nbaallstarstats.csv")

as_stats <- read.csv("Desktop/UCLA/BSA/nbaallstarstats.csv")

as_stats_shoes <- as_stats[as_stats$shoe_deal,]

unique(as_stats_shoes$name)

library(ggplot2)

as_stats$name <- as.factor(as_stats$name)

plot(PTS ~ Season, data = as_stats_shoes, col = name, pch = 16)
#with(as_stats_shoes, legend("topleft", levels(name), col = 1:nlevels(name), pch = 16, cex = 0.6))
with(as_stats_shoes, for (i in seq_along(levels(name))) {
  lines(PTS ~ Season, data = as_stats_shoes[name == levels(name)[i], ], col = i)
  points(PTS ~ year_of_deal, data = as_stats_shoes[name == levels(name)[i] & Season == year_of_deal, ], 
         col = "goldenrod", pch = 21, cex = 2.2, bg = i)
  message(i)
})

plot(dif ~ Season, data = difs[difs$Season == as_stats_shoes$year_of_deal,], 
     col = name, pch = 16, cex = 1.5)

plot(PTS ~ Season, data = as_stats_shoes[difs$Season == as_stats_shoes$year_of_deal,], 
     col = name, pch = 16, cex = 1.5)


######### Stats by year graph

jpeg("Desktop/UCLA/BSA/nba3PM.jpeg")

plot(`FGA` ~ Season, data = as_stats_shoes[as_stats_shoes$Season == as_stats_shoes$year_of_deal,], 
       col = rainbow(length(name)), pch = 16, cex = 1.5, main = "FGA in Season Receiving Shoe Deal", ylab = "FGA")
with(as_stats, abline(h = mean(as_stats$`FGA`, na.rm = TRUE)))

lm_shoes <- lm(`FGA` ~ Season, data = as_stats_shoes[as_stats_shoes$Season == as_stats_shoes$year_of_deal,])

with(as_stats, abline(lm_shoes, col = "blue"))
with(as_stats, legend(2004.5, 33.5, legend = c(unique(levels(name[shoe_deal])[name[shoe_deal]]), "All-Star Avg."), cex = 0.42,
                      col = c(rainbow(length(unique(name[shoe_deal]))), "black"), pch = c(rep(16, 20), NA), lty = c(rep(NA, 20), 1)))
mean(as_stats$`FGA`, na.rm = TRUE)
mean(as_stats_shoes[as_stats_shoes$Season == as_stats_shoes$year_of_deal,]$`FGA`, na.rm = TRUE)


summary(lm_shoes)

dev.off()

#########

with(as_stats_shoes, for (i in seq_along(levels(name))) {
  lines(`X3P.` ~ Season, data = as_stats_shoes[name == levels(name)[i], ], col = i)
})

with(as_stats_season_num, hist(Season, breaks = 18))

as_stat_season_num <- as_stats_season_num %>% arrange(Season)
as_stat_season_num$group <- rep(1:5, c(4, 5, 4, 4, 3))

as_stats

hist
######### 

# TO USE: PPG, FGA, eFG%, 3PM

# Things that correlate: PTS (15+, 20+), MP (30+), FGA (15+), FG (7+), NOT FG%, 3P%, FTM (4+), FTA (4+), NOT FT%, NOT ORB/DRB/TRB, AST (4+), STL (1+), NOT BLK, TOV (2.5+)
# Things that have changed overtime: eFG%, 3PM, 3PA, 3P%, DRB/TRB


stat_plot("PTS")

plot(`FG` ~ Season, data = as_stats_shoes[difs$Season == as_stats_shoes$year_of_deal,], 
     col = name, pch = 16, cex = 1.5)
with(as_stats, abline(h = mean(as_stats[!shoe_deal,]$FG, na.rm = TRUE)))

with(as_stats, mean(as_stats[!shoe_deal,]$eFG.))

points(`eFG.` ~ Season, data = as_stats %>% group_by(name) %>% filter(`eFG.` == max(`eFG.`, na.rm = TRUE), !shoe_deal),
        col = "goldenrod", pch = 10, cex = 2.2)


with(as_stats, vapply(as_stats, max, numeric(1), na.rm = TRUE))

with(as_stats[!is.na(as_stats$`eFG.`),], as_stats[`eFG.`== max(`eFG.`, na.rm = TRUE), ])

as_stats_shoes$PTS

library(tidyverse)

difs <- as_stats_shoes %>% group_by(name) %>% mutate(dif = c(0, diff(PTS))) %>% select(name, Season, dif, PTS, year_of_deal)

print(difs, n = 25)

table(with(as_stats_shoes, as_stats_shoes[Season == year_of_deal,])$Tm)

unique(as_stats$name)

#### Log Regression

library(aod)

#  + insta_followers + STL + TOV + BLK + AST + TRB + FT + eFG. + X3P + FGA + FG + MP + GS + G, 


as_stats_noNA <- as_stats
as_stats_noNA <- as_stats[!is.na(as_stats$PTS),]

logit_nba <- glm(shoe_deal ~ insta_followers + PTS, 
                 data = as_stats_noNA, family = "binomial")
summary(logit_nba)

newdata <- data.frame(insta_followers = seq(min(as_stats$insta_followers, na.rm = TRUE), max(as_stats$insta_followers, na.rm = TRUE), len = 500))

newdata$shoe_deal <- predict(logit_nba, newdata, type = "response")

plot(shoe_deal ~ insta_followers, data = as_stats)

lines(shoe_deal ~ insta_followers, newdata, lwd = 2)

