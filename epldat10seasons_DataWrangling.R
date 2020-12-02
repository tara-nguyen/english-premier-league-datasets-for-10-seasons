## Data wrangling of English Premier League data from Season 2010/11 to Season 2019/20
##
## AUTHOR: TARA NGUYEN
## Completed in November 2020

########## DATA IMPORT AND CLEANING ##########

n_teams_per_season <- 20
n_games_per_season <- n_teams_per_season * (n_teams_per_season - 1)

season_years <- seq(1011, 1920, 101)
filepaths <- paste0('https://www.football-data.co.uk/mmz4281/', 
	season_years, '/E0.csv')

epldat <- data.frame()   ## empty data frame
colnames <- c('Date', 'Referee', 'HomeTeam', 'AwayTeam', 'FTR', 'HTR',
	'FTHG', 'HTHG', 'HS', 'HST', 'HC', 'HF', 'HY', 'HR',
	'FTAG', 'HTAG', 'AS', 'AST', 'AC', 'AF', 'AY', 'AR')

for (filepath in filepaths) {
	epldat_temp <- readr::read_csv(filepath)
	epldat_temp <- epldat_temp[1:n_games_per_season, colnames]
	epldat <- rbind(epldat, epldat_temp)
}
summary(epldat)

## rename columns

ncol <- ncol(epldat)
colnames(epldat)[5:ncol] <- c('FullTime', 'Halftime', 'HomeGoals',
	'HomeGoalsHalftime', 'HomeShots', 'HomeShotsOnTarget', 'HomeCorners',
	'HomeFouls', 'HomeYellowCards', 'HomeRedCards', 'AwayGoals',
	'AwayGoalsHalftime', 'AwayShots', 'AwayShotsOnTarget', 'AwayCorners',
	'AwayFouls', 'AwayYellowCards', 'AwayRedCards')
colnames(epldat)
	
## turn dates into the form yyyy-mm-dd

newDate <- gsub('/20([^$])','/\\1', as.character(epldat$Date))
epldat$Date <- as.Date(newDate, '%d/%m/%y')
str(epldat$Date)

## change factor labels of FullTime and of Halftime

epldat$FullTime <- factor(epldat$FullTime, levels = c('H', 'D', 'A'),
	labels = c('HomeWin', 'Draw', 'AwayWin'))
str(epldat$FullTime)
epldat$Halftime <- factor(epldat$Halftime, levels = c('H', 'D', 'A'),
	labels = c('HomeWin', 'Draw', 'AwayWin'))
str(epldat$Halftime)

########## DATA TRANSFORMATION ##########

## add Season column

epldat$Season <- rep(paste0('20', substr(season_years, 1, 2), '/',
	substr(season_years, 3, 4)), each = n_games_per_season)
seasons <- unique(epldat$Season)

## rearrange columns - all numeric factors at the end

ncol <- ncol(epldat)
str(epldat)
names(epldat)
epldat <- subset(epldat, select = c(Season, Date:AwayRedCards))
colnames <- names(epldat)
colnames

## function for saving new datasets to csv files

save_to_csv <- function(data, rownames = F, filename) {
	write.csv(data, row.names = rownames,
		file = paste0('epldat10seasons/', filename, '.csv'))
}

save_to_csv(epldat, filename = 'epl-allseasons-matchstats')
		
##### CREATE NEW DATASETS BASED ON epldat #####

### SEASON-END LEAGUE TABLES ###

## contigency tables

teams_byseason <- table(epldat$HomeTeam, epldat$Season)

results_byhometeam_byseason <- xtabs(~ HomeTeam + FullTime + Season, epldat)
results_byawayteam_byseason <- xtabs(~ AwayTeam + FullTime + Season, epldat)
wins_byteam_byseason <- results_byhometeam_byseason[, 'HomeWin', ] +
	results_byawayteam_byseason[, 'AwayWin', ]
draws_byteam_byseason <- results_byhometeam_byseason[, 'Draw', ] +
	results_byawayteam_byseason[, 'Draw', ]
losses_byteam_byseason <- results_byhometeam_byseason[, 'AwayWin', ] +
	results_byawayteam_byseason[, 'HomeWin', ]

goals_byhometeam_byseason <- xtabs(
	cbind(HomeGoals, AwayGoals) ~ HomeTeam + Season, epldat)
goals_byawayteam_byseason <- xtabs(
	cbind(HomeGoals, AwayGoals) ~ AwayTeam + Season, epldat)
goalsscored_byteam_byseason <- goals_byhometeam_byseason[,, 'HomeGoals'] +
	goals_byawayteam_byseason[,, 'AwayGoals']
goalsconceded_byteam_byseason <- goals_byhometeam_byseason[,, 'AwayGoals'] +
	goals_byawayteam_byseason[,, 'HomeGoals']
goaldiff_byteam_byseason <- goalsscored_byteam_byseason -
	goalsconceded_byteam_byseason
	
points_byteam_byseason <- wins_byteam_byseason * 3 + draws_byteam_byseason

## league tables

get_league_tab <- function(season_index) {
	season <- seasons[season_index]
	teams <- names(teams_byseason[teams_byseason[, season] > 0, season])
	tab <- tibble::tibble(
		Club	      = teams,
		Matches       = (n_teams_per_season - 1) * 2,
		Wins          = wins_byteam_byseason[teams, season],
		Draws         = draws_byteam_byseason[teams, season],
		Losses        = losses_byteam_byseason[teams, season],
		GoalsScored   = goalsscored_byteam_byseason[teams, season],
		GoalsConceded = goalsconceded_byteam_byseason[teams, season],
		GoalDiff      = goaldiff_byteam_byseason[teams, season],
		Points        = points_byteam_byseason[teams, season])
	tab <- tab[order(tab$Points, tab$GoalDiff, tab$GoalsScored, 
		decreasing=T), ]
	tab$Position <- rownames(tab)
	ncol <- ncol(tab)
	tab <- tab[, c(ncol, 1:(ncol-1))]   ## rearrange columns
	
	## save new dataset to a csv file
	
	save_to_csv(tab, 
		filename = paste0('epl', season_years[season_index], 'leaguetable'))
	
	return(tab)
}

epl1011tab <- get_league_tab(1)
epl1112tab <- get_league_tab(2)
epl1213tab <- get_league_tab(3)
epl1314tab <- get_league_tab(4)
epl1415tab <- get_league_tab(5)
epl1516tab <- get_league_tab(6)
epl1617tab <- get_league_tab(7)
epl1718tab <- get_league_tab(8)
epl1819tab <- get_league_tab(9)
epl1920tab <- get_league_tab(10)

### MATCHDAY TABLES ###

get_matchday_tab1 <- function(season_index) {
	season <- seasons[season_index]
	teams <- names(teams_byseason[teams_byseason[, season] > 0, season])
	seasonstats <- subset(epldat, Season == season, 
		select = c(Date, HomeTeam, AwayTeam, FullTime))
	
	## contigency tables
	
	hometeam_bydate <- xtabs(~ Date + HomeTeam, seasonstats)[, teams]
	awayteam_bydate <- xtabs(~ Date + AwayTeam, seasonstats)[, teams]
	whoplayed_bydate <- hometeam_bydate + awayteam_bydate
	
	wins_bydate_byhometeam <- xtabs(~ Date + HomeTeam + FullTime, 	
		seasonstats)[, teams, 'HomeWin']
	draws_bydate_byhometeam <- xtabs(~ Date + HomeTeam + FullTime, 	
		seasonstats)[, teams, 'Draw']
	losses_bydate_byhometeam <- xtabs(~ Date + HomeTeam + FullTime, 	
		seasonstats)[, teams, 'AwayWin']
	wins_bydate_byawayteam <- xtabs(~ Date + AwayTeam + FullTime, 	
		seasonstats)[, teams, 'AwayWin']
	draws_bydate_byawayteam <- xtabs(~ Date + AwayTeam + FullTime, 	
		seasonstats)[, teams, 'Draw']
	
	## result table
	
	results_bydate_byteam <- whoplayed_bydate
	indices <- whoplayed_bydate == 1
	results_bydate_byteam[indices] <- 
		ifelse(wins_bydate_byhometeam[indices] == 1, 'HomeW',
			ifelse(draws_bydate_byhometeam[indices] == 1, 'HomeD',
				ifelse(losses_bydate_byhometeam[indices] == 1, 'HomeL',
					ifelse(wins_bydate_byawayteam[indices] == 1,'AwayW',
						ifelse(draws_bydate_byawayteam[indices] == 1,
							'AwayD', 'AwayL')))))
	results_bydate_byteam[whoplayed_bydate == 0] <- 'NotPlayed'
		
	## matchday table
	
	tab <- c()
	for (team in teams) {
		team_results <- as.vector(results_bydate_byteam[, team])
		team_results <- team_results[team_results != 'NotPlayed']
		team_points <- sapply(team_results, function(x) {
			ifelse(x == 'HomeW' | x == 'AwayW', 3,
				ifelse(x == 'HomeD' | x == 'AwayD', 1, 0))
		}, USE.NAMES = F)
		team_performance <- c(team, team_results, team_points)
		tab <- rbind(tab, team_performance)
	}
	tab <- tibble::as_tibble(tab)
	n_games_per_team <- 1 : ((n_teams_per_season - 1) * 2)
	colnames(tab) <- c('Club', 
		paste0('M', n_games_per_team, 'Results'),
		paste0('M', n_games_per_team, 'Points'))
	
	## save new dataset to a csv file
	
	save_to_csv(tab, filename = paste0('epl', season_years[season_index], 
		'matchday-results-pts'))

	return(tab)
}

get_matchday_tab2 <- function(season_index) {
	season <- seasons[season_index]
	teams <- names(teams_byseason[teams_byseason[, season] > 0, season])
	seasonstats <- subset(epldat, Season == season, 
		select = c(Date, HomeTeam, AwayTeam, HomeGoals, AwayGoals,
			HomeShots, AwayShots, HomeShotsOnTarget, AwayShotsOnTarget))
	
	## contigency tables
	
	hometeam_bydate <- xtabs(~ Date + HomeTeam, seasonstats)[, teams]
	awayteam_bydate <- xtabs(~ Date + AwayTeam, seasonstats)[, teams]
	whoplayed_bydate <- hometeam_bydate + awayteam_bydate
	
	goals_bydate_byhometeam <- xtabs(
		cbind(HomeGoals, AwayGoals) ~ Date + HomeTeam, seasonstats)[,teams,]
	goals_bydate_byawayteam <- xtabs(
		cbind(HomeGoals, AwayGoals) ~ Date + AwayTeam, seasonstats)[,teams,]
	goalsscored_bydate_byteam <- goals_bydate_byhometeam[,, 'HomeGoals'] +
		goals_bydate_byawayteam[,, 'AwayGoals']
	goalsscored_bydate_byteam[whoplayed_bydate == 0] <- NA
	goalsconceded_bydate_byteam <- goals_bydate_byhometeam[,, 'AwayGoals'] +
		goals_bydate_byawayteam[,, 'HomeGoals']
	goalsconceded_bydate_byteam[whoplayed_bydate == 0] <- NA
	
	shots_bydate_byhometeam <- xtabs(
		cbind(HomeShots, AwayShots) ~ Date + HomeTeam, seasonstats)[,teams,]
	shots_bydate_byawayteam <- xtabs(
		cbind(HomeShots, AwayShots) ~ Date + AwayTeam, seasonstats)[,teams,]
	shots_bydate_byteam <- shots_bydate_byhometeam[,, 'HomeShots'] +
		shots_bydate_byawayteam[,, 'AwayShots']
	shots_bydate_byteam[whoplayed_bydate == 0] <- NA
	shotsontarget_bydate_byhometeam <- xtabs(
		cbind(HomeShotsOnTarget, AwayShotsOnTarget) ~ Date + HomeTeam, 
		seasonstats)[,teams,]
	shotsontarget_bydate_byawayteam <- xtabs(
		cbind(HomeShotsOnTarget, AwayShotsOnTarget) ~ Date + AwayTeam, 
		seasonstats)[,teams,]
	shotsontarget_bydate_byteam <- 
		shotsontarget_bydate_byhometeam[,, 'HomeShotsOnTarget'] +
		shotsontarget_bydate_byawayteam[,, 'AwayShotsOnTarget']
	shotsontarget_bydate_byteam[whoplayed_bydate == 0] <- NA
		
	## matchday table
	
	tab <- c()
	for (team in teams) {
		team_goalsscored <- as.vector(goalsscored_bydate_byteam[, team])
		team_goalsscored <- team_goalsscored[!is.na(team_goalsscored)]
		
		team_goalsconceded <- as.vector(goalsconceded_bydate_byteam[, team])
		team_goalsconceded <- team_goalsconceded[!is.na(team_goalsconceded)]
		
		team_shots <- as.vector(shots_bydate_byteam[, team])
		team_shots <- team_shots[!is.na(team_shots)]
		
		team_shotsontarget <- as.vector(shotsontarget_bydate_byteam[, team])
		team_shotsontarget <- team_shotsontarget[!is.na(team_shotsontarget)]
		
		team_performance <- c(team_goalsscored, team_goalsconceded,
			team_shots, team_shotsontarget)
		
		tab <- rbind(tab, team_performance)
	}
	tab <- tibble::as_tibble(tab)
	tab$Club <- teams
	ncol <- ncol(tab)
	tab <- tab[, c(ncol, 1:(ncol-1))]   ## rearrange columns
	n_games_per_team <- 1 : ((n_teams_per_season - 1) * 2)
	colnames(tab) <- c('Club', 
		paste0('M', n_games_per_team, 'GoalsScored'),
		paste0('M', n_games_per_team, 'GoalsConceded'),
		paste0('M', n_games_per_team, 'Shots'),
		paste0('M', n_games_per_team, 'ShotsOnTarget'))
	
	## save new dataset to a csv file
	
	save_to_csv(tab, filename = paste0('epl', season_years[season_index], 
		'matchday-goals-shots'))

	return(tab)
}

epl1011matchdaystats1 <- get_matchday_tab1(1)
epl1112matchdaystats1 <- get_matchday_tab1(2)
epl1213matchdaystats1 <- get_matchday_tab1(3)
epl1314matchdaystats1 <- get_matchday_tab1(4)
epl1415matchdaystats1 <- get_matchday_tab1(5)
epl1516matchdaystats1 <- get_matchday_tab1(6)
epl1617matchdaystats1 <- get_matchday_tab1(7)
epl1718matchdaystats1 <- get_matchday_tab1(8)
epl1819matchdaystats1 <- get_matchday_tab1(9)
epl1920matchdaystats1 <- get_matchday_tab1(10)

epl1011matchdaystats2 <- get_matchday_tab2(1)
epl1112matchdaystats2 <- get_matchday_tab2(2)
epl1213matchdaystats2 <- get_matchday_tab2(3)
epl1314matchdaystats2 <- get_matchday_tab2(4)
epl1415matchdaystats2 <- get_matchday_tab2(5)
epl1516matchdaystats2 <- get_matchday_tab2(6)
epl1617matchdaystats2 <- get_matchday_tab2(7)
epl1718matchdaystats2 <- get_matchday_tab2(8)
epl1819matchdaystats2 <- get_matchday_tab2(9)
epl1920matchdaystats2 <- get_matchday_tab2(10)

### HEAD-TO-HEAD TABLES ###

## functions for retaining only the lower triangular part of a table

format_h2h_tab1 <- function(tab) {
	diag(tab) <- NA
	tab[upper.tri(tab)] <- NA
	return(tab)
}

format_h2h_tab2 <- function(tab) {
	for (i in 1:dim(tab)[3]) {
		diag(tab[,, i]) <- NA
		tab[upper.tri(tab[,, i])] <- NA		
	}
	return(tab)
}

## head-to-head tables

get_h2h_tab <- function(data) {
	h2h_n_meets <- xtabs(~ HomeTeam + AwayTeam, data)
	h2h_n_meets <- format_h2h_tab1(h2h_n_meets)
	
	h2h_goals <- xtabs(
		cbind(HomeGoals, AwayGoals) ~ HomeTeam + AwayTeam, data)
	h2h_goals <- format_h2h_tab2(h2h_goals)
	
	h2h_shots <- xtabs(
		cbind(HomeShots, AwayShots) ~ HomeTeam + AwayTeam, data)
	h2h_shots <- format_h2h_tab2(h2h_shots)
	
	h2h_shotsontarget <- xtabs(
		cbind(HomeShotsOnTarget, AwayShotsOnTarget) ~ HomeTeam + AwayTeam, 
		data)
	h2h_shotsontarget <- format_h2h_tab2(h2h_shotsontarget)
	
	h2h_corners <- xtabs(
		cbind(HomeCorners, AwayCorners) ~ HomeTeam + AwayTeam, data)
	h2h_corners <- format_h2h_tab2(h2h_corners)
	
	h2h_fouls <- xtabs(
		cbind(HomeFouls, AwayFouls) ~ HomeTeam + AwayTeam, data)
	h2h_fouls <- format_h2h_tab2(h2h_fouls)
	
	h2h_yellowcards <- xtabs(
		cbind(HomeYellowCards, AwayYellowCards) ~ HomeTeam + AwayTeam, 
		data)
	h2h_yellowcards <- format_h2h_tab2(h2h_yellowcards)
	
	h2h_redcards <- xtabs(
		cbind(HomeRedCards, AwayRedCards) ~ HomeTeam + AwayTeam, data)
	h2h_redcards <- format_h2h_tab2(h2h_redcards)
		
	h2h_all_stats <- abind::abind(h2h_n_meets, h2h_goals, h2h_shots, 
		h2h_shotsontarget, h2h_corners, h2h_fouls, h2h_yellowcards, 
		h2h_redcards)
	dimnames(h2h_all_stats) <- list(
		HomeTeam 	= dimnames(h2h_all_stats)[[1]],
		AwayTeam 	= dimnames(h2h_all_stats)[[2]],
		Stats		= c('MatchCount', dimnames(h2h_all_stats)[[3]][-1]))
	return(h2h_all_stats)
}

## stats across all seasons

epl_allseasons_h2h <- get_h2h_tab(epldat)

## save new datasets to csv files

statsnames <- tolower(dimnames(epl_allseasons_h2h)[[3]])
for (i in 1:length(statsnames)) {
	save_to_csv(epl_allseasons_h2h[,, i], rownames = T,
		filename = paste0('epl-allseasons-head2head-', statsnames[i]))
}

## stats for each season

get_h2h_tab_byseason <- function(season_index) {
	season <- seasons[season_index]
	teams <- names(teams_byseason[teams_byseason[,season]>0, season])
	seasonstats <- subset(epldat, Season == season)
	h2h_stats <- get_h2h_tab(seasonstats)
}

## (optional) save new datasets to csv files

# for (season_index in 1:length(seasons)) {
# 	h2h_seasonstats <- get_h2h_tab_byseason(season_index)
# 	statsnames <- dimnames(h2h_seasonstats)[[3]]
# 	for (i in 1:length(statsnames)) {
#		save_to_csv(h2h_seasonstats[i], rownames = T,
#			filename = paste0('epldat10seasons/epl', 
#				season_years[season_index], '-head2head-', statsnames[i]))
# 	}	
# }