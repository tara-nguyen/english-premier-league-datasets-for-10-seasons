# English Premier League Data

AUTHOR: [**TARA NGUYEN**](https://www.linkedin.com/in/taranguyen264/)

## Background

The English Premier League is the top level of competition in English football (or soccer, as Americans like to call it). It is widely regarded as one of the most competitive and is one of the most watched sports competitions in the world. Each season typically lasts from mid-August to mid-May (with the exception of the 2019/2020 season, which was postponed for three months due to COVID-19). Each season 20 teams compete for the Premier League trophy, as well as for the top four spots, because the top four teams will automatically be eligible for the next season of the Champions League (which is one of the most prestigious football tournaments not just in Europe but also in the world).

## Datasets

The repo contains 46 datasets for ten seasons of the Premier League, from the 2010/2011 season to the 2019/2020 season. The 2020/2021 season was not included because it is an ongoing season.

### Original Datasets

The original datasets came from https://www.football-data.co.uk/englandm.php. Ten datasets (one for each season) were imported, each containing match statistics and betting odds for each game in one season.

### Data Cleaning and Wrangling

All steps of data cleaning and wrangling were done entirely in R (see [`epldat10seasons_DataWrangling.R`](epldat10seasons_DataWrangling.R)). The original ten datasets were cleaned, transformed, and merged into one big dataset ([`epl-allseasons-matchstats.csv`](epldat10seasons/epl-allseasons-matchstats.csv)) containing the following information:
- Season
- Date
- Referee
- Home teams and away teams
- Results at full time and at half-time
- Number of goals scored by the home team at full time and that at half-time
- Number of goals scored by the away team at full time and that at half-time
- Number of: shots, shots on target, corner kicks, fouls committed, yellow cards received, and red cards received. Each of these pieces of information is available for both the home team and the away team.

### Additional datasets

Forty-five other datasets were created based on the [`epl-allseasons-matchstats.csv`](epldat10seasons/epl-allseasons-matchstats.csv) dataset. They include:
- 10 season-end league tables, one for each season covered by the data;
- 10 datasets (one for each season) containing the result and number of points each team got after each game/match;
- 10 datasets (one for each season) containing the number of goals scored and number of goals conceded by each team after each game/match; and
- 15 datasets containing the head-to-head match statistics across all ten seasons.

#### Note

Included at the end of [`epldat10seasons_DataWrangling.R`](epldat10seasons_DataWrangling.R) is the code for 150 additional datasets containing head-to-head statistics for each of the ten seasons. The reader is free to try out the code to obtain the desired dataset(s).

## Usage Note

You are free to use any of the materials in this repo. **If you do use any, please remember to give credit (e.g. by mentioning either my name or the repo, by giving a url link to it, etc.).**
