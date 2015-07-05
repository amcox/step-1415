load_step_data <- function(y=2015){
  read.csv(file=paste0("./../Data/step data/step data ", y, ".csv"), head=TRUE, na.string=c("", " ", "  "))
}

load_data_with_calculated_fields <- function(y=2015, gaps=T){
	if(gaps){
		d <- load_step_data(y=y)
		d <- calculate_gaps_return_wide(d, step.year=y)
		add_standard_calculated_fields(d, gaps)
	}else{
		add_standard_calculated_fields(load_step_data(y=y), gaps)
	}
}

load_data_with_gaps_long <- function(y=2015){
  df <- load_step_data(y=y)
  return(calculate_gaps_return_long(df, step.year=y))
}

load_step_goals <- function(){
  read.csv(file="./../Data/step goals.csv", head=TRUE,
  	na.string=c("", " ", "  ")
  )
}

load_step_goals_to_wave_and_year <- function(w=6, y=c(2014)){
  df <- load_step_goals()
  subset(df, wave <= w & year %in% y)
}

load_step_goals_to_wave_and_year_just_ends <- function(w=6, y=c(2014)){
  df <- load_step_goals_to_wave_and_year(w, y)
  df %>% group_by(year, grade) %>% arrange(wave) %>% slice(c(1, n()))
}

load_map_data <- function(){
  read.csv(file="./../Data/map data all.csv", head=TRUE, na.string=c("", " ", "  "),
    stringsAsFactors=F
  )
}

load_leap_data <- function(){
  d <- read.csv(file="./../Data/benchmark and leap scores.csv", head=TRUE, na.string=c("", " ", "  "),
    stringsAsFactors=F
  )
  names(d) <- tolower(names(d))
  return(d)
}

read_step_file_and_add_year <- function(filename) {
  library(stringr)
	d <- read.csv(file=filename, head=TRUE, na.string=c("", " ", "  "), stringsAsFactors=F)
	year.string <- str_extract(filename, "20[0-9][0-9]")
	d$year <- rep(year.string, nrow(d))
	return(d)
}

load_all_step_data_return_long <- function() {
	library(tidyr)
  filenames <- list.files("./../Data/step data", pattern=".csv", full.names=TRUE)
  ldf <- lapply(filenames, read_step_file_and_add_year)
  res <- lapply(ldf, gather, wave, level, -id, -last.name, -first.name, -school, -grade, -home.room, -year)
  d <- rbind_all(res)
	d$wave <- as.numeric(gsub("w", "", d$wave))
  return(d)
}

