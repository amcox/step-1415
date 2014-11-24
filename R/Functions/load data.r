load_latest_step_data <- function(){
  read.csv(file="./../Data/step data.csv", head=TRUE, na.string=c("", " ", "  "))
}

load_data_with_calculated_fields <- function(gaps=T){
  add_standard_calculated_fields(load_latest_step_data(), gaps)
}

load_data_with_gaps_long <- function(){
  df <- load_latest_step_data()
  return(calculate_gaps_return_long(df))
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
  ddply(df, .(year, grade), .fun=function(d){
    sorted.d <- d[order(d$wave),]
    r1 <- sorted.d[1,]
    r2 <- sorted.d[nrow(sorted.d),]
    data.frame(rbind(r1, r2))
  })
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