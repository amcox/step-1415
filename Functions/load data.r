load_latest_step_data <- function(){
  read.csv(file="./../Data/step data.csv", head=TRUE, na.string=c("", " ", "  "))
}

load_data_with_calculated_fields <- function(){
  add_standard_calculated_fields(load_latest_step_data())
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