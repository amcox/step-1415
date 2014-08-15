fall_winter_rit_growth <- function(r){
  return(as.numeric(r["winter.rit"])-as.numeric(r["fall.rit"]))
}

winter_spring_rit_growth <- function(r){
  return(as.numeric(r["spring.rit"])-as.numeric(r["winter.rit"]))
}

fall_spring_rit_growth <- function(r){
  return(as.numeric(r["spring.rit"])-as.numeric(r["fall.rit"]))
}

fall_winter_rit_growth_dif <- function(r){
  as.numeric(r[['fall.winter.rit.growth']]) - as.numeric(r[['goal.fall.winter']])
}

fall_winter_percentile_growth <- function(r){
  as.numeric(r[['winter.percentile']]) - as.numeric(r[['fall.percentile']])
}