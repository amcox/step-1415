calculate_gaps_return_long <- function(d, step.year=2015){
  library(reshape2)
  original.cols <- names(d)
  d.m <- melt(d,
    id.vars=original.cols[!original.cols %in% c("w1", "w2", "w3", "w4", 'w5', 'w6')],
    variable.name="wave", value.name="level"
  )
  d.m$wave <- as.numeric(gsub("w", "", d.m$wave))
  step.goals <- subset(load_step_goals(), year == step.year)[, c("wave", "grade", "level")]
  names(step.goals) <- c("wave", "grade", "goal.level")
  d.with.goals <- merge(d.m, step.goals, all.x=T)
  d.with.goals <- d.with.goals %>% mutate(gap = level - goal.level)
  return(d.with.goals)
}

calculate_gaps_return_wide <- function(d, step.year=2015){
  library(reshape2)
  d <- calculate_gaps_return_long(d, step.year)
	
	# Turn levels back into wide
  a <- dcast(d, id + last.name + first.name + school + grade + home.room ~ wave, value.var='level')
  a1 <- select(a, id, last.name, first.name, school, grade, home.room)
  a2 <- select(a, -c(id, last.name, first.name, school, grade, home.room))
  colnames(a2) <- paste("w", colnames(a2), sep = "")
  a3 <- cbind(a1, a2)
  
	# Turn gaps back into wide
  b <- dcast(d, id + last.name + first.name + school + grade + home.room ~ wave, value.var='gap')
  b1 <- select(b, id, last.name, first.name, school, grade, home.room)
  b2 <- select(b, -c(id, last.name, first.name, school, grade, home.room))
  colnames(b2) <- paste("w", colnames(b2), ".gap", sep = "")
  b3 <- cbind(b1, b2)
	
	# Combine the wave levels and gaps back into one DF
	c <- merge(a3, b3, by=c("id", "last.name", "first.name", "school", "grade", "home.room"))
	
  return(c)
}

create_w_first_last_cols <- function(d){
  w_col_data <- d[,colnames(d) %in% c("w1", "w2", "w3", "w4", 'w5', 'w6')]
  d$first <- apply(w_col_data, 1, function(r){
    r[!is.na(r)][1]
  })
  d$latest <- apply(w_col_data, 1, function(r){
    not.nas <- r[!is.na(r)]
    if(length(not.nas) > 0){
      return(not.nas[length(not.nas)])
    }else{
      return(NA)
    }
  })
  return(d)
}

create_gap_first_last_cols <- function(d){
  w_col_data <- d[,colnames(d) %in% c("w1.gap", "w2.gap", "w3.gap", "w4.gap", 'w5.gap', 'w6.gap')]
  d$first.gap <- apply(w_col_data, 1, function(r){
    r[!is.na(r)][1]
  })
  d$latest.gap <- apply(w_col_data, 1, function(r){
    not.nas <- r[!is.na(r)]
    if(length(not.nas) > 0){
      return(not.nas[length(not.nas)])
    }else{
      return(NA)
    }
  })
  return(d)
}

add_standard_calculated_fields <- function(df, gaps=T){
  df <- create_w_first_last_cols(df)
  if(gaps){
    df <- create_gap_first_last_cols(df)
    df$gap.growth <- (df$first.gap - df$latest.gap)
    df$w12.gap.growth <- (df$w1.gap - df$w2.gap)
    df$w23.gap.growth <- (df$w2.gap - df$w3.gap)
    df$w34.gap.growth <- (df$w3.gap - df$w4.gap)
    if('w5.gap' %in% colnames(df)){
      df$w45.gap.growth <- (df$w4.gap - df$w5.gap)
    }
    if('w6.gap' %in% colnames(df)){
      df$w56.gap.growth <- (df$w5.gap - df$w6.gap)
    }
  }
  df$growth <- (df$latest - df$first)
  df$w2.growth <- (df$w2 - df$first)
  df$w3.growth <- (df$w3 - df$first)
  df$w4.growth <- (df$w4 - df$first)
  if('w5' %in% colnames(df)){
    df$w5.growth <- (df$w5 - df$first)
  }
  if('w6' %in% colnames(df)){
    df$w6.growth <- (df$w6 - df$first)
  }
  df$w12.growth <- (df$w2 - df$w1)
  df$w23.growth <- (df$w3 - df$w2)
  df$w34.growth <- (df$w4 - df$w3)
  if('w5' %in% colnames(df)){
    df$w45.growth <- (df$w5 - df$w4)
  }
  if('w6' %in% colnames(df)){
    df$w56.growth <- (df$w6 - df$w5)
  }
  return(df)
}

