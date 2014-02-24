# Percent of students on level by wave
make_plot_percent_of_students_on_level <- function(df, title="Percent of Students on Level in STEP by Wave"){
  d <- df[, colnames(df) %in% c("id", "home.room", "school", "grade", "year", "w1.gap", "w2.gap", 
  						"w3.gap", "w4.gap", "w5.gap", "w6.gap")]
  names(d) <- c("id", "home.room", "school", "grade", "w1", "w2", 
  						"w3", "w4", "w5", "w6")[1:length(colnames(d))]
  dm <- melt(d, id.vars=c("id", "home.room", "school", "grade"),
  						variable.name="wave", value.name="gap"
  )
  dm.s <- drop.levels(ddply(dm, .(wave, school, grade), summarize,
  													on.level=length(gap[gap >= 0 & !is.na(gap)]) / 
  													length(gap[!is.na(gap)])
  										)
  )
  ggplot(dm.s, aes(x=wave, y=on.level, color=school))+
  	geom_line(aes(group=school), alpha=.5)+
  	geom_point()+
  	scale_color_brewer(palette="Set1")+
  	scale_y_continuous(labels=percent, breaks=seq(0,1,.1), limits=c(0,1))+
  	labs(title=title,
  				x="Wave",
  				y="Percent of Students on Level in STEP"
  	)+
  	theme_bw()+
  	theme(axis.text.x=element_text(angle=90, vjust=0.5)
  	)+
  	facet_wrap( ~ grade, ncol=4)
}