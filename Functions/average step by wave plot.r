make_plot_average_step <- function(df, df.goals, title="STEP Average Level by Wave"){
  d.sub <- df[, colnames(df) %in% c("id", "home.room", "school", "grade", "year", "w1", "w2", 
  						"w3", "w4", "w5", "w6")]
  dm <- melt(d.sub, id.vars=c("id", "home.room", "school", "grade"),
  						variable.name="wave", value.name="level"
  )
  dm.s <- drop.levels(ddply(dm, .(wave, school, grade), summarize,
  													mean.level=mean(level, na.rm=T))
  )
  ggplot(dm.s, aes(x=wave, y=mean.level, color=school))+
  	geom_line(aes(group=school), alpha=.5)+
  	geom_point()+
    geom_line(data=df.goals, aes(x=wave, y=level), color="blue", size=.25)+
  	scale_color_brewer(palette="Set1")+
  	labs(title=title,
  				x="Wave",
  				y="Average Level in STEPs"
  	)+
  	theme_bw()+
  	theme(axis.text.x=element_text(angle=90, vjust=0.5)
  	)+
  	facet_wrap( ~ grade, nrow=1)
}