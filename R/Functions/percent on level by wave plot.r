make_plot_percent_of_students_on_level <- function(df, title="Percent of Students on Level in STEP by Wave"){
  # Percent of students on level by wave
  # Requires long format data
  dm <- drop.levels(ddply(df, .(wave, school, grade), summarize,
  													on.level=length(gap[gap >= 0 & !is.na(gap)]) / 
  													length(gap[!is.na(gap)])
  										)
  )
  ggplot(dm, aes(x=wave, y=on.level, color=school))+
  	geom_line(aes(group=school), alpha=.5)+
  	geom_point()+
  	scale_color_brewer(palette="Set1")+
  	scale_y_continuous(labels=percent, breaks=seq(0,1,.1), limits=c(0,1))+
    scale_x_continuous(limits=c(1,4), breaks=seq(1,4,1))+
  	labs(title=title,
  				x="Wave",
  				y="Percent of Students on Level in STEP"
  	)+
  	theme_bw()+
  	theme(axis.text.x=element_text(angle=90, vjust=0.5)
  	)+
  	facet_wrap( ~ grade, ncol=4)
}