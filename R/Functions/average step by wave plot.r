make_plot_average_step <- function(df, df.goals, title="STEP Average Level by Wave, 2014-15"){
  # Line graph of STEP average gap over one year
  # Requires data in long format
  library(gdata)
  dm.s <- drop.levels(ddply(df, .(wave, school, grade), summarize,
  													mean.level=mean(level, na.rm=T))
  )
  ggplot(dm.s, aes(x=wave, y=mean.level, color=school))+
  	geom_line(aes(group=school), alpha=.5)+
  	geom_point()+
    geom_line(data=df.goals, aes(x=wave, y=level), color="blue", size=.25)+
  	scale_color_brewer(palette="Set1")+
    scale_x_continuous(limits=c(1,4), breaks=seq(1,4,1))+
  	labs(title=title,
  				x="Wave",
  				y="Average Level in STEPs"
  	)+
  	theme_bw()+
  	theme(axis.text.x=element_text(angle=90, vjust=0.5)
  	)+
  	facet_wrap( ~ grade, nrow=1)
}