# Line graph of STEP average gap over one year
make_plot_average_gap <- function(df, title="STEP Average Gap from Grade Level by Wave, 2013-14"){
  d <- df[, colnames(df) %in% c("id", "home.room", "school", "grade", "w1.gap", "w2.gap", 
  						"w3.gap", "w4.gap")]
  dm <- melt(d, id.vars=c("id", "home.room", "school", "grade"), variable.name="wave",
  					value.name="gap"
  )
  dm.s <- ddply(dm, .(wave, school, grade), summarize, mean.gap=mean(gap, na.rm=T))
  ggplot(dm.s, aes(x=wave, y=mean.gap, color=school))+
  	geom_line(aes(group=school), alpha=.5)+
  	geom_point()+
  	geom_hline(y.intercept=0, color="blue", size=.25)+
  	scale_color_brewer(palette="Set1")+
  	labs(title=title,
  				x="Wave",
  				y="Average Gap from Grade Level in STEPs (negative is below level)"
  	)+
  	theme_bw()+
  	theme(axis.text.x=element_text(angle=90, vjust=0.5))+
  	facet_wrap( ~ grade, ncol=4)
}