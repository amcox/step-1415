library(reshape2)
library(plyr)
library(ggplot2)
library(scales)
library(gdata)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

# Compare STEP and MAP data (W1-Fall, W3-Winter) scatterplot
df.map <- subset(load_map_data(), subject == 'reading')
df.step <- load_data_with_gaps_long()

df.s.sub <- subset(df.step, wave == 3)

df.b <- merge(df.s.sub, df.map, by.x='id', by.y='id')

ggplot(df.b, aes(x=winter.percentile, y=gap))+
  geom_jitter(alpha=.3, position=position_jitter(width=.1, height=.25))+
  geom_smooth()+
  scale_y_continuous(breaks=seq(-10,10,2))+
  labs(title="STEP Gap and MAP Percentile",
    x="National Percentile on Winter MAP Reading",
    y="Gap from Grade Level in STEPs"
  )+
  theme_bw()+
  facet_grid(grade.x ~ school.x, margins=T)
  
# 2 STEP and MAP Goal Comparison
df <- load_data_with_calculated_fields(gaps=F)
df$grew2 <- cut(df$growth, c(-100, 2, 100),
  labels=c("grew < 2", "grew >= 2"), right=FALSE
)
df.map <- subset(load_map_data(), subject == 'reading')
df.map$fall.winter.rit.growth <- apply(df.map, 1, fall_winter_rit_growth)
df.map$fall.winter.rit.growth.dif <- apply(df.map, 1, fall_winter_rit_growth_dif)
df.b <- merge(df, df.map, by.x='id', by.y='id')

# by STEP growth categories
p <- ggplot(df.b, aes(x=grew2, y=fall.winter.rit.growth.dif))+
  geom_boxplot(notch=T, outlier.size=0)+
  geom_jitter(position=position_jitter(width=.4, height=0), alpha=0.25)+
  scale_y_continuous(breaks=seq(-30,30,2))+
  labs(title="MAP Growth Goals by STEP Growth Goals",
    x="STEP Growth from BOY to Wave 3",
    y="Difference Between RIT Fall to Winter Growth Goal and Actual (Positive is Beating Goal)"
  )+
  theme_bw()+
  facet_wrap( ~ grade.x)
save_plot_as_pdf(p, "STEP Growth and MAP Goals by STEP Categories")
  

# by MAP growth categories
df.b$met.rit.goal <- cut(df.b$fall.winter.rit.growth.dif, c(-100, 0, 100),
  labels=c("did not meet", "met"), right=FALSE
)
d.p <- ddply(df.b, .(grade.x), function(d){
  out <- data.frame(prop.table(table(d$met.rit.goal, d$grew2), 1))
  names(out) <- c("map", "step", "perc")
  return(out)
})
d.p <- subset(d.p, step == 'grew >= 2')
p <- ggplot(d.p, aes(x=map, y=perc, fill=step))+
  geom_bar(stat="identity", fill="#009900")+
  scale_y_continuous(labels=percent, limits=c(0,1), breaks=seq(0,1,.1))+
  labs(title="STEP Growth Goals by MAP Goals",
    x="Did Students Meet or Exceed Their Fall to Winter MAP Growth Goal?",
    y="Percent of Students Growing at Least 2 STEPs from BOY to Wave 3"
  )+
  theme_bw()+
  facet_wrap( ~ grade.x)
save_plot_as_pdf(p, "STEP Growth and MAP Goals by MAP Categories")
