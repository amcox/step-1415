library(reshape2)
library(dplyr)
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

# Requires MAP data exported from the MAP repo using the generic export file.
df.map <- subset(load_map_data(), subject == 'reading')

df.step <- load_data_with_gaps_long()

df.s.sub <- subset(df.step, wave == 3)

df.b <- merge(df.s.sub, df.map, by.x='id', by.y='id')

# STEP Gap and MAP Percentile plot
p <- ggplot(df.b, aes(x=winter.percentile, y=gap))+
  geom_jitter(alpha=.3, position=position_jitter(width=.1, height=.25))+
  geom_smooth()+
  scale_y_continuous(breaks=seq(-10,10,2))+
  labs(title="STEP Gap and MAP Percentile",
    x="National Percentile on Winter MAP Reading",
    y="Gap from Grade Level in STEPs"
  )+
  theme_bw()+
  facet_grid(grade ~ school.x, margins=T)
save_plot_as_pdf(p, 'STEP Gap and MAP Percentile 14-15, W3-Winter')
  
# Band Comparison of STEP and MAP
d <- subset(df.b, !is.na(home.room))
make_step_map_band_plot_by_hr <- function(school.name, d) {
  p <- ggplot(subset(d, school.y == school.name), aes(x=winter.percentile, y=gap))+
    geom_point(shape=1)+
    geom_hline(yintercept=c(1.5, -1.5))+
    geom_vline(xintercept=c(40, 60))+
    scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10))+
    scale_y_continuous(breaks=seq(-12, 12, 1))+
    labs(x='MAP Percentile Rank',
      y='Gap in STEPs to Grade Level\n(Positive is Good, 3 STEPs = 1 Year)',
      title=paste0('Student Performance on STEP Wave 3 and Winter MAP Reading, ', school.name)
    )+
    theme_bw()+
    theme(axis.text.x=element_text(size=7),
      axis.text.y=element_text(size=7)
    )+
    facet_wrap(~ home.room)
    save_plot_as_pdf(p, paste0('Band Scatter STEP Wave 3 and Winter MAP by Teacher, ', school.name))
}
lapply(schools, make_step_map_band_plot_by_hr, d=d)
make_step_map_band_plot <- function(school.name, d) {
  p <- ggplot(subset(d, school.y == school.name), aes(x=fall.percentile, y=gap))+
    geom_point(shape=1)+
    geom_hline(yintercept=c(1.5, -1.5))+
    geom_vline(xintercept=c(40, 60))+
    scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10))+
    scale_y_continuous(breaks=seq(-12, 12, 1))+
    labs(x='MAP Percentile Rank',
      y='Gap in STEPs to Grade Level\n(Positive is Good, 3 STEPs = 1 Year)',
      title=paste0('Student Performance on STEP Wave 1 and Fall MAP Reading, ', school.name)
    )+
    theme_bw()+
    theme(axis.text.x=element_text(size=7),
      axis.text.y=element_text(size=7)
    )+
    facet_wrap(school.y ~ grade)
    save_plot_as_pdf(p, paste0('Band Scatter STEP Wave 1 and Fall MAP, ', school.name))
}
lapply(schools, make_step_map_band_plot, d=d)
  
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
calculate_comparison_table_map_step <- function(d) {
  out <- data.frame(prop.table(table(d$met.rit.goal, d$grew2), 1))
  names(out) <- c("map", "step", "perc")
  return(out)
}
d.p <- df.b %>% group_by(grade) %>% do(calculate_comparison_table_map_step(.))
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
