library(tidyr)
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

df <- load_data_with_gaps_long()
d <- subset(df, grade == -1 & !is.na(level))

# C wants to see the percent of students at each STEP by wave, by school and by class

# Percent of students at each STEP by wave by school
d.percs <- d %>% group_by(school, wave) %>% do(percents_of_total(.$level, 'level'))
d.percs$level <- factor(d.percs$level)
d.percs$level <- reorder.factor(d.percs$level, new.order=c('3', '2', '1', '0', '-1'))
d.percs <- d.percs[order(as.numeric(d.percs$level)),]

p <- ggplot(subset(d.percs, level != -1), aes(x=wave, y=perc, fill=level))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=pk.wave.pal, name='STEP Level')+
  scale_y_continuous(labels=percent, breaks=seq(0,1,.1), limits=c(0,1))+
  labs(title='Percent of Students at Each STEP Level for PK 2013-14 By School',
    x='Wave',
    y='Percent of Students'
  )+
  theme_bw()+
  facet_wrap(~school, nrow=1)
  
save_plot_as_pdf(p, 'PK Percent of Students at Each Level 2013-14 By School')

# Percent of students at each STEP by wave by homeroom
d.percs <- d %>% group_by(home.room, wave) %>% do(percents_of_total(.$level, 'level'))
d.percs$level <- factor(d.percs$level)
d.percs$level <- reorder.factor(d.percs$level, new.order=c('3', '2', '1', '0', '-1'))
d.percs <- d.percs[order(as.numeric(d.percs$level)),]

p <- ggplot(subset(d.percs, level != -1), aes(x=wave, y=perc, fill=level))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=pk.wave.pal, name='STEP Level')+
  scale_y_continuous(labels=percent, breaks=seq(0,1,.1), limits=c(0,1))+
  labs(title='Percent of Students at Each STEP Level for PK 2013-14 By Teacher',
    x='Wave',
    y='Percent of Students'
  )+
  theme_bw()+
  facet_wrap(~home.room, nrow=2)
  
save_plot_as_pdf(p, 'PK Percent of Students at Each Level 2013-14 By Teacher')