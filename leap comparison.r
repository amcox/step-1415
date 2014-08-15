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

df.step <- load_data_with_gaps_long()
df.step <- subset(df.step, wave == 3 & !is.na(level))
df.step$level[df.step$level == 'FP'] <- 13
d.l <- load_leap_data()
d.l <- subset(d.l, test == 'L14' & achievement_level %in% c('A', 'M', 'B', 'AB', 'U'))
d.l <- d.l[, c('achievement_level', 'student_number', 'subject')]

d <- merge(df.step, d.l, by.x='id', by.y='student_number')

d$level <- as.numeric(d$level)

steps <- unique(d$level)


# TODO: Make work for separate subjects (math, ela), then facet for schools


find_step_leap_prof_percs <- function(d) {
  find_percent_basic <- function(step.cut, data){
  	mean(data[data$level >= step.cut, ]$achievement_level %in% c('A', 'M', 'B'))
  }
  
  steps <- unique(d$level)
  data.frame(step=steps, perc.prof=sapply(steps, find_percent_basic, d))
}

# Graphs of percents basic and above at each STEP level
d.perc <- d %>% group_by(subject, grade) %>% do(find_step_leap_prof_percs(.))

p <- ggplot(d.perc, aes(x=step, y=perc.prof))+
	scale_x_continuous(breaks=seq(1, 13, 1))+
	scale_y_continuous(labels=percent)+
	geom_point()+
	labs(title="2014 LEAP Scores by 2014 Wave 3 STEP Level",
				x="STEP",
				y="Percent of Students at or Above that STEP Level Scoring Basic or Above"
	)+
	theme_bw()+
  facet_grid(grade ~ subject)
save_plot_as_pdf(p, '2013-14 LEAP and STEP Scores, By Grade and Subject')

d.perc <- d %>% group_by(subject, grade, school) %>% do(find_step_leap_prof_percs(.))
p <- ggplot(subset(d.perc, subject == 'ela'), aes(x=step, y=perc.prof))+
	scale_x_continuous(breaks=seq(1, 13, 1))+
	scale_y_continuous(labels=percent)+
	geom_point()+
	labs(title="2014 LEAP Scores by 2014 Wave 3 STEP Level, ELA",
				x="STEP",
				y="Percent of Students at or Above that STEP Level Scoring Basic or Above"
	)+
	theme_bw()+
  facet_grid(grade ~ school)
save_plot_as_pdf(p, '2013-14 LEAP and STEP Scores, ELA By Grade and School')

# Plots of ALs at each STEP
d <- d %>% mutate(al.cat=achievement_level %in% c('A', 'M', 'B'))
d$al.cat[d$al.cat] <- 'CR'
d$al.cat[d$al.cat == 'FALSE'] <- 'NCR'

dh <- d %>% group_by(subject, grade, al.cat) %>% do(get_counts(., 'level', seq(-1, 13,1)))
p <- ggplot(subset(dh, subject %in% c('ela', 'math')), aes(x=h.mids+0.5, y=h.counts, color=al.cat))+
  geom_line()+
  scale_x_continuous(breaks=seq(-1, 13, 1))+
  scale_color_manual(values=c('CR'='#198D33', 'NCR'='#D16262'),
    labels=c('CR'='Basic or Above', 'NCR'='Below Basic')
  )+
  labs(x='Wave 3 STEP',
    y='Number of Students',
    title='Number of Students Proficient on LEAP by STEP\n2013-14 By Subject - Grade'
  )+
  theme_bw()+
  theme(
    legend.title=element_blank()
  )+
  facet_grid(grade ~ subject)
save_plot_as_pdf(p, '2013-14 LEAP and STEP Counts, By Grade and Subject')

dh <- d %>% group_by(subject, grade, al.cat, school) %>% do(get_counts(., 'level', seq(-1, 13,1)))
p <- ggplot(subset(dh, subject %in% c('ela')), aes(x=h.mids+0.5, y=h.counts, color=al.cat))+
  geom_line()+
  scale_x_continuous(breaks=seq(-1, 13, 1))+
  scale_color_manual(values=c('CR'='#198D33', 'NCR'='#D16262'),
    labels=c('CR'='Basic or Above', 'NCR'='Below Basic')
  )+
  labs(x='Wave 3 STEP',
    y='Number of Students',
    title='Number of Students Proficient on ELA LEAP by STEP\n2013-14 By School - Grade'
  )+
  theme_bw()+
  theme(
    legend.title=element_blank()
  )+
  facet_grid(grade ~ school)
save_plot_as_pdf(p, '2013-14 LEAP and STEP Counts, ELA By Grade and School')

