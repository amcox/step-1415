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
d.l <- select(d.l, subject:scaled_score, percent, on_level, test_name, student_number)

d <- merge(df.step, d.l, by.x='id', by.y='student_number')
d <- subset(d, !is.na(percent) & !is.na(level) & !is.na(grade))


make_step_bench_plot_by_school <- function(d, title.string) {
  ggplot(subset(d, test_name == 'B3'), aes(x=factor(level), y=percent))+
  #geom_boxplot()+
  geom_point(alpha=.5, shape=20, aes(color=achievement_level), position = position_jitter(width = .2))+
  scale_y_continuous(labels=percent)+
  scale_color_manual(values=alPalette)+
  labs(x='STEP',
    y='Percent Correct on DCI Test',
    title= paste0(long_labeller("school", s), ' 2014-15 B3 Performance by W3 STEP Level')
  )+
  theme_bw()+
  facet_grid(subject ~ grade)
}

for(s in schools){
  ds <- subset(d, school==s)
  p <- make_step_bench_plot_by_school(ds,s)
  save_plot_as_pdf(p, paste0('2014-15 B3 and STEP W3 Performance, ',s))
}

