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
df.step <- subset(df.step, wave == 1 & !is.na(level))
df.step$level[df.step$level == 'FP'] <- 13

d.l <- load_leap_data()
d.l <- select(d.l, subject:scaled_score, percent, on_level, test_name, student_number)

d <- merge(df.step, d.l, by.x='id', by.y='student_number')
d <- subset(d, !is.na(percent) & !is.na(level) & !is.na(grade))

p <- ggplot(subset(d, test_name == 'B1'), aes(x=factor(level), y=percent))+
  geom_boxplot()+
  scale_y_continuous(labels=percent)+
  labs(x='STEP',
    y='Percent Correct on DCI Test',
    title='Benchmark 1 Performance by W1 STEP Level'
  )+
  theme_bw()+
  facet_grid(subject ~ grade)
save_plot_as_pdf(p, '2014-15 B1 and STEP W1 Performance')