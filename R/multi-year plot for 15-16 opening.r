library(dplyr)
library(ggplot2)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

# Load data
d <- load_all_step_data_return_long()
goals <- load_step_goals_to_wave_and_year_just_ends(w=6, y=c(2011, 2012, 2013, 2014, 2015))

# Calculate average level per school-grade-wave-year and then only take the first and last waves,
# renaming them as BOY and EOY, respectively
d.s <- subset(d, grade %in% c(0, 1, 2))
d.m <- d.s %>% group_by(school, grade, year, wave) %>% summarize(avg.level = mean(level, na.rm=T))
d.ms <- d.m %>% group_by(school, year, grade) %>% arrange(wave) %>% slice(c(1, n()))
d.ms$wave <- str_replace_all(d.ms$wave, '1', 'BOY')
d.ms$wave <- str_replace_all(d.ms$wave, '[0-9]', 'EOY')

# Rename the first and last waves of the goal data
goals.s <- subset(goals, grade %in% c(0, 1, 2))
goals.s$wave <- str_replace_all(goals.s$wave, '1', 'BOY')
goals.s$wave <- str_replace_all(goals.s$wave, '[0-9]', 'EOY')

# Make and save plot with average level at BOY and EOY for all grades and years
p <- ggplot(d.ms, aes(x=wave, y=avg.level, color=school))+
	geom_line(data=goals.s, aes(x=wave, y=level, group=year), color="blue", size=.5, linetype=2)+
	geom_line(aes(group=school), alpha=.8, size=1.5)+
	scale_color_manual(values=schools.pal, name='School')+
	scale_y_continuous(breaks=seq(-1, 10, 1))+
	scale_x_discrete(expand = c(0,0.1))+
	labs(title="STEP Average Level",
				x="Wave",
				y="Average Level in STEPs"
	)+
	theme_bw()+
	theme(axis.text.x=element_text(angle=90, vjust=0.5, size=8),
				axis.text.y=element_text(size=8),
				axis.title.x=element_text(size=9),
				axis.title.y=element_text(size=9),
				title=element_text(size=10)
	)+
	facet_grid(grade ~ year)
save_plot_as_pdf(p, 'Average STEP Level for All Grades, All Years')

# Save summary table with average level
save_df_as_csv(d.ms, 'Average STEP Level for All Grades, All Years')

# Make and save summary table with percent on-level
g <- goals
names(g)[names(g) == 'level'] <- 'goal.level'
d.wg <- merge(d.s, g, all.x=T)
d.wg <- d.wg %>% mutate(gap = level - goal.level)
d.wg <- subset(d.wg, !is.na(gap))
d.sum <- d.wg %>% group_by(school, grade, year, wave) %>% summarize(perc.on.level = mean(gap >= 0, na.rm=T))
d.sum$wave <- str_replace_all(d.sum$wave, '1', 'BOY')
d.sum$wave <- str_replace_all(d.sum$wave, '[0-9]', 'EOY')

save_df_as_csv(d.sum, 'STEP Percent On-Level for All Grades, All Years')