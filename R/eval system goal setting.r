library(reshape2)
library(dplyr)
library(ggplot2)
library(scales)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

make_step_eval_setting_plot <- function(d, year.string) {
	ggplot(d, aes(x=percentile.ecdf, y=perc.3.plus))+
		geom_text(aes(label=round(perc.3.plus*100)), size=1.75)+
		geom_vline(x=c(0.1, 0.55, 0.9), linetype=3)+
		scale_y_continuous(limits=c(0,1), label=percent)+
		scale_x_continuous(limits=c(0,1), breaks=seq(0, 1, 0.1), label=percent)+
		labs(x="Percent of Teachers at or Below Performance Level",
			y="Percent of Students in Class\nGrowing Three or More STEPs",
			title=paste0("K-2 STEP Data, ", year.string)
		)+
		theme_bw()+
		theme(axis.title.x = element_text(size=8),
			axis.title.y = element_text(size=8),
			axis.text.x = element_text(size=7),
			axis.text.y = element_text(size=7),
			plot.title = element_text(size=9)
		)
}

# 12-13 STEP Data Eval Setting Plot
d <- load_step_data(y=2013)
d <- load_data_with_calculated_fields(y=2013)
d <- subset(d, grade < 3)

dg <- d %>% group_by(home.room) %>%
	summarize(perc.3.plus = sum(growth >= 3)/n(), n = n()) %>%
	mutate(percentile.ecdf=ecdf(perc.3.plus)(perc.3.plus)) %>%
	arrange(perc.3.plus)	

p <- make_step_eval_setting_plot(dg, '2012-13')
save_plot_as_pdf_adjustable(p, 'K-2 STEP 12-13', w=7.5, h=3)

dg$year <- rep('2013', nrow(dg))
teacher.table <- dg

# 13-14 STEP Data Eval Setting Plot
d <- load_step_data(y=2014)
d <- load_data_with_calculated_fields(y=2014)
d <- subset(d, grade < 3)

dg <- d %>% group_by(home.room) %>%
	summarize(perc.3.plus = sum(growth >= 3)/n(), n = n()) %>%
	mutate(percentile.ecdf=ecdf(perc.3.plus)(perc.3.plus)) %>%
	arrange(perc.3.plus)	

p <- make_step_eval_setting_plot(dg, '2013-14')
save_plot_as_pdf_adjustable(p, 'K-2 STEP 13-14', w=7.5, h=3)

dg$year <- rep('2014', nrow(dg))
teacher.table <- rbind(teacher.table, dg)

# 14-15 STEP Data Eval Setting Plot
d <- load_step_data(y=2015)
d <- load_data_with_calculated_fields(y=2015)
d <- subset(d, grade < 3)

dg <- d %>% group_by(home.room) %>%
	summarize(perc.3.plus = sum(growth >= 3)/n(), n = n()) %>%
	mutate(percentile.ecdf=ecdf(perc.3.plus)(perc.3.plus)) %>%
	arrange(perc.3.plus)	

p <- make_step_eval_setting_plot(dg, '2014-15')
save_plot_as_pdf_adjustable(p, 'K-2 STEP 14-15', w=7.5, h=3)

dg$year <- rep('2015', nrow(dg))
teacher.table <- rbind(teacher.table, dg)

save_df_as_csv(teacher.table, 'K-2 STEP Eval Setting Teacher Percentiles')