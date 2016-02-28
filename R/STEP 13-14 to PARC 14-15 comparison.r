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

df.step <- load_data_with_gaps_long(y=2014)
df.step <- subset(df.step, wave == 4 & !is.na(level) & grade == 2)
df.step$level[df.step$level == 'FP'] <- 13
df.step <- df.step %>% rename(school.step = school)
# Save a version of the STEP df with levels not as factors
df.step.num <- df.step
df.step$level <- factor(df.step$level)

dl <- load_parc_data_15()
dl.al <- dl %>% select(id, school, ela.al, math.al, sci.al, soc.al) %>%
	gather(subject, al, -(id:school)) %>%
	separate(subject, into = c("subject", "del"), sep = "\\.") %>%
	select(-del)
dl.pts <- dl %>% select(id, ela.pts, math.pts, sci.pts, soc.pts) %>%
	gather(subject, pts, -id) %>%
	separate(subject, into = c("subject", "del"), sep = "\\.") %>%
	select(-del)
dl.ss <- dl %>% select(id, ela.ss, math.ss) %>%
	gather(subject, ss, -id) %>%
	separate(subject, into = c("subject", "del"), sep = "\\.") %>%
	select(-del)
dl.comb <- merge(dl.al, merge(dl.pts, dl.ss)) %>% rename(school.parc = school)

d <- merge(df.step, dl.comb)
# Make a version of with STEP not as factors
d.num <- merge(df.step.num, dl.comb)

cuts <- load_parc_cuts_15()
	
# Made plot basics
p <- ggplot(d, aes(x=level, y=ss))+
	geom_point(alpha = 0.3, position = position_jitter(width = .2, height = 0))+
	geom_hline(data=cuts, aes(yintercept=cut))+
  labs(title="2013-14 2nd Grade STEP Predicting\nPerformance on 2014-15 3rd Grade PARC",
        x="2013-14 Wave 4 STEP, 2nd Grade",
        y="2014-15 PARC 3rd Scaled Score"
  )+
	theme_bw()+
	facet_wrap(~subject)

# Version with smoothing line
p.smooth <- p + geom_smooth(data = d %>% mutate(as.numeric(level)), aes(group = 1))
save_plot_as_pdf(p.smooth, '13-14 2nd STEP vs 2014-15 3rd PARC (smooth)')
# Version with box plots
p.box <- p + geom_boxplot(outlier.size = 0, alpha = 0.3)
save_plot_as_pdf(p.box, '13-14 2nd STEP vs 2014-15 3rd PARC (box)')

# Threshold plots
calc_basic_and_mastery_threshes <- function(d){
	find_percent_basic <- function(cut, data){
		mean(data[data$level >= cut,]$al %in% c('B', 'M', 'A'), na.rm=TRUE)
	}
	find_percent_mast <- function(cut, data){
		mean(data[data$level >= cut,]$al %in% c('M', 'A'), na.rm=TRUE)
	}
	step.levels <- unique(d$level)
	basic.percs <- sapply(step.levels, find_percent_basic, data=d)
	mast.percs <- sapply(step.levels, find_percent_mast, data=d)
	d.m <- data.frame(step.level=step.levels, perc=mast.percs, al=rep('M', length(mast.percs)))
	d.b <- data.frame(step.level=step.levels, perc=basic.percs, al=rep('B', length(basic.percs)))
	rbind(d.m, d.b)
}

make_thresh_plot <- function(d, title) {
	ggplot(threshes, aes(x=step.level, y=perc, color=al))+
		geom_point()+
	  scale_y_continuous(breaks=seq(0,1,.05), label=percent)+
	  scale_x_continuous(breaks=seq(-1,13,1))+
		scale_color_discrete(name="Achievement\nLevel",
		  breaks=c("B", "M"),
		  labels=c("Basic", "Mastery")
		)+
	  labs(title=title,
	        x="STEP Level on Wave 4 2nd Grade STEP",
	        y="Percent of Students at or Above the STEP Level in 2nd Grade\nthat Scored at or Above the Achievement Level on 3rd Grade PARC"
	  )+
		theme_bw()
}

threshes <- d.num %>% subset(subject == 'ela') %>% do(calc_basic_and_mastery_threshes(.))
p <- make_thresh_plot(threshes, '2013-14 2nd Grade STEP to 2014-15 3rd Grade PARC ELA')
save_plot_as_pdf(p, 'Thresholds for 13-14 2nd STEP to 14-15 3rd PARC, ELA')

threshes <- d.num %>% subset(subject == 'math') %>% do(calc_basic_and_mastery_threshes(.))
p <- make_thresh_plot(threshes, '2013-14 2nd Grade STEP to 2014-15 3rd Grade PARC Math')
save_plot_as_pdf(p, 'Thresholds for 13-14 2nd STEP to 14-15 3rd PARC, Math')