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

# 14-15 STEP Data Eval Setting Plot
d <- load_step_data(y=2015)
d <- load_data_with_calculated_fields(y=2015)
d <- subset(d)

dg <- d %>% group_by(home.room, school) %>%
	summarize(mean.w1.level = mean(w1, na.rm=T),
		mean.w2.level = mean(w2, na.rm=T),
		mean.w3.level = mean(w3, na.rm=T),
		mean.w4.level = mean(w4, na.rm=T)
	)

save_df_as_csv(dg, 'tfa step data')