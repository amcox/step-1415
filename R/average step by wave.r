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

df <- load_data_with_gaps_long()
df <- subset(df, school %in% schools & grade < 3)
df.goals <- load_step_goals_to_wave_and_year_just_ends(w=4, y=c(2015))

save_plot_as_pdf(make_plot_average_step(df, df.goals), "STEP Average Level")