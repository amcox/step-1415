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

save_plot_as_pdf(make_plot_percent_of_students_on_level(df), "STEP Percent On Level")