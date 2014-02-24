library(reshape2)
library(plyr)
library(ggplot2)
library(scales)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

df <- load_data_with_calculated_fields()
df <- subset(df, school %in% schools)

save_plot_as_pdf(make_plot_average_gap(df), "STEP Average Gap")