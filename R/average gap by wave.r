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

df <- load_data_with_gaps_long()
df <- subset(df, school %in% schools & grade < 3)

p <- make_plot_average_gap(df)
save_plot_as_pdf(p, "STEP Average Gap")