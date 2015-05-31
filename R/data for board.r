library(reshape2)
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

df <- load_data_with_gaps_long()
df <- subset(df, school %in% schools & grade < 3 & grade > -1)

d <- load_step_data()
d.s <- d %>% group_by(school) %>% summarize(w1.mean=mean(w1, na.rm=T),
  w2.mean=mean(w2, na.rm=T),
  w3.mean=mean(w3, na.rm=T),
  w4.mean=mean(w4, na.rm=T)
) %>% mutate(growth=w4.mean - w1.mean)

save_plot_as_pdf(make_plot_average_step(df, df.goals), "STEP Average Level")