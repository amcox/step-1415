library(reshape2)
library(dplyr)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

df.14 <- load_data_with_gaps_long(y=2014)