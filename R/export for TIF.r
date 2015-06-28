library(dplyr)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

d <- load_data_with_calculated_fields()
ds <- d %>% select(id, w1:w4, first, latest, growth)

save_df_as_csv(ds, 'step data for tif')