library(dplyr)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

df <- load_data_with_gaps_long()
df$wave <- paste0('w', df$wave)
df <- df[, c('wave', 'id', 'level')]
df$level[df$level=='FP'] <- 13
df$level <- as.numeric(df$level)

dw <- dcast(df, id ~ wave)

dw <- create_w_first_last_cols(dw)
dw$first.last.growth <- dw$last - dw$first

save_df_as_csv(dw, 'step data for tif')