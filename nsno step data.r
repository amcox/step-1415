update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

df <- load_latest_step_data()

# NSNO STEP Reporting
d <- df[,c(1, 5:25)]
d$hash.id <- apply(d, 1, function(r){
	digest(as.numeric(r[['id']]), algo="sha256")
})
d <- d[,c(2:23)]
write.csv(d, "ReNEW STEP data for NSNO.csv", na="", row.names=F)