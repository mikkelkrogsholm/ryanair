library(magrittr)
source("functions.R")

df <- get_times(org = "CPH", dest = "VIE")
plot_df(df, max_days = 4, min_days = 2, max_amount = 500)


###




