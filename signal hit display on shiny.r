source("/var/spool/chun/shiny/helpers.R")
source("/var/spool/chun/shiny/util.R")

i_date <- "2015-01-01"

optionListFile 	<- read.csv(file = "/var/spool/chun/shiny/optionList.txt", header = TRUE)
optionList 		<- optionListFile[,'code']

option_stock_code <- sapply(optionList, function(x){
											paste0("'", str_pad(x,width=4, side="left", pad="0"), "'")
										})
option_stock_list <- paste0(option_stock_code, collapse = ",")


drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "stock",host = "127.0.0.1",user = "postgres")

signal.hit.query <- sprintf("select * from signal_hit where code in (%s) and date >= '%s';", option_stock_list, i_date)
query_signalStrength <- sprintf("select code, signal, value_all from signal_strength where code in (%s) order by code asc;", option_stock_list)

df.option.signalHit <- dbGetQuery(con, signal.hit.query)
df.option.signalStrength <- dbGetQuery(con, query_signalStrength)

dbDisconnect(con)

# Merge signal strength, get 
df.option.signalHit.history <- merge(x = df.option.signalHit, y = df.option.signalStrength, by = c("code", "signal"), all.x = TRUE)
df.option.signalHit.history <- df.option.signalHit.history %>% mutate(hit_value = ifelse(value_all >= 0, max_high, ifelse(value_all < 0, min_low, 0)))
df.option.signalHit.history <- df.option.signalHit.history %>% mutate(hit_good = ifelse((hit_value >= 0.03 | hit_value <= -0.03),1,0))
df.option.signalHit.history <- df.option.signalHit.history %>% mutate(hit_good_value = hit_good * hit_value) 

# summarize 
df.option.signalHit.history.eval <- df.option.signalHit.history %>% 
										dplyr::group_by(code, signal,value_all) %>% 
										dplyr::summarize(n_hit = sum(hit), 
														n_good_hit = sum(hit_good), 
														hit_summary = sprintf("%s (%s)", n_good_hit, n_hit), 
														hit_median = median(hit_good_value[hit_good_value != 0], na.rm = TRUE)) %>% 
										ungroup()

										
