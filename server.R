source("helpers.R")



# TODO: Temp way to save option list
optionListFile 	<- read.csv(file = "/var/spool/chun/shiny/optionList.txt", header = TRUE)
optionList 		<- optionListFile[,'code']

option_stock_code <- sapply(optionList, function(x){
											paste0("'", str_pad(x,width=4, side="left", pad="0"), "'")
										})
option_stock_list <- paste0(option_stock_code, collapse = ",")


#drv <- dbDriver("PostgreSQL")

pool <- dbPool(
  #drv = RPostgreSQL::PostgreSQL(),
  drv = dbDriver("PostgreSQL"),
  dbname = "stock",
  host = "127.0.0.1",
  user = "postgres",
  password = "",
  maxSize = 10
  
  
)

function(input, output, session){
	
	###############################################################################################################
	###																											###
	###	Session Update Input Fields																				###
	###																											###
	###############################################################################################################
	
	# Automatic update date input on summary page to get latest page sumamry
	updateTextInput(session, "sumTab_dateInput", value = Sys.Date())
	
	# Automatic update date range input on plot tab to [Sys.Date() - 14 to Sys.Date() + 3]
	updateDateRangeInput(session, "plotTab_daterange", start = Sys.Date() - 14, end = Sys.Date() + 3)
	
	
	
	
	###############################################################################################################
	###																											###
	###	Input + Reactive value																					###
	###																											###
	###		datasetInput - get dataset based on input code and date fields										###
	###																											###
	###############################################################################################################
	
	datasetInput <- reactive({
	
		stock_code <- NA
		df_stock <- NA
		result <- NA


		if(input$plotTab_code != ""){
			
			
			#con <- dbConnect(drv, dbname = "stock",host = "127.0.0.1",user = "postgres")
			conn <- poolCheckout(pool)
			
			stock_code = str_pad(input$plotTab_code, width=4, side="left", pad="0")
			query_str <- sprintf("SELECT * from stock_price where code = '%s' order by date desc", stock_code)
			
			#df_stock <- dbGetQuery(con, query_str)
			df_stock <- dbGetQuery(conn, query_str)
			poolReturn(conn)
			
			if(!is.na(input$plotTab_daterange[1]) & !is.na(input$plotTab_daterange[2]) & input$plotTab_code != ""){
				result  <- df_stock %>% filter(date <= input$plotTab_daterange[2] & date >= input$plotTab_daterange[1])
			}
			else{
				result <- df_stock
			}
			
			
			# dbDisconnect(con)
			
		}

		result

	})
	
	###############################################################################################################
	###																											###
	###	Output - plotTab																						###
	###																											###
	###		plotTab_mainplot 	- Graphical Area to display plot, candle stick									###
	###																											###
	###		plotTab_test		- test signal radio button, signal history										###
	###		plotTab_summary 	- for display, get head for dataset												###
	###		plotTab_debug 		- Debug	purpose																	###
	###																											###
	###																											###
	###############################################################################################################

	# plotTab_mainplot Session
	
	output$plotTab_mainplot <- renderPlot({
		df_stock <- datasetInput()
		if(!is.na(df_stock)){

			df_stock.zoo <- zoo(x = df_stock[,3:8], order.by = df_stock$date)
			df_stock.zoo.ohlc <- as.quantmod.OHLC(df_stock.zoo,
								   col.names = c("open", "high",
												 "low", "close",
												 "volumn", "adj"))
			chartSeries(df_stock.zoo.ohlc, name = df_stock$code[1], theme='white')
			
			

		}
	})
	
	# plotTab_test Session
	
	output$plotTab_test <- renderPrint({
		print(paste0(input$plotTab_chooseSignal, " + ", input$plotTab_daterange[1], " + ", input$plotTab_code))

			
	})
	
	# plotTab_summary Session
	
	output$plotTab_summary <- renderPrint({
		df_stock <- datasetInput()
		head(df_stock)

	})
	
	# plotTab_debug Session	

	output$plotTab_debug <- renderPrint({
		df_stock <- datasetInput()
		if(!is.na(df_stock)){
			df_stock.zoo <- zoo(x = df_stock[,3:9], order.by = df_stock$date)
			df_stock.zoo.ohlc <- as.quantmod.OHLC(df_stock.zoo,
								   col.names = c("open", "high",
												 "low", "close",
												 "volumn", "adj", "code"))
			print(df_stock.zoo.ohlc)
			
		}
		
		
		
	})
	
	###############################################################################################################
	###																											###
	###	Output - sumTab																							###
	###																											###
	###		sumTab_dateDisplay 	- Show latest signal date														###
	###		sumTab_summary 		- Summary of today day end signals, calc strength, display hit history			###
	###		sumTab_test			- Test Session																	###
	###		sumTab_debug		- Debug Session																	###
	###																											###
	###																											###
	###############################################################################################################
	
	
	# sumTab_dateDisplay Session
	
	output$sumTab_dateDisplay <- renderText({			
		
		# Get max date from signal, then compare with input date
		input_date <- input$sumTab_dateInput
		
		tryCatch({
			assert_that(grepl("([12]\\d{3}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01]))",input_date))
			
			conn <- poolCheckout(pool)
			query_str <- sprintf("SELECT max(date) as date from stock_price")
			df_max_date <- dbGetQuery(conn, query_str)
			
			poolReturn(conn)
			
			max_date <- df_max_date$date
			
			return_signal_date <- ifelse(input_date <= max_date, as.character(input_date), as.character(max_date))
			
			paste0("Signal Date ", return_signal_date) 
			},
			error = function(e){
				print(e)
				print("Invalid Date")
			},
			finally  = {
				#if(exists("conn")) {poolReturn(conn)}
			}
		)
		

	})
	
	# # sumTab_summary Session

	output$sumTab_summary <- renderFormattable({
		

		
		# Define Column Order for elementwise matrix multiplication, otherwise it will be sorted alphabetically 
		col_order <- c('code', signal_list)
		
		
		# Input box for date 
		i_start_date <- ""
		i_end_date <- ""
		i_end_date <- input$sumTab_dateInput
		
		# Derived
		n_past_n_days <- ymd(i_end_date) - 5 * 365		# hit history within last 3 years
		
		

		# Get max date to compare with the input date
		conn <- poolCheckout(pool)
		max.date.query <- sprintf("SELECT MAX(date) as date FROM stock_price")
		df.max.date <- dbGetQuery(conn, max.date.query)
		
		if(exists("df.max.date")){max_date <- df.max.date$date}
		
		i_end_date <- ifelse(i_end_date <= max_date, as.character(i_end_date), as.character(max_date))
		i_start_date <- as.character(as.Date(i_end_date,"%Y-%m-%d") - 14)
		
		# get data within date range, signal strength, hit history, within the past 3 years
		option.data.query <- sprintf("SELECT * FROM stock_price WHERE code IN (%s) AND date >= '%s' AND date <= '%s' order by date desc", option_stock_list, i_start_date, i_end_date)
		option.data.signalStrength.query <- sprintf("SELECT * FROM signal_strength WHERE code IN (%s) order by code asc", option_stock_list)
		signal.hit.query <- sprintf("select * FROM signal_hit WHERE code IN (%s) AND date >= '%s';", option_stock_list, n_past_n_days)
		

		df.option <- dbGetQuery(conn, option.data.query)
		df.option.signalStrength <- dbGetQuery(conn, option.data.signalStrength.query)
		df.option.signalHit <- dbGetQuery(conn, signal.hit.query)
		poolReturn(conn)

		
		df.option <- df.option %>% arrange(code, desc(date))
		
		
		# Calculate signal for the latest date
		df.option.signal <- getSignal(df.option)
		df.option.signal <- df.option.signal %>% arrange(code, desc(date)) %>% group_by_(~ code) %>% do(head(., n = 1)) %>% ungroup()
		df.option.signal <- df.option.signal %>% filter(date == i_end_date)
		df.option.signal <- df.option.signal[, c('date', col_order)] %>% arrange(code)
		
		# Get Signal Strength for each code in option list
		df.option.signalStrength.original <- df.option.signalStrength %>% select(-id, -value_recent)
		df.option.signalStrength <- dcast(df.option.signalStrength.original, code ~ signal, value.var = c("value_all"))
		df.option.signalStrength <- df.option.signalStrength[, col_order] %>% arrange(code)
		
		# Signal Strength for 1880 missing, find intersect
		codeIntersect <- intersect(df.option.signalStrength$code, df.option.signal$code)
		df.option.signalStrength <- df.option.signalStrength %>% filter(code %in% codeIntersect)
		
		# Elementwise Multiplication 
		df.option.signalStrength.onlySignal <- df.option.signalStrength %>% select(-code)
		df.option.signal.onlySignal 		<- df.option.signal 		%>% select(-date,-code)
		signal.matrix <- df.option.signalStrength.onlySignal * df.option.signal.onlySignal
		
		
		# get hit history summary
		df.option.signalHit.history <- merge(x = df.option.signalHit, y = df.option.signalStrength.original, by = c("code", "signal"), all.x = TRUE)
		df.option.signalHit.history <- df.option.signalHit.history %>% mutate(hit_value = ifelse(value_all >= 0, max_high, ifelse(value_all < 0, min_low, 0)),
																			hit_good = ifelse((hit_value >= 0.03 | hit_value <= -0.03),1,0),
																			hit_good_value = hit_good * hit_value
																			)
		
		
		df.option.signalHit.history.eval <- df.option.signalHit.history %>% 
												dplyr::group_by(code, signal,value_all) %>% 
												dplyr::summarize(n_hit = sum(hit), 
																n_good_hit = sum(hit_good), 
																hit_summary = sprintf("%s (%s)", n_good_hit, n_hit), 
																hit_median = sprintf("%1.1f%%",100 * mean(hit_good_value[hit_good_value != 0], na.rm = TRUE))) %>% 
																ungroup()
		
		
		# Append date, code column back to final df
		df.result.wide 	<- cbind(date = df.option.signal$date, code = df.option.signal$code, signal.matrix)
		df.result.long 	<- melt(df.result.wide, id.vars=c("date", "code"), variable.name="signal", value.name="value") %>% arrange(code)
		df.result 		<- df.result.long %>% select(-date) %>% filter(value != 0)
		df.result		<- df.result %>% mutate(check = ifelse(value >=90 | value <= -90, TRUE, FALSE))
		
		# merge df.result with hit history
		df.option.signalHit.history.eval <- df.option.signalHit.history.eval %>% select(code, signal, hit_summary, hit_median)
		
		df.result <- merge(x = df.result, y = df.option.signalHit.history.eval, by = c("code", "signal"), all.x = TRUE)
		
		
		# Map eng signal to chinese name + Reorder column for display
		signal_mapping <- data.frame(eng = signal_list, chi = signal_list_chinese)
		df.result$signal <- with(signal_mapping, chi[match(df.result$signal, eng)])
		df.result <- df.result %>% select(code, signal, value, hit_summary, hit_median, check)
		
		
		formattable(df.result,list(
			code 	= formatter("span",
							  style = ~ ifelse(check == TRUE, style(color = "black", font.weight = "bold"), style(color = "lightgrey"))),
			signal 	= formatter("span",
							  style = ~ ifelse(check == TRUE, style(color = "black", font.weight = "bold", "font-family" = "cwTeXHei"), style(color = "lightgrey"))),				  
			value 	= formatter("span",
								style = x ~ style(color = ifelse(x >= 90, "green", ifelse(x<=-90, "red", "lightgrey"))),
								x ~ sprintf("%.2f", x)),
			check 	= formatter("span", 
							   style = x ~ style(color = ifelse(x, "green", "lightgrey")),
							   x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
			hit_summary 	= formatter("span", 
							  style = ~ ifelse(check == TRUE, style(color = "black", font.weight = "bold", "font-family" = "cwTeXHei"), style(color = "lightgrey"))),
			hit_median 		= formatter("span", 
							  style = ~ ifelse(check == TRUE, style(color = "black", font.weight = "bold", "font-family" = "cwTeXHei"), style(color = "lightgrey")))
		))
		
		
		
	

		
	})
	
	# output$sumTab_test Session
	
	output$sumTab_test <- renderText({
		
		

	})
	
	# output$sumTab_debug Session
	
	output$sumTab_debug <- renderText({
		
		# result <- observeEvent(input$sumTab_dateInput[1],{print(input$sumTab_dateInput[1])})
		i_end_date <- input$sumTab_dateInput
		i_start_date <- "2010"
		paste("input$date is", as.character(as.Date(i_end_date,"%Y-%m-%d")))
	})
	
	
	
	###############################################################################################################
	###																											###
	###	Output - aboutTab																						###
	###																											###
	###		about 	- Include html page to briefly describe the project											###
	###																											###
	###############################################################################################################
	
	output$aboutTab_about <- renderPrint({

		print("About the project")

	})
	
	###############################################################################################################
	###																											###
	###	Output - Text + dummy page																				###
	###																											###
	###		formattableExample 	- Test out formattable, table formatting										###
	###																											###
	###############################################################################################################
	
	output$formattableExample <- renderFormattable({
		df <- data.frame(
				  id = 1:10,
				  name = c("Bob", "Ashley", "James", "David", "Jenny", 
						   "Hans", "Leo", "John", "Emily", "Lee"), 
				  age = c(28, 27, 30, 28, 29, 29, 27, 27, 31, 30),
				  grade = c("C", "A", "A", "C", "B", "B", "B", "A", "C", "C"),
				  test1_score = c(8.9, 9.5, 9.6, 8.9, 9.1, 9.3, 9.3, 9.9, 8.5, 8.6),
				  test2_score = c(9.1, 9.1, 9.2, 9.1, 8.9, 8.5, 9.2, 9.3, 9.1, 8.8),
				  final_score = c(9, 9.3, 9.4, 9, 9, 8.9, 9.25, 9.6, 8.8, 8.7),
				  registered = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
				  stringsAsFactors = FALSE)
	

		formattable(df, list(
			age = color_tile("white", "orange"),
			grade = formatter("span",
							  style = x ~ ifelse(x == "A", style(color = "green", font.weight = "bold"), NA)),
			test1_score = color_bar("pink"),
			test2_score = color_bar("pink"),
			final_score = formatter("span",
									style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
									x ~ sprintf("%.2f (rank: %02d)", x, rank(-x))),
			registered = formatter("span", 
								   style = x ~ style(color = ifelse(x, "green", "red")),
								   x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))
		))


	})
	


}