library(dplyr)
library(lubridate)
library(stringr)
library(quantmod)
library(RPostgreSQL)

source("helpers.R")

# Temp way to save 
optionListFile 	<- read.csv(file = "/var/spool/chun/shiny/optionList.txt", header = TRUE)
optionList 		<- optionListFile[,'code']


drv <- dbDriver("PostgreSQL")

function(input, output, session){
	datasetInput <- reactive({
		stock_code <- NA
		df_stock <- NA
		result <- NA


		if(input$i_code != ""){
			
			
			con <- dbConnect(drv, dbname = "stock",host = "127.0.0.1",user = "postgres")
		
			stock_code = str_pad(input$i_code, width=4, side="left", pad="0")
			query_str <- sprintf("SELECT * from stock_price where code = '%s' order by date desc", stock_code)
			
			df_stock <- dbGetQuery(con, query_str)
			
			if(!is.na(input$i_daterange[1]) & !is.na(input$i_daterange[2]) & input$i_code != ""){
				result  <- df_stock %>% filter(date <= input$i_daterange[2] & date >= input$i_daterange[1])
			}
			else{
				result <- df_stock
			}
			
			
			dbDisconnect(con)
			
		}
	

	
	

		result

	})

	output$out_mainplot <- renderPlot({
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

	output$out_summary <- renderPrint({
		df_stock <- datasetInput()
		# if(!is.na(df_stock)){
			# head(df_stock)
		# }
		# else{
			# cat("No Data")
		# }
		
		head(df_stock)

	})
	

	output$out_debug <- renderPrint({
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
	
	#Summary Tab
	output$sumTab_summary <- renderPrint({
		#print(optionList)
		
		con <- dbConnect(drv, dbname = "stock",host = "127.0.0.1",user = "postgres")
		option_stock_code <- sapply(optionList, function(x){
													paste0("'", str_pad(x,width=4, side="left", pad="0"), "'")
												})
		option_stock_list <- paste0(option_stock_code, collapse = ",")
		
		query_str <- sprintf("SELECT * from stock_price where code in (%s) and date = '%s' order by date desc", option_stock_list, '2017-09-08')
		df_optioncode <- dbGetQuery(con, query_str)
		print(query_str)
		
		dbDisconnect(con)
	})

  
}