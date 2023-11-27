userInput = function() {
  
  itemNum = as.integer(readline(prompt = "How many stocks to compare? (1-6 only): "))
  
  #Validate input
  while (!(is.numeric(itemNum) && itemNum >= 1 && itemNum <= 6)) {
    cat("Invalid input. Please enter a number between 1 and 6.\n")
    itemNum <- as.integer(readline(prompt = "How many stocks to compare? (1-6 only): "))
  }
  
  items = character(itemNum)  #initialize a vector
  
  # loop through items to get the amount of stock to compare
  for (i in 1:itemNum) {
    items[i] <- readline(prompt = paste("Enter stock", i, "name: "))
  }
  
  # Get user input for date and validate
  dateInput = readline(prompt = "What is the date you would like to search back to? (YYYY/MM/DD): ")
  while (!grepl("^\\d{4}/\\d{2}/\\d{2}$", dateInput)) {
    cat("Invalid date format. Please enter a date in the format YYYY/MM/DD.\n")
    dateInput <- readline(prompt = "What is the date you would like to search back to? (YYYY/MM/DD): ")
  }
  
  #Convert date 
  timeSet = as.Date(dateInput, format = "%Y/%m/%d")
  
  return(list(items = items, timeSet = timeSet))
}

#scrape financial data from Yahoo Finance
scrapper = function(item, timeSet) {
  base_url = "https://finance.yahoo.com/quote/"
  data_list = list()
  data = data.frame()
  
  for (i in 1:length(item)) {
    #cannot just use numeric Dates, had to use POSIXct 
    start_date = as.numeric(as.POSIXct(timeSet))
    end_date = as.numeric(as.POSIXct(Sys.Date()))
    
    #create the url call
    url = paste0(base_url, item[i], "/history?period1=", start_date, "&period2=", end_date, "&interval=1d&filter=history&frequency=1d&includeAdjustedClose=true")
    print(url)
    #html data from the loaded page
    page = read_html(url)
    
    #extract and clean data using pipe operator f1 -> f2 -> f3 = table_data
    table_data = page %>%
      html_nodes("table") %>%
      html_table(header = TRUE, fill = TRUE)
    
    #make sure the table isn't empty
    if (length(table_data) > 0) {
      #Clean the data by selecting only the required columns
      table_data_clean = table_data[[1]] %>%
        filter(!grepl("\\*", Date)) %>%
        select(Date, Open, High, Low, `Close*`, `Adj Close**`, Volume)
      
      # Store the cleaned data frame in the list
      data_list[[item[i]]] = table_data_clean
      #Append table
      data = bind_rows(data, table_data_clean)
    } else {
      cat("Table not found for", item[i], "\n")
    }
  }
  
  return(data_list)
}

#general time series line graph 
graph <- function(df, y) {
  
  ggplot(df, aes(x = Date, y = !!sym(y),color=Stock)) +
    geom_line() +  
    labs(title = paste("Security -", y),
         x = "Date",
         y = y) +
    theme_economist()
}

#Tree map diagram histogram = function(df){
treeMap = function(df, y) {
  # Find the minimum and maximum dates
  minDate = min(df$Date)
  maxDate = max(df$Date)
  
  # Calculate the change in y values and the percentage change
  df = df %>%
    group_by(Stock) %>%
    summarize(yChange = (max(!!sym(y)) - min(!!sym(y))),
              PercentChange = (max(!!sym(y)) - min(!!sym(y))) / abs(min(!!sym(y))) * 100,
              Direction = ifelse(max(!!sym(y)) > (min(!!sym(y)) + max(!!sym(y))) / 2, "Up", "Down"))
  
  ggplot(df, aes(area = PercentChange, fill = Direction, label = paste0(Stock, "\n", sprintf("%.2f%%", PercentChange)))) +
    treemapify::geom_treemap() +
    geom_treemap_text(place = "centre", size = 12) +
    scale_fill_manual(values = c("Up" = "green", "Down" = "red")) +  
    labs(title = paste("Movement of", y, "Direction for timeframe", minDate, maxDate))
}
#histogram of the data
#The geom_histogram uses sturges rule as a means to calculate bin size
#since it is a dynmaic app, I figured it is good enough to get the point across
histogram = function(df,y){
  ggplot(df,aes(x=!!sym(y),color=Stock))+
    geom_histogram(fill="white", alpha=0.3, position="identity")+
    ggtitle(paste("Histogram of ", y))
}

violinPlot = function(df,y){
  ggplot(df, aes(x = factor(Stock), y = !!sym(y), fill = Stock)) +
    geom_violin(trim = FALSE, scale = "width", alpha = 0.8) +
    geom_boxplot(width = 0.2, fill = "white", color = "black", outlier.shape = NA) +
    labs(title = "Violin Plot of Security Closing Prices",
         x = "Security",
         y = "Closing Price")+
    theme_minimal()
  
}

#This was a very difficult chart to make, I followed this turtorial and added some of the options in
#the tutorial 
candleStick = function(df, statType) {
  
  switch(statType,
         "none" = {
           ggplot(df, aes(x = Date, y = `Close*`, group = Stock)) +
             geom_candlestick(aes(open = Open, high = High, low = Low, close = `Close*`)) +
             labs(title = "Securities Candlestick Chart", y = "Closing Price", x = "") +
             facet_wrap(~ Stock, ncol = 2, scales = "free_y") +
             theme_tq()
         },
         "VWAP" = {
           
           
           ggplot(df, aes(x = Date, y = `Close*`, volume = volume, group = Stock)) +
             geom_candlestick(aes(open = Open, high = High, low = Low, close = `Close*`)) +
             geom_ma(aes(group = Stock), ma_fun = VWAP, wilder = TRUE, linetype = 5) +
             labs(title = "Securities Candlestick Chart", 
                  subtitle = "Volume Weighted Moving Averages (n is default period)", 
                  y = "Closing Price", x = "") + 
             facet_wrap(~ Stock, ncol = 2, scales = "free_y") + 
             theme_tq()
         },
         "SMA" = {
           ggplot(df, aes(x = Date, y = `Close*`, group = Stock)) +
             geom_candlestick(aes(open = Open, high = High, low = Low, close = `Close*`)) +
             geom_ma(aes(group = Stock), ma_fun = SMA,  color = "darkblue", size = 1) +
             labs(title = "Securities Candlestick Chart", 
                  subtitle = "With Simple Moving Averages (SMA) (n is default period)",
                  y = "Closing Price", x = "") + 
             facet_wrap(~ Stock, ncol = 2, scale = "free_y") +
             theme_tq()
         },
         "bollingerbands" = {
           
           ggplot(df, aes(x = Date, y = `Close*`, 
                          open = Open, high = High, low = Low, close = `Close*`, 
                          group = Stock)) +
             geom_barchart() +
             geom_bbands(aes(group = Stock), ma_fun = SMA, sd = 2,  linetype = 5) +
             labs(title = "Securities Candlestick Chart", 
                  subtitle = "BBands with SMA Applied. (n default, 2 standard deviations)", 
                  y = "Closing Price", x = "") + 
             facet_wrap(~ Stock, ncol = 2, scales = "free_y") + 
             theme_tq()
         },
         stop("Invalid statType. Please choose one of: none, VWAP, SMA, bollingerbands")
  )
}

cleanData = function(df){
  #need to fix types in our dataframe, Volume and Date were previously Character types
  df$Volume = as.numeric(gsub(",", "", df$Volume))
  df$Date = as.Date(df$Date, format="%b%d,%Y")
  df$`Close*` = as.numeric(df$`Close*`)
  df$Open = as.numeric(df$Open)
  df$High = as.numeric(df$High)
  df$Low = as.numeric(df$Low)
  df$`Adj Close**`= as.numeric(df$`Adj Close**`)
  na.omit(df)
  return(df)
}

avgTotalGains= function(df) {
  data = df[order(df$Stock,df$Date)]
  data$Gain = c(0, diff(data$Close))
  avgGains = tapply(data$Daily_Gain, data$Stock, mean, na.rm = TRUE)
  return(avgGains)
}

createTable = function(df) {
  stockNames= unique(df$Stock)
  
  # Reshape the data using spread, brings the rows to the columns by stock and Close values
  df2= spread(df, key = Stock, value = `Close*`)
  df2 = df2 %>% select(-Open, -High, -Low, -`Adj Close**`, -Volume)
  
  # Group by Date and summarize to handle repeated dates
  df2 = df2 %>%
    group_by(Date) %>%
    summarise_all(function(x) ifelse(all(is.na(x)), NA, na.omit(x)[1]))
  
  # Print correlation between specific stocks (e.g., msft and tsla)
  print(cor(df2[, stockNames], use = "complete.obs"))
  
  # Create a correlation matrix for all stocks
  correlationMatrix <- cor(df2[, stockNames], use = "complete.obs")
  
  # Print the correlation matrix
  cat("Correlation Matrix:\n")
  print(correlationMatrix)
  
  corrplot.mixed(correlationMatrix, lower = 'shade', upper = 'pie', order = 'hclust',
                 title = paste("Correlation between", paste(stockNames, collapse = ", ")))
}
# Example usage
# Replace 'your_dataframe' with the actual name of your data frame
# createTable(your_dataframe)



#model predictions


#main type shit
#get user input
item = userInput()
#scrape data/web crawler
scrappedData = scrapper(item$items, item$timeSet)
df = bind_rows(scrappedData, .id = "Stock")
df = as.data.frame(df)
df = cleanData(df)
print(df)
graph(df,"Close*")
treeMap(df,"Close*")
histogram(df, "Close*")
violinPlot(df,"Close*")
#make a map specifically for financial data?
candleStick(df,"bollingerbands")
createTable(df)