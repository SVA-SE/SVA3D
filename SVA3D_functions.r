
# function to be able to decide the number of plots reactively
#in this case barplots for svaga
get_plot_output_list <- function(max_plots, barplot.data, weeks, weeks.to.plot) {
  # Insert plot output objects the list
  plot_output_list <- lapply(1:max_plots, function(i) {
    plotname <- paste("plot", i, sep="")
    plot_output_object <- plotlyOutput(plotname, height = 280, width = 250)
    plot_output_object <- renderPlotly({
      
      data.length = dim(barplot.data)[1]
      plot.window = (data.length-weeks.to.plot+1):data.length
      agens = colnames(barplot.data)[i]
      total.data <- barplot.data[plot.window,i,1]   #(total.data=posit.data+rpois(49,5))
      pos.data <- barplot.data[plot.window,i,2]     #(posit.data=rpois(49,5))
      neg.data <- total.data - pos.data  
      
      
      plot_ly(x = weeks[plot.window]) %>% 
        add_bars(y = pos.data,
                 name = 'Number of POSITIVE samples', type = 'scatter',
                 marker = list(color = 'red'))%>%
        add_bars(y = neg.data,
                 name = 'Number of tested samples', type = 'scatter',
                 marker = list(color = 'grey'))%>%
        layout(title = agens, barmode='stack',
               paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
               xaxis = list(title = "",
                            gridcolor = 'rgb(255,255,255)',
                            showgrid = TRUE,
                            showline = FALSE,
                            showticklabels = TRUE,
                            tickcolor = 'rgb(127,127,127)',
                            ticks = 'outside',
                            zeroline = FALSE),
               yaxis = list(title = "tests per week",
                            gridcolor = 'rgb(255,255,255)',
                            showgrid = TRUE,
                            showline = FALSE,
                            showticklabels = TRUE,
                            tickcolor = 'rgb(127,127,127)',
                            ticks = 'outside',
                            zeroline = FALSE),
               legend=list(orientation="h"))    
      
      
    })
  })
  
  do.call(tagList, plot_output_list) # needed to display properly.
  
  return(plot_output_list)
}


eliminate.swedish.the.wrong.way <- function(x){
  x<- str_replace(x,"\u00D6","O")
  x<- str_replace(x,"\u00C4","A")
  x<- str_replace(x,"\u00C5","A")
  x<- str_replace(x,"\u00F6","o")
  x<- str_replace(x,"\u00E4","a")
  x<- str_replace(x,"\u00E5","a")
  return(x)
}


fix.encoding <- function(df, originalEncoding = "latin1") {
  numCols <- ncol(df)
  df <- data.frame(df)
  for (col in 1:numCols)
  {
    if(class(df[, col]) == "character"){
      Encoding(df[, col]) <- originalEncoding
    }
    
    if(class(df[, col]) == "factor"){
      Encoding(levels(df[, col])) <- originalEncoding
    }
  }
  return(df)
}

