# load data ----
load(paste0(sourcefiles,"/SVA3d/AMR/AMR_total.RData"))

# res1 ----
res1 <- reactive({
  load(paste0(sourcefiles,"/SVA3d/AMR/AMR_total.RData"))
  
  #res <- fix.encoding(res)
  columns.res <- eliminate.swedish.the.wrong.way(colnames(res))
  colnames(res) <- columns.res
  
  
  res1 <- res[(
    (res$source%in%input$source)&
    (res$year%in%input$year)&
    # (res$Djurslag%in%input$species_AMR)&
    #   (res$Material%in%input$material)&
    #   (res$Bakterienamn%in%input$bacteria)&
    #   (res$Undersokning%in%input$test)&
      (res$Utsvarat%in%input$utsvarat)
  ),]
  
  return(res1)
})


#choose year ----
output$year <- renderUI({
  checkboxGroupInput("year", "YEAR", 
               choices = as.list(sort(unique(res$year))),
               selected = as.list(sort(unique(res$year)))     )
  
})

#choose species ----
output$species_AMR <- renderUI({
  
  list.species <- unique(c(base.species,sort(unique(res1()$Djurslag))))
  
  checkboxGroupInput("species_AMR", "Djurslag", 
                     choices = as.list(list.species),
                     #selected=as.list(sort(unique(res.display()$Djurslag))))
                      selected = if ((input$select_species == 0)|(input$select_species%%2 == 0)){
                        as.list(sort(unique(res1()$Djurslag)))
                      }else{selected=character(0)})
})




#res2 ----

res2 <- reactive({
  
  
  res2 <- res1()
  
  res2 <- res2[(
    (res2$Djurslag%in%input$species_AMR)
  ),]
  
  return(res2)
})



#choose material ----
output$material <- renderUI({
  checkboxGroupInput("material", "Material",
                     choices = as.list(sort(unique(res2()$Material))),
                     selected = if ((input$select_material == 0)|(input$select_material%%2 == 0)){
                       as.list(sort(unique(res2()$Material)))
                     }else{selected=character(0)})

})

#choose bacteria ----
output$bacteria <- renderUI({
  checkboxGroupInput("bacteria", "Bakterienamn",
                     choices = as.list(sort(unique(res2()$Bakterienamn))),
                     selected = if ((input$select_bacteria == 0)|(input$select_bacteria%%2 == 0)){
                       as.list(sort(unique(res2()$Bakterienamn)))
                     }else{selected=character(0)})

})

#choose test ----
output$test <- renderUI({
  checkboxGroupInput("test", "Undersokning",
                     choices = as.list(sort(unique(res2()$Undersokning))),
                     selected = if ((input$select_test == 0)|(input$select_test%%2 == 0)){
                       as.list(sort(unique(res2()$Undersokning)))
                     }else{selected=character(0)})

})


#res3 ----

res3 <- reactive({
  
  
  res3 <- res2()
  
  res3 <- res3[(
       
         (res3$Material%in%input$material)&
         (res3$Bakterienamn%in%input$bacteria)&
         (res3$Undersokning%in%input$test)
  ),]
  
  res3 <- res3[, colSums(!is.na(res3)) != 0]
  
  for (c in 1:dim(res3)[2]){
  res3[,c]<- as.character(res3[,c])
  res3[which(is.na(res3[,c])),c]<- ""
  }
  
  return(res3)
})


# output table ----

output$columns.table_AMR <- renderUI({
 # exclude.columns=c("ab.nummer","cefinas","esbl.a.pcr","esbl.m.pcr","kvot.caz.och.caz.c","kvot.ctx.och.ctx.c","maldi.tof","penicillinatest","staf.maldi","tmsz.fran.biomic")
checkboxGroupInput(inputId="columns.table_AMR", label="Select columns to display",
                   choices=colnames(res3()),
                   selected=as.list(colnames(res3())[!(colnames(res3())%in%c("ab.nummer","cefinas","esbl.a.pcr","esbl.m.pcr","kvot.caz.och.caz.c","kvot.ctx.och.ctx.c","maldi.tof","penicillinatest","staf.maldi","tmsz.fran.biomic"))],
                   inline = TRUE))
})


output$table_AMR <- DT::renderDataTable(DT::datatable(rownames= FALSE,{
  req(input$table.go_AMR)
  
  data <- res3()

  data[,input$columns.table_AMR, drop = FALSE]
})
%>%
  DT::formatStyle(columns = input$columns.table_AMR, fontSize = '80%')
)#%>% withSpinner()

#})

#download table ----

output$downloadData <- downloadHandler(
  filename = function() {
    paste0("resistentsrapport-",Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv2(res3()[,input$columns.table_AMR, drop = FALSE], file, row.names = FALSE)
  }
)



# MIC ----





# code to make links between tabs ----

# When we change from one `tabPanel` to another, update the URL hash
observeEvent(input$tabs_AMR, {
  
  # No work to be done if input$tabs and the hash are already the same
  if (getUrlHash() == input$tabs_AMR) return()
  
  # The 'push' argument is necessary so that the hash change event occurs and
  # so that the other observer is triggered.
  updateQueryString(
    paste0(getQueryString(), input$tabs_AMR),
    "push"
  )
  # Don't run the first time so as to not generate a circular dependency 
  # between the two observers
}, ignoreInit = TRUE)

# When the hash changes (due to clicking on the link in the sidebar or switching
# between the `tabPanel`s), switch tabs and update an input. Note that clicking 
# another `tabPanel` already switches tabs.
observeEvent(getUrlHash(), {
  hash <- getUrlHash()
  
  # No work to be done if input$tabs and the hash are already the same
  if (hash == input$tabs_AMR) return()
  
  valid <- c("#panel_MIC_AMR", "#panel_data_AMR")
  
  if (hash %in% valid) {
    updateTabsetPanel(session, "tabs_AMR", hash)
  }
})


# troubleshooting ----
output$text1 <- renderText({
  paste0("The value of input$n is: ", colnames(res3()))
})
# output$text2 <- renderText({
#   paste0("The value of input$n is: ", input$bacteria)
# })
# output$text3 <- renderText({
#   paste0("The value of input$n is: ", input$test)
# })