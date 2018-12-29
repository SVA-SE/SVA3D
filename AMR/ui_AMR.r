AMR_tab <- tabPanel("AMR-Dashboard",
                       
                    sidebarPanel(width=2,
                               
                                 checkboxGroupInput("source", "Select (one or multiple):", 
                                                    choices = list("ANT" = "ANT", "BKT" = "BKT"),
                                                    selected = list("ANT","BKT")     ),
                                 checkboxGroupInput("utsvarat", "Utsvarat:", 
                                                    choices = list("Ja" = "Ja", "Nej" = "Nej"),
                                                    selected = list("Ja","Nej")     ),
                                 
                                 uiOutput("year"),
                                 actionLink("select_species","Select/Deselect All"),
                                 uiOutput("species_AMR"),
                                 actionLink("select_material","Select/Deselect All"),
                                 uiOutput("material")
                    ),
                     sidebarPanel(width=2,
                                  actionLink("select_bacteria","Select/Deselect All"),
                                  uiOutput("bacteria"),
                                  actionLink("select_test","Select/Deselect All"),
                                  uiOutput("test")
                     ),
                     
                    mainPanel(
                      tabsetPanel(id = "tabs_AMR",
                    
                                  #TAB Summary----
                                  tabPanel("MIC", value ="#panel_MIC_AMR",
                                           #plotOutput(),
                                           #verbatimTextOutput("text1"),
                                           # verbatimTextOutput("text2"),
                                           # verbatimTextOutput("text3"),
                                           
                                           #h4("add text here"),
                                           tags$a("Go to the DATA", href = "#panel_data_AMR")
                                           #br(),
                                           #tags$a("Go to MAPS", href = "#panel_maps"),
                                  ),
                         
                         #TAB Alarm charts----
                         
                         tabPanel("Data", value ="#panel_data_AMR",
                                  
                                  
                                  fluidRow(
                                    uiOutput("columns.table_AMR")
                                    
                                  ),
                                  actionButton("table.go_AMR", "CLICK to generate table (only needed the first time, once generated, updates automatically)"),
                                  downloadButton("downloadData", "Download"),
                                  
                                  fluidRow(
                                    withSpinner(DT::dataTableOutput("table_AMR"))
                                  ),
                                  
                                  
                                  tags$a("Go to the MIC charts", href = "#panel_MIC_AMR")
                                  
                         )#end tabPanel("Data"
                         )#tabsetPanel
                    
                    
                    )#mainPanel
)#tabPanel