
library(shiny)
library(data.table)
library(mongolite)

ui <- fluidPage(

    
    titlePanel("reprex for mongodb connection"),

    
    sidebarLayout(
        sidebarPanel(
          fileInput("select","select",buttonLabel = "select", multiple = T, accept = ".txt"),
          actionButton("save", "save"),
            
        ),

       
        mainPanel(
          uiOutput("files")
           
        )
    )
)


server <- function(input, output) {
  
  databaseName<-"swahili_insha"
  collectionName<-"all_texts"
  
  
  # Function to save data to the database
  saveData <- function(data) {
    # Connect to the database
    db <- mongo(collection = collectionName, db=databaseName, 
                url = "mongodb+srv://kokonyaedwin:<passsword>@insha.bkrf4yr.mongodb.net/?retryWrites=true&w=majority")
    
    # Insert the data into the mongo collection as a data.frame
    data <- as.data.frame(data)
    db$insert(data)
  }
  # how to load data from the database into the shinyapp
  loadData <- function() {
    # Connect to the database
    db <- mongo(collection = collectionName, db=databaseName,
                url="mongodb+srv://kokonyaedwin:<passsword>@insha.bkrf4yr.mongodb.net/")
    # Read all the entries
    data_output <- db$find()
    data_output
  }
  
  
  output$files<-renderUI({
    observe(input$save,{
      if(is.null(input$select)){return()}
      dt<-input$select
      saveData(dt)
    })
    tb<-loadData()
    table<-data.table(tb)
    
    return(table)
  })
  

    
}

# Run the application 
shinyApp(ui = ui, server = server)
