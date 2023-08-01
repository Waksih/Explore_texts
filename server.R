server<-function(input,output,session){
  options(shiny.debug = TRUE)
  options(shiny.error = browser)
  
  #create reactive values to store cookie information
  cookies<- reactiveValues()
  
  # Track whether the user is logged in
  isLoggedIn <- reactiveVal(FALSE)
  showSignup <- reactiveVal(FALSE)
  userRole <- reactiveVal("")
  selectedMenuItem <- reactiveVal("Homescreen")#create a reactive value to store the selected menu item
  fileSaved <- reactiveVal(FALSE)# Create a reactive value to track if a file is uploaded and saved
  values <- reactiveValues(file_upload = NULL)# Define a reactiveValues object to store the file input value
  
  
  # Show the login section and hide the signup section by default
  shinyjs::hide("signup")
  
  #Check if the user is already logged in
  observe({
    isLoggedIn(!is.null(cookies$session))
  })
  
  # Function to handle the signup button click
  observeEvent(input$signupBtn, {
    username <- input$signupUsernameInput
    password <- input$signupPasswordInput
    role <- input$roleInput
    
    if (is.null(username) || username == "") {
      showModal(modalDialog("Please enter a username.", easyClose = TRUE))
    } else if (is.null(password) || password == "") {
      showModal(modalDialog("Please enter a password.", easyClose = TRUE))
    } else {
    if (userExists(username)) {
      showModal(modalDialog("Username already exists. Please choose a different username.", easyClose = TRUE))
    } else {
      
      #set the user role
      userRole(role)
      
      # Check if the user is allowed to register as an Admin
      if (role == "Admin") {
        # Add validation to restrict Admin role to specific usernames
        allowedAdminUsernames <- c("Edwin", "Kathleen")
        if (!username %in% allowedAdminUsernames) {
          showModal(modalDialog("Sorry. You do not meet the requirements of being set as Admin.", easyClose = TRUE))
          # Reset the signup form
          updateTextInput(session, "signupUsernameInput", value = "")
          updateTextInput(session, "signupPasswordInput", value = "")
          updateRadioButtons(session, "roleInput", selected = "Standard")
          
          return()  # Exit the observe block
        }
      }
      
      
      # Insert the new user into the database
      insertUser(username, password, role)
      
      
      # Display a success message
      showModal(modalDialog("User registered successfully. You can now log in.", easyClose = TRUE))
      
      # Reset the signup form
      updateTextInput(session, "signupUsernameInput", value = "")
      updateTextInput(session, "signupPasswordInput", value = "")
      updateRadioButtons(session, "roleInput", selected = "Standard")
      
      showSignup(FALSE)
      
      session$sendCustomMessage("setInputs", list(isLoggedIn = isLoggedIn(), showSignup = showSignup()))
      
      # Switch to the login section
      shinyjs::show("login")
      shinyjs::hide("signup")
    } 
    }
  })
  
  
  # Function to handle the login button click
  observeEvent(input$loginBtn, {
    username <- input$usernameInput
    password <- input$passwordInput
    
    
    if (is.null(username) || username == "") {
      showModal(modalDialog("Please enter a username.", easyClose = TRUE))
    } else if (is.null(password) || password == "") {
      showModal(modalDialog("Please enter a password.", easyClose = TRUE))
    } else {
    if (userExists(username) && validatePassword(username, password)) {
      # Successful login
      isLoggedIn(TRUE)
      showSignup(FALSE)# Set the value accordingly
      
      # Set the user's session information
      cookies$session<- username
      
      #retrieve the user's role from the database
      role<-getUserRole(username)
      
      #update the user role
      userRole(role)
      
      #set the user session information
      session$user <-reactiveValues(username = username, role = userRole())
      
      session$sendCustomMessage("setInputs", list(isLoggedIn = isLoggedIn(), showSignup = showSignup()))
      
      
      selectedMenuItem("Homescreen")
      
      # Switch to the login section
      shinyjs::hide("login")
      
    } else {
      # Invalid login, display an error message
      showModal(modalDialog("Invalid username or password. Please try again.", easyClose = TRUE))
      
      
      # Reset the login form
      updateTextInput(session, "usernameInput", value = "")
      updateTextInput(session, "passwordInput", value = "")
    }  
    }
      
  })
  
  
  # Make the isLoggedIn reactive value available to the UI
  isLoggedInOutput <- reactive({
    isLoggedIn()
  })
  
  # Make the selectedMenuItem reactive value available to the UI
  selectedMenuItemOutput <- reactive({
    selectedMenuItem()
  })
  
  #the dashboard body content
  output$dashboardContentUI<- renderUI({
    if(isLoggedIn()){
      if (selectedMenuItem() == "Homescreen") {
        if (userRole() == "Admin") {
          div(
            id = "dashboardContent",
            tabsetPanel(
              tabPanel(title = "About", value = "hmscrn", icon = icon("address-card"), 
                       h5("Welcome, Admin! This dashboard is designed specifically for the exploration of Kiswahili texts.
                        Upload a text file and click 'save' to begin. you can generate word clouds for any file you upload to the database.
                        All you need to do is search the file you want to analyze in the files tab of the Home menu, take note of the topic it belongs, then
                        go to the topic menu where you can select the file.You can also download the 
                        word cloud in PNG format for later reference. You can perform sentiment analysis
                        including the sentiment scores for each document. and keyword seraches. If you select a different document to analyze, upload and
                        save a file or clear the table, 
                        click the update button to get the  corresponding results.")
              ),
              tabPanel(title = "Files", value = "all_files", icon = icon("address-card"), dataTableOutput("files"))
            )
          )
        } else if (userRole() == "Standard") {
          div(
            id = "dashboardContent",
            tabsetPanel(
              tabPanel(title = "About", value = "hmscrn", icon = icon("address-card"), 
                       h5("Welcome,This dashboard is designed specifically for the exploration of Kiswahili texts.
                        You can generate word clouds for any file you select.
                        Start by searching the file you want to analyze in the files tab of the Home menu,
                        take note of the topic it belongs, then
                        go to the topic menu where you can select the file.You can also download the 
                        word cloud in PNG format for later reference. You can perform sentiment analysis
                        including the sentiment scores for each document. and keyword seraches. 
                        If you select a different document to analyze,
                        click the update button to get the  corresponding results for the selected document.")
              ),
              tabPanel(title = "Files", value = "all_files", icon = icon("address-card"), dataTableOutput("files"))
            )
          )
        }
      }
      else if (selectedMenuItem() == "Politics") {
        div(
          id = "dashboardContent",
          tabsetPanel(
            
            tabPanel("files", icon = icon("address-card"), 
                     tabsetPanel(
                       tabPanel(title = "politics", value = "politics_files",
                                fluidRow(selectInput("pol_files", "Select File", choices = NULL)),
                                fluidRow(tableOutput("politics_files"))
                       ),
                       tabPanel(title = "Text", value = "politics_text",
                                fluidRow(textOutput("politics_header")),
                                fluidRow(textOutput("politics_text"))
                       )
                     )),
            tabPanel("wordcloud", icon = icon("address-card"), plotOutput("pol_wc"),downloadButton("pol_downloadwrd", "Download_wordcloud")),
            tabPanel("sentiment analysis",icon = icon("address-card"),
                     fluidRow(tableOutput("pol_scores")),
                     fluidRow(dataTableOutput("pol_sentiments"))),
            tabPanel("key word Search", icon = icon("address-card"), 
                     fluidRow(textInput("pol_search", "search_text")),
                     fluidRow(dataTableOutput("pol_ngrams")))
          )
        )
      }
      else if (selectedMenuItem() == "Activism") {
        div(
          id = "dashboardContent",
          tabsetPanel(
            
            tabPanel("files", icon = icon("address-card"), 
                     tabsetPanel(
                       tabPanel(title = "activism", value = "activism_files",
                                fluidRow(selectInput("act_files", "Select File", choices = NULL)),
                                fluidRow(tableOutput("activism_files"))
                       ),
                       tabPanel(title = "Text", value = "activism_text",
                                fluidRow(textOutput("activism_header")),
                                fluidRow(textOutput("activism_text"))
                       )
                     )),
            tabPanel("wordcloud", icon = icon("address-card"), plotOutput("act_wc"),downloadButton("act_downloadwrd", "Download_wordcloud")),
            tabPanel("sentiment analysis",icon = icon("address-card"),
                     fluidRow(tableOutput("act_scores")),
                     fluidRow(dataTableOutput("act_sentiments"))),
            tabPanel("key word Search", icon = icon("address-card"), 
                     fluidRow(textInput("act_search", "search_text")),
                     fluidRow(dataTableOutput("act_ngrams")))
          )
        )
      }
      else if (selectedMenuItem() == "Healthcare") {
        div(
          id = "dashboardContent",
          tabsetPanel(
            
            tabPanel("files", icon = icon("address-card"), 
                     tabsetPanel(
                       tabPanel(title = "healthcare", value = "health_files",
                                fluidRow(selectInput("hlth_files", "Select File", choices = NULL)),
                                fluidRow(tableOutput("health_files"))
                       ),
                       tabPanel(title = "Text", value = "health_text",
                                fluidRow(textOutput("health_header")),
                                fluidRow(textOutput("health_text"))
                       )
                     )),
            tabPanel("wordcloud", icon = icon("address-card"), plotOutput("hlth_wc"),downloadButton("hlth_downloadwrd", "Download_wordcloud")),
            tabPanel("sentiment analysis",icon = icon("address-card"),
                     fluidRow(tableOutput("hlth_scores")),
                     fluidRow(dataTableOutput("hlth_sentiments"))),
            tabPanel("key word Search", icon = icon("address-card"), 
                     fluidRow(textInput("hlth_search", "search_text")),
                     fluidRow(dataTableOutput("hlth_ngrams")))
          )
        )
      }
      else if (selectedMenuItem() == "Technology") {
        div(
          id = "dashboardContent",
          tabsetPanel(
            
            tabPanel("files", icon = icon("address-card"), 
                     tabsetPanel(
                       tabPanel(title = "technology", value = "technology_files",
                                fluidRow(selectInput("tech_files", "Select File", choices = NULL)),
                                fluidRow(tableOutput("technology_files"))
                       ),
                       tabPanel(title = "Text", value = "technology_text",
                                fluidRow(textOutput("technology_header")),
                                fluidRow(textOutput("technology_text"))
                       )
                     )),
            tabPanel("wordcloud", icon = icon("address-card"), plotOutput("tech_wc"),downloadButton("tech_downloadwrd", "Download_wordcloud")),
            tabPanel("sentiment analysis",icon = icon("address-card"),
                     fluidRow(tableOutput("tech_scores")),
                     fluidRow(dataTableOutput("tech_sentiments"))),
            tabPanel("key word Search", icon = icon("address-card"), 
                     fluidRow(textInput("tech_search", "search_text")),
                     fluidRow(dataTableOutput("tech_ngrams")))
          )
        )
      }
      else if (selectedMenuItem() == "Agriculture") {
        div(
          id = "dashboardContent",
          tabsetPanel(
            
            tabPanel("files", icon = icon("address-card"), 
                     tabsetPanel(
                       tabPanel(title = "agriculture", value = "agriculture_files",
                                fluidRow(selectInput("agri_files", "Select File", choices = NULL)),
                                fluidRow(tableOutput("agriculture_files"))
                       ),
                       tabPanel(title = "Text", value = "agriculture_text",
                                fluidRow(textOutput("agriculture_header")),
                                fluidRow(textOutput("agriculture_text"))
                       )
                     )),
            tabPanel("wordcloud", icon = icon("address-card"), plotOutput("agri_wc"),downloadButton("agri_downloadwrd", "Download_wordcloud")),
            tabPanel("sentiment analysis",icon = icon("address-card"),
                     fluidRow(tableOutput("agri_scores")),
                     fluidRow(dataTableOutput("agri_sentiments"))),
            tabPanel("key word Search", icon = icon("address-card"), 
                     fluidRow(textInput("agri_search", "search_text")),
                     fluidRow(dataTableOutput("agri_ngrams")))
          )
        )
      }
      else if (selectedMenuItem() == "Finance") {
        div(
          id = "dashboardContent",
          tabsetPanel(
            
            tabPanel("files", icon = icon("address-card"), 
                     tabsetPanel(
                       tabPanel(title = "finance", value = "finance_files",
                                fluidRow(selectInput("fin_files", "Select File", choices = NULL)),
                                fluidRow(tableOutput("finance_files"))
                       ),
                       tabPanel(title = "Text", value = "finance_text",
                                fluidRow(textOutput("finance_header")),
                                fluidRow(textOutput("finance_text"))
                       )
                     )),
            tabPanel("wordcloud", icon = icon("address-card"), plotOutput("fin_wc"),downloadButton("fin_downloadwrd", "Download_wordcloud")),
            tabPanel("sentiment analysis",icon = icon("address-card"),
                     fluidRow(tableOutput("fin_scores")),
                     fluidRow(dataTableOutput("fin_sentiments"))),
            tabPanel("key word Search", icon = icon("address-card"), 
                     fluidRow(textInput("fin_search", "search_text")),
                     fluidRow(dataTableOutput("fin_ngrams")))
          )
        )
      }
    }
  })
  

  # Update sidebar menu based on user role
  sidebarMenuItems <- reactive({
    if (userRole() == "Admin") {
      list(
        menuItem("Home", tabName = "Homescreen", icon = icon("home")),
        fileInput("file_upload", "Upload file", buttonLabel = "Upload", multiple = TRUE, accept = c(".txt", "application/pdf")),
        actionButton("save", "Save"),
        actionButton("update", "Update"),
        menuItem("Politics", tabName = "Politics"),
        menuItem("Activism", tabName = "Activism"),
        menuItem("Healthcare", tabName = "Healthcare"),
        menuItem("Technology", tabName = "Technology"),
        menuItem("Agriculture", tabName = "Agriculture"),
        menuItem("Finance", tabName = "Finance"),
        actionButton("clear", "Clear Table")
      )
    } else if (userRole() == "Standard") {
      list(
        menuItem("Home", tabName = "Homescreen", icon = icon("home")),
        actionButton("update", "Update"),
        menuItem("Politics", tabName = "Politics"),
        menuItem("Activism", tabName = "Activism"),
        menuItem("Healthcare", tabName = "Healthcare"),
        menuItem("Technology", tabName = "Technology"),
        menuItem("Agriculture", tabName = "Agriculture"),
        menuItem("Finance", tabName = "Finance")
      )
    } else {
      list()  # Empty menu items for other roles
    }
  })
  
  # Render the sidebar menu
  output$sidebar <- renderMenu({
    if(isLoggedIn()){
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      sidebarMenu(id = "navigation", sidebarMenuItems())
    }else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      NULL
    }
   
  })
  
  
   #process the uploaded file
  observeEvent(input$save, {
    req(input$file_upload)
    
    # Get the uploaded files
    uploaded_files <- input$file_upload
    n_files<- length(uploaded_files$datapath)
    
    # Reset the file input to be blank
    shinyjs::reset("file_upload")
    
    # Validate file format
    valid_mimes <- c("text/plain")
    
    #to sort through multiple uploads
    for (i in 1:n_files) {
      uploaded_file <- uploaded_files$datapath[i]
      file_name <- uploaded_files$name[i]
      
      # Check file MIME type
      file_mime <- mime::guess_type(uploaded_file)[[1]]
      if (!(file_mime %in% valid_mimes)) {
        showModal(modalDialog("Invalid file format. Only text documents are allowed.", easyClose = TRUE))
        return()  # Exit the observe block
      }
      
      
      # Check if the file already exists in the database
      query <- glue("SELECT COUNT(*) FROM text_files WHERE file_name = '{file_name}'")
      result <- dbGetQuery(con, query)
      if (result[1, 1] > 0) {
        # File already exists, skip uploading
        next
      }
      topic <- getTopic(file_name)
      
      # Read the file and extract header and content
      file_contents <- readLines(uploaded_file)
      header <- file_contents[1]
      content <- paste(file_contents[-1], collapse = "\n")
      
      # Insert the data into the table
      query <- "INSERT INTO text_files (file_name, topic, headers, content) VALUES ($1, $2, $3, $4)"
      RPostgres::dbExecute(con, query, params = list(file_name, topic, header, content))
      
      
    }
    
    fileSaved(TRUE)
    # Switch to the Homescreen tab
    selectedMenuItem("Homescreen")
    
  })
  
  #clear the database
  observeEvent(input$clear, {
    # Display a confirmation dialog
    showModal(
      modalDialog(
        title = "Clear Uploaded Files",
        "Are you sure you want to delete all uploaded files?",
        footer = tagList(
          actionButton(
            "cancelClear",
            "Cancel",
            class = "btn-primary"
          ),
          actionButton(
            "clearConfirmButton",
            "Clear",
            class = "btn-danger"
          )
        )
      )
    )
  })
  
  # Add an observeEvent for the confirmed clear action
  observeEvent(input$clearConfirmButton, {
    if (input$clearConfirmButton) {
      # Perform the clear action
      query <- "DELETE FROM text_files"
      RPostgres::dbExecute(con, query)
      
      # Switch to the Homescreen tab
      selectedMenuItem("Homescreen")
    }
    
    # Close the modal
    removeModal()
  })
  
  # Close the modal when the "Cancel" button is clicked
  observeEvent(input$cancelClear, {
    removeModal()
  })
  
  
  
  # Create a reactive value to store the selected header
  selectedFile <- reactiveVal()
  
  # Create a reactive function to retrieve the data based on the selected file
  getFileData <- function(topic) {
    req(selectedFile())
    
    headers <- selectedFile()
    topic <- selectedMenuItem()
    
    query <- glue::glue("SELECT headers, content FROM text_files WHERE topic = '{topic}' AND headers = '{headers}'")
    files<-dbGetQuery(con, query)
    return(files)
  }
  
  # Update the select Input choices based on the selected tab and inputId
  updateFileSelectInput <- function(active_tab, inputId) {
    query <- glue::glue("SELECT headers FROM text_files WHERE topic = '{active_tab}'")
    choices <- dbGetQuery(con, query)
    updateSelectInput(session, inputId, choices = choices$headers)
    
    selectedFile(choices$headers[1]) # assign the selected file to a reactive value
  }
  
  # An observer to update the select input choices and selected file when the tab changes
  observeEvent(input$navigation, {
    active_tab <- input$navigation
    inputId <- switch(active_tab,
                      "Politics"     = "pol_files",
                      "Activism"     = "act_files",
                      "Healthcare"   = "hlth_files",
                      "Technology"   = "tech_files",
                      "Agriculture"  = "agri_files",
                      "Finance"      = "fin_files",
                      "Homescreen"   = ""
    )
    
    
    updateFileSelectInput(active_tab, inputId)
    
    selectedMenuItem(active_tab) #update the selected menu item
    
  })
  
  observeEvent(input$update, {
    if (fileSaved()) {
      selectedMenuItem("Homescreen")  # Switch to the Homescreen tab
      
      # Retrieve the files for display
      query <- "SELECT headers, topic FROM text_files"
      files <- dbGetQuery(con, query)
      
      # Update the table output in the Homescreen tab
      output$files <- renderDataTable({
        datatable(files, options = list(paging = FALSE))
      })
    }
  })
  
  
  #HOME TAB
  # Display the table of uploaded files
  output$files <- renderDataTable({
    query <- "SELECT headers, topic FROM text_files"
    RPostgres::dbGetQuery(con, query)
  }) 
  
    
  #POLITICS TAB
  #display the table of uploaded  POLITICS files 
  output$politics_files<- renderTable({
    query<- "SELECT headers, topic FROM text_files WHERE topic = 'Politics'"
    RPostgres::dbGetQuery(con, query)
  })
  # Event listener for header selection
  observeEvent(input$pol_files, {
    selectedFile(input$pol_files)
    files <- getFileData()
    
    output$politics_header<- renderText({
      req(input$pol_files)
      
      headers<- files$headers
      
      headers
    })
    
    output$politics_text<- renderText({
      req(input$pol_files)
      
      content<-files$content
      
      content
    })
    
  })
  
  
  #ACTIVISM TAB
  #display the table of uploaded  ACTIVISM files 
  output$activism_files<- renderTable({
    query<- "SELECT headers, topic FROM text_files WHERE topic = 'Activism'"
    RPostgres::dbGetQuery(con, query)
    
  })
  # Event listener for header selection
  observeEvent(input$act_files, {
    selectedFile(input$act_files)
    files <- getFileData()
    
    output$activism_header<- renderText({
      req(input$act_files)
      
      headers<- files$headers
      
      headers
    })
    
    output$activism_text<- renderText({
      req(input$act_files)
      
      content<-files$content
      
      content
    })
    
  })
  
  
  #HEALTHCARE TAB
  #display the table of uploaded HEALTH files 
  output$health_files<- renderTable({
    query<- "SELECT headers, topic FROM text_files WHERE topic = 'Healthcare'"
    RPostgres::dbGetQuery(con, query)
    
  })
  # Event listener for header selection
  observeEvent(input$hlth_files, {
    selectedFile(input$hlth_files)
    files <- getFileData()
    
    output$health_header<- renderText({
      req(input$hlth_files)
      
      headers<- files$headers
      
      headers
    })
    
    output$health_text<- renderText({
      req(input$hlth_files)
      
      content<-files$content
      
      content
    })
    
  })
  
  
  #TECHNOLOGY TAB
  #display the table of uploaded  TECHNOLOGY files 
  output$technology_files<- renderTable({
    query<- "SELECT headers, topic FROM text_files WHERE topic = 'Technology'"
    RPostgres::dbGetQuery(con, query)
    
  })
  # Event listener for header selection
  observeEvent(input$tech_files, {
    selectedFile(input$tech_files)
    files <- getFileData()
    
    output$technology_header<- renderText({
      req(input$tech_files)
      
      headers<- files$headers
      
      headers
    })
    
    output$technology_text<- renderText({
      req(input$tech_files)
      
      content<-files$content
      
      content
    })
    
  })
  
  
  #AGRICULTURE TAB
  #display the table of uploaded  AGRICULTURE files 
  output$agriculture_files<- renderTable({
    query<- "SELECT headers, topic FROM text_files WHERE topic = 'Agriculture'"
    RPostgres::dbGetQuery(con, query)
    
  })
  # Event listener for header selection
  observeEvent(input$agri_files, {
    selectedFile(input$agri_files)
    files <- getFileData()
    
    output$agriculture_header<- renderText({
      req(input$agri_files)
      
      headers<- files$headers
      
      headers
    })
    
    output$agriculture_text<- renderText({
      req(input$agri_files)
      
      content<-files$content
      
      content
    })
    
  })
  
  
  
  #FINANCE TAB
  #display the table of uploaded FINANCE files 
  output$finance_files<- renderTable({
    query<- "SELECT headers, topic FROM text_files WHERE topic = 'Finance'"
    RPostgres::dbGetQuery(con, query)
    
  })
  # Event listener for header selection
  observeEvent(input$fin_files, {
    selectedFile(input$fin_files)
    files <- getFileData()
    
    output$finance_header<- renderText({
      req(input$fin_files)
      
      headers<- files$headers
      
      headers
    })
    
    output$finance_text<- renderText({
      req(input$fin_files)
      
      content<-files$content
      
      content
    })
    
  })
  
  
    
  #WORDCLOUD PER TOPIC
  wrdcld<-reactive({
    
    input$update
    
    isolate({
      
      withProgress({
        setProgress(message = " processing corpus...")
        wc_text<-getFileData()
        
        
        wc_corpus<-Corpus(VectorSource(wc_text))
        wc_corpus_clean<-tm_map(wc_corpus, tolower)
        wc_corpus_clean<-tm_map(wc_corpus_clean, removeWords, stopword$word)
        wc_corpus_clean<-tm_map(wc_corpus_clean,removeNumbers )
        wc_corpus_clean<-tm_map(wc_corpus_clean, stripWhitespace)
        
        
      })
    })
  })
  wordcloud_rep<-repeatable(wordcloud) 
  
  #politics wordcloud
  output$pol_wc<-renderPlot({
    withProgress({
      setProgress(message = "creating wordcloud...")
      wc_corpus<-wrdcld()
      wc_tdm<-TermDocumentMatrix(wc_corpus)
      wc_matrix<-as.matrix(wc_tdm)
      wc_sort<-sort(rowSums(wc_matrix),decreasing = T)
      dtf<-data.frame(word=names(wc_sort), freq=wc_sort)
      
      wordcloud(words = dtf$word,freq=dtf$freq, min.freq=3, max.words = 200,
                colors = brewer.pal(9,"Set1"), random.order = T,rot.per = .50)
      
    })
    output$pol_downloadwrd<-downloadHandler(
      filename = function(){
        paste("pol_wc","png", sep=".")
      },
      content = function(file){
        png(file)
        wordcloud(words = dtf$word,freq=dtf$freq, min.freq=3, max.words = 200,
                  colors = brewer.pal(9,"Set1"), random.order = T,rot.per = .50)
        dev.off()
      }
    )
  })
  #activism wordcloud
  output$act_wc<-renderPlot({
    withProgress({
      setProgress(message = "creating wordcloud...")
      wc_corpus<-wrdcld()
      wc_tdm<-TermDocumentMatrix(wc_corpus)
      wc_matrix<-as.matrix(wc_tdm)
      wc_sort<-sort(rowSums(wc_matrix),decreasing = T)
      dtf<-data.frame(word=names(wc_sort), freq=wc_sort)
      
      wordcloud(words = dtf$word,freq=dtf$freq, min.freq=3, max.words = 200,
                colors = brewer.pal(9,"Set1"), random.order = T,rot.per = .50)
      
    })
    output$act_downloadwrd<-downloadHandler(
      filename = function(){
        paste("act_wc","png", sep=".")
      },
      content = function(file){
        png(file)
        wordcloud(words = dtf$word,freq=dtf$freq, min.freq=3, max.words = 200,
                  colors = brewer.pal(9,"Set1"), random.order = T,rot.per = .50)
        dev.off()
      }
    )
  })
  #healthcare wordcloud
  output$hlth_wc<-renderPlot({
    withProgress({
      setProgress(message = "creating wordcloud...")
      wc_corpus<-wrdcld()
      wc_tdm<-TermDocumentMatrix(wc_corpus)
      wc_matrix<-as.matrix(wc_tdm)
      wc_sort<-sort(rowSums(wc_matrix),decreasing = T)
      dtf<-data.frame(word=names(wc_sort), freq=wc_sort)
      
      wordcloud(words = dtf$word,freq=dtf$freq, min.freq=3, max.words = 200,
                colors = brewer.pal(9,"Set1"), random.order = T,rot.per = .50)
      
    })
    output$hlth_downloadwrd<-downloadHandler(
      filename = function(){
        paste("hlth_wc","png", sep=".")
      },
      content = function(file){
        png(file)
        wordcloud(words = dtf$word,freq=dtf$freq, min.freq=3, max.words = 200,
                  colors = brewer.pal(9,"Set1"), random.order = T,rot.per = .50)
        dev.off()
      }
    )
  })
  #technology wordcloud
  output$tech_wc<-renderPlot({
    withProgress({
      setProgress(message = "creating wordcloud...")
      wc_corpus<-wrdcld()
      wc_tdm<-TermDocumentMatrix(wc_corpus)
      wc_matrix<-as.matrix(wc_tdm)
      wc_sort<-sort(rowSums(wc_matrix),decreasing = T)
      dtf<-data.frame(word=names(wc_sort), freq=wc_sort)
      
      wordcloud(words = dtf$word,freq=dtf$freq, min.freq=3, max.words = 200,
                colors = brewer.pal(9,"Set1"), random.order = T,rot.per = .50)
      
    })
    output$tech_downloadwrd<-downloadHandler(
      filename = function(){
        paste("tech_wc","png", sep=".")
      },
      content = function(file){
        png(file)
        wordcloud(words = dtf$word,freq=dtf$freq, min.freq=3, max.words = 200,
                  colors = brewer.pal(9,"Set1"), random.order = T,rot.per = .50)
        dev.off()
      }
    )
  })
  #agriculture wordcloud
  output$agri_wc<-renderPlot({
    withProgress({
      setProgress(message = "creating wordcloud...")
      wc_corpus<-wrdcld()
      wc_tdm<-TermDocumentMatrix(wc_corpus)
      wc_matrix<-as.matrix(wc_tdm)
      wc_sort<-sort(rowSums(wc_matrix),decreasing = T)
      dtf<-data.frame(word=names(wc_sort), freq=wc_sort)
      
      wordcloud(words = dtf$word,freq=dtf$freq, min.freq=3, max.words = 200,
                colors = brewer.pal(9,"Set1"), random.order = T,rot.per = .50)
      
    })
    output$agri_downloadwrd<-downloadHandler(
      filename = function(){
        paste("agri_wc","png", sep=".")
      },
      content = function(file){
        png(file)
        wordcloud(words = dtf$word,freq=dtf$freq, min.freq=3, max.words = 200,
                  colors = brewer.pal(9,"Set1"), random.order = T,rot.per = .50)
        dev.off()
      }
    )
  })
  #finance wordcloud
  output$fin_wc<-renderPlot({
    withProgress({
      setProgress(message = "creating wordcloud...")
      wc_corpus<-wrdcld()
      wc_tdm<-TermDocumentMatrix(wc_corpus)
      wc_matrix<-as.matrix(wc_tdm)
      wc_sort<-sort(rowSums(wc_matrix),decreasing = T)
      dtf<-data.frame(word=names(wc_sort), freq=wc_sort)
      
      wordcloud(words = dtf$word,freq=dtf$freq, min.freq=3, max.words = 200,
                colors = brewer.pal(9,"Set1"), random.order = T,rot.per = .50)
      
    })
    output$fin_downloadwrd<-downloadHandler(
      filename = function(){
        paste("fin_wc","png", sep=".")
      },
      content = function(file){
        png(file)
        wordcloud(words = dtf$word,freq=dtf$freq, min.freq=3, max.words = 200,
                  colors = brewer.pal(9,"Set1"), random.order = T,rot.per = .50)
        dev.off()
      }
    )
  })
  
  
  #SENTIMENTS PER TOPIC
  sent.analysis<-reactive({
    
    input$update
    
    isolate({
      
      withProgress({
        setProgress(message = "processing sentiments ...")
        
        sent<- getFileData()$content
        
        snt<-tibble(line=1:length(sent), text= sent)  
        snt %>% 
          mutate(linenumber = row_number()) %>% 
          unnest_tokens(output=word, input=text) %>% 
          anti_join(stopword, by = "word") %>% 
          left_join(my_lexicon, by = "word") %>% 
          count(word, sort = TRUE)
      })
      
      
    })
    
  })
  
  #politics sentiments
  output$pol_sentiments<-renderDataTable({
    withProgress({
      
      setProgress(message = "creating sentiment table ...")
      
      snt<-sent.analysis()
      
      
      
      j<-merge.data.frame(snt, my_lexicon,all.x = TRUE, sort=T, no.dups = T) 
      j$sentiment[is.na(j$sentiment)] <- "neutral"
      j$polarity[is.na(j$polarity)] <- 0
      j<-j %>% arrange(desc(n))
      k<-as.data.frame(j)
      
    })
    
  })
  #activism sentiments
  output$act_sentiments<-renderDataTable({
    withProgress({
      
      setProgress(message = "creating sentiment table ...")
      
      snt<-sent.analysis()
      
      
      
      j<-merge.data.frame(snt, my_lexicon,all.x = TRUE, sort=T, no.dups = T) 
      j$sentiment[is.na(j$sentiment)] <- "neutral"
      j$polarity[is.na(j$polarity)] <- 0
      j<-j %>% arrange(desc(n))
      k<-as.data.frame(j)
      
    })
  })
  #healthcare sentiments
  output$hlth_sentiments<-renderDataTable({
    withProgress({
      
      setProgress(message = "creating sentiment table ...")
      
      snt<-sent.analysis()
      
      
      
      j<-merge.data.frame(snt, my_lexicon,all.x = TRUE, sort=T, no.dups = T) 
      j$sentiment[is.na(j$sentiment)] <- "neutral"
      j$polarity[is.na(j$polarity)] <- 0
      j<-j %>% arrange(desc(n))
      k<-as.data.frame(j)
      
    })
  })
  #technology sentiments
  output$tech_sentiments<-renderDataTable({
    withProgress({
      
      setProgress(message = "creating sentiment table ...")
      
      snt<-sent.analysis()
      
      
      
      j<-merge.data.frame(snt, my_lexicon,all.x = TRUE, sort=T, no.dups = T) 
      j$sentiment[is.na(j$sentiment)] <- "neutral"
      j$polarity[is.na(j$polarity)] <- 0
      j<-j %>% arrange(desc(n))
      k<-as.data.frame(j)
      
    })
  })
  #agriculture sentiments
  output$agri_sentiments<-renderDataTable({
    withProgress({
      
      setProgress(message = "creating sentiment table ...")
      
      snt<-sent.analysis()
      
      
      
      j<-merge.data.frame(snt, my_lexicon,all.x = TRUE, sort=T, no.dups = T) 
      j$sentiment[is.na(j$sentiment)] <- "neutral"
      j$polarity[is.na(j$polarity)] <- 0
      j<-j %>% arrange(desc(n))
      k<-as.data.frame(j)
      
    })
  })
  #finance sentiments
  output$fin_sentiments<-renderDataTable({
    withProgress({
      
      setProgress(message = "creating sentiment table ...")
      
      snt<-sent.analysis()
      
      
      
      j<-merge.data.frame(snt, my_lexicon,all.x = TRUE, sort=T, no.dups = T) 
      j$sentiment[is.na(j$sentiment)] <- "neutral"
      j$polarity[is.na(j$polarity)] <- 0
      j<-j %>% arrange(desc(n))
      k<-as.data.frame(j)
      
    })
  })
  
  
  #SCORES PER TOPIC
  
  score<-reactive({
    input$update
    isolate({
      snt<-sent.analysis()
      
      
      
      j<-merge.data.frame(snt, my_lexicon,all.x = TRUE, sort=T, no.dups = T) 
      j$sentiment[is.na(j$sentiment)] <- "neutral"
      j$polarity[is.na(j$polarity)] <- 0
      j<-j %>% arrange(desc(n))
      k<-as.data.frame(j)
    })
    
  }) 
  #politics scores
  output$pol_scores<-renderTable({
    scr1<-score()
    score_tb1<-table(scr1$sentiment)/nrow(scr1)
  })
  #activism scores
  output$act_scores<-renderTable({
    scr2<-score()
    score_tb2<-table(scr2$sentiment)/nrow(scr2)
  })
  #healthcare scores
  output$hlth_scores<-renderTable({
    scr3<-score()
    score_tb3<-table(scr3$sentiment)/nrow(scr3)
  })
  #technology scores
  output$tech_scores<-renderTable({
    scr4<-score()
    score_tb4<-table(scr4$sentiment)/nrow(scr4)
  })
  #agriculture scores
  output$agri_scores<-renderTable({
    scr5<-score()
    score_tb5<-table(scr5$sentiment)/nrow(scr5)
  })
  #finance scores
  output$fin_scores<-renderTable({
    scr6<-score()
    score_tb6<-table(scr6$sentiment)/nrow(scr6)
  })
  
 
  #KEYWORD SEARCH PER TOPIC
  
  #politics search
  output$pol_ngrams <- renderDataTable({
    kyd <- getFileData()$content
    kyd <- unlist(tokenize_sentences(kyd))
    kw <- data.frame(text = kyd)
    
    datatable(kw, options = list(
      searchHighlight = TRUE,
      dom = "lt",
      search = list(
        regex = TRUE,
        search = paste0("\\b", tolower(input$pol_search), "\\b")
      ),
      columnDefs = list(
        list(visible = FALSE, targets = c(0))  # Hide the first column
      )
    ))
  })
  #activism search
  output$act_ngrams <- renderDataTable({
    kyd <- getFileData()$content
    kyd <- unlist(tokenize_sentences(kyd))
    kw <- data.frame(text = kyd)
    
    datatable(kw, options = list(
      searchHighlight = TRUE,
      dom = "lt",
      search = list(
        regex = TRUE,
        search = paste0("\\b", tolower(input$act_search), "\\b")
      ),
      columnDefs = list(
        list(visible = FALSE, targets = c(0))  # Hide the first column
      )
    ))
  })
  #healthcare search
  output$hlth_ngrams <- renderDataTable({
    kyd <- getFileData()$content
    kyd <- unlist(tokenize_sentences(kyd))
    kw <- data.frame(text = kyd)
    
    datatable(kw, options = list(
      searchHighlight = TRUE,
      dom = "lt",
      search = list(
        regex = TRUE,
        search = paste0("\\b", tolower(input$hlth_search), "\\b")
      ),
      columnDefs = list(
        list(visible = FALSE, targets = c(0))  # Hide the first column
      )
    ))
  })
  #technology search
  output$tech_ngrams <- renderDataTable({
    kyd <- getFileData()$content
    kyd <- unlist(tokenize_sentences(kyd))
    kw <- data.frame(text = kyd)
    
    datatable(kw, options = list(
      searchHighlight = TRUE,
      dom = "lt",
      search = list(
        regex = TRUE,
        search = paste0("\\b", tolower(input$tech_search), "\\b")
      ),
      columnDefs = list(
        list(visible = FALSE, targets = c(0))  # Hide the first column
      )
    ))
  })
  #agriculture search
  output$agri_ngrams <- renderDataTable({
    kyd <- getFileData()$content
    kyd <- unlist(tokenize_sentences(kyd))
    kw <- data.frame(text = kyd)
    
    datatable(kw, options = list(
      searchHighlight = TRUE,
      dom = "lt",
      search = list(
        regex = TRUE,
        search = paste0("\\b", tolower(input$agri_search), "\\b")
      ),
      columnDefs = list(
        list(visible = FALSE, targets = c(0))  # Hide the first column
      )
    ))
  })
  #finance search
  output$fin_ngrams <- renderDataTable({
    kyd <- getFileData()$content
    kyd <- unlist(tokenize_sentences(kyd))
    kw <- data.frame(text = kyd)
    
    datatable(kw, options = list(
      searchHighlight = TRUE,
      dom = "lt",
      search = list(
        regex = TRUE,
        search = paste0("\\b", tolower(input$fin_search), "\\b")
      ),
      columnDefs = list(
        list(visible = FALSE, targets = c(0))  # Hide the first column
      )
    ))
  })
  
 
}


