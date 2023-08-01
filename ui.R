ui<-dashboardPage(
  
  dashboardHeader(title = "Exploratory Analysis"),
                    
  dashboardSidebar(
    collapsed = T,
    sidebarMenuOutput("sidebar")),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    
    uiOutput("dashboardContentUI"),
    
    tags$head(
      tags$script(src = "login.js")
    ),
    
    div(
      class = "login-container",
      conditionalPanel(
        condition = "!input.isLoggedIn",
        div(
          id = "login",
          textInput("usernameInput", "Username:"),
          passwordInput("passwordInput", "Password:"),
          actionButton("loginBtn", "Login"),
          p("Don't have an account? Click ", tags$a(href = "#", "here", onclick = "signupLinkClick();"), " to sign up."),
          tags$style(
            HTML(
              "
        .login-container {
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          height: 100vh;
        }
        "
            )
          )
        )
      ),
      conditionalPanel(
        condition = "!input.isLoggedIn && input.showSignup",
        div(
          id = "signup",
          textInput("signupUsernameInput", "Username:"),
          passwordInput("signupPasswordInput", "Password:"),
          radioButtons("roleInput", "Role:", choices = c("Admin", "Standard"), selected = "Standard"),
          actionButton("signupBtn", "Sign Up"),
          p("Already have an account? Click ", tags$a(href = "#", "here", onclick = "loginLinkClick();"), " to log in."),
          tags$style(
            HTML(
              "
        .login-container {
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          height: 100vh;
        }
        "
            )
          )
        )
      )
    )
  )
)