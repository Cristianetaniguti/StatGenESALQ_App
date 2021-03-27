#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinymanager
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(
      dashboardHeader(title = "StatGen-ESALQ App"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("About", tabName = "about", icon = icon("address-card")),
          menuItem("Selection indices", tabName = "indices", icon = icon("crosshairs")),
          tags$li(class = "dropdown",
                  tags$a(href="https://statgen-esalq.github.io/", target="_blank", 
                         tags$img(height = "60px", alt="Logo", src="logo.png")
                  ))
        )
      ),
      dashboardBody(
        # Lab colors
        tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #003350;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #003350;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #003350;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #003350;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color:  #003350;
                              color: #ffffff;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color:  #003350;
                              color: #ffffff;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #cc662f;
                              color: #000000;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #003350;
         }
                              
        .box.box-solid.box-primary>.box-header {
          color:#fff;
          background:#cc662f
                            }
        
        .box.box-solid.box-primary{
        border-bottom-color:#cc662f;
        border-left-color:#cc662f;
        border-right-color:#cc662f;
        border-top-color:#cc662f;
        }
                              '))),
        
        tabItems(
          # First tab content
          tabItem(tabName = "about",
                  includeMarkdown(system.file("ext", "about.Rmd", package = "StatGenESALQ"))
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'StatGenESALQ'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

