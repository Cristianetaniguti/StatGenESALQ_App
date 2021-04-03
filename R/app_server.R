#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  passwords <- c(Cris = "DanaBrisa%2021", # Author
                 Kaio = "z8*R@3X1",
                 Camila = "pFfPpa&85",
                 Wellingson = "6V1VTUy^#",
                 Augusto = "S9rFy8^Z8",
                 Epagri = "Oq%RnT399"# committee
  ) # lab member
  
  credentials <- data.frame(
    user =     names(passwords), # mandatory
    password = as.vector(passwords), # mandatory
    start = c(rep("2021-02-28", length(passwords))), # optinal (all others)
    expire = c(rep(NA,length(passwords))),
    admin = c(TRUE, rep(FALSE, length(passwords)-1)),
    comment = "Simple and secure authentification mechanism 
    for single ‘Shiny’ applications.",
    stringsAsFactors = FALSE
  )
  
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  callModule(mod_assumptionsTest_server, "assumptionsTest_ui_1")
  callModule(mod_indices_server, "indices_ui_1")
  callModule(mod_METassumptionsTest_server, "METassumptionsTest_ui_1")
  callModule(mod_METindices_server, "METindices_ui_1")
  callModule(mod_met_server, "met_ui_1")
}
