#' METassumptionsTest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_METassumptionsTest_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' METassumptionsTest Server Function
#'
#' @noRd 
mod_METassumptionsTest_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_METassumptionsTest_ui("METassumptionsTest_ui_1")
    
## To be copied in the server
# callModule(mod_METassumptionsTest_server, "METassumptionsTest_ui_1")
 
