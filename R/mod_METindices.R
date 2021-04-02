#' METindices UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_METindices_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' METindices Server Function
#'
#' @noRd 
mod_METindices_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_METindices_ui("METindices_ui_1")
    
## To be copied in the server
# callModule(mod_METindices_server, "METindices_ui_1")
 
