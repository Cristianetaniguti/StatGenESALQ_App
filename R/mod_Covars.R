##################################################
# Multi environment two stages analysis
# Models with Geographic covariates
# 
# Written by Camila Godoy and Cristiane Taniguti
##################################################
#' Covars UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Covars_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(style = "height:5000px",
             box(width = 12, 
                 p("Run multi environment analysis using two stage model and including geographic coords.")
             ),
             box(width = 12,
                 selectInput(ns("design"), label = h4("Experiment design"), 
                             choices = list("Randomized complete block" = "block"), 
                             selected = "block")
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Input file",
                 p("The input file is a tab delimited table with a column called 'local' defining the environment, 
                   other called 'gen' defining the genotypes and other called 'block' defining the block number. 
                   The adjusted phenotypic means should be included in extra columns. Download here an input file example:"),
                 downloadButton(ns("data_example")), hr(),
                 p("Upload here your file:"),
                 tags$b("Warning:"), p("To not overload our server, this feature is blocked. Please run the app locally with:"),
                 tags$code("devtools::install_github('statgen-esalq/StatGenESALQ_App')"), br(),
                 tags$code("library(StatGenESALQ)"), br(),
                 tags$code("run_app()"), p("or"), br(),
                 tags$code("docker run --rm -e USERID=$(id -u) -e GROUPID=$(id -g) -p 80:80 -e DISABLE_AUTH=true cristaniguti/statgenapp"),
                 hr(),
                 p("If you do not have an file to be upload you can still check this app features with our example file. The example file is automatically upload, you just need to procedure to the other buttons."),
                 br(),
                 actionButton(ns("read_data"), "Read the file",icon("refresh")), hr()
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, status="primary", title = "Select variables",
                 box(width = 6,
                     radioButtons(ns("trait"), label = p("Choose the traits to be evaluated:"),
                                  choices = "Press 'Read the file' button to update",
                                  selected = "Press 'Read the file' button to update"),
                 ),
                 box(width = 6,
                     checkboxGroupInput(ns("local"), label = p("Choose the location to be evaluated:"),
                                        choices = "Press 'Read the file' button to update",
                                        selected = "Press 'Read the file' button to update")
                 ),
                 box(width = 6,
                     textInput(ns("cities"), label = p("Add City and State:"), value = "Canoinhas SC ,Chapecó SC, 
                            Chapecó SC,Campos Novos SC,Ituporanga SC, Ponte Serrada SC, Urussanga SC, Xanxerê SC"),
                 ),
                 box(width = 6,
                     textInput(ns("altitude"), label = p("Add cities altitude:"), value = "765, 674, 674, 947, 830, 1067, 49, 803"),
                 ),
                 br(),
                 actionButton(ns("run_analysis"), "Read the file",icon("refresh")), hr()
             ), hr(),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Multi environment two stage model:",
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Exploratory plot",
                     plotOutput(ns("plot_expl_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Overview",
                     DT::dataTableOutput(ns("multi_two_stages_out"))
                 )
             ),
             box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, status="info", title = "Geographic coefficients:",
                 
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Overview",
                     DT::dataTableOutput(ns("multi_covar_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Genotypic sensibility coefficient for longitude",
                     plotOutput(ns("plot_sens_alt_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Genotypic sensibility coefficient for latitude",
                     plotOutput(ns("plot_sens_lat_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Genotypic sensibility coefficient for altitude",
                     plotOutput(ns("plot_sens_long_out"))
                 ),
                 box(width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = T, title = "Fraction of GxL explained by geographic coords",
                     plotOutput(ns("plot_gl_out"))
                 )
             )
    )
  )
}

#' Covars Server Function
#'
#'
#' @import emmeans
#' @import RgoogleMaps
#' @import frGIS
#' @import plyr
#' @import maditr
#' @import lattice
#' 
#' @noRd 
mod_Covars_server <- function(input, output, session){
  ns <- session$ns
  ## download input
  output$data_example <- downloadHandler(
    filename =  function() {
      paste("example_data.csv")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$design == "block"){
        dat <- read.csv(system.file("ext","example_inputs/data_bean.csv", package = "StatGenESALQServer"))
      } else {
        dat <- read.csv(system.file("ext","example_inputs/data_corn.csv", package = "StatGenESALQServer"))
      }
      write.csv(dat, file = file, row.names = F)
    } 
  )
  
  button1 <- eventReactive(input$read_data, {
    if(is.null(input$data_input)){
      if(input$design == "block"){
        dat <- read.csv(system.file("ext","example_inputs/data_bean.csv", package = "StatGenESALQServer"))
      } else {
        dat <- read.csv(system.file("ext","example_inputs/data_corn.csv", package = "StatGenESALQServer"))
      }
    } else {
      dat <- read.csv(input$data_input)
    }
    dat
  })
  
  observe({
    
    if(any(colnames(button1()) %in% "rep"))
      choices_trait_temp <- colnames(button1())[-c(1:4)] else
        choices_trait_temp <- colnames(button1())[-c(1:3)]
      
      choices_trait <- choices_trait_temp
      names(choices_trait) <- choices_trait_temp
      
      choices_locations_temp <- unique(button1()[,"local"])
      choices_locations <- choices_locations_temp
      names(choices_locations) <- choices_locations_temp
      
      updateRadioButtons(session, "trait",
                         label="Choose the trait to be evaluated:",
                         choices = choices_trait,
                         selected = unlist(choices_trait)[1])
      
      updateCheckboxGroupInput(session, "local",
                               label="Choose the locations to be evaluated:",
                               choices = choices_locations,
                               selected = unlist(choices_locations)[1:2])
  })
  
  button2 <- eventReactive(input$run_analysis, {
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      dat <- button1()
      dat$block <- as.factor(dat$block)
      dat$gen <- as.factor(dat$gen)
      dat$local <- as.factor(dat$local)
      
      if(input$design == "block"){
        if(!all(c("local", "block", "gen") %in% colnames(dat)) | ("rep" %in% colnames(dat)))
          stop(safeError("Randomized complete block design should have columns 'local', 'block' and 'gen'."))
        dat <- dat %>% select(c("local", "gen", "block",input$trait)) %>%
          filter(local %in% input$local) %>% droplevels()

        head(dat)
      } else {
        if(!all(c("local", "block", "gen", "rep") %in% colnames(dat)))
          stop(safeError("Alpha lattice design should have columns 'local', 'block', 'rep', and 'gen'."))
        dat$rep <- as.factor(dat$rep)
        
        dat <- dat %>% select(c("local", "gen", "block","rep",input$trait)) %>%
          filter(local %in% input$local) %>% droplevels()
        
        dat$local <- as.factor(dat$rep)
      }
      
      # Obtendo as médias ajustadas por local (análise individual)
      trait = parse(text = input$trait) 
      envs = levels(dat$local)
      results = vector("list", length(envs))
      
      #selecionar o trait de interesse
      #envs corresponde ao número de ambientes
      #criação do objeto resuls para receber os resulados do loop 
      
      # Loops Envs
      for (i in envs) {

        edata_1 = droplevels(subset(dat, local == i))

        # Model
        model_1 = aov(eval(trait) ~ gen + block, data = edata_1)
        temp_1 = data.frame(lsmeans(model_1, ~ gen))
        results[[i]] <- data.frame(gen = temp_1$gen, 
                                   trait = temp_1$lsmean, 
                                   local = factor(levels(edata_1$local)), 
                                   local_mean = mean(temp_1$lsmean))
        
        rm(edata_1, model_1, temp_1)
      }
      
      #o loop vai a nível de ambientes
      #droplevels e subset para separar os ambientes
      #ajustar o modelo fixo (função eval para selecionar o trait)
      #pegar médias ajustadas do modelo usando a função lsmeans
      #receber esses resultados dentro da lista results
      #data.frame (gen, trait(médias ajustadas), local e média dos locais
      #rm para remoção dos arquivos temporários
      
      
      # Médias ajustadas
      StageI = do.call(rbind, results) 
      colnames(StageI)[2] = trait
      rm(results, i, envs)
      
      #juntando os resuldados de todos os ambientes no objeto StageI
      #trocar o nome da coluna 2 para trait=rend
      
      # Análise conjunta usando as médias ajustadas
      model_2 = lm(eval(trait) ~ gen + local, data = StageI)
      all(rownames(StageI) == names(model_2$residuals))
      StageI$gl = model_2$residuals
      
      #ajustar o modelo da análise conjunta
      #note que estamos usando os dados StageI (médias ajustadas)
      #o resíduo desse modelo é igual a interação GxA
      #confirmar se a ordem dos resíduos está igual a ordem do banco de dados
      #adicionar os resíduos no banco de dados
      
      # Obtendo as covariáveis geográficas
      local = unlist(strsplit(input$cities, ","))
      
      # c("Canoinhas SC","Chapecó SC","Chapecó SC","Campos Novos SC","Ituporanga SC", 
      #   "Ponte Serrada SC", "Urussanga SC", "Xanxerê SC")
      
      SIG = data.frame(local = local, lat = NA, lon = NA)
      SIG = with(SIG, data.frame(local=local, t(sapply(SIG$local, getGeoCode))))
      
      #SIG$alt = c(765, 674, 674, 947, 830, 1067, 49, 803)
      SIG$alt = as.numeric(unlist(strsplit(input$altitude, ",")))

      #criar um data frame vazio com local, latitude e longitude
      #a função getGeoCode adiciona as informações faltantes no data frame
      #adicionar informações de altitude (pacote não faz)
      
      # Obtendo os valores escalados (média = 0 e variância = 1)
      SIG[,-1] = apply(SIG[,-1], 2, function(x) {scale(x, center = TRUE)})

      rownames(SIG) = NULL
      SIG$local = factor(levels(dat$local))
      
      # ADD Cols
      StageI$alt = StageI$lat = StageI$lon = NA
      
      #adicionar coluna atl, lat e long no arquvi de dados StageI
      
      # Loop
      for(i in 1:nlevels(SIG$local)){
        StageI[which(StageI$local == SIG$local[i]), c("alt","lat", "lon")] = SIG[i, c("alt","lat", "lon")]
      }
      rm(local, i, SIG)
      
      #o loop a nivel de locais para adicionar faltantes
      
      # Regressão Fatorial
      genotype = levels(StageI$gen)
      StageI$u_1 = NA
      
      for(i in 1:length(genotype)){
        edata_2 = droplevels(StageI[which(StageI$gen == genotype[i]),]) 
        temp_2 = rownames(edata_2) 
        rownames(edata_2) = edata_2$env
        model_3 = lm(gl ~ -1 + lat + lon + alt, data = edata_2)
        StageI[temp_2,]$u_1 = residuals(model_3)
        rm(model_3, temp_2, edata_2)
      }

      #regressão fatorial para cada genótipo
      #o loop vai a nível de genótipos
      #droplevels para separar os genótipos
      #fazezr uma regressão para cada genótipo (tirar o intercepto)
      #adicionar o resíduo da regressão no conjunto de dados para comparar com o modelo 3
      
      #ao ajustar o modelo obtemos os coeficientes
      #coeficiente de regressão associado ao efeito da covariável
      #coeficiente de sensibilidade genotípica
      #aqui utilizamos apenas a matriz GE para obter os coeficientes
      
      # Adaptação Regional
      StageI$AD = StageI[,input$trait] - StageI$local_mean
      genotype = levels(StageI$gen)
      StageI$u_2 = NA

      #diferença entre as médias ajustadas e as médias do ambiente
      #usamos o G+GE para obter os coeficientes
      #outra maneira de reescrever o modelo 3
      #os resíduos dos dois modelos são equivalentes
      for(i in 1:length(genotype)){
        edata_3 = droplevels(StageI[which(StageI$gen == genotype[i]),]) 
        temp_3 = rownames(edata_3) 
        rownames(edata_3) = edata_3$Env
        model_4 = lm(AD ~ 1 + lat + lon + alt, data = edata_3)
        StageI[temp_3,]$u_2 = residuals(model_4)
        rm(model_4, temp_3, edata_3)
      }
     
      
      df.y = StageI[, c("local", "gen", "AD", "lon", "lat", "alt")]
      colnames(df.y)[1:3] = c("env", "gid", "ggl")
      cat("passei")
      str(df.y)
      output = FR.model(df.y = df.y, intercept = T)
      cat("passei2")
      #selecionar as colunas de interesse do cojunto de dados StageI
      #renomear as três primeiras colunas
      #pacote considerando os efeitos G+GE
      
      # output$coefficients   # genotypic coefficients
      # output$sum.of.squares # anova output for each genotype
      # output$frac.ss        # fraction of phenotypic variance explained by the effect of environment covariates
      
      str(output)
      incProgress(0.25, detail = paste("Doing part", 2))
      list(dat, StageI, output)
    })
  })
  
  output$plot_expl_out <- renderPlot({
    dat <- button2()[[1]]
    form1 <- as.formula(paste(input$trait, "~ gen | local"))
    xyplot(form1, data = dat, type = c("p", "a"), 
           col.line = "#003060", col=c("#003060"), 
           pch=21, lwd = 3, scales=list(x=list(rot=90, cex=0.7), y=list(cex=0.7)), 
           ylab = list(label=expression("kg/ha"), 
                       fontsize=9), xlab = list(label="genotype", fontsize=9))
  })
  
  output$multi_two_stages_out <- DT::renderDataTable(
    DT::datatable(data.frame(button2()[[2]]),  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf'),
                    scrollX = TRUE
                  ),
                  class = "display")
  )
  
  output$multi_covar_out <- DT::renderDataTable(
    DT::datatable(data.frame(button2()[[3]]$coefficients),  
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtlp',
                    buttons = c('copy', 'csv', 'excel', 'pdf'),
                    scrollX = TRUE
                  ),
                  class = "display")
  )
  
  output$plot_sens_long_out <- renderPlot({
    ggplot(button2()[[3]]$coefficients, aes(x=gid, y=lon)) + geom_bar(stat = "identity", fill = "#4d728d") + 
      labs(title ="Longitude", x="Genotype", y = "Genotipic sensibility coefficient") + 
      theme(axis.text=element_text(size=8), axis.text.x = element_text(angle=90), text = element_text(size=9))
  })
  
  output$plot_sens_lat_out <- renderPlot({
    ggplot(button2()[[3]]$coefficients, aes(x=gid, y=lat)) + geom_bar(stat = "identity", fill = "#4d728d") + 
      labs(title ="Latitude", x="Genotype", y = "Genotipic sensibility coefficient") + theme(axis.text=element_text(size=8), axis.text.x = element_text(angle=90), text = element_text(size=9))
  })
  
  output$plot_sens_alt_out <- renderPlot({
    ggplot(button2()[[3]]$coefficients, aes(x=gid, y=alt)) + geom_bar(stat = "identity", fill = "#4d728d") + 
      labs(title ="Altitude", x="Genotype", y = "Genotipic sensibility coefficient") + theme(axis.text=element_text(size=8), axis.text.x = element_text(angle=90), text = element_text(size=9))
  })
  
  output$plot_gl_out <- renderPlot({
    button2()[[3]]$frac.ss %>% 
      gather("var", "value", -gid) %>% 
      ggplot() + geom_bar(stat = "identity", aes(x = gid, y = value, fill = var)) + 
      labs(title ="Fração da GxL", x="genótipo", y = "fração da GxL") + 
      scale_y_continuous(labels = scales::percent) + 
      theme(axis.text=element_text(size=8), axis.text.x = element_text(angle=90), text = element_text(size=9))
  })
  
}

## To be copied in the UI
# mod_Covars_ui("Covars_ui_1")

## To be copied in the server
# callModule(mod_Covars_server, "Covars_ui_1")

