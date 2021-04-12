#https://shaun-jacks.shinyapps.io/visionanalyze/
ui <- function(request){
 
  shinydashboard::dashboardPage(
    tags$head(HTML("<title>SeaportAI|Image analysis</title> <link rel='icon' type='image/gif/png' href='log.png'>")),
    header = shinydashboard::dashboardHeader(titleWidth = 400,
      title = div(img(src="log.png",height=45), 'SeaportAI Image analysis')),
    
    ##### Sidebar #####
    sidebar = shinydashboard::dashboardSidebar(
     disable = T
    ),
    
    ##### Body #####
    body = shinydashboard::dashboardBody(
        # 
        # column(width = 4,
        #         shinydashboard::box(width = NULL,
        #            title = "Input Image",
        #            status = "danger",
        #            solidHeader = FALSE,
        #        shinycssloaders::withSpinner( imageOutput('images') ))
        #        
        #        
        # ),
      
      
      column(5,
             
             shinydashboard::box(width = NULL,
                                 title = "Output Image",
                                 status = "danger",
                                 solidHeader = FALSE,
                                 shinycssloaders::withSpinner(  # Spinner loading bar
                                   imageOutput("picture2") ) # Outputted Image
                                 
             )
      ),
      
        column(7,
               shinydashboard::box(width = NULL,
                                   title = "Image and Analysis Selection",
                                   status = "danger",
                                   solidHeader = FALSE,
          uiOutput("image_analyze"))
        ),
      
        
        column(12,
               shinydashboard::box(width = NULL,
                                   title = "Data Output",
                                   status = "danger",
                                   solidHeader = FALSE,
               DT::dataTableOutput("results"))
        )
        
      
    
  ))
}