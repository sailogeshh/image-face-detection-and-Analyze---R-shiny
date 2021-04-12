library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyanimate)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(magrittr)
library(ggthemes) 
library(ggpubr)
library(ggplot2)
library(flexdashboard)
library(RoogleVision)
library(magick)
library(plotly)
library(shinycssloaders)
library(leaflet)


ui <- fluidPage(
  tags$style(HTML('body {font-family:"Verdana",Georgia,Serif; background-color:white}')),
  theme = shinytheme("yeti"),
  withAnim(),
  #setBackgroundImage(src = "w.jpg"),
  tags$head(
    tags$style(type = 'text/css', 
               HTML('
                    .navbar-default .navbar-brand{color: ;}
                    .tab-panel{ background-color: #; color: #}
                    .navbar-default .navbar-nav > .active > a, 
                    .navbar-default .navbar-nav > .active > a:focus, 
                    .navbar-default .navbar-nav > .active > a:hover {
                    color: #;
                    background-color: #;
                    
                    }')
                                                )
               ),
  
  tags$style(HTML(".navbar  {
                  background-color:#005380; }
                  
                  .navbar .navbar-nav {float: right; margin-right: 35px;
                  margin-top: 26px;
                  color: #; 
                  font-size: 18px; 
                  background-color: #; }
                  
                  .navbar.navbar-default.navbar-static-top{ 
                  color: #; 
                  font-size: 23px; 
                  background-color: # ;}
                  
                  .navbar .navbar-header {
                  float: left;
                  background-color: # ;}
                  
                  .navbar-default .navbar-brand { color: #e6e6e6; 
                  margin-top: 20px;
                  font-size: 24px; 
                  background-color: # ;} 
                  
                  ")),
  tags$style(type="text/css",
             "#well0{
             background: #f2f2f2;
                  }"),
  tags$style(type="text/css",
             "#well2{
             padding: 100px;
             background: #;
             border: 1px;
             box-shadow:2px 2px;}"),
  tags$style(type="text/css",
             "#well8{
             padding: 100px;
             background: #;
             border: 1px;
             box-shadow: 2px 2px;}"),
  tags$style(type="text/css",
             "#rrr{
             padding: 100px;
             background: #;
             border: 0px;
             box-shadow: 0px 0px;}"),
  tags$head(
    tags$style(HTML("
                    input[type=\"number\"] {
                    font-size: 20px;height:50px;
                    }
                    
                    "))
    ),
  #tags$style(type='text/css','#qq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
  #tags$style(type='text/css','#qqq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
  #tags$style(type='text/css','#qqqq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
  #tags$style(type='text/css','#qqqqq {background-color: #d2d3d4; color: #004264;font-size:120%}'),
  
  tags$head(HTML("<title>Image Analytics</title> <link rel='icon' type='image/gif/png' href='t.png'>")),
  navbarPage(id="tabset",tags$li(class = "dropdown",
                                 tags$style(".navbar {min-height:100px }")
  ),
  #title = ,position = "fixed-top",selected = "Upload",inverse = TRUE,
  title = tags$div(img(src="log.png",strong("SeaportAI(Analytics|Robotics)"), style="margin-top: -14px;margin-left: 30px;", height = 60)),position = "fixed-top",selected = strong("Upload"),inverse = F,
  tabPanel(title = strong("Upload"),icon = icon("upload"),
           
           fluidPage(
             
             tags$style(" #modal1 .modal-header {background-color:#; border-top-left-radius: 0px; border-top-right-radius: 0px}"),
             
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             tags$head(tags$style("#pppp{color:black; font-size:35px; font-style:italic; text-align=center;
                                  overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
             tags$head(tags$style("#roi{color:black; font-size:35px; font-style:italic; text-align=center;
                                  overflow-y:scroll; max-height: 300px; background: ghostwhite;}")),
             
             
             br(),
             br(),
          
             
             column(7,
                    withAnim(),
                    # tags$h3(strong(em("Aim of this Analysi(s):")),style="text-align:center;color:#004264;font-size:180%"),br(),
                    # tags$div(h4("The identification of rare items, events or observations which raise suspicions",style="text-align:center;color:dimgrey"),align="center"),
                    # tags$div(h4("by differing significantly from the majority of the data.",style="text-align:center;color:dimgrey"),align="center"),
                    br(),br(),br(),
                    #tags$div(id = 'logo1',img(src="ee.png",height='75%',width='75%'),align="center")
                    tags$div(id="ooo",h1(strong("Input Image:"),style="color:#024b74;font-size:180%;"),align="center"),br(),
                    wellPanel(id="well0",style = "overflow-y: ;width:100%;height:720px",
                              tags$div(withSpinner(imageOutput("image")),align="center")),
                    uiOutput("ggg")
                    
             ),
             
             br(),
             br(),
             
             column(5,
                    
                    
                    bootstrapPage(useShinyjs(),
                                  br(),
                                  
                                  tags$h3(strong(em("Image Analytics")),style="text-align:center;color:#024b74;font-size:180%"),
                                  br(),
                                  
                                  tags$div(id = 'logo2',img(src="tt.png",height='65%',width='65%'),align="center"),
                                  bsPopover(id="logo2",title = "Welcome to SeaportAI Image Analytics",content = NULL,placement = "top"),
                                  
                                  
                                  br(),
                                  withAnim(),
                                  
                                 column(12,column(1),
                                        column(6,uiOutput('fileupload')), column(3,uiOutput('num')),column(2)
                                        ),
                                  uiOutput('checkbox'),
                                  uiOutput("button"),
                                  uiOutput("helptext"),
                                  br(),
                                  br(),
                                  bsPopover(id="check",title = "Note:",content = "I accept the SeaportAI Terms & Conditions.. Show the Analyse button",placement = "right"),
                                  tags$div(bsButton("reset", label = "Reset ?", icon =   icon("repeat",lib = "glyphicon"),block = F, style="danger",size = "small"),align="center"),
                                  bsPopover(id="fileupload",title = "Formats only accept..",content = "(.png/.jpeg/.jpg/.pdf)",placement = "bottom"),
                                  
                                  #tags$h1(actionButton("myuser","Logout",icon=icon("user")),style="text-align:center"),
                                  br(),
                                  
                                  tags$div(class = "header", checked = NA,style="text-align:center;color:#929292;font-size:100%",
                                           tags$tbody("Need Help ?"),
                                           tags$a(href = "http://seaportai.com/contact-us/", "Contact Us...")
                                  )
                    )
             )
             
             
             
             )),
  
  
  
  tabPanel(title = strong("|")),    
  
  
  tabPanel(title = "Output",
           tags$head(
             tags$style(type = 'text/css', 
                        HTML('.navbar { background-color: red;}
                             .navbar-default .navbar-brand{color: white;}
                             .tab-panel{ background-color: red; color: white}
                             .navbar-default .navbar-nav > .active > a, 
                             .navbar-default .navbar-nav > .active > a:focus, 
                             .navbar-default .navbar-nav > .active > a:hover {
                             color: white;
                             background-color: #191919;
                             }')
                          )
                        ),
           #          tags$style(".nav-tabs {
           # background-color:white;
           #                              }
           #                              
           #                         "),
           
           fluidRow(
             br(),br(),br(),br(), br(),
             tabBox(id = "tabset2",
                    height = "100px",width = 12,
                    selected ="Facial Detection",
                    tabPanel("Facial Detection", br(),
                             wellPanel(div(strong("Output Image"),style="color:#024b74;font-size:150%;",align="center")),
                             wellPanel(br(),
                                       splitLayout(	
                                         
                                         withSpinner( imageOutput("outimage",height = "600px")),
                                         
                                         withSpinner(dataTableOutput("outdata"))
                                       )
                             )
                    ),
                    
                    
                    
                    tabPanel("Label Detection", br(),
                             wellPanel(div(strong("Output Image"),style="color:#024b74;font-size:150%;",align="center")),
                             wellPanel(uiOutput("outdataa"),br(),
                                       splitLayout(	
                                         
                                         withSpinner(imageOutput("outimage2",height = "600px")),
                                         
                                         withSpinner(plotlyOutput("gauge22",height = "600px"))
                                       )
                             )
                    ),
                    
                 
                    
                    tabPanel("Landmark Detection",  br(),
                             wellPanel(div(strong("Output Image"),style="color:#024b74;font-size:150%;",align="center")),
                             wellPanel(br(),
                                       splitLayout(	
                                         
                                         withSpinner(imageOutput("outimage3",height = "600px")),
                                         
                                         withSpinner(leafletOutput("gauge3",height = "600px"))
                                       ),withSpinner(plotlyOutput("lyp",height = "100px"))
                             )
                             ),
                    
                    tabPanel("Logo Detection",  br(),
                             wellPanel(div(strong("Output Image"),style="color:#024b74;font-size:150%;",align="center")),
                             wellPanel(br(),
                                       splitLayout(	
                                         
                                         tags$div(withSpinner(imageOutput("outimage4",height = "600px")),align="center"),
                                         
                                          withSpinner(plotlyOutput("gauge4",height = "600px"))
                                        )
                             )),
                    
                    # tabPanel("Object Detection",  br(),
                    #          wellPanel(div(strong("Output Image"),style="color:#024b74;font-size:150%;",align="center")),
                    #          wellPanel(br(),
                    #                    splitLayout(	
                    #                      
                    #                      withSpinner(imageOutput("outimage5",height = "600px")),
                    #                      
                    #                      withSpinner(plotlyOutput("gauge5",height = "600px"))
                    #                    )
                    #          )),
                    
                    tabPanel("Text Detection",  br(),
                             wellPanel(div(strong("Output Image"),style="color:#024b74;font-size:150%;",align="center")),
                             wellPanel(uiOutput("textdataa"),br(),
                                       splitLayout(	
                                         
                                         withSpinner(imageOutput("outimage6",height = "600px")),
                                         
                                         withSpinner(DT::dataTableOutput("gauge6"))
                                       )
                             ))
                    
             )
           )
           
                        ), 
  
  
  
  tabPanel(title = strong("|")), 
  
  
  navbarMenu("More",icon = icon("plus-square"),
             tabPanel(
               tags$div(tags$a(href="javascript:history.go(0)",bsButton("logoutadmin", label = "Logout", icon =   icon("repeat",lib = "glyphicon"),block = F, style="success"),style="text-align:center"),align="center"),
               br()
             )
  ))
           )



server <- function(input, output, session) {
  options(shiny.maxRequestSize=50*1024^2)
  
  id="1049450539767-1ht9e2epl8unh0dr133qs010flurbhlm.apps.googleusercontent.com"
  secret="xIk8p9RQc_z8aE54o3_m1xEZ"
  
  options("googleAuthR.client_id" = id)
  options("googleAuthR.client_secret" = secret)
  options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))
  googleAuthR::gar_auth(".httr-oauth")
  
  ##################################################################################################################################################
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
      updateTabItems(session, "tabset",selected = "Output")
    }
  })
  
  
  observeEvent(input$reset,{
    reset(id = "file")
  })
  
  output[["fileupload"]] <- renderUI({
    input$reset
    tags$div(fileInput("file1",label = tags$h4(strong(em("Upload Image..")),style="color:#004264;font-size:160%"),accept=c('.png','.jpeg','.jpg','.pdf')),align="center")
    
  })
  
  output[["num"]] <- renderUI({
    tags$div(numericInput("numeric",label = tags$h4(strong(em("No.of results..?")),style="color:#004264;font-size:160%"),value = 6,width ="100%"),align="center")
    
  })
  
  output[["checkbox"]] <- renderUI({
    input$reset
    tags$div(checkboxInput("check",tags$a(href = "http://seaportai.com/privacy-policy/", "Terms & Conditions",style="color:green;"),value = TRUE),align="center")
    
  })
  
  output[["button"]] <- renderUI({
    if(input$check==TRUE){
      tags$div(bsButton("analyze",strong("Lets Go..!"),icon = icon("refresh"),style = "primary",size="medium"),
               style="color:white;font-weight:100%;",align="center")
    }
  })
  
  
  observeEvent(input$outdata22,{		
    showModal(tags$div(id="modal1", modalDialog(		
      inputId = 'Dialog1', 		
      title = HTML('<span style="color:grey; font-size: 20px;text-align:center; font-weight:bold; font-family:sans-serif "><span>		
                   <button type = "button" class="close" data-dismiss="modal" ">		
                   <span style="color:black; ">x <span>		
                   </button> '),		
      footer = modalButton("Close"),		
      size = "l",		
      DT::dataTableOutput("outdata2"),
      easyClose = TRUE		
      )))		
  })
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
  output[["outdataa"]] <- renderUI({
   
      tags$div(bsButton("outdata22",strong("Download Data..!"),icon = icon("download"),style = "danger",size="medium"),
               style="color:white;font-weight:100%;",align="right")
      })
    }
  })
  
  output[["helptext"]] <- renderUI({
    if(input$check==TRUE){
      tags$div(helpText("To get results, click the 'Lets go!' button...",style="text-align:center"),align="center")
    }
  })
  
  
  observe(addHoverAnim(session, 'logo1', 'pulse'))
  observe(addHoverAnim(session, 'image', 'pulse'))
  observe(addHoverAnim(session, 'analyze', 'shake'))
  observe(addHoverAnim(session, 'reset', 'shake'))
  
  
  observeEvent(input$analyze, {
    confirmSweetAlert(
      session = session,
      inputId = "confirmation",
      type = "warning",
      title = "Are you sure the uploaded data was correct ?",
      btn_labels = c("Nope", "Yep"),
      danger_mode = TRUE
    )
  })
  
  
  
  ############################################################################################################################### 
  
  data <- reactive({
    
    shiny::withProgress(message = "Obtaining Google Vision Analysis", value = 0.1, {
      
      shiny::incProgress(amount = 0, detail = "sending photo to Google Vision")
      # send to Google Vision
      data <- input$file1
      final <- data$datapath
      shiny::incProgress(amount = .8, detail = "Done")
      
    })
    final
  })
  
  pic <-reactive({
    file1 <- input$file1
    if(is.null(file1)) {return(NULL)}
    pic  <- file1$datapath
    pic
    
  })
  
  output$image <- renderImage({
    
    if (is.null(input$file1)) {
      outfile  <- 'ff.png'	
      content_type  <- 'image/png'
    } 
    else {
      outfile  <- data()	
      content_type  <- input$file1$type
    }
    list(src = outfile, contentType = content_type,width = 980, height =680) #
    
  }, deleteFile = FALSE)
  
  
  output$ggg <- renderUI({
    bsPopover(id="image",title = "Note",content = "Once You Upload the file You can see your file here..",placement = "bottom")
    
  })
  ######################################################### FACE ################################################################################   
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
      output$outimage <- renderImage({
        shiny::withProgress(message = "Rendering Image", value = 0.1, {
          
          req(data())
          
          shiny::incProgress(
            amount = 0, 
            detail = "Reading image and converting to image pointer with Magick"
          )
          
          photo = magick::image_read(data())
          shiny::incProgress(0.1)
          # obtain photo dimensions
          photo_info = magick::image_info(photo)
          width = photo_info$width
          height = photo_info$height
          max_height <- 400
          rescale_lwd <- 2/max_height * height
          
          shiny::incProgress(amount = 0, detail = "Using graphics device to draw image")
          magick::image_draw(photo)  # draw photo with graphics device
          shiny::incProgress(amount = 0.3)
          
          img_res <- getGoogleVisionResponse(data(),feature = "FACE_DETECTION")
          
          face_detected <- !is.null(img_res$fdBoundingPoly$vertices)
          
          if (face_detected) {
            shiny::incProgress(
              amount = 0, 
              detail = "Drawing bounding boxes and facial landmarks"
            )
            bounding_boxes = img_res$fdBoundingPoly$vertices  # save list of bounding boxes
            points_lst = img_res$landmarks  # save list of point locations of facial landmarks
            mapply(x = bounding_boxes, 
                   y = c(1:length(bounding_boxes)), 
                   z = points_lst, 
                   function(x, y, z) {
                     polygon(x = x[[1]], y = x[[2]], border = 'green', lwd = rescale_lwd)
                     # label each image as referenced in table
                     text(x = x[[1]][[3]], y = x[[2]][[3]], 
                          labels = as.character(paste0("Face",y)),
                          offset = 1,
                          cex = 3,  # text magnified by this factor relative to default
                          col = "#feea0d")
                     points(x = z$position$x,  # plot the x coordinates of facial landmarks
                            y = z$position$y,  # plot the y coordinates of facial landmarks
                            col = "green", 
                            lwd = rescale_lwd, 
                            pch = 3)
                   })
          } else {
            text(x = width/2, y = height/2, labels = "No features detected", 
                 col = "red",  offset = 1, cex = 5)
          }
          
          shiny::incProgress(
            amount = 0.3, 
            detail = "Capturing image and storing into magick image pointer"
          )
          # Save the graphics output into a magick image object
          photo = magick::image_capture()  
          shiny::incProgress(0.1)
          dev.off()
          
          tmpFile <- image_write(
            photo, 
            tempfile(fileext='jpg'),
            format = 'jpg'
          )  # tmpfile for output of image to shiny
          
          shiny::incProgress(0.1)
          
        }) # end withProgress bar
        
        # src is image to output, and height is max height of image on shiny output
        list(src = tmpFile,width = 900, height =580)
        
      })
 
    }
  })
  
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
      output$outdata <- DT::renderDataTable({
        img_res <- getGoogleVisionResponse(data(),feature = "FACE_DETECTION")
        
        as.data.frame(
          list(
            joy = img_res$joyLikelihood, 
            sorrow = img_res$sorrowLikelihood,
            anger = img_res$angerLikelihood, 
            surprise = img_res$surpriseLikelihood,
            underExposed = img_res$underExposedLikelihood,
            blurred = img_res$blurredLikelihood,
            headwear = img_res$headwearLikelihood
          )
        )
        
      }, extensions = c('Buttons'),editable = F,
      class   = DT:::DT2BSClass(c('compact', 'cell-border')),
      escape  = F, selection = 'multiple',
      options = list(
        pageLength = 6,
        dom = 'Bfrtip',		
        deferRender = TRUE,
        buttons = c('excel', 'pdf', 'print'),
        autoWidth  = T,
        targets = 6,
        LengthMenu = c(5, 30, 50),
        columnDefs = list(list(
          className = 'dt-body-left'))
      ))
    }
  })
  
  
 
  ###################################################### Label ###########################################################################
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
       output$outimage2 <- renderImage({
         # outfile  <- input$file1$datapath	
         # content_type  <- input$file1$type
         # 
         # list(src = outfile, contentType = content_type,width = 900, height =580) 
          shiny::withProgress(message = "Rendering Image", value = 0.1, {
           
           req(data())
           
           shiny::incProgress(
             amount = 0, 
             detail = "Reading image and converting to image pointer with Magick"
           )
           
           photo = magick::image_read(data())
           shiny::incProgress(0.1)
           # obtain photo dimensions
           photo_info = magick::image_info(photo)
           width = photo_info$width
           height = photo_info$height
           max_height <- 400
           rescale_lwd <- 2/max_height * height
           
           shiny::incProgress(amount = 0, detail = "Using graphics device to draw image")
           magick::image_draw(photo)  # draw photo with graphics device
           shiny::incProgress(amount = 0.3)
           shiny::incProgress(
             amount = 0.3, 
             detail = "Capturing image and storing into magick image pointer"
           )
           # Save the graphics output into a magick image object
           photo = magick::image_capture()  
           shiny::incProgress(0.1)
           dev.off()
           
           tmpFile <- image_write(
             photo, 
             tempfile(fileext='jpg'),
             format = 'jpg'
           )  # tmpfile for output of image to shiny
           
           shiny::incProgress(0.1)
           
          }) # end withProgress bar
         
         # src is image to output, and height is max height of image on shiny output
         list(src = tmpFile,width = 900, height =580)
         
       })
    } 
    })


  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
      output$outdata2 <- DT::renderDataTable({
        img_res <- getGoogleVisionResponse(input$file1$datapath,feature = "LABEL_DETECTION",numResults = input$numeric)
                data=data.frame(img_res[,c(2,3)])
      }, extensions = c('Buttons'),editable = F,
      class   = DT:::DT2BSClass(c('compact', 'cell-border')),
      escape  = F, selection = 'multiple',
      options = list(
        pageLength = 6,
        dom = 'Bfrtip',		
        deferRender = TRUE,
        buttons = c('excel', 'pdf', 'print'),
        autoWidth  = T,
        targets = 6,
        LengthMenu = c(5, 30, 50),
        columnDefs = list(list(
          className = 'dt-body-left'))
      ))
    }
  })
  
      
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){  
      output$gauge22 <- plotly::renderPlotly({
        
        img_res <- getGoogleVisionResponse(input$file1$datapath,feature = "LABEL_DETECTION",numResults = input$numeric)
        data=data.frame(img_res[,c(2,3)])
        
        p1 <- plot_ly(x = ~round(data[,2],2), y = ~reorder(data[,1], data[,2]), 
                      type = 'bar', orientation = 'h',
                      marker = list(color = 'rgba(50, 171, 96, 0.6)',
                                    line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
          layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85),title="",tickfont=list(size=20)),
                 xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = F, showgrid = F,title=""
                 )) %>%
          add_annotations(xref = 'x1', yref = 'y',
                          x = data[,2]+0.05,  y = data[,1],
                          text = paste(round(data[,2], 2)*100, '%'),
                          font = list(family = 'Arial', size = 20, color = 'rgb(50, 171, 96)'),
                          showarrow = FALSE)
        p1
      })
    }
  })
     
  ###################################### LANDMARK #######################################################################
      
      
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
      output$outimage3 <- renderImage({
        shiny::withProgress(message = "Rendering Image", value = 0.1, {
          
          req(input$file1$datapath)
          
          shiny::incProgress(
            amount = 0, 
            detail = "Reading image and converting to image pointer with Magick"
          )
          
          photo = magick::image_read(input$file1$datapath)
          shiny::incProgress(0.1)
          # obtain photo dimensions
          photo_info = magick::image_info(photo)
          width = photo_info$width
          height = photo_info$height
          max_height <- 400
          rescale_lwd <- 2/max_height * height
          
          shiny::incProgress(amount = 0, detail = "Using graphics device to draw image")
          magick::image_draw(photo)  # draw photo with graphics device
          shiny::incProgress(amount = 0.3)
          
          img_res <- getGoogleVisionResponse(input$file1$datapath,feature = "LANDMARK_DETECTION")
            # If no features detected
            landmark_detected <- !is.null(img_res$boundingPoly$vertices)
            if(!landmark_detected) {
              text(x = width/2, y = height/2, labels = "No features detected",
                   col = "red", offset = 1, cex = 5)
              
            } else {
              shiny::incProgress(
                amount = 0, 
                detail = "Drawing landmark bounding boxes detected"
              )
              # save bounding boxes, and vector of descriptions and predictions
              bounding_boxes = img_res$boundingPoly$vertices
              name_land = img_res$description
              pred_score = img_res$score
              # output bounding boxes, descriptions, and predictions, with graphics
              mapply(vertice = bounding_boxes,
                     name_land = name_land, 
                     pred_score = pred_score,
                     function(vertice, name_land, pred_score) {
                       polygon(x = vertice[[1]], y = vertice[[2]], border = 'red', 
                               lwd = rescale_lwd)
                       text(x = vertice[[1]][[3]], y = vertice[[2]][[3]], 
                            labels = paste(as.character(name_land), 
                                           as.character(round(pred_score,2)*100),"%"),
                            offset = 1,
                            cex = 5, 
                            col = "green")
                     })
            }
            shiny::incProgress(
              amount = 0.3, 
              detail = "Capturing image and storing into magick image pointer"
            )
            # Save the graphics output into a magick image object
            photo = magick::image_capture()  
            shiny::incProgress(0.1)
            dev.off()
            
            tmpFile <- image_write(
              photo, 
              tempfile(fileext='jpg'),
              format = 'jpg'
            )  # tmpfile for output of image to shiny
            
            shiny::incProgress(0.1)
            
        }) # end withProgress bar
        
        # src is image to output, and height is max height of image on shiny output
        list(src = tmpFile,width = 900, height =580)
        
      })
    }
  })
    
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){  
      output$gauge3 <- renderLeaflet({
        landmark=getGoogleVisionResponse(pic(),feature = 'LANDMARK_DETECTION')
        latt = landmark$locations[[1]][[1]][[1]]
        lon = landmark$locations[[1]][[1]][[2]]
        m = leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = lon, lat = latt, zoom = 15) %>%
          addMarkers(lng = lon, lat = latt)
        m
      })
    }
  })
  
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
      output$lyp <- renderPlotly({
      y=getGoogleVisionResponse(pic(),feature = "LANDMARK_DETECTION")
      
      plot_ly(x = ~round(y[,3],2), y = ~reorder(y[,2], y[,3]), 
              type = 'bar', orientation = 'h',
              marker = list(color = 'rgba(50, 171, 96, 0.6)',
                            line = list(color = 'rgba(50, 171, 96, 1.0)', width = 0.05))) %>%
        layout(height = 100,
          yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85),title="",tickfont=list(size=20)),
               xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = F, showgrid = F,title="Detection Confidence",
                            titlefont=list(size=20,color="black")
               )) %>%
        add_annotations(xref = 'x1', yref = 'y',
                        x = y[,3]+0.03,  y = y[,2],
                        text = paste(round(y[,3], 2)*100, '%'),
                        font = list(family = 'Arial', size = 20, color = 'rgb(50, 171, 96)'),
                        showarrow = FALSE)
      })
    }
  })
      ################################################# LOGO ##############################################################
      
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
      output$gauge4 <- plotly::renderPlotly({
        
        img_res <- getGoogleVisionResponse(data(),feature = "LOGO_DETECTION")
        data=data.frame(description=img_res$description,score=img_res$score)
        
        p1 <- plot_ly(x = ~round(data[,2],2), y = ~reorder(data[,1], data[,2]), 
                      type = 'bar', orientation = 'h',
                      marker = list(color = 'rgba(50, 171, 96, 0.6)',
                                    line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
          layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85),title="",tickfont=list(size=20)),
                 xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = F, showgrid = F,title=""
                 )) %>%
          add_annotations(xref = 'x1', yref = 'y',
                          x = data[,2]+0.05,  y = data[,1],
                          text = paste(round(data[,2], 2)*100, '%'),
                          font = list(family = 'Arial', size = 20, color = 'rgb(50, 171, 96)'),
                          showarrow = FALSE)
        p1
      })
    }
  })
      
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){
      output$outimage4 <- renderImage({
        shiny::withProgress(message = "Rendering Image", value = 0.1, {
          
          req(input$file1$datapath)
          
          shiny::incProgress(
            amount = 0, 
            detail = "Reading image and converting to image pointer with Magick"
          )
          
          photo = magick::image_read(input$file1$datapath)
          shiny::incProgress(0.1)
          # obtain photo dimensions
          photo_info = magick::image_info(photo)
          width = photo_info$width
          height = photo_info$height
          max_height <- 400
          rescale_lwd <- 2/max_height * height
          
          shiny::incProgress(amount = 0, detail = "Using graphics device to draw image")
          magick::image_draw(photo)  # draw photo with graphics device
          shiny::incProgress(amount = 0.3)
          
          img_res <- getGoogleVisionResponse(input$file1$datapath,feature = "LOGO_DETECTION")
          logo_detected <- !is.null(img_res$boundingPoly$vertices)
          if (logo_detected) {
            shiny::incProgress(
              amount = 0, 
              detail = "Drawing bounding boxes detected for logo detection"
            )
            
            bounding_boxes <- img_res$boundingPoly$vertices
            mapply(x =bounding_boxes, y = c(1:length(bounding_boxes)), 
                   function(x, y) {
                     polygon(x = x[[1]], 
                             y = x[[2]], 
                             border = 'green', 
                             lwd = rescale_lwd)
                     text(x = x[[1]][[1]], 
                          y = x[[2]][[1]], 
                          labels = as.character(y),
                          offset = 1,
                          cex = 3, 
                          col = "white")
                   }) 
          } else {
            text(x = width/2, y = height/2, labels = "No features detected", 
                 col = "red", offset = 1, cex = 5)
          }
        
          shiny::incProgress(
            amount = 0.3, 
            detail = "Capturing image and storing into magick image pointer"
          )
          # Save the graphics output into a magick image object
          photo = magick::image_capture()  
          shiny::incProgress(0.1)
          dev.off()
          
          tmpFile <- image_write(
            photo, 
            tempfile(fileext='jpg'),
            format = 'jpg'
          )  # tmpfile for output of image to shiny
          
          shiny::incProgress(0.1)
          
        }) # end withProgress bar
        
        # src is image to output, and height is max height of image on shiny output
        list(src = tmpFile,width = 900, height =580)
        
      })
    }
  })
      
      
      #################################### object detection ####################################
     
      
      # 
      # output$outimage5 <- renderImage({
      #   shiny::withProgress(message = "Rendering Image", value = 0.1, {
      #     
      #     req(input$file1$datapath)
      #     
      #     shiny::incProgress(
      #       amount = 0, 
      #       detail = "Reading image and converting to image pointer with Magick"
      #     )
      #     
      #     photo = magick::image_read(input$file1$datapath)
      #     shiny::incProgress(0.1)
      #     # obtain photo dimensions
      #     photo_info = magick::image_info(photo)
      #     width = photo_info$width
      #     height = photo_info$height
      #     max_height <- 400
      #     rescale_lwd <- 2/max_height * height
      #     
      #     shiny::incProgress(amount = 0, detail = "Using graphics device to draw image")
      #     magick::image_draw(photo)  # draw photo with graphics device
      #     shiny::incProgress(amount = 0.3)
      #     
           # library(image.darknet)
           # yolo_tiny_voc <- image_darknet_model(type = "detect", 
           #                                      model = "tiny-yolo-voc.cfg", 
           #                                      weights = system.file(package="image.darknet", "models", "tiny-yolo-voc.weights"), 
           #                                      labels = system.file(package="image.darknet", "include", "darknet","data", "voc.names"))
           # 
           # 
           # model <- system.file(package="image.darknet", "include", "darknet", "cfg", "tiny.cfg")
           # weights <- system.file(package="image.darknet", "models", "tiny.weights")
           # labels <- system.file(package="image.darknet", "include", "darknet", "data", "imagenet.shortnames.list")
           # labels <- readLines(labels)
           # darknet_tiny <- image_darknet_model(type = 'detect',
           #                                     model = model, weights = weights, labels = labels)
           # ##
           # ## Classify new images alongside the model
           # ##
           # f <- system.file("include", "darknet", "data", "dog.jpg", package="image.darknet")
           # x <- image_darknet_detect(file = "C:\\Users\\SeaportAI\\Desktop\\visionAnalyze - Copy\\face.jpg", object = yolo_tiny_voc,threshold = 0.19)
           #  magick::image_read("predictions.png")
      #     
      #     shiny::incProgress(
      #       amount = 0.3, 
      #       detail = "Capturing image and storing into magick image pointer"
      #     )
      #     # Save the graphics output into a magick image object
      #     photo = magick::image_capture()  
      #     shiny::incProgress(0.1)
      #     dev.off()
      #     
      #     tmpFile <- image_write(
      #       photo, 
      #       tempfile(fileext='jpg'),
      #       format = 'jpg'
      #     )  # tmpfile for output of image to shiny
      #     
      #     shiny::incProgress(0.1)
      #     
      #   }) # end withProgress bar
      #   
      #   # src is image to output, and height is max height of image on shiny output
      #   list(src = tmpFile,width = 900, height =580)
      #   
      # })
      
      ########################################## TEXT #############################################
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){    
  output$outimage6 <- renderImage({
        shiny::withProgress(message = "Rendering Image", value = 0.1, {
          
          req(input$file1$datapath)
          
          shiny::incProgress(
            amount = 0, 
            detail = "Reading image and converting to image pointer with Magick"
          )
          
          photo = magick::image_read(input$file1$datapath)
          shiny::incProgress(0.1)
          # obtain photo dimensions
          photo_info = magick::image_info(photo)
          width = photo_info$width
          height = photo_info$height
          max_height <- 400
          rescale_lwd <- 2/max_height * height
          
          shiny::incProgress(amount = 0, detail = "Using graphics device to draw image")
          magick::image_draw(photo)  # draw photo with graphics device
          shiny::incProgress(amount = 0.3)
          
          img_res <- getGoogleVisionResponse(input$file1$datapath,feature = "TEXT_DETECTION")
          
          text_detected <- !is.null(img_res$boundingPoly$vertices)
          if (text_detected) {
            shiny::incProgress(
              amount = 0, 
              detail = "Drawing bounding boxes detected for text detection"
            )
            bounding_boxes <- img_res$boundingPoly$vertices
            mapply(x =bounding_boxes, y = c(1:length(bounding_boxes)), function(x, y) {
              polygon(x = x[[1]], 
                      y = x[[2]], 
                      border = 'green', 
                      lwd = rescale_lwd)
              text(x = x[[1]][[3]], 
                   y = x[[2]][[3]], 
                   labels = as.character(y),
                   offset = 1,
                   cex = 1, 
                   font = rescale_lwd * 6,
                   col = "white")
            }) 
            
          } else {
            text(x = width/2, y = height/2, labels = "No features detected", 
                 col = "red", offset = 1, cex = 5)
          }
          
          shiny::incProgress(
            amount = 0.3, 
            detail = "Capturing image and storing into magick image pointer"
          )
          # Save the graphics output into a magick image object
          photo = magick::image_capture()  
          shiny::incProgress(0.1)
          dev.off()
          
          tmpFile <- image_write(
            photo, 
            tempfile(fileext='jpg'),
            format = 'jpg'
          )  # tmpfile for output of image to shiny
          
          shiny::incProgress(0.1)
          
        }) # end withProgress bar
        
        # src is image to output, and height is max height of image on shiny output
        list(src = tmpFile,width = 900, height =580)
        
      })
    }
  })
  observeEvent(input$confirmation, {
    if(input$confirmation==TRUE){  
      output$gauge6 <- renderDataTable({
        # getGoogleVisionResponse("golf.jpg",feature = "TEXT_DETECTION")
        img_res <- getGoogleVisionResponse(input$file1$datapath,feature = "TEXT_DETECTION")
        x=data.frame(img_res[,c(1,2)])
        colnames(x) <- c("Language","Words")
        x
        
      }, extensions = c('Buttons'),editable = F,
      class   = DT:::DT2BSClass(c('compact', 'cell-border')),
      escape  = F, selection = 'multiple',
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',		
        deferRender = TRUE,
        buttons = c('excel', 'pdf', 'print'),
        autoWidth  = T,
        targets = 6,
        LengthMenu = c(5, 30, 50),
        columnDefs = list(list(
          className = 'dt-body-left'))
      ))
    }
  })
      
}



shinyApp(ui, server)