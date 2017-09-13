#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param db PARAM_DESCRIPTION
#' @param f PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[hbgd]{get_avail_methods}}
#' @rdname memofit
#' @export 
#' @author Jonathan Sidi
#' @importFrom ggplot2 ggplot geom_point aes geom_line scale_x_continuous
#' @importFrom hbgd get_avail_methods
#' @importFrom miniUI miniPage gadgetTitleBar miniTitleBarButton miniContentPanel
#' @importFrom purrr map_df
#' @import shiny 
memofit <- function(data, db, f) {
  
  synch_remote(action = 'pull')
  
  server <- function(input, output, session) {
    
    shiny::observeEvent(plot_data(),{
      if(length(plot_data())>0){
        output$fitPlot <- shiny::renderPlot({
          ggplot2::ggplot()+
            ggplot2::geom_point(data=plot_data()$xy,ggplot2::aes(x=x,y=y,colour=subjid))+
            ggplot2::geom_line(data=plot_data()$fitgrid,ggplot2::aes(x=x,y=y,colour=subjid))+
            ggplot2::scale_x_continuous(limits=input$x_range)
        })}
    })
    
    
    fit <- shiny::eventReactive(input$go,{
      
      if(input$method=='face')
        if(!'face'%in%row.names(installed.packages()))
          devtools::install_github('HBGDki/face')
      
      memoise_wrapper(f=f,db=db, dat = data, y_var = input$y_var, method=input$method)  
    })
    
    
    plot_data <- shiny::eventReactive(fit(),{
      suppressMessages({
        
        fit_traj <- lapply(as.numeric(input$subjid), function(x,fit0){
          hbgd::fit_trajectory(subset(data, subjid == x), fit = fit0) 
        },fit0=fit())
        
      })
      
      if(length(input$subjid)==1){
        
        fit_traj <- fit_traj[[1]]
        
        xy <- fit_traj$xy
        xy$subjid <- input$subjid
        fitgrid <- fit_traj$fitgrid
        fitgrid$subjid <- input$subjid
        
      }else{
        names(fit_traj) <- input$subjid
        xy <- purrr::map_df(fit_traj,.f=function(x) x$xy,.id='subjid')
        fitgrid <- purrr::map_df(fit_traj,.f=function(x) x$fitgrid,.id='subjid')
      }
      
      return(list(xy=xy,fitgrid=fitgrid))
      
    })
    
    observeEvent(input$prerun,{
      
      sel <- names(data)[1]
      
      if(length(list.files('.rcache',pattern = '^_'))>0)
        sel <- readRDS(file.path(db$path,input$prerun))$y_var

      output$y_var <- shiny::renderUI({
        shiny::selectInput(inputId = 'y_var',
                           label = 'conditional variable',
                           choices = names(data), 
                           selected = sel
        )
      })  
      
      output$method <- shiny::renderUI({
        
        methods <- hbgd::get_avail_methods()

        sel <- methods[1]
                
        if(length(list.files('.rcache',pattern = '^_'))>0)
          sel <- readRDS(file.path(db$path,input$prerun))$method

        shiny::selectInput(inputId = 'method',
                           label = 'fit method',
                           choices = methods, 
                           selected = sel
        )
      })
      
    })
    
    
    
    
    output$subjid <- shiny::renderUI({
      subjs <- as.character(unique(data$subjid))
      shiny::selectInput(inputId = 'subjid',
                         label = 'select subject to plot',
                         choices = subjs, 
                         multiple = TRUE,
                         selected = subjs[1])
    })
    
    shiny::observeEvent(input$exit,{
      synch_remote(action = 'push')
      shiny::stopApp()
    })
    
    output$prerun <- shiny::renderUI({
      
      FILES <- list.files('.rcache',pattern = '^_')
      
      if(length(FILES)==0){FL = NULL}else{FL=FILES[1]}
      
      shiny::selectInput(inputId = 'prerun',
                         label = 'Pre Run Fits',
                         choices = FILES,
                         selected = FL)
    })
    
    output$tbl <- shiny::renderDataTable(
      readRDS(file.path(db$path,input$prerun))$dat
    )
    
  }
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(title = 'Memoised HBGD Trajectory Fit',
                           left = miniUI::miniTitleBarButton( "exit", "Quit"),
                           right = miniUI::miniTitleBarButton(inputId = "go","Run",primary = TRUE)),
    miniUI::miniContentPanel(
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::uiOutput('prerun'),
          shiny::uiOutput('subjid'),
          shiny::sliderInput(inputId = "x_range",label =  "Days Range", min = 0, max = 1000, value = c(0,755)),
          shiny::uiOutput('y_var'),
          shiny::uiOutput('method')
        ),
        shiny::mainPanel(
          shiny::tabsetPanel(
            shiny::tabPanel(
              title = 'Plot',shiny::plotOutput("fitPlot")),
          shiny::tabPanel(
              title = 'Prerun Setting',
              shiny::dataTableOutput('tbl')
              )
          )
        )
      )
    ))
  
  shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 450))
  
}
