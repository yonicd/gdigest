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
#' @importFrom plyr ldply
#' @import shiny 
memofit <- function(data, db, f) {
  
  synch_remote(action = 'pull')
  
  server <- function(input, output, session) {
    
    shiny::observeEvent(plot_data(),{
      if(!is.null(plot_data())){
        output$fitPlot <- shiny::renderPlot({
          ggplot2::ggplot()+
            ggplot2::geom_point(data=plot_data()$xy,ggplot2::aes(x=x,y=y,colour=subjid))+
            ggplot2::geom_line(data=plot_data()$fitgrid,ggplot2::aes(x=x,y=y,colour=subjid))+
            ggplot2::scale_x_continuous(limits=input$x_range)
        })}
    })
    
    
    fit <- shiny::eventReactive(input$go,{
      memoise_wrapper(f=f,db=db, dat = smc, y_var = input$y_var, method=input$method)  
    })
    
    
    plot_data <- shiny::reactive({
      suppressMessages({
        
        fit_traj <- lapply(as.numeric(input$subjid), function(x,fit0){
          fit_trajectory(subset(smc, subjid == x), fit = fit0) 
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
        xy <- plyr::ldply(fit_traj,function(x) x$xy,.id='subjid')
        fitgrid <- plyr::ldply(fit_traj,function(x) x$fitgrid,.id='subjid')
      }
      
      return(list(xy=xy,fitgrid=fitgrid))
      
    })
    
    output$y_var <- shiny::renderUI({
      shiny::selectInput(inputId = 'y_var',
                         label = 'conditional variable',
                         choices = names(smc), 
                         selected = 'haz')
    })
    
    output$method <- shiny::renderUI({
      shiny::selectInput(inputId = 'method',
                         label = 'fit method',
                         choices = hbgd::get_avail_methods(), 
                         selected = 'fda')
    })
    
    output$subjid <- shiny::renderUI({
      shiny::selectInput(inputId = 'subjid',
                         label = 'select subject to plot',
                         choices = as.character(unique(smc$subjid)), 
                         multiple = TRUE,
                         selected = '10001')
    })
    
    shiny::observeEvent(input$exit,{
      synch_remote(action = 'push')
      shiny::stopApp()
    })
    
    output$prerun <- shiny::renderUI({
      
      FILES <- list.files('.rcache',pattern = '^_')
      
      shiny::selectInput(inputId = 'prerun',
                         label = 'Pre Run Fits',
                         choices = FILES,
                         selected = FILES[1])
    })
    
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
        shiny::mainPanel(shiny::plotOutput("fitPlot"))
      )
    ))
  
  shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 450))
  
}
