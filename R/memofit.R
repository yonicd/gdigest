#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param db PARAM_DESCRIPTION
#' @param f PARAM_DESCRIPTION
#' @param synch_cache PARAM DESCRIPTION, Default: FALSE
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
memofit <- function(data, db, f, synch_cache=FALSE) {
  
  if(synch_cache) synch_remote(action = 'pull')
  
  
  
  server <- function(input, output, session) {
  
    FILES <- shiny::reactivePoll(1000,
                        session = session,
                        checkFunc = {
                          function() {list.files('.rcache',pattern = '^_')}
                        },
                        valueFunc = {
                          function() {list.files('.rcache',pattern = '^_')}
                        })
      
    shiny::observeEvent(plot_data(),{
      if(length(plot_data())>0){
        output$fitPlot <- shiny::renderPlot({
          ggplot2::ggplot()+
            ggplot2::geom_point(data=plot_data()$xy,ggplot2::aes(x=x,y=y,colour=subjid))+
            ggplot2::geom_line(data=plot_data()$fitgrid,ggplot2::aes(x=x,y=y,colour=subjid))+
            ggplot2::scale_x_continuous(limits=input$x_range)+
            ggplot2::labs(y=input$y_var,title=sprintf('The fit method "%s" was applied',input$method), x='Days')
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
    
    output$subjid <- shiny::renderUI({
      subjs <- as.character(unique(data$subjid))
      shiny::selectInput(inputId = 'subjid',
                         label = 'select subject to plot',
                         choices = subjs, 
                         multiple = TRUE,
                         selected = subjs[1])
    })
    
    shiny::observeEvent(input$exit,{
      if(synch_cache) synch_remote(action = 'push')
      shiny::stopApp()
    })
    
    output$fprerun <- shiny::renderUI({
      
      if(length(FILES())==0){FL = NULL}else{FL=FILES()[1]}
      
      shiny::selectInput(inputId = 'fprerun',
                         label = 'Filtered Pre Run Fits',
                         choices = FILES(),
                         selected = FL)
    })
    
    observeEvent(input$fprerun,{
      
      sel <- names(data)[1]
      
        if(length(list.files(db$path))>0)
          sel <- readRDS(file.path(db$path,input$fprerun))$y_var
      
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
        
        if(length(list.files(db$path))>0)
           sel <- readRDS(file.path(db$path,input$fprerun))$method
        
        shiny::selectInput(inputId = 'method',
                           label = 'fit method',
                           choices = methods, 
                           selected = sel
        )
      })
      
    })
    
    output$choiceTbl <- DT::renderDataTable({
      df <- as.data.frame(t(sapply(FILES(),
                                   function(x){
                                     readRDS(file.path(db$path,x))[c('y_var','method')]
                                   })
                            )
                      )
      
      df$y_var <- unlist(df$y_var)
      df$method <- unlist(df$method)
      
      DT::datatable(df,caption = 'Fits that have been cached',
                    filter = 'top',
                    selection="multiple", 
                    escape=FALSE, 
                    colnames = c('Response','Fit Method'),
                    options = list(paging=FALSE,
                                   autoWidth = TRUE,
                                   bInfo=FALSE,
                                   sDom  = '<"top">lrt<"bottom">ip'))
    })
    
    observeEvent(input$choiceTbl_rows_all,{
      shiny::updateSelectInput(inputId = 'fprerun',session = session,
                         choices = FILES()[input$choiceTbl_rows_all])
    })
    
    observeEvent(input$choiceTbl_rows_selected,{
      shiny::updateSelectInput(inputId = 'fprerun',session = session,
                               choices = FILES()[input$choiceTbl_rows_selected])
    })
    
    output$tbl <- DT::renderDataTable({
      
      if(length(list.files(db$path))>0){
        df <- readRDS(file.path(db$path,input$fprerun))$dat
      }else{
        df <- data 
      }
      
      DT::datatable(df,
                    filter = 'top',
                    selection="multiple", 
                    escape=FALSE,
                    extensions = c('Scroller','FixedHeader','FixedColumns'),
                    options = list(autoWidth = TRUE,
                                   sDom  = '<"top">lrt<"bottom">ip',
                                   dom = 't',
                                   colReorder = TRUE,
                                   scrollX = TRUE,
                                   fixedColumns = TRUE,
                                   fixedHeader = TRUE,
                                   deferRender = TRUE,
                                   scrollY = 500,
                                   scroller = TRUE))
      
    })
    
  }
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(title = 'Memoised HBGD Trajectory Fit',
                           left = miniUI::miniTitleBarButton( "exit", "Quit"),
                           right = miniUI::miniTitleBarButton(inputId = "go","Run",primary = TRUE)),
    miniUI::miniContentPanel(
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::wellPanel(                            
          DT::dataTableOutput('choiceTbl'),
          hr(),
          shiny::uiOutput('fprerun')),
          shiny::uiOutput('subjid'),
          shiny::column(6,shiny::uiOutput('y_var')),
          shiny::column(6,shiny::uiOutput('method')),
          shiny::sliderInput(inputId = "x_range",label =  "Days Range", min = 0, max = 1000, value = c(0,755))
        ),
        shiny::mainPanel(
          shiny::tabsetPanel(
            shiny::tabPanel(
              title = 'Plot',shiny::plotOutput("fitPlot")),
          shiny::tabPanel(
              title = 'Fit Data',
              DT::dataTableOutput('tbl')
              )
          )
        )
      )
    ))
  
  shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 450))
  
}
