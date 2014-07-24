library(shiny)
is.letter <- function(x) grepl("[[:alpha:]]", x)
parse.function.string <- function(x){
    array <- vector()
    for(i in 1:nchar(x)){
        if(substr(x,i,i)!="x"){symbol<- substr(x,i,i)}
        else if(x=="x"){symbol <- "(x())"}
        else if((substr(x,i,i)=="x")&(is.letter(substr(x,i-1,i-1))==TRUE)){symbol <-"x"}
        else{symbol <- "(x())"}
        if(i==1){array <- symbol}
        else{array <- c(array, symbol)}
    }
    return(paste0(array, collapse=""))
}

shinyServer(
    function(input, output) {
        #x <- reactive({input$lower_boundery})
        #y <- reactive({eval(parse(text=input$function_string))})
        #gsub("x", input$function_string, "(x())")
        
        x <- reactive({seq(from=input$lower_boundery, 
                                  to=input$upper_boundery,
                                  length.out=input$chunks+1)})
        y <- reactive(({eval(parse(text=parse.function.string(input$function_string)))}))

        integral <- reactive(sum((x()[2:length(x())] - x()[1:(length(x())-1)])*((y()[2:length(y())] + y()[1:(length(y())-1)])/2)))
        


        output$graphic <- renderPlot({plot(x(), y())})
        output$integral <- renderText(as.character(integral()))
        
        
    }
)