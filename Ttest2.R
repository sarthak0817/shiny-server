library(shiny)
require(shinyjs)
require(ggplot2)
require(nortest)
#library(xlsx)

ui<-fluidPage(
        headerPanel("T-Test V-2.1"),
        sidebarPanel(
                useShinyjs(debug = TRUE),
                selectInput("DataChoice","Data Format:",choices = c("Vector","Data Frame"),
                            selected = "Data Frame"),
                helpText("Please Upload the structured file"),
                fileInput("fileIP","Upload a csv file",multiple = F, accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")),
                hidden(numericInput("output_var","Enter the number of output variable",value=4)),
                hidden(uiOutput("var_one")),
                hidden(uiOutput("var_two")),
                uiOutput("choose_varone"),
                uiOutput("choose_vartwo"),
                tags$hr(),
                selectInput("alternative","Alternative:",choices = c("two.sided","greater","less")),
                checkboxInput("variance", "Equal Variance = True", T),
                checkboxInput("header", "Header", TRUE),
                hidden(uiOutput("plot_var")),
                hidden(uiOutput("plot_var2")),
                helpText("Results could be downloaded once they are displayed on screen"),
                downloadButton('downloadData', 'Download Results')
        ),
        mainPanel(
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                ),
                htmlOutput("tstats"),
                htmlOutput("fstats"),
                tags$hr(),
                p(strong("Normal Probability Plot"), style="color:green"),
                column(4,plotOutput("plot",height = 250, width = 300)),
                column(6,plotOutput("plot2",height = 250, width = 300)),
                column(8,tags$hr()),
                column(8,p(strong("Anderson-Darling normality test"), style="color:green")),
                column(5,tableOutput("adtest")),
                column(6,tableOutput("adtest2"))
                #,tableOutput("contents")
                
        )
)

server<-function(input,output){
        
        #Assign uploaded file to a oblect/global variable
        file<-reactive({inFile <- input$fileIP
        if (is.null(inFile))
                return(NULL)
        data<-read.csv(inFile$datapath, header = input$header)
        data
        })
        
        #Show contents of file
        output$contents <- renderTable({
                #file()[grep(as.character(input$name_var1),file()[,4]),]
                file()[,1]
        })
        
        #Show options for DataFrame
        observe({shinyjs::toggle("choose_varone",condition = input$DataChoice=="Data Frame")
                shinyjs::toggle("choose_vartwo",condition = input$DataChoice=="Data Frame")
                shinyjs::toggle("var_one",condition = input$DataChoice=="Data Frame")
                shinyjs::toggle("var_two",condition = input$DataChoice=="Data Frame")
                shinyjs::toggle("plot_var",condition = input$DataChoice=="Data Frame")
                shinyjs::toggle("plot_var2",condition = input$DataChoice=="Data Frame")
                shinyjs::toggle("output_var",condition = input$DataChoice=="Data Frame")
        })
        
        #Disply option to enter names of variables through user
        output$var_one=renderUI({
                items=names(file())
                names(items)=items
                textInput("name_var1", "Enter the name of first variable:")
        })
        output$var_two=renderUI({
                items=names(file())
                names(items)=items
                textInput("name_var2", "Enter the name of second variable:")
        })
     
        #Display coulumns dynamically to plot the graph
        output$plot_var=renderUI({
                items=names(file())
                names(items)=items
                selectInput("plot_col", "Select Columns to plot:",items)
        })
        output$plot_var2=renderUI({
                items=names(file())
                names(items)=items
                selectInput("plot_col2", "Select Columns to plot:",items)
        })
        #Disply Plots
        output$plot=renderPlot({
                col_name<-input$plot_col
                if(input$DataChoice=="Data Frame"){
                ggplot(file()[grep(as.character(input$name_var1),file()[,input$output_var]),]
                       ,aes_string(sample=col_name))+stat_qq()}
                else if(input$DataChoice=="Vector"){
                        ggplot(file(), aes_string(sample=file()[,1]))+stat_qq()
                }
        })
        output$plot2=renderPlot({
                col_name<-input$plot_col2
                if(input$DataChoice=="Data Frame"){
                ggplot(file()[grep(as.character(input$name_var2),file()[,input$output_var]),]
                       ,aes_string(sample=col_name))+stat_qq()}
                else if(input$DataChoice=="Vector"){
                        ggplot(file(), aes_string(sample=file()[,2]))+stat_qq()
                }
        })
        
        #Anderson Darling test
        output$adtest=renderTable({
                if(input$DataChoice=="Data Frame"){
                        data<-file()[grep(as.character(input$name_var1),file()[,input$output_var]),]
                        text<-ad.test(data[,input$plot_col])}
                else if(input$DataChoice=="Vector"){
                        text<-ad.test(file()[,1])
                }
                Statistics<-as.data.frame(unlist(text),row.names = NULL)
                Statistics<-Statistics[-c(3,4),]
                Statistics<-as.data.frame(Statistics)
                Value<-data.frame()
                Value<-c("A statistics", "p-value")
                Value<-cbind(Value,Statistics)
                
        })
        output$adtest2=renderTable({
                if(input$DataChoice=="Data Frame"){
                        data<-file()[grep(as.character(input$name_var2),file()[,input$output_var]),]
                        text<-ad.test(data[,input$plot_col2])}
                else if(input$DataChoice=="Vector"){
                        text<-ad.test(file()[,2])
                }
                Statistics<-as.data.frame(unlist(text),row.names = NULL)
                Statistics<-Statistics[-c(3,4),]
                Statistics<-as.data.frame(Statistics)
                Value<-data.frame()
                Value<-c("A statistics", "p-value")
                Value<-cbind(Value,Statistics)
                
        })
        
        
        #T-Stats 
        output$tstats<-renderUI({
                if (input$DataChoice=="Vector"){
                        y<-t.test(file()[,1], file()[,2], var.equal=input$variance,alternative = input$alternative)
                }
                else if(input$DataChoice=="Data Frame"){
                        y<-t.test(file()[,1]~ file()[,4], var.equal=input$variance,alternative = input$alternative)
                }
                y=unlist(y)
                y=y[-11]
                FinalDF=data.frame(y)
                HTML(paste(a(strong("T-test"), style="color:green"),paste("t = ",y[1]),paste("df = ",y[2]),paste("p-value = ",y[3]),
                           paste("95% confidence of mean 1 - mean 2 = ","(",FinalDF[4,],",",FinalDF[5,],")"),sep = '<br/>'))
        })
        
        #F-Stats
        output$fstats<-renderUI({
                if (input$DataChoice=="Vector"){
                        f<-var.test(file()[,1], file()[,2],conf.level = 0.95)
                }
                else if(input$DataChoice=="Data Frame"){
                        f<-var.test(file()[,1]~ file()[,4],conf.level = 0.95)
                }
                f=unlist(f)
                f=f[-c(11,10,9,8,3,2,1)]
                FinalDFF=data.frame(f)
                if (as.numeric(as.character(FinalDFF[1,])) < 0.05 & input$variance==T){
                        display<-("Assumptions of equal variance seems to be invalid since 
                                  p-value is less than 0.05")
                }else if(as.numeric(as.character(FinalDFF[1,])) > 0.05 & as.numeric(as.character(FinalDFF[1,])) < 0.10 & input$variance==T){
                        display<-("You may like to reconsider assumption of equal variance
                                  because p-value is between 0.05 & 0.10")
                }else if(as.numeric(as.character(FinalDFF[1,])) >= 0.80 & input$variance==F){
                        display<-("You may like to consider equal variance because p-value is 
                                  greater than or equal to 0.80")
                }else display<-""
                HTML(paste(a(strong("F-test"), style="color:green"),paste("p - value = ",as.numeric(as.character(FinalDFF[1,]))),
                           paste("95 % confidence interval of ratio of variance = ","( ",FinalDFF[2,],", ",FinalDFF[3,]," )"),
                           paste("Ratio of sample estimates sof variances = ",FinalDFF[4,]),"<br/>",display,
                           sep = '<br/>'))
                })
}
shinyApp(ui=ui,server=server)

