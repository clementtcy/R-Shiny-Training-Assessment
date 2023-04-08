library(shiny)

ui <- fluidPage(
  fileInput("file1","Upload your CSV (WARNING: Please clear number format) here: "),
  numericInput("tfactor", "Enter desired tail factor: ",value = 1.0),
  textOutput("cumtri"),
  tableOutput("table1"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file1$datapath)
  })
  output$table1 <- renderTable({
    df <- read.csv(data())
    colnames(df) <- c("LY", "DY", "CLM")
    tri <- matrix(
      nrow=length(unique(df$LY)),
      ncol=length(unique(df$DY)),
      dimnames=list(
        ORIGIN=sort(unique(df$LY)),
        DEV=sort(unique(df$DY))
      )
    )
    
    tri[cbind(factor(df$LY),df$DY)] <- df$CLM
    tricum = t(apply(tri, 1, cumsum))
    tricum
    
    n <- max(df$DY)
    f <- sapply(1:(n-1),
                function(i){
                  sum(tricum[c(1:(n-i)),i+1])/sum(tricum[c(1:(n-i)),i])
                }
    )
    f.tail <- input$tfactor
    
    f <- c(f, f.tail)
    fulltricum <- cbind(tricum, rep(0,n))
    for(k in 1:n) {
      fulltricum[(n-k+1):n, k+1] <- fulltricum[(n-k+1):n,k]*f[k]
      
    }
    colnames(fulltricum)[n+1] <- n+1
    round(fulltricum)
    
  }, rownames = TRUE)
  
  output$plot <- renderPlot({
    df <- read.csv(data())
    colnames(df) <- c("LY", "DY", "CLM")
    tri <- matrix(
      nrow=length(unique(df$LY)),
      ncol=length(unique(df$DY)),
      dimnames=list(
        ORIGIN=sort(unique(df$LY)),
        DEV=sort(unique(df$DY))
      )
    )
    
    tri[cbind(factor(df$LY),df$DY)] <- df$CLM
    tricum = t(apply(tri, 1, cumsum))
    tricum
    
    n <- max(df$DY)
    f <- sapply(1:(n-1),
                function(i){
                  sum(tricum[c(1:(n-i)),i+1])/sum(tricum[c(1:(n-i)),i])
                }
    )
    f.tail <- input$tfactor
    
    f <- c(f, f.tail)
    fulltricum <- cbind(tricum, rep(0,n))
    for(k in 1:n) {
      fulltricum[(n-k+1):n, k+1] <- fulltricum[(n-k+1):n,k]*f[k]
      
    }
    colnames(fulltricum)[n+1] <- n+1
    matplot(t(fulltricum), type = "l", lwd = 2, main="Cumulative Paid Claims", 
            ylab = "Cumulative Paid Claims ($)", xlab = "Development Year",
            pch = 19, col = c(1:nrow(fulltricum)))
    LossYear <- rownames(fulltricum)
    legend("topleft", inset = 0.01, legend = LossYear, pch = 19, col = c(1:nrow(fulltricum)), horiz = FALSE, cex = 1.0)
  })
  
  output$cumtri <- renderText("Cumulative Paid Claims Triangle:")
  
}

shinyApp(ui, server)