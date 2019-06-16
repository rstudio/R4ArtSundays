library(shiny)
library(ggplot2)
CreatePlot = function (ang=pi*(3-sqrt(5)), nob=150, siz=15, alp=0.8, sha=16, col="black", bac="white") {
    ggplot(data.frame(r=sqrt(1:nob), t=(1:nob)*ang*pi/180), aes(x=r*cos(t), y=r*sin(t)))+
        geom_point(colour=col, alpha=alp, size=siz, shape=sha)+
        scale_x_continuous(expand=c(0,0), limits=c(-sqrt(nob)*1.4, sqrt(nob)*1.4))+
        scale_y_continuous(expand=c(0,0), limits=c(-sqrt(nob)*1.4, sqrt(nob)*1.4))+
        theme(legend.position="none",
              panel.background = element_rect(fill=bac),
              panel.grid=element_blank(),
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              axis.text=element_blank())}
shinyApp(
    ui = fluidPage(
        titlePanel("Phyllotaxis by Shiny"),
        fluidRow(
            column(3,
                   wellPanel(
                       selectInput("col", label = "Colour of points:", choices = colors(), selected = "black"),
                       selectInput("bac", label = "Background colour:", choices = colors(), selected = "white"),
                       selectInput("sha", label = "Shape of points:",
                                   choices = list("Empty squares" = 0, "Empty circles" = 1, "Empty triangles"=2,
                                                  "Crosses" = 3, "Blades"=4, "Empty diamonds"=5,
                                                  "Inverted empty triangles"=6, "Bladed squares"=7,
                                                  "Asterisks"=8, "Crosed diamonds"=9, "Crossed circles"=10,
                                                  "Stars"=11, "Cubes"=12, "Bladed circles"=13,
                                                  "Filled squares" = 15, "Filled circles" = 16, "Filled triangles"=17,
                                                  "Filled diamonds"=18), selected = 16),
                       sliderInput("ang", label = "Angle (degrees):", min = 0, max = 360, value = 180*(3-sqrt(5)), step = .05),
                       sliderInput("nob", label = "Number of points:", min = 1, max = 1500, value = 60, step = 1),
                       sliderInput("siz", label = "Size of points:", min = 1, max = 60, value = 10, step = 1),
                       sliderInput("alp", label = "Transparency:", min = 0, max = 1, value = .5, step = .01)
                   )
            ),
            mainPanel(
                plotOutput("Phyllotaxis")
            )
        )
    ),
    server = function(input, output) {
        output$Phyllotaxis=renderPlot({
            CreatePlot(ang=input$ang, nob=input$nob, siz=input$siz, alp=input$alp, sha=as.numeric(input$sha), col=input$col, bac=input$bac)
        }, height = 650, width = 650 )}
)