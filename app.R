library(shiny)
library(shinypanels)
library(shinyinvoer)
library(dsmodules)
library(paletero)
library(homodatum)
library(ggmagic)

ui <-   panelsPage(
  panel(
    title = "Image Upload", 
    width = 250,
    body = list(
      imageInputUI("imageIn",
                   selected = "sampleData",
                   choices = list("Sample images" = "sampleData",
                                  "Upload" = "fileUpload",
                                  "URL" = "url")),
      imageOutput("img_preview"),
      verbatimTextOutput("debug")
    )
  ), 
  panel(
    title = "Options", 
    width = 250,
    body = list(
      uiOutput("color_palette"),
      sliderInput("n_colors", "How many colors", 
                  min = 2, max = 12, value = 4),
      hr(),
      # selectInput("palette_type", "Type", 
      #             choices = c("Categorical" = "cat", "Numeric" = "num", "Sequential" = "seq")),
      sliderInput("n_quant", "Preprocess Quantaization N Colors", 
                  min = 2, max = 30, value = 14),
      numericInput("fuzz", "Fuzz (to calculate background)", value = 12, min = 4)
    )
  ),
  panel(
    title = "Upload Data", 
    width = NULL,
    body = list(
      h4("Palette:"),
      verbatimTextOutput("palette_text"),
      p("Last value is the background"),
      br(),
      h4("How it looks in a data visualization"),
      plotOutput("plot")
    )
  )
)



server <- function(input, output, session) {
  
  inputImage <- callModule(imageInput,
                           "imageIn",
                           sampleFile = list("We can do it" = "img/we-can-do-it.jpg",
                                             "Bogotá" = "img/bogota.jpg"),
                           infoList = list("url" =  "Image address"))
  
  output$debug <- renderPrint({
    # list(
    #   n = input$n_colors,
    #   type = "cat", # input$palette_type
    #   n_quant = input$n_quant,
    #   fuzz = input$fuzz
    # )
    img_colors()
  })
  
  output$img_preview <- renderImage({
    inputImage()
  }, deleteFile = FALSE)
  
  img_path <- reactive({
    inputImage()$src
  })
  
  img_colors <- reactive({
    # if(is.null(img_path())) return()
    input$n_colors
    pal_colors <- paletero::img_palette(img_path(), 
                                    n = input$n_colors,
                                    type = "cat", #input$palette_type,
                                    n_quant = input$n_quant,
                                    fuzz = input$fuzz,
                                    include_bg = FALSE)
    background <- paletero::img_background_color(img_path())
    list(colors = pal_colors, background = background)
  })
  
  output$color_palette <- renderUI({
    palette <- img_colors()
    list(
      colorPaletteInput("palette", label = "Palette", colors = palette$colors),
      colorPaletteInput("background", label = "Background", colors = palette$background)
    )
  })
  
  output$palette_text <- renderText({
    palette <- img_colors()
    c(palette$colors, palette$background)
  })
  
  output$plot <- renderPlot({
    if(is.null(input$palette)) return()
    palette <- img_colors()
    n <- length(palette$colors)
    data <- sample_data('Cat-Num', n = 20, 
                        addNA = FALSE,loremNames = TRUE,
                        nlevels = min(length(palette$colors), length(input$palette)))
    gg_bar_CatNum(data, 
                  color_by = names(data)[1],
                  palette_colors = input$palette, 
                  background_color = input$background)
  })
  
}

shinyApp(ui, server)

