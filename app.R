library(shiny)
library(shinypanels)
library(shinyinvoer)
library(dsmodules)
library(paletero)
library(homodatum)
library(ggmagic)
library(pins)
library(dspins)

if(Sys.info()[['sysname']] == 'Linux'){
  message("create .fonts")
  dir.create('~/.fonts')
  message("copy to .fonts")
  file.copy("fonts/IBMPlexSans-Regular.ttf", "~/.fonts")
  message("fc-cache -f ~/.fonts")
  system('fc-cache -f -v ~/.fonts')
  message("\n\nls ~/.fonts\n\n")
  system('ls ~/.fonts')
  message("\n\nfc-list\n\n")
  system('fc-list')
  message("\n\nfc-match IBM Plex Sans\n\n")
  system('fc-match IBM Plex Sans')
}

user_name <- "brandon"
user_id <- "5efa17497caa2b00156a6468"

pin.fringe <- function(f, name = NULL, description = NULL, board = NULL, ...) {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  saveRDS(f, file.path(path, "data.rds"), version = 2)
  
  fringe_write(f, path = path, overwrite_dic = TRUE)
  metadata <- f$meta
  metadata$title <- f$name
  metadata$stats <- f$stats
  
  args <- list(...)
  if(!is.null(args$user_id)){
    board <- board_name(args$user_id)
  }
  
  assign("board", board, envir = globalenv())
  assign("path", path, envir = globalenv())
  assign("f", f, envir = globalenv())
  assign("metadata", metadata, envir = globalenv())
  #upload_url <- paste0("https://s3.amazonaws.com/",board_name(user_id),"/some-file")
  upload_url <- tryCatch(board_pin_store(board, path, f$slug, f$description, "fringe",
                                         extract = FALSE,
                                         metadata,...),
                         error = function(e){
                           upload_url
                         },
                         finally = {
                           # message("Fringe uploaded to: ", upload_url)
                         })
  upload_url
}



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
    title = "Preview Palette", 
    title_plugin = downloadTableUI("download_plot", dropdownLabel = "Download", text = "Download", formats = c("link", "csv", "xlsx"),
                                   display = "dropdown", dropdownWidth = 170, getLinkLabel = "Get link", modalTitle = "Get link",
                                   modalBody = NULL),
    color = "chardonnay",
    can_collapse = FALSE,
    width = NULL,
    body = list(
      h4("Palette:"),
      verbatimTextOutput("palette_text"),
      p("Last value is the background"),
      br(),
      h4("How it looks in a data visualization"),
      plotOutput("plot")
    )
  ),
  showDebug(hosts = c("127.0.0.1","randommonkey.shinyapps.io"))
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
    req(img_path())
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
                  title = "This is a chart",
                  #text_family = "Ubuntu",
                  color_by = names(data)[1],
                  palette_colors = input$palette, 
                  background_color = input$background)
  })
  
  
  # output$download <- renderUI({
  #   downloadTableUI("download_plot", dropdownLabel = "Save/Download", 
  #                   formats = c("link","csv", "xlsx"), 
  #                   display = "dropdown",
  #                   getLinkLabel = "Save to library",
  #                   modalTitle = "Save to library"
  #                   )
  # })
  # 
  saveFringeUrl <- function(element, user_id, user_name, fringe_name, ...) {
    
    if (is.reactive(fringe_name)) fringe_name <- fringe_name()
    if (is.reactive(element)) element <- element()
    args <- lapply(list(...), function(s) {
      if (is.reactive(s)) {
        s()
      } else {
        s
      }
    })
    assign("l0", args, envir = globalenv())
    args$name <- fringe_name
    args$slug <- fringe_name
    if (!is_fringe(element)) {
      element <- fringe(element)
    }
    f <- modifyList(element, args)
    Sys.setlocale(locale = "en_US.UTF-8")
    dspins_user_board_connect(user_id)
    message("\n\nSAVING PIN\n\n")
    pin_url <- pin(f, user_id = user_id)
    message("\n\nSAVED PIN\n\n", pin_url)
    
    url <-  paste0(user_name, ".datasketch.co/", f$name)
    if (is.null(pin_url)) url <- "pinnotfound"
    url
  }
  
  palette_table <- reactive({
    # cars
    req(img_colors())
    palette <- img_colors()
    table <- data.frame(palette = c(palette$colors, palette$background),
                        stringsAsFactors = FALSE)
    fringe(table)
  })
  # 
  # callModule(downloadTable, "download_plot", table = palette_table(), 
  #            formats = c("link","csv", "xlsx"), 
  #            modalFunction = paste,
  #            modalFunctionArgs = saveUrl(palette_table(), user_id, user_name))
  # 
  # 
  
  # observe({
  #   req(palette_table())
  #   # print("W")
  #   # print(input$`download_plot-link-name`)
  #   # print(input$`download_plot-slug`)
  #   # print(input$`download_plot-description`)
  #   # print(input$`download_plot-license`)
  #   # print(input$`download_plot-tags`)
  #   # print(input$`download_plot-category`)
  # })
  
  callModule(downloadTable, "download_plot", table = reactive(palette_table()$data), name = "table",
             formats = c("link", "csv", "xlsx"), modalFunction = saveFringeUrl, 
             element = reactive(palette_table()), user_id = user_id, user_name = user_name, 
             fringe_name = reactive(input$`download_plot-link-name`),
             slug = input$`download_plot-slug`,
             description = input$`download_plot-description`, license = input$`download_plot-license`,
             tags = input$`download_plot-tags`, category = input$`download_plot-category`)
  
  
}

shinyApp(ui, server)

