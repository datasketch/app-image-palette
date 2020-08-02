library(paletero)

img_path <- "img/bogota.jpg"

#paletero::img_palette(img_path, include_bg = TRUE)
colors <- paletero::img_palette(img_path, 
                      n = 8,
                      n_quant = 4,
                      type = "cat",
                      include_bg = FALSE)
background <- paletero::img_background_color(img_path)
palette <- list(colors = colors, background = background)

data <- sample_data('Cat-Num', n = 20, nlevels = length(palette$colors))
gg_bar_CatNum(data, 
              title = "Title with font",
              color_by = names(data)[1],
              palette_colors = palette$colors, 
              background_color = palette$background,
              text_family = "IBMPlexSans"
              )


# Download fonts

font_url <- "https://github.com/google/fonts/raw/master/ofl/ibmplexsans/IBMPlexSans-Regular.ttf"
download.file(font_url, destfile = "fonts/IBMPlexSans-Regular.ttf")




font_url <- "https://github.com/google/fonts/raw/master/ofl/ibmplexsans/IBMPlexSans-Regular.ttf"
download.file(font_url, destfile = "fonts/IBMPlexSans-Regular.ttf")


