# Funktion, die die gecropte Zahl in ein Raster einteilt und zaehlt, wie viele
# Eintraege in jeder Zelle sind
conversion <- function(x, y) {
  require(raster)
  mxy <- max(y); mny <- min(y)
  mxx <- max(x); mnx <- min(x)
  # wenn Bild zu wenig breit is, fuege Pixel an den Aussen hinzu
  if ((mxy - mny) / (mxx - mnx) > 1.8) {mxx <- mxx + ((mxy - mny) / 2.5); mnx <- mnx - ((mxy - mny) / 2.5)}
  r <- raster(nrows = 14, ncols = 10, xmn = mnx, xmx = mxx, ymn = min(y), ymx = max(y))
  d <- data.frame(x = x, y = y)
  # weil erfasste Punkte weit auseinander liegen koennen, werden rekursiv 
  # zwischen allen Punkten dens-2 Punkte eingefuegt
  dens <- 14
  tmp <- data.frame(x = rep(NA, dens), y = rep(NA, dens))
  for (i in 1:(length(x)-1)) {
    tmp$x <- seq(x[i], x[i + 1], length.out = dens)
    tmp$y <- seq(y[i], y[i + 1], length.out = dens)
    d <- rbind(d, tmp[-c(1, dens), ])
  }
  r2 <- rasterize(d, r, fun = 'count')
  return(r2)
} 

# take raster (and counts; set NAs to 0, and make max count = 1)
counter <- function(frm) {
  pts <- data.frame(coordinates(frm), count=values(frm))
  pts$count[which(is.na(pts$count))] <- 0
  pts$count[pts$count > 1] <- 1
  return(pts)
}

# function that produces data to train nn
train.fct <- function(cnt, number) {
  train <- data.frame(matrix(c(number, cnt$count), nrow = 1, byrow = TRUE))
  # fix: hier koennte zB git repo eingefuegt werden
  load("~/git/lnf/code/NeuralNet/data/train_nn_14x10.rda")
  train_s <- rbind(train_s, train)
  save(train_s, file = "~/git/lnf/code/NeuralNet/data/train_nn_14x10.rda")
  return(train)
}

# function that predicts drawn number
pred.fct <- function(cnt) {
  tbp <- matrix(cnt$count, nrow = 1, byrow = TRUE) 
  model <- load_model_hdf5("~/git/lnf/code/NeuralNet/data/keras_14x10_softmax.h5")
  probs_pred <- model %>% predict(matrix(tbp, nrow = 1))
  return(probs_pred)
}

# Funktion, die Gewichte fuer grafische Darstellung vom NN generiert
wgts <- function(inneu) {
  model <- load_model_hdf5("~/git/lnf/code/NeuralNet/data/keras_14x10_softmax.h5")
  wts <- get_weights(model)
  wts_in <- NA
  for (i in 1:6) {
    wts_in <- c(wts_in, wts[[2]][i], wts[[1]][1:inneu, i])
  }
  for (i in 1:6) {
    wts_in <- c(wts_in, wts[[4]][i], wts[[3]][1:6, i])
  }
  for (i in 1:10) {
    wts_in <- c(wts_in, wts[[6]][i], wts[[5]][1:6, i])
  }
  wts_in <- wts_in[-1]
}

# load libraries
library(shiny)
library(raster)
library(ggplot2)
library(keras)
library(dplyr)
library(colorspace)
library(deepviz)
library(NeuralNetTools)
library(grid)

# user interface
ui <- fluidPage(
  h3("Neuronale Netze - in a Nutshell"),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Was geht in das Netz?", 
                         sliderInput("mywidth", "Dicke des Stifts", min=1, max=100, step=1, value=10),
                         actionButton("reset", "reset"),
                         actionButton("erkennen", "Erkennen!", class = "btn-info"),
                         fluidRow(
                           column(width = 4,
                                  plotOutput("plot", #width = "600px", height = "600px",
                                             hover=hoverOpts(id = "hover", delay = 70,
                                                             delayType = "throttle", clip = TRUE, nullOutside = TRUE),
                                             click="click")),
                           column(width = 4, 
                                  plotOutput("grid")),
                           column(width = 4, 
                                  plotOutput("pixeled"))
                         ),
                         numericInput("number", label = "Welche Nummer haben Sie gezeichnet?",
                                      min = 0, max = 9, step = 1, value = 0),
                         actionButton("train", "Trainieren!")),
                tabPanel("Was kommt aus dem Netz?",
                         verbatimTextOutput('Prediction'),
                         # plotOutput("deepviz"),
                         plotOutput("barprobs")
                ),
                tabPanel("Wie sieht das Netz aus?",
                         numericInput("noneu", label = "Wie viele Input-Neuronen sollen dargestellt werden?",
                                      min = 1, max = 140, step = 1, value = 3),
                         actionButton("visbutton", "Visualisieren!", class = "btn-info"),
                         plotOutput("netzviz")
                ))),
)

# actual computations
server <- function(input, output, session) {
  vals = reactiveValues(x=NULL, y=NULL)
  draw = reactiveVal(FALSE)
  observeEvent(input$click, handlerExpr = {
    temp <- draw(); draw(!temp)
    if(!draw()) {
      vals$x <- c(vals$x, NA)
      vals$y <- c(vals$y, NA)
    }})
  observeEvent(input$reset, handlerExpr = {
    vals$x <- NULL; vals$y <- NULL
  })
  observeEvent(input$hover, {
    if (draw()) {
      vals$x <- c(vals$x, input$hover$x)
      vals$y <- c(vals$y, input$hover$y)
    }})
  x <- eventReactive({c(
    input$erkennen,
    input$train
  )}, {
    vals$x[!is.na(vals$x)]
  })
  y <- eventReactive({c(
    input$erkennen,
    input$train
  )}, {
    vals$y[!is.na(vals$y)]
  })
  d <- eventReactive({c(
    input$erkennen,
    input$train
  )}, {
    conversion(x(), y())
  })
  cnt <- eventReactive({c(
    input$erkennen,
    input$train
  )}, {
    counter(d())
  })
  
  # rufe train.fct auf, um eingegebene Zahl und gemalte Zahl zu speichern
  observeEvent(input$train, {
    train.fct(cnt(), input$number)
  })
  
  # Schaetzung der Wk fuer jede Kategorie
  probs_out <- eventReactive(input$erkennen, {
    pred.fct(cnt())
  })
  
  # welche Zahl hat maximale Wk?
  number_out <- eventReactive(input$erkennen, {
    apply(probs_out(), 1, which.max) - 1
  })
  
  # einige Werte zur Kontrolle anzeigen
  output$stats = renderPrint({
    print('vals')
    x()
    print('d')
    train()
  })
  
  # welche Zahl hat maximale Wk?
  wgts_out <- eventReactive(input$visbutton, {
    wgts(input$noneu)
  })
  
  # Stuktur als reactive value
  structr <- eventReactive(input$visbutton, {
    c(input$noneu, 6, 6, 10)
  })
  
  # plot in den Zahlen eingezeichnet werden
  output$plot= renderPlot({
    plot(x=vals$x, y=vals$y, xlim=c(0, 28), ylim=c(0, 28), ylab="", xlab="", lwd=input$mywidth, main = "Zeichnen Sie eine Zahl (0-9):", axes = FALSE, type = "l") + box()
  })
  
  observe({
    if (!is.null(input$plot_path)) {
      # do something with the drawn path data stored in input$plot_path
    }
  })
  
  # zeigen, wie grid um die Zahlen gelegt wird
  output$grid= renderPlot({
    mxy <- max(y()); mny <- min(y())
    mxx <- max(x()); mnx <- min(x())
    if ((mxy - mny) / (mxx - mnx) > 1.8) {mxx <- mxx + ((mxy - mny) / 2.5); mnx <- mnx - ((mxy - mny) / 2.5)}
    gx <- seq(mnx, mxx, length.out = 15)
    gy <- seq(mny, mxy, length.out = 11)
    plot(x(), y(), type = "l", lwd = input$mywidth, xlim = c(mnx, mxx), main = 'Raster wird über den Plot gelegt.')
    abline(h = gy, v = gx, lty = 2)
  })
  
  # plot in dem verpixelte Zahl angezeigt wird
  output$pixeled = renderPlot({
    theme_set(theme_bw())
    ggplot(cnt(), aes(x = x, y = y, fill = factor(count))) +
      geom_tile() + 
      geom_text(label = paste(1:140), size = 3) +
      labs(
        title = 'Zelle = 1, wenn ein Teil der Zahl in ihr liegt, 0 sonst.',
        fill = "variable value"
      ) +
      scale_fill_discrete_qualitative(palette = "Harmonic") 
  })
  
  # Vorhersage
  output$Prediction = renderPrint({
    cat('Ich glaube, du hast eine ', number_out(), 'gezeichnet.')
  })
  output$barprobs = renderPlot({
    barplot(probs_out(), names.arg = paste(0:9), ylab = 'Wahrscheinlichkeit', main = "Wie sicher ist sich das Netz?")
  })
  
  # Visualisierung Netz
  output$netzviz = renderPlot({
    plotnet(wgts_out(), struct = structr())
  })
  
}


# call shinyapp
shinyApp(ui, server)
