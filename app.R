
# Enable sharing cache multiple R processes.
shinyOptions(cache = diskCache("./data/plot_cache"))

library(shiny)


ui <- fluidPage(

    titlePanel("shiny-nginx-lb sample app"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
        ),

        mainPanel(plotOutput("distPlot"))
    ),

    # Display all parameters available in the session request.
    h4('Request Headers'),
    tableOutput('headers'),

    br(),
    br(),

    # Display installed packages (will vary depending on if using rocker/shiny-verse or rocker/shiny in docker/Dockerfile.shiny).
    h4('Installed Packages'),
    tableOutput('installed_packages')
)

server <- function(input, output, session) {

    output$distPlot <- renderCachedPlot({
        # Log request, if we see this message then the cache missed this key.
        cat(as.character(Sys.time()), '- nbins', input$bins, '\n')


        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    }, cacheKeyExpr = { input$bins })  # Set the bin count as the cache key.

    # List keys in the session$request environment.
    request_info <- ls(session$request)
    # Exclude httpuv and rook vars (these are environments and not data.frame castable)
    #   and HEADERS, as these can be found in the session$request root.
    request_info <- request_info[!grepl('rook|httpuv|HEADERS', request_info)]

    # Organize these properties in a single data.frame with (key, value) columns.
    info <- lapply(request_info, function(x) data.frame(property=x, value=session$request[[x]], stringsAsFactors = F))
    info <- do.call(rbind, info)

    output$headers <- renderTable(info)

    output$installed_packages <- renderTable(installed.packages())
}


shinyApp(ui = ui, server = server)

