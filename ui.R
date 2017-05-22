library(shiny)
function(request){
  shinyUI(
    fluidPage(
      fluidRow(style = "height:50px;",
        column(6, offset = 3, h2("CitiBikeEDB", style =  "text-align:center;")),
        column(3, checkboxInput("showTempFiltred", label = "Show plot filtering on plots", value = FALSE))
      ),
      fluidRow(
        column(6, leafletOutput("map", height = "500px")),
        column(6,
               fluidRow(
                 column(8, plotOutput("hourPlot", height = "250px",
                                                       brush = brushOpts(id = "hourPlot_brush", direction = "x", resetOnNew = FALSE))),
                 column(4, HTML('<img src="triade_cbEDB.png" class="img-responsive alt="YMC_cbEDB_texte">'))),
                fluidRow(
                  column(6, plotOutput('dayPlot', height = "250px",
                                       brush = brushOpts(id = "dayPlot_brush", direction = "x", resetOnNew = FALSE))),
                  column(6, plotOutput("monthPlot", height = "250px",
                                       brush = brushOpts(id = "monthPlot_brush", direction = "x", resetOnNew = FALSE))))
               )),
      fluidRow(
        column(4, plotOutput("durationPlot",height = "250px",
                             brush = brushOpts(id = "durationPlot_brush", direction = "x", resetOnNew = FALSE))),
        column(4, plotOutput("agePlot", height = "250px",
                             brush = brushOpts(id = "agePlot_brush", direction = "x", resetOnNew = FALSE))),
        column(4, plotOutput("sexPlot",height = "250px",
                             brush = brushOpts(id = "sexPlot_brush", direction = "x", resetOnNew = FALSE)))
      )
    )
  )
}