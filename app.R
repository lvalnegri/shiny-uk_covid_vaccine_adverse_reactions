dmpkg.funs::load_pkgs(c('data.table', 'DT', 'htmltools', 'shiny'))

dpath <- file.path(datauk_path, 'covid', 'vaccine')
dts <- fst::read_fst(file.path(dpath, 'vaccine_adverse_reactions'), as.data.table = TRUE)
up_date <- format(as.Date(readLines(file.path(dpath, 'vaccine_adverse_reactions.date'))), '%d %B %Y')

ui <- fluidPage(

    h2('UK Covid Vaccines Adverse Reactions'),
    
    tags$br(),

    shinyWidgets::radioGroupButtons('rdb_grp', 'AGGREGATION:', 
        choices = c('None', 'NEC', 'SOC', 'Brand'),
        individual = TRUE,
        checkIcon = list( 
            yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),
            no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")
        )
    ),

    tags$br(),

    DTOutput('out_tbl')

)

server <- function(input, output, session){

    output$out_tbl <- renderDT({

        switch(input$rdb_grp,

            'NEC' = { 
                y <- dts[, .(Total = sum(Total), Fatal = sum(Fatal)), .(Brand, SOC, NEC)] %>% 
                    dcast(SOC+NEC~Brand, value.var = c('Total', 'Fatal'), fill = 0)
                setcolorder(y, c('SOC', 'NEC', 'Total_AstraZ', 'Fatal_AstraZ', 'Total_Pfizer', 'Fatal_Pfizer'))
                sketch <- withTags(table(
                        class = 'display',
                        thead(
                            tr(
                                th(rowspan = 2, 'SOC'),
                                th(rowspan = 2, 'NEC'),
                                th(colspan = 2, span('AstraZeneca', style = "color:gold; font-family:'times'; font-size:20pt; display:table; margin:0 auto;") ),
                                th(colspan = 2, span('Pfizer', style = "color:cyan; font-family:'times'; font-size:20pt; display:table; margin:0 auto;") )
                            ),
                            tr( th('Total'), th('Fatal'), th('Total'), th('Fatal') )
                        )
                ))
            },

            'SOC' = { 
                y <- dts[, .(Total = sum(Total), Fatal = sum(Fatal)), .(Brand, SOC)] %>% 
                    dcast(SOC~Brand, value.var = c('Total', 'Fatal'), fill = 0)
                setcolorder(y, c('SOC', 'Total_AstraZ', 'Fatal_AstraZ', 'Total_Pfizer', 'Fatal_Pfizer'))
                sketch <- withTags(table(
                        class = 'display',
                        thead(
                            tr(
                                th(rowspan = 2, 'SOC'),
                                th(colspan = 2, span('AstraZeneca', style = "color:gold; font-family:'times'; font-size:20pt; display:table; margin:0 auto;") ),
                                th(colspan = 2, span('Pfizer', style = "color:cyan; font-family:'times'; font-size:20pt; display:table; margin:0 auto;") )
                            ),
                            tr( 
                                th('Total', style = "text-align: center"), th('Fatal', style = "text-align: center"), 
                                th('Total', style = "text-align: center"), th('Fatal', style = "text-align: center")
                            )
                        )
                ))
            },

            'Brand' = { 
                y <- dts[, .(Total = sum(Total), Fatal = sum(Fatal)), .(Brand)] 
                sketch <- withTags(table(
                    class = 'display',
                    thead( tr( th(colspan = 1, 'Brand'), th(colspan = 1, 'Total'), th(colspan = 1, 'Fatal') ))
                ))
            },

            { 
                y <- dcast(dts, SOC+NEC+Reaction~Brand, value.var = c('Total', 'Fatal', 'rnk'), fill = 0)
                setcolorder(y, c(
                    'SOC', 'NEC', 'Reaction', 
                    'Total_AstraZ', 'Fatal_AstraZ', 'rnk_AstraZ',
                    'Total_Pfizer', 'Fatal_Pfizer', 'rnk_Pfizer'
                ))
                sketch <- withTags(table(
                        class = 'display',
                        thead(
                            tr(
                                th(rowspan = 2, 'SOC'),
                                th(rowspan = 2, 'NEC'),
                                th(rowspan = 2, 'Reaction'),
                                th(colspan = 3, span('AstraZeneca', style = "color:gold; font-family:'times'; font-size:20pt; display:table; margin:0 auto;") ),
                                th(colspan = 3, span('Pfizer', style = "color:cyan; font-family:'times'; font-size:20pt; display:table; margin:0 auto;") )
                            ),
                            tr( th('Total'), th('Fatal'), th('Rank'), th('Total'), th('Fatal'), th('Rank') )
                        )
                ))
            }
        
        )

        dt <- datatable(
            y, 
            rownames = FALSE, 
            container = sketch, 
            selection = 'none',
            class = 'cell-border nowrap',
            extensions = c('Buttons', 'Scroller'),
            caption = tags$caption(
                style = 'caption-side:bottom;text-align:right;font-size:12px',
                withTags(div(HTML(paste(
                    '<em>Data From ',
                    '<a href="https://www.gov.uk/government/publications/coronavirus-covid-19-vaccine-adverse-reactions/coronavirus-vaccine-summary-of-yellow-card-reporting">MHRA UK</a>. ',
                    'Last Updated:', up_date, '<em>'
                ))))
            ),
            options = list(
                scrollX = TRUE,
                scrollY = 400,
                scroller = TRUE,
                ordering = TRUE,
                searchHighlight = TRUE,
                deferRender = TRUE,
                buttons = c('copy', 'csv', 'print'),
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    "}"
                ),
                dom = 'Biftp'
            )
        )

        if(input$rdb_grp == 'Brand'){        
           dt <- dt %>%  formatCurrency(c('Total', 'Fatal'), '', digits = 0)
        } else {
           dt <- dt %>%  formatCurrency(c('Total_AstraZ', 'Fatal_AstraZ', 'Total_Pfizer', 'Fatal_Pfizer'), '', digits = 0)
        }

        dt
 
    })

}

shinyApp( ui = ui, server = server )
