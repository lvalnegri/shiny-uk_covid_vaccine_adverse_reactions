dmpkg.funs::load_pkgs(c('data.table', 'DT', 'htmltools', 'shiny'))

dpath <- file.path(datauk_path, 'covid', 'vaccine')
dts <- fst::read_fst(file.path(dpath, 'vaccine_adverse_reactions'), as.data.table = TRUE)
dts[, rnk := NULL]
up_date <- format(as.Date(readLines(file.path(dpath, 'vaccine_adverse_reactions.date'))), '%d %B %Y')

js_footer <- function(x){
    paste0(
        'tot_', x, ' = api.column(', x, ', {search:"applied", page:"all"}).data().reduce(function(a, b){return a + b;})
        $(api.column(', x, ').footer()).html(tot_', x, '.toLocaleString());'
    )
}

ui <- fluidPage(

    titlePanel('UK Covid Vaccines Yellow Card Reporting (potential adverse reactions)'), br(),

    shinyWidgets::radioGroupButtons('rdb_grp', 'AGGREGATION:', 
        choices = c('None', 'Group', 'Class', 'Brand'),
        individual = TRUE,
        checkIcon = list( 
            yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),
            no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")
        )
    ), br(),

    DTOutput('out_tbl')

)

server <- function(input, output, session){

    dtbl <- reactive({
        
        switch(input$rdb_grp,

            'Group' = { 
                y <- dts[, .(Total = sum(Total), Fatal = sum(Fatal)), .(Brand, Class, Group)] %>% 
                    dcast(Class+Group~Brand, value.var = c('Total', 'Fatal'), fill = 0)
                setcolorder(y, c('Class', 'Group', 'Total_AstraZeneca', 'Fatal_AstraZeneca', 'Total_Pfizer', 'Fatal_Pfizer'))
                sketch <- withTags(table(
                        class = 'display',
                        thead(
                            tr(
                                th(rowspan = 2, 'Class'),
                                th(rowspan = 2, 'Group'),
                                th(colspan = 2, span('AstraZeneca', style = "color:gold; font-family:'times'; font-size:20pt; display:table; margin:0 auto;") ),
                                th(colspan = 2, span('Pfizer', style = "color:cyan; font-family:'times'; font-size:20pt; display:table; margin:0 auto;") )
                            ),
                            tr( th('Total'), th('Fatal'), th('Total'), th('Fatal') )
                        ),
                        tableFooter(c('', 'TOTAL TABLE: ', 0, 0, 0, 0))
                ))
                foot_dt <- paste(js_footer(2), js_footer(3), js_footer(4), js_footer(5))
            },

            'Class' = { 
                y <- dts[, .(Total = sum(Total), Fatal = sum(Fatal)), .(Brand, Class)] %>% 
                    dcast(Class~Brand, value.var = c('Total', 'Fatal'), fill = 0)
                setcolorder(y, c('Class', 'Total_AstraZeneca', 'Fatal_AstraZeneca', 'Total_Pfizer', 'Fatal_Pfizer'))
                sketch <- withTags(table(
                        class = 'display',
                        thead(
                            tr(
                                th(rowspan = 2, 'Class'),
                                th(colspan = 2, span('AstraZeneca', style = "color:gold; font-family:'times'; font-size:20pt; display:table; margin:0 auto;") ),
                                th(colspan = 2, span('Pfizer', style = "color:cyan; font-family:'times'; font-size:20pt; display:table; margin:0 auto;") )
                            ),
                            tr( 
                                th('Total', style = "text-align: center"), th('Fatal', style = "text-align: center"), 
                                th('Total', style = "text-align: center"), th('Fatal', style = "text-align: center")
                            )
                        ),
                        tableFooter(c('TOTAL TABLE: ', 0, 0, 0, 0))
                ))
                foot_dt <- paste(js_footer(1), js_footer(2), js_footer(3), js_footer(4))
            },

            'Brand' = { 
                y <- dts[, .(Total = sum(Total), Fatal = sum(Fatal)), Brand] 
                sketch <- withTags(table(
                    class = 'display',
                    thead( tr( th(colspan = 1, 'Brand'), th(colspan = 1, 'Total'), th(colspan = 1, 'Fatal') )),
                    tableFooter(c('TOTAL TABLE: ', 0, 0))
                ))
                foot_dt <- paste(js_footer(1), js_footer(2))
            },

            { 
                y <- dcast(dts, Class+Group+Reaction~Brand, value.var = c('Total', 'Fatal'), fill = 0)
                setcolorder(y, c('Class', 'Group', 'Reaction', 'Total_AstraZeneca', 'Fatal_AstraZeneca', 'Total_Pfizer', 'Fatal_Pfizer'))
                sketch <- withTags(table(
                        class = 'display',
                        thead(
                            tr(
                                th(rowspan = 2, 'Class'),
                                th(rowspan = 2, 'Group'),
                                th(rowspan = 2, 'Reaction'),
                                th(colspan = 2, span('AstraZeneca', style = "color:gold; font-family:'times'; font-size:20pt; display:table; margin:0 auto;") ),
                                th(colspan = 2, span('Pfizer', style = "color:cyan; font-family:'times'; font-size:20pt; display:table; margin:0 auto;") )
                            ),
                            tr( th('Total'), th('Fatal'), th('Total'), th('Fatal') )
                        ),
                        tableFooter(c('', '', 'TOTAL TABLE: ', 0, 0, 0, 0))
                ))
                foot_dt <- paste(js_footer(3), js_footer(4), js_footer(5), js_footer(6))
            }
        
        )
        
        y[y == 0] <- NA
        dt <- datatable(
            y, 
            rownames = FALSE, 
            container = sketch, 
            selection = 'none',
            class = 'cell-border nowrap',
            extensions = c('Buttons', 'Scroller'),
            caption = tags$caption(
                style = 'caption-side:bottom;text-align:right;font-size:12px',
                withTags(div(HTML(paste0(
                    '<em>Data From ',
                    '<a href="https://www.gov.uk/government/publications/coronavirus-covid-19-vaccine-adverse-reactions/coronavirus-vaccine-summary-of-yellow-card-reporting">MHRA UK</a>. ',
                    'Last Updated: ', up_date,
                    ' (App code on <a href=" https://github.com/lvalnegri/shiny-uk_covid_vaccine_adverse_reactions">Github</a>)<em>'
                ))))
            ),
            options = list(
#                scrollX = TRUE,
                scrollY = 400,
                scroller = TRUE,
                ordering = TRUE,
                searchHighlight = TRUE,
                deferRender = TRUE,
                buttons = c('copy', 'csv', 'print'),
                initComplete = JS(
                    "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'text-align': 'center', 'font-size': '120%'});",
                        "$(this.api().table().footer()).css({'background-color': '#000', 'color': '#fff', 'text-align': 'center', 'font-size': '120%'});",
                    "}"
                ),
                footerCallback = JS(paste0(
                    "function( tfoot, data, start, end, display ) {",
                        "var api = this.api(), data;", foot_dt,
                    "}"
                )),
                dom = 'Biftp'
            )
        )

        if(input$rdb_grp == 'Brand'){        
           dt <- dt %>% formatCurrency(c('Total', 'Fatal'), '', digits = 0)
        } else {
           dt <- dt %>% formatCurrency(c('Total_AstraZeneca', 'Fatal_AstraZeneca', 'Total_Pfizer', 'Fatal_Pfizer'), '', digits = 0)
        }

        dt
        
        
    })

    output$out_tbl <- renderDT( dtbl(), server = FALSE )

}

shinyApp( ui = ui, server = server )
