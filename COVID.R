## Load Packages
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
#library(tidyverse)#
library(ggplot2)
library(plotly)
library(gt)
library(directlabels)
library(ggrepel)

## Set the owrk directory
setwd('D:/COVID-BRAZIL')

## Load the Brazilian COVID data set
covid <- read.csv(
        './data/HIST_PAINEL_COVIDBR_12nov2021.csv',
        header = TRUE,
        sep = ";",
        dec = ",")

## Get more information about the data set
glimpse(covid)

### ******************* BRAZIL *******************
## Summary data set per state
covid_states <- covid %>%
        filter(
                regiao != "Brasil",
                estado != "",
                municipio == '',
                is.na(codmun)
        ) %>%
        filter(data == "2021-11-12") %>%
        group_by(estado) %>%
        #distinct_all() %>%
        rename(
                Casos_Acumulados = casosAcumulado,
                Obitos_Acumulados = obitosAcumulado
        ) %>%
        arrange(desc(Casos_Acumulados, Obitos_Acumulados)) %>%
        select(estado, Casos_Acumulados, Obitos_Acumulados)

write.csv2(covid_states, './output/covid_states.csv', row.names = FALSE)

covid.states <- read.csv2('./output/covid_states.csv') %>%
        group_by(estado) %>%
        summarise(Casos_Acumulados = sum(Casos_Acumulados), 
                  Obitos_Acumulados = sum(Obitos_Acumulados)) %>%
        mutate(N = 1:n()) %>%
        select(4, 1, 2, 3)
        

tab.State <- covid.states %>%
        gt(rowname_col = 'N') %>%
        tab_stubhead(label = 'N') %>%
        fmt_number(
                columns = c('Casos_Acumulados', 'Obitos_Acumulados'),
                decimals = 0, 
                sep_mark = '.'
        ) %>%
        summary_rows(
                groups = NULL,
                columns = c('Casos_Acumulados', 'Obitos_Acumulados'),
                fns = list(TOTAL = 'sum'),
                formatter = fmt_number,
                decimals = 0,
                sep_mark = '.',
                use_seps = TRUE,
                missing_text = ""
        ) %>%
        cols_label(   
                N = md('**N**'),
                estado = md('**UF**'),
                Casos_Acumulados = md('**Casos Acumulados**'),
                Obitos_Acumulados = md('**Óbitos Acumulados**')
        ) %>%
        tab_source_note(
                source_note = md("_Fonte: Secretarias Estaduais de Saúde. Brasil, 2021._")
        ) %>%
        tab_source_note(
                source_note = md("_Atualizado: 07/09/2021._")
        ) %>%
        # tab_style(
        #         cell_text(
        #                 align = "center", weight = "bold"
        #         ),
        #         locations = cells_summary(columns = T, rows = T)
        # ) %>%
        tab_options(
                ## Change the vertical lines
                stub.border.color = "white",
                ## change the 'Total' fields (summary)
                stub.font.weight = "bold",
                grand_summary_row.border.color = "black",
                ## change the column labels section
                column_labels.border.top.color = "black",
                column_labels.border.bottom.color = "black",
                column_labels.font.weight = "bold",
                table_body.vlines.color = "black",
                table.background.color = "white",
                ## change the bottom of the body
                table_body.border.bottom.color = "black",
                table_body.border.top.color = "black",
                ## Change the bottom-most line or footnotes
                table.border.top.color = "black",
                table.border.bottom.color = "black",
                summary_row.border.color = "black",
        ) 

tab.State

## Cumulative COVID Cases in Brazil
casosAcumTab <- covid %>%
        filter(
                data == '2021-11-12',
                regiao == 'Brasil'
        ) %>%
        #mutate(Data = as.Date(data, "%Y-%m-%d")) %>%
        summarise(
                Casos_Acumulados = sum(casosAcumulado, na.rm = TRUE),
                                           
                Obitos_Acumulados = sum(obitosAcumulado, na.rm = TRUE),
                                           
                obitos_Novos = sum(obitosNovos, na.rm = TRUE),
                                     
                Casos_Novos = sum(casosNovos, na.rm = TRUE),
                             
                # Recuperadosnovos = formatC(sum(Recuperadosnovos, na.rm = TRUE),
                #                            big.mark = '.'),
                # emAcompanhamentoNovos = formatC(sum(emAcompanhamentoNovos, na.rm = TRUE),
                #                                 big.mark = '.')
        ) %>%
        mutate(Casos_Acumulados = as.numeric(as.character(Casos_Acumulados))) %>%
        mutate(Obitos_Acumulados = as.numeric(as.character(Obitos_Acumulados))) %>%
        mutate(obitos_Novos = as.numeric(as.character(obitos_Novos))) %>%
        mutate(Casos_Novos = as.numeric(as.character(Casos_Novos)))


as_tibble(casosAcumTab)

gt(casosAcumTab) %>%
        fmt_number(
                columns = c(
                        'Casos_Acumulados', 'Casos_Novos', 'obitos_Novos', 
                        'Obitos_Acumulados'
                ),
                decimals = 0,
                sep_mark = '.'
        ) %>%
        cols_label( ## Change the names of the columns
                Casos_Acumulados = md('**Casos Confirmados**'),
                Casos_Novos = md('**Casos Novos**'),
                obitos_Novos = md('***obitos Novos***'),
                Obitos_Acumulados = md('**Obitos Acumulados**'),
                #Recuperadosnovos = md('**Casos Recuperados**')
                #emAcompanhamentoNovos = md('**Em Acompanhamento**')
        ) %>%
        tab_source_note(
                source_note = md("_Atualizado: 07/09/2021._")
        ) %>%
        tab_source_note(
                source_note = md("_Fonte: Secretarias Estaduais de Saúde. Brasil, 2021._")
        ) %>%
        # tab_style(
        #         cell_text(
        #                 align = "center", weight = "bold"
        #         ),
        #         locations = cells_summary(columns = T, rows = T)
        # ) %>%
        tab_options(
                ## Change the vertical lines
                
                ## change the 'Total' fields (summary)
                stub.font.weight = "bold",
                grand_summary_row.border.color = "black",
                ## change the column labels section
                column_labels.border.top.color = "black",
                column_labels.border.bottom.color = "black",
                column_labels.font.weight = "bold",
                table_body.vlines.color = "black",
                table.background.color = "#29a329",
                ## change the bottom of the body
                table_body.border.bottom.color = "black",
                table_body.border.top.color = "black",
                ## Change the bottom-most line or footnotes
                table.border.top.color = "black",
                table.border.bottom.color = "black",
                summary_row.border.color = "black",
        ) 

## Plot Cumulative COVID Cases in Brazil
#text = paste('Data:', casosAcum$Data, '<br>', 'Casos Acumulados:', casosAcum$Casos_Acumulados)
casosAcum <- covid %>%
        filter(regiao == 'Brasil') %>%
        mutate(Data = as.Date(data, "%Y-%m-%d")) %>%
        select(Data, casosAcumulado)

ca <- ggplot(
        casosAcum,
        aes(Data, casosAcumulado)
        ) +
        geom_line(color = "#006400", size = 1.6) +
        geom_area(fill = "#69b3a2") +
        scale_x_date(
                date_labels = "%d/%m/%y", date_breaks = "1 month"
                ) +
        scale_y_continuous(
                breaks = seq(0, 22000000, by = 5000000),
                labels = scales::number_format(accuracy = 1,
                                               big.mark = '.',
                                               decimal.mark = ',')
                ) +
        theme(
                axis.text.x = element_text(angle = 50),
                axis.title = element_text(size = 14),
                plot.title = element_text(
                        hjust = 0.5, size = 14, face = "bold"),
                plot.caption = element_text(
                        size = 7, color = "blue", face = "italic")
                ) +
        labs(
                title = "Casos Acumulados de COVID-19 no Brasil",
                x = "Data da notificação",
                y = "Casos Acumulados",
                caption = "Fonte: Secretarias Estaduais de Saúde. Brasil, 2021"
                )

ggplotly(ca)

###***************************Ranking by States******************************###
## Cumulative COVID cases by Brazilian states
#as.vector(covid_states[1:10, 2])  ## Set ranking the ten most affected states
ranking <- covid_states[1:5, 1:2]

covid_UF <- covid %>%
        filter(
                regiao != "Brasil",
                #estado != "",
                municipio == '',
                is.na(codmun),
                estado %in% ranking$estado
        ) %>%
        mutate(Data = as.Date(data, "%Y-%m-%d")) %>%
        rename(UF = estado) %>%
        group_by(Data, UF) %>%
        distinct_all() %>%
        summarise(
                Casos_Acumulados = sum(unique(casosAcumulado), na.rm = TRUE),
                Obitos_Acumulados = sum(unique(obitosAcumulado), na.rm = TRUE),
                Casos_Novos = sum(unique(casosNovos, na.rm = TRUE))
        )


## Ranking of Accumulated COVID Cases by States
covidUFacc <- ggplot(
        covid_UF, aes(
                x = Data, y = Casos_Acumulados, 
                color = UF, group = UF
        )
) +
        geom_line(size = 0.8) +
        scale_x_date(
                date_labels = "%d/%m/%y", date_breaks = "3 weeks"
        ) +
        scale_y_continuous(
                labels = scales::label_number(big.mark = '.'), 
                breaks = seq(0, 5000000, by = 500000)
        ) +
        theme(
                axis.text.x = element_text(angle = 50),
                axis.title = element_text(size = 14),
                plot.title = element_text(
                        hjust = 0.5, size = 14, face = "bold"),
                plot.caption = element_text(
                        size = 7, color = "blue", face = "italic")
        ) +
        labs(
                title = "Rank dos Estados com Maiores Casos Acumulados de COVID-19",
                x = "Data da notificação",
                y = "Casos Acumulados",
                caption = "Fonte: Secretarias Estaduais de Saúde. Brasil, 2021."
        )
        

ggplotly(covidUFacc)

## Ranking of COVID Deaths by States
covidUFdeath <- ggplot(
        covid_UF, aes(
                x = Data, y = Obitos_Acumulados, 
                color = UF, group = UF
        )
) +
        geom_line(size = 0.8) +
        scale_x_date(
                date_labels = "%d/%m/%y", date_breaks = "3 weeks"
        ) +
        scale_y_continuous(
                labels = scales::label_number(big.mark = '.', decimal.mark = ','), 
                breaks = seq(0, 150000, by = 10000)
        ) +
        theme(
                axis.text.x = element_text(angle = 50),
                axis.title = element_text(size = 14),
                plot.title = element_text(
                        hjust = 0.5, size = 14, face = "bold"),
                plot.caption = element_text(
                        size = 7, color = "blue", face = "italic")
        ) +
        labs(
                title = "Rank dos Estados com Maiores Óbitos Acumulados por COVID-19",
                x = "Data da notificação",
                y = "Casos Acumulados",
                caption = "Fonte: Secretarias Estaduais de Saúde. Brasil, 2021."
        )

ggplotly(covidUFdeath)

## Ranking of New COVID Cases by State
covidNewCases <- ggplot(
        covid_UF, aes(
                x = Data, y = Casos_Novos, color = UF
                )
        ) +
        geom_smooth(method = "gam", se = FALSE, size = 0.4) +
        scale_x_date(
                date_labels = "%d/%m/%y", date_breaks = "3 weeks"
        ) +
        scale_y_continuous(
                labels = scales::label_number(big.mark = '.', decimal.mark = ','), 
                breaks = seq(0, 65000, by = 2000)
        ) +
        theme(
                axis.text.x = element_text(angle = 50),
                axis.title = element_text(size = 14),
                plot.title = element_text(
                        hjust = 0.5, size = 14, face = "bold"),
                plot.caption = element_text(
                        size = 7, color = "blue", face = "italic")
        ) +
        labs(
                title = "Rank de Novos Casos de COVID-19 por Estado",
                x = "Data da notificação",
                y = "Casos Acumulados",
                caption = "Fonte: Secretarias Estaduais de Saúde. Brasil, 2021."
        )

ggplotly(covidNewCases)


#write.csv2(covid[covid$estado=="SP", c("data", "estado", "casosAcumulado")], "D:/Downloads/test.csv", row.names = F)

### ******************* State of Pará *******************
covid_PA <- covid %>%
        filter(estado == 'PA') %>%
        #filter(data != "2020-12-24", data != "2021-01-13") %>%
        mutate(Data = as.Date(data, '%Y-%m-%d')) %>%
        rename(Casos_Acumulados = casosAcumulado) %>%
        group_by(Data, estado, Casos_Acumulados)

## Set the plot of COVID cases in the Para state
pa <- ggplot(covid_PA, aes(Data, Casos_Acumulados)) +
        geom_line(color = "#006400", size = 2) +
        #geom_area(fill = "#69b3a2") +
        scale_x_date(
                date_labels = "%d/%m/%y", date_breaks = "1 month"
        ) +
        scale_y_continuous(
                labels = scales::label_number(big.mark = '.')
        ) +
        theme(
                axis.text.x = element_text(angle = 50),
                axis.title = element_text(size = 14),
                plot.title = element_text(
                        hjust = 0.5, size = 14, face = "bold"),
                plot.caption = element_text(
                        size = 7, color = "blue", face = "italic")
        ) +
        labs(
                title = "Casos Acumulados de COVID-19 no Estado do Pará",
                x = "Data da notificação",
                y = "Casos Acumulados",
                caption = "Fonte: Secretarias Estaduais de Saúde. Brasil, 2021."
        )

ggplotly(pa)

## Cases of COVID in the state of Pará by epidemiological week


