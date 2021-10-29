## Load Packages
library(dplyr)
library(tidyr)
library(tidyverse)#
library(ggplot2)
library(plotly)
library(DT)#
library(gt)

## Load the Brazilian COVID data set
covid <- read.csv(
        "D:/Downloads/HIST_PAINEL_COVIDBR_29jan2021.csv",
        header = TRUE,
        sep = ";",
        dec = ",")

## Get more information about the data set
str(covid)

### ******************* BRAZIL *******************
## Summary data set per state
covid %>%
        filter(regiao != "Brasil",
               estado != "",
               !is.na(estado),
               municipio == "") %>%
        filter(data == "2021-01-29") %>%
        rename(UF = estado) %>%
        group_by(UF) %>%
        distinct_all() %>%
        summarise(
                Casos_Acumulados = sum(unique(casosAcumulado), 
                                       na.rm = TRUE),
                Obitos_Acumulados = sum(unique(obitosAcumulado), 
                                        na.rm = TRUE),
                Casos_Novos = sum(unique(casosNovos, na.rm = TRUE))) %>%
        arrange(desc(Casos_Acumulados, Obitos_Acumulados)) %>%
        mutate(N = row_number()) %>%
        select(N, UF, Casos_Acumulados, Obitos_Acumulados, Casos_Novos) %>%
        gt() %>%
        fmt_number(vars(Casos_Acumulados,
                        Obitos_Acumulados, 
                        Casos_Novos),
                   decimals = 0, 
                   sep_mark = '.') %>%
        cols_label(Casos_Acumulados = 'Casos Acumulados',
                   Obitos_Acumulados = 'Óbitos Acumulados',
                   Casos_Novos = 'Casos Novos') %>%
        tab_source_note(source_note = "Fonte: Secretarias Estaduais de Saúde. Brasil, 2020.") %>%
        tab_source_note(source_note = "Atualizado: 29/01/2021.")

## Cumulative COVID cases in Brazil
casosAcum <- covid %>%
        filter(regiao == "Brasil") %>%
        filter(data == "2021-01-29") %>%
        mutate(Data = as.Date(data, "%Y-%m-%d"),
               casosAcumulado = unique(casosAcumulado)) %>%
        group_by(Data)
        #select(Data, Casos_Acumulados, casosNovos, obitosAcumulado, obitosNovos, Recuperadosnovos, emAcompanhamentoNovos)

as_tibble(casosAcum)

#text = paste('Data:', casosAcum$Data, '<br>', 'Casos Acumulados:', casosAcum$Casos_Acumulados)
ca <- ggplot(
        na.omit(casosAcum),
        aes(Data, casosAcumulado)
        ) +
        geom_line(color = "#006400", size = 2) +
        geom_area(fill = "#69b3a2") +
        scale_x_date(
                date_labels = "%d/%m/%y", date_breaks = "1 month"
                ) +
        scale_y_continuous(
                breaks = seq(0, 9058687, by = 10^6),
                labels = scales::number_format(accuracy = 1, big.mark = '.')
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
                title = "Casos Acumulados de COVID-19 por Data de Notificação",
                x = "Data da notificação",
                y = "Casos Acumulados",
                caption = "Fonte: Secretarias Estaduais de Saúde. Brasil, 2020"
                )

ggplotly(ca)

## Cumulative COVID cases by Brazilian states
covid_UF <- covid %>%
        filter(regiao != "Brasil",
               estado != "",
               !is.na(estado),
               !(data > "2020-04-01" & casosAcumulado <= 1),
               municipio == "") %>%
        filter(data != "2020-12-24", data != "2021-01-13") %>%
        rename(UF = estado) %>%
        group_by(UF) %>%
        summarize(casosAcumulado)
                
        
#write.csv2(covid[covid$estado=="SP", c("data", "estado", "casosAcumulado")], "D:/Downloads/test.csv", row.names = F)

### ******************* State of Pará *******************
covid_PA <- covid %>%
        filter(regiao != "Brasil",
               estado != "",
               estado == "PA",
               !is.na(estado),
               !(data > "2020-04-01" & casosAcumulado <= 1),
               municipio == "") %>%
        filter(data != "2020-12-24", data != "2021-01-13") %>%
        mutate(Data = as.Date(data, "%Y-%m-%d")) %>%
        rename(Casos_Acumulados = casosAcumulado) %>%
        group_by(Data, estado, Casos_Acumulados)

## Set the plot of COVID cases in the Para state
pa <- ggplot(covid_PA, aes(Data, Casos_Acumulados)) +
        geom_line(color = "#006400", size = 2) +
        geom_area(fill = "#69b3a2") +
        scale_x_date(
                date_labels = "%d/%m/%y", date_breaks = "1 month"
        ) +
        scale_y_continuous(labels = scales::label_number(big.mark = '.'), breaks = seq(0, 325562, by = 25000)) +
        theme(
                axis.text.x = element_text(angle = 50),
                axis.title = element_text(size = 14),
                plot.title = element_text(
                        hjust = 0.5, size = 14, face = "bold"),
                plot.caption = element_text(
                        size = 7, color = "blue", face = "italic")
        ) +
        labs(
                title = "Casos Acumulados de COVID-19 no Estado do Pará por Data de Notificação",
                x = "Data da notificação",
                y = "Casos Acumulados",
                caption = "Fonte: Secretarias Estaduais de Saúde. Brasil, 2020"
        )

ggplotly(pa)

## Cases of COVID in the state of Pará by epidemiological week


