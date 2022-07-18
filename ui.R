library(shiny) # paquete principal del panel de visualización
library(shinydashboard) # paquete que modifica la estructura de comandos shiny (algunos), pero da un mejor aspecto visual
library(fmsb) # gráfico de araña
library(openxlsx) # carga de archivos
library(ggplot2) # gráficos
library(ggcorrplot) # Correlation plot for ggplot
library(gridExtra) # agrupar gráficos de ggplot
library(viridis) # paleta de colores
library(kableExtra) # renderizado de tablas
library(dplyr) # tubería
library(ggpubr) # agrupar gráficos de ggplot con una sola leyenda
# library(plotly) # gráficos interactivos (se puede combinar con ggplot fácilmente)
library(Cairo) # mejora resolución y renderizado de gráficos
options(shiny.usecairo = T)
library(mice) # Imputación de datos
library(UBL) # Balanceo: Oversampling con observaciones sintéticas, Over & Under por muestreo simple
library(blorr) # Prueba Delta-Chi
library(lmtest) # TVR
library(ranger) # Paquete adicional para los árboles generados en "mice"
library(psych) # Paquete para resumen de datos de variables, función describe()

# library(dashboardthemes) # Paquete para temas
# library(shinythemes) # temas pre establecidos, se puede descargar el css para modificar, además tienen varias opciones html
# library(bslib) # paquete de temas, hay preestablecidos, o se pueden editar, bs_theme()
# library(thematic) # para hacer que los gráficos se acoplen a los colores de tema del panel (dejar colores dinámicos en ggplot)
# thematic::thematic_shiny() # Ejecución del paquete anterior
# theme = bslib::bs_theme( # Sirve para personalizar temas, para luego ingresarlo como argumento
#   version = 4, bootswatch = "cosmo",
#   font_scale = 1
# )

########################### Datos del panel ###########################

linebreaks <- function(n){HTML(strrep(br(), n))}
cods = read.xlsx("Asignaturas.xlsx")

########################### Elementos generales del panel ###########################

# Header Panel
headerpanel = dashboardHeader(title = "Seguimiento",
                              tags$li(class = "dropdown",
                                      tags$a(href = "https://www.linkedin.com/in/dfranzani/",
                                             icon("linkedin"), "", target = "_blank")),
                              tags$li(class = "dropdown",
                                      tags$a(href = "https://github.com/Dfranzani",
                                             icon("github"), "", target = "_blank")),
                              tags$li(class = "dropdown",
                                      tags$a(href = "https://dfranzani.netlify.app/",
                                             icon("blog"), "", target = "_blank")))

# Sidebar Menu Item pages
sidebar = dashboardSidebar(
  collapsed = TRUE,
  sidebarMenu(
    menuItem("Diagnóstico 2022", tabName = "diagnostico", icon = icon("stats", lib = "glyphicon")),
    menuItem("Riesgo inicial 2022", tabName = "riesgoinicial", icon = icon("dashboard", lib = "glyphicon"))
  )
)

########################### Elementos de paneles: Panel 1 ###########################

# Histogramas de proporciones de logro por área
h1 = plotOutput("Hist.mat", height = 250)
h2 = plotOutput("Hist.len", height = 250)
h3 = plotOutput("Hist.geo", height = 250)

# Gráficos de dispersión
ptu.pruebas = plotOutput("Scatter1", height = 283)
nem.pruebas = plotOutput("Scatter2", height = 283)
ranking.pruebas = plotOutput("Scatter3", height = 283)

# Gráficos de barra de TICS
tics.valorativas = plotOutput("Tics1", height = 250)
tics.frecuencias = plotOutput("Tics2", width = 350, height = 250)

# Botón de reinicio de entradas
Reboot = actionButton(inputId = "reboot", label = "Reiniciar", class = "btn-primary", style = "color: white;")

# Opción de limpieza parcial
ParcialReboot = checkboxInput(inputId = "parcialReboot", label = "Reiniciar pestañas", value = TRUE)

# Indicadores numéricos
recuento1 = textOutput("RecuentoTotal")
recuento2 = textOutput("RecuentoSexoH")
recuento3 = textOutput("RecuentoSexoM")
recuento4 = textOutput("RecuentoViaPTU")
recuento5 = textOutput("RecuentoViaOtras")

# Tablas
table.rate = tableOutput("TableRate")
table.entries.rates = tableOutput("TableEntrieRate")
table.sex.rates = tableOutput("TableSexRate")

# Niveles de logro por competencia
resumen.competencias.Mate = tableOutput("ResumenCompentenciasMate")
resumen.competencias.Leng = tableOutput("ResumenCompentenciasLeng")
resumen.competencias.Geo = tableOutput("ResumenCompentenciasGeo")
competencias.Mate = plotOutput("CompetenciasMate", height = 550)
competencias.Leng = plotOutput("CompetenciasLeng", height = 550)
competencias.Geo = plotOutput("CompetenciasGeo", height = 550)
titulo.competencias = uiOutput("tituloCompetencias")

# Frecuencia de logros por competencia
competencias.adecuadas.mat = plotOutput("CompAdecuadasMat")
competencias.adecuadas.len = plotOutput("CompAdecuadasLen")
competencias.adecuadas.geo = plotOutput("CompAdecuadasGeo")

# Spider-chart de promedios por resultado de aprendizaje
resultados.aprendizaje = plotOutput("RAprendizajes")

# Caja de aviso
caja.filtros = box(id = "filters_information", width = NULL, solidHeader = TRUE, background = "red",
                       title = strong("Información de los filtros", style = 'font-size:14px;'),
                       collapsed = FALSE, collapsible = TRUE,
                       p("Los filtros pueden conbinarse como lo estime conveniente. Sin embargo,
                              al elegir una determinada carrera no será posible elegir una facultad. 
                              Por otro lado, para poder elegir una determinada facultad se debe tener
                              seleccionada la opción de 'Institucional' en el filtro de carera",
                         style = 'font-size:14px;'))
caja.indicadores = box(id = "indicators_information", width = 2, solidHeader = TRUE, background = "red",
                       title = strong("Información de los indicadores", style = 'font-size:14px;'),
                       collapsed = TRUE, collapsible = TRUE,
                       p("Los indicadores de la parte superior de esta pestaña, están calculados 
                                        considerando a los y las estudiantes que rindieron al menos una de las 
                                        pruebas (incluyendo el cuestionario de TIC).",
                         style = 'font-size:14px;'))

########################### Distribución de paneles: Panel 1 ###########################

# Sidebar Panel de la primera hoja
sidebar1 = sidebarPanel(width = 2,
                        Reboot, ParcialReboot,
                        caja.filtros,
                        selectInput(inputId = "carrera", label = "Carrera",
                                    choices = c("Institucional", unique(cods$CARRERA)[order(unique(cods$CARRERA))])),
                        selectInput(inputId = "facultad", label = "Facultad",
                                    choices = c("Institucional", unique(cods$FACULTAD)[order(unique(cods$FACULTAD))], "Ingeniería 2030")),
                        selectInput(inputId = "sexo", label = "Sexo", choices = c("Ambos","Hombre", "Mujer")),
                        selectInput(inputId = "via", label = "Vía de ingreso", choices = c("Ambas","PTU", "Otras"))#,
                        # selectInput(inputId = "cohorte", label = "Cohorte", choices = c(2022:2019))
                        )

main1 = mainPanel(width = 10,
                  fluidRow(box(id = "p1t0", width = 12, title = "Desempeños generales por prueba",
                               collapsible = TRUE, solidHeader = TRUE, status = "primary", collapsed = F,
                               fluidRow(
                                 infoBox("Total", recuento1, icon = icon("users"), width = 2, fill = TRUE, color = "blue"),
                                 infoBox("Hombres", recuento2, icon = icon("male"), width = 2, fill = TRUE, color = "blue"),
                                 infoBox("MUjeres", recuento3, icon = icon("female"), width = 2, fill = TRUE, color = "blue"),
                                 infoBox("Ingreso PTU", recuento5, icon = icon("pencil", lib = "glyphicon"), width = 2, fill = TRUE, color = "blue"),
                                 infoBox(HTML(paste("Ingreso", br(), "Otras vías")), recuento4, icon = icon("th-large"), width = 2, fill = TRUE, color = "blue"),
                                 caja.indicadores
                                 ),
                               fluidRow(tabBox(id = "p1t1", width = 5, height = 350,
                                               tabPanel(title = "Tasa de respuesta", table.rate, value = "p1t1.1"), 
                                               tabPanel(title = "Por sexo", table.sex.rates),
                                               tabPanel(title = "Por vía de ingreso", table.entries.rates)),
                                        tabBox(id = "p1t2", width = 7, height = 350,
                                               tabPanel(title = "PTU vs Pruebas", ptu.pruebas, value = "p1t2.1"),
                                               tabPanel(title = "NEM vs Pruebas", nem.pruebas),
                                               tabPanel(title = "Ranking vs Pruebas", ranking.pruebas))),
                               fluidRow(tabBox(id = "p1t3", width = 5,
                                               tabPanel(title = "Matemática", h1, value = "p1t3.1"),
                                               tabPanel(title = "Lenguaje", h2),
                                               tabPanel(title = "Geometría", h3)),
                                        tabBox(id = "p1t4", width = 7,
                                               tabPanel(title = "TIC: Respuestas valorativas", tics.valorativas, value = "p1t4.1"),
                                               tabPanel(title = "TIC: Respuestas de frecuencia", tics.frecuencias)))
                               )
                           ),
                  fluidRow(box(id = "p1t5", width = 12, title = span("Desempeños por competencia"), # , style = "color: black;"
                               collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "warning",
                               tabBox(id = "p1t5.1", width = 12, height = 650, 
                                      tabPanel(title = "Matemática", value = "p1t5.1.1",
                                               fluidRow(column(width = 4, resumen.competencias.Mate,
                                                               competencias.adecuadas.mat),
                                                        column(width = 8, 
                                                               h3(p(em(strong("Distribución de niveles de desempeño"))), align = "center"),
                                                               competencias.Mate))),
                                      tabPanel(title = "Lenguaje",
                                               fluidRow(column(width = 4, resumen.competencias.Leng,
                                                               competencias.adecuadas.len),
                                                        column(width = 8, 
                                                               h3(p(em(strong("Distribución de niveles de desempeño"))), align = "center"),
                                                               competencias.Leng))),
                                      tabPanel(title = "Geometría",
                                               fluidRow(column(width = 4, resumen.competencias.Geo,
                                                               competencias.adecuadas.geo),
                                                        column(width = 8, 
                                                               titulo.competencias,
                                                               competencias.Geo)))))),
                  fluidRow(box(id = "p1t6", width = 12, title = "Desempeños por resultado de aprendizaje",
                               collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "success",
                               h3(p(em(strong("Logro promedio por resultado de aprendizaje"))), align = "center"),
                               resultados.aprendizaje))
                  )

########################### Elementos de paneles: Panel 2 ###########################

# Total de reprobados y aprobados
aprobados = textOutput("Aprobados")
reprobados = textOutput("Reprobados")

# Histogramas de variables descriptivas
hist.nem = plotOutput("Hist.nem", height = 250)
hist.ptu = plotOutput("Hist.ptu", height = 250)
hist.logro = plotOutput("Hist.logro", height = 250)

# Gráfico de correlación
corrs = plotOutput("Corrs", height = 250)

# Resumen de la data 2021
summary.2021 = tableOutput("Summary2021")

# Resumen de la data 2022
summary.2022 = tableOutput("Summary2022")

# Gráfico de residuos y ajustes
ajustes = plotOutput("Ajustes", height = 250)

# Métricas de entrenamiento
train.accuracy = textOutput("TrainAccuracy")
train.precision = textOutput("TrainPrecision")
train.recall = textOutput("TrainRecall")
train.f1 = textOutput("TrainF1")

# Métricas de prueba
test.accuracy = textOutput("TestAccuracy")
test.precision = textOutput("TestPrecision")
test.recall = textOutput("TestRecall")
test.f1 = textOutput("TestF1")

ROC = plotOutput("roc", height = 250)
PR = plotOutput("pr", height = 250)
KS = plotOutput("ks", height = 250)

# Resumen del modelo
summary.model = verbatimTextOutput("SummaryModel")
bondad.ajuste = verbatimTextOutput("BondadAjuste")
devianza = verbatimTextOutput("Devianza")
# confidenceInterval = verbatimTextOutput("confidence.interval")

# Gráficos descriptivos variables muestra 2022
descriptivos2022 = plotOutput("Violines", height = 250) 
correlaciones2022 = plotOutput("Correlaciones", height = 250) 

# Gráficos de niveles de riesgo
nivelesderiesgo = plotOutput("Niveles", height = 250)

########################### Distribución de paneles: Panel 2 ###########################

# Sidebar Panel de la primera hoja
# sidebar2 = sidebarPanel(width = 2,
#                         Reboot, ParcialReboot
# )

main2 = fluidPage(fluidRow(box(id = "p2t0", width = 12, title = "Análisis descriptivo 2021",
                               collapsible = TRUE, solidHeader = TRUE, status = "primary", collapsed = FALSE,
                               fluidRow(
                                 column(width = 3,
                                        fluidRow(infoBox("Aprobados", aprobados, icon = icon("users"), width = 12, fill = TRUE, color = "blue")),
                                        fluidRow(infoBox("Reprobados", reprobados, icon = icon("users"), width = 12, fill = TRUE, color = "red"))
                                        ),
                                 column(width = 3, hist.nem),
                                 column(width = 3, hist.ptu),
                                 column(width = 3, hist.logro)
                                 ),
                               fluidRow(column(width = 3, corrs),
                                        column(width = 9, h4("Resumen de las covariables"), summary.2021))
                               )
                           ),
                  fluidRow(box(id = "p2t1", width = 12, title = span("Modelamiento de la reprobación (GLM)"), # , style = "color: black;"
                               collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary",
                               tabBox(id = "p2t1_0", width = 12,
                                 tabPanel(id = "p2t1_0_1", title = "Elaboración del modelo",
                                          fluidRow(
                                            column(width = 3,
                                                   box(width = NULL, title = span("Imputación", style = "color: white;"), status = "primary",
                                                       solidHeader = TRUE,
                                                       selectInput(inputId = "tecnica_imputacion", label = "Técnica",
                                                                   choices = c("Media muestral", "Muestreo simple", "Árboles de regresión",
                                                                               "Bosque aleatorio", "Regresión lineal bayesiana"),
                                                                   selected = "Bosque aleatorio"),
                                                         condition = "n_imputaciones_multiples != 'Omisión'",
                                                       sliderInput(inputId = "n_imputaciones_multiples",
                                                                   label = "Número de imputaciones múltiples",
                                                                   min = 1, max = 10, value = 5, step = 1),
                                                       sliderInput(inputId = "n_iteraciones", label = "Número de iteraciones",
                                                                   min = 1, max = 10, value = 5, step = 1)),
                                                   actionButton(inputId = "cambios_modelo", label = "Aplicar cambios",
                                                                class = "btn-primary", style = "color: white;"), br(), br(),
                                                   actionButton(inputId = "reiniciar_cambios_modelo", label = "Reiniciar selecciones")
                                            ),
                                            column(width = 3,
                                                   box(width = NULL, title = span("Parámetros", style = "color: white;"), status = "primary",
                                                       solidHeader = TRUE,
                                                       checkboxGroupInput(inputId = "covariables", label = "Covariables",
                                                                          choices = c("NEM", "PTU", "Logro"),
                                                                          selected = c("NEM", "PTU", "Logro")),
                                                       sliderInput(inputId = "n_entrenamiento", label = "Partición de entrenamiento",
                                                                   min = 0.6, max = 0.9, value = 0.8, step = 0.05),
                                                       radioButtons(inputId = "enlace_modelo", label = "Función de enlace",
                                                                    choices = c("logit", "cloglog", "probit"), selected = "logit"))
                                            ),
                                            column(width = 3,
                                                   box(width = NULL, title = span("Balanceo", style = "color: white;"), status = "primary",
                                                       solidHeader = TRUE,
                                                       selectInput(inputId = "tecnica_balanceo", label = "Técnica de balanceo",
                                                                   choices = c("Undersampling", "Oversampling", "Smote", "Adasyn"),
                                                                   selected = "Smote"),
                                                       # conditionalPanel(
                                                       #   condition = "input.tecnica_balanceo == 'Undersampling'",
                                                       #   sliderInput(inputId = "n_balanceo1", label = "Tamaño del balanceo",
                                                       #               min = 100, max = 900, value = 500, step = 10)
                                                       # ),
                                                       # conditionalPanel(
                                                       #   condition = "input.tecnica_balanceo == 'Oversampling'",
                                                       #   sliderInput(inputId = "n_balanceo2", label = "Tamaño del balanceo",
                                                       #               min = 100, max = 900, value = 500, step = 10)
                                                       # ),
                                                       conditionalPanel(
                                                         condition = "input.tecnica_balanceo == 'Smote'",
                                                         sliderInput(inputId = "n_vecinos1", label = "Número de vecinos",
                                                                     min = 1, max = 10, value = 5, step = 1),
                                                         checkboxInput(inputId = "repetir_observaciones", label = "Repetición de observaciones"),
                                                         selectInput(inputId = "n_distancia1", label = "Distancia",
                                                                     choices = c("Manhattan", "Euclidean", "Canberra", "Chebyshev"),
                                                                     selected = "Canberra")
                                                       ),
                                                       conditionalPanel(
                                                         condition = "input.tecnica_balanceo == 'Adasyn'",
                                                         sliderInput(inputId = "equilibrio", label = "Nivel de equilibrio",
                                                                     min = 1, max = 5, value = 1, step = 1),
                                                         sliderInput(inputId = "desequilibrio", label = "Umbral de desiquilibrio",
                                                                     min = 0.60, max = 0.99, value = 0.95, step = 0.01),
                                                         sliderInput(inputId = "n_vecinos2", label = "Número de vecinos",
                                                                     min = 1, max = 10, value = 5, step = 1),
                                                         selectInput(inputId = "n_distancia2", label = "Distancia",
                                                                     choices = c("Manhattan", "Euclidean", "Canberra", "Chebyshev"),
                                                                     selected = "Canberra")
                                                         )
                                                       )
                                                   ),
                                            column(width = 3,
                                                   box(width = NULL, title = span("Opciones", style = "color: white;"), status = "primary",
                                                       solidHeader = TRUE,
                                                       checkboxInput(inputId = "p2t1_8", label = "Estandarizar variables", value = TRUE),
                                                       checkboxInput(inputId = "p2t1_4", label = "Fijar semilla", value = TRUE),
                                                       checkboxInput(inputId = "na_omit",
                                                                     label = "Omisión de valores faltantes (anula el proceso de imputación en el entrenamiento pero no en la predicción de muestras futuras)",
                                                                     value = FALSE),
                                                       checkboxInput(inputId = "p2t1_6",
                                                                     label = "Eliminar puntos influyentes presentes en los 3 gráficos de influencia (pestaña de Resultados)",
                                                                     value = TRUE),
                                                       checkboxInput(inputId = "p2t1_7",
                                                                     label = "Eliminar puntos influyentes presentes en al menos 1 de los 3 gráficos de influencia",
                                                                     value = FALSE))
                                                   )
                                            )
                                          ),
                               tabPanel(id = "p2t_0_2", width = 12, title = "Resultados",
                                        fluidRow(column(width = 12, h3("Gráficos de influencia", align = "center"), ajustes)),
                                        fluidRow(column(width = 6, h3("Resumen del modelo", align = "center"), summary.model),
                                                 column(width = 6, h3("Medidas de bondad y ajuste", align = "center"), bondad.ajuste,
                                                        devianza)),
                                        fluidRow(column(width = 12, h3("Métricas de entrenamiento", align = "center"),
                                                        valueBox(train.accuracy, "Exactitud", icon = icon("menu-hamburger", lib ="glyphicon"), width = 3, color = "aqua"),
                                                        valueBox(train.precision, "Precisión", icon = icon("menu-hamburger", lib ="glyphicon"), width = 3, color = "yellow"),
                                                        valueBox(train.recall, "Sensibilidad", icon = icon("menu-hamburger", lib ="glyphicon"), width = 3, color = "green"),
                                                        valueBox(train.f1, "Puntaje F1", icon = icon("menu-hamburger", lib ="glyphicon"), width = 3, color = "purple")
                                                        )
                                                 ),
                                        fluidRow(column(width = 12, h3("Métricas de prueba", align = "center"),
                                                        valueBox(test.accuracy, "Exactitud", icon = icon("menu-hamburger", lib ="glyphicon"), width = 3, color = "aqua"),
                                                        valueBox(test.precision, "Precisión", icon = icon("menu-hamburger", lib ="glyphicon"), width = 3, color = "yellow"),
                                                        valueBox(test.recall, "Sensibilidad", icon = icon("menu-hamburger", lib ="glyphicon"), width = 3, color = "green"),
                                                        valueBox(test.f1, "Puntaje F1", icon = icon("menu-hamburger", lib ="glyphicon"), width = 3, color = "purple")
                                                        )
                                        ),
                                        fluidRow(column(width = 4, ROC),
                                                 column(width = 4, PR),
                                                 column(width = 4, KS))
                                        ),
                               )
                               )
                           ),
                  fluidRow(box(id = "p2t3", width = 12, title = "Predicciones de riesgo inicial 2022",
                               collapsible = TRUE, collapsed = FALSE, solidHeader = TRUE, status = "primary",
                               fluidRow(column(width = 6,
                                               selectInput(inputId = "carrera2", label = "Carrera",
                                                           choices = c("Institucional",
                                                                       unique(cods$CARRERA)[order(unique(cods$CARRERA))]))
                                               ),
                                        column(width = 6, selectInput(inputId = "facultad2", label = "Facultad",
                                                                      choices = c("Institucional", 
                                                                                  unique(cods$FACULTAD)[order(unique(cods$FACULTAD))],
                                                                                  "Ingeniería 2030"))
                                               )
                                        ),
                               fluidRow(column(width = 6, descriptivos2022),
                                        column(width = 6, correlaciones2022)
                                        ),
                               fluidRow(column(width = 7, h4("Resumen de las covariables"), summary.2022),
                                        column(width = 5, nivelesderiesgo)
                                        )
                               )
                           )
)


########################### Despliegue ###########################

# Primera hoja
hoja1 = tabItem(tabName = "diagnostico", fluidPage(sidebarLayout(sidebar1, main1)))
# Segunda hoja
hoja2 = tabItem(tabName = "riesgoinicial", main2)

# Cuerpo de todas hojas
bodypanel = dashboardBody(tabItems(hoja1, hoja2))

# Panel completo
ui = dashboardPage(header = headerpanel, sidebar = sidebar, body = bodypanel)