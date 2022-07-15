server = function(input, output, session){
  
  #### Carga de datos ####
  
  df = list(
    "M" = read.xlsx("base+pruebas.xlsx", sheet = 2),
    "L" = read.xlsx("base+pruebas.xlsx", sheet = 1),
    "G" = read.xlsx("base+pruebas.xlsx", sheet = 4),
    "T" = read.xlsx("base+pruebas.xlsx", sheet = 3))
  cods = read.xlsx("Asignaturas.xlsx")
  regulares = read.xlsx("Regulares.xlsx")
  
  risk = read.xlsx("risk.xlsx")
  risk$y = factor(risk$y)
  
  df.predict = read.xlsx("base+prediccion.xlsx")
  
  # Competencias por área
  c.mat = list(
    c(13,14,15,18,20,30,21,32),
    c(1,2,4,6,7,8,11,16),
    c(22,23,24,25,26,27,28,29),
    c(3,5,9,10,12,17,19,21)
  )
  
  c.leng = list(
    c(15,16,21,22,25,28,33,34,35),
    c(1:12,36),
    c(13,14,19,20,31,32,37,38),
    c(17,18,23,24,26,27,29,30)
  )
  
  c.geo = list(
    c(42,43,44,45,46,47)-32,
    c(48:53)-32,
    c(33:41)-32
  )
  
  # Resultados de aprendizaje por área
  
  r.mat = list(
    c(13,18,20,30),
    c(14,15,31,32),
    c(1,2,4,6),
    c(7,8,11,16),
    c(26,27,28,29),
    c(22,23,24,25),
    c(12,17,19,21),
    c(3,5,9,10)
  )
  
  r.len = list(
    c(15,16,22,28),
    c(21,25,33,34,35),
    c(1,4,9,10,36),
    c(2,5,8,11),
    c(3,6,7,12),
    c(13,14,19,20),
    c(31,32,37,38),
    c(17,23,27,29),
    c(18,24,26,30)
  )
  
  r.geo = list(
    c(42,43,44)-32,
    c(45,46,47)-32,
    c(48,49,50)-32,
    c(51,52,53)-32,
    c(33,34,35)-32,
    c(39,40,41)-32,
    c(37,37,38)-32
  )
  
  # Filtro de duplicados y valores como factor
  df = lapply(df, function(page){
    page = unique(page)
    page$sexo = factor(page$sexo, levels = c("Hombre", "Mujer"))
    page$tipo_ingreso = factor(page$tipo_ingreso, levels = c("Otras","PTU"))
    page = page[!is.na(page$prs_rut),]
    return(page)
  })
  
  #### Funciones de filtro y edición panel 1 ####
  
  # Reinicio de parámetros de entrada y de posición de pestañas
  observeEvent(input$reboot, {
    updateSelectInput(session, "facultad",
                      choices = c("Institucional",
                                  unique(cods$FACULTAD)[order(unique(cods$FACULTAD))],
                                  "Ingeniería 2030"))
    updateSelectInput(session, "carrera",
                      choices = c("Institucional",
                                  unique(cods$CARRERA)[order(unique(cods$CARRERA))]))
    updateSelectInput(session, "sexo",
                      choices = c("Ambos","Hombre", "Mujer"))
    updateSelectInput(session ,"via",
                      choices = c("Ambas","PTU", "Otras"))
    updateSelectInput(session ,"cohorte",
                      choices = c(2022:2018))
    
    if(input$parcialReboot == TRUE){
      updateTabsetPanel(session, "p1t1",
                        selected = "p1t1.1")
      updateTabsetPanel(session, "p1t2",
                        selected = "p1t2.1")
      updateTabsetPanel(session, "p1t3",
                        selected = "p1t3.1")
      updateTabsetPanel(session, "p1t4",
                        selected = "p1t4.1")
      updateTabsetPanel(session, "p1t5.1",
                        selected = "p1t5.1.1")
    }
  })
  
  # Gráfico vacío con mensaje
  empty.plot = function(text){
    aux = data.frame()
    ggplot(aux) + geom_point() + xlim(-5, 5) + ylim(-5, 5) + theme_void() +
      annotate("text", label = text,
               x = 0, y = 0, size = 8, colour = "black", hjust = 0.5)
  }
  
  # Conversión de datos a porcentaje
  ftabla = function(x){
    aux = paste0("(", format(round(x/sum(x)*100,1), nsmall = 1), "%", ")")
    aux = gsub(" ", "", aux)
    aux = paste(x, aux, sep = " ")
    aux = ifelse(grepl("(%)", aux, fixed = T), "0 (0.0 %)", aux)
    return(aux)
  }
  
  # Función para filtrar por sexo las bases que corresponden
  sex.filter = function(base, sex){
    base = lapply(base, function(page){
      if(sex == "Ambos") {
        return(page)
      } else {
        return(subset(page, sexo == sex))
      }
    })
    return(base)
  }
  
  # Función para filtrar por vía de ingreso las bases que corresponden
  entrie.filter = function(base, filter.entrie){
    base = lapply(base, function(page){
      if(filter.entrie == "Ambas"){ 
        return(page)
      } else{
        return(subset(page, tipo_ingreso == filter.entrie))
      }
    })
    return(base)
  }
  
  # Función para filtrar por cohorte (no terminada y no implementada)
  cohorte.filter = function(base, cohorte){
    if(cohorte != 2022){ # Simulación de otras cortes en base a muestreo
      set.seed(100)
      base = lapply(base, function(page){
        # Toca hacer un muestreo por carrera para evitar errores
        # page = page[sample(c(1:dim(page)[1]), round(dim(page)[1]*1,0), replace = F),]
        return(page)
      })
      return(base)
    } else {
      return(base) 
    }
  }

  # Filtros de la base de datos
  filter.df = reactive({
    
    if(input$carrera == "Institucional" & input$facultad == "Institucional"){
      sex = ifelse(input$sexo == "Ambos", c("Hombre", "Mujer"), "Hombre")
      aux = sex.filter(df, input$sexo)
      aux = entrie.filter(aux, input$via)
      # aux = cohorte.filter(aux, input$cohorte)
      # return(aux)  
    } else if (input$carrera != "Institucional"){
      cod = cods$CODIGO[cods$CARRERA == input$carrera]
      aux = lapply(df, function(page){
        return(subset(page, codigo_carrera == cod))
      }) 
      aux = sex.filter(aux, input$sexo)
      aux = entrie.filter(aux, input$via)
      # aux = cohorte.filter(aux, input$cohorte)
      updateSelectInput(session, "facultad", 
                        choices = c("Institucional",
                                    unique(cods$FACULTAD)[order(unique(cods$FACULTAD))],
                                    "Ingeniería 2030"))
      # return(aux)  
    } else {
      if(input$facultad == "Ingeniería 2030"){
        cod = c(21041, 21049, 21074, 21075, 21076, 21087, 21073)
        aux = lapply(df, function(page){
          return(subset(page, codigo_carrera %in% cod))
        })
        aux = sex.filter(aux, input$sexo)
        aux = entrie.filter(aux, input$via)
        # aux = cohorte.filter(aux, input$cohorte)
        # return(aux)
      } else {
        cod = cods$CODIGO[cods$FACULTAD == input$facultad] 
        aux = lapply(df, function(page){
          return(subset(page, codigo_carrera %in% cod))
        }) 
        aux = sex.filter(aux, input$sexo)
        aux = entrie.filter(aux, input$via)
        # aux = cohorte.filter(aux, input$cohorte)
        # return(aux)  
      }
    }
    # if(Total(aux, "prs_rut") == 0){
    #   showNotification("Elija otros filtros", type = "error")
    # }
    return(aux)
  }) %>%
    bindCache(input$carrera, input$facultad, input$sexo, input$via, input$cohorte)
  
  #### Funciones del panel 1 ####
  
  # Función para las tasas de respuesta generales
  answer.rates = function(base){
    
    tasas = function(){
      aux.tasas = lapply(base, function(aux.base){
        aux.rut = aux.base$prs_rut
        aux.tasa = round(length(aux.rut)/length(regulares$RUT[regulares$codigo_carrera %in% unique(aux.base$codigo_carrera)])*100, 1) 
        aux.tasa = format(aux.tasa, nsmall = 1)
        aux.tasa = paste(aux.tasa, "%", sep = " ")
        aux.tasa = gsub("NaN", "0.0", aux.tasa)
        return(aux.tasa)
      })
      aux.tasas = unlist(aux.tasas)
      return(aux.tasas)
    }
    
    tasa.respuesta = c(tasas()[1:3], # M, L y TIC
                       tasas()[4]) # G
    tasa.respuesta = tasa.respuesta[c(1,2,4,3)]
    
    estudiantes.por.prueba = lapply(base, function(prueba){
      return(dim(prueba)[1])
    })
    estudiantes.por.prueba = estudiantes.por.prueba[c(1,2,4,3)]
    
    conteo = rbind(estudiantes.por.prueba, unlist(tasa.respuesta))
    
    if(dim(base[[3]])[1] > 0){
      colnames(conteo) = c("Matemática", "Lenguaje", "TIC", "Geometría")
      row.names(conteo) = c("Estudiantes", "Tasa de respuesta")
      knitr::kable(conteo, format = "html", align = "c") |>
        kable_styling(position = "center", bootstrap_options = c("striped", "condensed")) |>
        add_header_above(c("","Áreas del PMR" = 3, "Eje diferenciado \n (Matemática)" = 1))
    } else{
      conteo = conteo[,c(-4)]
      colnames(conteo) = c("Matemática", "Lenguaje", "TIC")
      row.names(conteo) = c("Estudiante", "Tasa de respuesta")
      knitr::kable(conteo, format = "html", align = "c",
            caption = "") |>
        kable_styling(position = "center", bootstrap_options = c("striped", "condensed")) |>
        add_header_above(c("","Áreas del PMR" = 3))
    }
  }
  
  # Función para las tasas de respuesta por sexo
  sex.rates = function(pruebas, aux.sex){
    
    col = c("Hombre", "Mujer", "Ambos")
    col = col[col == aux.sex]
    if(col == "Ambos") col = c("Hombre", "Mujer")
    
    if(length(col) == 2){
      tabla = ftabla(table(pruebas[["M"]]$sexo))
      tabla1 = ftabla(table(pruebas[["L"]]$sexo))
      tabla2 = ftabla(table(pruebas[["T"]]$sexo))
      tabla3 = ftabla(table(pruebas[["G"]]$sexo)) 
    } else {
      tabla = length(na.omit(pruebas[["M"]]$sexo))
      tabla1 = length(na.omit(pruebas[["L"]]$sexo))
      tabla2 = length(na.omit(pruebas[["T"]]$sexo))
      tabla3 = length(na.omit(pruebas[["G"]]$sexo))
    }
    
    if(dim(pruebas[[3]])[1] > 0){
      tabla = rbind(tabla, tabla1, tabla2, tabla3)
      tabla = structure(.Data = tabla, .dim = c(4,length(col)),
                        .Dimnames = list(c("Matemática", "Lenguaje", "TIC", "Geometría"),
                                         c(col)))
      
      knitr::kable(tabla, format = "html", align = "c",
            caption = "") |>
        kable_styling(position = "center",  bootstrap_options = c("striped", "condensed")) |>
        add_header_above(c("","Sexo" = length(col))) |>
        pack_rows("Área", 1,3) |>
        pack_rows("Eje Diferenciado", 4,4)
    } else {
      tabla = rbind(tabla, tabla1, tabla2)
      tabla = structure(.Data = tabla, .dim = c(3,length(col)),
                        .Dimnames = list(c("Matemática", "Lenguaje", "TIC"),
                                         c(col)))
      
      knitr::kable(tabla, format = "html", align = "c", linesep = "",
            caption = "") |>
        kable_styling(position = "center", bootstrap_options = c("striped", "condensed")) |>
        add_header_above(c("","Sexo" = length(col))) |>
        pack_rows("Área", 1,3)
    }
  }
  
  # Función para las tasas de respuesta por vía de ingreso
  entries.rates = function(pruebas, aux.entrie){
    
    col = c("PTU", "Otras", "Ambas")
    col = col[col == aux.entrie]
    if(col == "Ambas") col = c("Otras", "PTU")
    
    if(length(col) == 2){
      tabla = ftabla(table(pruebas[["M"]]$tipo_ingreso))
      tabla1 = ftabla(table(pruebas[["L"]]$tipo_ingreso))
      tabla2 = ftabla(table(pruebas[["G"]]$tipo_ingreso))
      tabla3 = ftabla(table(pruebas[["T"]]$tipo_ingreso))
    } else {
      tabla = length(pruebas[["M"]]$tipo_ingreso)
      tabla1 = length(pruebas[["L"]]$tipo_ingreso)
      tabla2 = length(pruebas[["G"]]$tipo_ingreso)
      tabla3 = length(pruebas[["T"]]$tipo_ingreso)
    }
    
    if(dim(pruebas[[3]])[1] > 0){
      tabla = rbind(tabla, tabla1, tabla3, tabla2)
      rownames(tabla) = c("Matemática","Lenguaje","TIC","Geometría")
      colnames(tabla) = col
      
      knitr::kable(tabla, format = "html", align = "c",
            caption = "") |>
        kable_styling(position = "center", bootstrap_options = c("striped", "condensed")) |>
        add_header_above(c("","Vía de ingreso" = length(col))) |>
        pack_rows("Área", 1,3) |>
        pack_rows("Eje Diferenciado", 4,4)
    } else {
      tabla = rbind(tabla, tabla1, tabla3)
      rownames(tabla) = c("Matemática","Lenguaje","TIC")
      colnames(tabla) = col
      
      knitr::kable(tabla, format = "html", align = "c",
            caption = "") |>
        kable_styling(position = "center", bootstrap_options = c("striped", "condensed")) |>
        add_header_above(c("","Vía de ingreso" = length(col))) |>
        pack_rows("Área", 1,3)
    }
  }
  
  # Función para los histogramas de proporción de logro
  histg = function(base, variable, title = "Distribución de logros"){
    
    if(dim(base)[1] == 0){
      empty.plot("No hay información")
    } else {
      g = ggplot(data = base, aes(.data[[variable]])) +
        geom_histogram(breaks = seq(0,1,0.1), closed = "left", aes(fill = ..count..), col = "white") # , col = "white" out aes()
      
      aux = ggplot_build(g)$data[[1]]$count
      g + stat_bin(breaks = seq(0,1,0.1), closed = "left", geom = 'text', # color = "black",
                   aes(label = paste(format(round(100*(..count..)/sum(..count..),1), nsmall = 1), "%", sep = "")),
                   position = "stack", vjust = -0.4, size = 4) +
        labs(title = title, x = "Proporción de logro", y = "Frecuencia de estudiantes") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold.italic"),
              axis.title = element_text(size = 12),
              axis.text = element_text(size = 12),
              legend.position = "none") +
        scale_x_continuous(breaks = seq(0,1,0.1), labels = seq(0,1,0.1), limits = c(0,1)) +
        scale_y_continuous(limits = c(0, 1.2*max(aux))) +
        scale_fill_continuous(type = "gradient") + # low = "blue", high = "darkblue"
        annotate("text", label = paste("n = ", sum(aux), collapse = ""),
                 x = 0, y = 1.2*max(aux), size = 4, hjust = 0)
      # ggplotly(g)
    }
  }
  
  # Función para los gráficos de dispersión de proporción de logro vs --
  scatter = function(data, v1, v2, title, yname, filter.entrie = c("PTU")){
    
    cor = round(cor(data[[v1]], data[[v2]], use = "na.or.complete"),2)
    
    if(is.na(cor)){
      empty.plot("No hay información")
    } else {
      data = subset(data, tipo_ingreso %in% filter.entrie)
      ggplot(data = data,
             mapping = aes(y = .data[[v1]], x = .data[[v2]])) +
        geom_point(colour = "darkblue", size = 1.5, alpha = 0.5) + # col = "black"
        geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) + # , color="red"
        theme_minimal() +
        labs(title = title, x = "Proporción de logro", y = yname) + 
        theme(legend.position = "none",
              axis.title = element_text(size = 11),
              axis.text = element_text(size = 10),
              plot.title = element_text(hjust = 0.5, face = "bold.italic")) +
        scale_x_continuous(limits = c(0,1)) +
        scale_y_continuous(limits = c(150,850))  +
        annotate("text", label = paste("Correlación: ", cor, collapse = ""),
                 x = 0, y = 150, size = 4, hjust = 0) # , colour = "black"
    }
  }
  
  # Tasas de respuesta en TIC -- 1
  tics1 = function(base){
    
    if(dim(base)[1] == 0){
      empty.plot("No hay información")
    } else {
      tabla.tic = function(rango, titulo){
        rango = rango + 7
        t = unlist(base[,rango])
        t = factor(t, levels = c("No sé hacerlo/ lo desconozco",
                                 "Puedo hacerlo, pero necesito ayuda",
                                 "Sé hacerlo correctamente"))
        t = prop.table(table(t))
        
        ggplot(data = as.data.frame(t), mapping = aes(x = t, y = Freq, fill = t)) +
          geom_bar(stat = "identity", position = "dodge") + 
          labs(title = titulo) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold.italic"), # , color = "black"
                axis.text = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                legend.title = element_blank(),
                legend.text = element_text(size = 13, hjust = 0),
                legend.spacing.y = unit(1.0,"cm")) +
          guides(fill = guide_legend(byrow = TRUE)) +
          scale_fill_manual(values = viridis(3, begin = 0.1, end = 0.5, alpha = 1),
                            labels = c("No sé hacerlo/ \nlo desconozco",
                                       "Puedo hacerlo, \npero necesito ayuda",
                                       "Sé hacerlo \ncorrectamente")) +
          scale_y_continuous(limits = c(0,1.1)) +
          geom_text(size = 4, mapping = aes(label = paste(format(round(Freq*100,1), nsmall = 1), "%", sep = "")),
                    vjust = -0.4, position = "stack")
        
      }
      
      t1 = tabla.tic(c(20:24), " Eje: Convivencia digital")
      t2 = tabla.tic(c(1:13), "Eje: Información")
      t3 = tabla.tic(c(14:19), "Eje: Tecnología")
      
      ggarrange(t1,t2,t3, nrow = 1, common.legend = T, legend = "right")
    }
  }
  
  # Tasas de respuesta en TIC -- 2
  tics2 = function(base){
    
    if(dim(base)[1] == 0){
      empty.plot("No hay información")
    } else {
      tabla.tic = function(rango, titulo){
        rango = rango + 7
        t = unlist(base[,rango])
        t = factor(t, levels = c("Nunca", "Ocasionalmente", "Siempre"))
        t = prop.table(table(t))
        
        ggplot(data = as.data.frame(t), mapping = aes(x = t, y = Freq, fill = t)) +
          geom_bar(stat = "identity", position = "dodge") + 
          labs(title = titulo) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold.italic"), # , color = "black"
                axis.text = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                legend.title = element_blank(),
                legend.text = element_text(size = 13, hjust = 0)) +
          scale_fill_manual(values = viridis(3, begin = 0.1, end = 0.5, alpha = 1)) +
          scale_y_continuous(limits = c(0,1.1)) +
          geom_text(size = 4, mapping = aes(label = paste(format(round(Freq*100,1), nsmall = 1), "%", sep = "")),
                    vjust = -0.4, position = "stack")
      }
      tabla.tic(c(25:30), "Eje: Convivencia digital")
    }
  }
  
  # Indicadores de la parte superior del panel
  Total = function(base, variable){
    base = lapply(base, function(prueba){
      if(variable == "prs_rut"){
        return(prueba[,c(1)])
      } else {
        col = which(colnames(prueba) == variable)
        return(prueba[,c(1,col)])
      }
    })
    
    if(variable == "prs_rut"){
      base = c(base[[1]], base[[2]], base[[3]], base[[4]])
      base = unique(base)
      return(length(base))
    } else {
      base = rbind(base[[1]], base[[2]], base[[3]], base[[4]])
      base = unique(base)
      base = base[!duplicated(base[,1]),]
      base = table(base[,2])
      base = as.matrix(base)
      return(base) 
    }
  }
  
  # Tablas de resumen por competencia
  resumen.competencia = function(base, competencias){

    # Competencias es una lista
    medidas = lapply(competencias, function(c){
      x = base[, c + 7]
      x$Logro = apply(x, 1, function(fila){
        fila = as.numeric(fila)
        return(sum(fila)/length(fila))
      })
      x.barra = format(round(mean(x$Logro),2), nsmall = 2)
      x.sd = format(round(sd(x$Logro),2), nsmall = 2)
      x.cv = paste(format(round(sd(x$Logro/mean(x$Logro))*100,0), nsmall = 0),"%",sep = "")
      return(c(x.barra,x.sd,x.cv))
    })
    
    if(length(medidas) == 4){
      medidas = rbind(medidas[[1]], medidas[[2]], medidas[[3]], medidas[[4]])
      nombres = c("A" , "B", "C", "D")
    } else{ 
      medidas = rbind(medidas[[1]], medidas[[2]], medidas[[3]])
      nombres = c("A" , "B", "C")
    }
    
    medidas = apply(medidas, 2, function(fila){
      fila = ifelse(grepl("NA", fila), "-", fila)
      fila = ifelse(grepl("NaN", fila), "-", fila)
      return(fila)
    })
    
    medidas = structure(.Data = medidas,
                        .Dim = c(length(nombres),3),
                        .Dimnames = list(nombres, c("Promedio",
                                                    "Desviación estándar",
                                                    "Coeficiente de variación")))
    
    kbl(medidas, format = "html", align = "c") |>
      kable_styling(position = "center", bootstrap_options = c("striped", "condensed"))
  }
  
  # Función para gráficos de barras apiladas para la proporción de niveles por competencia
  circular.plot = function(base, competencias){
    
    if(dim(base)[1] == 0){
      empty.plot("No hay información")
    } else {
      data = lapply(competencias, function(c){
        x = base[, c + 7]
        x$Logro = apply(x, 1, function(fila){
          fila = as.numeric(fila)
          return(sum(fila)/length(fila))
        })
        Nivel = ifelse(x$Logro <= 0.25, "Insuficiente",
                       ifelse(x$Logro <= 0.50, "Básico",
                              ifelse(x$Logro <= 0.79, "Elemental", "Adecuado")))
        Nivel = factor(Nivel, levels = c("Adecuado", "Elemental", "Básico", "Insuficiente"))
        Nivel = as.data.frame(prop.table(table(Nivel))*100)
        return(Nivel)
      })
      
      if(length(data) == 4){
        data = rbind(data[[1]], data[[2]], data[[3]], data[[4]])
        data$group = c(rep("A",4), rep("B",4), rep("C",4), rep("D",4))
      } else{ 
        data = rbind(data[[1]], data[[2]], data[[3]])
        data$group = c(rep("A",4), rep("B",4), rep("C",4))
      }
      
      colnames(data)[1:2] = c("individual","value")
      data$group = as.factor(data$group)
      
      empty_bar = 3
      to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
      colnames(to_add) = colnames(data)
      to_add$group = rep(levels(data$group), each = empty_bar)
      data = rbind(data, to_add)
      data = data %>% arrange(group)
      data$id = seq(1, nrow(data))
      
      label_data = data
      number_of_bar = nrow(label_data)
      angle = 90 - 360 * (label_data$id-0.5) / number_of_bar
      label_data$hjust = ifelse( angle < -90, 1, 0)
      label_data$angle = ifelse(angle < -90, angle + 180, angle)
      
      base_data = data %>% 
        group_by(group) %>% 
        summarize(start = min(id), end = max(id) - empty_bar) %>% 
        rowwise() %>% 
        mutate(title = mean(c(start, end)))
      
      grid_data = base_data
      grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
      grid_data$start = grid_data$start - 1
      grid_data = grid_data[-1,]
      
      if(length(unique(data$group)) == 4){
        pos.leg = c(0.8,0.6)
      } else {
        pos.leg = c(0.8,0.5)
      }
      
      p = ggplot(data) +
        geom_bar(aes(x = as.factor(id), y = value, fill = as.factor(individual)),
                 stat = "identity", alpha = 0.8) +
        ylim(-40,170) +
        theme_minimal() +
        coord_polar() +
        theme(
          legend.position = pos.leg,
          legend.direction = "vertical",
          legend.title = element_text(size = 8, face = "italic"),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.2, "cm"),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(rep(-3,4), "cm")
        ) +
        scale_fill_manual(name = "Niveles", values = viridis(4)) +
        
        geom_text(data = label_data,
                  aes(x = id, y = value + 5, label = paste(round(value,0),"%", sep = ""), hjust = hjust),
                  color = "black", fontface = "bold", alpha = 1,
                  size = 3, angle = label_data$angle, inherit.aes = FALSE ) +
        
        geom_segment(data = base_data,
                     aes(x = start, y = -5, xend = end, yend = -5),
                     colour = "black", alpha = 1, size = 0.9 , inherit.aes = FALSE)  +
        
        geom_text(data = base_data,
                  aes(x = title, y = -18, label = group),
                  hjust = ifelse(length(unique(data$group)) == 3, c(0,0,0), c(1,1,0,0)),
                  colour = "black", alpha = 1,
                  size = 4, fontface = "bold", inherit.aes = FALSE)
  
      p
    }
  }
  
  # Función para gráfico Lollipop de frecuencias de competencias en nivel adecuado
  lollipop = function(base, competencias){
    
    if(dim(base)[1] == 0){
      empty.plot("No hay información")
    } else {
      data = lapply(competencias, function(c){
        x = base[, c + 7]
        x$Logro = apply(x, 1, function(fila){
          fila = as.numeric(fila)
          return(sum(fila)/length(fila))
        })
        Nivel = ifelse(x$Logro <= 0.25, "Insuficiente",
                       ifelse(x$Logro <= 0.50, "Básico",
                              ifelse(x$Logro <= 0.79, "Elemental", "Adecuado")))
        # Nivel = factor(Nivel, levels = c("Adecuado", "Elemental", "Básico", "Insuficiente"))
        return(Nivel)
      })
      
      base = data[[1]]
      for (i in 2:length(data)) {
        base = cbind(base, data[[i]])
      }
      
      base = apply(base, 1, function(fila){
        return(sum(fila == "Adecuado"))
      })
      
      base = factor(base, levels = 0:ifelse(length(data) == 4, 4, 3))
      base = as.data.frame(table(base))
      colnames(base) = c("x", "y")
      
      ggplot(base, aes(x = x, y = y)) +
        geom_segment(aes(x = x, xend = x, y = 0, yend = y),
                     color = "orange", lwd = 1, alpha = 0.8) +
        geom_point(size = 11, pch = 21, fill = "orange", colour = "orange", alpha = 0.8) +
        geom_text(aes(label = paste(format(round(y/sum(y)*100), nsmall = 0), "%", sep = "")),
                  color = "black", size = 4) +
        scale_x_discrete(labels = 0:4) +
        coord_flip() +
        theme_minimal() +
        theme(axis.text = element_text(colour = "black", size = 12),
              axis.title = element_text(size = 13),
              plot.title = element_text(face = "bold.italic", hjust = 0.5, size = 14)) +
        labs(title = "Frecuencia de competencias \n en nivel adecuado",
             x = "Cantidad de competencias",
             y = "Frecuencia")
      # ggplotly(g)
    }
  }
  
  # Función para el gráfico de red de los resultados de aprendizaje
  spiderchart = function(n, aprendizajes, zoom = 1.2, title, zoom2 = 1.5){
    
    if(dim(filter.df()[[n]])[1] == 0){
    plot(0,0, col = "white", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         xlim = c(-5,5) ,ylim = c(-5,5))
    text(0,0, "No hay información", cex = 2)
      # empty.plot("No hay información")
    } else {
      base = filter.df()[[n]]
      base = lapply(aprendizajes, function(c){
        Logro = apply(base[, c + 7], 1, function(fila){
          fila = as.numeric(fila)
          return(sum(fila)/length(fila))
        })
        return(mean(Logro))
      })
      base = unlist(base)
      base = rbind(rep(1,length(base)), rep(0, length(base)), base)
      if (n == 1) {
        colnames(base) = paste(c("A1:", "A2:", "B1:", "B2:", "C1:", "C2:", "D1:", "D2:"), format(round(base[3,],2), nsmall = 2))
      } else if (n == 2){
        colnames(base) = paste(c("A1:", "A2:", "B1:", "B2:", "B3:", "C1:", "C2:", "D1:", "D2:"), format(round(base[3,],2), nsmall = 2))
      } else {
        colnames(base) = paste(c("A1:", "A2:", "B1:", "B2:", "C1:", "C2:", "C3:"), format(round(base[3,],2), nsmall = 2))
      }

      base = as.data.frame(base)
      base = base[, c(1,dim(base)[2]:2)]

      fmsb::radarchart(base, axistype = 0, title = title, cex.main = zoom2,
                 pcol = rgb(0.2,0.5,0.5,0.9) , pfcol = rgb(0.2,0.5,0.5,0.5) , plwd = 2 ,
                 cglcol = "black", cglty = 2, axislabcol = "white", cglwd = 0.2,
                 vlcex = zoom)
    }
  }
  
  #### Elementos del panel 1 ####
  
  output$Hist.mat = renderPlot({
    histg(filter.df()[[1]], "Logro")
  }, height = 250)
  
  output$Hist.len = renderPlot({
    histg(filter.df()[[2]], "Logro")
  }, height = 250)
  
  output$Hist.geo = renderPlot({
    if(length(na.omit(filter.df()[[3]]$Logro)) == 0) {
      # showNotification("No hay gráfico")
      empty.plot("Esta carrera no rinde esta prueba")
    } else {
      histg(filter.df()[[3]], "Logro")
    }
  }, height = 250)
  
  output$Scatter1 = renderPlot({
    # if(input$via == "Otras"){
    #   empty.plot("No presentan puntajes de ingreso PTU")
    # } else {
    s1 = scatter(filter.df()[[1]], "MAT", "Logro",
                 "PTU Matemática vs \n Prueba de matemática",
                 "Puntaje PTU")
    
    s2 = scatter(filter.df()[[2]], "LEN", "Logro",
                 "PTU Lenguaje vs \n Prueba de lenguaje",
                 "Puntaje PTU")
    
    if(length(na.omit(filter.df()[[3]]$Logro)) != 0){
      s3 = scatter(filter.df()[[3]], "MAT", "Logro",
                   "PTU Matemática vs \n Prueba de geometría",
                   "Puntaje PTU")
      grid.arrange(s1, s2, s3, nrow = 1)
    } else {
      grid.arrange(s1, s2, nrow = 1)
    }
    # }
  }, height = 283)
  
  output$Scatter2 = renderPlot({
    s1 = scatter(filter.df()[[1]], "NEM", "Logro",
                 "Puntaje NEM vs \n Prueba de matemática",
                 "Puntaje NEM")
    s2 = scatter(filter.df()[[2]], "NEM", "Logro",
                 "Puntaje NEM vs \n Prueba de lenguaje",
                 "Puntaje NEM")
    
    if(length(na.omit(filter.df()[[3]]$Logro)) != 0){
      s3 = scatter(filter.df()[[3]], "NEM", "Logro",
                   "Puntaje NEM vs \n Prueba de geometría",
                   "Puntaje NEM")
      grid.arrange(s1, s2, s3, nrow= 1)
    } else {
      grid.arrange(s1, s2, nrow= 1)
    }
  }, height = 283)
  
  output$Scatter3 = renderPlot({
    empty.plot("Aún no se cuenta con esta información")
  }, height = 283)
  
  output$Tics1 = renderPlot({
    tics1(filter.df()[[4]])
  }, height = 250)
  
  output$Tics2 = renderPlot({
    tics2(filter.df()[[4]])
  }, width = 350, height = 250)
  
  output$RecuentoTotal = renderText({
    Total(filter.df(), "prs_rut")
  })
  
  output$RecuentoSexoH = function(){
    Total(filter.df(), "sexo")[1]
  }
  
  output$RecuentoSexoM = function(){
    Total(filter.df(), "sexo")[2]
  }
  
  output$RecuentoViaPTU = renderText({
    Total(filter.df(), "tipo_ingreso")[1]
  })
  
  output$RecuentoViaOtras = renderText({
    Total(filter.df(), "tipo_ingreso")[2]
  })
  
  output$TableRate = function(){
    answer.rates(filter.df())
  }
  
  output$TableSexRate = function(){
    sex.rates(filter.df(), input$sexo)
  }
  
  output$TableEntrieRate = function(){
    entries.rates(filter.df(), input$via)
  } 
  
  output$ResumenCompentenciasMate = function(){
    resumen.competencia(filter.df()[[1]], c.mat)
  }
  
  output$ResumenCompentenciasLeng = function(){
    resumen.competencia(filter.df()[[2]], c.leng)
  }
  
  output$ResumenCompentenciasGeo = function(){
    if(length(na.omit(filter.df()[[3]]$Logro)) == 0) {
      return(" ")
    } else {
      resumen.competencia(filter.df()[[3]], c.geo)
    }
  }
  
  # Gráficos circulares
  
  output$CompetenciasMate = renderPlot({
    circular.plot(filter.df()[[1]], c.mat)
  }, height = 550, res = 120)

  output$CompetenciasLeng = renderPlot({
    circular.plot(filter.df()[[2]], c.leng)
  }, height = 550, res = 120)

  output$CompetenciasGeo = renderPlot({
    if(length(na.omit(filter.df()[[3]]$Logro)) == 0) {
      empty.plot(" ")
    } else {
      circular.plot(filter.df()[[3]], c.geo)
    }
  }, height = 550, res = 120)
  
  # Gráficos de cinta
  
  # output$CompetenciasMate = renderPlot({
    # circular.plot(filter.df()[[1]], c.mat)
  # }, height = 550)
  
  # output$CompetenciasLeng = renderPlot({
    # circular.plot(filter.df()[[2]], c.leng)
  # }, height = 550)
  
  # output$CompetenciasGeo = renderPlot({
    # if(length(na.omit(filter.df()[[3]]$Logro)) == 0) {
    #   empty.plot(" ")
    # } else {
    #   circular.plot(filter.df()[[3]], c.geo)
    # }
  # }, height = 550)
  
  output$CompAdecuadasMat = renderPlot({
    lollipop(filter.df()[[1]], c.mat)
  })
  
  output$CompAdecuadasLen = renderPlot({
    lollipop(filter.df()[[2]], c.leng)
  })
  
  output$CompAdecuadasGeo = renderPlot({
    if(length(na.omit(filter.df()[[3]]$Logro)) == 0) {
      empty.plot("Esta carrera no rinde está prueba")
    } else {
      lollipop(filter.df()[[3]], c.geo)
    }
  })
  
  output$tituloCompetencias = renderUI({
    if(length(na.omit(filter.df()[[3]]$Logro)) == 0) {
      return(" ")
    } else {
      return(h3(p(em(strong("Distribución de niveles de desempeño"))), align = "center"))
    }
    
  })
  
  output$RAprendizajes = renderPlot({
    if(length(na.omit(filter.df()[[3]]$Logro)) == 0){
      par(mfrow = c(1,2), mar = c(0,0,5,0))
      spiderchart(1, r.mat, 1.2, title = "Matemática")
      spiderchart(2, r.len, 1.2, title = "Lenguaje")
    } else {
      par(mfrow = c(1,3), mar = c(0,0,5,0))
      spiderchart(1, r.mat, 1.8, title = "Matemática", 2)
      spiderchart(2, r.len, 1.8, title = "Lenguaje", 2)
      spiderchart(3, r.geo, 1.8, title = "Geometría", 2)
    }
    
  })
  
  #### Funciones de filtro y edición panel 2 ####
  
  observeEvent(input$reiniciar_cambios_modelo, {
    updateSelectInput(session, "tecnica_imputacion", selected = "Bosque aleatorio")
    updateSliderInput(session, "n_imputaciones_multiples", value = 5)
    updateSliderInput(session, "n_iteraciones", value = 5)
    updateCheckboxGroupInput(session, "covariables", selected = c("NEM", "PTU", "Logro"))
    updateSliderInput(session, "n_entrenamiento", value = 0.8)
    updateRadioButtons(session, "enlace_modelo", selected = "logit")
    updateSelectInput(session, "tecnica_balanceo", selected = "Smote")
    updateSliderInput(session, "n_vecinos1", value = 5)
    updateCheckboxInput(session, "repetir_observaciones", value = FALSE)
    updateSelectInput(session, "n_distancia1", selected = "Canberra")
    updateSliderInput(session, "equilibrio", value = 1)
    updateSliderInput(session, "desequilibrio", value = 0.95)
    updateSliderInput(session, "n_vecinos2", value = 5)
    updateSelectInput(session, "n_distancia2", selected = "Canberra")
    updateCheckboxInput(session, "p2t1_8", value = TRUE)
    updateCheckboxInput(session, "p2t1_4", value = TRUE)
    updateCheckboxInput(session, "na_omit", value = FALSE)
    updateCheckboxInput(session, "p2t1_6", value = TRUE)
    updateCheckboxInput(session, "p2t1_7", value = FALSE)
  })
  
  observeEvent(input$p2t1_6, {
    if(input$p2t1_6 == TRUE){
      updateCheckboxInput(session, "p2t1_7", value = FALSE)
    }
  })
  
  observeEvent(input$p2t1_7, {
    if(input$p2t1_7 == TRUE){
      updateCheckboxInput(session, "p2t1_6", value = FALSE)
    }
  })
  
  filter.predict.imputed = reactive({
    aux.predict = as.data.frame(cbind(df.predict, model()[["Predicciones2022"]]$Probabilidad))
    colnames(aux.predict) = c(colnames(df.predict), "Probabilidad")
    if(input$carrera2 == "Institucional" & input$facultad2 == "Institucional"){
      aux = aux.predict
    } else if (input$carrera2 != "Institucional"){
      cod = cods$CODIGO[cods$CARRERA == input$carrera2]
      aux = subset(aux.predict, RUT %in% regulares$RUT[grepl(cod, regulares$CARRERA)])
      updateSelectInput(session, "facultad2", 
                        choices = c("Institucional",
                                    unique(cods$FACULTAD)[order(unique(cods$FACULTAD))],
                                    "Ingeniería 2030"))
    } else {
      if(input$facultad2 == "Ingeniería 2030"){
        cod = c(21041, 21049, 21074, 21075, 21076, 21087, 21073)
        aux = subset(aux.predict, RUT %in% regulares$RUT[substr(regulares$CARRERA, 1, 5) %in% cod])
      } else {
        cod = cods$CODIGO[cods$FACULTAD == input$facultad2] 
        aux = subset(aux.predict, RUT %in% regulares$RUT[substr(regulares$CARRERA, 1, 5) %in% cod])
      }
    }
    return(aux)
  })
  
  #### Funciones del panel 2 ####
  
  histg2 = function(base, variable, title, domain, xname, yname){
    
    g = ggplot(data = base, aes(.data[[variable]])) +
      geom_histogram(closed = "left", aes(fill = ..count..), col = "white") # , col = "white" out aes()
    
    aux = ggplot_build(g)$data[[1]]$count
    # g + stat_bin(closed = "left", geom = 'text', # color = "black",
    #              aes(label = paste(format(round(100*(..count..)/sum(..count..),1), nsmall = 1), "%", sep = "")),
    #              position = "stack", vjust = -0.4, size = 4) +
      g + labs(title = title, x = xname, y = yname) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold.italic"),
            axis.title = element_text(size = 12, face = "italic"),
            axis.text = element_text(size = 12),
            legend.position = "none") +
      scale_x_continuous(limits = domain) +
      # scale_y_continuous(limits = c(0, 1.2*max(aux))) +
      scale_fill_continuous(type = "gradient") # + low = "blue", high = "darkblue"
      # annotate("text", label = paste("n = ", sum(aux), collapse = ""),
      #          x = domain[1], y = max(aux), size = 4, hjust = 0)
    # ggplotly(g)
  }
  
  metricas = function(reales, predichos){
    TP = sum(reales == 1 & predichos == 1)
    FN = sum(reales == 1 & predichos == 0)
    FP = sum(reales == 0 & predichos == 1)
    TN = sum(reales == 0 & predichos == 0)
    aux = c(
      "Recall" = TP/(TP + FN),
      "Precision" = TP/(TP + FP),
      "Accuracy" = (TP + TN)/(TP + TN + FP + FN),
      "F1 Score" = 2*TP/(2*TP+FP+FN)
    )
    return(aux)
  }
  
  model.roc = function(realestrain, probabilidadestrain, realestest, probabilidadestest){
    
    AUC = c()
    
    aux.calculus = function(reales, probabilidades){
      levels(reales) = c(FALSE, TRUE)
      reales = as.logical(reales)
      reales = reales[order(probabilidades, decreasing = TRUE)]
      TPR = cumsum(reales)/sum(reales)
      FPR = cumsum(!reales)/sum(!reales)
      n = length(TPR)
      AUC <<- c(AUC, format(round(sum((FPR[2:n] - FPR[1:(n - 1)])*TPR[2:n])*100, 2), nsmall = 2))
      return(data.frame("TPR" = TPR, "FPR" = FPR))
    }
    
    aux = rbind(aux.calculus(realestrain, probabilidadestrain),
                aux.calculus(realestest, probabilidadestest))
    aux$Conjunto = c(rep("Entrenamiento", length(realestrain)),
                     rep("Prueba", length(realestest)))
    
    g = ggplot(data = aux) +
      geom_line(aes(x = FPR, y = TPR, colour = Conjunto)) + 
      # geom_line(aes(x = c(0,1), y = c(0,1)), linetype = "dashed", colour = "red", size = 0.3) +
      geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", size = 0.1) +
      theme_minimal() +
      labs(title = "Curva de ROC") +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = c(1, 0),
            legend.justification = c("right", "bottom"),
            legend.title = element_blank()) +
      scale_x_continuous(limits = c(0,1)) +
      scale_y_continuous(limits = c(0,1)) +
      scale_color_manual(labels = c(paste0("Entrenamiento: AUC = ", AUC[1], "%", collapse = ""),
                                    paste0("Prueba: AUC = ", AUC[2], "%", collapse = "")),
                         values = c("darkred","darkblue"))
      # annotate("text", label = paste("AUC = ", AUC, "%", collapse = ""),
      #          x = 1, y = 0, size = 4, hjust = 1)
    # g = ggplotly(g)
    return(g)
  }
  
  model.pr = function(realestrain, probabilidadestrain, realestest, probabilidadestest){
    
    aux.calculus = function(reales, probabilidades, conjunto){
      cuts = seq(0, 1, 0.01)
      Precision = Recall = c()
      for (i in 1:length(cuts)) {
        predichos = ifelse(probabilidades <= cuts[i], 0, 1)
        TP = sum(reales == 1 & predichos == 1)
        FN = sum(reales == 1 & predichos == 0)
        FP = sum(reales == 0 & predichos == 1)
        Precision[i] = TP/(TP + FP)
        Recall[i] = TP/(TP + FN)
      }
      return(data.frame("Precision" = Precision, "Recall" = Recall,
                        "Conjunto" = conjunto))
    }
    
    aux = rbind(aux.calculus(realestrain, probabilidadestrain, "Entrenamiento"),
                aux.calculus(realestest, probabilidadestest, "Prueba"))

    g = ggplot(data = aux, mapping = aes(x = Recall, y = Precision, colour = Conjunto)) +
      geom_line() + 
      theme_minimal() +
      labs(title = "Curva PR") +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = c(1, 1),
            legend.justification = c("right", "top"),
            legend.title = element_blank()) +
      scale_x_continuous(limits = c(0,1)) +
      scale_y_continuous(limits = c(0,1)) +
      scale_color_manual(values = c("darkred","darkblue"))
    # g = ggplotly(g)
    return(g)
  }
  
  model.ks = function(realestrain, probabilidadestrain, realestest, probabilidadestest){
    
    ks = corte = c()
    
    aux.calculus = function(reales, probabilidades, conjunto){
      reales = reales[order(probabilidades, decreasing = TRUE)]
      acum.unos = cumsum(as.numeric(reales) - 1)
      acum.ceros = (seq(1,length.out = length(acum.unos)) - acum.unos)
      acum.unos = acum.unos/max(acum.unos)
      acum.ceros = acum.ceros/max(acum.ceros)
      ks <<- c(ks, format(round(max(acum.unos - acum.ceros)*100,2), nsmall = 1))
      corte <<- c(corte, which.max(acum.unos - acum.ceros))
      ind = seq(1, length.out = length(acum.ceros))
      ind = ind/length(ind)
      return(data.frame("Reprobados" = acum.unos, "Aprobados" = acum.ceros,
                        "index" = ind, "Conjunto" = conjunto))
    }
    
    aux = rbind(aux.calculus(realestrain, probabilidadestrain, "Entrenamiento"),
                aux.calculus(realestest, probabilidadestest, "Prueba"))
    
    g = ggplot() +
      geom_line(data = aux, aes(x = index, y = Reprobados, colour = interaction(Conjunto))) +
      geom_line(data = aux, aes(x = index, y = Aprobados, colour = interaction(Conjunto))) +
      # geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), size = 0.1) +
      theme_minimal() +
      labs(title = "Gráfico KS", y = "Chance acumulada", x = "Tamaño de la muestra") +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = c(1, 0),
            legend.justification = c("right", "bottom"),
            legend.title = element_blank()) +
      scale_color_manual(labels = c(paste0("Entrenamiento: KS = ", ks[1], "%", collapse = ""),
                                    paste0("Prueba: KS = ", ks[2], "%", collapse = "")),
                         values = c("darkred","darkblue"))
    # g = ggplotly(g)
    return(g)
  }
  
  scatter.model = function(data, color, title){
    if(length(color) != 0){
      data = data.frame("v1" = data, "v2" = seq(1:length(data)),
                        "v3" = ifelse(seq(1:length(data)) %in% color, "red", "black"))
    } else {
      data = data.frame("v1" = data, "v2" = seq(1:length(data)),
                        "v3" = "black")
    }
    
    ggplot(data = data, mapping = aes(x = v2, y = v1)) +
      geom_point(colour = data$v3, size = 1.5) +
      theme_minimal() +
      labs(title = title, x = "", y = "") + 
      theme(legend.position = "none",
            axis.title = element_text(size = 11),
            axis.text = element_text(size = 10),
            plot.title = element_text(hjust = 0.5, face = "bold"))
  }
  
  violin = function(data, v, titulo, eje.y, lim = c(0,1)){
    ggplot(data, aes(x = 1 , y = .data[[v]])) + 
      geom_violin(trim = T, alpha = 0.5, fill = "aquamarine3") +
      labs(title = titulo,
           x = "", y = eje.y) +
      theme_minimal() +  geom_boxplot(width = 0.1, color = "black", alpha = 0.3, fill = "darkblue")  +
      stat_summary(fun = mean, geom = "point", size = 2, color = "black") +
      scale_y_continuous(limits = lim) +
      theme(plot.title = element_text(hjust = 0.5, color = "black", size = 13, face = "bold.italic"),
            axis.title = element_text(size = 12, color = "black", face = "italic"),
            axis.text.x = element_blank(),
            legend.position = "none",
            axis.ticks.x = element_blank())
  }
  
  scatter.predict = function(data, v1, v2, breaks, limits, title){
    ggplot(data = data,
           mapping = aes(y = .data[[v1]], x = .data[[v2]])) +
      geom_point(colour = "darkblue", size = 1.5, alpha = 0.5) +
      geom_smooth(method = "lm", color = "red", se = FALSE)  +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, color = "black", size = 13, face = "bold.italic"),
            legend.position = "none",
            axis.title = element_text(size = 12, face = "italic")) +
      labs(title = title) +
      scale_y_continuous(name = v1, breaks = seq(0,1,0.1), labels = seq(0,1,0.1), limits = c(0,1)) +
      scale_x_continuous(name = v2, breaks = breaks, labels = breaks, limits = limits) +
      annotate("text", label = paste("Correlación: ",
                                     round(cor(data[[v1]],data[[v2]], use = "na.or.complete"),3),
                                     collapse = ""),
               x = limits[1], y = 0, size = 4, colour = "black", hjust = 0)
  }
  
  niveles = function(Probabilidad){
    riesgo = ifelse(Probabilidad < 0.3, "Bajo",
                    ifelse(Probabilidad < 0.6, "Medio", "Alto"))
    riesgo = factor(riesgo, levels = c("Bajo", "Medio", "Alto"))
    riesgo = prop.table(table(riesgo))
    
    ggplot(data = as.data.frame(riesgo), mapping = aes(x = riesgo, y = Freq, fill = riesgo)) +
      geom_bar(stat = "identity", position = "dodge") + 
      theme_minimal() +
      theme(axis.title.y = element_text(face = "italic"),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_text(face = "italic"),
            axis.text.x = element_text(face = "bold", color = "black", size = 11),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold.italic")) +
      labs(x = "Nivel de riesgo académico",
           y = "",
           title = "Distribución de niveles de riesgo") +
      scale_y_continuous(limits = c(0,1.2)) +
      scale_fill_manual(name = "", values = c("darkgreen", "darkorange", "red")) +
      geom_text(mapping = aes(label = paste(format(round(Freq*100,1), nsmall = 1), "%", sep = "")),
                vjust = -0.5, position = position_dodge(0.9), size = 4)
  }
  
  model = eventReactive(input$cambios_modelo,{
    
    if(input$p2t1_4 == TRUE){
      semilla = 123
      set.seed(123)
    } else {
      semilla = NA
    }
    
    # Imputación
    if(input$na_omit == FALSE){
      df.risk = mice::mice(risk, m = input$n_imputaciones_multiples, seed = semilla,
                maxit = input$n_iteraciones,
                print = F, method = switch(input$tecnica_imputacion,
                                           "Media muestral" = "pmm",
                                           "Muestreo simple" = "sample",
                                           "Árboles de regresión" = "cart",
                                           "Bosque aleatorio" = "rf",
                                           "Regresión lineal bayesiana" = "norm"))
      df.risk = mice::complete(df.risk)
    } else {
      df.risk = risk
      df.risk = na.omit(df.risk)
    }

    predict2022 = mice::mice(df.predict[,2:4], m = input$n_imputaciones_multiples, seed = semilla,
                       maxit = input$n_iteraciones,
                       print = F, method = switch(input$tecnica_imputacion,
                                                  "Media muestral" = "pmm",
                                                  "Muestreo simple" = "sample",
                                                  "Árboles de regresión" = "cart",
                                                  "Bosque aleatorio" = "rf",
                                                  "Regresión lineal bayesiana" = "norm"))
    predict2022 = mice::complete(predict2022)
    
    # Estandarización de las variables
    if(input$p2t1_8 == TRUE){
      for (i in 2:4) {
        df.risk[,i] = (df.risk[,i]- mean(df.risk[,i]))/sd(df.risk[,i])
        predict2022[,i-1] = (predict2022[,i-1]- mean(predict2022[,i-1]))/sd(predict2022[,i-1])
      } 
    }
    
    # Partición 
    p = input$n_entrenamiento
    n = c(sample(which(df.risk$y == 1), p*length(df.risk$y[df.risk$y == 1])), # Muestra de 1
          sample(which(df.risk$y == 0), p*length(df.risk$y[df.risk$y == 0]))) # Muestra de 0
    train = df.risk[n,]
    test = df.risk[-n,]
    
    # Balanceo
    if(input$tecnica_balanceo == "Undersampling"){
      # train = ROSE::ovun.sample(formula = y ~ ., data = train, method = "under")$data
      train = UBL::RandUnderClassif(y~., dat = train)
    } else if(input$tecnica_balanceo == "Oversampling"){
      # train = ROSE::ovun.sample(formula = y~., data = train, method = "over")$data
      train = UBL::RandOverClassif(y~., dat = train)
    } else if(input$tecnica_balanceo == "Smote"){
      train = UBL::SmoteClassif(y~., dat = train, dist = input$n_distancia1, k = input$n_vecinos1,
                           repl = input$repetir_observaciones)
    } else if(input$tecnica_balanceo == "Adasyn"){
      train = UBL::AdasynClassif(y~., dat = train, k = input$n_vecinos2, beta = input$equilibrio,
                            dist = input$n_distancia2, dth = input$desequilibrio)
    } else {
      train = train
    }
    
    # GLM
    formula = as.formula(paste("y ~", paste(input$covariables, collapse = "+")))
    modelo = glm(formula, data = train, family = binomial(link = input$enlace_modelo))
    
    # Puntos influyentes
    cook = stats::cooks.distance(modelo)
    n1 = which(cook >= 4/(dim(train)[1]-length(modelo$coefficients)-2))
    
    deltachi = blorr::blr_plot_difchisq_fitted(modelo)
    deltachi = deltachi$plot_env$y
    n2 = which(deltachi >= 5)
    
    residuos = rstudent(modelo)
    n3 = which(abs(residuos) > 2)
    
    n_1 = intersect(intersect(n1, n2), n3)
    n_2 = unique(c(n1, n2, n3))
    
    if(input$p2t1_6 == TRUE & length(n_1) != 0){
      train = train[-c(n_1),]
      modelo = glm(formula, data = train, family = binomial(link = input$enlace_modelo))
    } else if(input$p2t1_7 == TRUE & length(n_2) != 0){
      train = train[-c(n_2),]
      modelo = glm(formula, data = train, family = binomial(link = input$enlace_modelo))
    } else {
      modelo = glm(formula, data = train, family = binomial(link = input$enlace_modelo))
    }
    
    # Métricas entrenamiento
    metricas.entrenamiento = metricas(train$y, ifelse(predict(modelo, type = "response") < 0.5 , 0, 1))

    # Métricas Prueba
    metricas.prueba = metricas(test$y, ifelse(predict(modelo, newdata = test, type = "response") < 0.5 , 0, 1))
    
    # Data frame predicción 2022
    predict2022$Probabilidad = predict(modelo, newdata = predict2022, type = "response")
    predict2022 = cbind(df.predict$RUT, predict2022)
    
    # Curvas
    roc = model.roc(train$y, predict(modelo, type = "response"),
                    test$y, predict(modelo, newdata = test, type = "response"))
    pr = model.pr(train$y, predict(modelo, type = "response"),
                  test$y, predict(modelo, newdata = test, type = "response"))
    ks = model.ks(train$y, predict(modelo, type = "response"),
                  test$y, predict(modelo, newdata = test, type = "response"))
        
    # Formato de la métricas para output
    metricas.entrenamiento = paste(format(round(metricas.entrenamiento*100, 2), nsmall = 2), "%", sep = " ")
    metricas.prueba = paste(format(round(metricas.prueba*100, 2), nsmall = 2), "%", sep = " ")
    
    # Modelo nulo para bondad y ajuste
    null.model = glm(y ~ 1, family = binomial(link = input$enlace_modelo), data = train)
    
    modelo = list("Model" = modelo,
                  "Null model" = null.model,
                  "Train_Metrics" = metricas.entrenamiento,
                  "Test_Metrics" = metricas.prueba,
                  "Cooks" = list(cook, n1),
                  "DeltaChi" = list(deltachi, n2),
                  "Residuos" = list(residuos, n3),
                  "ROC" = roc,
                  "PR" = pr,
                  "KS" = ks,
                  "Predicciones2022" = predict2022)
    
    return(modelo)
  }, ignoreNULL = FALSE)

  #### Elementos del panel 2 ####
  
  output$Hist.nem = renderPlot({
    histg2(risk, "NEM", title = "Distribución del puntaje NEM", domain = c(150,850),
           yname = "Frecuencia", xname = "Puntaje")
  }, height = 250)
  
  output$Hist.ptu = renderPlot({
    histg2(risk, "PTU", title = "Distribución del puntaje PTU", domain = c(150,850),
           yname = "Frecuencia", xname = "Puntaje")
  }, height = 250)
  
  output$Hist.logro = renderPlot({
    histg2(risk, "Logro", title = "Distribución de la proporción de logro", domain = c(0,1),
           yname = "Frecuencia", xname = "Proporción de logro")
  }, height = 250)
  
  output$Aprobados = renderText({
    sum(risk$y == 0)
  })
  
  output$Reprobados = renderText({
    sum(risk$y == 1)
  })
  
  output$Ajustes = renderPlot({ # Prueba del evento reactivo del modelo
    g1 = scatter.model(model()[["Residuos"]][[1]], model()[["Residuos"]][[2]], title = "Residuos estandarizados")
    g2 = scatter.model(model()[["Cooks"]][[1]], model()[["Cooks"]][[2]], title = "Distancias de Cook")
    g3 = scatter.model(model()[["DeltaChi"]][[1]], model()[["DeltaChi"]][[2]], title = "Valores Delta - Chi")
    grid.arrange(g1, g2, g3, nrow = 1)
  }, height = 250)
  
  output$TrainAccuracy = renderText({
    model()[["Train_Metrics"]][3]
  })
  
  output$TrainPrecision = renderText({
    model()[["Train_Metrics"]][2]
  })
  
  output$TrainRecall = renderText({
    model()[["Train_Metrics"]][1]
  })
  
  output$TrainF1 = renderText({
    model()[["Train_Metrics"]][4]
  })
  
  output$TestAccuracy = renderText({
    model()[["Test_Metrics"]][3]
  })
  
  output$TestPrecision = renderText({
    model()[["Test_Metrics"]][2]
  })
  
  output$TestRecall = renderText({
    model()[["Test_Metrics"]][1]
  })
  
  output$TestF1 = renderText({
    model()[["Test_Metrics"]][4]
  })
  
  output$SummaryModel = renderPrint({
    summary(model()[["Model"]])
  })
  
  output$BondadAjuste = renderPrint({
    lmtest::lrtest(model()[["Null model"]], model()[["Model"]])
  })
  
  output$Devianza = renderPrint({
    D = model()[["Null model"]]$null.deviance - model()[["Model"]]$deviance
    p = 1 - pchisq(D, model()[["Null model"]]$df.null - model()[["Model"]]$df.residual)
    c("Deviance" = D, "p-value" = p)
  })
  
  # output$confidence.interval = renderPrint({
  #   confint(model()[["Model"]])
  # })
  
  output$roc = renderPlot({
    model()[["ROC"]]
  }, height = 250)
  
  output$pr = renderPlot({
    model()[["PR"]]
  }, height = 250)

  output$ks = renderPlot({
    model()[["KS"]]
  }, height = 250)
  
  output$Violines = renderPlot({
    g1 = violin(filter.predict.imputed(), "NEM", "Distribución de \n los puntajes NEM", "Puntaje", lim = c(150,850))
    g2 = violin(filter.predict.imputed(), "PTU", "Distribución de \n los puntajes PTU", "Puntaje", lim = c(150,850))
    g3 = violin(filter.predict.imputed(), "Logro", "Distribución de la \n Proporción de Logro", "Proporción de logro")
    grid.arrange(g1,g2,g3,nrow = 1)
  }, height = 250)
  
  output$Correlaciones = renderPlot({
    g2 = scatter.predict(filter.predict.imputed(), "Probabilidad", "PTU", seq(150,850,100), c(150,850),
                         "Puntaje NEM vs \n probabilidad de riesgo")
    g1 = scatter.predict(filter.predict.imputed(), "Probabilidad", "NEM", seq(150,850,100), c(150,850),
                         "Puntaje PTU vs \n probabilidad de riesgo")
    g3 = scatter.predict(filter.predict.imputed(), "Probabilidad", "Logro", seq(0,1,0.1), c(0,1),
                         "Proporción de logro vs \n probabilidad de riesgo")
    grid.arrange(g1,g2,g3, nrow = 1)
  }, height = 250)
  
  output$Niveles = renderPlot({
    niveles(filter.predict.imputed()$Probabilidad)
  })
  
  output$Corrs = renderPlot({
    ggcorrplot::ggcorrplot(round(cor(risk[,2:4], use = "na.or.complete"),2), type = "lower",
               lab = TRUE, show.legend = FALSE, title = "Correlaciones entre \n las covariables") + 
      theme(plot.title = element_text(face = "bold.italic", hjust = 0.5, size = 14))
  }, height = 250)
  
  output$Summary2021 = function(){
    t = round(psych::describe(risk[,-1])[,-c(1,13)], 2)
    kbl(t, format = "html", align = "c",
          col.names = c("Total", "Media", "Desviación estándar", "Mediana",
                        "Media recortada", "Desviación absoluta mediana",
                        "Mínimo", "Máximo", " Rango", "Skewness", "Kurtosis")) |>
      kable_styling(position = "center", bootstrap_options = c("striped")) |>
      column_spec(column = 1:12, width = "1.2cm")
  }
  
  output$Summary2022 = function(){
    t = round(psych::describe(filter.predict.imputed()[,-c(1,5)])[,-c(1,13)], 2)
    kbl(t, format = "html", align = "c",
        col.names = c("Total", "Media", "Desviación estándar", "Mediana",
                      "Media recortada", "Desviación absoluta mediana",
                      "Mínimo", "Máximo", " Rango", "Skewness", "Kurtosis")) |>
      kable_styling(position = "center", bootstrap_options = c("striped"))
  }
  
}














