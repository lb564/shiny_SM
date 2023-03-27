### PARTE GRAFICA

##Tema di ggplot2
theme_pitch <- function(...) {
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.line = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5))
}

## Inizializzazione del campo di gioco
campo_vuoto <- function(panchina = TRUE) {
  campo <- ggplot() + 
    geom_rect(aes(xmin = -1, xmax = 37, ymin = -1, ymax = 25), fill='green', color="white", alpha=0.5) +
    geom_rect(aes(xmin = 0, xmax = 36, ymin = 0, ymax = 24), fill='green', color="white", size = 2, alpha=0.5) +
    geom_rect(aes(xmin = 9.936, xmax = 26.064, ymin = 0, ymax = 6.6), fill='green', color="white", size = 2, alpha=0.5) +
    geom_rect(aes(xmin = 14.336, xmax = 21.664, ymin = 0, ymax = 2.2), fill='green', color="white", size = 2, alpha=0.5) +
    geom_arc(aes(x0 = 18, y0 = 24, r = 3.66, start = pi/2, end = 3*pi/2), color = "white", size = 2) +
    geom_arc(aes(x0 = 18, y0 = 4.4, r = 3.66, start = pi/3.4, end = -pi/3.4), color = "white", size = 2) +
    geom_arc(aes(x0 = 0, y0 = 0, r = 0.4, start = pi/2, end = 0), color = "white", size = 2) +
    geom_arc(aes(x0 = 36, y0 = 0, r = 0.4, start = -pi/2, end = 0), color = "white", size = 2) +
    geom_point(aes(18, 4.4), color = "white", size = 2)
  if(isTRUE(panchina)) {
    campo <- campo +
      geom_rect(aes(xmin = 40, xmax = 55, ymin = -1, ymax = 25), fill = "green", color = "white", alpha = 0.5) +
      geom_rect(aes(xmin = 41, xmax = 54, ymin = 0, ymax = 24), fill = 'green', color = "white", size = 2, alpha = 0.5)
  }
  campo <- campo +
    coord_fixed() +
    theme_pitch()
}

## Dataset per i punti in campo e in panchina
plot_points <- function(modulo, b, panchina = TRUE) {
  cols <- c('blue', 'red', 'yellow')
  x_cord <- 18; y_cord <- 2.5; colore <- "blue"
  for (k in 1:3) {
    x <- as.numeric(strsplit(modulo, "-")[[1]][k])
    dist_x <- seq(abs(x-5)+18/(x+1), 36-(abs(x-5)+18/(x+1)), length.out = x)
    if(x == 1) { dist_x <- 18 }
    dist_y <- 24/4 + (k-1)/2
    for (j in 1:x) {
      x_cord <- c(x_cord, dist_x[j])
      y_cord <- c(y_cord, k*dist_y+0.5)
      colore <- c(colore, cols[k])
    }
  }
  if(isTRUE(panchina)) {
    for (i in 1:b) {
      x_cord <- c(x_cord, 43)
      y_cord <- c(y_cord, 24 - i*2)
      colore <- c(colore, "gray")
    }
  }
  
  return(data.frame(x_cord = x_cord, y_cord = y_cord, colore = colore, stringsAsFactors = FALSE))
}

## Sistemazione giocatori in campo e in panchina
formazioni_campo <- function(modulo, b, panchina = TRUE) {
  campo <- campo_vuoto(panchina = panchina) + geom_point(aes_(18, 2.5), pch = 21, size = 7, fill = 'blue', colour = "black")
  for (k in 1:3) {
    campo <- giocatore_campo(campo = campo, k = k, modulo = modulo)
  }
  if(isTRUE(panchina)) {
    for (i in 1:b) {
      campo <- campo +
        geom_point(aes_(43, 24 - i*2), pch = 21, size = 7, fill = "gray", colour = "black")
    }
  }
  
  return(campo)
}

giocatore_campo <- function(k, campo, modulo) {
  cols <- c('blue', 'red', 'yellow')
  x <- as.numeric(strsplit(modulo, "-")[[1]][k])
  dist_x <- seq(abs(x-5)+18/(x+1), 36-(abs(x-5)+18/(x+1)), length.out = x)
  if(x == 1) { dist_x <- 18 }
  dist_y <- 24/4 + (k-1)/2
  for (j in 1:x) {
    campo <- campo + 
      geom_point(aes_(dist_x[j], k*dist_y+0.5), pch = 21, size = 7, fill = cols[k], colour = "black")
  }
  return(campo)
}

get_role_from_cord <- function(formazione) {
  x <- formazione %>% 
    filter(colore == "gray", Nome != "") %>% 
    select(Nome) %>% 
    mutate(Ruolo = rep("R", nrow(filter(formazione, colore == "gray", Nome != ""))))
  
  y <- formazione %>% 
    filter(colore != "gray") %>% 
    select(-colore) %>%
    mutate(y_cord = recode(y_cord, '2.5' = "P", '6.5' = "D", '13.5' = "C", '21.5' = "A")) %>% 
    mutate(x_cord = round(x_cord, digits = 2)) %>% 
    mutate(x_cord = recode(x_cord, '18' = "C", '13.53' = "C", '22.47' = "C", '6.50' = "C", '29.50' = "C", '10.50' = "C", '25.50' = "C",
                           '3' = "S", '33' = "D", '4.60' = "S", '31.40' = "D", '9' = "A", '27' = "A", .default = "ATTENZIONE")) %>% 
    mutate(Ruolo = paste(y_cord, x_cord, sep = "")) %>% 
    select(-x_cord, -y_cord) %>% 
    mutate(Ruolo = recode(Ruolo, 'PC' = 'P', 'AA' = "A", 'AC' = "A", .default = levels(Ruolo))) %>% 
    filter(Nome != "")
  
  formazione <- rbind(y, x)
  formazione <- data.frame(Nome = apply(formazione, 1, function(x) strsplit(x[1], "\\(")[[1]][1]), Ruolo = formazione$Ruolo,
                           stringsAsFactors = FALSE)
  return(formazione)
}



#### Costruisco la tabella con i valori dei calciatori
get_team <- function(codice) {
  pagina <- paste("https://www.scudettomondiale.it/rosasquadra", codice, ".html", sep = "")
  x <- readLines(pagina)
  x <- x[grep("calciatore", x)]
  x <- strsplit(x, "</TR>")[[1]]
  x <- unlist(lapply(x[-length(x)], get_player_info))
  # x <- x[lapply(x, nchar) > 0]
  team <- data.frame(matrix(x, ncol = 11, byrow = TRUE), stringsAsFactors = FALSE)
  colnames(team) <- c("Nome", "Forma", "P", "DD", "DC", "DS", "CD", "CC", "CS", "A", "Note")
  return(team)
}

# Estraggo le informazioni del singolo calciatore
get_player_info <- function(riga) {
  x1 <- strsplit(riga, "</TD>")[[1]]
  x1 <- unlist(lapply(x1, function(x) gsub("<[^>]+>", "", x, perl = TRUE)))[-(2:6)]
  x1 <- gsub("[^0-9A-Z ]", "", x1)
  
  codice <- str_extract_all(riga, "calciatore(\\d+).html")[[1]]
  pagina <- paste("https://www.scudettomondiale.it/", codice, sep = "")
  x2 <- readLines(pagina)
  x3 <- ""
  if(length(grep("ammonito.gif", x2)) > 0) {x3 <- paste(x3, "Ammonizione", sep = "")}
  if(length(grep("ammonito2.gif", x2)) > 0) {x3 <- paste(x3, "Doppia Ammonizione", sep = "")}
  if(length(grep("squalificato.gif", x2)) > 0) {x3 <- paste(x3, "Espulsione", sep = "")}
  if(length(grep("infortunato.gif", x2)) > 0) {x3 <- paste(x3, "Infortunio", sep = "")}
  x2 <- unlist(lapply(x2, function(x) gsub("<[^>]+>", "", x, perl = TRUE)))
  x2 <- unlist(lapply(x2, function(x) gsub("\t", "", x, perl = TRUE)))
  x2 <- x2[grepl("[[:punct:]]", x2) + grepl("[a-zA-Z]", x2) + grepl(" ", x2) == 0]
  x2 <- x2[lapply(x2, nchar) > 0]
  x2 <- x2[lapply(x2, nchar) < 4]
  
  player <- c(x1, x2, x3)
  return(player)
}






#### Data il modulo e la rosa con le indicazioni di titolari e riserve, determino la formazione ideale
get_best_formation <- function(modulo, formazione) {
  modulo_backup <- as.numeric(strsplit(modulo, "-")[[1]])
  modulo <- get_form_df(modulo = modulo)
  
  # Pre-Allocazione: inserisco in formazione tutti i titolari e rimuovo tutti i panchinari
  for (i in  which(formazione$Ruolo == "T")) {
    formazione[i, "Ruolo"] <- colnames(formazione)[which.max(formazione[i, 3:10])+2]
  }
  for (i in which(formazione$Ruolo %in% c("P", "DS", "DC", "DD", "CS", "CC", "CD", "A"))) {
    ind <- intersect(which(colnames(modulo) == formazione$Ruolo[i]), which(modulo[1, ] == ""))[1]
    modulo[1, ind] <- formazione$Nome[i]
    modulo[2, ind] <- formazione[i, eval(formazione$Ruolo[i])]
    modulo[3, ind] <- formazione$Forma[i]
  }
  formazione <- formazione[formazione$Ruolo == "", ]
  
  # Allocazione: per i posti rimanenti scelgo i giocatori migliori tra quelli rimasti
  for (i in which(modulo[1, ] == "")) {
    ind <- which.max(formazione[, colnames(modulo)[i]])[1]
    modulo[1, i] <- formazione[ind, 1]
    modulo[2, i] <- formazione[ind, eval(colnames(modulo)[i])]
    modulo[3, i] <- formazione[ind, 2]
    formazione <- formazione[-ind, ]
  }
  
  # Calcolo dei punteggi
  d <- (sum(as.numeric(modulo[2, 2:(modulo_backup[1]+1)])) + 3*as.numeric(modulo[2, 1])/2)/(10*(modulo_backup[1]+1))
  c <- (sum(as.numeric(modulo[2, (modulo_backup[1]+2):(modulo_backup[1]+modulo_backup[2]+1)])))/(10*modulo_backup[2])
  a <- (sum(as.numeric(modulo[2, (modulo_backup[1]+modulo_backup[2]+2):(modulo_backup[1]+modulo_backup[2]+modulo_backup[3]+1)])))/
    (10*modulo_backup[3])
  
  d <- appross(d + 0.2*d*(modulo_backup[1]-4))
  c <- appross(c + 0.2*c*(modulo_backup[2]-4))
  a <- appross(a + 0.2*a*(modulo_backup[3]-2))
  
  return(list(modulo = modulo, dif = d, cen = c, att = a))
}

# Funzione di approssimazione
appross <- function(x) {
  if(x > 0) {
    if(x %% 1 >= 0.5) {
      x <- ceiling(x)
    } else {
      x <- floor(x)
    }
  } else {
    if(x %% 1 >= 0.5) {
      x <- floor(x)
    } else {
      x <- ceiling(x)
    }
  }
  return(x)
}

# Dal modulo al data.frame
get_form_df <- function(modulo) {
  modulo <- as.numeric(strsplit(modulo, "-")[[1]])
  x1 <- c("P", rep("D", modulo[1]), rep("C", modulo[2]), rep("A", modulo[3]))
  x2 <- c("", 
          rep("S", ceiling(modulo[1]/3)-1), rep("C", modulo[1] - 2*(ceiling(modulo[1]/3)-1)), rep("D", ceiling(modulo[1]/3)-1),
          rep("S", ceiling(modulo[2]/3)-1), rep("C", modulo[2] - 2*(ceiling(modulo[2]/3)-1)), rep("D", ceiling(modulo[2]/3)-1),
          rep("", modulo[3]))
  x <- paste0(x1, x2)
  df <- data.frame(matrix(rep("", 33), nrow = 3), stringsAsFactors = FALSE)
  colnames(df) <- x
  return(df)
}

# Creo il data.frame unendo rosa e informazioni su titolari e panchinari
get_formation_from_tit_ris <- function(rosa, tit_ris) {
  ind <- rosa$Note %in% c("Espulsione", "Infortunio")
  rosa <- rosa[!ind, ]
  rosa$Ruolo <- rep("", nrow(rosa))
  if(!is.null(tit_ris)) {
    for (i in 1:nrow(tit_ris)) {
      rosa[rosa$Nome == trimws(tit_ris$Nome[i]), "Ruolo"] <- tit_ris$Ruolo[i]
    }
  }
  rosa <- rosa %>%
    mutate(P = as.numeric(P)*as.numeric(Forma)/100, DS = as.numeric(DS)*as.numeric(Forma)/100, DC = as.numeric(DC)*as.numeric(Forma)/100,
           DD = as.numeric(DD)*as.numeric(Forma)/100, CS = as.numeric(CS)*as.numeric(Forma)/100, CC = as.numeric(CC)*as.numeric(Forma)/100,
           CD = as.numeric(CD)*as.numeric(Forma)/100, A = as.numeric(A)*as.numeric(Forma)/100) %>%
    mutate(Ruolo = recode(Ruolo, "2" = "T", "1" = "R", .default = levels(Ruolo))) %>%
    select(-"Note")
  return(rosa)
}



#### Date le due formazioni ideali (fissato il modulo), calcolo il risultato della partita
get_match_results <- function(d_io, c_io, a_io, d_av, c_av, a_av, att_io, att_av, casa) {
  x <- appross(((d_io + c_io + a_io) - (d_av + c_av + a_av))/2)
  ifelse(x > 0, c_io <- c_io + abs(x), c_av <- c_av + abs(x))
  ifelse(casa == 1, d_io <- d_io + 3, d_av <- d_av + 3)
  y <- c_io - c_av
  ifelse(y > 0, ifelse(att_io == "OFF", a_io <- a_io + abs(y), d_io <- d_io + abs(y)), 
                ifelse(att_av == "OFF", a_av <- a_av + abs(y), d_av <- d_av + abs(y)))
  gf <- a_io - d_av
  gs <- a_av - d_io
  conv_vect <- c(1, 5, 13, 25, 41, 61, 85, 113)
  gf <- max(which(sort(c(gf, conv_vect)) == gf)) - 1
  gs <- max(which(sort(c(gs, conv_vect)) == gs)) - 1
  return(list(gf, gs))
}






