#### Stagione
get_season <- function() {
  pagina <- "https://www.scudettomondiale.it/partite2.php?squadra=960"
  stagione <- readLines(pagina)
  stagione <- stagione[grep("option value", stagione)]
  stagione <- gsub("<[^>]+>", " ", stagione, perl = TRUE)
  stagione <- gsub("[^0-9]", " ", stagione, perl = TRUE)
  stagione <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", stagione, perl = TRUE)
  stagione <- max(as.numeric(strsplit(stagione, " ", fixed = TRUE)[[1]]))
  saveRDS(stagione, file = "stagione.rds")
}

#### Squadre e relativi codici
get_code <- function(squadra) {
  if (!file.exists("squadre.rds")) {
    get_my_calendar()
  }
  squadre <- readRDS(file = "squadre.rds")
  
  codice <- unname(squadre$codice[squadre$nome == squadra])
  codice <- gsub("[^0-9]", "", codice)
  return(codice)
}

#### Formazioni Usate
get_result <- function(codice) {
  pagina <- paste("https://www.scudettomondiale.it/", codice, sep = "/")
  risultato <- readLines(pagina)
  risultato <- risultato[sort(c(grep("MODULO", risultato), grep("MODULO", risultato) + 2))]
  risultato <- lapply(risultato, function(x) gsub("<[^>]+>", " ", x, perl = TRUE))
  risultato <- lapply(risultato, function(x) gsub("\t", "", x, perl = TRUE))
  risultato <- sapply(risultato, paste, collapse = " ")
  risultato <- c(paste(risultato[1], risultato[2]), paste(risultato[3], risultato[4]))
  risultato <- lapply(risultato, function(x) gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", x, perl = TRUE))
  risultato <- sapply(risultato, paste, collapse = " ")
  risultato <- unname(sapply(risultato, function(x) paste(substr(x, 8, nchar(x)-6), ".", sep = "")))
  return(risultato)
}

# Dalla pagina del Calendario estrare un blocco per ciascuna partita
page_to_chunks <- function(page) {
  x <- readLines(page)
  x <- x[grep("dettaglio_squadra", x)]
  x <- strsplit(x, "<tr>")
  x <- lapply(x, function(x) gsub("\t", "", x, perl = TRUE))[[1]]
  x <- x[lapply(x, nchar) > 0]
  x <- x[is.na(str_extract(x, "partitacoppa.php\\?id=(\\d+)"))]
  return(x)
}

# Dal blocco alle informazioni
chunk_to_infos <- function(chunk, lista = FALSE) {
  if (isTRUE(lista)) {
    x3 <- str_extract_all(chunk, "dettaglio_squadra.php\\?squadra=(\\d+)")[[1]]
  }
  x2 <- str_extract(chunk, "partita[a-z]*.php\\?id=(\\d+)")
  x1 <- gsub("<[^>]+>", "*", chunk, perl = TRUE)
  x1 <- gsub("-", "", x1, perl = TRUE)
  x1 <- gsub("^[*]+|[*]+$", "", x1)
  x1 <- gsub("([*])\\1+", "\\1", x1, perl = TRUE)
  x1 <- strsplit(x1, "*", fixed = TRUE)[[1]]
  if (nchar(x1[2]) <= 2) {
    if (!is.na(x2)) {
      x2 <- get_result(codice = x2)
    } else {
      x2 <- c("", "")
    }
    x <- t(data.frame(c(c(x1, "", "")[1:6], x2)))
    if (isTRUE(lista)) {
      y <- c(x1[3:4], x3)
      return(list(x, y))
    }
      else {
        return(x)
      }
  }
}

#### Mio Calendario: Creazione
get_my_calendar <- function() {
  if (!file.exists("stagione.rds")) {
    get_season()
  }
  stagione <- readRDS(file = "stagione.rds")
  
  pagina <- paste("https://www.scudettomondiale.it/partite2.php?squadra=960&stagione=", stagione, sep = "")
  chunks <- page_to_chunks(page = pagina)
  infos <- lapply(chunks, chunk_to_infos, TRUE)
  infos <- infos[lengths(infos) != 0]
  
  calendario <- data.frame(matrix(unlist(sapply(infos, function(x) x[1])), ncol = 8, byrow = T), stringsAsFactors = FALSE)
  colnames(calendario) <- c("Data", "Giornata", "Squadra1", "Squadra2", "Risultato1", "Risultato2", "Modulo1", "Modulo2")
  saveRDS(calendario, file = "960.rds")
  
  x <- prova <- unlist(sapply(infos, function(x) x[2]))
  nomi <- unique(x[as.integer(sapply(seq(1, length(x), by = 4), seq, length.out = 2))])
  codici <- unique(x[setdiff(1:length(x), as.integer(sapply(seq(1, length(x), by = 4), seq, length.out = 2)))])
  squadre <- data.frame(nome = nomi, codice = codici)
  saveRDS(squadre, file = "squadre.rds")
}

#### Mio Calendario: Aggiornamento (prima di ogni partita)
update_my_calendar <- function() {
  if (!file.exists("960.rds") || !file.exists("stagione.rds")) {
    get_my_calendar()
  }
  stagione <- readRDS(file = "stagione.rds")
  calendario <- readRDS(file = "960.rds")
  aggiunta <- which(calendario$Risultato1 == "")[1]
  
  pagina <- paste("https://www.scudettomondiale.it/partite2.php?squadra=960&stagione=", stagione, sep = "")
  chunks <- page_to_chunks(page = pagina)
  infos <- lapply(chunks[aggiunta:length(chunks)], chunk_to_infos)
  infos <- infos[lengths(infos) != 0]
  infos <- data.frame(matrix(unlist(infos), ncol = 8, byrow = T), stringsAsFactors = FALSE)
  calendario[aggiunta:nrow(calendario), ] <- infos
  
  saveRDS(calendario, file = "960.rds")
  return(calendario)
}

#### Mie Gare Da Giocare
get_my_matches <- function() {
  my_matches <- readRDS(file = "960.rds")
  my_matches <- my_matches[my_matches$Risultato1 == "", 3:4]
  my_matches <- unname(apply( my_matches[ , 1:2], 1, paste, collapse = " - " ))
  return(my_matches)
}

#### Calendario Altro
get_his_calendar <- function(codice, nome) {
  if (!file.exists("stagione.rds")) {
    get_season()
  }
  stagione <- readRDS(file = "stagione.rds")

  pagina <- paste("https://www.scudettomondiale.it/partite2.php?squadra=", codice, "&stagione=", stagione, sep = "")
  chunks <- page_to_chunks(page = pagina)
  infos <- lapply(chunks, chunk_to_infos)
  infos <- infos[lengths(infos) != 0]
  calendario <- data.frame(matrix(unlist(infos), ncol = 8, byrow = T), stringsAsFactors = FALSE)
  colnames(calendario) <- c("Data", "Giornata", "Squadra1", "Squadra2", "Risultato1", "Risultato2", "Modulo1", "Modulo2")
  
  saveRDS(calendario, file = paste(nome, ".rds", sep = ""))
  return(calendario)
}

# Vinto - Pareggiato - Perso
result <- function(x) {
  if (x > 0) x <- "V"
  if (x == 0) x <- "N"
  if (x < 0) x <- "P"
  return(x)
}

# Elenco partite dato il risultato
elenco <- function(riga) {
  if (riga[7] == "Casa") {
    x <- paste(riga[6], ": ", riga[1], " - ", riga[2], " (", riga[3], " - ", riga[4], ")", sep = "")
    # x <- paste("Contro ", riga[6], ": ", riga[1], " - ", riga[2], " (", riga[3], " - ", riga[4], ")", sep = "")
  } else {
    x <- paste(riga[6], ": ", riga[2], " - ", riga[1], " (", riga[4], " - ", riga[3], ")", sep = "")
    # x <- paste("Contro ", riga[6], ": ", riga[2], " - ", riga[1], " (", riga[4], " - ", riga[3], ")", sep = "")
  }
}

#### Prospetto Riassuntivo
get_summary <- function(nome) {8
  moduli <- c("4-4-2 OFF.", "4-3-3 OFF.", "4-5-1 OFF.", "3-5-2 OFF.", "3-4-3 OFF.", "5-4-1 OFF.", "5-3-2 OFF.",
              "4-4-2 DIF.", "4-3-3 DIF.", "4-5-1 DIF.", "3-5-2 DIF.", "3-4-3 DIF.", "5-4-1 DIF.", "5-3-2 DIF.")
  calendario <- readRDS(file = paste(nome, ".rds", sep = ""))
  subject <- names(sort(table(calendario$Squadra1), decreasing = TRUE)[1])
  calendario <- calendario[calendario$Modulo1 != "", -(1:2)]
  x1 <- calendario[calendario$Squadra1 == subject, ]
  x1$casa_trasferta <- rep("Casa", nrow(x1))
  x2 <- calendario[calendario$Squadra2 == subject, ][, c(2, 1, 4, 3, 6, 5)]
  colnames(x2) <- c("Squadra1", "Squadra2", "Risultato1", "Risultato2", "Modulo1", "Modulo2")
  x2$casa_trasferta <- rep("Trasferta", nrow(x2))
  calendario <- rbind(x1, x2)
  calendario$vnp <- unname(apply(calendario, 1, function(x) as.numeric(x[3]) - as.numeric(x[4])))
  calendario$vnp <- lapply(calendario$vnp, result)
  
  specchietto_2 <- vector("list", 3*length(moduli))
  names(specchietto_2) <- c(paste(moduli, "V", sep = " "), paste(moduli, "N", sep = " "), paste(moduli, "P", sep = " "))
  
  specchietto_1 <- data.frame(matrix(rep(0, 56), ncol = 4), stringsAsFactors = FALSE)
  colnames(specchietto_1) <- c("V", "N", "P", "Tot")
  rownames(specchietto_1) <- moduli

  for (i in 1:nrow(calendario)) {
    specchietto_1[calendario[i, 5], calendario[i, 8][[1]]] <- specchietto_1[calendario[i, 5], calendario[i, 8][[1]]] + 1
    indice <- paste(calendario[i, 5], calendario[i, 8][[1]], sep = " ")
    add <- elenco(riga = calendario[i, ])
    specchietto_2[indice][[1]] <- c(specchietto_2[indice][[1]], add)
  }
  specchietto_1 <- specchietto_1 %>% mutate(Tot = V + N + P)
  specchietto_1 <- cbind(specchietto_1[1:7, ], specchietto_1[8:14, ])
  rownames(specchietto_1) <- c("4-4-2", "4-3-3", "4-5-1", "3-5-2", "3-4-3", "5-4-1", "5-3-2")
  colnames(specchietto_1) <- c("OFF_V", "OFF_N", "OFF_P", "Totale_OFF", "DIF_V", "DIF_N", "DIF_P", "Totale_DIF")
  return(list(specchietto_1, specchietto_2))
}

#### Propetti_riassuntivo per HTML
get_specchietto <- function(nome) {
  specchietto <- get_summary(nome = nome)
  specchietto_df <- specchietto[[1]]
  specchietto_df <- specchietto_df %>% 
    mutate(Tot_V = OFF_V + DIF_V, Tot_N = OFF_N + DIF_N, Tot_P = OFF_P + DIF_P, Tot_Tot = Totale_OFF + Totale_DIF)
  specchietto_df <- rbind(specchietto_df, colSums(specchietto_df))
  specchietto_info <- specchietto[[2]]
  specchietto_info <- lapply(specchietto_info, function(x) paste0("<li>", paste(x, collapse = "</li><li>"), "</li>"))
  specchietto_info <- gsub("<li></li>", " ", specchietto_info)
  xx <- data.frame(x1 = unname(unlist(specchietto_info[1:7])), x2 = unname(unlist(specchietto_info[15:21])), x3 = unname(unlist(specchietto_info[29:35])), stringsAsFactors = FALSE)
  yy <- data.frame(y1 = unname(unlist(specchietto_info[8:14])), y2 = unname(unlist(specchietto_info[22:28])), y3 = unname(unlist(specchietto_info[36:42])), stringsAsFactors = FALSE)
  specchietto_info <- cbind(within(xx,  x4 <- paste0(x1, x2, x3)), within(yy,  y4 <- paste0(y1, y2, y3)))
  specchietto_info <- specchietto_info %>% 
    mutate(z1 = paste0(x1, y1), z2 = paste0(x2, y2), z3 = paste0(x3, y3), z4 = paste0(x4, y4))
  specchietto_info <- rbind(specchietto_info, apply(specchietto_info, 2, paste, collapse = ""))
  
  return(list(specchietto_df, specchietto_info))
}



