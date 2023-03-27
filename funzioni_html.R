df2html <- function(df, best, col_names = TRUE, align = c("l", rep("c", (ncol(df)-1)))) {
  
  align <- plyr::revalue(align, c("l" = "left", "c" = "center", "r" = "right"))
  nomi_col <- colnames(df)
  colfunc <- colorRampPalette(c("white", "blue"))
  colori_sfondo <- colfunc(12)
  colori_testo <- c(rep("#000000", 8), rep("#FFFFFF", 4))
  
  for(i in 1:nrow(df)) {
    colonna <- best[i]
    for (j in 1:ncol(df)) {
      if(j == 2) {
        if(as.numeric(df[i, j]) >=89) {
          k <- as.numeric(df[i, j])-88
          df[i, j] <- paste0("<td style = 'background-color:", colori_sfondo[k], "; color:", colori_testo[k], 
                             "; text-align: ", align[j], "'>", df[i, j], "</td>")
        } else {
          df[i, j] <- paste0("<td style = 'text-align: ", align[j], "'>", df[i, j], "</td>")
        }
      } else if(j == colonna) {
        df[i, j] <- paste0("<td style = 'background-color:#fffb00; text-align: ", align[j], "'>", df[i, j], "</td>")
      } else {
        df[i, j] <- paste0("<td style = 'text-align: ", align[j], "'>", df[i, j], "</td>")
      }
    }
  }
  df <- tidyr::unite(df, "new", 1:ncol(df), sep = " ")
  df[] <- lapply(df, function(x) paste0("<tr>", x, "</tr>"))
  df <- paste(unname(as.character(df$new)), collapse = " ")
  if(isTRUE(col_names)) {
    for(i in 1:length(nomi_col)) {
      nomi_col[i] <- paste0("<th height='20' style = 'text-align: ", align[i], "'>", nomi_col[i], "</th>")
    }
    nomi_col <- paste("<tr>", paste(nomi_col, collapse = " "), "</tr>")
    df <- paste(nomi_col, df)
  }
  df <- paste("<table id='tab'>", df, "</table>")
  
  pre <- "<head><style>table {width:80%;}table, th, td {border: 1px solid black; border-collapse: collapse;}th, td {padding: 5px; text-align: left;}</style></head><body>"
  post <- "</body>"

  df <- paste(pre, df, post)
  return(df)
}















df2html_button <- function(df, details) {
  
  nomi_col <- colnames(df)
  nomi_rig <- rownames(df)
  moduli <- c("4-4-2", "4-3-3", "4-5-1", "3-5-2", "3-4-3", "5-4-1", "5-3-2", "Totale")
  moduli_col_1 <- c(rep(" Offensivo", 4), rep(" Difensivo", 4), rep("", 4))
  moduli_col_2 <- rep(c(": Vinte", ": Pareggiate", ": Perse", ": Totale"), 3)
  
  k <- 1
  modal_vec <- character()
  script_vec <- character()
  
  for(i in 1:nrow(df)) {
    if(i != 8) {
      nomi_rig[i] <- paste0("<th style='width:8.8%; text-align:left; border-right: 2px solid black'>", moduli[i], "</th>")
      for(j in 1:ncol(df)) {
        if(j %in% c(4, 8, 12)) {
          if(df[i, j] != 0) {
            modal_vec_new <- paste0("<div id='mod", k, "' class='modal specch'><div class='modal-content specch'><p>Testo</p></div></div>")
            aggiunta <- paste0(moduli[i], moduli_col_1[j], moduli_col_2[j], details[i, j])
            modal_vec_new <- gsub("Testo", aggiunta, modal_vec_new)
            modal_vec <- c(modal_vec, modal_vec_new)
            script_vec <- c(script_vec, paste0("var modal", k, " = document.getElementById('mod", k, "'); var button", k, " = document.getElementById('btn", k, "'); button", k, ".onclick = function() {modal", k, ".style.display = 'block';}; window.addEventListener('click', function(event) {if (event.target == modal", k, "){modal", k, ".style.display = 'none';}});"))
            df[i, j] <- paste0("<td style='width:7.6%; background-color:#0294f5; border-left: 2px dashed black; border-right:2px solid black'><button id='btn", k, "' style='width:100%' class='button'>", df[i, j], "</button></td>")
          } else {
            df[i, j] <- paste0("<td style='width:7.6%; background-color:#0294f5; border-left: 2px dashed black; border-right:2px solid black'><button id='btn", k, "' style='width:100%' class='button disabled'>", df[i, j], "</button></td>")
          }
        } else {
          if(df[i, j] != 0) {
            modal_vec_new <- paste0("<div id='mod", k, "' class='modal specch'><div class='modal-content specch'><p>Testo</p></div></div>")
            aggiunta <- paste0(moduli[i], moduli_col_1[j], moduli_col_2[j], details[i, j])
            modal_vec_new <- gsub("Testo", aggiunta, modal_vec_new)
            modal_vec <- c(modal_vec, modal_vec_new)
            script_vec <- c(script_vec, paste0("var modal", k, " = document.getElementById('mod", k, "'); var button", k, " = document.getElementById('btn", k, "'); button", k, ".onclick = function() {modal", k, ".style.display = 'block';}; window.addEventListener('click', function(event) {if (event.target == modal", k, "){modal", k, ".style.display = 'none';}});"))
            df[i, j] <- paste0("<td style='width:7.6%'><button id='btn", k, "' style='width:100%' class='button'>", df[i, j], "</button></td>")
          } else {
            df[i, j] <- paste0("<td style='width:7.6%'><button id='btn", k, "' style='width:100%' class='button disabled'>", df[i, j], "</button></td>")
          }
        }
        k <- k + 1
      }
    } else {
      nomi_rig[i] <- paste0("<th style='width:8.8%; text-align:left; border-right: 2px solid black; border-top: 2px solid black'>", moduli[i], "</th>")
      for(j in 1:ncol(df)) {
        if(j %in% c(4, 8, 12)) {
          if(df[i, j] != 0) {
            modal_vec_new <- paste0("<div id='mod", k, "' class='modal specch'><div class='modal-content specch'><p>Testo</p></div></div>")
            aggiunta <- paste0(moduli[i], moduli_col_1[j], moduli_col_2[j], details[i, j])
            modal_vec_new <- gsub("Testo", aggiunta, modal_vec_new)
            modal_vec <- c(modal_vec, modal_vec_new)
            script_vec <- c(script_vec, paste0("var modal", k, " = document.getElementById('mod", k, "'); var button", k, " = document.getElementById('btn", k, "'); button", k, ".onclick = function() {modal", k, ".style.display = 'block';}; window.addEventListener('click', function(event) {if (event.target == modal", k, "){modal", k, ".style.display = 'none';}});"))
            df[i, j] <- paste0("<td style='width:7.6%; background-color:#0294f5; border-top: 2px solid black; border-left: 2px dashed black; border-right: 2px solid black'><button id='btn", k, "' style='width:100%' class='button'>", df[i, j], "</button></td>")
          } else {
            df[i, j] <- paste0("<td style='width:7.6%; background-color:#0294f5; border-top: 2px solid black; border-left: 2px dashed black; border-right: 2px solid black'><button id='btn", k, "' style='width:100%' class='button disabled'>", df[i, j], "</button></td>")
          }
        } else {
          if(df[i, j] != 0) {
            modal_vec_new <- paste0("<div id='mod", k, "' class='modal specch'><div class='modal-content specch'><p>Testo</p></div></div>")
            aggiunta <- paste0(moduli[i], moduli_col_1[j], moduli_col_2[j], details[i, j])
            modal_vec_new <- gsub("Testo", aggiunta, modal_vec_new)
            modal_vec <- c(modal_vec, modal_vec_new)
            script_vec <- c(script_vec, paste0("var modal", k, " = document.getElementById('mod", k, "'); var button", k, " = document.getElementById('btn", k, "'); button", k, ".onclick = function() {modal", k, ".style.display = 'block';}; window.addEventListener('click', function(event) {if (event.target == modal", k, "){modal", k, ".style.display = 'none';}});"))
            df[i, j] <- paste0("<td style='width:7.6%; background-color:#0294f5; border-top: 2px solid black'><button id='btn", k, "' style='width:100%' class='button'>", df[i, j], "</button></td>")
          } else {
            df[i, j] <- paste0("<td style='width:7.6%; background-color:#0294f5; border-top: 2px solid black'><button id='btn", k, "' style='width:100%' class='button disabled'>", df[i, j], "</button></td>")
          }
        }
        k <- k + 1
      }
    }
    df[i, 1] <- paste(nomi_rig[i], df[i, 1])
  }
  
  df <- tidyr::unite(df, "new", 1:ncol(df), sep = " ")
  df[] <- lapply(df, function(x) paste0("<tr>", x, "</tr>"))
  df <- paste(unname(as.character(df$new)), collapse = " ")
  df_header <- "<tr><th style='width:8.8%; text-align:left; border:2px solid black;' rowspan='2'>Modulo</th><th style='border: 2px solid black' colspan='4'>Offensivo</th><th style='border: 2px solid black' colspan='4'>Difensivo</th><th style='border: 2px solid black' colspan='4'>Totale</th></tr><tr><th style='width:7.6%; border-bottom: 2px solid black'>Vinte</th><th style='width:7.6%; border-bottom: 2px solid black'>Pareggiate</th><th style='width:7.6%; border-bottom: 2px solid black'>Perse</th><th style='width:7.6%; border: 2px solid black;'>Totale</th><th style='width:7.6%; border-bottom: 2px solid black'>Vinte</th><th style='width:7.6%; border-bottom: 2px solid black'>Pareggiate</th><th style='width:7.6%; border-bottom: 2px solid black'>Perse</th><th style='width:7.6%; border: 2px solid black;'>Totale</th><th style='width:7.6%; border-bottom: 2px solid black'>Vinte</th><th style='width:7.6%; border-bottom: 2px solid black'>Pareggiate</th><th style='width:7.6%; border-bottom: 2px solid black'>Perse</th><th style='width:7.6%; border: 2px solid black;'>Totale</th></tr>"
  df <- paste(df_header, df)
  df <- paste("<table class='center'>", df, "</table>")
  
  modal_vec <- paste(modal_vec, collapse = " ")
  script_vec <- paste("<script>", paste(script_vec, collapse = " "), "</script>")

  pre <- "<head><style>table{border-collapse: collapse; border: 2px solid black; width: 99%;} th,td{padding: 5px; border: 1px solid gray; text-align: center;} table.center{margin-left:auto; margin-right:auto;} .button{background-color: white; color: #00d415; border: 2px solid #009e10; cursor: pointer; border-radius: 5px;} .button:hover{background-color: #00d415; color: white;} .disabled{opacity: 0.6; cursor: not-allowed;} .modal.specch{display: none; position: fixed; z-index: 1; left: 0; top: 0; width: 100%; height: 100%; overflow: auto; background-color: rgb(0, 0, 0); background-color: rgba(0, 0, 0, 0);} .modal-content.specch{background-color: rgba(0, 212, 21, 0.9); margin: 15% auto; padding: 10px; border: 5px solid #009e10; width: 30%; transform: translate(55%, -20%);} .modal-content.specch p{text-align: center; font-family: 'Open Sans', sans-serif; font-size: 14px; -webkit-border-radius: 5px !important; -moz-border-radius: 5px !important; border-radius: 5px !important;}</style></head><body>"
  post <- "</body>"
  prova <<- details
  df <- paste(pre, df, modal_vec, script_vec, post)
  print(df)
  return(df)
}
