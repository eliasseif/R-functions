
library(tidyverse)

desc_cat <- function(df, var_sec, var_main = NULL, var_lay = NULL, rnd = 2, na = "no") {
  
    # Lista para armazenar dataframes gerados
  lista_contig <- list()
  
  
  ### loop para todos os argumentos
  if (!is.null(var_main) && !is.null(var_lay)) {
    for (lay in var_lay) {
      df[, lay] <- as.factor(df[, lay])
      lay_col_name <- names(df)[lay]
      
      for (main in var_main) {
        df[, main] <- as.factor(df[, main])
        main_col_name <- names(df)[main]
        
        for (sec in var_sec) {
          df[, sec] <- as.factor(df[, sec])
          sec_col_name <- names(df)[sec]
          
          # Criar a tabela de contingência
          cont_table <- table(df[[lay]], df[[main]], df[[sec]], useNA = na) %>% 
            as.data.frame()
          names(cont_table) <- c("v_lay", "v_main", "v_sec", "n")
          
          # Adicionar frequências relativas
          cont_table <- cont_table %>%
            mutate(porc_TOT = n / sum(n) * 100,
                   porc_TOT = round(porc_TOT, digits = rnd)) %>%
            ungroup()
          
          # Adicionar frequências relativas para a variável de estratificação
          cont_table <- cont_table %>%
            group_by(v_lay) %>%
            mutate(porc_LAY = n / sum(n) * 100,
                   porc_LAY = round(porc_LAY, digits = rnd)) %>%
            ungroup()
          
          # Adicionar frequências relativas para a variável principal
          cont_table <- cont_table %>%
            group_by(v_main) %>%
            mutate(porc_VI = n / sum(n) * 100,
                   porc_VI = round(porc_VI, digits = rnd)) %>%
            ungroup()
          
          # Adicionar frequências relativas para a variável secundária
          cont_table <- cont_table %>%
            group_by(v_sec) %>%
            mutate(porc_VD = n / sum(n) * 100,
                   porc_VD = round(porc_VD, digits = rnd)) %>%
            ungroup()
          
          # Criar linha separadora
          separador <- tibble(
            v_lay = "----------------",
            v_main = "----------------",
            v_sec = paste("Frequências de", toupper(lay_col_name), 
                          "x", toupper(main_col_name), 
                          "x", toupper(sec_col_name)))
          
          # Adicionar separador e tabela à lista
          lista_contig[[paste(lay_col_name, main_col_name, sec_col_name, sep = "_")]] <- bind_rows(separador, cont_table)
        }
      }
    }
  } else if (!is.null(var_main)) {
    for (main in var_main) {
      df[, main] <- as.factor(df[, main])
      main_col_name <- names(df)[main]
      
      for (sec in var_sec) {
        df[, sec] <- as.factor(df[, sec])
        sec_col_name <- names(df)[sec]
        
        # Criar a tabela de contingência
        cont_table <- table(df[[main]], df[[sec]], useNA = na) %>% 
          as.data.frame()
        names(cont_table) <- c("v_main", "v_sec", "n")
        
        # Adicionar frequências relativas
        cont_table <- cont_table %>%
          mutate(porc_TOT = n / sum(n) * 100,
                 porc_TOT = round(porc_TOT, digits = rnd)) %>%
          ungroup()
        
        # Adicionar frequências relativas para a variável principal
        cont_table <- cont_table %>%
          group_by(v_main) %>%
          mutate(porc_VI = n / sum(n) * 100,
                 porc_VI = round(porc_VI, digits = rnd)) %>%
          ungroup()
        
        # Adicionar frequências relativas para a variável secundária
        cont_table <- cont_table %>%
          group_by(v_sec) %>%
          mutate(porc_VD = n / sum(n) * 100,
                 porc_VD = round(porc_VD, digits = rnd)) %>%
          ungroup()
        
        # Criar linha separadora
        separador <- tibble(
          v_main = "----------------",
          v_sec   = paste("Frequências de", toupper(main_col_name), 
                          "x", toupper(sec_col_name)))
        
        # Adicionar separador e tabela à lista
        lista_contig[[paste(main_col_name, sec_col_name, sep = "_")]] <- bind_rows(separador, cont_table)
      }
    }
  } else {
    for (sec in var_sec) {
      df[, sec] <- as.factor(df[, sec])
      sec_col_name <- names(df)[sec]
      
      # Criar a tabela de contingência
      cont_table <- table(df[[sec]], useNA = na) %>% 
        as.data.frame()
      names(cont_table) <- c("v_sec", "n")
      
      # Adicionar frequências relativas
      cont_table <- cont_table %>%
        mutate(porc_TOT = n / sum(n) * 100,
               porc_TOT = round(porc_TOT, digits = rnd)) %>%
        ungroup()
      
      # Criar linha separadora
      separador <- tibble(
        v_sec   = paste("Frequências de", toupper(sec_col_name)))
      
      # Adicionar separador e tabela à lista
      lista_contig[[sec_col_name]] <- bind_rows(separador, cont_table)
    }
  }
  
  # Unir todos os dataframes em um único dataframe
  all_contig <- bind_rows(lista_contig) %>%
    as.data.frame()
  
  return(all_contig)
}

