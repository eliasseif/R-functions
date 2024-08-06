

### Função para análise descritiva de variáveis numéricas utilizando o 
### numero ou nome da coluna que deseja descrever


library(dplyr)

# Definir a função para calcular as descrições
desc_num <- function(df, v_dep, 
                     v_indep = NULL, v_indep2 = NULL, 
                     rnd = 2, na = TRUE) {
  
  #### Função para calcular o intervalo de confiança (CI95%)
  ci <- function(x, conf = 0.95) {
    n <- length(x)
    se <- sd(x) / sqrt(n)
    alpha <- 1 - conf
    error_margin <- qt(1 - alpha/2, df = n - 1) * se
    mean_x <- mean(x)
    c(lower = mean_x - error_margin, upper = mean_x + error_margin)
  }
  
  #### Lista para armazenar a tabela de descrição gerada
  desc_list <- list()
  
  #### Estrutura de loop que calcula medidas descritivas quando indep 1 e 2 TRUE
  
  if (!is.null(v_indep) & !is.null(v_indep2)) {
    for (indep2 in v_indep2) {
      indep2_col_name <- names(df)[indep2]
      
      for (indep in v_indep) {
        indep_col_name <- names(df)[indep]
        
        for (dep in v_dep) {
          dep_col_name <- names(df)[dep]
          
          desc <- df %>%
            group_by(.data[[indep_col_name]], 
                     .data[[indep2_col_name]]) %>%   # Define variáveis independentes
            
            summarise(                               # Calcula descrições
              mean = round(mean(.data[[dep_col_name]], na.rm = na), rnd), 
              median = round(median(.data[[dep_col_name]], na.rm = na), rnd),
              sd = round(sd(.data[[dep_col_name]], na.rm = na), rnd),
              min = min(.data[[dep_col_name]], na.rm = na),
              max = max(.data[[dep_col_name]], na.rm = na),
              q25 = round(quantile(.data[[dep_col_name]], 0.25, na.rm = na), rnd),
              q75 = round(quantile(.data[[dep_col_name]], 0.75, na.rm = na), rnd),
              ci_lower = round(ci(.data[[dep_col_name]])[1], rnd),
              ci_upper = round(ci(.data[[dep_col_name]])[2], rnd)) %>%
            
            arrange(.data[[indep_col_name]], 
                    .data[[indep2_col_name]]) %>%     # Ordena pelos nomes das variáveis independentes
            mutate(var_dep = dep_col_name) %>%
            select(var_dep, everything())
          
          desc_list[[paste(indep_col_name, indep2_col_name, dep_col_name, sep = "_")]] <- desc
        }
      }
    }
  } else if (!is.null(v_indep)) {                  # medidas descritivas quando indep 1 TRUE
    for (indep in v_indep) {
      indep_col_name <- names(df)[indep]
      
      for (dep in v_dep) {
        dep_col_name <- names(df)[dep]
        
        desc <- df %>%
          group_by(.data[[indep_col_name]]) %>%    # Define variável independente
          
          summarise(                               # Calcula descrições
            mean = round(mean(.data[[dep_col_name]], na.rm = na), rnd), 
            median = round(median(.data[[dep_col_name]], na.rm = na), rnd),
            sd = round(sd(.data[[dep_col_name]], na.rm = na), rnd),
            min = min(.data[[dep_col_name]], na.rm = na),
            max = max(.data[[dep_col_name]], na.rm = na),
            q25 = round(quantile(.data[[dep_col_name]], 0.25, na.rm = na), rnd),
            q75 = round(quantile(.data[[dep_col_name]], 0.75, na.rm = na), rnd),
            ci_lower = round(ci(.data[[dep_col_name]])[1], rnd),
            ci_upper = round(ci(.data[[dep_col_name]])[2], rnd)) %>%
          
          arrange(.data[[indep_col_name]]) %>%     # Ordena pelo nome da variável independente
          mutate(var_dep = dep_col_name) %>%
          select(var_dep, everything())
        
        desc_list[[paste(indep_col_name, dep_col_name, sep = "_")]] <- desc
      }
    }
  } else {
    for (dep in v_dep) {    #### medidas descritivas quando indep 1 e 2  FALSE
      dep_col_name <- names(df)[dep]
      
      desc <- df %>%
        summarise(                               # Calcula descrições
          mean = round(mean(.data[[dep_col_name]], na.rm = na), rnd), 
          median = round(median(.data[[dep_col_name]], na.rm = na), rnd),
          sd = round(sd(.data[[dep_col_name]], na.rm = na), rnd),
          min = min(.data[[dep_col_name]], na.rm = na),
          max = max(.data[[dep_col_name]], na.rm = na),
          q25 = round(quantile(.data[[dep_col_name]], 0.25, na.rm = na), rnd),
          q75 = round(quantile(.data[[dep_col_name]], 0.75, na.rm = na), rnd),
          ci_lower = round(ci(.data[[dep_col_name]])[1], rnd),
          ci_upper = round(ci(.data[[dep_col_name]])[2], rnd)) %>%
        
        mutate(var_dep = dep_col_name) %>%
        select(var_dep, everything())
      
      desc_list[[dep_col_name]] <- desc
    }
  }
  
  #### Mescla listas geradas em um único dataframe
  all_desc <- bind_rows(desc_list) %>% 
    as.data.frame()
  
  return(all_desc)
}

