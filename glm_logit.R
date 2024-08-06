
library(car)
library(pscl)
library(lmtest)
library(broom)
library(tidyselect)





# Função principal
glm_logit <- function(df, var_dep, var_indep, rnd = 2, rnd_p = 4, na = TRUE) {
  
  # Função para remover níveis com zero indivíduos
  remove_empty_levels <- function(df, col_name) {
    valid_levels <- names(table(df[[col_name]]))[table(df[[col_name]]) > 0]
    df[[col_name]] <- factor(df[[col_name]], levels = valid_levels)
    return(df)
  }
  
  # Data frame para armazenar os resultados
  results <- data.frame(
    indep_variavel = character(),
    ref_indep = character(),
    comp_indep = character(),
    dep_variavel = character(),
    ref_dep = character(),
    comp_dep = character(),
    estimate = numeric(),
    std_error = numeric(),
    z_value = numeric(),
    p_value = numeric(),
    odds_ratio = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    sig = character(),
    AIC = numeric(),
    BIC = numeric(),
    logLik = numeric(),
    deviance = numeric(),
    mcFadden_r2 = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Preparar o dataframe removendo níveis vazios para variáveis dependentes e independentes
  for (dep_var in var_dep) {
    dep_col_name <- names(df)[dep_var]
    df <- remove_empty_levels(df, dep_col_name)
  }
  
  for (indep_var in var_indep) {
    indep_col_name <- names(df)[indep_var]
    df <- remove_empty_levels(df, indep_col_name)
  }
  
  # Loop para ajustar o modelo para cada variável
  for (dep_var in var_dep) {
    dep_col_name <- names(df)[dep_var]
    
    # Obter os níveis da variável dependente
    dep_levels <- levels(df[[dep_col_name]])
    
    for (indep_var in var_indep) {
      indep_col_name <- names(df)[indep_var]
      
      # Obter os níveis da variável independente
      indep_levels <- levels(df[[indep_col_name]])
      
      for (i in 2:length(dep_levels)) {
        comp_dep <- dep_levels[i]
        
        for (j in 2:length(indep_levels)) {
          comp_indep <- indep_levels[j]
          
          # Subset dos dados com níveis válidos da variável dependente e independente
          subset_data <- df[df[[dep_col_name]] %in% c(dep_levels[1], comp_dep) &
                              df[[indep_col_name]] %in% c(indep_levels[1], comp_indep), ]
          
          # Verifica se há pelo menos dois níveis na variável dependente no subset
          if (length(unique(subset_data[[dep_col_name]])) > 1) {
            formula <- as.formula(paste(dep_col_name, "~", indep_col_name))
            model <- glm(formula, data = subset_data, family = binomial)
            summary_model <- summary(model)
            
            estimate <- summary_model$coefficients[2, "Estimate"]
            ci <- confint(model)[2, ]  # Intervalos de confiança para o coeficiente
            
            # Calcular o pseudo R² de McFadden
            mcFadden_r2 <- 1 - (logLik(model)[1] / logLik(update(model, . ~ 1))[1])
            
            result <- data.frame(
              indep_variavel = indep_col_name,
              ref_indep = levels(df[[indep_col_name]])[1],
              comp_indep = comp_indep,
              dep_variavel = dep_col_name,
              ref_dep = dep_levels[1],
              comp_dep = comp_dep,
              estimate = round(estimate, rnd),
              std_error = round(summary_model$coefficients[2, "Std. Error"], rnd),
              z_value = round(summary_model$coefficients[2, "z value"], rnd),
              p_value = round(summary_model$coefficients[2, "Pr(>|z|)"], rnd_p),
              odds_ratio = round(exp(estimate), rnd),
              ci_lower = round(exp(ci[1]), rnd),
              ci_upper = round(exp(ci[2]), rnd),
              AIC = round(AIC(model), rnd),
              BIC = round(BIC(model), rnd),
              logLik = round(logLik(model), rnd),
              deviance = round(deviance(model), rnd),
              mcFadden_r2 = round(mcFadden_r2, rnd_p),
              stringsAsFactors = FALSE
            ) %>% 
              mutate(sig = ifelse(p_value < 0.05, "yes", "no"))
            
            # Adiciona os resultados ao data frame
            results <- bind_rows(results, result)
          }
        }
      }
    }
  }
  
  return(results)
}



var_dep <- c(6)
var_indep <- c(5, 7)

str(df)

results <- glm_logit(df, var_dep, var_indep)



