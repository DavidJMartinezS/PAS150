Estadisticos_fun <- function(a, A, data, sp) {
  # sp <- sp %>% str_replace(" ",".")
  df <- data %>% 
    select(sp, Nha_Total) %>% 
    mutate_all(round) %>% 
    pivot_longer(cols = 1:2, names_to = "Variable", values_to = "Nha") %>% 
    group_by(Variable) %>% 
    summarise(
      Rango = str_c(min(Nha)," - ", max(Nha)),
      n = n(),
      Promedio = mean(Nha, na.rm = T) %>% round(),
      s2 = ((1-(n*a/A))*(sd(Nha)^2/n) )%>% round(2),
      CV = ((sqrt(s2)/Promedio)*100) %>% round(1),
      T_est = qt(0.975,n-1),
      E_abs = (T_est * sqrt(s2)) %>% round(1),
      E_rel = ((E_abs/Promedio)*100) %>% round(1),
      Int_conf = str_c(round(Promedio - E_abs), " - ", round(Promedio + E_abs))
    ) %>% 
    select(-T_est) 
    return(df)
}
