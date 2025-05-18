# Datos
set.seed(123)  # Para reproducibilidad
datos_poblacion1 <- c(9.7, 10.2, 10.9, 8.6, 10.3)
datos_poblacion2 <- c(8.2, 6.1, 9.6, 8.2, 8.9)

# Prueba de hipótesis no paramétrica (Wilcoxon rank-sum test)
prueba_wilcoxon <- wilcox.test(datos_poblacion1, datos_poblacion2, alternative = "two.sided")
p_valor_wilcoxon <- prueba_wilcoxon$p.value
cat("P-valor de la prueba de Wilcoxon:", p_valor_wilcoxon, "\n")

# Función para realizar pruebas de Monte Carlo
test_monte_carlo <- function(s) {
  p_valores <- numeric(s)
  n1 <- length(datos_poblacion1)
  n2 <- length(datos_poblacion2)
  
  for (i in 1:s) {
    datos_permutados <- sample(c(datos_poblacion1, datos_poblacion2))
    grupo1 <- datos_permutados[1:n1]
    grupo2 <- datos_permutados[(n1 + 1):(n1 + n2)]
    
    p_valores[i] <- wilcox.test(grupo1, grupo2, alternative = "two.sided")$p.value
  }
  return(mean(p_valores))  # Promedio de p-valores simulados
}

# Monte Carlo con diferentes números de simulaciones
s_values <- c(19, 49, 99, 999)
p_valores_monte_carlo <- sapply(s_values, test_monte_carlo)

# Comparación de resultados
resultados <- data.frame(Repeticiones = s_values, P_Valor_MonteCarlo = p_valores_monte_carlo)
print(resultados)
