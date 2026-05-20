#LÍBRERÍA
library(dplyr)
library(ggplot2)

# 1.  Cargar datos
load("Análisis de viviendas/Data/hogares.RData")

# 2. Test de Kruskal-Wallis
kruskal_result <- kruskal.test(sueldo ~ macrogrupo, data = hogares)

# Mostrar resultados
print(kruskal_result)

# 3. Post-hoc test (Dunn's test) para ver diferencias específicas entre grupos
library(dunn.test)
dunn_result <- dunn.test::dunn.test(hogares$sueldo, 
                                    hogares$macrogrupo, 
                                    method = "bonferroni")

# Mostrar resultados del post-hoc
print(dunn_result)

# 4. Resumen estadístico por grupo
summary_stats <- hogares %>%
  group_by(macrogrupo) %>%
  summarise(
    n = n(),
    median_sueldo = median(sueldo, na.rm = TRUE),
    mean_sueldo = mean(sueldo, na.rm = TRUE),
    iqr_sueldo = IQR(sueldo, na.rm = TRUE),
    sd_sueldo = sd(sueldo, na.rm = TRUE)
  ) %>%
  arrange((median_sueldo))

print(summary_stats)
