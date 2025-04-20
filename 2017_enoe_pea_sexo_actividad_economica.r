library(data.table)
library(ggplot2)

g_1 <- fread("/Users/kgb/Desktop/INEGI_ENOE/csv/2017_trim1_enoe_csv/conjunto_de_datos_sdem_enoe_2017_1t/conjunto_de_datos/conjunto_de_datos_sdem_enoe_2017_1t.csv")
g_2 <- fread("/Users/kgb/Desktop/INEGI_ENOE/csv/2017_trim2_enoe_csv/conjunto_de_datos_sdem_enoe_2017_2t/conjunto_de_datos/conjunto_de_datos_sdem_enoe_2017_2t.csv")
g_3 <- fread("/Users/kgb/Desktop/INEGI_ENOE/csv/2017_trim3_enoe_csv/conjunto_de_datos_sdem_enoe_2017_3t/conjunto_de_datos/conjunto_de_datos_sdem_enoe_2017_3t.csv")
g_4 <- fread("/Users/kgb/Desktop/INEGI_ENOE/csv/2017_trim4_enoe_csv/conjunto_de_datos_sdem_enoe_2017_4t/conjunto_de_datos/conjunto_de_datos_sdem_enoe_2017_4t.csv")

# Unir los cuatro trimestres en una sola tabla
enoe_2017 <- rbindlist(list(g_1, g_2, g_3, g_4))

# Filtrar y seleccionar columnas
enoe_pea_sex_actividad_salario <- enoe_2017[clase1 == 1 & sex %in% c(1, 2) & scian %in% c(1:21), .(sex, scian)]

# Crear tabla de frecuencia y convertirla en data.table
conteo_enoe_sex_actividad <- as.data.table(table(enoe_pea_sex_actividad_salario$sex, enoe_pea_sex_actividad_salario$scian))

# Renombrar columnas
setnames(conteo_enoe_sex_actividad, c("V1", "V2", "N"), c("Sexo", "Actividad económica", "Frecuencia"))

# Renombrar sex: 1 -> Hombres, 2 -> Mujeres
conteo_enoe_sex_actividad[, Sexo := factor(Sexo, levels = c(1, 2), labels = c("Hombres", "Mujeres"))]

# Convertir Actividad económica a integer antes de asignar nombres
conteo_enoe_sex_actividad[, `Actividad económica` := as.integer(`Actividad económica`)]

# Definir nombres de actividades económicas
actividades <- c(
    "Agricultura, ganadería, aprovechamiento forestal, pesca y caza",
    "Minería",
    "Generación y distribución de electricidad, suministro de agua y gas",
    "Construcción",
    "Industrias manufactureras",
    "Comercio al por mayor",
    "Comercio al por menor",
    "Transportes, correos y almacenamiento",
    "Información en medios masivos",
    "Servicios financieros y de seguros",
    "Servicios inmobiliarios y de alquiler de bienes",
    "Servicios profesionales, científicos y técnicos",
    "Corporativos",
    "Servicios de apoyo a los negocios y manejo de desechos",
    "Servicios educativos",
    "Servicios de salud y de asistencia social",
    "Servicios de esparcimiento, culturales y deportivos",
    "Servicios de hospedaje y de preparación de alimentos y bebidas",
    "Otros servicios, excepto actividades gubernamentales",
    "Actividades gubernamentales y de organismos internacionales",
    "No especificado"
)

# Asignar los nombres de actividades económicas a la tabla de frecuencia
conteo_enoe_sex_actividad[, Nombre := actividades[as.integer(`Actividad económica`)]]

# Ordenar por actividad y sexo
setorder(conteo_enoe_sex_actividad, `Actividad económica`,`Sexo`)

View(conteo_enoe_sex_actividad)


# # Verificación
# print("Columnas en conteo_enoe_sex_actividad")
# print(conteo_enoe_sex_actividad)

# Crear la gráfica
g <- ggplot(conteo_enoe_sex_actividad, aes(x = `Actividad económica`, y = Frecuencia, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribución de hombres y mujeres por actividad económica (2017)",
       x = "Actividad económica",
       y = "Frecuencia",
       fill = "Sexo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

print(g)

# Guardar el gráfico como PDF
ggsave("2017_enoe_pea_sexo_actividad.pdf", plot = g, device = "pdf", width = 10, height = 8)
