library(data.table)
library(ggplot2)

# Cargar datos
g_1 <- fread("/Users/kgb/Desktop/INEGI_ENOE/csv/2017_trim1_enoe_csv/conjunto_de_datos_sdem_enoe_2017_1t/conjunto_de_datos/conjunto_de_datos_sdem_enoe_2017_1t.csv")
g_2 <- fread("/Users/kgb/Desktop/INEGI_ENOE/csv/2017_trim2_enoe_csv/conjunto_de_datos_sdem_enoe_2017_2t/conjunto_de_datos/conjunto_de_datos_sdem_enoe_2017_2t.csv")
g_3 <- fread("/Users/kgb/Desktop/INEGI_ENOE/csv/2017_trim3_enoe_csv/conjunto_de_datos_sdem_enoe_2017_3t/conjunto_de_datos/conjunto_de_datos_sdem_enoe_2017_3t.csv")
g_4 <- fread("/Users/kgb/Desktop/INEGI_ENOE/csv/2017_trim4_enoe_csv/conjunto_de_datos_sdem_enoe_2017_4t/conjunto_de_datos/conjunto_de_datos_sdem_enoe_2017_4t.csv")

# Filtrar y seleccionar columnas relevantes
procesar_trimestre <- function(df, trimestre) {
    df_filtrado <- df[clase1 == 1 & sex %in% c(1, 2) & scian %in% c(1:21),
                      .(sex, scian, ingocup)]
    df_promedio <- df_filtrado[, .(Salario_Promedio = mean(ingocup, na.rm = TRUE)), by = .(scian, sex)]
    df_promedio[, sex := factor(sex, levels = c(1, 2), labels = c("Hombres", "Mujeres"))]
    df_promedio[, Trimestre := trimestre] # Agregar la columna de trimestre
    return(df_promedio)
}

# Procesar cada trimestrg
salario_promedio_g_1 <- procesar_trimestre(g_1, "Trimestre 1")
salario_promedio_g_2 <- procesar_trimestre(g_2, "Trimestre 2")
salario_promedio_g_3 <- procesar_trimestre(g_3, "Trimestre 3")
salario_promedio_g_4 <- procesar_trimestre(g_4, "Trimestre 4")

# Unir todos los trimestres en un solo data.table
salario_promedio <- rbindlist(list(salario_promedio_g_1, salario_promedio_g_2, salario_promedio_g_3, salario_promedio_g_4))

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

# Asignar nombres de actividades económicas
salario_promedio[, actividad := actividades[scian]]

View((salario_promedio))

# Crear la gráfica con facetas por trimestre
g <- ggplot(salario_promedio, aes(x = actividad, y = Salario_Promedio, fill = sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    facet_wrap(~Trimestre) +  # Separa la gráfica por trimestre
    labs(title = "Salario mensual promedio por actividad económica y género (2017)",
         x = "Actividad económica", y = "Salario promedio (MXN)", fill = "Sexo") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

print(g)

# Guardar el gráfico como PDF
ggsave("2017_enoe_salario_sexo_actividad.pdf", plot = g, device = "pdf", width = 10, height = 8)