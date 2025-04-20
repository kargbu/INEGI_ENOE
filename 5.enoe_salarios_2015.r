library(data.table)
library(ggplot2)

# Cargar los datos de los 4 trimestres
e_1 <- fread("/Users/kgb/Desktop/INEGI_ENOE/2015_trim1_enoe_csv/SDEMT115.csv")
e_2 <- fread("/Users/kgb/Desktop/INEGI_ENOE/2015_trim2_enoe_csv/SDEMT215.csv")
e_3 <- fread("/Users/kgb/Desktop/INEGI_ENOE/2015_trim3_enoe_csv/SDEMT315.csv")
e_4 <- fread("/Users/kgb/Desktop/INEGI_ENOE/2015_trim4_enoe_csv/SDEMT415.csv")

# Unir los cuatro trimestres en una sola tabla
enoe_2015 <- rbindlist(list(e_1, e_2, e_3, e_4))

# Filtrar datos por PEA, sexo y actividad económica
salarios_pea <- enoe_2015[clase1 == 1 & sex %in% c(1, 2) & scian %in% c(1:21), .(sex, scian, ingocup)]

# Calcular el salario promedio por actividad económica y sexo
salario_promedio <- salarios_pea[, .(Salario_Promedio = mean(ingocup, na.rm = TRUE)), by = .(scian, sex)]

# Renombrar sex: 1 -> Hombres, 2 -> Mujeres
salario_promedio[, sex := factor(sex, levels = c(1, 2), labels = c("Hombres", "Mujeres"))]

# Crear un vector con los nombres de actividades económicas
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
# Asignar los nombres de actividades económicas
salario_promedio[, Actividad := actividades[scian]]
# Ordenar por actividad y sexo
setorder(salario_promedio, scian, sex)

# Mostrar resultados
Summary(salario_promedio)

ggplot(salario_promedio, aes(x = Actividad, y = Salario_Promedio, fill = sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "Salario mensual promedio por actividad económica y género",
         x = "Actividad económica", y = "Salario promedio (MXN)") +
    theme_minimal()

