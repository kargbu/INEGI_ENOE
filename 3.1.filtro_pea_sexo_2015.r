library(data.table)

# Cargar el archivo CSV
e_1 <- fread("/Users/kgb/Desktop/INEGI_ENOE/2015_trim1_enoe_csv/SDEMT115.csv")
e_2 <- fread("/Users/kgb/Desktop/INEGI_ENOE/2015_trim2_enoe_csv/SDEMT215.csv")
e_3 <- fread("/Users/kgb/Desktop/INEGI_ENOE/2015_trim3_enoe_csv/SDEMT315.csv")
e_4 <- fread("/Users/kgb/Desktop/INEGI_ENOE/2015_trim4_enoe_csv/SDEMT415.csv")

# Unir los cuatro trimestres en una sola tabla
enoe_2015 <- rbindlist(list(e_1, e_2, e_3, e_4))

# Filtrar la población económicamente activa (clase1 == 1) y sexo válido (1==hombre, 2==mujer)
e_4_pea_sex <- enoe_2015[clase1 == 1 & sex %in% c(1, 2), .(clase1, sex)]

# Ver los primeros registros del data.table resultante
#print(head(e_4_pea_sex))

# Conteo de hombre y mujeres pea
conteo_sexo <- table(e_4_pea_sex$sex)

# Imprimir el conteo
print(conteo_sexo) #Total de hombres pea == 107552 y mujeres == 69348
hombres_pea <- (107552/404432)* 100
mujeres_pea <- (69348/404432)* 100

print('Porcentajes de hombres económicamente activos')
print(hombres_pea)

print('Porcentajes de mujeres económicamente activas')
print(mujeres_pea)