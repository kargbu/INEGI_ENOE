library(data.table)

h_1 <- fread("/Users/kgb/Desktop/INEGI_ENOE/csv/2018_trim1_enoe_csv/conjunto_de_datos_sdem_enoe_2018_1t/conjunto_de_datos/conjunto_de_datos_sdem_enoe_2018_1t.csv")
h_2 <- fread("/Users/kgb/Desktop/INEGI_ENOE/csv/2018_trim2_enoe_csv/conjunto_de_datos_sdem_enoe_2018_2t/conjunto_de_datos/conjunto_de_datos_sdem_enoe_2018_2t.csv")
h_3 <- fread("/Users/kgb/Desktop/INEGI_ENOE/csv/2018_trim3_enoe_csv/conjunto_de_datos_sdem_enoe_2018_3t/conjunto_de_datos/conjunto_de_datos_sdem_enoe_2018_3t.csv")
h_4 <- fread("/Users/kgb/Desktop/INEGI_ENOE/csv/2018_trim4_enoe_csv/conjunto_de_datos_sdem_enoe_2018_4t/conjunto_de_datos/conjunto_de_datos_sdem_enoe_2018_4t.csv")

# Unir los cuatro trimestres en una sola tabla
enoe_2018 <- rbindlist(list(h_1, h_2, h_3, h_4))

# Filtrar la población económicamente activa (clase1 == 1) y sexo válido (1==hombre, 2==mujer)
e_4_pea_sex <- enoe_2018[clase1 == 1 & sex %in% c(1, 2), .(clase1, sex)]

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