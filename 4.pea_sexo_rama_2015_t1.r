library(data.table)

e_4 <- fread("/Users/del_mal/Desktop/INEGI_ENOE/2015_trim1_enoe_csv/SDEMT115.csv")

# Filtrar mujeres económicamente activa profesionales, técnicos y trabajadores del arte
e_4_pea_sex_rama <- e_4[clase1 == 1 & sex %in% c(1, 2) & c_ocu11c %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) , .(sex, c_ocu11c)]
conteo_sex_rama <- table(e_4_pea_sex_rama$c_ocu11c, e_4_pea_sex_rama$sex)
print(conteo_sex_rama)

# print(dim(e_4_pea_sex_rama1)) # Hay 17486 personas que trabajan en la rama 1

# conteo_rama1 <- table(e_4_pea_sex_rama1$sex)
# print(conteo_rama1) # Hay 10164 hombres y 7322 mujeres