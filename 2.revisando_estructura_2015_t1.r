# Cargar los datos
library(data.table)  # Para manejar datos grandes eficientemente

# Leer el archivo de la ENOE_2015(ajusta la ruta según tu caso)
e_1 <- fread("/Users/del_mal/Desktop/INEGI_ENOE/2015_trim1_enoe_csv/COE1T115.csv")



#c <- colnames(e_1)  # Muestra los nombres de las columnas. Se utiliza "sex" %in% c para saber si sex es parte de las cabeceras del csv.
# h <- head(e_1)  # Muestra las primeras filas para entender la estructura
# s <- str(e_1)   # Verifica los tipos de datos

# SEXO: Género de la persona (1 = Hombre, 2 = Mujer)

# CLASE1: Clasificación del sector económico basada en el SCIAN


#e_1_pea <- e_1[e_1$p1 == 1, ] # P1: Población ocupada == 1 == TRUE

#e_1_pea <- e_1[e_1$p1 == 1, c("sex", names(e_1))]




