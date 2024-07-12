install.packages("tidyverse")
install.packages("Rtools")
install.packages("sf")
install.packages("ggrepel")


library(tidyverse)
library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)

# Cargar los datos
data <- read.csv("C:/Users/devan/Downloads/Desaparicion/verdata-desaparicion-R1.csv",
                 sep = ",")

# Visualizar las primeras filas
head(data)
#view(data)


# Eliminar las columnas desde 'replica' hasta 'in_34'
# y eliminar las que no se usarán
data_mod <- data %>%
  select(-replica:-in_34)

data_mod <- data_mod %>%
  select(dept_code_hecho,edad_categoria,etnia, is_forced_dis
         ,p_str,sexo,yy_hecho)

# Verificar que las columnas se han eliminado
colnames(data_mod)
#view(data_mod)

# Filtrar datos relevantes y calcular frecuencias de edad
age_distribution <- data_mod %>%
  filter(!is.na(edad_categoria)) %>%
  count(edad_categoria)

# Crear un dataframe con los nombres y códigos de los departamentos
dept_codes <- data.frame(
  Codigo = c(91, 5, 81, 8, 11, 13, 15, 17, 18, 85, 19, 20, 27, 23, 25, 94, 41, 44, 47, 50, 52, 54, 86, 63, 66, 88, 68, 70, 73, 76, 97, 99),
  Dept = c("Amazonas", "Antioquia", "Arauca", "Atlántico", "Bogotá D.C.", "Bolívar", "Boyacá", "Caldas", "Caquetá", "Casanare", "Cauca", "Cesar",
             "Chocó", "Córdoba", "Cundinamarca", "Guainía", "Huila", "La Guajira", "Magdalena", "Meta", "Nariño", "Norte de Santander",
             "Putumayo", "Quindío", "Risaralda", "San Andrés", "Santander", "Sucre", "Tolima", "Valle del Cauca", "Vaupés", "Vichada")
)

# Unir los datos modificados con el dataframe de códigos de departamentos
data_mod <- data_mod %>%
  left_join(dept_codes, by = c("dept_code_hecho" = "Codigo"))

# Verificar cuántos valores faltantes hay por columna
missing_values <- colSums(is.na(data_mod))
cat(paste0("Valores faltantes: "), missing_values)

# Mostrar los datos después de la limpieza y modificación del dataset
head(data_mod, n = 100)


# Calcular frecuencias de municipios usando los nombres
municipio_distribution <- data_mod %>%
  filter(!is.na(Dept)) %>%
  count(Dept)

# Remover las filas con NA
data_mod <- data_mod %>%
  drop_na()

# Verificar cuántas filas se removieron
nrow(data_mod) - nrow(data_mod)

# Verificar cuántos valores faltantes hay por columna

missing_values <- colSums(is.na(data_mod))
cat(paste0("Valores faltantes: "), missing_values)

# visualización
#view(data_mod)

# Visualizar la distribución de edades
ggplot(age_distribution, aes(x = edad_categoria, y = n)) +
  geom_bar(stat = "identity", fill = "#000") +
  labs(title = "Distribución de Edad de las Víctimas en el Conflicto Armado en Colombia",
       x = "Edad",
       y = "Número de Víctimas") +
  theme_minimal()

# Visualizar la distribución de departamentos
ggplot(municipio_distribution, aes(x = reorder(Dept, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#A1EBC5") +
  labs(title = "Distribución de Víctimas del Conflicto Armado por Departamento en Colombia",
       x = "Departamento",
       y = "Número de Víctimas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Encontrar la edad con mayor frecuencia
mode_age <- age_distribution %>%
  filter(n == max(n)) %>%
  pull(edad_categoria)
mode_age

# Calcular frecuencias de Departamentos y sexos
municipio_sexo_distribution <- data_mod %>%
  count(Dept, sexo)

# Visualizar la distribución de sexos por Departamento
ggplot(municipio_sexo_distribution, aes(x = reorder(Dept, -n), y = n, fill = sexo)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribución de Sexo por Departamento de victimas del Conflicto Armado",
       x = "Departamento",
       y = "Número de Víctimas",
       fill = "Sexo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Contar el número de incidentes por año de desaparición forzada, valor = TRUE
yearly_distribution <- data_mod %>%
  filter(is_forced_dis == TRUE) %>%
  filter(!is.na(yy_hecho)) %>%
  count(yy_hecho)

# Visualizar la distribución de incidentes por año
ggplot(yearly_distribution, aes(x = yy_hecho, y = n)) +
  geom_line(color = "#000") +
  geom_point(color = "red") +
  geom_text(aes(label = yy_hecho), vjust = -0.5, size = 3) +
  labs(title = "Distribución Anual de Victimas de Desaparición Forzada",
       x = "Año",
       y = "Número de Incidentes") +
  theme_minimal()

# Visuaizar el boxplot entre yy_hechos y Dept
ggplot(data_mod, aes(x = Dept, y = yy_hecho)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16,
               outlier.size = 2, notch = TRUE) +
  labs(title = "Distribución de Años de los Hechos por Departamento",
       x = "Departamento",
       y = "Año del Hecho") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Cargar los datos del shapefile de los departamentos
data_mod_sf <- st_read("C:/Users/devan/Downloads/MGN_DPTO_POLITICO/MGN_DPTO_POLITICO.shp")

# Verificar la estructura de los datos
print(unique(data_mod$dept_code_hecho))
print(unique(data_mod_sf$DPTO_CCDGO))


# Asegurarse de que los códigos de los departamentos están como numéricos
data_mod_sf <- data_mod_sf %>%
  mutate(DPTO_CCDGO = as.numeric(DPTO_CCDGO))

if (!"incident_count" %in% names(data_mod_sf)) {
  data_mod_sf$incident_count <- 0
}

#view(data_mod_sf)

# Calcular frecuencias de municipios usando los codigos
municipio_distribution <- data_mod %>%
  filter(!is.na(dept_code_hecho)) %>%
  count(dept_code_hecho, p_str, name = "incident_count")

# Unir los datos de conteo de incidentes con el shapefile
data_mod_sf <- data_mod_sf %>%
  left_join(municipio_distribution, by = c("DPTO_CCDGO" = "dept_code_hecho"))

# Reemplazar NA en la columna de conteo de incidentes con 0
# data_mod_sf$incident_count <- ifelse(is.na(data_mod_sf$incident_count), 0, data_mod_sf$incident_count)

is.na(data_mod$etnia)
sum(is.na(data_mod$etnia))

unique(data_mod$etnia)

# Remover las filas con NA
data_mod_sf <- data_mod_sf %>%
  drop_na()

# Verificar cuántas filas se removieron
nrow(data_mod) - nrow(data_mod)

# Visualizar el mapa

ggplot(data = data_mod_sf) +
  geom_sf(aes(fill = incident_count.y)) +
  scale_fill_continuous(name = "Número de incidentes") +
  facet_wrap(~ p_str, ncol = 5) +
  theme_void() +
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 10, face = "bold"),  
        legend.text = element_text(size = 8),  
        legend.title = element_text(size = 10), 
        panel.spacing = unit(1, "lines")) +  # Espacio entre facetas) +
  labs(title = "Número de Incidentes de Desaparición Forzada por Departamento")

# Contar las frecuencias de cada etnia
etnia_counts <- data_mod %>%
  filter(!is.na(etnia)) %>%
  count(etnia)

# Calcular porcentajes
etnia_counts <- etnia_counts %>%
  mutate(pct = n / sum(n) * 100)

# Crear el diagrama de torta
ggplot(etnia_counts, aes(x = "", y = n, fill = etnia)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text_repel(aes(label = ifelse(pct > 0, paste0(round(pct, 2), "%"), "")), 
            position = position_stack(vjust = 0.5),
            direction = "y") +
  labs(title = "Distribución de las Etnias de las Víctimas en el Conflicto Armado en Colombia",
       x = NULL,
       y = NULL,
       fill = "Etnia") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
