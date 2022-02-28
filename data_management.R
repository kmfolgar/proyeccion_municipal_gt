library(tidyverse) 
library(readxl)
library(stringr)

files <- list.files("data/")
data1 <- data.frame()
counter=0
for (departamento in files)
  {
  dir <- paste0("data/", departamento)
  municipios <- excel_sheets(dir)
  
  for (municipio in municipios) {
    pop <- read_xlsx(dir, sheet = municipio, skip=7)
    
    hombres <- pop[89:174,] %>% 
      rename(Age=`...1`) %>% 
      mutate(Sexo="Male",
             Age=if_else(Age=="70 años o más", "70", Age)) %>% 
      mutate(n1=str_detect(Age, "a")) %>% 
      filter(n1==FALSE) %>% 
      select(-n1) %>% 
      gather(Year, Population, -Sexo, -Age) 
    
    mujeres <- pop[176:261,] %>% 
      rename(Age=`...1`) %>% 
      mutate(Sexo="Female",
             Age=if_else(Age=="70 años o más", "70", Age)) %>% 
      mutate(n1=str_detect(Age, "a")) %>% 
      filter(n1==FALSE) %>% 
      select(-n1) %>% 
      gather(Year, Population, -Sexo, -Age) 
    
    muni <- rbind(hombres,mujeres) %>% mutate(depto=departamento, mupio=municipio)
    
    data1 <- rbind(data1, muni)
    counter = counter + 1
    counter
  }
}

data1 %>% 
  mutate(depto1=str_sub(depto, 4, -6)) %>% 
  select(-depto) -> data0

write.csv(data0, "proyeccion_municipal_edad_simple_por_sexo_2015-2030.csv")
