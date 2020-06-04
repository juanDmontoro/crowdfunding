

# Loading libraries: rvest ------------------------------------------------------------------

library(rvest)
library(tidyverse)
library(stringr)



# Kickstarter data --------------------------------------------------------

webpage <- read_html("https://www.kickstarter.com/discover/categories/music")
results <- webpage%>% html_nodes(".grid-col-4-lg")
results[[1]] # promete
item <- results[[1]]  

# La estrategia habitual de limpiar el markup code para dejar solo texto NO FUNCIONA
html_text(results,trim=T) 

# Veamos con un análisis más detallado 
results %>%str # results es una lista 
length(results) # con 12 elementos
results[[4]] # llamada a uno de los elementos
results[[4]] %>% str # la estructura es la de xml_node que se puede analizar usando las utilidades de la libreria xml2

item <- results[[4]]
item
xml_attrs(item) # attributos del objeto
xml_attr(item,"id") # devuelve el valor del atributo id
xml_attr(item,"data-ref") # devuelve el valor del atributo id
xml_attr(item,"data-project") # contiene los valores relevantes del proyecto de crowdfunding

#
# Análisis detallado de los datos que disponemos del proyecto
#
proyecto <- xml_attr(item,"data-project")
proyecto %>% str # se trata de una cadena de caracteres. habría que extraer los trozos de info relevante. Creo que es factible

#limpiar códigos no alfanuméricos - no es buena idea: elimina todo, incluso delimitadores 
str_replace_all(proyecto, "[[:punct:]]", " ") 

# procesamiento restringido: eliminamos caracteres y partimos el resultado
str_replace_all(proyecto, "[\" { }]", " ") %>% #limpieza de comillas y llaves
  str_split(" :",simplify=T) %>% str # partimos la cadena usando " :" como delimiter
# el resultado es un vector con cadenas de 110 elementos 

# guardamos el resultado
proyecto_4 <- str_replace_all(proyecto, "[\" { }]", " ") %>%
  str_replace_all(" :", ": ") %>%
  str_split(": ",simplify=T) 

# LIMPIANDO
str_replace(gsub("\\s+", " ", str_trim(proyecto)), "B", "b")%>%
  str_replace_all( "[\" { }]", " ") %>%
  str_replace_all(" :", ": ")

# PROCEDIMIENTO DEFINITIVO
proyecto_4 <- str_split(proyecto, boundary("word")) # esto!!!! 
# codigos a buscar: 
# id(*), name(*), goal, pledged, currency, deadline, state_changed_at, created_at
# launched_at, staff_pick, is_starrable, backers_count, static_usd_rate
# usd_pledged, project_id, state, percent_funded
# (*) ojo! hay varios name e id!!!! como proceder????


# estrategia: función que toma una cadena, la busca en el vector y devuelve elitem a continuación

proyecto_4 <- unlist(proyecto_4) # convertimos en unb array simple

match("project_id",proyecto_4) # devuelve la posición
match("project_id",proyecto_4) # devuelve la posición -> problema: hay 4 pero solo devuelve el primero
"id" %in% proyecto_4 # indica si hay elemento "id"

#opción DF
#convirtiendo el vector en DF se puede identificar claramente 
# la ubicación de los contenidos que nos interesan
data.table::shift(proyecto_4,n = -1) # deplaza los elementos de un vector  hacia atrás

df <- tibble(code=proyecto_4,
             content=data.table::shift(proyecto_4,n = -1),
             i=seq_along(proyecto_4))
df%>% filter(code=="id")
df%>% filter(code=="project_id")
df%>% filter(code=="launched_at")

# CVOMPLETAR FUNCION PARA DEVOLVER DE UN PROYECTO EL CONTENIDO INTERESANTE! 

return_the_value <- function(vector,cadena){
  
}




# Failed attempts ---------------------------------------------------------

# ulule: no luck
webpage <- read_html("https://es.ulule.com/discover/?categories=music&sorting=amount&statuses=ended")
webpage
str(webpage)
body_nodes <- webpage %>% html_node("body") %>%   html_children()
body_nodes[[1]]

body_nodes%>% html_children()


# extract nodes
webpageNodes <- html_nodes(webpage, "table")


results <- webpage%>% html_nodes("sc-fzoNJl fgPYNY")
results
results <- webpage%>% html_nodes(".kbERCF")
results
results <- webpage%>% html_nodes(".khYBej")
results
results <- webpage%>% html_nodes(".b-list--inline")
results

results <- webpage%>% html_nodes(".fgPYNY")
results  

# option A
table<-html_nodes(webpage, "tbody tr")
table<-html_nodes(webpage, "table")

webpage%>% html_structure()



# INDIEGOGO. Neither

webpage <- read_html("https://www.indiegogo.com/explore/music?project_type=campaign&project_timing=all&sort=trending")
webpage
results <- webpage%>% html_nodes(".discoverableCard-percent")
results

table<-html_nodes(webpage, "table")





# Uso de Rselenium 1 --------------------------------------------------------


rD1 <- RSelenium::rsDriver(browser = "chrome", port = 4567L, geckover = NULL, 
                           chromever = "83.0.4103.39")
remote_driver <- rD1[["client"]]
remote_driver$navigate("https://www.latlong.net/convert-address-to-lat-long.html")

# in the above page we take a look at the html code.
# When looking at the HTML code, then we can see that the box is located in 
# this snippet above with the XPath @class = “width70”. So, the code below shows how to 
#navigate to that particular text box.

address_element <- remote_driver$findElement(using = 'class', value = 'width70')

# Now, we have to let RSelenium type in the address we want to get coordinates for.
address_element$sendKeysToElement(list("Erudito Orellana, Valencia, Spain"))

# finally find the button to click
button_element <- remote_driver$findElement(using = 'class', value = "button")
button_element$clickElement()

out <- remote_driver$findElement(using = "class", value="coordinatetxt")
lat_long <- out$getElementText() # datos para casa

# pedimos varias direccions: OJO! limita el número de accesos!

street_names <- c("Avenida de los naranjos, Valencia, Spain", 
                  "Calle Salamanca, Valencia, España", 
                  "Calle de la Gardenia, El Tosalet, Xabia, Spain", 
                  "Plaza Mayor, Sumacarcer, Spain")

get_lat_lon <- function(street_names) {
  remote_driver$navigate("https://www.latlong.net/convert-address-to-lat-long.html")
  final <- c()
  for(i in 1:length(street_names)) {
    
    remote_driver$refresh()
    Sys.sleep(1)
    
    address_element <- remote_driver$findElement(using = 'class', value = 'width70')
    
    address_element$sendKeysToElement(list(street_names[i]))
    button_element <- remote_driver$findElement(using = 'class', value = "button")
    
    button_element$clickElement()
    Sys.sleep(3)
    
    out <- remote_driver$findElement(using = "class", value = "coordinatetxt")
    output <- out$getElementText()
    final <- c(final, output)
    
  }
  
  return(final)
}


vector_out <- get_lat_lon(street_names)

vector_out[[2]]



# Uso de R Selenium 2 -----------------------------------------------------






###############################################################################################

driver <- RSelenium::rsDriver(browser = "chrome",
                              chromever =
                                system2(command = "wmic",
                                        args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
                                        stdout = TRUE,
                                        stderr = TRUE) %>%
                                stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
                                magrittr::extract(!is.na(.)) %>%
                                stringr::str_replace_all(pattern = "\\.",
                                                         replacement = "\\\\.") %>%
                                paste0("^",  .) %>%
                                stringr::str_subset(string =
                                                      binman::list_versions(appname = "chromedriver") %>%
                                                      dplyr::last()) %>%
                                as.numeric_version() %>%
                                max() %>%
                                as.character())

remote_driver <- driver[["client"]] 
remote_driver$navigate("https://www.latlong.net/convert-address-to-lat-long.html")

