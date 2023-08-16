#' get_soil_texture
#' @author Vincyane Badouard
#' 
#' @param data (data.frame) with `Clay %`,`Sand %` and`Silt %` columns
#'
#' @return The initial data.frame with a computed `Texture_Jamagne` column.
#' 
#' @details
#' Texture selon triangle de Jamagne (1977) (Texture_Jamagne): 
#' S : `Sand %`>80 & `Clay %`<10 & `Silt %`<20
#' SL : `Sand %`>55  & `Sand %`<80 & `Silt %`>10 & `Silt %`<45 &`Clay %`<15
#' SA :`Clay %`>10 & `Clay %`<25 & `Sand %`>55 & `Sand %`<90 &`Silt %`<35
#' LLS : `Clay %`<5 & `Sand %`>15 & `Sand %`<55 & `Silt %`>35 & `Silt %`<85
#' LL : `Clay %`<5 & `Sand %`<15 & `Silt %`>75
#' LS : `Clay %`>5 & `Clay %`<15 & `Sand %`>35 & `Sand %`<55 & `Silt %`>25 &
#' `Silt %`<55
#' LMS : `Clay %`>5 & `Clay %`<15 &`Sand %`>15 & `Sand %`<35 & `Silt %`>45 &
#' `Silt %`<75
#' LM : `Clay %`>5 & `Clay %`<15 & `Sand %`<15 & `Silt %`>65 & `Silt %`<95
#' LSA : `Clay %`>15 & `Clay %`<30 & `Sand %`>35 & `Sand %`<55 & `Silt %`>20 &
#' `Silt %`<45
#' LAS : `Clay %`>15 & `Clay %`<30 & `Sand %`>15 & `Sand %`<35 & `Silt %`>35 &
#' `Silt %`<65
#' LA : `Clay %`>15 & `Clay %`<30 & `Sand %`<15 & `Silt %`>55 & `Silt %`<85
#' AS : `Clay %`> 25 & `Clay %`< 45 & `Sand %` > 45 & `Sand %`< 75 & `Silt %`<25
#' A : `Clay %`> 30 & `Clay %`< 45 & `Sand %`>20 & `Sand %` <45 & `Silt %`>10 &
#' `Silt %`<50
#' AL : `Clay %`> 30 & `Clay %`< 45 & `Sand %`< 20 & `Silt %`>10 & `Silt %`<50
#' Alo : `Clay %`> 45 & `Sand %`<55 & `Silt %`<55
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' Rslt <- get_soil_texture(data)
#' }
#' 
get_soil_texture <- function(data)
{
  data <- data %>% 
    mutate(Texture_Jamagne = case_when(
      `Sand %`>80 & `Clay %`<10 & `Silt %`<20 ~ "S",
      `Sand %`>55 & `Sand %`<80 & `Silt %`>10 & `Silt %`<45 &`Clay %`<15 ~ "SL",
      `Clay %`>10 & `Clay %`<25 & `Sand %`>55 & `Sand %`<90 &`Silt %`<35 ~ "SA",
      `Clay %`<5 & `Sand %`>15 & `Sand %`<55 & `Silt %`>35 & `Silt %`<85 ~ "LLS",
      `Clay %`<5 & `Sand %`<15 & `Silt %`>75 ~ "LL",
      `Clay %`>5 & `Clay %`<15 & `Sand %`>35 & `Sand %`<55 & `Silt %`>25 & `Silt %`<55 ~ "LS",
      `Clay %`>5 & `Clay %`<15 &`Sand %`>15 & `Sand %`<35 & `Silt %`>45 & `Silt %`<75 ~ "LMS",
      `Clay %`>5 & `Clay %`<15 & `Sand %`<15 & `Silt %`>65 & `Silt %`<95 ~ "LM",
      `Clay %`>15 & `Clay %`<30 & `Sand %`>35 & `Sand %`<55 & `Silt %`>20 & `Silt %`<45 ~ "LSA",
      `Clay %`>15 & `Clay %`<30 & `Sand %`>15 & `Sand %`<35 & `Silt %`>35 & `Silt %`<65 ~ "LAS",
      `Clay %`>15 & `Clay %`<30 & `Sand %`<15 & `Silt %`>55 & `Silt %`<85 ~ "LA",
      `Clay %`>25 & `Clay %`<45 & `Sand %`>45 & `Sand %`<75 & `Silt %`<25 ~ "AS",
      `Clay %`>30 & `Clay %`<45 & `Sand %`>20 & `Sand %`<45 & `Silt %`>10 & `Silt %`<50 ~ "A",
      `Clay %`>30 & `Clay %`<45 & `Sand %`<20 & `Silt %`>10 & `Silt %`<50 ~ "AL",
      `Clay %`>45 & `Sand %`<55 & `Silt %`<55 ~ "Alo"
    ))
  
  return(data)
}