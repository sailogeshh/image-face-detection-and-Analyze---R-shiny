# ==== Load required libraries ====
library(tidyverse)
library(httr)
# ==== Microsoft's Azure Face API ====
end.point <- "https://westcentralus.api.cognitive.microsoft.com/face/v1.0/detect"
key1 <- "9b390663867a48cfb30ea59e07f30fb6"
#To get things going, lets check that the API and key works. We’ll send a simple image (of me) to the API and see what comes out.

sample.img.simple <- POST(url = end.point,
                          add_headers(.headers = c("Ocp-Apim-Subscription-Key" = key1)),
                          body = '{"url":"https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQ9IL3_LYsKw20lBASSeNuqfTeTkGhRxqtng26HoyMDLjHm8RBo"}',
                          query = list(returnFaceAttributes = "emotion"),
                          accept_json())
as_tibble(content(sample.img.simple)[[1]]$faceAttributes$emotion) %>% t()
FaceAttributes = "emotion,age,gender,hair,makeup,accessories"
magick::image_read("http://www.sarid-ins.co.il/files/TheTeam/Adi_Sarid.jpg")
