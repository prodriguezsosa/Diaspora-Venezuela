
plot_wordcloud <- function(data, min_size){
  # keep relevant variables
  data <- data[,c("ID","Employer")]
  # upper case
  data[,"Employer":= stri_trans_totitle(`Employer`)]
  # replace spaces with _
  data[,"Employer":= gsub(" ", "_", `Employer`)]
  # count frquency
  data <- data[,.N, by = `Employer`] %>% set_colnames(c("word", "freq")) %>% data.frame(., stringsAsFactors = FALSE)
  wordcloud2(data, color = "random-dark", minRotation = -pi/6, maxRotation = -pi/6, minSize = min_size, rotateRatio = 1)
  #letterCloud(data, word = "V", color = "random-dark", minSize = min_size)
}
