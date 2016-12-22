library(htmltools)

vizlisting <- function() {

  students <- sort(unique(coursedata$Nimi))
  
  tags$div(
    tags$ul(
      class="students",
      lapply(
        students,
        function(s){
          tags$li(
            tags$a(
              href=paste0(gsub("[ ,]","",s),".html"),
              s
            )
          )
        }
      )
    ))

}
 


