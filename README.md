coursevizsite
=============

Coursevizsite plots barcharts from student course data as a HTML site. If you are looking for a single page solution, see [courseviz](https://github.com/tts/courseviz)

### Prerequisites

* basic **course** data in CSV from reporting services: student number, student name, course code, course name, year, term, study points, score
* an Excel spreadsheet to **color** courses by type: course code, name of the course type, and color name for that type (TODO: matching step)
* an Excel spreadsheet with the **recommended order** of taking courses: year (1, 2 etc), term, course name, study points, course codes (alternatives in successive cells in the same row)
* about.Rmd
* footer.html
* header.html
* styles.css
* _site.yml

### How to use

If you are working with RStudio, open up a new project, save data files in `data` subdirectory, and run `rmarkdown::render_site()`. The HTML files are outputted in the subdirectory defined in `_site.yml`, one file per student, along with index.html etc.

### Credits

Thanks to RStudio for [R Markdown Websites](http://rmarkdown.rstudio.com/rmarkdown_websites.html), Yihui Xie et al. for [knitr](https://github.com/yihui/knitr), Hadley Wickham et al. for [dplyr](https://github.com/hadley/dplyr), [tidyr](https://github.com/tidyverse/tidyr), [ggplot2](https://github.com/tidyverse/ggplot2) and [readxl](https://github.com/hadley/readxl), David Gohel for [ggiraph](https://github.com/davidgohel/ggiraph), and the whole R community for all the rest. 

For a useful example of how to put this all together, thanks to [bookdown](https://github.com/rstudio/bookdown).
