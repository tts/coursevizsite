library(dplyr)
library(tidyr)
library(ggplot2)
library(ggiraph)
library(readxl)

####################
#
# Determine OS
#
####################

# https://github.com/hadley/rappdirs/blob/master/R/utils.r#L1

get_os <- function() {
  if (.Platform$OS.type == "windows") { 
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac" 
  } else if (.Platform$OS.type == "unix") { 
    "unix"
  } else {
    stop("Unknown OS")
  }
}

os <- get_os()

##################
#
# Parameters
#
##################

colour_file <- "data/colors.xlsx"
data_file <- ifelse(os == "win", "data/CSExport.xlsx", "data/CSExport.csv")
recommendations_file <- "data/suositukset.xlsx"

kevat <- "Kevät"

remove_blank_courses <- TRUE
colour_course_default <- "cornsilk"
colour_rect_margin <- "gray10"
colour_absent <- "darkred"
colour_term_spring <- "darkolivegreen"
colour_term_autumn <- "darkgoldenrod1"
colour_term_spring_sum_points <- "white"
colour_term_autumn_sum_points <- "black"
colour_css_hoverfill <- "fill:orange; opacity: 0.5;"
label_substring_length <- 35
term_height_base <- -0.30
term_label_x_offset <- 1
term_label_y <- -0.15
y_axis_ticks <- c(1,2,"3 (hyv, tt)",4,"5 (ht, et, kh)")
y_axis_empty_ticks <- c("","","","") # first is 0
y_axis_nr_empty_ticks_above_zero <- 3
dark_colors <- c("darkblue", "blue1") # white font for these
y_axis_max <- 3
width_absent <- 10.1
width_zeropoint <- 2.1
height_absent <- 0.2
font_size <- 6
y_axis_font_size <- 20
year_font_size <- 20
title_font_size <- 20
# Recommendation plot y axis
y_axis_fix_value <- 3



#######################################
#
# Functions for plotting and filtering
#
######################################

# Draws the plot that shows which courses the student has done
draw <- function(df) {
  
  # All SVG rect elements here have a CSS class attribute the name of which starts with the string cl_data_id_.
  # Rects have also given a data-id attribute, defined as the "uid" variable in the dataframes to be
  # plotted. The value of the data-id attribute is either a single course code, or the combined key of 
  # all possible course code variants. The former, if the course has not been done. The latter,
  # if it is done. 
  #
  # Whenever one of the bars is clicked, all bars with the same data-id are highlighted, if found. This is how
  # it happens:
  # 
  # The data-id attribute value of the clicked bar is stored in the "this_data_id" variable.
  # Then, rect elements are selected with a "starts with" attribute selector [=^] that matches to cl_data_id_,
  # and looped over. If, among the classes of the rect, there is none with the name "myclass" but the data-id value is the same
  # as the "this_data_id" variable value, the myclass is added - and its CSS style definitions are fired.
  #
  # If, on the other hand, there already is a "myclass" class (meaning the highlight is on), it is removed,
  # and the original color of the bar is returned.
  df$onclick <- sprintf("highlight(this); function highlight(e) {var this_data_id = e.getAttribute(\"data-id\"); var rect = document.querySelectorAll(\"rect[class^=cl_data_id_]\"); for (var i = 0; i < rect.length; i++) { if ( rect[i].classList.contains(\"myclass\")  ) { rect[i].classList.remove(\"myclass\"); } else if ( !rect[i].classList.contains(\"myclass\") && rect[i].getAttribute(\"data-id\")===this_data_id ) { rect[i].classList.add(\"myclass\"); } }}")
  
  p <- ggplot(df, aes(ymin = -0.20)) + 
    geom_rect_interactive(aes(xmin = wm,
                              xmax = w,
                              ymin = 0,
                              ymax = height,
                              tooltip = tooltip,
                              onclick = onclick,
                              data_id = uid),
                          fill = df$color,
                          colour = colour_rect_margin) +
    geom_rect(aes(xmin = wm, # Lukukausi
                  xmax = w,
                  ymin = term_height_base,
                  ymax = 0),
              fill = ifelse(df$Lukukausi == kevat, colour_term_spring, colour_term_autumn)) +
    geom_text(aes(x = w-term_label_x_offset, # Opintopisteet per lukukausi
                  y = term_label_y,
                  label = termsum),
              size = font_size,
              colour=ifelse(df$Lukukausi == kevat, colour_term_spring_sum_points, colour_term_autumn_sum_points)) +
    geom_text(aes(x = wt, # Opintopisteet
                  y = height + 0.1,
                  label = ifelse(width != width_absent, width, "")),
              #   ifelse(Kurssi_koodi == 'P' || Kurssi_koodi == 'Zero', "", 0))),
              size = font_size) +
    geom_text(aes(x = wt, # Kurssikoodi ja nimi
                  y = height * 0.5,
                  label = ifelse(Kurssi_koodi != "P" & height > 2 , paste0(Kurssi_koodi, " ",Kurssi_nimi), 
                                 ifelse(Kurssi_koodi != "P" & height == 2, Kurssi_koodi,
                                        ifelse(Kurssi_koodi != "P" & height <= 1, "", 
                                               ""))),
                  angle = 90),
              size = font_size,
              colour = ifelse(df$color %in% dark_colors, 'white', 'black')) +
    theme_light() +
    theme(legend.position="none",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=y_axis_font_size)) + 
    labs(y = "") +
    scale_y_continuous(breaks = seq(0,
                                    y_axis_nr_empty_ticks_above_zero+length(y_axis_ticks),
                                    by=1),
                       labels = c(y_axis_empty_ticks, y_axis_ticks))  +
    ggtitle(paste0(df$Opiskelijanumero, " ",  df$Nimi[1])) +
    theme(plot.title = element_text(size = title_font_size))
  
  return(p)
  
}

# Draws the plot that shows the recommended order of courses
draw_order <- function(df) {
  
  df$onclick <- sprintf("highlight(this); function highlight(e) {var this_data_id = e.getAttribute(\"data-id\"); var rect = document.querySelectorAll(\"rect[class^=cl_data_id_]\"); for (var i = 0; i < rect.length; i++) { if ( rect[i].classList.contains(\"myclass\")  ) { rect[i].classList.remove(\"myclass\"); } else if ( !rect[i].classList.contains(\"myclass\") && rect[i].getAttribute(\"data-id\")===this_data_id ) { rect[i].classList.add(\"myclass\"); } }}")
  
  # You'll get "Warning: Ignoring unknown aesthetics: tooltip, onclick, data_id" which is probably due to
  # https://github.com/tidyverse/ggplot2/issues/1909
  p <- ggplot(df, aes(ymin = -0.20)) + 
    geom_rect_interactive(aes(xmin = wm,
                              xmax = w,
                              ymin = 0,
                              ymax = y_axis_fix_value,
                              tooltip = ttip,
                              onclick = onclick,
                              data_id = uid),
                          fill = df$Vari,
                          colour = colour_rect_margin) +
    geom_rect(aes(xmin = wm, # Lukukausi
                  xmax = w,
                  ymin = term_height_base,
                  ymax = 0),
              fill = ifelse(df$Lukukausi == "Syksy", colour_term_autumn, colour_term_spring)) +
    geom_text(aes(x = wt, # Opintopisteet
                  y = y_axis_fix_value + 0.1,
                  label = ifelse(!width %in% c(1.1,5.1), width, "")), # Kyps etc
              size = font_size) +
    geom_text(aes(x = wt, # Kurssikoodi ja nimi
                  y = y_axis_fix_value * 0.5,
                  # label = Kurssikoodi,
                  label = substr(Kurssinimi, 1, label_substring_length),
                  angle = 90),
              size = font_size,
              colour = ifelse(df$Vari %in% dark_colors, 'white', 'black')) +
    theme_light() +
    theme(legend.position="none",
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank())
  
  return(p)
  
}


# Filters data by student, checks if there are terms with no "P" (=poissaolo) and no points (>"Zero"),
# and calculates bar dimensions and x axis values for the plots
filterdata <- function(df, p) {
  
  courses_of_student <- df %>%
    filter(Nimi == p)
  
  # Zero terms processing
  # All combinations of term+schoolyear
  min_year <- min(courses_of_student$schoolyear)
  max_year <- max(courses_of_student$schoolyear)
  schoolyear <- rep(seq(min_year, max_year, 1), each=2)
  Lukukausi <- rep(c("Syksy", kevat), length.out=length(schoolyear))
  year_term <- data.frame(schoolyear, Lukukausi, stringsAsFactors = F)
  
  courses_join_year_term <-  merge(courses_of_student, year_term, by = c("Lukukausi", "schoolyear"), all.y = T)
  
  # These terms are not present
  zero_terms <- courses_join_year_term[is.na(courses_join_year_term$Vuosi),]
  
  # Fill in data
  zero_terms_df <- zero_terms %>% 
    mutate(Opiskelijanumero = courses_of_student$Opiskelijanumero[1]) %>% 
    mutate(Nimi = courses_of_student$Nimi[1]) %>% 
    mutate(Kurssi_koodi = "Zero") %>% 
    mutate(Kurssi_nimi = "Ei suorituksia, ei poissaoloa") %>% 
    mutate(Opintopisteet = 0) %>% 
    mutate(Vuosi = ifelse(Lukukausi == kevat, schoolyear+1, schoolyear)) %>% 
    mutate(Vari = "black") %>% 
    mutate(schoolyearrange = ifelse(Lukukausi == kevat, paste0(Vuosi-1, "-", Vuosi), paste0(Vuosi,"-",Vuosi+1)))  %>% 
    mutate(color = "black") %>% 
    mutate(height = height_absent) %>% 
    mutate(width = width_absent) %>% 
    # Hack, but needs to be done in Windows as long as ggiraph does not produce correct encoding
    mutate(tooltip = ifelse(os == "win",
                            paste0(schoolyearrange, " ", ifelse(Lukukausi=="Syksy", "Syksy", "Kevat"), ": Ei suorituksia"),
                            ifelse(os != "win", paste0(schoolyearrange, " ", Lukukausi, ": Ei suorituksia")))) %>% 
    select(Opiskelijanumero,Nimi,Kurssi_koodi,Kurssi_nimi,Opintopisteet,Lukukausi,Vuosi,Vari,schoolyear,schoolyearrange,color,height,width,tooltip)
  
  # and add to rest of data
  courses_of_student_w_zeros <- rbind(courses_of_student, zero_terms_df)
  
  # Arrange data by time
  courses_of_student <- courses_of_student_w_zeros %>% 
    arrange(schoolyear, desc(Lukukausi)) 
  
  # Calculate x axis
  courses_grouped <- courses_of_student %>%
    group_by(schoolyear) %>%
    mutate(w = cumsum(width)) %>%
    mutate(wm = w - width) %>%
    mutate(wt = wm + (w - wm)/2) %>% 
    group_by(schoolyear, Lukukausi) %>% 
    mutate(termsum = ifelse(row_number()==n(), as.character(sum(Opintopisteet)), "")) # Save the sum(Opintopisteet) in the last course on that term
  
  return(courses_grouped)
}

# Labels for schoolyears 
labels <- function(variable, value){
  if (variable == "schoolyear"){
    value[value == 1980] <- "1980-1981"
    value[value == 1981] <- "1981-1982"
    value[value == 1982] <- "1982-1983"
    value[value == 1983] <- "1983-1984"
    value[value == 1984] <- "1984-1985"
    value[value == 1985] <- "1985-1986"
    value[value == 1986] <- '1986-1987'
    value[value == 1987] <- "1987-1988"
    value[value == 1988] <- "1988-1989"
    value[value == 1989] <- "1989-1990"
    value[value == 1990] <- "1990-1991"
    value[value == 1991] <- "1991-1992"
    value[value == 1992] <- "1992-1993"
    value[value == 1993] <- "1993-1994"
    value[value == 1994] <- '1994-1995'
    value[value == 1995] <- "1995-1996"
    value[value == 1996] <- "1996-1997"
    value[value == 1997] <- "1997-1998"
    value[value == 1998] <- "1998-1999"
    value[value == 1999] <- "1999-2000"
    value[value == 2000] <- "2000-2001"
    value[value == 2001] <- "2001-2002"
    value[value == 2002] <- '2002-2003'
    value[value == 2003] <- "2003-2004"
    value[value == 2004] <- "2004-2005"
    value[value == 2005] <- "2005-2006"
    value[value == 2006] <- "2006-2007"
    value[value == 2007] <- "2007-2008"
    value[value == 2008] <- "2008-2009"
    value[value == 2009] <- "2009-2010"
    value[value == 2010] <- '2010-2011'
    value[value == 2011] <- "2011-2012"
    value[value == 2012] <- "2012-2013"
    value[value == 2013] <- "2013-2014"
    value[value == 2014] <- "2014-2015"
    value[value == 2015] <- "2015-2016"
    value[value == 2016] <- "2016-2017"
    value[value == 2017] <- "2017-2018"
    value[value == 2018] <- "2018-2019"
    value[value == 2019] <- "2019-2020"
  }
  return(value)
}



################################
#
# Main process begins here.
#
#
#
# 1. Import colors from Excel
#
################################

colordata <- read_excel(colour_file, sheet = 1)

cColors <-
  with(colordata,
       data.frame(course = Kurssikoodi,
                  color = I(Vari)))

#######################################
#
# 2. Import course data, and add color
#
######################################

if( os == "win") {
  data <- openxlsx::read.xlsx(data_file, sheet = "CSExport")
} else {
  data <- read.csv(data_file, stringsAsFactors = F, fileEncoding  = "UTF-8-BOM")
}
               

# Sample
#
data <- data[1:50,]

# Rename columns to be the same regardless of the OS and import file type
names(data) <- c("Opiskelijanumero", "Nimi", "Kurssi_koodi", "Kurssi_nimi", "Arvosana", "Opintopisteet", "Lukukausi", "Vuosi")

# Similarly, change NA's to empty string
data$Kurssi_nimi[is.na(data$Kurssi_nimi)] <- ""
data$Arvosana[is.na(data$Arvosana)] <- ""

# Remove single quotes from names
data$Nimi <- gsub("'", "", data$Nimi)
data$Kurssi_nimi <- gsub("'", "", data$Kurssi_nimi)

# Remove courses with no code nore name
if(remove_blank_courses){
  data <- data[data$Kurssi_koodi != "",]
}

# Add color
data_joined <- left_join(data, colordata, by=c("Kurssi_koodi"= "Kurssikoodi"))

# Define schoolyear ranges, bar colors/heights/widths, and tooltip text
coursedata <- data_joined %>%
  mutate(schoolyear = ifelse(Lukukausi == kevat, Vuosi-1, Vuosi)) %>% # Kev?t belongs to the school year that started the previous fall
  mutate(schoolyearrange = ifelse(Lukukausi == kevat, paste0(Vuosi-1, "-", Vuosi), paste0(Vuosi,"-",Vuosi+1))) %>%  # for the tooltip
  mutate(color = ifelse(is.na(Vari) & Kurssi_koodi != 'P', colour_course_default, 
                        ifelse(is.na(Vari) & Kurssi_koodi == 'P', colour_absent, 
                               cColors$color[match(Kurssi_koodi, cColors$course)]))) %>% 
  mutate(height = sapply(Arvosana, function(x){
    as.numeric(ifelse(x == 'hyv', 3+y_axis_max  , # hyv?ksytty
                      ifelse(x == 'kh', 5+y_axis_max, # kh esim. TFM.kand
                             ifelse(x == 'tt', 3+y_axis_max, # tyydytt?v?t tiedot
                                    ifelse(x == 'ht', 5+y_axis_max,  # hyv?t tiedot
                                           ifelse(x == 'et', 5+y_axis_max, # erinomaiset tiedot
                                                  ifelse(x == '', height_absent, as.numeric(x)+y_axis_max)))))))
  })) %>%
  mutate(width = sapply(Opintopisteet, function(x){
    ifelse(x == 0.0, width_zeropoint, x)
  })) %>%
  mutate(width2 = ifelse(is.na(width), width_absent, width)) %>%
  rename(oldwith = width) %>%
  rename(width = width2) %>%
  # Hack, but needs to be done in Windows as long as ggiraph does not produce correct encoding
  mutate(tooltip = ifelse(os == "win" & Kurssi_nimi != "", paste0(schoolyearrange, " ", ifelse(Lukukausi=="Syksy", "Syksy", "Kevat"), ": ", Kurssi_nimi, " (", Arvosana, ")"),
                          ifelse(os == "win" & Kurssi_nimi == "", paste0(schoolyearrange, " ", ifelse(Lukukausi=="Syksy", "Syksy", "Kevat"), ": Poissaolo"),
                                 ifelse(os != "win" & Kurssi_nimi != "", paste0(schoolyearrange, " ", Lukukausi, ": ", Kurssi_nimi, " (", Arvosana, ")"),
                                         ifelse(os != "win" & Kurssi_nimi == "", paste0(schoolyearrange, " ", Lukukausi, ": Poissaolo"), "NA"))))) %>% 
  select(-Arvosana, -oldwith, -Koodi)

################################################
#
# 2.1 Import recommendations data, and add color
#
################################################

recomm <- read_excel(recommendations_file, sheet = 1)
# Name empty columns
names(recomm)[6:ncol(recomm)] <- paste0("col", seq(from=6, to=ncol(recomm), by=1))
# Delete whitespace
recomm$Lukukausi <- gsub("\\s","", recomm$Lukukausi)
# Join with color data
recomm_joined <- left_join(recomm, colordata, by=c("Kurssikoodi"= "Kurssikoodi"))

# Construct a combined key from all course code variations
recomm_united <- tidyr::unite_(recomm_joined, "combined", colnames(recomm_joined)[5:(ncol(recomm_joined)-2)])
recomm_united$combined <- gsub("_NA","",recomm_united$combined)

####################################################
#
# 3. Render course data by student as a HTML file 
#
####################################################

sapply(unique(coursedata$Nimi), function(x) {
  
  Nimi <- x
  
  # Other output formats are possible, too. Change to something like
  ## sapply(c("pdf", "html", "doc"), function(y) {...}
  
  
  sapply("html", function(y) {
    
    rmarkdown::render(paste0("_template_", y, ".Rmd"),
                      output_file = paste0(gsub("[ ,]","", Nimi), ".", y),
                      params = list(
                        person = Nimi))
  })
  
})


