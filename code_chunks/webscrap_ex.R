# links ------------

# Web scraping notes
https://stackoverflow.com/a/41321597/7023826 
https://ropensci.org/tutorials/rselenium_tutorial/ 
  http://p2c.greensboro-nc.gov/dailybulletin.aspx 

# ---------------
[Alt](./file.pdf){width=100% height=400}  
[thisworksheet](attach/wb.xlsx)

# -----------
addLogo(m, 'https://www.conehealth.com/app/files/public/9889/CH-Logo.svg',
        position = "bottomleft",
        offset.x = 5,
        offset.y = 10,
        width = 150,
        height = 200)

# -----------
  active_files <- fs::Dir_info("that directory")



# ---------
https://maps3.arcgisonline.com/arcgis/rest/services
https://services.arcgis.com/iFBq2AW9XO0jYYF7/ArcGIS/rest/services



# webscraping example --------------

# Create a possibly() version of read_lines()
possible_read <- possibly(read_lines, otherwise = 404)

# Map this function on urls, pipe it into set_names()
res <- map(urls, possible_read) %>% set_names(urls)

# Paste each element of the list 
res_pasted <- paste(res, sep = "", collapse = " ")

# Keep only the elements which are equal to 404
map(res_pasted, ~ .x == 404)

-------------
  # Complete the function definition
  url_tester <- function(url_list, type = c("result", "error")) {
    type <- match.arg(type)
    url_list %>%
      # Apply safe_read to each URL
      map(safe_read) %>%
      # Set the names to the URLs
      set_names(url_list) %>%
      # Transpose into a list of $result and $error
      transpose()  %>%
      # Pluck the type element
      pluck(type) 
  }

# Try this function on the urls object
url_tester(urls, type = "error") 

--------------
  url_tester <- function(url_list){
    url_list %>%
      # Map a version of GET() that would otherwise return NULL 
      map( possibly(GET, otherwise = NULL) ) %>%
      # Set the names of the result
      set_names( urls ) %>%
      # Remove the NULL
      compact() %>%
      # Extract all the "status_code" elements
      map("status_code")
  }

# Try this function on the urls object
url_tester(urls)


--------
  # Negate the %in% function 
  `%not_in%` <- negate(`%in%`)


