# purrr examples

# Name gh_users with the names of the users
gh_users_named <- gh_users %>% 
  set_names(map_chr(gh_users, "name"))

# Check gh_repos structure
str(gh_repos)

# Name gh_repos with the names of the repo owner 
gh_repos_named <- gh_repos %>% 
  map_chr(~ .[[1]]$owner$login) %>% 
  set_names(name, .)


# Map over gh_repos to generate numeric output
map(gh_repos, 
    ~map_dbl(gh_repos, 
             ~.x[["size"]])) %>%
  # Grab the largest element
  map(~max())



-------------
  # Map over gh_repos to generate numeric output
  map(gh_repos, 
      ~map_dbl(.x, 
               ~.[["size"]])) %>%
  # Grab the largest element
  map(~max(.x))


gh_repos %>% map( ~map_dbl(.x, "size")) %>% map( ~max(.x))

----------
  # Turn data into correct dataframe format
  film_by_character <- tibble(filmtitle = map_chr(sw_films, "title")) %>%
  mutate(filmtitle, characters = map(sw_films, "characters")) %>%
  unnest()

# Pull out elements from sw_people
sw_characters <- map_df(sw_people, `[`, c("height", "mass", "name", "url"))

# Join the two new objects
character_data <- inner_join(film_by_character, sw_characters, by = c("characters" = "url")) %>%
  # Make sure the columns are numbers
  mutate(height = as.numeric(height), mass = as.numeric(mass))

# Plot the heights, faceted by film title
ggplot(character_data, aes(x = height)) +
  geom_histogram(stat = "count") +
  facet_wrap(~ filmtitle)