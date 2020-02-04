library(tidyverse)


# Statistisches Bundesamt -------------------------------------------------

input <- readr::read_csv2("day_two/data/12211-0042.csv", 
                          skip = 6, 
                          locale = readr::locale(encoding = "latin1"))
df <- input %>% 
  dplyr::rename(year = X1, gender = X2, state = X3, 
                total_respondents = Insgesamt,
                still_in_school = `Noch in schulischer Ausbildung`,
                degree_hauptschule = `Haupt- (Volks-)schulabschluss`,
                degree_polytechnical = `Abschluss der polytechnischen Oberschule`,
                degree_realschule = `Realschule oder gleichwertiger Abschluss`,
                degree_abitur = `Fachhochschul- oder Hochschulreife`,
                no_information = `Ohne Angabe zur Art des Abschlusses`,
                no_degree = `Ohne allgemeinen Schulabschluss`) %>%
  dplyr::filter(gender %in% c("männlich", "weiblich")) %>%
  tidyr::gather(education, value, 
                still_in_school:no_degree) %>% 
  dplyr::mutate(year = as.numeric(year),
                value = as.numeric(value),
                education = factor(education, ordered = TRUE,
                         levels = c("degree_abitur",
                                    "degree_realschule",
                                    "degree_polytechnical",
                                    "degree_hauptschule",
                                    "still_in_school",
                                    "no_information",
                                    "no_degree")))
  

# unicef ------------------------------------------------------------------
library(hrbrthemes)
input <- read_csv("day_two/data/UNIGME Rates & Deaths_Under5.csv",
                            skip = 10)

# Tidy data
df <- input %>% 
  dplyr::filter(!is.na(`ISO Code`)) %>% 
  tidyr::gather(variable, value, -c(`ISO Code`:`Uncertainty bounds*`)) %>% 
  tidyr::separate(variable, c("type", "year"),-4) %>% 
  dplyr::filter(!is.na(value)) %>% 
  dplyr::mutate(year = as.numeric(year),
                value = as.numeric(value))

# Plot trends
df %>% 
  dplyr::group_by(type, `Uncertainty bounds*`, year) %>% 
  dplyr::summarise(value = mean(value, na.rm=TRUE)) %>% 
  tidyr::spread(`Uncertainty bounds*`, value) %>% 
  ggplot(aes(x=year, y = Median, ymin = Lower, ymax = Upper)) +
  geom_ribbon(alpha = 0.2) +  
  geom_line() +
  theme_ipsum() +
  labs(x = "", y = "") +
  facet_wrap(~type, scales = "free")
  
