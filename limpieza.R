library(dplyr)
library(stringr)
library(lubridate)
library(stringi)

Lynx  <- read.csv("Lynx.csv",              sep = "\t", quote = "")
Quebr <- read.csv("Quebrantahuesos.csv",   sep = "\t", quote = "")

lynx_clean <- Lynx |> distinct(gbifID, .keep_all = TRUE)

lynx_clean <- lynx_clean[, c(
  "decimalLatitude", "decimalLongitude", "eventDate",
  "year", "month", "countryCode",
  "individualCount", "stateProvince", "locality"
)]

nrow(lynx_clean)

lynx_clean <- lynx_clean[
  !is.na(lynx_clean$decimalLatitude) &
    !is.na(lynx_clean$decimalLongitude), ]

nrow(lynx_clean)


lynx_clean <- lynx_clean %>%
  mutate(
    eventDate = if_else(eventDate == "1955-02", "1955-02-01", eventDate),
    eventDate = if_else(eventDate == "1988-11", "1988-11-01", eventDate)
  )


lynx_clean2 <- lynx_clean %>%
  mutate(
    eventDate = as.character(eventDate),
    eventDate = stri_replace_all_regex(eventDate, "\\p{Pd}", "-"),
    eventDate = na_if(str_trim(eventDate), "")
  ) %>%
  mutate(
    tipo_fecha = case_when(
      str_detect(eventDate, "^\\d{4}$")         ~ "solo_año",
      str_detect(eventDate, "^\\d{4}-\\d{2}$")  ~ "año_mes",
      str_detect(eventDate, "^\\d{4}/\\d{4}$")  ~ "intervalo_años",
      str_detect(eventDate, "Z$")                ~ "iso8601_Z",
      str_detect(eventDate, "T\\d{2}:\\d{2}")    ~ "iso8601",
      str_detect(eventDate, "^\\d{4}-\\d{2}-\\d{2}$") ~ "fecha_simple",
      str_detect(eventDate, "/")                 ~ "intervalo",
      TRUE                                       ~ "otro"
    )
  ) %>%
  mutate(
    fecha_parseada = case_when(
      tipo_fecha == "solo_año"      ~ ymd(paste0(eventDate, "-01-01")),
      tipo_fecha == "año_mes"       ~ ymd(paste0(eventDate, "-01")),
      tipo_fecha == "intervalo_años"~ ymd(paste0(str_extract(eventDate, "^\\d{4}"), "-01-01")),
      tipo_fecha %in% c("iso8601", "iso8601_Z") ~ as.Date(coalesce(
        ymd_hms(eventDate, tz = "UTC", quiet = TRUE),
        ymd_hm(eventDate,  tz = "UTC", quiet = TRUE)
      )),
      tipo_fecha == "fecha_simple"  ~ ymd(eventDate),
      tipo_fecha == "intervalo"     ~ as.Date(coalesce(
        ymd_hms(str_extract(eventDate, "^[^/]+"), quiet = TRUE),
        ymd_hm(str_extract(eventDate,  "^[^/]+"), quiet = TRUE),
        ymd(str_extract(eventDate,     "^[^/]+"), quiet = TRUE)
      )),
      TRUE ~ NA_Date_
    )
  ) %>%
  mutate(
    año = year(fecha_parseada),
    mes = month(fecha_parseada),
    dia = day(fecha_parseada)
  )

# Vérifier qu'il n'y a pas de dates non parsées
lynx_clean2 %>% filter(is.na(fecha_parseada)) %>% count()

# Filtrer
lynx_clean2 <- lynx_clean2[!is.na(lynx_clean2$año) & lynx_clean2$año >= 1900, ]

nrow(lynx_clean2)

quebr_clean <- Quebr |> distinct(gbifID, .keep_all = TRUE)

quebr_clean <- quebr_clean[, c(
  "decimalLatitude", "decimalLongitude", "eventDate",
  "year", "month", "countryCode",
  "individualCount", "stateProvince", "locality"
)]

quebr_clean <- quebr_clean[
  !is.na(quebr_clean$decimalLatitude) &
    !is.na(quebr_clean$decimalLongitude), ]

quebr_clean <- quebr_clean |>
  mutate(eventDate = na_if(str_trim(eventDate), "")) |>
  filter(!is.na(eventDate))

nrow(quebr_clean)

df <- quebr_clean %>%
  mutate(
    eventDate  = as.character(eventDate),
    eventDate  = stri_replace_all_regex(eventDate, "\\p{Pd}", "-"),
    eventDate  = na_if(str_trim(eventDate), "")
  ) %>%
  mutate(
    tipo_fecha = case_when(
      str_detect(eventDate, "^\\d{4}$")              ~ "solo_año",
      str_detect(eventDate, "^\\d{4}-\\d{2}$")       ~ "año_mes",
      str_detect(eventDate, "^\\d{4}/\\d{4}$")        ~ "intervalo_años",
      str_detect(eventDate, "Z$")                     ~ "iso8601_Z",
      str_detect(eventDate, "T\\d{2}:\\d{2}")         ~ "iso8601",
      str_detect(eventDate, "^\\d{4}-\\d{2}-\\d{2}$")~ "fecha_simple",
      str_detect(eventDate, "/")                      ~ "intervalo",
      TRUE                                            ~ "otro"
    )
  ) %>%
  mutate(fecha_parseada = NA_Date_)

# Parser
idx <- df$tipo_fecha == "solo_año"
df$fecha_parseada[idx] <- ymd(paste0(df$eventDate[idx], "-01-01"))

idx <- df$tipo_fecha == "año_mes"
df$fecha_parseada[idx] <- ymd(paste0(df$eventDate[idx], "-01"))

idx <- df$tipo_fecha == "intervalo_años"
df$fecha_parseada[idx] <- ymd(paste0(str_extract(df$eventDate[idx], "^\\d{4}"), "-01-01"))

idx <- df$tipo_fecha == "fecha_simple"
df$fecha_parseada[idx] <- ymd(df$eventDate[idx])

idx <- df$tipo_fecha %in% c("intervalo", "iso8601_Z", "iso8601")
df$fecha_parseada[idx] <- as.Date(parse_date_time(
  str_extract(df$eventDate[idx], "^[^/]+"),
  orders = c("ymd HMS", "ymd HM", "ymd"), tz = "UTC"
))

quebr_clean2 <- df %>%
  mutate(
    año = year(fecha_parseada),
    mes = month(fecha_parseada),
    dia = day(fecha_parseada)
  )

# Vérifier + filtrer
quebr_clean2 %>% filter(is.na(fecha_parseada)) %>% count()
quebr_clean2 <- quebr_clean2[!is.na(quebr_clean2$año) & quebr_clean2$año >= 1900, ]

nrow(quebr_clean2)


saveRDS(lynx_clean2,  "lynx_clean2.rds")
saveRDS(quebr_clean2, "quebr_clean2.rds")
saveRDS(lynx_clean,   "lynx_clean.rds")
saveRDS(quebr_clean,  "quebr_clean.rds")

message("✅ Sauvegarde terminée !")