parks_n <- readRDS("../data/parks_n.rds")
parks_20 <- parks_n %>%
  filter(YEAR == "2022")|>
  arrange(desc(TOTAL))|>
  slice_head(n=30)

park_boundary_m <- readRDS('../data/park_boundary_m.rds')
park_fire_incidents_r <- readRDS('../data/park_fire_incidents.rds')

park_boundary_m|>
  filter(UNIT_CODE == "GRSM")

park_fire_incidents_r %>%
  filter(UNIT_CODE == "GRSM", parsed_date >= 2019-01-01)|>
  ggplot()+
  geom_vline(aes(xintercept = parsed_date),
             linetype = "dashed", color = "red"
  )

p<- ggplot(parks_n|>
             filter(PARKCODE == "GRSM") |>
             select(-X, -PARKCODE, -TOTAL)|>
             gather(key = "Month", value = "Value", -YEAR, -Name)|>
             mutate(MonthNumeric = month_mapping[Month])|>
             mutate(Date = make_date(YEAR, MonthNumeric))|>
             arrange(Date)|>
             filter(Date >= 2019-01-01), aes(x = Date, y = Value))+
  geom_line()

park_fire_incidents_r %>%
  filter(UNIT_CODE == "GRSM", parsed_date >= 2019-01-01)
         

park_fire_incidents_r %>%
  filter(UNIT_CODE == "GRSM", parsed_date >= 2019-01-01)|>
  pull(parsed_date)
