student_names <- readr::read_csv(here::here("csv_data/student_names.csv"))

continue <- 0

while(continue != 'stop'){
  cat(sample(student_names$name, size = 1))
  continue <- readline()
}

