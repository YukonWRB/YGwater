# Download data from EQWin
master <- YGwater::eq_fetch(EQcode = "EG",
                            stationIDs = c("MW24-01S", "MW24-01D", "MW24-02S", "MW24-02D", "MW24-03S", "MW24-03D", "MW24-04", "MW24-05", "MW24-06S", "MW24-06D", "MW24-07S", "MW24-07D", "MW24-08S", "MW24-08D", "MW24-09", "MW24-10", "MW24-11", "MW24-12"),
                            paramIDs = c("CN-WAD", "CN-SAD", "CN-Fre", "CN-CNS", "CN-CNO", "N-NO3", "N-NO2", "N-NH4"),
                            dates = c("1990-01-01", "2024-10-10"),
                            BD = 2,
                            apply_standards = FALSE,
                            path = "G:\\water\\Data\\Databases_virtual_machines\\databases\\EQWinDB\\WaterResources.mdb")

# Create folder structure, one folder per date when plots are generated and one folder for each parameter
folder <- paste0("G:\\water\\ENV Assessment\\2-Quartz\\Eagle-Gold\\4-Post-license\\2024-ISSUE-Heap-Slide\\Data-Analysis\\Rplots\\MW24-Series-Wells_", Sys.Date())

if (!exists(folder)) {
  dir.create(paste0("G:\\water\\ENV Assessment\\2-Quartz\\Eagle-Gold\\4-Post-license\\2024-ISSUE-Heap-Slide\\Data-Analysis\\Rplots\\MW24-Series-Wells_", Sys.Date()))
}
for(i in colnames(master[[1]][["stndata"]])[grepl("\\(.*\\)", colnames(master[[1]][["stndata"]]))]){
  dir.create(paste0(folder, "\\", sub(" .*", "", i)))
}

# Generate plots and save to wd
for(i in names(master)) {
  df <- master[[i]][["stndata"]] %>%
    tidyr::pivot_longer(cols = grep("\\(.*\\)", colnames(master[[1]][["stndata"]])))
  for (j in unique(df$name)) {
    param <- sub(" .*", "", j)
    plotdf <- df %>%
      dplyr::filter(stringr::str_detect(name, param)) %>%
      dplyr::mutate(.data$CollectDateTime = as.Date(.data$CollectDateTime, format = "%Y-%m-%d"))
    p <- ggplot2::ggplot() +
      ggplot2::geom_line(data = plotdf, mapping = ggplot2::aes(x = CollectDateTime, y = value), colour = "orange", linewidth = 0.5) +
      ggplot2::geom_point(data = plotdf, mapping = ggplot2::aes(x = CollectDateTime, y = value), colour = "orange", size = 1) +
      ggplot2::labs(x = "Sample Date",
           y =" Concentration (mg/L)") +
      ggplot2::scale_x_date(date_labels = "%y-%m-%d", date_breaks = "7 days") +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(paste(i, j)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, vjust = 1, hjust = 1))
    p
      
    ggplot2::ggsave(plot = p, filename = paste0(folder, "\\", param, "\\", "(EG)", i, ".png"), width = 8, height = 5, units = "cm")
  }
}
