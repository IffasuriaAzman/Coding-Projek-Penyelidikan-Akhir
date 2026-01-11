library(dplyr)
library(tidyr)
library(ggplot2)
library (ggthemes)

Trend_Negeri <- read.csv(file.choose())

Negeri_Long <- Trend_Negeri %>%
  pivot_longer(
    cols = c(`Jenayah.Kekerasan`, `Jenayah.Harta.Benda`),
    names_to = "Kategori_Jenayah",
    values_to = "Jumlah_Kes"
  )

ggplot(Negeri_Long, aes(x = Tahun, y = Jumlah_Kes,
                        color = Kategori_Jenayah,
                        group = Kategori_Jenayah)) +
  
  geom_line(linewidth = 1.5, alpha = 0.8) +
  geom_point(size = 3) +
  
  facet_wrap(~ Negeri, scales = "free_y", ncol = 4) +
  
  # --- BAHAGIAN YANG DIBETULKAN (Susunan Warna) ---
  scale_color_manual(values = c("#2D3E4F", "#DA291C"), # Tukar susunan warna
                     # Susunan abjad kategori: Jenayah.Harta.Benda, Jenayah.Kekerasan
                     # Warna 1: #2D3E4F (untuk Jenayah.Harta.Benda - yang lebih tinggi)
                     # Warna 2: #DA291C (untuk Jenayah.Kekerasan - yang lebih rendah)
                     labels = c("Jenayah Harta Benda", "Jenayah Kekerasan")) + # Tukar susunan label
  
  labs(
    title = "Trend dan Corak Jenayah dalam Negeri di Malaysia (2016–2023)",
    subtitle = "Jenayah Kekerasan vs Harta Benda",
    x = "Tahun",
    y = "Bilangan Kes",
    color = NULL 
  ) +
  
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0), 
    plot.subtitle = element_text(size = 14, hjust = 0, margin = margin(b = 15)),
    
    panel.background = element_rect(fill = "white", color = "grey90"),
    panel.grid.major = element_line(color = "grey85", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey60", linewidth = 0.8),
    
    strip.background = element_rect(fill = "#e0e0e0", color = "grey60"),
    strip.text = element_text(face = "bold", size = 11, color = "black"),
    
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )
