library(ggplot2)
library(tidyr)
library(dplyr)

Data_Jenayah <- data.frame(
  Tahun = c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023),
  Jenayah_Kekerasan = c(22327, 21366, 16902, 16489, 13279, 11495, 10348, 10453),
  Jenayah_Harta_Benda = c(90028, 77804, 71760, 66967, 52344, 41479, 40465, 41991)
)

# Kira jumlah kes jika perlu
Data_Jenayah <- Data_Jenayah %>%
  mutate(Jumlah = Jenayah_Kekerasan + Jenayah_Harta_Benda)

#long format
Long_Jenayah <- Data_Jenayah %>%
  pivot_longer(cols = c(Jenayah_Kekerasan, Jenayah_Harta_Benda),
               names_to = "Kategori_Jenayah",
               values_to = "Kes")

# Plot Trend Jenayah Corak Fokus Perbandingan
ggplot(Long_Jenayah, aes(x = Tahun, y = Kes,
                         color = Kategori_Jenayah,
                         group = Kategori_Jenayah)) +
  
  # 1. Tambah Kawasan (Area) di bawah garisan untuk menunjukkan skala secara visual
  geom_area(aes(fill = Kategori_Jenayah), # Guna fill di sini
            alpha = 0.15, # Warna lembut
            position = "identity") +
  
  # 2. Garisan tebal dan jelas
  geom_line(linewidth = 1.5) +
  
  # 3. Titik yang menonjol (bulatan kosong)
  geom_point(size = 3.5, shape = 21, fill = "white", stroke = 1) +
  
  # Palet Warna: Warna Monokromatik (Biru & Ungu) untuk rupa canggih
  scale_color_manual(values = c("red", "darkblue")) + # Ungu Diraja & Biru Cerah
  scale_fill_manual(values = c("red", "darkblue")) +
  
  # Pastikan semua label tahun dipaparkan
  scale_x_continuous(breaks = unique(Long_Jenayah$Tahun),
                     expand = expansion(add = c(0.1, 0.1))) +
  
  # Memformat Paksi Y agar mudah dibaca (dalam K - Ribuan)
  scale_y_continuous(labels = scales::comma) + # Guna koma sebagai pemisah ribu
  
  # Pelabelan dan Tajuk
  labs(
    title = "Trend Kategori Jenayah di Malaysia: 
    Kekerasan vs. Harta Benda (2016–2023)",
    x = "Tahun",
    y = "Bilangan Kes",
    color = NULL, # Buang tajuk legenda
    fill = NULL # Buang tajuk legenda
  ) +
  
  # TEMA BARU: theme_minimal() yang diubah suai untuk rupa majalah
  theme_minimal(base_size = 14) +
  theme(
    # Tajuk Utama
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5, color = "#2C3E50"),
    plot.subtitle = element_text(size = 13, hjust = 0.05, color = "grey40", margin = margin(b = 10)),
    
    # Grid: Hanya grid Y yang sangat lembut
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", color = "grey85"),
    panel.grid.minor = element_blank(),
    
    # Legenda: Mendatar di atas
    legend.position = "top",
    legend.direction = "horizontal",
    
    # Teks Paksi
    axis.text = element_text(color = "grey50"),
    axis.title = element_text(face = "bold", color = "grey30")
  )

