# ==============================================================================
# VISUALISASI: STACKED BAR CHART MENGIKUT TAHUN (UPDATED - BOLD & BESAR)
# ==============================================================================

library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# 1. Load Data
perbandingan_deskriptif <- read_excel("C:/Users/admin/OneDrive - Universiti Kebangsaan Malaysia/UKM/DEGREE/SEM 6/FYP/CODING/FINALIZED/OBJEKTIF 2 & 3/OBJEKTIF 02.xlsx")

# ==============================================================================
# BAHAGIAN 1: JENAYAH KEKERASAN (BOLD & BESAR)
# ==============================================================================

# A. Data Preparation
data_kekerasan_tahun <- perbandingan_deskriptif %>%
  filter(Kategori == "Kekerasan") %>%
  group_by(Negeri, Jenis, Tahun) %>%
  summarise(Total_Jenayah = sum(`Bilangan Jenayah`, na.rm = TRUE), .groups = "drop") %>%
  group_by(Negeri) %>%
  mutate(Ranking_Total = sum(Total_Jenayah)) %>%
  ungroup()

# B. Plotting
ggplot(data_kekerasan_tahun, aes(x = reorder(Negeri, Ranking_Total), y = Total_Jenayah, fill = Jenis)) +
  geom_col(width = 0.7) +
  coord_flip() +
  facet_wrap(~Tahun, ncol = 4, scales = "free_x") + 
  theme_bw() + 
  scale_y_continuous(labels = comma) + 
  labs(
    title = "Trend Jenayah Kekerasan Mengikut Negeri (2016-2023)",
    subtitle = "Dipecahkan mengikut tahun.",
    x = "Negeri",
    y = "Jumlah Bilangan Kes",
    fill = "Jenis Jenayah"
  ) +
  theme(
    legend.position = "bottom",
    
    # --- SETTING FON (BOLD & SAIZ BESAR) ---
    
    # 1. Tajuk Utama Graf (Lebih besar)
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    
    # 2. Tajuk Paksi (Perkataan 'Negeri' & 'Jumlah Bilangan Kes')
    axis.title = element_text(face = "bold", size = 14), 
    
    # 3. Label Paksi (Nama Negeri & Nombor)
    # axis.text.x = Label Negeri (sebab dah coord_flip), axis.text.y = Nombor
    axis.text = element_text(face = "bold", size = 11, color = "black"), 
    
    # 4. Label Header Tahun (Strip text)
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white", face = "bold", size = 12),
    
    # 5. Legend (Petunjuk Warna)
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11)
  )

# ==============================================================================
# BAHAGIAN 2: JENAYAH HARTA BENDA (BOLD & BESAR)
# ==============================================================================

# A. Data Preparation
data_hartabenda_tahun <- perbandingan_deskriptif %>%
  filter(Kategori == "Harta Benda") %>%
  group_by(Negeri, Jenis, Tahun) %>%
  summarise(Total_Jenayah = sum(`Bilangan Jenayah`, na.rm = TRUE), .groups = "drop") %>%
  group_by(Negeri) %>%
  mutate(Ranking_Total = sum(Total_Jenayah)) %>%
  ungroup()

# B. Plotting
ggplot(data_hartabenda_tahun, aes(x = reorder(Negeri, Ranking_Total), y = Total_Jenayah, fill = Jenis)) +
  geom_col(width = 0.7) +
  coord_flip() +
  facet_wrap(~Tahun, ncol = 4, scales = "free_x") + 
  theme_bw() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Trend Jenayah Harta Benda Mengikut Negeri (2016-2023)",
    subtitle = "Dipecahkan mengikut tahun.",
    x = "Negeri",
    y = "Jumlah Bilangan Kes",
    fill = "Jenis Jenayah"
  ) +
  theme(
    legend.position = "bottom",
    
    # --- SETTING FON (BOLD & SAIZ BESAR) ---
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    
    axis.title = element_text(face = "bold", size = 14),
    
    # Menebalkan nama negeri dan nombor paksi
    axis.text = element_text(face = "bold", size = 11, color = "black"),
    
    strip.background = element_rect(fill = "darkblue"), 
    strip.text = element_text(color = "white", face = "bold", size = 12),
    
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11)
  )


