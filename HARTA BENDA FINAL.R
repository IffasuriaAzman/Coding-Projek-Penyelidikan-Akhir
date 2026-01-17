
library(dplyr)
library(tidyr)
library(readxl)
library(pheatmap)
library(igraph)
library(ggraph)
library(scales)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(concaveman)
library(ggforce)
library(reshape2)
library(forcats)
library(corrplot)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(geodata)
library(stringr)

target_kategori_hb <- "Harta Benda" 
nilai_ambang_hb    <- 0.9 
tahun_mula_hb      <- 2016
tahun_akhir_hb     <- 2023

path_negeri <- "C:/Users/admin/OneDrive - Universiti Kebangsaan Malaysia/UKM/DEGREE/SEM 6/FYP/CODING/FINALIZED/OBJEKTIF 2 & 3/OBJEKTIF 02.xlsx"
path_daerah <- "C:/Users/admin/OneDrive - Universiti Kebangsaan Malaysia/UKM/DEGREE/SEM 6/FYP/CODING/FINALIZED/OBJEKTIF 2 & 3/OBJEKTIF 02 DAERAH.xlsx"

warna_status_hb <- c("C2" = "#D7191C", "C1" = "#FDAE61", "C3" = "#1A9641") 
warna_extra_hb <- scales::hue_pal()(5)
names(warna_extra_hb) <- paste0("C", 4:8)
warna_final_gabungan_hb <- c(warna_status_hb, warna_extra_hb)

objektif_02Negeri_hb <- read_excel(path_negeri)
data_negeri_clean_hb <- objektif_02Negeri_hb %>%
  filter(Kategori == target_kategori_hb, between(Tahun, tahun_mula_hb, tahun_akhir_hb)) %>%
  group_by(Negeri, Jenis, Tahun) %>%
  summarise(n = sum(as.numeric(`Bilangan Jenayah`), na.rm = TRUE), .groups = "drop")

susunan_tahun_hb <- tahun_mula_hb:tahun_akhir_hb
susunan_jenis_hb <- sort(unique(data_negeri_clean_hb$Jenis))
data_negeri_lengkap_hb <- data_negeri_clean_hb %>%
  complete(Negeri, Tahun = susunan_tahun_hb, Jenis = susunan_jenis_hb, fill = list(n = 0)) %>%
  arrange(Negeri, Tahun, Jenis)

matriks_negeri_wide_hb <- data_negeri_lengkap_hb %>%
  group_by(Negeri) %>%
  summarise(vector = list(n), .groups = "drop") 
gabung_objektif_02_hb <- do.call(cbind, lapply(matriks_negeri_wide_hb$vector, as.numeric))
colnames(gabung_objektif_02_hb) <- matriks_negeri_wide_hb$Negeri

# Korelasi Negeri
Korelasi_Spearman_Negeri_hb <- cor(gabung_objektif_02_hb, method = "spearman", use = "pairwise.complete.obs")

# 2. Visualisasi Heatmap
pheatmap(Korelasi_Spearman_Negeri_hb,
         main = paste("Korelasi Spearman Negeri:", target_kategori_hb),
         display_numbers = TRUE, number_format = "%.2f",
         color = colorRampPalette(c("blue", "white", "red"))(100))
matriks_temp_hb <- Korelasi_Spearman_Negeri_hb
matriks_temp_hb[lower.tri(matriks_temp_hb, diag = TRUE)] <- NA
edges_negeri_hb <- as.data.frame(as.table(matriks_temp_hb)) %>%
  filter(!is.na(Freq)) %>%               # Buang nilai NA
  filter(abs(Freq) >= nilai_ambang_hb) %>% # Ambil nilai melebihi 0.9 sahaja
  rename(from = Var1, to = Var2, weight = Freq) %>%
  arrange(desc(weight))                  # Susun dari korelasi paling tinggi
print(paste("Jumlah pasangan dijumpai:", nrow(edges_negeri_hb)))

if(nrow(edges_negeri_hb) == 0) {
  warning("TIADA DATA: Tiada pasangan negeri yang mempunyai korelasi melebihi 0.9. Sila rendahkan 'nilai_ambang_hb'.")
}

matriks_temp_hb <- Korelasi_Spearman_Negeri_hb
matriks_temp_hb[lower.tri(matriks_temp_hb, diag = TRUE)] <- NA
edges_negeri_hb <- as.data.frame(as.table(matriks_temp_hb)) %>%
  filter(abs(Freq) >= nilai_ambang_hb) %>%
  rename(from = Var1, to = Var2, weight = Freq)

nodes_negeri_hb <- data.frame(name = colnames(Korelasi_Spearman_Negeri_hb), stringsAsFactors = FALSE)
g_negeri_hb <- graph_from_data_frame(edges_negeri_hb, directed = FALSE, vertices = nodes_negeri_hb)
E(g_negeri_hb)$w_abs <- abs(E(g_negeri_hb)$weight)
E(g_negeri_hb)$sign  <- ifelse(E(g_negeri_hb)$weight >= 0, "Positif", "Negatif")
V(g_negeri_hb)$deg   <- degree(g_negeri_hb)

set.seed(42)
lay_negeri_hb <- create_layout(g_negeri_hb, layout = "kk", weights = E(g_negeri_hb)$w_abs)

plot_negeri_hb <- ggraph(lay_negeri_hb) +
  geom_edge_link(aes(width = w_abs, colour = sign), alpha = 0.8) +
  scale_edge_width(range = c(0.5, 3), name = "Nilai Mutlak Berpemberat") +
  scale_edge_color_manual(values = c("Positif" = "red", "Negatif" = "blue"), name = "Tanda Korelasi") +
  geom_node_point(aes(size = deg), fill = "white", shape = 21, color = "black") +
  scale_size_continuous(range = c(5, 15), name = "Degree", breaks = seq(0, 20, 2)) +
  geom_node_text(aes(label = name), repel = TRUE, fontface = "bold") +
  theme_void() +
  labs(title = paste("Rangkaian Negeri:", target_kategori_hb), subtitle = paste("r >=", nilai_ambang_hb))

print(plot_negeri_hb)
df_Centrality_Negeri_hb <- data.frame(
  Negeri = V(g_negeri_hb)$name,
  Degree = degree(g_negeri_hb),
  Closeness = round(closeness(g_negeri_hb, normalized = TRUE), 4),
  Betweenness = round(betweenness(g_negeri_hb, normalized = TRUE), 4)
) %>% arrange(desc(Degree))
rownames(df_Centrality_Negeri_hb) <- NULL
print(df_Centrality_Negeri_hb)

Daerah_HartaBenda_raw <- read_excel(path_daerah)
df_daerah_raw_hb <- Daerah_HartaBenda_raw %>% 
  filter(Kategori == target_kategori_hb, between(Tahun, tahun_mula_hb, tahun_akhir_hb))
edges_all_combined_hb <- data.frame() 
LIST_KORELASI_ND_hb <- data.frame() 
senarai_negeri_hb <- unique(df_daerah_raw_hb$Negeri)
master_jenis_hb <- sort(unique(data_negeri_clean_hb$Jenis)) 
master_tahun_hb <- tahun_mula_hb:tahun_akhir_hb
master_skeleton_hb <- expand.grid(Tahun = master_tahun_hb, Jenis = master_jenis_hb, stringsAsFactors = FALSE) %>% arrange(Tahun, Jenis)

for(negeri_semasa in senarai_negeri_hb) {

  vec_negeri_check_hb <- data_negeri_clean_hb %>%
    filter(Negeri == negeri_semasa) %>%
    right_join(master_skeleton_hb, by = c("Tahun", "Jenis")) %>% 
    mutate(n = ifelse(is.na(n), 0, n)) %>% arrange(Tahun, Jenis)
  vec_negeri_semasa_hb <- vec_negeri_check_hb$n
  
  data_sub_hb <- df_daerah_raw_hb %>%
    filter(Negeri == negeri_semasa) %>%
    mutate(ID_Daerah = paste0(Negeri, "_", Daerah)) %>% 
    group_by(ID_Daerah, Jenis, Tahun) %>%
    summarise(n = sum(Bilangan, na.rm = TRUE), .groups = "drop")
  
  if(nrow(data_sub_hb) == 0) next
  
  senarai_daerah_local_hb <- unique(data_sub_hb$ID_Daerah)
  matriks_list_hb <- list()
  
  for(d in senarai_daerah_local_hb) {
    d_data_hb <- data_sub_hb %>% filter(ID_Daerah == d)
    d_fixed_hb <- master_skeleton_hb %>%
      left_join(d_data_hb, by = c("Tahun", "Jenis")) %>%
      select(Tahun, Jenis, n) %>% 
      mutate(n = ifelse(is.na(n), 0, n)) %>% arrange(Tahun, Jenis)
    matriks_list_hb[[d]] <- d_fixed_hb$n
  }
  
  matriks_sub_hb <- do.call(cbind, matriks_list_hb)
  valid_cols_hb <- apply(matriks_sub_hb, 2, sd) > 0 
  matriks_sub_hb <- matriks_sub_hb[, valid_cols_hb, drop = FALSE]
  
  if(ncol(matriks_sub_hb) < 2) next 
  
  # A. Daerah-Daerah (Guna ambang 0.9)
  cor_sub_hb <- cor(matriks_sub_hb, method = "spearman", use = "pairwise.complete.obs")
  cor_sub_hb[lower.tri(cor_sub_hb, diag=TRUE)] <- NA 
  edges_dd_hb <- as.data.frame(as.table(cor_sub_hb)) %>%
    filter(!is.na(Freq), abs(Freq) >= 0.9) %>% 
    rename(from = Var1, to = Var2, weight = Freq) %>%
    mutate(type = "District_District")
  edges_all_combined_hb <- bind_rows(edges_all_combined_hb, edges_dd_hb)
  
  # B. Negeri-Daerah (Guna ambang 0.3)
  if(negeri_semasa %in% colnames(gabung_objektif_02_hb)) {
    cor_nd_hb <- cor(vec_negeri_semasa_hb, matriks_sub_hb, method = "spearman", use = "pairwise.complete.obs")
    
    df_temp_cor_hb <- data.frame(
      Negeri = negeri_semasa,
      Daerah = sub(paste0(negeri_semasa, "_"), "", colnames(cor_nd_hb)), 
      Korelasi = round(as.numeric(cor_nd_hb[1,]), 4)
    ) %>% arrange(desc(Korelasi))
    LIST_KORELASI_ND_hb <- bind_rows(LIST_KORELASI_ND_hb, df_temp_cor_hb)
    
    edges_nd_hb <- data.frame(from = negeri_semasa, to = colnames(cor_nd_hb), weight = as.numeric(cor_nd_hb[1,])) %>%
      filter(!is.na(weight), abs(weight) >= 0.3) %>% 
      mutate(type = "State_District")
    edges_all_combined_hb <- bind_rows(edges_all_combined_hb, edges_nd_hb)
  }
}

message("\n>>> JADUAL ANALISIS: KORELASI NEGERI VS DAERAH (HB) <<<")
print(head(LIST_KORELASI_ND_hb %>% arrange(Negeri, desc(Korelasi)), 20))
view(LIST_KORELASI_ND_hb)

edges_nn_hb <- edges_negeri_hb %>% mutate(type = "Negeri-Negeri")
all_edges_final_hb <- bind_rows(edges_all_combined_hb, edges_nn_hb)

nodes_final_hb <- data.frame(name = unique(c(all_edges_final_hb$from, all_edges_final_hb$to))) %>%
  mutate(level = ifelse(name %in% colnames(gabung_objektif_02_hb), "Negeri", "Daerah"),
         short_name = ifelse(level == "Daerah", sub(".*_", "", name), name)) 

g_clean_hb <- graph_from_data_frame(all_edges_final_hb, vertices = nodes_final_hb, directed = FALSE)
V(g_clean_hb)$deg <- degree(g_clean_hb)


edges_nn_hb <- edges_negeri_hb %>% mutate(type = "Negeri-Negeri")
all_edges_final_hb <- bind_rows(edges_all_combined_hb, edges_nn_hb)

nodes_final_hb <- data.frame(name = unique(c(all_edges_final_hb$from, all_edges_final_hb$to))) %>%
  mutate(
    # Logik Baru: Kalau ada "_", ia confirm Daerah. Kalau takde, ia Negeri.
    level = ifelse(grepl("_", name), "Daerah", "Negeri"),
    
    short_name = ifelse(level == "Daerah", sub(".*_", "", name), name)
  ) 

g_clean_hb <- graph_from_data_frame(all_edges_final_hb, vertices = nodes_final_hb, directed = FALSE)
V(g_clean_hb)$deg <- degree(g_clean_hb)

set.seed(42) 
p_network_final_hb <- ggraph(g_clean_hb, layout = "kk") +
  
  geom_edge_link(aes(color = type, width = abs(weight)), alpha = 0.6) + 
  
  scale_edge_color_manual(
    values = c("Negeri-Negeri" = "red", 
               "State_District" = "blue", 
               "District_District" = "darkgreen"),
    
    labels = c("Daerah-Daerah", "Negeri-Daerah", "Negeri-Negeri"),
    breaks = c("District_District", "State_District", "Negeri-Negeri"),
    
    name = "Jenis Hubungan"
  ) +
  
  scale_edge_width(range = c(0.5, 1.5), guide = "none") +
  
  geom_node_point(aes(size = deg, fill = level), shape = 21, color = "black", stroke = 0.3) +
  
  scale_fill_manual(
    values = c("Negeri" = "mediumorchid", "Daerah" = "yellow"), 
    name = "Peringkat"
  ) +
  
  geom_node_text(aes(label = short_name), repel = TRUE, size = 2.5, max.overlaps = Inf, fontface = "bold") +
  
  theme_void() +
  labs(title = paste("Rangkaian Gabungan:", target_kategori_hb), 
       subtitle = paste("Analisis Jenayah Harta Benda"))

print(p_network_final_hb)

# BAHAGIAN C: OBJEKTIF 3 (LOUVAIN & PETA NEGERI - RENAMED)

set.seed(42)
comm_negeri_hb <- cluster_louvain(g_negeri_hb, weights = E(g_negeri_hb)$w_abs)
V(g_negeri_hb)$community <- factor(membership(comm_negeri_hb))
V(g_negeri_hb)$Label <- paste0("C", V(g_negeri_hb)$community)

mod_score_negeri_hb <- modularity(comm_negeri_hb)
num_comm_negeri_hb <- length(unique(membership(comm_negeri_hb)))

# Jadual Komuniti
comm_df_hb <- tibble(
  Negeri   = V(g_negeri_hb)$name,
  Komuniti = as.integer(V(g_negeri_hb)$community),
  Label    = V(g_negeri_hb)$Label,
  Degree   = degree(g_negeri_hb)
) %>% arrange(Komuniti, desc(Degree), Negeri)
print(comm_df_hb)

plot_louvain_negeri_hb <- ggraph(g_negeri_hb, layout = "kk") +
  geom_mark_hull(aes(x, y, fill = Label), concavity = 1, alpha = 0.2, show.legend = TRUE) +
  geom_edge_link(aes(width = w_abs), alpha = 0.5, color = "grey50") +
  scale_edge_width(range = c(0.5, 3), name = "Nilai Mutlak") +
  geom_node_point(aes(color = Label), size = 8) +
  geom_node_text(aes(label = name), size = 3.5, fontface = "bold", color = "black") +
  
  scale_fill_manual(values = warna_final_gabungan_hb, name = "Komuniti") +
  scale_color_manual(values = warna_final_gabungan_hb, name = "Komuniti") +
  
  theme_void() +
  labs(title = paste("Pembentukan Komuniti Louvain Negeri:", "Jenayah" ,target_kategori_hb),
       subtitle = paste("Bilangan Komuniti =", num_comm_negeri_hb, "| Q =", round(mod_score_negeri_hb, 4)))
print(plot_louvain_negeri_hb)

# Peta Negeri
malaysia_sf_hb <- ne_states(country = "Malaysia", returnclass = "sf")
malaysia_sf_clean_hb <- malaysia_sf_hb %>%
  mutate(Negeri_Standard = case_when(
    name == "Johore" ~ "Johor", name == "Malacca" ~ "Melaka", name == "Penang" ~ "Pulau Pinang",
    name == "Trengganu" ~ "Terengganu", name == "Negri Sembilan" ~ "Negeri Sembilan",
    name == "Kuala Lumpur" ~ "W.P. Kuala Lumpur", name == "Labuan" ~ "W.P. Labuan", 
    name == "Putrajaya" ~ "W.P. Putrajaya", TRUE ~ name 
  ))

data_map_negeri_hb <- data.frame(Negeri = V(g_negeri_hb)$name, Label = V(g_negeri_hb)$Label)
peta_negeri_final_hb <- left_join(malaysia_sf_clean_hb, data_map_negeri_hb, by = c("Negeri_Standard" = "Negeri"))

plot_peta_negeri_hb <- ggplot(peta_negeri_final_hb) +
  geom_sf(aes(fill = Label), color = "white", size = 0.2) +
  geom_sf_text(aes(label = Negeri_Standard), size = 2.5, fontface = "bold", color = "black", check_overlap = TRUE) +
  
  scale_fill_manual(values = warna_final_gabungan_hb, name = "Komuniti", na.translate = FALSE) + 
  
  theme_void() + 
  labs(title = paste("Peta Komuniti Peringkat Negeri:", "Jenayah", target_kategori_hb),
       subtitle = paste("Bilangan Komuniti =", num_comm_negeri_hb, "| Q =", round(mod_score_negeri_hb, 3)))
print(plot_peta_negeri_hb)

# BAHAGIAN C-2: ANALISIS STATUS KESELAMATAN & PROFIL DOMINASI 
message(">>> Memulakan Analisis Kesimpulan: Status Keselamatan & Profil (HB)...")

# Pastikan info komuniti wujud
info_komuniti_hb <- data.frame(
  Negeri = V(g_negeri_hb)$name,
  Label = V(g_negeri_hb)$Label
)

# 1. Analisis Keselamatan (FIXED: Guna 'n' bukan 'n_hb')
analisis_risiko_hb <- data_negeri_clean_hb %>%
  group_by(Negeri) %>%
  summarise(Total_Kes_Semua_Tahun = sum(n, na.rm = TRUE)) %>%  # <--- FIX DI SINI (Guna 'n')
  inner_join(info_komuniti_hb, by = "Negeri") %>%
  group_by(Label) %>%
  summarise(
    Bil_Negeri = n(),
    Total_Kes_Komuniti = sum(Total_Kes_Semua_Tahun),
    Purata_Beban_Tahunan = (Total_Kes_Komuniti / (tahun_akhir_hb - tahun_mula_hb + 1)) / Bil_Negeri
  ) %>%
  arrange(desc(Purata_Beban_Tahunan)) %>%
  mutate(
    Status_Keselamatan = case_when(
      row_number() == 1 ~ "Berisiko Tinggi (High Risk)",
      row_number() == n() ~ "Paling Selamat (Low Risk)",
      TRUE ~ "Sederhana (Moderate)"
    )
  )

message("\n>>> JADUAL KESIMPULAN STATUS KESELAMATAN (HB) <<<")
print(analisis_risiko_hb)

# 2. Plot Status Keselamatan
plot_status_keselamatan_hb <- ggplot(analisis_risiko_hb, aes(x = reorder(Label, -Purata_Beban_Tahunan), 
                                                             y = Purata_Beban_Tahunan, 
                                                             fill = Status_Keselamatan)) +
  geom_bar(stat = "identity", width = 0.7) +
  
  # Label Teks
  geom_text(aes(label = scales::comma(round(Purata_Beban_Tahunan, 0))), 
            vjust = -0.5, 
            fontface = "bold", 
            size = 4) + 
  
  # Warna Manual
  scale_fill_manual(values = c("Berisiko Tinggi (High Risk)" = "#D7191C", 
                               "Paling Selamat (Low Risk)" = "#1A9641", 
                               "Sederhana (Moderate)" = "#FDAE61")) +
  
  # Ruang Atas
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  
  theme_minimal() +
  labs(
    title = paste("Analisis Status Keselamatan Komuniti:", target_kategori_hb),
    subtitle = "Klasifikasi berdasarkan Purata Beban Jenayah Tahunan per Negeri",
    x = "Komuniti Louvain", y = "Purata Kes Setahun", fill = "Status"
  ) +
  theme(panel.grid.major.x = element_blank())

print(plot_status_keselamatan_hb)

# 3. Profil Dominasi (FIXED: Guna 'n' di sini juga)
profil_dominasi_hb <- data_negeri_clean_hb %>%
  inner_join(info_komuniti_hb, by = "Negeri") %>%
  group_by(Label, Jenis) %>%
  summarise(Total = sum(n, na.rm = TRUE), .groups = "drop") %>% # <--- FIX DI SINI (Guna 'n')
  group_by(Label) %>%
  mutate(Peratus = Total / sum(Total)) %>%
  ungroup()

plot_profil_dominasi_hb <- ggplot(profil_dominasi_hb, aes(x = Label, y = Peratus, fill = Jenis)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = ifelse(Peratus > 0.05, paste0(round(Peratus*100, 0), "%"), "")), 
            position = position_fill(vjust = 0.5), size = 3, color = "white", fontface = "bold") +
  scale_fill_brewer(palette = "Dark2", name = "Jenis Jenayah") +
  theme_minimal() +
  labs(title = paste("Profil Dominasi Jenayah:", target_kategori_hb),
       subtitle = "Komposisi Jenis Jenayah Mengikut Komuniti",
       x = "Komuniti", y = "Peratusan Sumbangan (%)")

print(plot_profil_dominasi_hb)

# Profil Dominasi
profil_dominasi_hb <- data_negeri_clean_hb %>%
  inner_join(info_komuniti_hb, by = "Negeri") %>%
  group_by(Label, Jenis) %>%
  summarise(Total = sum(n), .groups = "drop") %>%
  group_by(Label) %>%
  mutate(Peratus = Total / sum(Total)) %>%
  ungroup()

plot_profil_dominasi_hb <- ggplot(profil_dominasi_hb, aes(x = Label, y = Peratus, fill = Jenis)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = ifelse(Peratus > 0.05, paste0(round(Peratus*100, 0), "%"), "")), 
            position = position_fill(vjust = 0.5), size = 3, color = "white", fontface = "bold") +
  scale_fill_brewer(palette = "Dark2", name = "Jenis Jenayah") +
  theme_minimal() +
  labs(title = paste("Profil Dominasi Jenayah:", target_kategori_hb),
       subtitle = "Komposisi Jenis Jenayah Mengikut Komuniti",
       x = "Komuniti", y = "Peratusan Sumbangan (%)")

print(plot_profil_dominasi_hb)

# ==============================================================================
# BAHAGIAN D: OBJEKTIF 3 - PETA VORONOI DAERAH (RENAMED _hb)
# ==============================================================================
message(">>> Memulakan Objektif 3: Louvain Daerah (HB)...")

set.seed(42)
louvain_gabungan_hb <- cluster_louvain(g_clean_hb, weights = abs(E(g_clean_hb)$weight))
V(g_clean_hb)$community <- factor(membership(louvain_gabungan_hb))
mod_score_hb <- modularity(louvain_gabungan_hb)
num_comm_hb <- length(unique(V(g_clean_hb)$community))

cat(">> Modularity (HB):", round(mod_score_hb, 5), "\n")

# ------------------------------------------------------------------------------
# LANGKAH KUNCI: TETAPKAN WARNA DAHULU (Supaya Network & Peta Sama)
# ------------------------------------------------------------------------------
# Kita tetapkan palet warna di sini supaya boleh dipakai oleh kedua-dua plot
warna_manual_daerah_hb <- c("#E6194B", "#4363D8", "#3CB44B", "#911EB4", 
                            "#F58231", "#FFE119", "#42D4F4", "#F032E6", 
                            "#800000", "#008080", "#9A6324", "#808000")

# Generate warna secukupnya mengikut bilangan komuniti
if(num_comm_hb > 12) {
  warna_final_daerah_hb <- c(warna_manual_daerah_hb, scales::hue_pal()(num_comm_hb - 12))
} else {
  warna_final_daerah_hb <- warna_manual_daerah_hb[1:num_comm_hb]
}

# ------------------------------------------------------------------------------
# PLOT 1: LOUVAIN NETWORK GRAPH (Dengan Warna Konsisten)
# ------------------------------------------------------------------------------
lay_gabungan_hb <- create_layout(g_clean_hb, layout = "fr", niter = 1000)
hull_data_hb <- lay_gabungan_hb %>% group_by(community) %>% filter(n() >= 3) %>% slice(chull(x, y))

plot_louvain_daerah_hb <- ggraph(lay_gabungan_hb) +
  geom_polygon(data = hull_data_hb, aes(x = x, y = y, group = community, fill = community), alpha = 0.15, color = NA) + 
  geom_edge_link(alpha = 0.2) + 
  geom_node_point(aes(color = community, size = deg)) +
  geom_node_text(aes(label = short_name), repel = TRUE, size = 2.5, max.overlaps = Inf) +
  # --- PENAMBAHAN PENTING: Guna palet warna yang sama ---
  scale_fill_manual(values = warna_final_daerah_hb) +
  scale_color_manual(values = warna_final_daerah_hb) +
  # ------------------------------------------------------
theme_void() +
  labs(title = paste("Komuniti Louvain Daerah:", target_kategori_hb))

print(plot_louvain_daerah_hb)

# ------------------------------------------------------------------------------
# MAPPING VORONOI (HB)
# ------------------------------------------------------------------------------
sf_use_s2(FALSE) 

raw_data_komuniti_hb <- data.frame(ID_Node = V(g_clean_hb)$name, Komuniti_ID = V(g_clean_hb)$community) %>% 
  filter(str_detect(ID_Node, "_")) %>%
  separate(ID_Node, into = c("Negeri_Asal", "Daerah_Asal"), sep = "_") %>%
  mutate(Daerah_Upper = str_trim(str_to_upper(Daerah_Asal))) %>%
  mutate(Daerah_Upper = case_when(
    Daerah_Upper == "W.P. KUALA LUMPUR" ~ "KUALA LUMPUR",
    Daerah_Upper == "ULU LANGAT" ~ "HULU LANGAT",
    Daerah_Upper == "ULU SELANGOR" ~ "HULU SELANGOR",
    Daerah_Upper == "ULU PERAK" ~ "HULU PERAK",
    Daerah_Upper == "ULU TERENGGANU" ~ "HULU TERENGGANU",
    Daerah_Upper == "BANDAR BAHRU" ~ "BANDAR BAHARU",
    Daerah_Upper == "KULAIJAYA" ~ "KULAI",
    Daerah_Upper == "BATANG PADANG" ~ "BATANG PADANG",
    TRUE ~ Daerah_Upper
  )) %>% distinct(Daerah_Upper, .keep_all = TRUE)

peta_daerah_raw_hb <- gadm(country = "MYS", level = 2, path = tempdir())
peta_daerah_sf_hb <- st_as_sf(peta_daerah_raw_hb) %>% mutate(Daerah_Map = str_trim(str_remove_all(str_to_upper(NAME_2), "DISTRICT")))

# Fix KL & Perlis Voronoi
poly_kl_hb <- peta_daerah_sf_hb %>% filter(NAME_1 == "Kuala Lumpur") %>% st_union()
titik_kl_hb <- data.frame(
  Daerah_Map = c("SENTUL", "WANGSA MAJU", "DANG WANGI", "BRICKFIELDS", "CHERAS"),
  X = c(101.685, 101.730, 101.700, 101.680, 101.730), 
  Y = c(3.190,   3.200,   3.155,   3.120,   3.090)
) %>% st_as_sf(coords = c("X", "Y"), crs = st_crs(peta_daerah_sf_hb))
voronoi_kl_hb <- st_voronoi(st_union(titik_kl_hb)) %>% st_collection_extract() %>% st_sf()
kl_split_hb <- st_intersection(voronoi_kl_hb %>% st_join(titik_kl_hb), poly_kl_hb)
kl_split_hb$NAME_1 <- "Kuala Lumpur"
kl_split_hb <- kl_split_hb %>% select(Daerah_Map, NAME_1, geometry)

poly_perlis_hb <- peta_daerah_sf_hb %>% filter(NAME_1 == "Perlis") %>% st_union()
titik_perlis_hb <- data.frame(
  Daerah_Map = c("KANGAR", "ARAU", "PADANG BESAR"),
  X = c(100.198, 100.275, 100.296), Y = c(6.441, 6.435, 6.600)
) %>% st_as_sf(coords = c("X", "Y"), crs = st_crs(peta_daerah_sf_hb))
voronoi_perlis_hb <- st_voronoi(st_union(titik_perlis_hb)) %>% st_collection_extract() %>% st_sf()
perlis_split_hb <- st_intersection(voronoi_perlis_hb %>% st_join(titik_perlis_hb), poly_perlis_hb)
perlis_split_hb$NAME_1 <- "Perlis"
perlis_split_hb <- perlis_split_hb %>% select(Daerah_Map, NAME_1, geometry)

peta_daerah_fixed_hb <- bind_rows(
  peta_daerah_sf_hb %>% filter(NAME_1 != "Perlis" & NAME_1 != "Kuala Lumpur") %>% select(Daerah_Map, NAME_1, geometry),
  perlis_split_hb,
  kl_split_hb
)

peta_daerah_fixed_hb <- peta_daerah_fixed_hb %>%
  mutate(Daerah_Map = case_when(
    Daerah_Map == "PENANG TIMUR LAUT" ~ "TIMUR LAUT",
    Daerah_Map == "TIMUR LAUT PULAU PINANG" ~ "TIMUR LAUT",
    Daerah_Map == "SEBERANG PERAI CENTRAL" ~ "SEBERANG PERAI TENGAH",
    Daerah_Map == "SEBERANG PERAI NORTH" ~ "SEBERANG PERAI UTARA",
    Daerah_Map == "SEBERANG PERAI SOUTH" ~ "SEBERANG PERAI SELATAN",
    Daerah_Map == "PUTRAJAYA" ~ "W.P. PUTRAJAYA",
    Daerah_Map == "ULU LANGAT" ~ "HULU LANGAT",
    Daerah_Map == "ULU SELANGOR" ~ "HULU SELANGOR",
    Daerah_Map == "ULU PERAK" ~ "HULU PERAK",
    Daerah_Map == "HULU PERAK" ~ "HULU PERAK",
    Daerah_Map == "HULU TERENGGANU" ~ "HULU TERENGGANU",
    Daerah_Map == "ULU TERENGGANU" ~ "HULU TERENGGANU",
    Daerah_Map == "BANDAR BAHARU" ~ "BANDAR BAHARU",
    Daerah_Map == "BANDAR BAHRU" ~ "BANDAR BAHARU", 
    Daerah_Map == "LANGKAWI" ~ "LANGKAWI",
    Daerah_Map == "MELAKA TENGAH" ~ "MELAKA TENGAH",
    Daerah_Map == "CENTRAL MELAKA" ~ "MELAKA TENGAH",
    Daerah_Map == "JOHOR BAHRU" ~ "JOHOR BAHRU",
    Daerah_Map == "KULAIJAYA" ~ "KULAI",
    Daerah_Map == "KULAI" ~ "KULAI",
    Daerah_Map == "KOTA KINABALU" ~ "KOTA KINABALU",
    Daerah_Map == "KUCHING" ~ "KUCHING",
    TRUE ~ Daerah_Map
  ))

peta_final_daerah_hb <- left_join(peta_daerah_fixed_hb, raw_data_komuniti_hb, by = c("Daerah_Map" = "Daerah_Upper"))

data_berwarna_index_hb <- peta_final_daerah_hb %>%
  filter(!is.na(Komuniti_ID)) %>%
  arrange(Komuniti_ID, Daerah_Map) %>%
  mutate(Indeks_ID = row_number())

label_index_coord_hb <- data_berwarna_index_hb %>% st_centroid() %>% mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2])
label_negeri_coord_hb <- peta_daerah_fixed_hb %>% group_by(NAME_1) %>% summarise(geometry = st_union(geometry)) %>% st_centroid() %>% mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2])

# ------------------------------------------------------------------------------
# PLOT 2: PETA VORONOI (Guna Warna Yang Sama)
# ------------------------------------------------------------------------------
plot_map_index_hb <- ggplot() +
  geom_sf(data = peta_daerah_fixed_hb, fill = "#F0F0F0", color = "grey90", size = 0.2) +
  geom_sf(data = data_berwarna_index_hb, aes(fill = factor(Komuniti_ID)), color = "white", size = 0.1) +
  geom_text(data = label_negeri_coord_hb, aes(x = X, y = Y, label = toupper(NAME_1)), color = "black", fontface = "bold", size = 2.5, alpha = 0.4, check_overlap = FALSE) +
  geom_text(data = label_index_coord_hb, aes(x = X, y = Y, label = Indeks_ID), color = "black", size = 2.2, fontface = "bold") +
  # --- PENTING: Guna variable 'warna_final_daerah_hb' yang sama ---
  scale_fill_manual(values = warna_final_daerah_hb, name = "Kumpulan", na.translate = FALSE) + 
  # ----------------------------------------------------------------
theme_void() +
  labs(title = paste("Peta Indeks Komuniti Daerah:", target_kategori_hb), subtitle = "Nombor mewakili daerah (Rujuk Jadual)", caption = "Sumber: Analisis Rangkaian")

print(plot_map_index_hb)

Jadual_Indeks_hb <- data_berwarna_index_hb %>% st_drop_geometry() %>% select(Indeks_ID, Daerah_Map, Komuniti_ID, Negeri_Asal) %>% arrange(Indeks_ID)
View(Jadual_Indeks_hb)
