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

target_kategori <- "Kekerasan"
nilai_ambang    <- 0.9
tahun_mula      <- 2016
tahun_akhir     <- 2023
path_negeri <- "C:/Users/admin/OneDrive - Universiti Kebangsaan Malaysia/UKM/DEGREE/SEM 6/FYP/CODING/FINALIZED/OBJEKTIF 2 & 3/OBJEKTIF 02.xlsx"
path_daerah <- "C:/Users/admin/OneDrive - Universiti Kebangsaan Malaysia/UKM/DEGREE/SEM 6/FYP/CODING/FINALIZED/OBJEKTIF 2 & 3/OBJEKTIF 02 DAERAH.xlsx"

message(paste(">>> Memulakan Objektif 2: Analisis Negeri (", target_kategori, ")..."))

objektif_02Negeri <- read_excel(path_negeri)
data_negeri_clean <- objektif_02Negeri %>%
  filter(Kategori == target_kategori, between(Tahun, tahun_mula, tahun_akhir)) %>%
  group_by(Negeri, Jenis, Tahun) %>%
  summarise(n = sum(as.numeric(`Bilangan Jenayah`), na.rm = TRUE), .groups = "drop")

susunan_tahun <- tahun_mula:tahun_akhir
susunan_jenis <- sort(unique(data_negeri_clean$Jenis))
data_negeri_lengkap <- data_negeri_clean %>%
  complete(Negeri, Tahun = susunan_tahun, Jenis = susunan_jenis, fill = list(n = 0)) %>%
  arrange(Negeri, Tahun, Jenis)

matriks_negeri_wide <- data_negeri_lengkap %>%
  group_by(Negeri) %>%
  summarise(vector = list(n), .groups = "drop") 
gabung_objektif_02 <- do.call(cbind, lapply(matriks_negeri_wide$vector, as.numeric))
colnames(gabung_objektif_02) <- matriks_negeri_wide$Negeri

# Kira Korelasi Negeri
Korelasi_Spearman_Negeri <- cor(gabung_objektif_02, method = "spearman", use = "pairwise.complete.obs")

pheatmap(Korelasi_Spearman_Negeri,
         main = paste("Korelasi Spearman Negeri:", target_kategori),
         display_numbers = TRUE, number_format = "%.2f",
         color = colorRampPalette(c("blue", "white", "red"))(100))

matriks_temp <- Korelasi_Spearman_Negeri
matriks_temp[lower.tri(matriks_temp, diag = TRUE)] <- NA
edges_negeri <- as.data.frame(as.table(matriks_temp)) %>%
  filter(abs(Freq) >= nilai_ambang) %>%
  rename(from = Var1, to = Var2, weight = Freq)

nodes_negeri <- data.frame(name = colnames(Korelasi_Spearman_Negeri), stringsAsFactors = FALSE)
g_negeri <- graph_from_data_frame(edges_negeri, directed = FALSE, vertices = nodes_negeri)
E(g_negeri)$w_abs <- abs(E(g_negeri)$weight)
E(g_negeri)$sign  <- ifelse(E(g_negeri)$weight >= 0, "Positif", "Negatif")
V(g_negeri)$deg   <- degree(g_negeri)

set.seed(42)
lay_negeri <- create_layout(g_negeri, layout = "kk", weights = E(g_negeri)$w_abs)

plot_negeri <- ggraph(lay_negeri) +
  geom_edge_link(aes(width = w_abs, colour = sign), alpha = 0.8) +
  scale_edge_width(range = c(0.5, 3), name = "Nilai Mutlak Berpemberat") +
  scale_edge_color_manual(values = c("Positif" = "red", "Negatif" = "blue"), name = "Tanda Korelasi") +
  geom_node_point(aes(size = deg), fill = "white", shape = 21, color = "black") +
  scale_size_continuous(range = c(5, 15), name = "Degree", breaks = seq(0, 20, 2)) +
  geom_node_text(aes(label = name), repel = TRUE, fontface = "bold") +
  theme_void() +
  labs(title = paste("Rangkaian Negeri:", target_kategori), subtitle = paste("r >=", nilai_ambang))

print(plot_negeri)

df_Centrality_Negeri <- data.frame(
  Negeri = V(g_negeri)$name,
  Degree = degree(g_negeri),
  Closeness = round(closeness(g_negeri, normalized = TRUE), 4),
  Betweenness = round(betweenness(g_negeri, normalized = TRUE), 4)
) %>% arrange(desc(Degree))
rownames(df_Centrality_Negeri) <- NULL

message("\n>>> JADUAL KIRAAN CENTRALITY (NEGERI) <<<")
print(df_Centrality_Negeri)

message(">>> Memulakan Objektif 2: Analisis Gabungan Daerah...")
Daerah_kekerasan <- read_excel(path_daerah)
df_daerah_raw <- Daerah_kekerasan %>% 
  filter(Kategori == target_kategori, between(Tahun, tahun_mula, tahun_akhir))

edges_all_combined <- data.frame() 
LIST_KORELASI_ND <- data.frame()
senarai_negeri <- unique(df_daerah_raw$Negeri)

for(negeri_semasa in senarai_negeri) {
  data_sub <- df_daerah_raw %>%
    filter(Negeri == negeri_semasa) %>%
    mutate(ID_Daerah = paste0(Negeri, "_", Daerah)) %>% 
    group_by(ID_Daerah, Jenis, Tahun) %>%
    summarise(n = sum(Bilangan, na.rm = TRUE), .groups = "drop")
  
  if(nrow(data_sub) == 0) next
  
  data_sub_lengkap <- data_sub %>%
    complete(ID_Daerah, Tahun = susunan_tahun, Jenis = susunan_jenis, fill = list(n = 0)) %>%
    arrange(ID_Daerah, Tahun, Jenis)
  
  matriks_sub <- data_sub_lengkap %>%
    pivot_wider(names_from = ID_Daerah, values_from = n) %>%
    select(-Jenis, -Tahun) %>%
    select(where(~ sd(.) > 0))
  
  if(ncol(matriks_sub) < 2) next 
  
  cor_sub <- cor(matriks_sub, method = "spearman", use = "pairwise.complete.obs")
  cor_sub[lower.tri(cor_sub, diag=TRUE)] <- NA 
  edges_dd <- as.data.frame(as.table(cor_sub)) %>%
    filter(!is.na(Freq), abs(Freq) >= nilai_ambang) %>% 
    rename(from = Var1, to = Var2, weight = Freq) %>%
    mutate(type = "District_District")
  edges_all_combined <- bind_rows(edges_all_combined, edges_dd)
  
  if(negeri_semasa %in% colnames(gabung_objektif_02)) {
    vec_negeri_semasa <- gabung_objektif_02[, negeri_semasa] 
    cor_nd <- cor(vec_negeri_semasa, matriks_sub, method = "spearman", use = "pairwise.complete.obs")
    
    df_temp_cor <- data.frame(
      Negeri = negeri_semasa,
      Daerah = sub(paste0(negeri_semasa, "_"), "", colnames(cor_nd)), 
      Korelasi = round(as.numeric(cor_nd[1,]), 4)
    ) %>% arrange(desc(Korelasi))
    LIST_KORELASI_ND <- bind_rows(LIST_KORELASI_ND, df_temp_cor)
    
    edges_nd <- data.frame(from = negeri_semasa, to = colnames(cor_nd), weight = as.numeric(cor_nd[1,])) %>%
      filter(!is.na(weight), abs(weight) >= nilai_ambang) %>% 
      mutate(type = "State_District")
    edges_all_combined <- bind_rows(edges_all_combined, edges_nd)
  }
}

edges_nn <- edges_negeri %>% mutate(type = "State_State")
all_edges_final <- bind_rows(edges_all_combined, edges_nn)

nodes_final <- data.frame(name = unique(c(all_edges_final$from, all_edges_final$to))) %>%
  mutate(level = ifelse(name %in% colnames(gabung_objektif_02), "Negeri", "Daerah"),
         short_name = ifelse(level == "Daerah", sub(".*_", "", name), name)) 

g_clean <- graph_from_data_frame(all_edges_final, vertices = nodes_final, directed = FALSE)
V(g_clean)$deg <- degree(g_clean)

set.seed(42) 
p_network_final <- ggraph(g_clean, layout = "kk") +
  geom_edge_link(aes(color = type, width = abs(weight)), alpha = 0.6) + 
  scale_edge_color_manual(values = c("State_State"="red", "State_District"="blue", "District_District"="darkgreen")) +
  scale_edge_width(range = c(0.5, 1.5), guide = "none") +
  geom_node_point(aes(size = deg, fill = level), shape = 21) +
  geom_node_text(aes(label = short_name), repel = TRUE, size = 2.5, max.overlaps = Inf) +
  theme_void() +
  labs(title = paste("Rangkaian Gabungan:", target_kategori), subtitle = paste("r >=", nilai_ambang))

print(p_network_final)

# 1. PENGIRAAN KOMUNITI
set.seed(42)
comm_negeri <- cluster_louvain(g_negeri, weights = E(g_negeri)$w_abs)
V(g_negeri)$community <- factor(membership(comm_negeri))
V(g_negeri)$Label <- paste0("C", V(g_negeri)$community)

mod_score_negeri <- modularity(comm_negeri)
num_comm_negeri <- length(unique(membership(comm_negeri)))

info_temp <- data.frame(Negeri = V(g_negeri)$name, Label = V(g_negeri)$Label)

ranking_temp <- data_negeri_clean %>%
  group_by(Negeri) %>%
  summarise(Total = sum(n)) %>%
  inner_join(info_temp, by = "Negeri") %>%
  group_by(Label) %>%
  summarise(Beban_Purata = (sum(Total)/8)/n()) %>%
  arrange(desc(Beban_Purata)) %>%
  mutate(
    Warna_Auto = case_when(
      row_number() == 1 ~ "#D7191C",   # MERAH (Paling Bahaya)
      row_number() == n() ~ "#1A9641", # HIJAU (Paling Selamat)
      TRUE ~ "#FDAE61"                 # OREN (Sederhana)
    )
  )

warna_final_kekerasan <- setNames(ranking_temp$Warna_Auto, ranking_temp$Label)
message(">>> Warna telah ditetapkan secara automatik ikut risiko:")
print(ranking_temp)

comm_df <- tibble(
  Negeri   = V(g_negeri)$name,
  Komuniti = as.integer(V(g_negeri)$community),
  Label    = V(g_negeri)$Label,
  Degree   = degree(g_negeri)
) %>% arrange(Komuniti, desc(Degree), Negeri)
print(comm_df)

# 3. PLOT RANGKAIAN (DENGAN WARNA AUTO & NO LABEL NA)
plot_louvain_negeri <- ggraph(g_negeri, layout = "kk") +
  geom_mark_hull(aes(x, y, fill = Label), 
                 concavity = 1, alpha = 0.2, expand = unit(3, "mm"), show.legend = TRUE) +
  geom_edge_link(aes(width = w_abs), alpha = 0.5, color = "grey50") +
  scale_edge_width(range = c(0.5, 3), name = "Nilai Mutlak") +
  geom_node_point(aes(color = Label), size = 8) +
  geom_node_text(aes(label = name), size = 3.5, fontface = "bold", color = "black") +
  
  scale_fill_manual(values = warna_final_kekerasan, name = "Komuniti (Auto-Risk)") +
  scale_color_manual(values = warna_final_kekerasan, name = "Komuniti (Auto-Risk)") +
  
  theme_void() +
  labs(title = paste("Komuniti Louvain Negeri:", target_kategori),
       subtitle = paste("Bil. Komuniti =", num_comm_negeri, "| Q =", round(mod_score_negeri, 4)))

print(plot_louvain_negeri)

# 4. PETA NEGERI
malaysia_sf <- ne_states(country = "Malaysia", returnclass = "sf")
malaysia_sf_clean <- malaysia_sf %>%
  mutate(Negeri_Standard = case_when(
    name == "Johore" ~ "Johor", name == "Malacca" ~ "Melaka", name == "Penang" ~ "Pulau Pinang",
    name == "Trengganu" ~ "Terengganu", name == "Negri Sembilan" ~ "Negeri Sembilan",
    name == "Kuala Lumpur" ~ "W.P. Kuala Lumpur", name == "Labuan" ~ "W.P. Labuan", 
    name == "Putrajaya" ~ "W.P. Putrajaya", TRUE ~ name 
  ))

data_map_negeri <- data.frame(Negeri = V(g_negeri)$name, Label = V(g_negeri)$Label)
peta_negeri_final <- left_join(malaysia_sf_clean, data_map_negeri, by = c("Negeri_Standard" = "Negeri"))

plot_peta_negeri <- ggplot(peta_negeri_final) +
  geom_sf(aes(fill = Label), color = "white", size = 0.2) +
  geom_sf_text(aes(label = Negeri_Standard), size = 2.5, fontface = "bold", color = "black", check_overlap = TRUE) +
  
  # FIX: BUANG KOTAK NA & GUNA WARNA AUTO
  scale_fill_manual(values = warna_final_kekerasan, name = "Komuniti", na.translate = FALSE) + 
  
  theme_void() + 
  labs(title = paste("Peta Komuniti Negeri:", target_kategori),
       subtitle = paste("Bil. Komuniti =", num_comm_negeri, "| Q =", round(mod_score_negeri, 3)))
print(plot_peta_negeri)

# BAHAGIAN C-2: ANALISIS STATUS KESELAMATAN & PROFIL DOMINASI 
# 1. ANALISIS KESELAMATAN
analisis_risiko <- data_negeri_clean %>%
  group_by(Negeri) %>%
  summarise(Total_Kes_Semua_Tahun = sum(n)) %>%
  inner_join(info_temp, by = "Negeri") %>% # Guna info_temp yang dah ada
  group_by(Label) %>%
  summarise(
    Bil_Negeri = n(),
    Total_Kes_Komuniti = sum(Total_Kes_Semua_Tahun),
    Purata_Beban_Tahunan = (Total_Kes_Komuniti / (tahun_akhir - tahun_mula + 1)) / Bil_Negeri
  ) %>%
  arrange(desc(Purata_Beban_Tahunan)) %>%
  mutate(
    Status_Keselamatan = case_when(
      row_number() == 1 ~ "Berisiko Tinggi",
      row_number() == n() ~ "Paling Selamat",
      TRUE ~ "Sederhana"
    )
  )

message("\n>>> JADUAL KESIMPULAN STATUS KESELAMATAN <<<")
print(analisis_risiko)

# Plot Bar Chart Keselamatan (FIXED)
plot_status_keselamatan <- ggplot(analisis_risiko, aes(x = reorder(Label, -Purata_Beban_Tahunan), 
                                                       y = Purata_Beban_Tahunan, 
                                                       fill = Label)) +
  geom_bar(stat = "identity", width = 0.7) +
  
  # Label Teks
  geom_text(aes(label = scales::comma(round(Purata_Beban_Tahunan, 0))), 
            vjust = -0.5,      # Tolak teks naik ke atas
            fontface = "bold", 
            size = 4) +        # Saiz tulisan supaya jelas
  
  # GUNA WARNA AUTO
  scale_fill_manual(values = warna_final_kekerasan) +
  
  # --- [FIX UTAMA] ---
  # 'expansion(mult = c(0, 0.15))' maksudnya:
  # Bawah = 0% ruang (rapat garisan), Atas = 15% ruang kosong (supaya nombor tak terpotong)
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  
  theme_minimal() +
  labs(title = paste("Analisis Status Keselamatan Komuniti:", target_kategori),
       subtitle = "Klasifikasi berdasarkan Purata Beban Jenayah Tahunan per Negeri",
       x = "Komuniti Louvain", y = "Purata Kes Setahun") +
  
  # Buang grid menegak supaya nampak lebih kemas
  theme(panel.grid.major.x = element_blank())

print(plot_status_keselamatan)

# 2. PROFIL DOMINASI
profil_dominasi <- data_negeri_clean %>%
  inner_join(info_temp, by = "Negeri") %>%
  group_by(Label, Jenis) %>%
  summarise(Total = sum(n), .groups = "drop") %>%
  group_by(Label) %>%
  mutate(Peratus = Total / sum(Total)) %>%
  ungroup()

plot_profil_dominasi <- ggplot(profil_dominasi, aes(x = Label, y = Peratus, fill = Jenis)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = ifelse(Peratus > 0.05, paste0(round(Peratus*100, 0), "%"), "")), 
            position = position_fill(vjust = 0.5), size = 3, color = "white", fontface = "bold") +
  scale_fill_brewer(palette = "Dark2", name = "Jenis Jenayah") +
  theme_minimal() +
  labs(title = paste("Profil Dominasi Jenayah:", target_kategori),
       subtitle = "Komposisi Jenis Jenayah Mengikut Komuniti",
       x = "Komuniti", y = "Peratusan Sumbangan (%)")

print(plot_profil_dominasi)

# BAHAGIAN D: OBJEKTIF 3 - PETA INDEKS VORONOI
message(">>> Memulakan Objektif 3: Louvain Daerah & Peta 

# 1. Louvain Network
set.seed(42)
louvain_gabungan <- cluster_louvain(g_clean, weights = abs(E(g_clean)$weight))
V(g_clean)$community <- factor(membership(louvain_gabungan))

# Kira Modularity
mod_score <- modularity(louvain_gabungan)
num_comm <- length(unique(V(g_clean)$community))

# Print info ke console juga untuk rujukan pantas
message(paste(">>> Modularity Score:", round(mod_score, 4)))
message(paste(">>> Jumlah Komuniti:", num_comm))

# 2. Plot Network
lay_gabungan <- create_layout(g_clean, layout = "fr", niter = 1000)

# Pastikan data hull wujud (filter group >= 3 nodes supaya boleh buat polygon)
hull_data <- lay_gabungan %>% 
  group_by(community) %>% 
  filter(n() >= 3) %>% 
  slice(chull(x, y))

plot_louvain_daerah <- ggraph(lay_gabungan) +
  geom_polygon(data = hull_data, aes(x = x, y = y, group = community, fill = community), alpha = 0.15, color = NA) + 
  geom_edge_link(alpha = 0.2) + 
  geom_node_point(aes(color = community, size = deg)) +
  geom_node_text(aes(label = short_name), repel = TRUE, size = 2.5, max.overlaps = Inf) +
  theme_void() +
  labs(
    title = paste("Komuniti Louvain Daerah:", target_kategori),
    # --- TAMBAHAN DI SINI ---
    subtitle = paste("Modulariti:", round(mod_score, 4), "| Jumlah Komuniti:", num_comm)
  )

print(plot_louvain_daerah)
# 3. Mapping (Voronoi KL & Perlis)
sf_use_s2(FALSE) 

# A. PREP DATA (JANGAN GABUNG KL!)
raw_data_komuniti <- data.frame(ID_Node = V(g_clean)$name, Komuniti_ID = V(g_clean)$community) %>% 
  filter(str_detect(ID_Node, "_")) %>%
  separate(ID_Node, into = c("Negeri_Asal", "Daerah_Asal"), sep = "_") %>%
  mutate(Daerah_Upper = str_trim(str_to_upper(Daerah_Asal))) %>%
  mutate(Daerah_Upper = case_when(
    # JANGAN TUKAR SENTUL/CHERAS DLL KEPADA KL. BIARKAN MEREKA.
    Daerah_Upper == "W.P. KUALA LUMPUR" ~ "KUALA LUMPUR", # Fallback jika ada
    Daerah_Upper == "ULU LANGAT" ~ "HULU LANGAT",
    Daerah_Upper == "ULU SELANGOR" ~ "HULU SELANGOR",
    Daerah_Upper == "ULU PERAK" ~ "HULU PERAK",
    Daerah_Upper == "ULU TERENGGANU" ~ "HULU TERENGGANU",
    Daerah_Upper == "BANDAR BAHRU" ~ "BANDAR BAHARU",
    Daerah_Upper == "KULAIJAYA" ~ "KULAI",
    Daerah_Upper == "BATANG PADANG" ~ "BATANG PADANG",
    TRUE ~ Daerah_Upper
  )) %>% distinct(Daerah_Upper, .keep_all = TRUE)

# B. PETA ASAL
peta_daerah_raw <- gadm(country = "MYS", level = 2, path = tempdir())
peta_daerah_sf <- st_as_sf(peta_daerah_raw) %>% mutate(Daerah_Map = str_trim(str_remove_all(str_to_upper(NAME_2), "DISTRICT")))

# --- FIX 1: PECAHKAN KUALA LUMPUR (Voronoi) ---
# Kita cipta titik koordinat anggaran untuk 5 daerah polis KL
poly_kl <- peta_daerah_sf %>% filter(NAME_1 == "Kuala Lumpur") %>% st_union()

# Koordinat anggaran pusat daerah polis
titik_kl <- data.frame(
  Daerah_Map = c("SENTUL", "WANGSA MAJU", "DANG WANGI", "BRICKFIELDS", "CHERAS"),
  X = c(101.685, 101.730, 101.700, 101.680, 101.730), 
  Y = c(3.190,   3.200,   3.155,   3.120,   3.090)
) %>% st_as_sf(coords = c("X", "Y"), crs = st_crs(peta_daerah_sf))

voronoi_kl <- st_voronoi(st_union(titik_kl)) %>% st_collection_extract() %>% st_sf()
kl_split <- st_intersection(voronoi_kl %>% st_join(titik_kl), poly_kl)
kl_split$NAME_1 <- "Kuala Lumpur"
kl_split <- kl_split %>% select(Daerah_Map, NAME_1, geometry)

# --- FIX 2: PECAHKAN PERLIS (Voronoi) ---
poly_perlis <- peta_daerah_sf %>% filter(NAME_1 == "Perlis") %>% st_union()
titik_perlis <- data.frame(
  Daerah_Map = c("KANGAR", "ARAU", "PADANG BESAR"),
  X = c(100.198, 100.275, 100.296), Y = c(6.441, 6.435, 6.600)
) %>% st_as_sf(coords = c("X", "Y"), crs = st_crs(peta_daerah_sf))

voronoi_perlis <- st_voronoi(st_union(titik_perlis)) %>% st_collection_extract() %>% st_sf()
perlis_split <- st_intersection(voronoi_perlis %>% st_join(titik_perlis), poly_perlis)
perlis_split$NAME_1 <- "Perlis"
perlis_split <- perlis_split %>% select(Daerah_Map, NAME_1, geometry)

# --- GABUNG SEMULA PETA (Map Asal - KL - Perlis + KL Baru + Perlis Baru) ---
peta_daerah_fixed <- bind_rows(
  peta_daerah_sf %>% 
    filter(NAME_1 != "Perlis" & NAME_1 != "Kuala Lumpur") %>% # Buang KL & Perlis asal
    select(Daerah_Map, NAME_1, geometry), 
  perlis_split,
  kl_split
)

# Fix Standard Names
peta_daerah_fixed <- peta_daerah_fixed %>%
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

# C. Join & Plot
peta_final_daerah <- left_join(peta_daerah_fixed, raw_data_komuniti, by = c("Daerah_Map" = "Daerah_Upper"))

# Semak jika ada daerah KL yang miss
check_kl <- peta_final_daerah %>% filter(NAME_1 == "Kuala Lumpur")
print(">>> Debug: Daerah KL dalam Peta <<<")
print(check_kl$Daerah_Map)

data_berwarna_index <- peta_final_daerah %>%
  filter(!is.na(Komuniti_ID)) %>%
  arrange(Komuniti_ID, Daerah_Map) %>%
  mutate(Indeks_ID = row_number())

label_index_coord <- data_berwarna_index %>% st_centroid() %>% mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2])
label_negeri_coord <- peta_daerah_fixed %>% group_by(NAME_1) %>% summarise(geometry = st_union(geometry)) %>% st_centroid() %>% mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2])

# Warna Manual Daerah
warna_manual_daerah <- c("#E6194B", "#4363D8", "#3CB44B", "#911EB4", "#F58231", "#FFE119", "#42D4F4", "#F032E6", "#800000", "#008080", "#9A6324", "#808000")
num_comm_final <- length(unique(data_berwarna_index$Komuniti_ID))
warna_final_daerah <- if(num_comm_final > 12) c(warna_manual_daerah, scales::hue_pal()(num_comm_final-12)) else warna_manual_daerah[1:num_comm_final]

plot_map_index <- ggplot() +
  geom_sf(data = peta_daerah_fixed, fill = "#F0F0F0", color = "grey90", size = 0.2) +
  geom_sf(data = data_berwarna_index, aes(fill = factor(Komuniti_ID)), color = "white", size = 0.1) +
  geom_text(data = label_negeri_coord, aes(x = X, y = Y, label = toupper(NAME_1)), color = "black", fontface = "bold", size = 2.5, alpha = 0.3, check_overlap = FALSE) +
  geom_text(data = label_index_coord, aes(x = X, y = Y, label = Indeks_ID), color = "black", size = 2.2, fontface = "bold") +
  scale_fill_manual(values = warna_final_daerah, name = "Kumpulan", na.translate = FALSE) +
  theme_void() +
  labs(title = paste("Peta Indeks Komuniti Daerah:", target_kategori), subtitle = "Nombor mewakili daerah (Rujuk Jadual)", caption = "Sumber: Analisis Rangkaian")

print(plot_map_index)

# Jadual Rujukan
Jadual_Indeks <- data_berwarna_index %>% st_drop_geometry() %>% select(Indeks_ID, Daerah_Map, Komuniti_ID, Negeri_Asal) %>% arrange(Indeks_ID)
View(Jadual_Indeks)

#__________________________________________________________________________________________________
# ==============================================================================
# SKRIP KHAS: PERBANDINGAN RISIKO (SIMPLE LABEL + COLOR HEATMAP)
# ==============================================================================

# 1. SETUP DATA
# Pastikan path file betul
path_file <- "C:/Users/admin/OneDrive - Universiti Kebangsaan Malaysia/UKM/DEGREE/SEM 6/FYP/CODING/FINALIZED/OBJEKTIF 2 & 3/OBJEKTIF 02.xlsx"
df_raw <- read_excel(path_file)

# 2. FUNGSI UNTUK KIRA & LABEL RISIKO (SIMPLIFIED)
kira_label_risiko <- function(data, kategori_nama) {
  data %>%
    filter(Kategori == kategori_nama) %>%
    group_by(Negeri) %>%
    summarise(Purata_Kes = mean(`Bilangan Jenayah`, na.rm = TRUE)) %>%
    mutate(
      # Label diringkaskan kepada 3 sahaja
      Kategori_Risk = case_when(
        Purata_Kes >= quantile(Purata_Kes, 0.66) ~ "BERISIKO",
        Purata_Kes >= quantile(Purata_Kes, 0.33) ~ "SEDERHANA",
        TRUE ~ "SELAMAT"
      )
    ) %>%
    select(Negeri, Kategori_Risk) %>%
    rename(!!sym(paste0("Status_", kategori_nama)) := Kategori_Risk)
}

# 3. JALANKAN ANALISIS
status_kekerasan <- kira_label_risiko(df_raw, "Kekerasan")
status_harta     <- kira_label_risiko(df_raw, "Harta Benda")

# 4. GABUNGKAN KEPUTUSAN
laporan_akhir <- status_kekerasan %>%
  inner_join(status_harta, by = "Negeri") %>%
  arrange(desc(`Status_Harta Benda`))

# 5. PAPARKAN JADUAL (TEXT OUTPUT)
print(">>> LAPORAN STATUS RISIKO RINGKAS <<<")
print(as.data.frame(laporan_akhir))

# ==============================================================================
# 6. VISUALISASI WARNA (REPORT CARD HEATMAP)
# ==============================================================================
# Ubah data ke bentuk panjang untuk plotting
data_plot <- laporan_akhir %>%
  pivot_longer(cols = starts_with("Status"), names_to = "Jenis_Jenayah", values_to = "Status") %>%
  mutate(
    # Bersihkan nama label supaya cantik dalam graf
    Jenis_Jenayah = gsub("Status_", "", Jenis_Jenayah),
    # Tetapkan urutan status supaya legend cantik
    Status = factor(Status, levels = c("BERISIKO", "SEDERHANA", "SELAMAT"))
  )

# Plot Heatmap
plot_risiko <- ggplot(data_plot, aes(x = Jenis_Jenayah, y = reorder(Negeri, desc(Negeri)), fill = Status)) +
  geom_tile(color = "white", linewidth = 1) + # Kotak berwarna
  geom_text(aes(label = Status), size = 3, fontface = "bold") + # Label teks dalam kotak
  
  # WARNA MANUAL: MERAH, KUNING, HIJAU
  scale_fill_manual(values = c("BERISIKO" = "#FF4D4D",   # Merah
                               "SEDERHANA" = "#FFD700",  # Kuning Emas
                               "SELAMAT" = "#66CC66")) + # Hijau
  
  theme_minimal() +
  labs(
    title = "Status Risiko Jenayah Mengikut Negeri",
    subtitle = "Perbandingan Status Risiko: Kekerasan vs Harta Benda",
    x = "Kategori Jenayah",
    y = "Negeri",
    fill = "Tahap Risiko"
  ) +
  theme(
    axis.text = element_text(color = "black", size = 10),
    panel.grid = element_blank() # Buang garisan grid belakang
  )

print(plot_risiko)


# ==============================================================================
# OBJEKTIF 3: ANALISIS LOUVAIN & PETA VORONOI (FULL & FIXED COLOR)
# ==============================================================================
library(ggraph)
library(igraph)
library(tidyverse)
library(sf)
library(scales) # Untuk fungsi hue_pal()
library(geodata) # Untuk gadm()

message(">>> Memulakan Proses Objektif 3 Sepenuhnya...")

# ------------------------------------------------------------------------------
# LANGKAH 1: LOUVAIN ALGORITHM & PENETAPAN WARNA (MASTER PALETTE)
# ------------------------------------------------------------------------------

# 1. Jalankan Louvain
set.seed(42)
louvain_gabungan <- cluster_louvain(g_clean, weights = abs(E(g_clean)$weight))
V(g_clean)$community <- factor(membership(louvain_gabungan))

# 2. Kira Statistik
mod_score <- modularity(louvain_gabungan)
num_comm <- length(unique(V(g_clean)$community))

# 3. TETAPKAN WARNA MASTER (PENTING!)
# Warna ini akan digunakan oleh Network DAN Peta.
warna_manual_daerah <- c(
  "#E6194B", "#4363D8", "#3CB44B", "#911EB4", "#F58231", 
  "#FFE119", "#42D4F4", "#F032E6", "#800000", "#008080", 
  "#9A6324", "#808000", "#a9a9a9", "#000075", "#000000"
)

# Jika ada lebih banyak kumpulan dari warna yang disediakan, tambah secara auto
if(num_comm > length(warna_manual_daerah)) {
  warna_final_daerah <- c(warna_manual_daerah, scales::hue_pal()(num_comm - length(warna_manual_daerah)))
} else {
  warna_final_daerah <- warna_manual_daerah[1:num_comm]
}

message(paste(">>> Modularity:", round(mod_score, 4)))
message(paste(">>> Jumlah Komuniti:", num_comm))

# ------------------------------------------------------------------------------
# LANGKAH 2: PLOT NETWORK (GRAF RANGKAIAN)
# ------------------------------------------------------------------------------

# Layout Network
lay_gabungan <- create_layout(g_clean, layout = "fr", niter = 1000)

# Buat polygon background (hull)
hull_data <- lay_gabungan %>% 
  group_by(community) %>% 
  filter(n() >= 3) %>% 
  slice(chull(x, y))

plot_louvain_daerah <- ggraph(lay_gabungan) +
  geom_polygon(data = hull_data, aes(x = x, y = y, group = community, fill = community), alpha = 0.15, color = NA) + 
  geom_edge_link(alpha = 0.2) + 
  geom_node_point(aes(color = community, size = deg)) +
  geom_node_text(aes(label = short_name), repel = TRUE, size = 2.5, max.overlaps = Inf) +
  
  # --- GUNA WARNA MASTER ---
  scale_color_manual(values = warna_final_daerah) + 
  scale_fill_manual(values = warna_final_daerah) +
  # -------------------------

theme_void() +
  labs(
    title = paste("Komuniti Louvain Daerah:", target_kategori),
    subtitle = paste("Modularity:", round(mod_score, 4), "| Jumlah Komuniti:", num_comm)
  )

print(plot_louvain_daerah)

# ------------------------------------------------------------------------------
# LANGKAH 3: PENYEDIAAN DATA PETA (DATA WRANGLING)
# ------------------------------------------------------------------------------
sf_use_s2(FALSE) 

# A. Ambil data komuniti dari network nodes
raw_data_komuniti <- data.frame(ID_Node = V(g_clean)$name, Komuniti_ID = V(g_clean)$community) %>% 
  filter(str_detect(ID_Node, "_")) %>%
  separate(ID_Node, into = c("Negeri_Asal", "Daerah_Asal"), sep = "_") %>%
  mutate(Daerah_Upper = str_trim(str_to_upper(Daerah_Asal))) %>%
  mutate(Daerah_Upper = case_when(
    # Standardkan nama supaya match dengan peta nanti
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

# B. Download Peta Malaysia (GADM)
peta_daerah_raw <- gadm(country = "MYS", level = 2, path = tempdir())
peta_daerah_sf <- st_as_sf(peta_daerah_raw) %>% 
  mutate(Daerah_Map = str_trim(str_remove_all(str_to_upper(NAME_2), "DISTRICT")))

# ------------------------------------------------------------------------------
# LANGKAH 4: TEKNIK PECAHKAN KL & PERLIS (VORONOI SPLIT)
# ------------------------------------------------------------------------------

# --- FIX 1: KUALA LUMPUR ---
poly_kl <- peta_daerah_sf %>% filter(NAME_1 == "Kuala Lumpur") %>% st_union()
titik_kl <- data.frame(
  Daerah_Map = c("SENTUL", "WANGSA MAJU", "DANG WANGI", "BRICKFIELDS", "CHERAS"),
  X = c(101.685, 101.730, 101.700, 101.680, 101.730), 
  Y = c(3.190,   3.200,   3.155,   3.120,   3.090)
) %>% st_as_sf(coords = c("X", "Y"), crs = st_crs(peta_daerah_sf))

voronoi_kl <- st_voronoi(st_union(titik_kl)) %>% st_collection_extract() %>% st_sf()
kl_split <- st_intersection(voronoi_kl %>% st_join(titik_kl), poly_kl)
kl_split$NAME_1 <- "Kuala Lumpur"
kl_split <- kl_split %>% select(Daerah_Map, NAME_1, geometry)

# --- FIX 2: PERLIS ---
poly_perlis <- peta_daerah_sf %>% filter(NAME_1 == "Perlis") %>% st_union()
titik_perlis <- data.frame(
  Daerah_Map = c("KANGAR", "ARAU", "PADANG BESAR"),
  X = c(100.198, 100.275, 100.296), Y = c(6.441, 6.435, 6.600)
) %>% st_as_sf(coords = c("X", "Y"), crs = st_crs(peta_daerah_sf))

voronoi_perlis <- st_voronoi(st_union(titik_perlis)) %>% st_collection_extract() %>% st_sf()
perlis_split <- st_intersection(voronoi_perlis %>% st_join(titik_perlis), poly_perlis)
perlis_split$NAME_1 <- "Perlis"
perlis_split <- perlis_split %>% select(Daerah_Map, NAME_1, geometry)

# --- GABUNG SEMULA (MERGE) ---
peta_daerah_fixed <- bind_rows(
  peta_daerah_sf %>% filter(NAME_1 != "Perlis" & NAME_1 != "Kuala Lumpur") %>% select(Daerah_Map, NAME_1, geometry), 
  perlis_split,
  kl_split
)

# Standardize Nama Daerah dalam Peta
peta_daerah_fixed <- peta_daerah_fixed %>%
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

# ------------------------------------------------------------------------------
# LANGKAH 5: PLOT PETA AKHIR (MAPPING)
# ------------------------------------------------------------------------------

# Join data spatial dengan data komuniti Louvain
peta_final_daerah <- left_join(peta_daerah_fixed, raw_data_komuniti, by = c("Daerah_Map" = "Daerah_Upper"))

# Buat Index untuk Label
data_berwarna_index <- peta_final_daerah %>%
  filter(!is.na(Komuniti_ID)) %>%
  arrange(Komuniti_ID, Daerah_Map) %>%
  mutate(Indeks_ID = row_number())

# Koordinat Label
label_index_coord <- data_berwarna_index %>% st_centroid() %>% mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2])
label_negeri_coord <- peta_daerah_fixed %>% group_by(NAME_1) %>% summarise(geometry = st_union(geometry)) %>% st_centroid() %>% mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2])

# Plot Peta
plot_map_index <- ggplot() +
  # Layer Dasar (Peta Kelabu)
  geom_sf(data = peta_daerah_fixed, fill = "#F0F0F0", color = "grey90", size = 0.2) +
  
  # Layer Berwarna (Ikut Komuniti)
  geom_sf(data = data_berwarna_index, aes(fill = factor(Komuniti_ID)), color = "white", size = 0.1) +
  
  # Label Negeri (Background)
  geom_text(data = label_negeri_coord, aes(x = X, y = Y, label = toupper(NAME_1)), color = "black", fontface = "bold", size = 2.5, alpha = 0.3, check_overlap = FALSE) +
  
  # Label Nombor (Index Daerah)
  geom_text(data = label_index_coord, aes(x = X, y = Y, label = Indeks_ID), color = "black", size = 2.2, fontface = "bold") +
  
  # --- GUNA WARNA MASTER (Sama dengan Louvain) ---
  scale_fill_manual(values = warna_final_daerah, name = "Kumpulan", na.translate = FALSE) +
  # -----------------------------------------------

theme_void() +
  labs(title = paste("Peta Indeks Komuniti Daerah:", target_kategori), 
       subtitle = "Nombor mewakili daerah (Rujuk Jadual)", 
       caption = "Sumber: Analisis Rangkaian")

print(plot_map_index)

# ------------------------------------------------------------------------------
# EXTRA: JADUAL RUJUKAN
# ------------------------------------------------------------------------------
Jadual_Indeks <- data_berwarna_index %>% 
  st_drop_geometry() %>% 
  select(Indeks_ID, Daerah_Map, Komuniti_ID, Negeri_Asal) %>% 
  arrange(Indeks_ID)

# Papar output
message(">>> Selesai. Semak plot Louvain dan Peta.")
head(Jadual_Indeks)
view (Jadual_Indeks)


