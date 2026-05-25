# Wczytanie bibliotek
library(readxl)
library(dplyr)
library(sf)
library(tmap)
library(stringr)
library(writexl)
library(clusterSim)
library(factoextra)
library(gridExtra)
library(ggplot2)
library(ggrepel)
library(ggforce)


#Wczytanie i przygotowanie danych
dane <- read_xlsx("dane_sport.xlsx", sheet = 1) %>%
  mutate(
    KOD = sprintf("%02d", as.numeric(KOD)),
    KOD = as.character(KOD),
    WOJEWÓDZTWO = tolower(WOJEWÓDZTWO),
    across(X1:X15, ~ round(.x, 2))
  )

# Wczytanie warstwy przestrzennej województw
woj_sf <- st_read("wojewodztwa/wojewodztwa.shp") %>%
  mutate(KOD = as.character(JPT_KOD_JE)) %>%
  dplyr::select(KOD, geometry)

# Połączenie danych statystycznych z warstwą przestrzenną
woj_n <- woj_sf %>%
  left_join(dane, by = "KOD")

## ============================================================
# Metoda TOPSIS
# ============================================================

# Utworzenie macierzy decyzyjnej
xij <- dane %>%
  dplyr::select(X1:X15) %>%
  as.matrix()

#Normalizacja macierzy xij
zij <- matrix(data = NA, nrow(xij), ncol(xij))

for (j in 1:15) {
  zij[, j] <- round(xij[, j] / sqrt(sum(xij[, j]^2)), 4)
}

# Przyjęcie jednakowych wag dla wszystkich zmiennych
wj <- rep(1, 15)

# Obliczenie ważonych wartości znormalizowanych
vij <- wj * zij

# Wyznaczenie wzorca i antywzorca
# Wszystkie zmienne mają charakter stymulant
apl <- apply(vij, 2, max)
ami <- apply(vij, 2, min)


# Obliczenie odległości euklidesowych od wzorca i antywzorca

dpl <- matrix(data = NA, nrow(xij), 1)
dmi <- matrix(data = NA, nrow(xij), 1)

for (i in 1:nrow(xij)) {
  dpl[i,1] <- sqrt(sum((vij[i,] - apl)^2))
  dmi[i,1] <- sqrt(sum((vij[i,] - ami)^2))
}

# Obliczenie syntetycznego miernika TOPSIS
TOPSIS <- round(dmi / (dmi + dpl), 4)
dane$TOPSIS <- as.vector(TOPSIS)


# Utworzenie rankingu TOPSIS
ranking_topsis <- dane %>%
  arrange(desc(TOPSIS)) %>%
  mutate(`Lp.` = row_number()) %>%
  mutate(Wynik = format(round(TOPSIS, 3), nsmall = 3)) %>%
  dplyr::select(`Lp.`, Województwo = WOJEWÓDZTWO, Wynik)

print(ranking_topsis)

# Dołączenie wyników TOPSIS do warstwy przestrzennej
woj_topsis <- woj_n %>%
  left_join(dane %>% dplyr::select(KOD, TOPSIS), by = "KOD")

# Przygotowanie klas do mapy TOPSIS na podstawie kwartyli
quantiles_topsis <- quantile(woj_topsis$TOPSIS, na.rm = TRUE)
formatted_quantiles_topsis <- format(round(quantiles_topsis, 3), nsmall = 3)

labels_topsis <- paste(
  formatted_quantiles_topsis[-length(formatted_quantiles_topsis)],
  formatted_quantiles_topsis[-1],
  sep = " - "
)

# Przygotowanie etykiet do mapy TOPSIS
woj_topsis$TOPSIS_label <- round(woj_topsis$TOPSIS, 3)

# Utworzenie mapy TOPSIS
topsis_map <- tm_shape(woj_topsis) +
  tm_polygons(
    col = "TOPSIS",
    border.col = "black",
    title = "TOPSIS",
    breaks = quantiles_topsis,
    labels = labels_topsis,
    palette = "YlOrRd"
  ) +
  tm_text("WOJEWÓDZTWO", size = 1.2, ymod = 0.7) +
  tm_text("TOPSIS_label", size = 1.0, ymod = -0.6)

topsis_map
# ============================================================
# Metoda GDM
# ============================================================

# Przygotowanie zbioru cech do metody GDM
x_gdm <- dane %>%
  dplyr::select(X1:X15)

# Wszystkie zmienne są stymulantami
typ_zmiennych <- rep("s", 15)

# Obliczenie rankingu metodą GDM
ranking_gdm <- pattern.GDM1(
  x_gdm,
  performanceVariable = typ_zmiennych,
  scaleType = "r",
  nomOptValues = NULL,
  normalization = "n4",
  weightsType = "equal",
  patternType = "upper"
)

# Dodanie wyników GDM do danych
dane$GDM <- round(ranking_gdm$distances, 4)

# Utworzenie rankingu GDM
# Mniejsza odległość od wzorca oznacza lepszą pozycję
ranking_gdm_tabela <- dane %>%
  arrange(GDM) %>%
  mutate(`Lp.` = row_number()) %>%
  mutate(Wynik = format(round(GDM, 3), nsmall = 3)) %>%
  dplyr::select(`Lp.`, Województwo = WOJEWÓDZTWO, Wynik)

print(ranking_gdm_tabela)


# Dołączenie wyników GDM do warstwy przestrzennej
woj_gdm <- woj_n %>%
  left_join(dane %>% dplyr::select(KOD, GDM), by = "KOD")

# Przygotowanie klas do mapy GDM na podstawie kwartyli
quantiles_gdm <- quantile(woj_gdm$GDM, na.rm = TRUE)
formatted_quantiles_gdm <- format(round(quantiles_gdm, 3), nsmall = 3)

labels_gdm <- paste(
  formatted_quantiles_gdm[-length(formatted_quantiles_gdm)],
  formatted_quantiles_gdm[-1],
  sep = " - "
)

# Przygotowanie etykiet do mapy GDM
woj_gdm$GDM_label <- round(woj_gdm$GDM, 3)

# Utworzenie mapy GDM
gdm_map <- tm_shape(woj_gdm) +
  tm_polygons(
    col = "GDM",
    border.col = "black",
    title = "GDM",
    breaks = quantiles_gdm,
    labels = labels_gdm,
    palette = "YlOrRd"
  ) +
  tm_text("WOJEWÓDZTWO", size = 1.2, ymod = 0.7) +
  tm_text("GDM_label", size = 1.0, ymod = -0.6)

gdm_map



# ============================================================
#  Porównanie rankingów TOPSIS i GDM
# ============================================================

# Utworzenie tabeli z pozycjami rankingowymi
df_rank <- dane %>%
  mutate(
    Województwo = WOJEWÓDZTWO,
    
    # TOPSIS: im większa wartość miernika, tym lepsza pozycja
    TOPSIS_rank = rank(-TOPSIS, ties.method = "average"),
    
    # GDM: im mniejsza odległość od wzorca, tym lepsza pozycja
    GDM_rank = rank(GDM, ties.method = "average")
  ) %>%
  dplyr::select(Województwo, TOPSIS_rank, GDM_rank)

# Przygotowanie danych do wykresu
df_long <- df_rank %>%
  tidyr::pivot_longer(
    cols = c("TOPSIS_rank", "GDM_rank"),
    names_to = "Metoda",
    values_to = "Pozycja"
  )

# Zmiana nazw metod na wykresie
df_long$Metoda <- dplyr::recode(
  df_long$Metoda,
  "TOPSIS_rank" = "TOPSIS",
  "GDM_rank" = "GDM"
)


df_long$Metoda <- factor(df_long$Metoda, levels = c("TOPSIS", "GDM"))

# Kolejność województw według rankingu TOPSIS
order_TOPSIS <- df_rank %>%
  arrange(TOPSIS_rank) %>%
  pull(Województwo)

df_long$Województwo <- factor(df_long$Województwo, levels = rev(order_TOPSIS))

# Liczba województw
N <- nrow(df_rank)

# Utworzenie wykresu porównawczego
plot_porownanie <- ggplot(df_long, aes(x = Metoda, y = Pozycja, group = Województwo)) +
  geom_hline(yintercept = 1:N, color = "grey85", linetype = "dashed") +
  geom_line(aes(color = Województwo), size = 0.8, alpha = 0.9) +
  geom_point(aes(color = Województwo), size = 2.5) +
  scale_y_reverse(breaks = 1:N) +
  labs(
    x = "Metoda rankingowania",
    y = "Pozycja w rankingu (1 = najlepsza)",
    title = "Porównanie rankingów województw metodami TOPSIS i GDM"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  )

# Dodanie etykiet województw po lewej i prawej stronie wykresu
plot_porownanie <- plot_porownanie +
  geom_text(
    data = df_long %>% filter(Metoda == "TOPSIS"),
    aes(label = Województwo, color = Województwo),
    hjust = 1.1,
    size = 4.5
  ) +
  geom_text(
    data = df_long %>% filter(Metoda == "GDM"),
    aes(label = Województwo, color = Województwo),
    hjust = -0.1,
    size = 4.5
  ) +
  expand_limits(x = c(0.7, 2.3))

# Wyświetlenie wykresu
plot_porownanie


# Korelacja rang Spearmana między rankingami TOPSIS i GDM
korelacja_spearman <- cor(df_rank$TOPSIS_rank, df_rank$GDM_rank, method = "spearman")
korelacja_spearman




# ============================================================
# Metoda Warda
# ============================================================

# Przygotowanie danych do analizy skupień
x_ward <- dane %>%
  dplyr::select(X1:X15)

nazwy_wojewodztw <- tolower(dane$WOJEWÓDZTWO)

# Standaryzacja zmiennych diagnostycznych
x_ward_stand <- scale(x_ward)

# Wyznaczenie macierzy odległości euklidesowych
macierz_odleglosci <- dist(
  x_ward_stand,
  method = "euclidean",
  diag = TRUE,
  upper = TRUE
)

# Przeprowadzenie hierarchicznej analizy skupień metodą Warda
ward_clustering <- hclust(macierz_odleglosci, method = "ward.D2")
ward_clustering$labels <- nazwy_wojewodztw

# Utworzenie dendrogramu
plot(
  ward_clustering,
  main = "Dendrogram - metoda Warda",
  xlab = "Województwa",
  ylab = "Odległość",
  cex = 0.8,
  hang = -1,
  sub = ""
)

# Ustalenie liczby skupień na podstawie kryterium Mojeny
wysokosci <- ward_clustering$height
srednia_wys <- mean(wysokosci)
odch_std_wys <- sd(wysokosci)

k_mojena <- 1.25
prog_mojena <- srednia_wys + k_mojena * odch_std_wys
liczba_skupien <- sum(wysokosci > prog_mojena) + 1
liczba_skupien

# Utworzenie dendrogramu z zaznaczonymi skupieniami
plot(
  ward_clustering,
  main = "Dendrogram - metoda Warda",
  xlab = "Województwa",
  ylab = "Odległość",
  cex = 0.8,
  hang = -1,
  sub = ""
)

rect.hclust(ward_clustering, k = liczba_skupien, border = "blue")

# Przypisanie województw do skupień
grupy_ward <- cutree(ward_clustering, k = liczba_skupien)

wyniki_ward <- data.frame(
  Województwo = dane$WOJEWÓDZTWO,
  Skupienie = grupy_ward
) %>%
  arrange(Skupienie)

print(wyniki_ward)

# Zapis wyników metody Warda do pliku Excel
write_xlsx(wyniki_ward, "skupienia_ward_wojewodztwa.xlsx")

# Przygotowanie tabeli z wynikami grupowania metodą Warda
wojewodztwa_ward <- dane %>%
  dplyr::select(KOD, WOJEWÓDZTWO) %>%
  mutate(cluster = as.factor(grupy_ward))

# Połączenie wyników grupowania z warstwą przestrzenną
woj_ward_sf <- woj_sf %>%
  left_join(wojewodztwa_ward, by = "KOD")

# Opcjonalna zmiana numeracji skupień na potrzeby kartogramu
woj_ward_sf$cluster <- dplyr::recode(
  as.character(woj_ward_sf$cluster),
  "1" = "2",
  "2" = "1",
  "3" = "3"
)

woj_ward_sf$cluster <- factor(
  woj_ward_sf$cluster,
  levels = c("1", "2", "3")
)

# Utworzenie kartogramu skupień metodą Warda
metoda_warda_map <- tm_shape(woj_ward_sf) +
  tm_polygons(
    col = "cluster",
    border.col = "black",
    title = "Skupienia",
    palette = "YlOrRd"
  ) +
  tm_text("WOJEWÓDZTWO", size = 1.2) +
  tm_layout(
    frame = TRUE,
    bg.color = "white",
    legend.outside = TRUE,
    legend.bg.color = "white",
    legend.bg.alpha = 1,
    legend.frame = TRUE
  )

metoda_warda_map


# ============================================================
# Analiza skupień metodą k-średnich
# ============================================================

# Przygotowanie danych do analizy k-średnich
x_kmeans <- dane %>%
  dplyr::select(X1:X15)

# Standaryzacja zmiennych diagnostycznych
x_kmeans_stand <- scale(x_kmeans)

# Wyznaczenie optymalnej liczby skupień - metoda łokcia
p1 <- fviz_nbclust(
  x_kmeans_stand,
  FUNcluster = kmeans,
  method = "wss",
  k.max = 10
) +
  labs(
    x = "Liczba skupień",
    title = "Metoda łokcia - optymalna liczba skupień",
    subtitle = "Szukamy zgięcia krzywej",
    y = "Wewnątrzgrupowa suma kwadratów"
  ) +
  geom_vline(xintercept = 3, linetype = 2, color = "red")

# Wyznaczenie optymalnej liczby skupień - metoda sylwetki
p2 <- fviz_nbclust(
  x_kmeans_stand,
  FUNcluster = kmeans,
  method = "silhouette",
  k.max = 10
) +
  labs(
    x = "Liczba skupień",
    title = "Metoda sylwetki - optymalna liczba skupień",
    subtitle = "Wybieramy k z największą wartością",
    y = "Średni współczynnik sylwetki"
  )

# Wyświetlenie wykresów pomocniczych
grid.arrange(p1, p2, ncol = 2, nrow = 1)

# Przyjęcie optymalnej liczby skupień
k_optymalne <- 3

# Przeprowadzenie analizy k-średnich
set.seed(123)
kmeans_wynik <- kmeans(
  x_kmeans_stand,
  centers = k_optymalne,
  iter.max = 100,
  nstart = 25
)

# Dodanie numerów skupień do danych
dane$Kmeans <- kmeans_wynik$cluster

# Zmiana numeracji skupień na potrzeby prezentacji wyników
dane$Kmeans_nowe <- recode(
  dane$Kmeans,
  `2` = 1,
  `3` = 2,
  `1` = 3
)

# Konwersja numerów skupień do typu faktor
dane$Kmeans_nowe <- factor(
  dane$Kmeans_nowe,
  levels = c(1, 2, 3),
  labels = c("1", "2", "3")
)

# Utworzenie tabeli z wynikami metody k-średnich
wyniki_kmeans <- dane %>%
  dplyr::select(Województwo = WOJEWÓDZTWO, Skupienie = Kmeans_nowe) %>%
  arrange(Skupienie)

print(wyniki_kmeans)

# Zapis wyników metody k-średnich do pliku Excel
write_xlsx(wyniki_kmeans, "skupienia_kmeans_wojewodztwa.xlsx")

# Lista województw w poszczególnych skupieniach
for (i in levels(dane$Kmeans_nowe)) {
  cat("Skupienie", i, ":\n")
  wojewodztwa_w_skupieniu <- dane %>%
    filter(Kmeans_nowe == i) %>%
    pull(WOJEWÓDZTWO)
  cat(paste(wojewodztwa_w_skupieniu, collapse = ", "), "\n\n")
}

# Dołączenie wyników grupowania k-średnich do warstwy przestrzennej
woj_kmeans <- woj_sf %>%
  left_join(
    dane %>% dplyr::select(KOD, WOJEWÓDZTWO, Kmeans_nowe),
    by = "KOD"
  )

# Utworzenie kartogramu skupień metodą k-średnich
mapa_kmeans <- tm_shape(woj_kmeans) +
  tm_polygons(
    col = "Kmeans_nowe",
    border.col = "black",
    title = "Skupienia",
    palette = "YlOrRd",
    textNA = ""
  ) +
  tm_text("WOJEWÓDZTWO", size = 1.2) +
  tm_layout(
    frame = TRUE,
    bg.color = "white",
    legend.outside = TRUE,
    legend.bg.color = "white",
    legend.bg.alpha = 1,
    legend.frame = TRUE
  )

mapa_kmeans

# Wizualizacja skupień k-średnich w przestrzeni dwóch pierwszych składowych PCA
pca_result <- prcomp(x_kmeans_stand, scale. = FALSE)

pca_coords <- as.data.frame(pca_result$x[, 1:2])
pca_coords$WOJEWÓDZTWO <- dane$WOJEWÓDZTWO
pca_coords$Skupienie <- as.factor(dane$Kmeans_nowe)

p3 <- ggplot(pca_coords, aes(x = PC1, y = PC2, color = Skupienie)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_mark_hull(
    aes(fill = Skupienie, label = Skupienie),
    concavity = 0,
    expand = unit(2, "mm"),
    alpha = 0.12,
    show.legend = FALSE
  ) +
  geom_text_repel(
    aes(label = WOJEWÓDZTWO),
    size = 7.0,
    box.padding = 0.8,
    point.padding = 0.4,
    segment.color = "grey40",
    segment.size = 0.4,
    max.overlaps = Inf,
    force = 3,
    force_pull = 0.1,
    min.segment.length = 0,
    seed = 42
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Skupienia województw - analiza k-średnich",
    subtitle = paste0(
      "k = ", k_optymalne, " skupienia | ",
      nrow(dane), " województw"
    ),
    x = paste0(
      "Składowa główna 1 (",
      round(summary(pca_result)$importance[2, 1] * 100, 1),
      "%)"
    ),
    y = paste0(
      "Składowa główna 2 (",
      round(summary(pca_result)$importance[2, 2] * 100, 1),
      "%)"
    ),
    color = "Skupienie",
    fill = "Skupienie"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray30"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p3)
