# Cria a coluna de regiões:
dados_tcc <- dados_tcc %>%
  mutate(nome_regiao = case_when(
    nome_uf %in% c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins") ~ "Norte",
    nome_uf %in% c("Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", 
                   "Pernambuco", "Alagoas", "Sergipe", "Bahia") ~ "Nordeste",
    nome_uf %in% c("Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo") ~ "Sudeste",
    nome_uf %in% c("Paraná", "Santa Catarina", "Rio Grande do Sul") ~ "Sul",
    nome_uf %in% c("Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal") ~ "Centro-Oeste",
    TRUE ~ NA_character_
  ))

# Cria um df com as coordenadas dos extremos de cada região como referência:
coord <- data.frame(nome_regiao = c("Sudeste", "Nordeste", "Norte", "Centro-Oeste", "Sul"),
                    xmin = c(-52.83681, -48.36894, -73.43913, -61.00999, -57.33525),
                    xmax = c(-39.09629, -32.42409, -46.41051, -46.06701, -48.36673),
                    ymin = c(-25.01837, -18.0592, -13.47488, -23.9427, -33.65253),
                    ymax = c(-14.38569, -1.35007, 4.68533, -8.757918, -22.57382))

#-------------------------Taxa de Mortalidade Materna---------------------------

# 2010
tmm2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = mortMat2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkblue",
                      limits = c(10,1000),
                      breaks = c(1,0,250,500,750,1000),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("01_tmm2010.png", tmm2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# 2022
tmm2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = mortMat2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkblue",
                      limits = c(10,1000),
                      breaks = c(1,0,250,500,750,1000),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("01_tmm2022.png", tmm2022,
       width = 16, height = 12, units = "in", dpi = 1200)

#                              ##################                               
#                                   Sudeste                                     
#                              ##################                               

# 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Sudeste")) +
  geom_sf(aes(geometry = geometry, fill = mortMat2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred", 
                      na.value = "grey85",
                      name = NULL) +
  labs(title = "2010") +
  coord_sf(xlim = c(-54,-39), ylim = c(-26,-14), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 30, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Sudeste")) +
  geom_sf(aes(geometry = geometry, fill = mortMat2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred", 
                      na.value = "grey85",
                      name = NULL) +
  labs(title = "2022") +
  coord_sf(xlim = c(-54,-39), ylim = c(-26,-14), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 30, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                   Nordeste                                    
#                              ##################                               

# 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Nordeste")) +
  geom_sf(aes(geometry = geometry, fill = mortMat2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred", 
                      na.value = "grey85",
                      name = NULL) +
  labs(title = "2010") +
  coord_sf(xlim = c(-49,-32), ylim = c(-19,-1), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 5, b = 30, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Nordeste")) +
  geom_sf(aes(geometry = geometry, fill = mortMat2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred", 
                      na.value = "grey85",
                      name = NULL) +
  labs(title = "2022") +
  coord_sf(xlim = c(-49,-32), ylim = c(-19,-1), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 5, b = 30, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                     Norte                                     
#                              ##################                               

# 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Norte")) +
  geom_sf(aes(geometry = geometry, fill = mortMat2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred", 
                      na.value = "grey85",
                      name = NULL) +
  labs(title = "2010") +
  coord_sf(xlim = c(-76,-44), ylim = c(-14,6), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Norte")) +
  geom_sf(aes(geometry = geometry, fill = mortMat2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred", 
                      na.value = "grey85",
                      name = NULL) +
  labs(title = "2022") +
  coord_sf(xlim = c(-76,-44), ylim = c(-14,6), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                 Centro-Oeste                                  
#                              ##################                               

# 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste")) +
  geom_sf(aes(geometry = geometry, fill = mortMat2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred", 
                      na.value = "grey85",
                      name = NULL) +
  labs(title = "2010") +
  coord_sf(xlim = c(-63,-44), ylim = c(-26,-7), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste")) +
  geom_sf(aes(geometry = geometry, fill = mortMat2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred", 
                      na.value = "grey85",
                      name = NULL) +
  labs(title = "2022") +
  coord_sf(xlim = c(-63,-44), ylim = c(-26,-7), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                     Sul                                       
#                              ##################                               

# 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Sul")) +
  geom_sf(aes(geometry = geometry, fill = mortMat2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred", 
                      na.value = "grey85",
                      name = NULL) +
  labs(title = "2010") +
  coord_sf(xlim = c(-60,-46), ylim = c(-34,-22), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Sul")) +
  geom_sf(aes(geometry = geometry, fill = mortMat2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred", 
                      na.value = "grey85",
                      name = NULL) +
  labs(title = "2022") +
  coord_sf(xlim = c(-60,-46), ylim = c(-34,-22), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#-----------------------Taxa de Gravidez na Adolescência------------------------

# 2010
tga2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = natAdolesc2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred",
                      limits = c(1,250),
                      breaks = c(1,75,175,250),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("02_tga2010.png", tga2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# 2022
tga2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = natAdolesc2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred",
                      limits = c(1,250),
                      breaks = c(1,75,175,250),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("02_tga2022.png", tga2022,
       width = 16, height = 12, units = "in", dpi = 1200)

escalaTGA <- scale_fill_gradient(
  low = "bisque", high = "darkgreen",
  limits = c(1,250),
  breaks = c(1,75,175,250),
  oob = scales::squish,       
  na.value = "grey85",
  name = NULL)

# escalaTGA2022 <- scale_fill_gradient(
#   low = "bisque", high = "darkgreen",
#   limits = range(dados_tcc$natAdolesc2022, na.rm = TRUE),
#   breaks = seq(min(dados_tcc$natAdolesc2022, na.rm = TRUE),
#                max(dados_tcc$natAdolesc2022, na.rm = TRUE),
#                length.out = 5),
#   labels = scales::number_format(accuracy = 1),
#   oob = scales::squish,       
#   na.value = "grey85",
#   name = NULL)

#                              ##################                               
#                                   Sudeste                                     
#                              ##################                               

# 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Sudeste")) +
  geom_sf(aes(geometry = geometry, fill = natAdolesc2010),
          color = "grey20", linewidth = 0.1) +
  escalaTGA +
  labs(title = "2010") +
  coord_sf(xlim = c(-54,-39), ylim = c(-26,-14), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 30, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Sudeste")) +
  geom_sf(aes(geometry = geometry, fill = natAdolesc2022),
          color = "grey20", linewidth = 0.1) +
  escalaTGA + 
  labs(title = "2022") +
  coord_sf(xlim = c(-54,-39), ylim = c(-26,-14), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 30, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                   Nordeste                                    
#                              ##################                               

# 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Nordeste")) +
  geom_sf(aes(geometry = geometry, fill = natAdolesc2010),
          color = "grey20", linewidth = 0.1) +
  escalaTGA + 
  labs(title = "2010") +
  coord_sf(xlim = c(-49,-32), ylim = c(-19,-1), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 5, b = 30, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Nordeste")) +
  geom_sf(aes(geometry = geometry, fill = natAdolesc2022),
          color = "grey20", linewidth = 0.1) +
  escalaTGA + 
  labs(title = "2022") +
  coord_sf(xlim = c(-49,-32), ylim = c(-19,-1), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 5, b = 30, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                     Norte                                     
#                              ##################                               

# 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Norte")) +
  geom_sf(aes(geometry = geometry, fill = natAdolesc2010),
          color = "grey20", linewidth = 0.1) +
  escalaTGA + 
  labs(title = "2010") +
  coord_sf(xlim = c(-76,-44), ylim = c(-14,6), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Norte")) +
  geom_sf(aes(geometry = geometry, fill = natAdolesc2022),
          color = "grey20", linewidth = 0.1) +
  escalaTGA + 
  labs(title = "2022") +
  coord_sf(xlim = c(-76,-44), ylim = c(-14,6), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                 Centro-Oeste                                  
#                              ##################                               

# 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste")) +
  geom_sf(aes(geometry = geometry, fill = natAdolesc2010),
          color = "grey20", linewidth = 0.1) +
  escalaTGA + 
  labs(title = "2010") +
  coord_sf(xlim = c(-63,-44), ylim = c(-26,-7), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste")) +
  geom_sf(aes(geometry = geometry, fill = natAdolesc2022),
          color = "grey20", linewidth = 0.1) +
  escalaTGA + 
  labs(title = "2022") +
  coord_sf(xlim = c(-63,-44), ylim = c(-26,-7), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                     Sul                                       
#                              ##################                               

# 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Sul")) +
  geom_sf(aes(geometry = geometry, fill = natAdolesc2010),
          color = "grey20", linewidth = 0.1) +
  escalaTGA + 
  labs(title = "2010") +
  coord_sf(xlim = c(-60,-46), ylim = c(-34,-22), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Sul")) +
  geom_sf(aes(geometry = geometry, fill = natAdolesc2022),
          color = "grey20", linewidth = 0.1) +
  escalaTGA + 
  labs(title = "2022") +
  coord_sf(xlim = c(-60,-46), ylim = c(-34,-22), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#--------------------------Participação no parlamento---------------------------

# Homem 2010
parlaHomem2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = p_homem2010*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#034402",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("03_parlaHomem2010.png", parlaHomem2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2010
parlaMulher2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = p_mulher2010*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#034402",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("03_parlaMulher2010.png", parlaMulher2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Homem 2022
parlaHomem2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = p_homem2022*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#034402",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("03_parlaHomem2022.png", parlaHomem2022,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2022
parlaMulher2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = p_mulher2022*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#034402",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("03_parlaMulher2022.png", parlaMulher2022,
       width = 16, height = 12, units = "in", dpi = 1200)

escalaParla <- scale_fill_gradient(
  low = "bisque", high = "darkblue",
  limits = c(0.001,1),
  breaks = c(0.001,0.25,0.5,0.75,1),
  labels = scales::number_format(accuracy = 0.01),
  oob = scales::squish,       
  na.value = "grey85",
  name = NULL)

#                              ##################                               
#                                   Sudeste                                     
#                              ##################                               

# Homem 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Sudeste")) +
  geom_sf(aes(geometry = geometry, fill = p_homem2010),
          color = "grey20", linewidth = 0.1) +
  escalaParla +
  labs(title = "2010") +
  coord_sf(xlim = c(-54,-39), ylim = c(-26,-14), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 30, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Sudeste")) +
  geom_sf(aes(geometry = geometry, fill = p_mulher2010),
          color = "grey20", linewidth = 0.1) +
  escalaParla +
  labs(title = "2010") +
  coord_sf(xlim = c(-54,-39), ylim = c(-26,-14), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 30, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Homem 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Sudeste")) +
  geom_sf(aes(geometry = geometry, fill = p_homem2022),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2022") +
  coord_sf(xlim = c(-54,-39), ylim = c(-26,-14), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 30, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Sudeste")) +
  geom_sf(aes(geometry = geometry, fill = p_mulher2022),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2022") +
  coord_sf(xlim = c(-54,-39), ylim = c(-26,-14), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 30, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                   Nordeste                                    
#                              ##################                               

# Homem 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Nordeste")) +
  geom_sf(aes(geometry = geometry, fill = p_homem2010),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2010") +
  coord_sf(xlim = c(-49,-32), ylim = c(-19,-1), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 5, b = 30, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Nordeste")) +
  geom_sf(aes(geometry = geometry, fill = p_mulher2010),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2010") +
  coord_sf(xlim = c(-49,-32), ylim = c(-19,-1), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 5, b = 30, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Homem 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Nordeste")) +
  geom_sf(aes(geometry = geometry, fill = p_homem2022),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2022") +
  coord_sf(xlim = c(-49,-32), ylim = c(-19,-1), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 5, b = 30, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Nordeste")) +
  geom_sf(aes(geometry = geometry, fill = p_mulher2022),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2022") +
  coord_sf(xlim = c(-49,-32), ylim = c(-19,-1), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 5, b = 30, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                     Norte                                     
#                              ##################                               

# Homem 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Norte")) +
  geom_sf(aes(geometry = geometry, fill = p_homem2010),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2010") +
  coord_sf(xlim = c(-76,-44), ylim = c(-14,6), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Norte")) +
  geom_sf(aes(geometry = geometry, fill = p_mulher2010),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2010") +
  coord_sf(xlim = c(-76,-44), ylim = c(-14,6), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Homem 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Norte")) +
  geom_sf(aes(geometry = geometry, fill = p_homem2022),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2022") +
  coord_sf(xlim = c(-76,-44), ylim = c(-14,6), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Norte")) +
  geom_sf(aes(geometry = geometry, fill = p_mulher2022),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2022") +
  coord_sf(xlim = c(-76,-44), ylim = c(-14,6), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                 Centro-Oeste                                  
#                              ##################                               

# Homem 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste")) +
  geom_sf(aes(geometry = geometry, fill = p_homem2010),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2010") +
  coord_sf(xlim = c(-63,-44), ylim = c(-26,-7), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste")) +
  geom_sf(aes(geometry = geometry, fill = p_mulher2010),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2010") +
  coord_sf(xlim = c(-63,-44), ylim = c(-26,-7), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Homem 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste")) +
  geom_sf(aes(geometry = geometry, fill = p_homem2022),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2022") +
  coord_sf(xlim = c(-63,-44), ylim = c(-26,-7), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste")) +
  geom_sf(aes(geometry = geometry, fill = p_mulher2022),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2022") +
  coord_sf(xlim = c(-63,-44), ylim = c(-26,-7), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                     Sul                                       
#                              ##################                               

# Homem 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Sul")) +
  geom_sf(aes(geometry = geometry, fill = p_homem2010),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2010") +
  coord_sf(xlim = c(-60,-46), ylim = c(-34,-22), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Sul")) +
  geom_sf(aes(geometry = geometry, fill = p_mulher2010),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2010") +
  coord_sf(xlim = c(-60,-46), ylim = c(-34,-22), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Homem 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Sul")) +
  geom_sf(aes(geometry = geometry, fill = p_homem2022),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2022") +
  coord_sf(xlim = c(-60,-46), ylim = c(-34,-22), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Sul")) +
  geom_sf(aes(geometry = geometry, fill = p_mulher2022),
          color = "grey20", linewidth = 0.1) +
  escalaParla + 
  labs(title = "2022") +
  coord_sf(xlim = c(-60,-46), ylim = c(-34,-22), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#-------------------------Educação secundária/superior--------------------------

# Homem 2010
educHomem2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = e_homem2010*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#D76C38",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("04_educHomem2010.png", educHomem2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2010
educMulher2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = e_mulher2010*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#D76C38",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("04_educMulher2010.png", educMulher2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Homem 2022
educHomem2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = e_homem2022*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#D76C38",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("04_educHomem2022.png", educHomem2022,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2022
educMulher2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = e_mulher2022*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#D76C38",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("04_educMulher2022.png", educMulher2022,
       width = 16, height = 12, units = "in", dpi = 1200)

escalaEduc <- scale_fill_gradient(
  low = "bisque", high = "black",
  limits = c(0.001,1),
  breaks = c(0.001,0.25,0.5,0.75,1),
  labels = scales::number_format(accuracy = 0.01),
  oob = scales::squish,       
  na.value = "grey85",
  name = NULL)

#                              ##################                               
#                                   Sudeste                                     
#                              ##################                               

# Homem 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Sudeste")) +
  geom_sf(aes(geometry = geometry, fill = e_homem2010),
          color = "grey20", linewidth = 0.1) +
  escalaEduc +
  labs(title = "2010") +
  coord_sf(xlim = c(-54,-39), ylim = c(-26,-14), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 30, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Sudeste")) +
  geom_sf(aes(geometry = geometry, fill = e_mulher2010),
          color = "grey20", linewidth = 0.1) +
  escalaEduc +
  labs(title = "2010") +
  coord_sf(xlim = c(-54,-39), ylim = c(-26,-14), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 30, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Homem 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Sudeste")) +
  geom_sf(aes(geometry = geometry, fill = e_homem2022),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2022") +
  coord_sf(xlim = c(-54,-39), ylim = c(-26,-14), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 30, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Sudeste")) +
  geom_sf(aes(geometry = geometry, fill = e_mulher2022),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2022") +
  coord_sf(xlim = c(-54,-39), ylim = c(-26,-14), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 30, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                   Nordeste                                    
#                              ##################                               

# Homem 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Nordeste")) +
  geom_sf(aes(geometry = geometry, fill = e_homem2010),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2010") +
  coord_sf(xlim = c(-49,-32), ylim = c(-19,-1), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 5, b = 30, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Nordeste")) +
  geom_sf(aes(geometry = geometry, fill = e_mulher2010),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2010") +
  coord_sf(xlim = c(-49,-32), ylim = c(-19,-1), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 5, b = 30, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Homem 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Nordeste")) +
  geom_sf(aes(geometry = geometry, fill = e_homem2022),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2022") +
  coord_sf(xlim = c(-49,-32), ylim = c(-19,-1), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 5, b = 30, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Nordeste")) +
  geom_sf(aes(geometry = geometry, fill = e_mulher2022),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2022") +
  coord_sf(xlim = c(-49,-32), ylim = c(-19,-1), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 5, b = 30, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                     Norte                                     
#                              ##################                               

# Homem 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Norte")) +
  geom_sf(aes(geometry = geometry, fill = e_homem2010),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2010") +
  coord_sf(xlim = c(-76,-44), ylim = c(-14,6), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Norte")) +
  geom_sf(aes(geometry = geometry, fill = e_mulher2010),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2010") +
  coord_sf(xlim = c(-76,-44), ylim = c(-14,6), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Homem 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Norte")) +
  geom_sf(aes(geometry = geometry, fill = e_homem2022),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2022") +
  coord_sf(xlim = c(-76,-44), ylim = c(-14,6), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Norte")) +
  geom_sf(aes(geometry = geometry, fill = e_mulher2022),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2022") +
  coord_sf(xlim = c(-76,-44), ylim = c(-14,6), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                 Centro-Oeste                                  
#                              ##################                               

# Homem 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste")) +
  geom_sf(aes(geometry = geometry, fill = e_homem2010),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2010") +
  coord_sf(xlim = c(-63,-44), ylim = c(-26,-7), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste")) +
  geom_sf(aes(geometry = geometry, fill = e_mulher2010),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2010") +
  coord_sf(xlim = c(-63,-44), ylim = c(-26,-7), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Homem 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste")) +
  geom_sf(aes(geometry = geometry, fill = e_homem2022),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2022") +
  coord_sf(xlim = c(-63,-44), ylim = c(-26,-7), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste")) +
  geom_sf(aes(geometry = geometry, fill = e_mulher2022),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2022") +
  coord_sf(xlim = c(-63,-44), ylim = c(-26,-7), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                     Sul                                       
#                              ##################                               

# Homem 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Sul")) +
  geom_sf(aes(geometry = geometry, fill = e_homem2010),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2010") +
  coord_sf(xlim = c(-60,-46), ylim = c(-34,-22), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Sul")) +
  geom_sf(aes(geometry = geometry, fill = e_mulher2010),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2010") +
  coord_sf(xlim = c(-60,-46), ylim = c(-34,-22), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Homem 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Sul")) +
  geom_sf(aes(geometry = geometry, fill = e_homem2022),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2022") +
  coord_sf(xlim = c(-60,-46), ylim = c(-34,-22), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Sul")) +
  geom_sf(aes(geometry = geometry, fill = e_mulher2022),
          color = "grey20", linewidth = 0.1) +
  escalaEduc + 
  labs(title = "2022") +
  coord_sf(xlim = c(-60,-46), ylim = c(-34,-22), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#-----------------------------Mercado de trabalho-------------------------------

# Homem 2010
trabHomem2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = t_homem2010*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#013A63",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("05_trabHomem2010.png", trabHomem2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2010
trabMulher2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = t_mulher2010*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#013A63",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("05_trabMulher2010.png", trabMulher2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Homem 2022
trabHomem2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = t_homem2022*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#013A63",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("05_trabHomem2022.png", trabHomem2022,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2022
trabMulher2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = t_mulher2022*100),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#013A63",
                      limits = c(0.1,100),
                      breaks = c(0.1,25,50,75,100),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("05_trabMulher2022.png", trabMulher2022,
       width = 16, height = 12, units = "in", dpi = 1200)

escalaTrab <- scale_fill_gradient(
  low = "bisque", high = "#013A63",
  limits = c(0.001,1),
  breaks = c(0.001,0.25,0.5,0.75,1),
  labels = scales::number_format(accuracy = 0.01),
  oob = scales::squish,       
  na.value = "grey85",
  name = NULL)

#                              ##################                               
#                                   Sudeste                                     
#                              ##################                               

# Homem 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Sudeste")) +
  geom_sf(aes(geometry = geometry, fill = t_homem2010),
          color = "grey20", linewidth = 0.1) +
  escalaTrab +
  labs(title = "2010") +
  coord_sf(xlim = c(-54,-39), ylim = c(-26,-14), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 30, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Sudeste")) +
  geom_sf(aes(geometry = geometry, fill = t_mulher2010),
          color = "grey20", linewidth = 0.1) +
  escalaTrab +
  labs(title = "2010") +
  coord_sf(xlim = c(-54,-39), ylim = c(-26,-14), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 30, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Homem 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Sudeste")) +
  geom_sf(aes(geometry = geometry, fill = t_homem2022),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2022") +
  coord_sf(xlim = c(-54,-39), ylim = c(-26,-14), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 30, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Sudeste")) +
  geom_sf(aes(geometry = geometry, fill = t_mulher2022),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2022") +
  coord_sf(xlim = c(-54,-39), ylim = c(-26,-14), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 30, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                   Nordeste                                    
#                              ##################                               

# Homem 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Nordeste")) +
  geom_sf(aes(geometry = geometry, fill = t_homem2010),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2010") +
  coord_sf(xlim = c(-49,-32), ylim = c(-19,-1), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 5, b = 30, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Nordeste")) +
  geom_sf(aes(geometry = geometry, fill = t_mulher2010),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2010") +
  coord_sf(xlim = c(-49,-32), ylim = c(-19,-1), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 5, b = 30, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Homem 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Nordeste")) +
  geom_sf(aes(geometry = geometry, fill = t_homem2022),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2022") +
  coord_sf(xlim = c(-49,-32), ylim = c(-19,-1), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 5, b = 30, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Nordeste")) +
  geom_sf(aes(geometry = geometry, fill = t_mulher2022),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2022") +
  coord_sf(xlim = c(-49,-32), ylim = c(-19,-1), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 5, b = 30, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                     Norte                                     
#                              ##################                               

# Homem 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Norte")) +
  geom_sf(aes(geometry = geometry, fill = t_homem2010),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2010") +
  coord_sf(xlim = c(-76,-44), ylim = c(-14,6), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Norte")) +
  geom_sf(aes(geometry = geometry, fill = t_mulher2010),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2010") +
  coord_sf(xlim = c(-76,-44), ylim = c(-14,6), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Homem 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Norte")) +
  geom_sf(aes(geometry = geometry, fill = t_homem2022),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2022") +
  coord_sf(xlim = c(-76,-44), ylim = c(-14,6), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Norte")) +
  geom_sf(aes(geometry = geometry, fill = t_mulher2022),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2022") +
  coord_sf(xlim = c(-76,-44), ylim = c(-14,6), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                 Centro-Oeste                                  
#                              ##################                               

# Homem 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste")) +
  geom_sf(aes(geometry = geometry, fill = t_homem2010),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2010") +
  coord_sf(xlim = c(-63,-44), ylim = c(-26,-7), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste")) +
  geom_sf(aes(geometry = geometry, fill = t_mulher2010),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2010") +
  coord_sf(xlim = c(-63,-44), ylim = c(-26,-7), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Homem 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste")) +
  geom_sf(aes(geometry = geometry, fill = t_homem2022),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2022") +
  coord_sf(xlim = c(-63,-44), ylim = c(-26,-7), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Centro-Oeste")) +
  geom_sf(aes(geometry = geometry, fill = t_mulher2022),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2022") +
  coord_sf(xlim = c(-63,-44), ylim = c(-26,-7), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 30, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#                              ##################                               
#                                     Sul                                       
#                              ##################                               

# Homem 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Sul")) +
  geom_sf(aes(geometry = geometry, fill = t_homem2010),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2010") +
  coord_sf(xlim = c(-60,-46), ylim = c(-34,-22), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2010:
ggplot(dados_tcc |> filter(nome_regiao == "Sul")) +
  geom_sf(aes(geometry = geometry, fill = t_mulher2010),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2010") +
  coord_sf(xlim = c(-60,-46), ylim = c(-34,-22), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Homem 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Sul")) +
  geom_sf(aes(geometry = geometry, fill = t_homem2022),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2022") +
  coord_sf(xlim = c(-60,-46), ylim = c(-34,-22), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

# Mulher 2022:
ggplot(dados_tcc |> filter(nome_regiao == "Sul")) +
  geom_sf(aes(geometry = geometry, fill = t_mulher2022),
          color = "grey20", linewidth = 0.1) +
  escalaTrab + 
  labs(title = "2022") +
  coord_sf(xlim = c(-60,-46), ylim = c(-34,-22), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 10),
    plot.margin = margin(t = 0, r = 35, b = 0, l = 5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 15)))

#--------------------------------Dimensão Saúde---------------------------------

# Mulher 2010
saudeMulher2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = saudeMulher2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkblue",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      labels = scales::number_format(accuracy = 0.01),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("06_saudeMulher2010.png", saudeMulher2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2022
saudeMulher2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = saudeMulher2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkblue",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      labels = scales::number_format(accuracy = 0.01),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("06_saudeMulher2022.png", saudeMulher2022,
       width = 16, height = 12, units = "in", dpi = 1200)

#----------------------------Dimensão Empoderamento-----------------------------

empodHomem2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = empodHomem2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#034402",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      labels = scales::number_format(accuracy = 0.01),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("07_empodHomem2010.png", empodHomem2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2010
empodMulher2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = empodMulher2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#034402",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      labels = scales::number_format(accuracy = 0.01),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("07_empodMulher2010.png", empodMulher2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Homem 2022
empodHomem2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = empodHomem2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#034402",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      labels = scales::number_format(accuracy = 0.01),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("07_empodHomem2022.png", empodHomem2022,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2022
empodMulher2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = empodMulher2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "#034402",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      labels = scales::number_format(accuracy = 0.01),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("07_empodMulher2022.png", empodMulher2022,
       width = 16, height = 12, units = "in", dpi = 1200)

#-------------------------Médias geométricas por gênero-------------------------

# Homem 2010
geomHomem2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = geomHomem2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "black",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("08_geomHomem2010.png", geomHomem2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2010
geomMulher2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = geomMulher2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "black",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("08_geomMulher2010.png", geomMulher2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# Homem 2022
geomHomem2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = geomHomem2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "black",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("08_geomHomem2022.png", geomHomem2022,
       width = 16, height = 12, units = "in", dpi = 1200)

# Mulher 2022
geomMulher2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = geomMulher2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "black",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("08_geomMulher2022.png", geomMulher2022,
       width = 16, height = 12, units = "in", dpi = 1200)

#---------------------------Gender Inequality Index-----------------------------

# 2010
gii2010 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = gii2010),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2010") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("09_gii2010.png", gii2010,
       width = 16, height = 12, units = "in", dpi = 1200)

# 2022
gii2022 <- ggplot(dados_tcc) +
  geom_sf(aes(geometry = geometry, fill = gii2022),
          color = "grey20", linewidth = 0.1) +
  scale_fill_gradient(low = "bisque", high = "darkred",
                      limits = c(0.001,1),
                      breaks = c(0.001,0.25,0.5,0.75,1),
                      oob = scales::squish,
                      na.value = "grey85",
                      name = NULL,
                      guide = guide_colorbar(
                        ticks.linewidth = 1,
                        ticks.colour = "white")) +
  labs(title = "2022") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5,
                              margin = margin(b = 15)),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text  = element_text(size = 20),
    legend.key.height = unit(1.5, "cm"), 
    legend.key.width  = unit(1.5, "cm"),
    plot.margin = margin(t = 40, r = 30, b = 0, l = 5),
    legend.position = c(0.97, 0.5))

ggsave("09_gii2022.png", gii2022,
       width = 16, height = 12, units = "in", dpi = 1200)