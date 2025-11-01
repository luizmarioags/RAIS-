# ============================================================================
# 0. PACOTES
# ============================================================================
library(data.table)
library(dplyr)
library(ggplot2)
library(scales)    # formatar eixos (R$, milhões)
library(sf)
library(geobr)
library(patchwork) # combinar gráficos (opcional)

# ============================================================================
# 1. CONSTRUÇÃO DA BASE DE DADOS
# Converter remunerações para numérico e criar salário aproximado
# ============================================================================
# (Pressupõe que o objeto `df` já está na memória)

# Converter remunerações para numérico
rem_cols <- grep("^valor_remuneracao_", names(df), value = TRUE)
if (length(rem_cols)) {
  df[, (rem_cols) := lapply(.SD, function(x) suppressWarnings(as.numeric(x))), .SDcols = rem_cols]
}
df[, valor_salario_contratual := suppressWarnings(as.numeric(valor_salario_contratual))]

# Estratégia: média anual (valor_remuneracao_media) > média 12 meses > salário contratual
if (length(rem_cols)) {
  df[, media12 := rowMeans(.SD, na.rm = TRUE), .SDcols = rem_cols]
} else {
  df[, media12 := NA_real_]
}

df[, salario_aproximado := fifelse(!is.na(valor_remuneracao_media) & valor_remuneracao_media > 0,
                                   valor_remuneracao_media, 
                                   media12)]

# Se ainda estiver NA ou não finito, usar salário contratual
df[is.na(salario_aproximado) | !is.finite(salario_aproximado), salario_aproximado := valor_salario_contratual]

# Remover coluna temporária
df[, media12 := NULL]

# ============================================================================
# 1.1. CRIAÇÃO DE VARIÁVEIS AUXILIARES
# ============================================================================
df[, `:=`(
  # 1) Regime de trabalho
  eh_clt         = fifelse(grepl("CLT", tipo_vinculo, ignore.case = TRUE), 1L, 0L),
  eh_estatutario = fifelse(grepl("ESTATUTARIO", tipo_vinculo, ignore.case = TRUE), 1L, 0L),
  
  # 2) Setor público (metodologia corrigida)
  eh_adm_publica = fcase(
    subsetor_ibge %in% c("1", "2"), 1L,
    cnae_1_descricao_secao == "Administração pública, defesa e seguridade social", 1L,
    default = 0L
  ),
  
  # 3) Empresas estatais (subsetor 3 e 4 do IBGE)
  eh_empresa_estatal = fifelse(subsetor_ibge %in% c("3", "4"), 1L, 0L),
  
  # 4) Grupo de raça
  raca_grupo = fcase(
    raca_cor == "Branca", "Branco",
    raca_cor %in% c("Preta", "Parda"), "Negro",
    default = "Outro"
  ),
  
  # 5) Salário válido (corte inferior para depuração de outliers e zeros)
  salario_valido = fifelse(!is.na(salario_aproximado) & salario_aproximado >= 500, salario_aproximado, NA_real_)
)]

# ============================================================================
# 1.2. CATEGORIAS MUTUAMENTE EXCLUSIVAS
# ============================================================================
cat("Criando categorias mutuamente exclusivas...\n")
df[, categoria_emprego := fcase(
  eh_adm_publica == 1,          "Setor Público",
  eh_empresa_estatal == 1,      "Empresa Estatal",
  eh_clt == 1,                  "CLT Privado",
  default =                     "Outros"
)]

# ============================================================================
# 1.3. AGREGAÇÃO MUNICÍPIO-ANO (LÓGICA CORRIGIDA)
# ============================================================================
cat("\nAgregando dados por município-ano (Versão Corrigida)...\n")

df_filtrado_salario <- df[!is.na(salario_valido)]

base_geral_municipio <- df_filtrado_salario[, .(
  emprego_total        = .N,
  emprego_clt_privado  = sum(categoria_emprego == "CLT Privado"),
  emprego_publico      = sum(categoria_emprego == "Setor Público"),
  salario_medio_total  = mean(salario_valido, na.rm = TRUE),
  salario_medio_clt    = mean(salario_valido[categoria_emprego == "CLT Privado"], na.rm = TRUE),
  salario_medio_publico= mean(salario_valido[categoria_emprego == "Setor Público"], na.rm = TRUE)
), by = .(ano, id_municipio, id_municipio_nome, sigla_uf)]

base_raca_municipio <- df_filtrado_salario[raca_grupo %in% c("Branco", "Negro"), .(
  n_brancos_total     = sum(raca_grupo == "Branco"),
  n_negros_total      = sum(raca_grupo == "Negro"),
  n_brancos_clt       = sum(raca_grupo == "Branco" & categoria_emprego == "CLT Privado"),
  n_negros_clt        = sum(raca_grupo == "Negro"  & categoria_emprego == "CLT Privado"),
  n_brancos_publico   = sum(raca_grupo == "Branco" & categoria_emprego == "Setor Público"),
  n_negros_publico    = sum(raca_grupo == "Negro"  & categoria_emprego == "Setor Público"),
  salario_branco_total   = mean(salario_valido[raca_grupo == "Branco"], na.rm = TRUE),
  salario_branco_clt     = mean(salario_valido[raca_grupo == "Branco" & categoria_emprego == "CLT Privado"], na.rm = TRUE),
  salario_branco_publico = mean(salario_valido[raca_grupo == "Branco" & categoria_emprego == "Setor Público"], na.rm = TRUE),
  salario_negro_total    = mean(salario_valido[raca_grupo == "Negro"], na.rm = TRUE),
  salario_negro_clt      = mean(salario_valido[raca_grupo == "Negro"  & categoria_emprego == "CLT Privado"], na.rm = TRUE),
  salario_negro_publico  = mean(salario_valido[raca_grupo == "Negro"  & categoria_emprego == "Setor Público"], na.rm = TRUE)
), by = .(ano, id_municipio)]

base_municipio <- merge(
  base_geral_municipio,
  base_raca_municipio,
  by = c("ano", "id_municipio")
)

base_municipio[, `:=`(
  razao_total   = salario_branco_total   / salario_negro_total,
  razao_clt     = salario_branco_clt     / salario_negro_clt,
  razao_publico = salario_branco_publico / salario_negro_publico
)]

base_municipio[, `:=`(
  razao_total   = fifelse(is.finite(razao_total),   razao_total,   NA_real_),
  razao_clt     = fifelse(is.finite(razao_clt),     razao_clt,     NA_real_),
  razao_publico = fifelse(is.finite(razao_publico), razao_publico, NA_real_)
)]

cat("Base agregada municipal corrigida criada com", nrow(base_municipio), "observações\n")
saveRDS(base_municipio, "base_municipio_ano.rds")
data.table::fwrite(base_municipio, "base_municipio_ano.csv")

# ============================================================================
# 2. TABELAS: ÍNDICES TEMPORAIS NACIONAIS
# ============================================================================
cat("\nCalculando tabelas temporais nacionais (médias ponderadas)...\n")

agregados_temporais <- base_municipio[, .(
  emprego_total_nacional   = sum(emprego_total,       na.rm = TRUE),
  emprego_clt_nacional     = sum(emprego_clt_privado, na.rm = TRUE),
  emprego_publico_nacional = sum(emprego_publico,     na.rm = TRUE),
  
  salario_medio_total_nacional   = weighted.mean(salario_medio_total,   w = emprego_total,       na.rm = TRUE),
  salario_medio_clt_nacional     = weighted.mean(salario_medio_clt,     w = emprego_clt_privado, na.rm = TRUE),
  salario_medio_publico_nacional = weighted.mean(salario_medio_publico, w = emprego_publico,     na.rm = TRUE),
  
  razao_media_total_nacional   = weighted.mean(razao_total,   w = (n_brancos_total   + n_negros_total),   na.rm = TRUE),
  razao_media_clt_nacional     = weighted.mean(razao_clt,     w = (n_brancos_clt     + n_negros_clt),     na.rm = TRUE),
  razao_media_publico_nacional = weighted.mean(razao_publico, w = (n_brancos_publico + n_negros_publico), na.rm = TRUE)
), by = ano]

setorder(agregados_temporais, ano)
cat("\n=== TABELA TEMPORAL: EMPREGO TOTAL NACIONAL ===\n")
print(agregados_temporais[, .(ano, emprego_total_nacional, emprego_clt_nacional, emprego_publico_nacional)])
cat("\n=== TABELA TEMPORAL: SALÁRIO MÉDIO NACIONAL (PONDERADO) ===\n")
print(agregados_temporais[, .(ano, salario_medio_total_nacional, salario_medio_clt_nacional, salario_medio_publico_nacional)])
cat("\n=== TABELA TEMPORAL: RAZÃO SALARIAL BRANCO/NEGRO NACIONAL (PONDERADA) ===\n")
print(agregados_temporais[, .(ano, razao_media_total_nacional, razao_media_clt_nacional, razao_media_publico_nacional)])

# ============================================================================
# 3. VISUALIZAÇÃO: EVOLUÇÃO TEMPORAL (RAZÃO SALARIAL)
# ============================================================================
cat("\nGerando gráfico de evolução temporal (Razão Salarial)...\n")
evolucao_long_razao <- melt(
  agregados_temporais[, .(ano, razao_media_clt_nacional, razao_media_publico_nacional)], 
  id.vars = "ano", variable.name = "categoria", value.name = "razao"
)
evolucao_long_razao[, categoria := fcase(
  categoria == "razao_media_clt_nacional",     "CLT Privado",
  categoria == "razao_media_publico_nacional", "Setor Público"
)]
evolucao_long_razao[, categoria := factor(categoria, levels = c("CLT Privado", "Setor Público"))]

anos_disponiveis <- sort(unique(base_municipio$ano))

ggplot(evolucao_long_razao, aes(x = ano, y = razao, color = categoria, group = categoria)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
  scale_color_manual(values = c("CLT Privado" = "#E67E22", "Setor Público" = "#2ECC71")) +
  scale_x_continuous(breaks = anos_disponiveis) +
  scale_y_continuous(breaks = seq(1.0, 1.5, 0.05), limits = c(0.95, 1.5)) +
  labs(
    title = "Evolução da Desigualdade Salarial Racial por Categoria",
    subtitle = "Razão média ponderada (por município) entre salário de brancos e negros",
    x = "Ano", y = "Razão Salário Médio Branco / Negro", color = "Categoria",
    caption = "Fonte: RAIS. (Sem filtro de amostra mínima).\nCLT Privado = setor privado. Setor Público = administração direta."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave("evolucao_razao_salarial_sem_filtro.png", width = 12, height = 8, dpi = 300)

# ============================================================================
# 4. VISUALIZAÇÃO: DISTRIBUIÇÃO RAZÃO SALARIAL (DENSIDADE, HISTOGRAMA, BOXPLOT)
# ============================================================================
cat("\nGerando gráficos de distribuição (Razão Salarial) para o ano mais recente...\n")
ano_mapa  <- max(base_municipio$ano)
dados_mapa <- base_municipio[ano == ano_mapa]

dados_dist_long <- melt(
  dados_mapa[, .(id_municipio, razao_clt, razao_publico)], 
  id.vars = "id_municipio", measure.vars = c("razao_clt", "razao_publico"),
  variable.name = "categoria", value.name = "razao"
)
dados_dist_long[, categoria := fcase(
  categoria == "razao_clt",     "CLT Privado",
  categoria == "razao_publico", "Setor Público"
)]
dados_dist_long[, categoria := factor(categoria, levels = c("CLT Privado", "Setor Público"))]
dados_dist_long_filtrado_viz <- dados_dist_long[!is.na(razao) & razao > 0.25 & razao < 3.0]

# Densidade
ggplot(dados_dist_long_filtrado_viz, aes(x = razao, fill = categoria)) +
  geom_density(alpha = 0.7) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 1) +
  scale_fill_manual(values = c("CLT Privado" = "#E67E22", "Setor Público" = "#2ECC71")) +
  scale_x_continuous(breaks = seq(0, 3, 0.25)) +
  labs(
    title = paste0("Distribuição da Desigualdade Salarial Racial (", ano_mapa, ")"),
    subtitle = "Distribuição da razão salarial Brancos/Negros pelos municípios",
    x = "Razão Salário Médio Branco / Negro", y = "Densidade", fill = "Categoria:",
    caption = paste0("Fonte: RAIS ", ano_mapa, ". (Sem filtro de amostra mínima).")
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
ggsave(paste0("distribuicao_densidade_", ano_mapa, "_sem_filtro.png"), width = 12, height = 7, dpi = 300)

# Histogramas
ggplot(dados_dist_long_filtrado_viz, aes(x = razao, fill = categoria)) +
  geom_histogram(binwidth = 0.05, alpha = 0.9, color = "white") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 1) +
  facet_wrap(~categoria, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("CLT Privado" = "#E67E22", "Setor Público" = "#2ECC71")) +
  scale_x_continuous(limits = c(0.5, 3.0), breaks = seq(0.5, 3.0, 0.25)) +
  labs(
    title = paste0("Histograma da Desigualdade Salarial Racial (", ano_mapa, ")"),
    subtitle = "Número de municípios por faixa de razão salarial",
    x = "Razão Salário Médio Branco / Negro", y = "Número de Municípios",
    caption = paste0("Fonte: RAIS ", ano_mapa, ". (Sem filtro de amostra mínima).")
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", strip.text = element_text(face = "bold", size = 12))
ggsave(paste0("distribuicao_histograma_", ano_mapa, "_sem_filtro.png"), width = 10, height = 8, dpi = 300)

# Boxplots
ggplot(dados_dist_long_filtrado_viz, aes(x = categoria, y = razao, fill = categoria)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "white") +
  scale_fill_manual(values = c("CLT Privado" = "#E67E22", "Setor Público" = "#2ECC71")) +
  scale_y_continuous(limits = c(0.5, 2.5), breaks = seq(0.5, 2.5, 0.25)) +
  labs(
    title = paste0("Dispersão da Desigualdade por Categoria (", ano_mapa, ")"),
    subtitle = "Cada ponto é um município. Diamante = média. Linha = mediana.",
    x = NULL, y = "Razão Salário Médio Branco / Negro",
    caption = paste0("Fonte: RAIS ", ano_mapa, ". (Sem filtro de amostra mínima).")
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
ggsave(paste0("distribuicao_boxplot_", ano_mapa, "_sem_filtro.png"), width = 8, height = 7, dpi = 300)

# ============================================================================
# 5. VISUALIZAÇÃO: MAPAS E DISPERSÃO RAZÃO SALARIAL (SEM FILTRO)
# ============================================================================
cat("\nGerando mapas e gráfico de dispersão (Razão Salarial)...\n")
cat("Baixando dados geoespaciais dos municípios...\n")

# Geobr geralmente possui shapes em 2010 e 2020; escolher o mais próximo
shape_year <- ifelse(ano_mapa <= 2010, 2010, 2020)
municipios_sf <- read_municipality(year = shape_year, showProgress = FALSE)

# Garantir code_muni numérico para o join
dados_mapa[, code_muni := as.numeric(id_municipio)]

# Mesclar shapes com base municipal (left join para manter geometria)
cat("Mesclando dados municipais com shapes geoespaciais...\n")
dados_mapa_sf <- municipios_sf %>%
  dplyr::left_join(dplyr::select(dados_mapa, -id_municipio_nome, -sigla_uf), by = "code_muni")

# Mapa 1: Razão CLT Privado
cat("Gerando mapa da desigualdade (CLT Privado)...\n")
dados_mapa_sf_clt <- dados_mapa_sf[!is.na(dados_mapa_sf$razao_clt) & dados_mapa_sf$razao_clt < 5,]
ggplot(dados_mapa_sf_clt) +
  geom_sf(aes(fill = razao_clt), color = NA) +
  scale_fill_viridis_c(
    option = "plasma", direction = -1,
    name = "Razão Branco/Negro\n(CLT Privado)",
    breaks = c(1, 1.25, 1.5, 1.75, 2),
    labels = c("1.0 (Igualdade)", "1.25", "1.5", "1.75", "2.0")
  ) +
  labs(
    title = paste0("Desigualdade Salarial Racial (CLT Privado) por Município (", ano_mapa, ")"),
    subtitle = "Razão Salário Médio Branco / Negro. (Sem filtro de amostra mínima)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    legend.position = "right",
    axis.text = element_blank(), axis.title = element_blank()
  )
ggsave(paste0("mapa_desigualdade_clt_", ano_mapa, "_sem_filtro.png"), width = 12, height = 10, dpi = 300)

# Mapa 2: Razão Setor Público
cat("Gerando mapa da desigualdade (Setor Público)...\n")
dados_mapa_sf_publico <- dados_mapa_sf[!is.na(dados_mapa_sf$razao_publico) & dados_mapa_sf$razao_publico < 5,]
ggplot(dados_mapa_sf_publico) +
  geom_sf(aes(fill = razao_publico), color = NA) +
  scale_fill_viridis_c(
    option = "plasma", direction = -1,
    name = "Razão Branco/Negro\n(Setor Público)",
    breaks = c(1, 1.25, 1.5, 1.75, 2),
    labels = c("1.0 (Igualdade)", "1.25", "1.5", "1.75", "2.0")
  ) +
  labs(
    title = paste0("Desigualdade Salarial Racial (Setor Público) por Município (", ano_mapa, ")"),
    subtitle = "Razão Salário Médio Branco / Negro. (Sem filtro de amostra mínima)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    legend.position = "right",
    axis.text = element_blank(), axis.title = element_blank()
  )
ggsave(paste0("mapa_desigualdade_publico_", ano_mapa, "_sem_filtro.png"), width = 12, height = 10, dpi = 300)

# Dispersão CLT vs. Público
dados_dispersao <- dados_mapa[!is.na(razao_clt) & !is.na(razao_publico) & razao_clt < 5 & razao_publico < 5]
if (nrow(dados_dispersao) > 0) {
  cat("Gerando gráfico de dispersão CLT vs. Setor Público...\n")
  ggplot(dados_dispersao, aes(x = razao_clt, y = razao_publico)) +
    geom_point(alpha = 0.6, size = 2, aes(color = sigla_uf)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
    geom_hline(yintercept = 1, linetype = "dotted", color = "red") +
    geom_vline(xintercept = 1, linetype = "dotted", color = "red") +
    scale_color_viridis_d(option = "viridis", name = "UF", guide = "none") +
    scale_x_continuous(limits = c(0.8, 2.5), breaks = seq(0.8, 2.5, 0.2)) +
    scale_y_continuous(limits = c(0.8, 2.5), breaks = seq(0.8, 2.5, 0.2)) +
    labs(
      title    = paste0("Comparativo da Desigualdade Salarial Racial (", ano_mapa, ")"),
      subtitle = "Cada ponto é um município: Razão Branco/Negro no CLT Privado vs. Setor Público",
      x        = "Razão Salário Branco/Negro (CLT Privado)",
      y        = "Razão Salário Branco/Negro (Setor Público)",
      caption  = "Fonte: RAIS. Linha tracejada = igualdade entre categorias."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, color = "gray30"),
      legend.position = "right"
    )
  ggsave(paste0("dispersao_clt_vs_publico_", ano_mapa, "_sem_filtro.png"), width = 12, height = 8, dpi = 300)
} else {
  cat("WARN: Não há dados suficientes para o gráfico de dispersão.\n")
}

# ============================================================================
# 6. VISUALIZAÇÃO: EVOLUÇÃO DE EMPREGO E SALÁRIOS GERAIS
# ============================================================================
cat("\nGerando gráficos de evolução de Emprego e Salários Médios Gerais...\n")

# Evolução do Emprego Nacional
emprego_long <- melt(
  agregados_temporais, id.vars = "ano",
  measure.vars = c("emprego_total_nacional", "emprego_clt_nacional", "emprego_publico_nacional"),
  variable.name = "categoria", value.name = "vinculos"
)
emprego_long[, categoria := fcase(
  categoria == "emprego_total_nacional",   "Total",
  categoria == "emprego_clt_nacional",     "CLT Privado",
  categoria == "emprego_publico_nacional", "Setor Público"
)]
emprego_long[, categoria := factor(categoria, levels = c("Total", "CLT Privado", "Setor Público"))]

ggplot(emprego_long, aes(x = ano, y = vinculos, color = categoria, group = categoria)) +
  geom_line(size = 1.5) + geom_point(size = 4) +
  scale_color_manual(values = c("Total" = "#3498DB", "CLT Privado" = "#E67E22", "Setor Público" = "#2ECC71")) +
  scale_x_continuous(breaks = anos_disponiveis) +
  scale_y_continuous(labels = label_number(unit = "M", scale = 1e-6, accuracy = 0.1)) +
  labs(
    title = "Evolução do Emprego Nacional por Categoria",
    subtitle = "Número de vínculos de trabalho (todos com salário válido)",
    x = "Ano", y = "Número de Vínculos (em Milhões)", color = "Categoria",
    caption = "Fonte: RAIS. (Sem filtro de amostra mínima)."
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16))
ggsave("evolucao_emprego_nacional.png", width = 12, height = 8, dpi = 300)

# Evolução do Salário Médio Nacional
salario_medio_long <- melt(
  agregados_temporais, id.vars = "ano",
  measure.vars = c("salario_medio_total_nacional", "salario_medio_clt_nacional", "salario_medio_publico_nacional"),
  variable.name = "categoria", value.name = "salario"
)
salario_medio_long[, categoria := fcase(
  categoria == "salario_medio_total_nacional",   "Total",
  categoria == "salario_medio_clt_nacional",     "CLT Privado",
  categoria == "salario_medio_publico_nacional", "Setor Público"
)]
salario_medio_long[, categoria := factor(categoria, levels = c("Total", "CLT Privado", "Setor Público"))]

ggplot(salario_medio_long, aes(x = ano, y = salario, color = categoria, group = categoria)) +
  geom_line(size = 1.5) + geom_point(size = 4) +
  scale_color_manual(values = c("Total" = "#3498DB", "CLT Privado" = "#E67E22", "Setor Público" = "#2ECC71")) +
  scale_x_continuous(breaks = anos_disponiveis) +
  scale_y_continuous(labels = label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Evolução do Salário Médio Nacional por Categoria",
    subtitle = "Salário médio ponderado (todos os trabalhadores com salário válido)",
    x = "Ano", y = "Salário Médio (R$)", color = "Categoria",
    caption = "Fonte: RAIS. (Sem filtro de amostra mínima)."
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16))
ggsave("evolucao_salario_medio_nacional.png", width = 12, height = 8, dpi = 300)

# Evolução do Salário Médio por Raça e Categoria
cat("Calculando agregados de salário por raça...\n")
agregados_raca_salario <- base_municipio[, .(
  salario_branco_total    = weighted.mean(salario_branco_total,    w = n_brancos_total,    na.rm = TRUE),
  salario_negro_total     = weighted.mean(salario_negro_total,     w = n_negros_total,     na.rm = TRUE),
  salario_branco_clt      = weighted.mean(salario_branco_clt,      w = n_brancos_clt,      na.rm = TRUE),
  salario_negro_clt       = weighted.mean(salario_negro_clt,       w = n_negros_clt,       na.rm = TRUE),
  salario_branco_publico  = weighted.mean(salario_branco_publico,  w = n_brancos_publico,  na.rm = TRUE),
  salario_negro_publico   = weighted.mean(salario_negro_publico,   w = n_negros_publico,   na.rm = TRUE)
), by = ano]

salario_raca_long <- melt(
  agregados_raca_salario, id.vars = "ano",
  measure.vars = c("salario_branco_total", "salario_negro_total",
                   "salario_branco_clt", "salario_negro_clt",
                   "salario_branco_publico", "salario_negro_publico"),
  variable.name = "categoria_cod", value.name = "salario"
)

salario_raca_long[, raca := fcase(
  grepl("branco", categoria_cod, ignore.case = TRUE), "Branco",
  grepl("negro",  categoria_cod, ignore.case = TRUE), "Negro"
)]
salario_raca_long[, categoria := fcase(
  grepl("total",   categoria_cod, ignore.case = TRUE), "Total",
  grepl("clt",     categoria_cod, ignore.case = TRUE), "CLT Privado",
  grepl("publico", categoria_cod, ignore.case = TRUE), "Setor Público"
)]
salario_raca_long[, categoria := factor(categoria, levels = c("Total", "CLT Privado", "Setor Público"))]
salario_raca_long[, raca := factor(raca, levels = c("Branco", "Negro"))]

ggplot(salario_raca_long, aes(x = ano, y = salario, color = raca, linetype = categoria, group = interaction(raca, categoria))) +
  geom_line(size = 1.3) +
  geom_point(size = 3, aes(shape = raca)) +
  scale_color_manual(values = c("Branco" = "#0072B2", "Negro" = "#D55E00")) +
  scale_linetype_manual(values = c("Total" = "solid", "CLT Privado" = "dashed", "Setor Público" = "dotted")) +
  scale_shape_manual(values = c(17, 16)) +
  scale_x_continuous(breaks = anos_disponiveis) +
  scale_y_continuous(labels = label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Evolução do Salário Médio Nacional por Raça e Categoria",
    subtitle = "Salário médio ponderado. O 'gap' é a distância vertical entre as linhas.",
    x = "Ano", y = "Salário Médio (R$)", color = "Raça/Cor:", linetype = "Categoria:", shape = "Raça/Cor:",
    caption = "Fonte: RAIS. (Sem filtro de amostra mínima)."
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", legend.box = "horizontal", plot.title = element_text(face = "bold", size = 16))
ggsave("evolucao_salario_medio_por_raca_categoria.png", width = 12, height = 8, dpi = 300)

# ============================================================================
# 7. (NOVO) MAPAS E DISTRIBUIÇÃO DE EMPREGO E SALÁRIO
# ============================================================================
cat("\nGerando mapas e distribuições de Emprego Total e Salário Médio...\n")

# Mapa de Emprego Total (escala log10)
dados_mapa_sf_emprego <- dados_mapa_sf %>% 
  dplyr::filter(!is.na(emprego_total) & emprego_total > 0)

if (nrow(dados_mapa_sf_emprego) > 0) {
  ggplot(dados_mapa_sf_emprego) +
    geom_sf(aes(fill = log10(emprego_total)), color = NA) +
    scale_fill_viridis_c(
      option = "cividis",
      name = "Nº de Vínculos\n(Escala Log10)",
      breaks = c(1, 2, 3, 4, 5, 6),
      labels = c("10", "100", "1.000", "10.000", "100.000", "1.000.000")
    ) +
    labs(
      title = paste0("Distribuição de Emprego Total por Município (", ano_mapa, ")"),
      subtitle = "Número de vínculos (todos com salário válido). Escala logarítmica."
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 16),
          legend.position = "right", axis.text = element_blank(), axis.title = element_blank())
  ggsave(paste0("mapa_emprego_total_", ano_mapa, "_corrigido.png"), width = 12, height = 10, dpi = 300)
} else {
  cat("WARN: Não há dados válidos para gerar o mapa de Emprego Total após filtros.\n")
}

# Mapa de Salário Médio Total
dados_mapa_sf_salario <- dados_mapa_sf %>%
  dplyr::filter(!is.na(salario_medio_total) & salario_medio_total > 500 & salario_medio_total < 15000)

if (nrow(dados_mapa_sf_salario) > 0) {
  ggplot(dados_mapa_sf_salario) +
    geom_sf(aes(fill = salario_medio_total), color = NA) +
    scale_fill_viridis_c(
      option = "inferno",
      name = "Salário Médio\n(R$)",
      labels = label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")
    ) +
    labs(
      title = paste0("Distribuição do Salário Médio Total por Município (", ano_mapa, ")"),
      subtitle = "Salário médio de todos os trabalhadores (com salário válido)."
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 16),
          legend.position = "right", axis.text = element_blank(), axis.title = element_blank())
  ggsave(paste0("mapa_salario_medio_total_", ano_mapa, "_corrigido.png"), width = 12, height = 10, dpi = 300)
} else {
  cat("WARN: Não há dados válidos para gerar o mapa de Salário Médio Total após filtros.\n")
}

# Distribuição do Salário Médio por Raça (Total)
cat("Gerando gráfico de distribuição de Salário Médio por Raça...\n")
dados_salario_raca_long <- melt(
  dados_mapa[, .(id_municipio, salario_branco_total, salario_negro_total)],
  id.vars = "id_municipio",
  measure.vars = c("salario_branco_total", "salario_negro_total"),
  variable.name = "grupo_raca", value.name = "salario_medio"
)
dados_salario_raca_long[, grupo_raca := fcase(
  grupo_raca == "salario_branco_total", "Brancos (Sal. Médio)",
  grupo_raca == "salario_negro_total",  "Negros (Sal. Médio)"
)]
dados_salario_raca_long_viz <- dados_salario_raca_long[!is.na(salario_medio) & salario_medio < 15000]

ggplot(dados_salario_raca_long_viz, aes(x = salario_medio, fill = grupo_raca)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c("Brancos (Sal. Médio)" = "#0072B2", "Negros (Sal. Médio)" = "#D55E00")) +
  scale_x_continuous(labels = label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ","), limits = c(0, 10000)) +
  labs(
    title = paste0("Distribuição do Salário Médio Municipal por Raça (", ano_mapa, ")"),
    subtitle = "Distribuição dos salários médios de brancos e negros entre os municípios.",
    x = "Salário Médio Municipal (R$)", y = "Densidade", fill = "Grupo:"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16))
ggsave(paste0("distribuicao_salario_medio_por_raca_", ano_mapa, ".png"), width = 12, height = 7, dpi = 300)

# Distribuição do Salário por Raça e Categoria
cat("Gerando gráfico de distribuição de Salário por Raça e Categoria...\n")
dados_salario_raca_cat_long <- melt(
  dados_mapa,
  id.vars = "id_municipio",
  measure.vars = c("salario_branco_clt", "salario_negro_clt", "salario_branco_publico", "salario_negro_publico"),
  variable.name = "grupo", value.name = "salario_medio"
)
dados_salario_raca_cat_long[, raca := fcase(
  grepl("branco", grupo, ignore.case = TRUE), "Branco",
  grepl("negro",  grupo, ignore.case = TRUE), "Negro"
)]
dados_salario_raca_cat_long[, categoria := fcase(
  grepl("clt",     grupo, ignore.case = TRUE), "CLT Privado",
  grepl("publico", grupo, ignore.case = TRUE), "Setor Público"
)]
dados_salario_raca_cat_long[, categoria := factor(categoria, levels = c("CLT Privado", "Setor Público"))]
dados_salario_raca_cat_long[, raca := factor(raca, levels = c("Branco", "Negro"))]
dados_salario_raca_cat_long_viz <- dados_salario_raca_cat_long[!is.na(salario_medio) & salario_medio < 15000]

ggplot(dados_salario_raca_cat_long_viz, aes(x = salario_medio, fill = raca)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~categoria) +
  scale_fill_manual(values = c("Branco" = "#0072B2", "Negro" = "#D55E00")) +
  scale_x_continuous(labels = label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ","), limits = c(0, 10000)) +
  labs(
    title = paste0("Distribuição do Salário Médio Municipal por Raça e Categoria (", ano_mapa, ")"),
    subtitle = "Comparando a distribuição dos salários médios dentro de cada categoria.",
    x = "Salário Médio Municipal (R$)", y = "Densidade", fill = "Raça/Cor:"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16),
        strip.text = element_text(face = "bold", size = 12))
ggsave(paste0("distribuicao_salario_raca_categoria_", ano_mapa, ".png"), width = 12, height = 7, dpi = 300)

# Distribuição do Número de Vínculos por Categoria
cat("Gerando gráfico de distribuição de vínculos por Categoria...\n")
dados_emprego_cat_long <- melt(
  dados_mapa[, .(id_municipio, emprego_clt_privado, emprego_publico)],
  id.vars = "id_municipio",
  measure.vars = c("emprego_clt_privado", "emprego_publico"),
  variable.name = "categoria", value.name = "vinculos"
)
dados_emprego_cat_long[, categoria := fcase(
  categoria == "emprego_clt_privado", "CLT Privado",
  categoria == "emprego_publico",     "Setor Público"
)]
dados_emprego_cat_long_viz <- dados_emprego_cat_long[vinculos > 0]

ggplot(dados_emprego_cat_long_viz, aes(x = vinculos, fill = categoria)) +
  geom_density(alpha = 0.7) +
  scale_x_log10(
    breaks = c(1, 10, 100, 1000, 10000, 100000),
    labels = c("1", "10", "100", "1k", "10k", "100k")
  ) +
  scale_fill_manual(values = c("CLT Privado" = "#E67E22", "Setor Público" = "#2ECC71")) +
  labs(
    title = paste0("Distribuição do Nº de Vínculos por Categoria entre Municípios (", ano_mapa, ")"),
    subtitle = "Mostra como o número de empregos em cada setor varia entre os municípios.",
    x = "Número de Vínculos (Escala Log10)", y = "Densidade", fill = "Categoria:"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16))
ggsave(paste0("distribuicao_vinculos_categoria_", ano_mapa, ".png"), width = 12, height = 7, dpi = 300)

# Distribuição do Número de Trabalhadores por Raça
cat("Gerando gráfico de distribuição de vínculos por Raça...\n")
dados_emprego_raca_long <- melt(
  dados_mapa[, .(id_municipio, n_brancos_total, n_negros_total)],
  id.vars = "id_municipio",
  measure.vars = c("n_brancos_total", "n_negros_total"),
  variable.name = "raca", value.name = "vinculos"
)
dados_emprego_raca_long[, raca := fcase(
  raca == "n_brancos_total", "Brancos",
  raca == "n_negros_total",  "Negros"
)]
dados_emprego_raca_long_viz <- dados_emprego_raca_long[vinculos > 0]

ggplot(dados_emprego_raca_long_viz, aes(x = vinculos, fill = raca)) +
  geom_density(alpha = 0.7) +
  scale_x_log10(
    breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000),
    labels = c("1", "10", "100", "1k", "10k", "100k", "1M")
  ) +
  scale_fill_manual(values = c("Brancos" = "#0072B2", "Negros" = "#D55E00")) +
  labs(
    title = paste0("Distribuição do Nº de Trabalhadores por Raça entre Municípios (", ano_mapa, ")"),
    subtitle = "Mostra como o número de trabalhadores brancos e negros varia entre os municípios.",
    x = "Número de Trabalhadores (Escala Log10)", y = "Densidade", fill = "Raça/Cor:"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16))
ggsave(paste0("distribuicao_vinculos_raca_", ano_mapa, ".png"), width = 12, height = 7, dpi = 300)

