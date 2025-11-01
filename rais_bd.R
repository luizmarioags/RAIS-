# ======================= PREPARO DO AMBIENTE =======================
pkgs <- c("basedosdados","bigrquery","data.table","dplyr","ggplot2","glue","scales")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# Diretório temporário sem espaços ou acentos
temp_dir <- "F:/Luiz_RAIS_Temp/temp_big"
dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
Sys.setenv(TMPDIR = temp_dir, TEMP = temp_dir, TMP = temp_dir)

# Projeto de cobrança do BigQuery
set_billing_id("rais-455114")

# =================== FUNÇÃO DE DOWNLOAD COM RETRY ==================
# Função para baixar dados por ano com tentativas de repetição
baixar_dados_por_ano <- function(ano, max_tentativas = 3) {
  message(paste("Baixando dados de", ano, "..."))
  
  query <- glue("
  WITH 
  dados_filtrados AS (
    SELECT *
    FROM `basedosdados.br_me_rais.microdados_vinculos`
    WHERE ano = {ano}
  ),
  dicionario_tipo_vinculo AS (
    SELECT chave AS chave_tipo_vinculo, valor AS descricao_tipo_vinculo
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE nome_coluna = 'tipo_vinculo' AND id_tabela = 'microdados_vinculos'
  ),
  dicionario_grau_instrucao_1985_2005 AS (
    SELECT chave AS chave_grau_instrucao_1985_2005, valor AS descricao_grau_instrucao_1985_2005
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE nome_coluna = 'grau_instrucao_1985_2005' AND id_tabela = 'microdados_vinculos'
  ),
  dicionario_grau_instrucao_apos_2005 AS (
    SELECT chave AS chave_grau_instrucao_apos_2005, valor AS descricao_grau_instrucao_apos_2005
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE nome_coluna = 'grau_instrucao_apos_2005' AND id_tabela = 'microdados_vinculos'
  ),
  dicionario_sexo AS (
    SELECT chave AS chave_sexo, valor AS descricao_sexo
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE nome_coluna = 'sexo' AND id_tabela = 'microdados_vinculos'
  ),
  dicionario_raca_cor AS (
    SELECT chave AS chave_raca_cor, valor AS descricao_raca_cor
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE nome_coluna = 'raca_cor' AND id_tabela = 'microdados_vinculos'
  ),
  dicionario_indicador_portador_deficiencia AS (
    SELECT chave AS chave_indicador_portador_deficiencia, valor AS descricao_indicador_portador_deficiencia
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE nome_coluna = 'indicador_portador_deficiencia' AND id_tabela = 'microdados_vinculos'
  ),
  dicionario_tipo_deficiencia AS (
    SELECT chave AS chave_tipo_deficiencia, valor AS descricao_tipo_deficiencia
    FROM `basedosdados.br_me_rais.dicionario`
    WHERE nome_coluna = 'tipo_deficiencia' AND id_tabela = 'microdados_vinculos'
  )
  SELECT
    dados.ano AS ano,
    dados.sigla_uf AS sigla_uf,
    diretorio_sigla_uf.nome AS sigla_uf_nome,
    dados.id_municipio AS id_municipio,
    diretorio_id_municipio.nome AS id_municipio_nome,
    descricao_tipo_vinculo AS tipo_vinculo,
    dados.valor_remuneracao_media AS valor_remuneracao_media,
    dados.valor_remuneracao_janeiro AS valor_remuneracao_janeiro,
    dados.valor_remuneracao_fevereiro AS valor_remuneracao_fevereiro,
    dados.valor_remuneracao_marco AS valor_remuneracao_marco,
    dados.valor_remuneracao_abril AS valor_remuneracao_abril,
    dados.valor_remuneracao_maio AS valor_remuneracao_maio,
    dados.valor_remuneracao_junho AS valor_remuneracao_junho,
    dados.valor_remuneracao_julho AS valor_remuneracao_julho,
    dados.valor_remuneracao_agosto AS valor_remuneracao_agosto,
    dados.valor_remuneracao_setembro AS valor_remuneracao_setembro,
    dados.valor_remuneracao_outubro AS valor_remuneracao_outubro,
    dados.valor_remuneracao_novembro AS valor_remuneracao_novembro,
    dados.valor_remuneracao_dezembro AS valor_remuneracao_dezembro,
    dados.valor_salario_contratual AS valor_salario_contratual,
    dados.subsetor_ibge AS subsetor_ibge,
    dados.cnae_1 AS cnae_1,
    diretorio_cnae_1.descricao AS cnae_1_descricao,
    diretorio_cnae_1.descricao_grupo AS cnae_1_descricao_grupo,
    diretorio_cnae_1.descricao_divisao AS cnae_1_descricao_divisao,
    diretorio_cnae_1.descricao_secao AS cnae_1_descricao_secao,
    dados.cnae_2 AS cnae_2,
    dados.idade AS idade,
    descricao_grau_instrucao_1985_2005 AS grau_instrucao_1985_2005,
    descricao_grau_instrucao_apos_2005 AS grau_instrucao_apos_2005,
    descricao_sexo AS sexo,
    descricao_raca_cor AS raca_cor,
    descricao_indicador_portador_deficiencia AS indicador_portador_deficiencia,
    descricao_tipo_deficiencia AS tipo_deficiencia
  FROM dados_filtrados AS dados
  LEFT JOIN (SELECT DISTINCT sigla, nome FROM `basedosdados.br_bd_diretorios_brasil.uf`) AS diretorio_sigla_uf
    ON dados.sigla_uf = diretorio_sigla_uf.sigla
  LEFT JOIN (SELECT DISTINCT id_municipio, nome FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
    ON dados.id_municipio = diretorio_id_municipio.id_municipio
  LEFT JOIN dicionario_tipo_vinculo
    ON dados.tipo_vinculo = chave_tipo_vinculo
  LEFT JOIN (SELECT DISTINCT cnae_1, descricao, descricao_grupo, descricao_divisao, descricao_secao FROM `basedosdados.br_bd_diretorios_brasil.cnae_1`) AS diretorio_cnae_1
    ON dados.cnae_1 = diretorio_cnae_1.cnae_1
  LEFT JOIN dicionario_grau_instrucao_1985_2005
    ON dados.grau_instrucao_1985_2005 = chave_grau_instrucao_1985_2005
  LEFT JOIN dicionario_grau_instrucao_apos_2005
    ON dados.grau_instrucao_apos_2005 = chave_grau_instrucao_apos_2005
  LEFT JOIN dicionario_sexo
    ON dados.sexo = chave_sexo
  LEFT JOIN dicionario_raca_cor
    ON dados.raca_cor = chave_raca_cor
  LEFT JOIN dicionario_indicador_portador_deficiencia
    ON dados.indicador_portador_deficiencia = chave_indicador_portador_deficiencia
  LEFT JOIN dicionario_tipo_deficiencia
    ON dados.tipo_deficiencia = chave_tipo_deficiencia
  ")
  
  for (tentativa in 1:max_tentativas) {
    tryCatch({
      # Executa a query (projeto de cobrança vindo da config global)
      tb_job <- bq_project_query(
        x = bq_project(get_billing_id()),
        query = as.character(query)
      )
      
      # Download com configurações mais conservadoras
      df <- bq_table_download(
        tb_job,
        page_size = 10000,   # páginas menores para estabilidade
        bigint   = "string"  # evita integer64 problemático
      )
      
      message(paste("Download de", ano, "concluído! Total de linhas:", nrow(df)))
      return(df)
      
    }, error = function(e) {
      message(paste("Tentativa", tentativa, "falhou:", e$message))
      if (tentativa < max_tentativas) {
        message("Aguardando 5 segundos antes de tentar novamente...")
        Sys.sleep(5)
      } else {
        stop(paste("Falha ao baixar dados de", ano, "após", max_tentativas, "tentativas"))
      }
    })
  }
}

# ======================= LOOP DE DOWNLOAD ==========================
anos <- c(2010, 2014, 2018)

# Remover checkpoints antigos (se existirem)
message("=== REMOVENDO CHECKPOINTS ANTIGOS ===")
for (ano in anos) {
  arquivo_antigo <- paste0("rais_", ano, ".rds")
  if (file.exists(arquivo_antigo)) {
    file.remove(arquivo_antigo)
    message(paste("Removido:", arquivo_antigo))
  }
}
message("Iniciando download do zero...\n")

lista_dfs <- list()

for (ano in anos) {
  arquivo_checkpoint <- paste0("rais_", ano, "_completo.rds")
  tryCatch({
    lista_dfs[[as.character(ano)]] <- baixar_dados_por_ano(ano)
    
    # Salva checkpoint
    saveRDS(lista_dfs[[as.character(ano)]], file = arquivo_checkpoint)
    message(paste("Checkpoint salvo:", arquivo_checkpoint))
    
    # Aguarda entre downloads (exceto após o último)
    if (ano != tail(anos, 1)) {
      message("Aguardando 5 segundos antes do próximo download...")
      Sys.sleep(5)
    }
    
  }, error = function(e) {
    message(paste("ERRO FATAL ao baixar dados de", ano, ":", e$message))
  })
}

# ==================== PADRONIZAÇÃO DE TIPOS ========================
padronizar_tipos <- function(df) {
  df %>%
    mutate(
      ano    = as.integer(ano),
      idade  = suppressWarnings(as.integer(idade)),
      
      # Remunerações para numérico
      valor_remuneracao_media     = suppressWarnings(as.numeric(valor_remuneracao_media)),
      valor_remuneracao_janeiro   = suppressWarnings(as.numeric(valor_remuneracao_janeiro)),
      valor_remuneracao_fevereiro = suppressWarnings(as.numeric(valor_remuneracao_fevereiro)),
      valor_remuneracao_marco     = suppressWarnings(as.numeric(valor_remuneracao_marco)),
      valor_remuneracao_abril     = suppressWarnings(as.numeric(valor_remuneracao_abril)),
      valor_remuneracao_maio      = suppressWarnings(as.numeric(valor_remuneracao_maio)),
      valor_remuneracao_junho     = suppressWarnings(as.numeric(valor_remuneracao_junho)),
      valor_remuneracao_julho     = suppressWarnings(as.numeric(valor_remuneracao_julho)),
      valor_remuneracao_agosto    = suppressWarnings(as.numeric(valor_remuneracao_agosto)),
      valor_remuneracao_setembro  = suppressWarnings(as.numeric(valor_remuneracao_setembro)),
      valor_remuneracao_outubro   = suppressWarnings(as.numeric(valor_remuneracao_outubro)),
      valor_remuneracao_novembro  = suppressWarnings(as.numeric(valor_remuneracao_novembro)),
      valor_remuneracao_dezembro  = suppressWarnings(as.numeric(valor_remuneracao_dezembro)),
      valor_salario_contratual    = suppressWarnings(as.numeric(valor_salario_contratual)),
      
      subsetor_ibge = as.character(subsetor_ibge),
      sigla_uf      = as.character(sigla_uf),
      id_municipio  = as.character(id_municipio)
    )
}

message("Padronizando tipos de dados...")
lista_dfs <- lapply(lista_dfs, padronizar_tipos)

# =================== COMBINAÇÃO E SALVAMENTO =======================
message("Combinando dataframes...")
df <- dplyr::bind_rows(lista_dfs)

# Converte para data.table
setDT(df)

# Salva o dataframe completo
saveRDS(df, "rais_completo.rds")

# Opcional: limpar memória
rm(lista_dfs)
gc()
