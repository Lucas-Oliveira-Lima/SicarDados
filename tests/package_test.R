# Testando o pacote 

## Carrega todas as funções e recursos diretamente no ambiente de desenvolvimento
devtools::load_all()

# Isso carrega todas as funções do pacote sem precisar instalá-lo.
# Você pode chamar as funções como se fossem parte de um pacote já instalado. Por exemplo:
teste <- batch_sicar_search(c("64893570110", "64987949253"))

## Verificação do pacote

# O devtools::check() faz uma verificação completa do pacote para garantir que
# ele esteja formatado corretamente e que as dependências estejam adequadas.
# Esse comando também executa qualquer teste que você tenha criado.
devtools::check()

# Testando as funções manualmente
resultado_car <- sicar_search_car_code("PA-1500107-E3F4583FFFBF4A9B9DAA5967D4045B70")
print(resultado_car)

# Testar em lote com vários códigos CAR
resultado_lote_car <- batch_sicar_search_car_code(dados_exemplo$CAR)
print(resultado_lote_car)


resultado_car_id <- extrair_dados_link(c(716355,
                                              716193,
                                              716183,
                                              720371,
                                              715631,
                                              715549,
                                              715283))
