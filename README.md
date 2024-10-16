
<!-- README.md is generated from README.Rmd. Please edit that file -->

[<img src="images/logo_sicardados-rpackage.png" width="300" />](https://github.com/Lucas-Oliveira-Lima/SicarDados)

# SicarDados

<!-- badges: start -->
<!-- badges: end -->

O pacote **SicarDados** oferece funções para realizar consultas no SICAR
e extrair informações sobre imóveis rurais.

## Instalação

Você pode instalar a versão de desenvolvimento do SicarDados diretamente
do GitHub:

``` r
# install.packages("pak")
pak::pak("Lucas-Oliveira-Lima/SicarDados")

# install.packages("devtools")
devtools::install_github("Lucas-Oliveira-Lima/SicarDados")
```

## Funcionalidades

### Consultar Dominialidade

A função \`sicar_consultar_dominialidade()\` permite consultar
informações sobre a dominialidade de um imóvel a partir do código CAR no
SICAR. Isso envolve a extração de dados como o nome do proprietário,
CPF/CNPJ e o tipo de pessoa (física ou jurídica) vinculada ao imóvel.

``` r
# Exemplo de uso:
resultado <- sicar_consultar_dominialidade_car("PA-1500909-B0D2560D157F4417BBF2E9E7590B6C97")
print(resultado)
```

- **Entrada**: Código CAR (por exemplo,
  `"PA-1500909-B0D2560D157F4417BBF2E9E7590B6C97"`)

- **Saída**: Um dataframe contendo o nome do proprietário, CPF/CNPJ, e o
  tipo de pessoa vinculada ao imóvel.

### **Consulta de Dominialidade para Vários Códigos CAR**

A função `batch_consultar_dominialidade` faz a consulta de dominialidade
para uma lista de códigos CAR e retorna um dataframe consolidado com as
informações de cada imóvel.

**Exemplo de uso**:

``` r
codigos_car <- c("PA-1500909-B0D2560D157F4417BBF2E9E7590B6C97", "PA-1500107-E3F4583FFFBF4A9B9DAA5967D4045B70") resultado <- batch_consultar_dominialidade(codigos_car) print(resultado)
```

- **Entrada**: Vetor de códigos CAR.

- **Saída**: Um dataframe com as informações de dominialidade (nome,
  CPF/CNPJ, tipo, código CAR, e ID).

### **Consulta de Imóveis CAR**

A função `sicar_search_car_code()` permite buscar imóveis vinculados a
um determinado código CAR no SICAR. A consulta retorna informações como
a área, nome do imóvel, status e protocolo.

**Exemplo de uso**:

``` r
imoveis <- sicar_search_car_code("PA-1500909-B0D2560D157F4417BBF2E9E7590B6C97")
print(imoveis)
```

- **Entrada**: Código CAR.

- **Saída**: Dataframe com informações detalhadas sobre os imóveis
  vinculados ao código CAR, como nome do imóvel, área, status, e
  protocolo.

A função `sicar_search_cpf()` permite buscar imóveis vinculados a um
determinado CPF CAR no SICAR. A consulta retorna informações como a
área, nome do imóvel, status e protocolo

### **Download de Shapefiles de Imóveis**

A função `sicar_download_car_shp()` faz o download dos shapefiles
(arquivos de georreferenciamento) de imóveis vinculados a um código CAR.
Esses shapefiles podem ser utilizados em sistemas de informação
geográfica (GIS) para análise espacial.

``` r
# Exemplo de uso:
baixar_imovel_car("PA-1500909-B0D2560D157F4417BBF2E9E7590B6C97", diretorio_destino = "data/shapefiles/")
```

- **Entrada**: Código CAR e o diretório de destino onde o shapefile será
  salvo.

- **Saída**: O shapefile do imóvel será salvo localmente no diretório
  especificado.

### **Download de Shapefiles para Vários Imóveis**

A função `batch_download_car_shp()` permite o download de shapefiles
para vários códigos CAR ao mesmo tempo. Ela organiza os shapefiles em um
diretório de trabalho.

**Exemplo de uso**:

``` r
codigos <- data.frame(codigo = c("PA-1500909-B0D2560D157F4417BBF2E9E7590B6C97", "PA-1500107-E3F4583FFFBF4A9B9DAA5967D4045B70"))
batch_download_car_shp(codigos, "codigo")
```

- **Entrada**: Dataframe com a coluna dos códigos CAR e diretório de
  destino (opcional).

- **Saída**: Shapefiles dos imóveis baixados.

### **Extração de Dados do Demonstrativo CAR**

A função `sicar_extract_demonstrativo()` é usada para extrair
informações detalhadas dos imóveis, como área consolidada, área de
regeneração, área de reserva legal, entre outras, a partir da página de
demonstração de um imóvel no SICAR.

**Exemplo de uso**:

``` r
demonstrativo <- sicar_extract_demonstrativo("PA-1500909-B0D2560D157F4417BBF2E9E7590B6C97")
print(demonstrativo)
```

- **Entrada**: Código CAR.

- **Saída**: Dataframe com informações detalhadas de uso da terra,
  cobertura, e outros dados ambientais do imóvel.

## Funcionalidades Planejadas (Futuras)

- **Análise espacial** com dados integrados do SICAR e outras fontes
  (como MapBiomas).

- **Relatórios personalizados** sobre a situação dos imóveis e sua
  conformidade ambiental.

## Contribuições

Contribuições são bem-vindas! Sinta-se à vontade para abrir uma issue ou
enviar um pull request.
