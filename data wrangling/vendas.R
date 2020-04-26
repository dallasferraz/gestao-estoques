library(tidyverse)

#apagando a primeira coluna
vendas <- vendas[,-1]

#quantidade de caracteres nas células de data e hora
ts.tam <- nchar(vendas[1,13])

#salvar em um vetor todas as datas e horas
time.stamp <- vector()

for(i in 1:4925){
  if(!is.na(vendas[i,13]) & (nchar(vendas[i,13])==ts.tam)){
    time.stamp[i] <- as.character(vendas[i,13])
  }
}

#ficando apenas com os valores no vetor que contém data e hora
time.stamp <- time.stamp[!is.na(time.stamp)]

#apagando as colunas de 6 até a 10 do dataframe
vendas <- vendas[,-c(6:10)]

#apagando a coluna x13
vendas <- vendas[,-7]

#apagando a coluna x18
vendas <- vendas[,-11]

#apagando a coluna x3
vendas <- vendas[,-2]

#apagando a coluna x5
vendas <- vendas[,-3]

#apagando a coluna x17
vendas <- vendas[,-8]

#apagando as linhas vazias
for(i in 1:4925){
  if(rowSums(is.na(vendas[i,]))==9){
    vendas <- vendas[-i,]
  }
}

#capturando o ID das vendas
id.venda <- vector()

#como o tamanho do ID só tem no máximo 3 caracteres
id.tam <- 3

for(i in 1:4925){
  if(!is.na(vendas[i,2]) & (nchar(vendas[i,2])<=id.tam)){
    id.venda[i] <- as.integer(vendas[i,2])
  }
}

#ficando apenas com os valores no vetor que contém os ID
id.venda <- id.venda[!is.na(id.venda)]

#se o valor da linha contiver "Clientes Diversos", apagar esta linha e a anterior
for(i in 1:2086){
  if(!is.na(vendas[i,2]) & vendas[i,2]=="Clientes Diversos"){
    vendas <- vendas[-c(i-1,i),]
  }
}

#apagando a coluna x4
vendas <- vendas[,-2]

#apagando a coluna x16 (não precisamos saber de custo numa venda)
vendas <- vendas[,-6]

#se o valor da linha contiver "Total do custo:", apagar esta linha
for(i in 1:1450){
  if(!is.na(vendas[i,5]) & vendas[i,5]=="Total do custo:"){
    vendas <- vendas[-i,]
  }
}

#renomeando "Vendidor por" para "Subtotal"
vendas[vendas=="Vendido Por"] <- "Subtotal"

#renomeando "Identificador" para "ID do produto"
vendas[vendas=="Identificador"] <- "ID do produto"

#eliminando a descrição do produto
vendas <- vendas[,-2]

#criar dataframe do cabeçalho de vendas
header.vendas <- data.frame(matrix("", ncol = 1, nrow = 318))

#colocar o id de vendas como primeira coluna
header.vendas <- as.data.frame(id.venda)

#transformar o vetor de timestamp em dataframe
time.stamp <- as.data.frame(time.stamp)

#unindo timestamp com os IDS
header.vendas <- cbind(header.vendas,time.stamp)

#separando o timestamp em data e horas
header.vendas <- header.vendas %>% separate(time.stamp,c("Data","Hora")," - ")

#renomeando a primeira coluna
names(header.vendas)[1] <- "ID da venda"

#sair do formato de lista
total.formato.correto <- as.data.frame(unlist(vendas[,5]))

#separar o que está antes da vírgula do que está de pois da vírgula
names(total.formato.correto)[1] <- "Valor"
total.formato.correto <- total.formato.correto %>% separate(Valor,c("Reais","Centavos"),",")

#unificar com ponto
total.formato.correto <- total.formato.correto %>% unite(`Valor correto`,c("Reais","Centavos"),sep=".")

#apagar o que não for valor
total.formato.correto <- as.data.frame(total.formato.correto[!total.formato.correto$`Valor correto`=="NA.NA",])
names(total.formato.correto)[1] <- "Valor"

#unificando o dataframe de valor ao dataframe de cabeçalho
header.vendas <- cbind(header.vendas,total.formato.correto)
remove(total.formato.correto)

#agora remover a coluna X15 do dataframe de vendas (não é mais necessário)
vendas <- vendas[,-4]

#removendo também a coluna X19
vendas <- vendas[,-4]

#apagar as linhas que estão vazias agora
for(i in 1:1132){
  if(rowSums(is.na(vendas[i,]))==4){
    vendas <- vendas[-i,]
  }
}

#transformando a primeira linha em cabeçalho do dataframe vendas
colnames(vendas) <- vendas[1,]
vendas <- vendas[-1,]

#nova coluna com o id da venda no dataframe venda
vendas$`Id venda` <- NA

#toda vez que tiver um cabeçalho repetido no dataframe vendas, colocar o valor de idvenda numa coluna ao lado
j <- 1

for(i in 1:811){
  if(vendas[i,1]=="ID do produto"){
    j <- j + 1
  }else{
    vendas[i,5] <- id.venda[j]
  }
}

#eliminando as linhas que tem cabeçalhos intermediários com base no NA
for(i in 1:811){
  if(is.na(vendas[i,5])){
    vendas <- vendas[-i,]
  }
}

#transformando a data no formato YYYY-MM-DD
#separando
header.vendas <- header.vendas %>% separate(Data,c("dia","mes","ano"),"/")

#unindo
header.vendas <- header.vendas %>% unite(Data,c("ano","mes","dia"),sep="-")

#exportando
write.csv(vendas, 'C:/Users/Dallas/Desktop/spreadsheets/vendas.csv')
write.csv(header.vendas, 'C:/Users/Dallas/Desktop/spreadsheets/cabecalho_vendas.csv')
