algoritmo "UniLibrary"
// Desafio 1 - Sistema de biblioteca universitaria - VisuAlg 3.0
// Constantes conceituais: limite 40, categorias 1 a 5,
// emprestimo = coluna 1, devolucao = coluna 2,
// disponivel = 1 e indisponivel = 0.
// Os limites do vetor sao fixos na declaracao.

tipo
   Livro = registro
      codigo: inteiro
      titulo: caractere
      autor: caractere
      ano: inteiro
      categoria: inteiro
      qtdTotal: inteiro
      qtdDisponivel: inteiro
      qtdEmprestimos: inteiro
      status: inteiro
   fimregistro

var
   acervo: vetor[1..40] de Livro
   ordenados: vetor[1..40] de Livro
   movimentacoes: vetor[1..5,1..2] de inteiro
   nomesCategorias: vetor[1..5] de caractere
   qtdLivros, opcao: inteiro
   MAX_LIVROS, MAX_CATEGORIAS: inteiro
   EMPRESTIMO, DEVOLUCAO: inteiro
   DISPONIVEL, INDISPONIVEL: inteiro

funcao localizarLivro(cod: inteiro): inteiro
var
   i: inteiro
inicio
   localizarLivro <- 0
   para i de 1 ate qtdLivros faca
      se acervo[i].codigo = cod entao
         localizarLivro <- i
      fimse
   fimpara
fimfuncao

funcao quantidadeEmprestada(pos: inteiro): inteiro
inicio
   quantidadeEmprestada <- acervo[pos].qtdTotal - acervo[pos].qtdDisponivel
fimfuncao

procedimento mostrarLivro(pos: inteiro)
inicio
   escreval("Codigo: ", acervo[pos].codigo)
   escreval("Titulo: ", acervo[pos].titulo)
   escreval("Autor: ", acervo[pos].autor)
   escreval("Ano: ", acervo[pos].ano)
   escreval("Categoria: ", nomesCategorias[acervo[pos].categoria])
   escreval("Total de exemplares: ", acervo[pos].qtdTotal)
   escreval("Exemplares disponiveis: ", acervo[pos].qtdDisponivel)
   escreval("Exemplares emprestados agora: ", quantidadeEmprestada(pos))
   escreval("Emprestimos realizados: ", acervo[pos].qtdEmprestimos)
   se acervo[pos].status = DISPONIVEL entao
      escreval("Status: DISPONIVEL")
   senao
      escreval("Status: INDISPONIVEL")
   fimse
fimprocedimento

procedimento inicializar()
var
   i, j: inteiro
inicio
   qtdLivros <- 0
   MAX_LIVROS <- 40
   MAX_CATEGORIAS <- 5
   EMPRESTIMO <- 1
   DEVOLUCAO <- 2
   DISPONIVEL <- 1
   INDISPONIVEL <- 0
   nomesCategorias[1] <- "Tecnologia"
   nomesCategorias[2] <- "Ciencias Exatas"
   nomesCategorias[3] <- "Ciencias Humanas"
   nomesCategorias[4] <- "Administracao e Negocios"
   nomesCategorias[5] <- "Literatura"
   para i de 1 ate MAX_CATEGORIAS faca
      para j de 1 ate 2 faca
         movimentacoes[i,j] <- 0
      fimpara
   fimpara
fimprocedimento

procedimento cadastrarLivro()
var
   novo: Livro
inicio
   se qtdLivros >= MAX_LIVROS entao
      escreval("Limite de 40 livros atingido.")
   senao
      repita
         escreva("Codigo (inteiro positivo e unico): ")
         leia(novo.codigo)
         se (novo.codigo <= 0) ou (localizarLivro(novo.codigo) <> 0) entao
            escreval("Codigo invalido ou duplicado. Tente novamente.")
         fimse
      ate (novo.codigo > 0) e (localizarLivro(novo.codigo) = 0)
      escreva("Titulo: ")
      leia(novo.titulo)
      escreva("Autor: ")
      leia(novo.autor)
      repita
         escreva("Ano de publicacao (1 a 2026): ")
         leia(novo.ano)
         se (novo.ano < 1) ou (novo.ano > 2026) entao
            escreval("Ano invalido.")
         fimse
      ate (novo.ano >= 1) e (novo.ano <= 2026)
      escreval("Categorias: 1 Tecnologia | 2 Exatas | 3 Humanas")
      escreval("            4 Administracao e Negocios | 5 Literatura")
      repita
         escreva("Categoria: ")
         leia(novo.categoria)
         se (novo.categoria < 1) ou (novo.categoria > MAX_CATEGORIAS) entao
            escreval("Categoria invalida.")
         fimse
      ate (novo.categoria >= 1) e (novo.categoria <= MAX_CATEGORIAS)
      repita
         escreva("Quantidade total de exemplares: ")
         leia(novo.qtdTotal)
         se novo.qtdTotal <= 0 entao
            escreval("A quantidade deve ser maior que zero.")
         fimse
      ate novo.qtdTotal > 0
      novo.qtdDisponivel <- novo.qtdTotal
      novo.qtdEmprestimos <- 0
      novo.status <- DISPONIVEL
      qtdLivros <- qtdLivros + 1
      acervo[qtdLivros] <- novo
      escreval("Livro cadastrado com sucesso!")
   fimse
fimprocedimento

procedimento listarAcervo()
var
   i: inteiro
inicio
   se qtdLivros = 0 entao
      escreval("Catalogo vazio.")
   senao
      para i de 1 ate qtdLivros faca
         escreval("-----------------------------")
         mostrarLivro(i)
      fimpara
   fimse
fimprocedimento

procedimento consultarLivro()
var
   cod, pos: inteiro
inicio
   se qtdLivros = 0 entao
      escreval("Catalogo vazio.")
   senao
      escreva("Informe o codigo: ")
      leia(cod)
      pos <- localizarLivro(cod)
      se pos = 0 entao
         escreval("Livro nao encontrado.")
      senao
         mostrarLivro(pos)
      fimse
   fimse
fimprocedimento

procedimento realizarEmprestimo()
var
   cod, pos, cat: inteiro
inicio
   escreva("Codigo do livro para emprestimo: ")
   leia(cod)
   pos <- localizarLivro(cod)
   se pos = 0 entao
      escreval("Livro inexistente.")
   senao
      se (acervo[pos].qtdDisponivel > 0) e (acervo[pos].status = DISPONIVEL) entao
         acervo[pos].qtdDisponivel <- acervo[pos].qtdDisponivel - 1
         acervo[pos].qtdEmprestimos <- acervo[pos].qtdEmprestimos + 1
         cat <- acervo[pos].categoria
         movimentacoes[cat,EMPRESTIMO] <- movimentacoes[cat,EMPRESTIMO] + 1
         se acervo[pos].qtdDisponivel = 0 entao
            acervo[pos].status <- INDISPONIVEL
         fimse
         escreval("Emprestimo registrado com sucesso!")
      senao
         escreval("Emprestimo negado: nenhum exemplar disponivel.")
      fimse
   fimse
fimprocedimento

procedimento realizarDevolucao()
var
   cod, pos, cat: inteiro
inicio
   escreva("Codigo do livro para devolucao: ")
   leia(cod)
   pos <- localizarLivro(cod)
   se pos = 0 entao
      escreval("Livro inexistente.")
   senao
      se (quantidadeEmprestada(pos) > 0) e (acervo[pos].qtdDisponivel < acervo[pos].qtdTotal) entao
         acervo[pos].qtdDisponivel <- acervo[pos].qtdDisponivel + 1
         acervo[pos].status <- DISPONIVEL
         cat <- acervo[pos].categoria
         movimentacoes[cat,DEVOLUCAO] <- movimentacoes[cat,DEVOLUCAO] + 1
         escreval("Devolucao registrada com sucesso!")
      senao
         escreval("Devolucao negada: nao ha exemplares emprestados.")
      fimse
   fimse
fimprocedimento

procedimento listarDisponiveis()
var
   i, encontrados: inteiro
inicio
   encontrados <- 0
   para i de 1 ate qtdLivros faca
      se (acervo[i].status = DISPONIVEL) e (acervo[i].qtdDisponivel > 0) entao
         escreval(acervo[i].codigo, " - ", acervo[i].titulo, " | Disponiveis: ", acervo[i].qtdDisponivel)
         encontrados <- encontrados + 1
      fimse
   fimpara
   se encontrados = 0 entao
      escreval("Nenhum livro disponivel.")
   fimse
fimprocedimento

// Troca registros inteiros, preservando todos os campos associados.
procedimento prepararRanking()
var
   i, j: inteiro
   temp: Livro
   trocar: logico
inicio
   para i de 1 ate qtdLivros faca
      ordenados[i] <- acervo[i]
   fimpara
   para i de 1 ate qtdLivros faca
      para j de 1 ate qtdLivros - i faca
         trocar <- falso
         se ordenados[j].qtdEmprestimos < ordenados[j+1].qtdEmprestimos entao
            trocar <- verdadeiro
         senao
            se ordenados[j].qtdEmprestimos = ordenados[j+1].qtdEmprestimos entao
               se ordenados[j].qtdDisponivel > ordenados[j+1].qtdDisponivel entao
                  trocar <- verdadeiro
               senao
                  se ordenados[j].qtdDisponivel = ordenados[j+1].qtdDisponivel entao
                     se ordenados[j].titulo > ordenados[j+1].titulo entao
                        trocar <- verdadeiro
                     fimse
                  fimse
               fimse
            fimse
         fimse
         se trocar entao
            temp <- ordenados[j]
            ordenados[j] <- ordenados[j+1]
            ordenados[j+1] <- temp
         fimse
      fimpara
   fimpara
fimprocedimento

procedimento exibirRanking()
var
   i: inteiro
inicio
   se qtdLivros = 0 entao
      escreval("Catalogo vazio.")
   senao
      prepararRanking()
      escreval("POSICAO | LIVRO | AUTOR | EMPRESTIMOS | DISPONIVEIS")
      para i de 1 ate qtdLivros faca
         escreval(i, " | ", ordenados[i].titulo, " | ", ordenados[i].autor, " | ", ordenados[i].qtdEmprestimos, " | ", ordenados[i].qtdDisponivel)
      fimpara
   fimse
fimprocedimento

procedimento exibirEstatisticas()
var
   i, j, totalEx, totalDisp, totalEmp, totalDev, atualEmp: inteiro
   catLivros: vetor[1..5] de inteiro
   posMaisEmp, posMaisDisp, catMaisEmp: inteiro
   somaMov, totalMat, maiorCategoria: inteiro
   percDisp, percEmp, media: real
inicio
   totalEx <- 0
   totalDisp <- 0
   totalEmp <- 0
   totalDev <- 0
   somaMov <- 0
   posMaisEmp <- 0
   posMaisDisp <- 0
   catMaisEmp <- 1
   maiorCategoria <- -1
   para i de 1 ate MAX_CATEGORIAS faca
      catLivros[i] <- 0
   fimpara
   para i de 1 ate qtdLivros faca
      totalEx <- totalEx + acervo[i].qtdTotal
      totalDisp <- totalDisp + acervo[i].qtdDisponivel
      catLivros[acervo[i].categoria] <- catLivros[acervo[i].categoria] + 1
      se posMaisEmp = 0 entao
         posMaisEmp <- i
         posMaisDisp <- i
      senao
         se acervo[i].qtdEmprestimos > acervo[posMaisEmp].qtdEmprestimos entao
            posMaisEmp <- i
         fimse
         se acervo[i].qtdDisponivel > acervo[posMaisDisp].qtdDisponivel entao
            posMaisDisp <- i
         fimse
      fimse
   fimpara
   atualEmp <- totalEx - totalDisp
   percDisp <- 0
   percEmp <- 0
   media <- 0
   se totalEx > 0 entao
      percDisp <- (totalDisp * 100.0) / totalEx
      percEmp <- (atualEmp * 100.0) / totalEx
   fimse
   se qtdLivros > 0 entao
      media <- totalEx / qtdLivros
   fimse
   escreval("========== ESTATISTICAS ==========")
   escreval("Livros cadastrados: ", qtdLivros)
   escreval("Total de exemplares: ", totalEx)
   escreval("Disponiveis: ", totalDisp)
   escreval("Emprestados no momento: ", atualEmp)
   escreval("Percentual disponivel: ", percDisp:0:2, "%")
   escreval("Percentual emprestado: ", percEmp:0:2, "%")
   escreval("Media de exemplares por livro: ", media:0:2)
   se qtdLivros > 0 entao
      escreval("Livro mais emprestado: ", acervo[posMaisEmp].titulo)
      escreval("Livro com mais disponiveis: ", acervo[posMaisDisp].titulo)
   senao
      escreval("Nao existem livros cadastrados.")
   fimse
   escreval("CATEGORIA | LIVROS | EMPRESTIMOS | DEVOLUCOES")
   para i de 1 ate MAX_CATEGORIAS faca
      somaMov <- 0
      para j de 1 ate 2 faca
         somaMov <- somaMov + movimentacoes[i,j]
      fimpara
      totalMat <- somaMov
      totalEmp <- totalEmp + movimentacoes[i,EMPRESTIMO]
      totalDev <- totalDev + movimentacoes[i,DEVOLUCAO]
      se movimentacoes[i,EMPRESTIMO] > maiorCategoria entao
         maiorCategoria <- movimentacoes[i,EMPRESTIMO]
         catMaisEmp <- i
      fimse
      escreval(nomesCategorias[i], " | ", catLivros[i], " | ", movimentacoes[i,EMPRESTIMO], " | ", movimentacoes[i,DEVOLUCAO])
   fimpara
   escreval("Total de emprestimos realizados: ", totalEmp)
   escreval("Total de devolucoes: ", totalDev)
   escreval("Total de movimentacoes: ", totalEmp + totalDev)
   se totalEmp > 0 entao
      escreval("Categoria com mais emprestimos: ", nomesCategorias[catMaisEmp])
   senao
      escreval("Nenhuma categoria tem emprestimos registrados.")
   fimse
fimprocedimento

// Busca binaria recursiva no vetor auxiliar ordenado por codigo.
funcao buscaBinaria(cod, esquerda, direita: inteiro): inteiro
var
   meio: inteiro
inicio
   se esquerda > direita entao
      buscaBinaria <- 0
   senao
      meio <- (esquerda + direita) div 2
      se ordenados[meio].codigo = cod entao
         buscaBinaria <- meio
      senao
         se cod < ordenados[meio].codigo entao
            buscaBinaria <- buscaBinaria(cod, esquerda, meio-1)
         senao
            buscaBinaria <- buscaBinaria(cod, meio+1, direita)
         fimse
      fimse
   fimse
fimfuncao

procedimento buscaRecursiva()
var
   i, j, cod, pos: inteiro
   temp: Livro
inicio
   se qtdLivros = 0 entao
      escreval("Catalogo vazio.")
   senao
      // Copia e ordena pelo codigo, sem alterar o acervo original.
      para i de 1 ate qtdLivros faca
         ordenados[i] <- acervo[i]
      fimpara
      para i de 1 ate qtdLivros faca
         para j de 1 ate qtdLivros - i faca
            se ordenados[j].codigo > ordenados[j+1].codigo entao
               temp <- ordenados[j]
               ordenados[j] <- ordenados[j+1]
               ordenados[j+1] <- temp
            fimse
         fimpara
      fimpara
      escreva("Codigo a procurar recursivamente: ")
      leia(cod)
      pos <- buscaBinaria(cod, 1, qtdLivros)
      se pos = 0 entao
         escreval("Livro nao encontrado pela busca recursiva.")
      senao
         escreval("Encontrado: ", ordenados[pos].titulo, " - ", ordenados[pos].autor)
         // A consulta completa utiliza a posicao no acervo original.
         mostrarLivro(localizarLivro(cod))
      fimse
   fimse
fimprocedimento

inicio
   inicializar()
   repita
      escreval("")
      escreval("UNILIBRARY - SISTEMA DE GERENCIAMENTO DE BIBLIOTECA")
      escreval("1. Cadastrar livro")
      escreval("2. Listar acervo")
      escreval("3. Consultar livro")
      escreval("4. Realizar emprestimo")
      escreval("5. Realizar devolucao")
      escreval("6. Exibir livros disponiveis")
      escreval("7. Exibir ranking do acervo")
      escreval("8. Exibir estatisticas")
      escreval("9. Realizar busca recursiva")
      escreval("10. Encerrar sistema")
      escreva("Digite uma opcao: ")
      leia(opcao)
      escolha opcao
         caso 1
            cadastrarLivro()
         caso 2
            listarAcervo()
         caso 3
            consultarLivro()
         caso 4
            realizarEmprestimo()
         caso 5
            realizarDevolucao()
         caso 6
            listarDisponiveis()
         caso 7
            exibirRanking()
         caso 8
            exibirEstatisticas()
         caso 9
            buscaRecursiva()
         caso 10
            escreval("Sistema encerrado.")
         outrocaso
            escreval("Opcao invalida. Tente novamente.")
      fimescolha
   ate opcao = 10
fimalgoritmo
