% 1) FATOS - sintomas por doença

% --- Tosse
%      doenca  sintoma                                                                           class
sintoma(gripe, tosse, intensidade(moderada), prob(0.7), duracao(dias), frequencia(intermitente), comum).
sintoma(resfriado, tosse, intensidade(leve), prob(0.8), duracao(dias), frequencia(intermitente), comum).
sintoma(covid19, tosse, intensidade(moderada), prob(0.85), duracao(dias), frequencia(continuo), critico).
sintoma(influenza, tosse, intensidade(alta), prob(0.9), duracao(dias), frequencia(continuo), critico).
sintoma(asma, tosse, intensidade(moderada), prob(0.6), duracao(dias), frequencia(intermitente), comum).
sintoma(rinite, tosse, intensidade(leve), prob(0.4), duracao(dias), frequencia(intermitente), raro).
sintoma(tuberculose, tosse, intensidade(severa), prob(0.9), duracao(semanas), frequencia(continuo), critico).
sintoma(pneumonia, tosse, intensidade(severa), prob(0.8), duracao(semanas), frequencia(continuo), critico).

% --- Falta de ar (Dispneia)
sintoma(covid19, falta_de_ar, intensidade(severa), prob(0.9), duracao(dias), frequencia(continuo), critico).
sintoma(influenza, falta_de_ar, intensidade(leve), prob(0.4), duracao(dias), frequencia(raro), comum).
sintoma(asma, falta_de_ar, intensidade(severa), prob(0.85), duracao(horas_dias), frequencia(intermitente), critico).
sintoma(tuberculose, falta_de_ar, intensidade(moderada), prob(0.7), duracao(semanas), frequencia(continuo), comum).
sintoma(pneumonia, falta_de_ar, intensidade(severa), prob(0.85), duracao(semanas), frequencia(continuo), critico).
sintoma(outras_respiratorias, falta_de_ar, intensidade(moderada), prob(0.75), duracao(dias), frequencia(intermitente), comum).

% --- Febre
sintoma(gripe, febre, intensidade(moderada), prob(0.8), duracao(dias), frequencia(continuo), comum).
sintoma(resfriado, febre, intensidade(leve), prob(0.2), duracao(horas), frequencia(raro), raro).
sintoma(covid19, febre, intensidade(alta), prob(0.9), duracao(dias), frequencia(continuo), critico).
sintoma(influenza, febre, intensidade(alta), prob(0.85), duracao(dias), frequencia(continuo), critico).
sintoma(pneumonia, febre, intensidade(moderada), prob(0.7), duracao(dias), frequencia(continuo), comum).

% --- Congestão Nasal
sintoma(resfriado, congestao_nasal, intensidade(moderada), prob(0.9), duracao(dias), frequencia(continuo), comum).
sintoma(rinite, congestao_nasal, intensidade(severa), prob(0.95), duracao(dias), frequencia(continuo), critico).
sintoma(gripe, congestao_nasal, intensidade(leve), prob(0.3), duracao(dias), frequencia(raro), raro).
sintoma(covid19, congestao_nasal, intensidade(leve), prob(0.2), duracao(dias), frequencia(raro), raro).

% --- Coriza
sintoma(resfriado, coriza, intensidade(moderada), prob(0.9), duracao(dias), frequencia(continuo), comum).
sintoma(rinite, coriza, intensidade(moderada), prob(0.95), duracao(dias), frequencia(continuo), critico).
sintoma(gripe, coriza, intensidade(leve), prob(0.3), duracao(dias), frequencia(raro), raro).

% --- Espirros
sintoma(resfriado, espirros, intensidade(leve), prob(0.85), duracao(dias), frequencia(intermitente), comum).
sintoma(rinite, espirros, intensidade(moderada), prob(0.9), duracao(dias), frequencia(continuo), critico).

% --- Dor de Garganta
sintoma(resfriado, dor_garganta, intensidade(moderada), prob(0.8), duracao(dias), frequencia(intermitente), comum).
sintoma(gripe, dor_garganta, intensidade(leve), prob(0.3), duracao(dias), frequencia(raro), raro).

% --- Dor de Cabeça
sintoma(pneumonia, dor_cabeca, intensidade(moderada), prob(0.6), duracao(dias), frequencia(intermitente), comum).
sintoma(covid19, dor_cabeca, intensidade(moderada), prob(0.5), duracao(dias), frequencia(intermitente), comum).
sintoma(influenza, dor_cabeca, intensidade(leve), prob(0.3), duracao(dias), frequencia(raro), raro).

% --- Mal-estar Geral
sintoma(gripe, mal_estar, intensidade(moderada), prob(0.8), duracao(dias), frequencia(continuo), comum).
sintoma(covid19, mal_estar, intensidade(alta), prob(0.85), duracao(dias), frequencia(continuo), critico).
sintoma(pneumonia, mal_estar, intensidade(alta), prob(0.7), duracao(dias), frequencia(continuo), comum).

% --- Fadiga
sintoma(covid19, fadiga, intensidade(severa), prob(0.85), duracao(dias_semanas), frequencia(continuo), critico).
sintoma(pneumonia, fadiga, intensidade(severa), prob(0.75), duracao(semanas), frequencia(continuo), critico).
sintoma(influenza, fadiga, intensidade(moderada), prob(0.4), duracao(dias), frequencia(intermitente), comum).

% --- Chiado no Peito
sintoma(asma, chiado, intensidade(severa), prob(0.95), duracao(horas_dias), frequencia(intermitente), critico).
sintoma(doencas_pulmonares_cronicas, chiado, intensidade(moderada), prob(0.8), duracao(semanas), frequencia(continuo), comum).

% --- Perda de Paladar / Olfato
sintoma(covid19, perda_paladar, intensidade(severa), prob(0.95), duracao(dias_semanas), frequencia(continuo), critico).
sintoma(covid19, perda_olfato, intensidade(severa), prob(0.95), duracao(dias_semanas), frequencia(continuo), critico).
sintoma(influenza, perda_paladar, intensidade(leve), prob(0.2), duracao(dias), frequencia(raro), raro).

% --- Hemoptise
sintoma(tuberculose, hemoptise, intensidade(severa), prob(0.9), duracao(semanas), frequencia(intermitente), critico).
sintoma(pneumonia, hemoptise, intensidade(moderada), prob(0.3), duracao(dias), frequencia(raro), comum).

% organizar as doencas de forma hierarquica, onde agrupamos as doencas por tipos (sao as relacoes semanticas)
e_uma_doenca(covid19, viral).
e_uma_doenca(gripe, viral).
e_uma_doenca(resfriado, viral).
e_uma_doenca(influenza, viral).
e_uma_doenca(tuberculose, bacteriana).
e_uma_doenca(pneumonia, bacteriana).
e_uma_doenca(asma, cronica).
e_uma_doenca(rinite, alergica).

% serve para ligar sintomas que sao sinonimos ou equivalentes (sao os subconjuntos semanticos)
subconjunto_sintomas(dispneia, falta_de_ar).
subconjunto_sintomas(falta_de_ar, dispneia).
subconjunto_sintomas(hemoptise, tosse_com_sangue).
subconjunto_sintomas(tosse_com_sangue, hemoptise).
subconjunto_sintomas(anosmia, perda_olfato).
subconjunto_sintomas(perda_olfato, anosmia).
subconjunto_sintomas(ageusia, perda_paladar).
subconjunto_sintomas(perda_paladar, ageusia).
subconjunto_sintomas(cefaleia, dor_cabeca).
subconjunto_sintomas(dor_cabeca, cefaleia).

% sao os valores usados no calculo de probabilidade, para montar o score sera: Probabilidade × Peso_Class × M_Intensidade × M_Frequência
% peso serve para dizer quao importante sao os sintomas
peso_critico(2.0).
peso_comum(1.0).
peso_raro(0.5).

% multiplicador da intensidade demonstra quao forte sao os sintomas 
m_leve(0.8).
m_moderada(1.0).
m_alta(1.1).
m_severa(1.2).

% multiplicador da frequencia demonstra a frequencia que ocorre
m_continuo(1.2). %tempo todo
m_intermitente(1.0). %de vez em quando
m_rarof(0.7). %raramente

% funcoes auxiliares manuais 
% verificar se item esta na lista, como: esta_na_lista(tosse, [tosse, febre, dor_garganta]) , entao da true
esta_na_lista(Item, [Item|_]).
esta_na_lista(Item, [_|Cauda]) :-
    esta_na_lista(Item, Cauda).

% verificar se NÃO esta na lista, como: se estiver na lista vai cair na 1 regra e corta, se nao encontrar vai cair nessa 2 regra
nao_esta_na_lista(Item, Lista) :-
    esta_na_lista(Item, Lista), !, fail.
nao_esta_na_lista(_, _).

% adicionar item a lista, como: colocar_na_lista(tosse, [febre, dor], NovaLista); ai tosse vira a cabeca da lista ficando: NovaLista = [tosse, febre, dor]
colocar_na_lista(Item, Lista, [Item|Lista]).

% reverter a lista, onde primeiro separa a cabeca da lista, depois chama reverter_lista com a cauda e vai tirando cada item por vez da lista
% apos isso retorna (recursao) colocando na lista do ultimo item ate a cabeca, assim o ultimo item vira a nova cabeca da lista revertida 
reverter_lista([], []).
reverter_lista([Cabeca|Cauda], Revertida) :-
    reverter_lista(Cauda, CaudaRevertida),
    concatenar([CaudaRevertida, [Cabeca]], Revertida).

% concatenar listas, serve para unir duas listas ou mais em uma unica lista; onde vai tirando um item por vez da primeira lista e une cada ultimo item da primeira lista a segunda concatenando-as 
concatenar([], []).
concatenar([Lista], Lista).
concatenar([Lista1, Lista2|Resto], Resultado) :-
    concatenar_duas(Lista1, Lista2, Temp),
    concatenar([Temp|Resto], Resultado).

concatenar_duas([], Lista, Lista).
concatenar_duas([Cabeca|Cauda], Lista2, [Cabeca|Resultado]) :-
    concatenar_duas(Cauda, Lista2, Resultado).

% contar elementos na lista, onde ignora cada item da lista por vez (comecando pela cabeca), diminuindo o tam. da lista ate fica vazia e soma cada vez que isso ocorre ate ela ficar vazia.
contar_elementos([], 0).
contar_elementos([_|Cauda], N) :-
    contar_elementos(Cauda, N1),
    N is N1 + 1.


% declaramos predicados que podem ser modificados em tempo de execução (adicionar/remover fatos). Pois serao usados no menu interativo
% dynamic serve para isso, pois fatos são fixos no código e dynamic permite usar o assert (adicionar) e o retract (remover) durante execucao
:- dynamic paciente/4. %(Nome, CPF, Telefone, Sintomas)
:- dynamic diag_paciente/3. % (CPF, Doenca, Score)
:- dynamic sessao_dia/1. % (NumPacientes)  

