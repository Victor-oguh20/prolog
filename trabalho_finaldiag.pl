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
    concatenar_lista(Lista1, Lista2, Temp),
    concatenar([Temp|Resto], Resultado).

concatenar_lista([], Lista, Lista).
concatenar_lista([Cabeca|Cauda], Lista2, [Cabeca|Resultado]) :-
    concatenar_lista(Cauda, Lista2, Resultado).

% contar elementos na lista, onde ignora cada item da lista por vez (comecando pela cabeca), diminuindo o tam. da lista ate fica vazia e soma cada vez que isso ocorre ate ela ficar vazia.
contar_elementos([], 0).
contar_elementos([_|Cauda], N) :-
    contar_elementos(Cauda, N1),
    N is N1 + 1.

% serve para coletar as doencas da base de conhecimento de forma unica, sem repetir. Onde busca pelos fatos apenas as doencas e vai agregando cada uma delas na lista sem repetir  
% usa o cut, pois apos encontrar todas as doencas disponiveis nao procura nenhuma outra mais 
coletar_doencas_sem_repetir(Doencas) :-
    coletar_doencas_aux([], DoencasInvertidas),
    reverter_lista(DoencasInvertidas, Doencas).

coletar_doencas_aux(ListaAtual, Doencas) :-
    sintoma(Doenca, _, _, _, _, _, _),
    nao_esta_na_lista(Doenca, ListaAtual),
    colocar_na_lista(Doenca, ListaAtual, NovaLista),
    coletar_doencas_aux(NovaLista, Doencas), !.
coletar_doencas_aux(Lista, Lista).

% adiciona sinonimos aos sintomas (o programa busca pelos sinonimos que o paciente disser que tem)
sinonimo_sintomas([], []).
sinonimo_sintomas([Sintoma|Resto], SintomasSinonimo) :-
    sinonimo_sintomas(Resto, RestoExpandido),
    (subconjunto_sintomas(Sintoma, Equivalente) ->
        (nao_esta_na_lista(Equivalente, RestoExpandido) ->
            SintomasSinonimo = [Sintoma, Equivalente|RestoExpandido];
            SintomasSinonimo = [Sintoma|RestoExpandido]
        );
        SintomasSinonimo = [Sintoma|RestoExpandido]
    ).

% busca o valor numerico de cada classificacao, para maior flexibilidade e mudar em um so lugar, caso necessario
obter_peso_classificacao(critico, Peso) :- peso_critico(Peso).
obter_peso_classificacao(comum, Peso) :- peso_comum(Peso).
obter_peso_classificacao(raro, Peso) :- peso_raro(Peso).

obter_multiplicador_intensidade(leve, Multi) :- m_leve(Multi).
obter_multiplicador_intensidade(moderada, Multi) :- m_moderada(Multi).
obter_multiplicador_intensidade(alta, Multi) :- m_alta(Multi).
obter_multiplicador_intensidade(severa, Multi) :- m_severa(Multi).

obter_multiplicador_frequencia(continuo, Multi) :- m_continuo(Multi).
obter_multiplicador_frequencia(intermitente, Multi) :- m_intermitente(Multi).
obter_multiplicador_frequencia(raro, Multi) :- m_rarof(Multi).

% calculo score de cada doenca de forma individual baseado nos sintomas do paciente 
% onde 1: expande o sintoma com seus sinonimos, 2: chama um auxiliar para calcular o score da doenca
calcular_score_doenca(Doenca, Sintomas, ScoreTotal, Detalhes) :-
    sinonimo_sintomas(Sintomas, SintomasSinonimo),
    calcular_score_aux(Doenca, SintomasSinonimo, 0, ScoreTotal, [], DetalhesInvertidos),
    reverter_lista(DetalhesInvertidos, Detalhes).

calcular_score_aux(_, [], Acum, Acum, DetalhesAcum, DetalhesAcum).
calcular_score_aux(Doenca, [Sintoma|Resto], Acum, ScoreTotal, DetalhesAcum, DetalhesFinal) :-
    (sintoma(Doenca, Sintoma, intensidade(Int), prob(P), _, frequencia(Freq), Class) ->
        obter_peso_classificacao(Class, PClass),
        obter_multiplicador_intensidade(Int, PInt),
        obter_multiplicador_frequencia(Freq, PFreq),
        Temp1 is P * PClass,
        Temp2 is Temp1 * PInt,
        ScoreIndividual is Temp2 * PFreq,
        NovoAcum is Acum + ScoreIndividual,
        Detalhe = detalhe(Sintoma, P, Class, Int, Freq, ScoreIndividual),
        colocar_na_lista(Detalhe, DetalhesAcum, NovosDetalhes) ;
        NovoAcum = Acum,
        NovosDetalhes = DetalhesAcum
    ),
    calcular_score_aux(Doenca, Resto, NovoAcum, ScoreTotal, NovosDetalhes, DetalhesFinal).

% calculo do score de todas as doencas, pois quando o paciente informa seus sintomas, ha varias doencas que apresentam esses sintomas como padrao
calcular_todos_scores(_, [], []).
calcular_todos_scores(Sintomas, [Doenca|Resto], Resultados) :-
    calcular_score_doenca(Doenca, Sintomas, Score, Detalhes),
    (Score > 0 ->
        Resultados = [(Doenca, Score, Detalhes)|ResultadosResto] ;
        Resultados = ResultadosResto
    ),
    calcular_todos_scores(Sintomas, Resto, ResultadosResto).

% ordena as doencas do maior para menor score, atraves de comparacao, ate as mudancas nao acontecerem mais e o primeiro elemento da lista for o de maior score entre todos
ordenar_resultados(Lista, Ordenada) :-
    troca(Lista, Lista1),
    (listas_iguais(Lista, Lista1) -> 
        Ordenada = Lista
    ; 
        ordenar_resultados(Lista1, Ordenada)
    ).

troca([], []).
troca([X], [X]).
troca([(D1,S1,Det1),(D2,S2,Det2)|Cauda], [(D2,S2,Det2)|Resultado]) :-
    S2 > S1, !,
    troca([(D1,S1,Det1)|Cauda], Resultado).
troca([X|Cauda], [X|Resultado]) :-
    troca(Cauda, Resultado).

listas_iguais([], []).
listas_iguais([X|C1], [X|C2]) :-
    listas_iguais(C1, C2).

% diagnostico principal, e onde junta e acontece tudo apos dizer os sintomas 
% tem no resultado final as doencas e os scores delas, mas retira os detalhes
diagnosticar_doenca(Sintomas, Resultado) :-
    coletar_doencas_sem_repetir(TodasDoencas),
    calcular_todos_scores(Sintomas, TodasDoencas, ResultadosBrutos),
    ordenar_resultados(ResultadosBrutos, ResultadosOrdenados),
    formatar_resultado(ResultadosOrdenados, Resultado).

formatar_resultado([], []).
formatar_resultado([(Doenca, Score, _)|Resto], [(Doenca, Score)|RestoFormatado]) :-
    formatar_resultado(Resto, RestoFormatado).

% todos os sintomas de uma unica doenca em especifico (junta todas os sintomas que tal doenca possui)
listar_sintomas(Doenca, Listar) :-
    listar_sintomas_aux(Doenca, [], ListaInvertida),
    reverter_lista(ListaInvertida, Listar).

listar_sintomas_aux(Doenca, Acum, Listar) :-
    sintoma(Doenca, Sintoma, _, _, _, _, _),
    nao_esta_na_lista(Sintoma, Acum),
    colocar_na_lista(Sintoma, Acum, NovoAcum),
    listar_sintomas_aux(Doenca, NovoAcum, Listar), !.
listar_sintomas_aux(_, Listar, Listar). 

% todas doencas que possuem um unico sintoma em especifico(processo inverso do anterior, onde junta todas as doencas que possuem tal sintoma)
quais_doencas_possui(Sintoma, Doencas) :-
    sinonimo_sintomas([Sintoma], SintomasSinonimo),
    quais_doencas_aux(SintomasSinonimo, [], DoencasInvertidas),
    reverter_lista(DoencasInvertidas, Doencas).

quais_doencas_aux([], Acum, Acum).
quais_doencas_aux([Sintoma|Resto], Acum, Doencas) :-
    coletar_doencas_com_sintoma(Sintoma, Acum, NovoAcum),
    quais_doencas_aux(Resto, NovoAcum, Doencas).

coletar_doencas_com_sintoma(Sintoma, Acum, Doencas) :-
    sintoma(Doenca, Sintoma, _, _, _, _, _),
    nao_esta_na_lista(Doenca, Acum),
    colocar_na_lista(Doenca, Acum, NovoAcum),
    coletar_doencas_com_sintoma(Sintoma, NovoAcum, Doencas), !.
coletar_doencas_com_sintoma(_, Acum, Acum).

% explicacao de como chegou no diagnostico, mostrando o score de cada sintoma
explicar(Doenca, Sintomas, Explicacao) :-
    sinonimo_sintomas(Sintomas, SintomasSinonimo),
    explicar_aux(Doenca, SintomasSinonimo, [], ExplicacaoInvertida),
    reverter_lista(ExplicacaoInvertida, Explicacao).

explicar_aux(_, [], Acum, Acum).
explicar_aux(Doenca, [Sintoma|Resto], Acum, Explicacao) :-
    (sintoma(Doenca, Sintoma, intensidade(Int), prob(P), _, frequencia(Freq), Class) ->
        obter_peso_classificacao(Class, PClass),
        obter_multiplicador_intensidade(Int, PInt),
        obter_multiplicador_frequencia(Freq, PFreq),
        Temp1 is P * PClass,
        Temp2 is Temp1 * PInt,
        Score is Temp2 * PFreq,
        Item = (Sintoma, prob=P, class=Class, int=Int, freq=Freq, score=Score),
        colocar_na_lista(Item, Acum, NovoAcum) ;
        NovoAcum = Acum
    ),
    explicar_aux(Doenca, Resto, NovoAcum, Explicacao).

% sistema interativo de pacientes

% enquanto o paciente digita sim (acrescenta o sintoma perguntado a lista e busca ao final descobrir a doenca que mais apresenta tais sintomas)
% se o paciente digita nao (a lista fica vazia [] para tal sintoma perguntado e nao acrescenta a concatenacao)
consulta_interativa :-
    write('perguntas sobre seu sintomas: '), nl, nl,
    
    perguntar_sintoma('voce tem tosse?', tosse, Sintomas1),
    perguntar_sintoma('voce sente febre?', febre, Sintomas2),
    perguntar_sintoma('voce tem falta de ar ou dificuldade para respirar?', falta_de_ar, Sintomas3),
    perguntar_sintoma('voce sente dor de garganta?', dor_garganta, Sintomas4),
    perguntar_sintoma('voce perdeu o olfato ou paladar?', perda_olfato, Sintomas5),
    perguntar_sintoma('voce tem congestão nasal?', congestao_nasal, Sintomas6),
    perguntar_sintoma('voce sente fadiga ou cansaço extremo?', fadiga, Sintomas7),
    
    concatenar([Sintomas1, Sintomas2, Sintomas3, Sintomas4, Sintomas5, Sintomas6, Sintomas7], TodosSintomas),
    
    nl, write('sintomas coletados: '), write(TodosSintomas), nl, nl,
    
    diagnosticar_doenca(TodosSintomas, Resultado),
    
    write('resultado do diagnostico: '), nl,
    mostrar_resultado_formatado(Resultado).

perguntar_sintoma(Pergunta, Sintoma, Lista) :-
    write(Pergunta), write(' (sim/nao): '),
    read(Resposta),
    (Resposta = sim -> Lista = [Sintoma] ; Lista = []).

mostrar_resultado_formatado([]) :-
    write('nenhuma doenca foi identificada com os sintomas fornecidos.'), nl.
mostrar_resultado_formatado([(Doenca, Score)|Resto]) :-
    write('doenca: '), write(Doenca), 
    write(' / score da doenca e: '), write(Score), nl,
    mostrar_resultado_formatado(Resto).


% declaramos predicados que podem ser modificados durante a execução (adicionar/remover fatos). Pois serao usados no menu interativo
% dynamic serve para isso, pois fatos são fixos no código e dynamic permite usar o assert (adicionar) e o retract (remover) durante execucao
:- dynamic paciente/4. %(Nome, CPF, Telefone, Sintomas)
:- dynamic diag_paciente/3. % (CPF, Doenca, Score)
:- dynamic sessao_dia/1. % (NumPacientes)  

% sistema de gestao de pacientes diarios
iniciar_sessao :-
    write(' gestao de pacientes diarios '), nl,
    write('quantos pacientes serao atendidos hoje? '),
    read(N),
    retractall(sessao_dia(_)),
    retractall(paciente(_,_,_,_)),
    retractall(diag_paciente(_,_,_)),
    assert(sessao_dia(N)),
    nl, write('sessao iniciada para '), write(N), write(' pacientes.'), nl, nl,
    atender_pacientes(N, 1).

atender_pacientes(Total, Atual) :-
    Atual > Total, !,
    nl, write('todos os pacientes foram atendidos!'), nl, nl,
    gerar_relatorio.

atender_pacientes(Total, Atual) :-
    nl, write('PACIENTE '), write(Atual),  nl,
    cadastrar_paciente,
    Proximo is Atual + 1,
    atender_pacientes(Total, Proximo).

cadastrar_paciente :-
    write('nome do paciente: '),
    read(Nome),
    write('CPF: '),
    read(CPF),
    write('telefone: '),
    read(Telefone),
    write('digite os sintomas assim -> [sintoma1, sintoma2, ...]: '),
    read(Sintomas),
    assert(paciente(Nome, CPF, Telefone, Sintomas)),
    processar_diagnostico(CPF, Sintomas).

processar_diagnostico(CPF, Sintomas) :-
    diagnosticar_doenca(Sintomas, Resultados),
    salvar_diagnosticos(CPF, Resultados),
    nl, write(' diagnostico '), nl,
    mostrar_resultado_formatado(Resultados), nl.

salvar_diagnosticos(_, []).
salvar_diagnosticos(CPF, [(Doenca, Score)|Resto]) :-
    assert(diag_paciente(CPF, Doenca, Score)),
    salvar_diagnosticos(CPF, Resto).

gerar_relatorio :-
    write('relatorio diario de pacientes'), nl,
    sessao_dia(Total),
    write('total de pacientes atendidos: '), write(Total), nl, nl,
    listar_todos_pacientes.

listar_todos_pacientes :-
    paciente(Nome, CPF, Telefone, Sintomas),
    write('paciente: '), write(Nome), nl,
    write('CPF: '), write(CPF), nl,
    write('telefone: '), write(Telefone), nl,
    write('sintomas relatados: '), write(Sintomas), nl,
    write('diagnosticos:'), nl,
    listar_diagnosticos_paciente(CPF),
    nl,
    fail. % fail força o programa a buscar todos os pacientes / sem fail, pararia no primeiro
listar_todos_pacientes.

listar_diagnosticos_paciente(CPF) :-
    diag_paciente(CPF, Doenca, Score),
    write(' '), write(Doenca), 
    write(' (Score: '), write(Score), write(')'), nl,
    fail.
listar_diagnosticos_paciente(_).
%retractall e assert sao funcoes de controle do sistema, como um write ou read. dessa forma, sao primitivas de prolog apenas para controle

% Limpa dados anteriores: retractall(sessao_dia(_)) Remove sessoes antigas 
% retractall(paciente(_,_,_,_)) Remove pacientes antigos
% retractall(diagnostico_pac(_,_,_)) Remove diagnósticos antigos

% ja o assert(sessao_dia(3)) registra os pacientes diarios (3 pacientes hoje)
% o atender_pacientes(3, 1)  chama o atendimento, como total: 3, Atual: 1 / o programa vai rodando ate o numero de pacientes for maior que o total

