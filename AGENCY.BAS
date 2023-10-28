10      ' PROGRAMA
15      DECLARE SUB Moldura (left!, top!, larg!, alt!)
20      ' CRIA ARQUIVO
30      CLS : DIM cr AS INTEGER: cr = 1: DIM saldo AS DOUBLE: DIM l AS DOUBLE:
40      filename$ = "CONTA.BCO"
50      OPEN filename$ FOR RANDOM AS #1 LEN = 110: cr = LOF(1) / 110
60      FIELD #1, 10 AS d$(1), 30 AS d$(2), 15 AS d$(3), 15 AS d$(4), 30 AS d$(5), 10 AS d$(6)
70      LOCATE 12, 25: PRINT "Inicializando o sistema..."
80      GOSUB 8000: IF cr = 0 THEN PUT #1, cr + 1
81      CLS : Moldura 10, 4, 60, 18: LOCATE 5, 20: PRINT "LISTA DAS CONTAS EXISTENTE"
        LOCATE 6, 20: PRINT STRING$(26, CHR$(61))
        FOR cd = 1 TO cr
         GET #1, cd
         LOCATE , 20: PRINT d$(1); " - "; d$(2); " - "
         LOCATE , 20: PRINT "R$ "; : v = CVD(d$(6)): GOSUB 4800: PRINT c8$
        NEXT
        r$ = INPUT$(1)
100     '
110     ' Menu
120     CLS : Moldura 10, 4, 60, 18
130     LOCATE 5, 30: PRINT "AGENCIA BANCARIA"
140     LOCATE 6, 30: PRINT STRING$(16, CHR$(61))
150     LOCATE 8, 20: PRINT "1 - CRIAR CONTA"
160     LOCATE , 20: PRINT "2 - DEPOSITAR"
170     LOCATE , 20: PRINT "3 - SACAR"
180     LOCATE , 20: PRINT "4 - TRANSFERIR"
190     LOCATE , 20: PRINT "5 - LISTAR CONTA"
200     LOCATE , 20: PRINT "6 - SAIR"
210     LOCATE 15, 20: PRINT "ENTRE COM SUA OPCAO: ";
280     r$ = INPUT$(1): PRINT r$: b = VAL(r$): IF b < 1 OR b > 6 THEN GOTO 210
290     ON b GOTO 300, 2000, 2500, 3500, 1000, 3000

300     '
310     ' Cria conta
320     '
330     CLS : Moldura 10, 4, 60, 18: CLS : LOCATE 13, 31: PRINT "Criar conta...": GOSUB 8000: CLS : Moldura 10, 4, 60, 18: LOCATE 5, 22: PRINT "AGENCIA BANCARIA - CRIACAO DE CONTA": LOCATE 6, 22: PRINT STRING$(35, CHR$(61))
335     cr = cr + 1: dc = cr 'IF cr > 0 THEN GET #1, cr
336     cp = cr: GOSUB 9000
        'END
340     LOCATE 8, 20: PRINT "Conta n§: "; conta$: LOCATE 9, 20: PRINT STRING$(40, CHR$(61))
'340     LOCATE 8, 20: PRINT "Conta n§: "; dc: LOCATE 9, 20: PRINT STRING$(40, CHR$(61))
350     LOCATE 10, 20: PRINT "Nome: "; : a = 1: GOSUB 400
360     LOCATE 11, 20: PRINT "CPF:"; : a = 2: GOSUB 400
370     LOCATE 12, 20: PRINT "Fone:"; : a = 3: GOSUB 400
380     LOCATE 13, 20: PRINT "Email:"; : a = 4: GOSUB 400
390     LOCATE 15, 20: PRINT STRING$(40, CHR$(196)): sd$ = "0.0": saldo = VAL(sd$)
391     LOCATE 16, 20: PRINT "(S)alvar (C)ancelar"; : t$ = "": t$ = INPUT$(1)
392     IF t$ = "s" OR t$ = "S" THEN GOTO 7000
393     IF t$ = "c" OR t$ = "C" THEN CLS : LOCATE 13, 30: PRINT "Voltando ao menu...": GOSUB 8000: CLS : GOTO 100
394     GOTO 391

400     '
420     'Controle de entrada
430     '
440     IF a = 1 THEN LOCATE 10, 30: LINE INPUT nome$
450     IF a = 2 THEN LOCATE 11, 30: LINE INPUT cpf$
460     IF a = 3 THEN LOCATE 12, 30: LINE INPUT fone$
470     IF a = 4 THEN LOCATE 13, 30: LINE INPUT email$
490     RETURN
1000    '
1010    ' Listar Conta
1020    '
1030    CLS : Moldura 10, 4, 60, 18: LOCATE 13, 29: PRINT "Listar Contas...": GOSUB 8000: CLS : Moldura 10, 4, 60, 18:
1040    LOCATE 5, 25: PRINT "AGENCIA BANCARIA - LISTAR CONTA": LOCATE 6, 25: PRINT STRING$(31, CHR$(61))
1050    LOCATE 8, 20: PRINT "Digite o numero da conta: "; : LINE INPUT conta$
1054    cp = VAL(conta$): cd = cp: GOSUB 9000 ': PRINT dv ': t$ = INPUT$(1)
1055    LOCATE 9, 20: PRINT "Digito Verificador da conta: "; : r$ = INPUT$(1): dx = VAL(r$)
1061    IF dx <> dv THEN CLS : LOCATE 13, 26: PRINT "CONTA NAO EXISTENTE!!!": GOSUB 8000: GOTO 100
1063    IF cp > cr THEN CLS : LOCATE 13, 26: PRINT "Sem conta para exibir": GOSUB 8000: GOTO 100
1065    IF cr = 0 OR cp = 0 THEN CLS : LOCATE 13, 26: PRINT "Sem conta para exibir": GOSUB 8000: GOTO 100
1066    LOCATE 8, 20: PRINT STRING$(50, 32)
1070    GET #1, cp
1250    LOCATE 10, 20: PRINT "Conta:  "; d$(1)
1260    LOCATE 12, 20: PRINT "Nome:  "; d$(2)
1270    LOCATE 13, 20: PRINT "CPF:   "; d$(3)
1280    LOCATE 14, 20: PRINT "Fone:  "; d$(4)
1290    LOCATE 15, 20: PRINT "Email: "; d$(5)
1300    LOCATE 16, 20: PRINT "Saldo: R$ "; : v = CVD(d$(6)): GOSUB 4800: PRINT c8$
1310    LOCATE 9, 20: PRINT STRING$(40, CHR$(61))
1320    LOCATE 11, 20: PRINT STRING$(40, CHR$(61))
1330    LOCATE 17, 20: PRINT STRING$(40, CHR$(61)): LOCATE 18, 20: PRINT "Pressione qualquer tecla para voltar ao menu"
1340    r$ = INPUT$(1): GOTO 100

2000    '
2010    ' Depositar na conta
2020    '
2030    CLS :  CLS : LOCATE 13, 30: PRINT "Abrindo conta...": GOSUB 8000: CLS : Moldura 10, 4, 60, 18
2040    LOCATE 5, 25: PRINT "AGENCIA BANCARIA - LISTAR CONTA": LOCATE 6, 25: PRINT STRING$(31, CHR$(61))
2050    LOCATE 8, 20: PRINT "Digite o numero da conta: "; : LINE INPUT conta$
2060    cp = VAL(conta$): IF cp > cr OR cp < 1 THEN GOTO 1050
2070    GET #1, cp
2080    LOCATE 10, 20: PRINT "Saldo atual: R$ "; : v = CVD(d$(6)): GOSUB 4800: PRINT c8$
2090    LOCATE 12, 20: PRINT "Digite o valor para deposito: R$ "; : LINE INPUT valor$
2100    ent = VAL(valor$)
2190    sd = CVD(d$(6)) + ent: LSET d$(6) = MKD$(sd)
2200    PUT #1, cp
2300    LOCATE 15, 20: PRINT "Pressione uma tecla para voltar ao menu": r$ = INPUT$(1)
2310    GOTO 100

2500    '
2510    ' Sacar da conta
2520    '
2530    CLS : LOCATE 13, 30: PRINT "Sacar da Conta...": GOSUB 8000: CLS : Moldura 10, 4, 60, 18:
2540    LOCATE 5, 25: PRINT "AGENCIA BANCARIA - SACAR DA CONTA": LOCATE 6, 25: PRINT STRING$(33, CHR$(61))
2550    LOCATE 8, 20: PRINT "Digite o numero da conta: "; : LINE INPUT conta$
2560    cp = VAL(conta$): IF cp > cr OR cp < 1 THEN GOTO 1050
2570    GET #1, cp
2580    LOCATE 10, 20: PRINT "Saldo atual: R$ "; : v = CVD(d$(6)): GOSUB 4800: PRINT c8$
2590    LOCATE 12, 20: PRINT "Digite o valor para deposito: R$ "; : LINE INPUT valor$: ent = VAL(valor$)
2600    LOCATE 12, 52:  sd = CVD(d$(6))
2610    IF (sd > 0 AND sd > ent) THEN
2620    sd = sd - ent
2630    LSET d$(6) = MKD$(sd)
2640    PUT #1, cp
2660    LOCATE 20, 20: PRINT "Saque realizado com Sucesso!"
2670    ELSE
2680    LOCATE 15, 20: PRINT "Saldo insulficiente!"
2690    END IF
2700    LOCATE 16, 20: PRINT "Presione uma tecla para voltar ao menu": r$ = INPUT$(1): GOTO 100

3000    '
3005    ' Finalizar o programa
3010    '
3020    CLS : Moldura 10, 4, 60, 18: LOCATE 12, 27: PRINT "ENCERRAMENTO DO PROGRAMA"
3030    LOCATE 13, 27: PRINT STRING$(24, CHR$(61)): CLOSE #1
3090    GOSUB 8000: CLS : END

3500    '
3510    ' Transferencia entre Contas
3520    '
3530    CLS : Moldura 10, 4, 60, 18: LOCATE 13, 30: PRINT "Transferencia...": GOSUB 8000: CLS : Moldura 10, 4, 60, 18
3540    LOCATE 5, 20: PRINT "AGENCIA BANCARIA - TRANSFERENCIA ENTRE CONTAS": LOCATE 6, 20: PRINT STRING$(45, CHR$(61))
3550    LOCATE 8, 20: PRINT "Digite a sua conta: "; : LINE INPUT conta$
3570    cOrigem = VAL(conta$): IF cOrigem > cr OR cOrigem < 1 THEN GOTO 3550
3580    GET #1, cOrigem
3590    LOCATE 10, 20: PRINT "Saldo atual: R$ "; : v = CVD(d$(6)): GOSUB 4800: PRINT c8$
3600    LOCATE 8, 20: PRINT "Digite a Conta Destino: "; : LINE INPUT destino$
3610    cDestino = VAL(destino$): IF cDestino > cr OR cDestino < 1 THEN GOTO 3550
3620    GET #1, cDestino
3630    LOCATE 12, 20: PRINT "Digite o valor para transferir: R$ "; : LINE INPUT valor$: ent = VAL(valor$)
3640    LOCATE 12, 52:  sd = CVD(d$(6))
3650    GET #1, cOrigem
3660    IF (ent <= sd) OR (sd > 0) THEN
3670    sd = sd - ent: LSET d$(6) = MKD$(sd)
3680    ELSE
3690    LOCATE 14, 20: PRINT "Saldo insulficiente!";
3700    LOCATE 15, 20: PRINT "Pressione uma tecla para voltar": r$ = INPUT$(1)
3710    LOCATE 14, 20: PRINT STRING$(40, CHR$(32)): LOCATE 15, 20: PRINT STRING$(40, CHR$(32))
3720    GOTO 3630
3750    END IF
3760    PUT #1, cOrigem
3770    GET #1, cDestino
3780    sd = CVD(d$(6)): sd = sd + ent: LSET d$(6) = MKD$(sd)
3790    PUT #1, cDestino
3800    LOCATE 19, 20: PRINT "Transferencia realizada com sucesso!"
3810    LOCATE 20, 20: PRINT "Pressione uma tecla para voltar para o menu": r$ = INPUT$(1)
3820    GOTO 100

4800    '
4810    ' Conversao R$ / STRing
4815    '
4820    v1 = FIX(v): v2 = ABS(v - v1) + .009: v1$ = STR$(v): v2 = VAL(LEFT$(STR$(v2), 4))
4830    v2$ = MID$(STR$(v1), 2): IF v2 < .01 THEN v3$ = "00": GOTO 4860
4340    v3$ = MID$(v1$, (INSTR(v1$, ".") + 1), 2): IF LEN(v3$) = 1 THEN v3$ = v3$ + "0"
4860    c$ = RIGHT$(SPACE$(11) + v2$ + v3$, 13): c8$ = LEFT$(c$, 2) + "." + MID$(c$, 3, 3) + "." + MID$(c$, 6, 3) + "." + MID$(c$, 9, 3) + "," + MID$(c$, 12, 2): RETURN

7000    '
7020    ' Gravacao dos dados
7030    '
7040    LSET d$(1) = conta$   'LSET d$(1) = STR$(dc)
7050    LSET d$(2) = nome$
7060    LSET d$(3) = cpf$
7070    LSET d$(4) = fone$
7080    LSET d$(5) = email$
7090    LSET d$(6) = MKD$(saldo)
7100    IF cl < 1 THEN cl = 1
7110    PUT #1, cr 'CLOSE #1
7120    GOTO 100

8000    '
8010    ' Animacao de barras
8020    '
8030
8040    Moldura 10, 4, 60, 18: COLOR 1, 0
8050    LOCATE 14, 17: PRINT CHR$(218) + STRING$(41, CHR$(196)) + CHR$(191)
8060    LOCATE 16, 17: PRINT CHR$(192) + STRING$(41, CHR$(196)) + CHR$(217)
8070    FOR z = 1 TO 20
8080    LOCATE 15, 36 + z: PRINT CHR$(219) + "  " + CHR$(179)
8090    LOCATE 15, 37 - z: PRINT CHR$(179) + "  " + CHR$(219)
8100    FOR x = 1 TO 80000: NEXT x: NEXT z: COLOR 7, 0
8110    RETURN

9000   '
9010   ' Criar conta
9020   '
9030  
9040 
9050 'cr = 1584: CLS
9060  
        p$ = LTRIM$(RTRIM$(STR$(cp)))
        k = LEN(p$)
        r$ = MID$("0000", 1, 4 - k)
        r$ = r$ + p$
        'PRINT r$
        'PRINT "Conta Formatada"
        d1$ = MID$(r$, 1, 1)
        d2$ = MID$(r$, 2, 1)
        d3$ = MID$(r$, 3, 1)
        d4$ = MID$(r$, 4, 1)

        'PRINT d1$; d2$; d3$; d4$
        'PRINT "----------------------"
        d1 = VAL(d1$): d2 = VAL(d2$): d3 = VAL(d3$): d4 = VAL(d4$):
soma = (d1 * 5) + (d2 * 4) + (d3 * 3) + (d4 * 2)
'PRINT
'PRINT "Soma: "; soma
'PRINT
x = soma MOD 11
digito = 11 - x
'PRINT "Diferenca: 11 - resto: "; digito
dv$ = STR$(digito)
IF digito = 10 THEN dv$ = "X"
IF digito = 11 THEN dv$ = "0"
'PRINT
dv$ = LTRIM$(RTRIM$(dv$))
'PRINT "CONTA: "; d1$ + d2$ + d3$ + d4$ + "-"; dv$
dv = VAL(dv$)
conta$ = d1$ + d2$ + d3$ + d4$ + "-" + dv$
'PRINT VAL(conta$)
RETURN

    SUB Moldura (left, top, larg, alt)
        LOCATE top, left: PRINT CHR$(218) + STRING$(larg, CHR$(196)) + CHR$(191)
        FOR n = top + 1 TO alt + 1
          LOCATE n, left: PRINT CHR$(179)
          LOCATE n, left + larg + 1: PRINT CHR$(179)
        NEXT n
        LOCATE top + alt - 2, left: PRINT CHR$(192) + STRING$(larg, CHR$(196)) + CHR$(217)
    END SUB

