10      '========================================================================'
20      '                   PROGRAMA COMPILAVEIS PARA EMPRESAS
30      '========================================================================'
100     CLS : WIDTH 80: KEY OFF: OPTION BASE 1
110     CLEAR : DEFINT A-F: DEFSNG Q, T, V: DIM d$(9), cn$(15), M$(5), t$(3): DIM ex(9)
120     LOCATE 5, 29: PRINT "CONTROLE DE ESTOQUE"
130     LOCATE 9, 22: PRINT "Nome do arquivo:": LOCATE 11, 33, 1, 2, 6: LINE INPUT na$: fl = INSTR(na$, ".")
140     IF fl THEN nb$ = LEFT$(na$, INSTR(na$, ",")) + "clt" ELSE nb$ = na$ + ".clt"
150     LOCATE 14, 26: PRINT "Insira o disquete apropriado..."
160     OPEN na$ FOR RANDOM AS #1 LEN = 128: OPEN nb$ FOR RANDOM AS #2 LEN = 32: cr = LOF(1) / 128
170     FIELD #1, 10 AS d$(1), 42 AS d$(2), 35 AS d$(3), 4 AS d$(4), 5 AS d$(5), 8 AS d$(6), 8 AS d$(7), 8 AS d$(8), 8 AS d$(9)
180     FIELD #2, 2 AS cv$, 2 AS cn$(1), 2 AS cn$(2), 2 AS cn$(3), 2 AS cn$(4), 2 AS cn$(5), 2 AS cn$(6), 2 AS cn$(7), 2 AS cn$(8), 2 AS cn$(9), 2 AS cn$(10), 2 AS cn$(11), 2 AS cn$(12), 2 AS cn$(13), 2 AS cn$(14), 2 AS cn$(15)
190     IF cr = 0 THEN LSET cv$ = MKI$(0): FOR cg = 1 TO 15: LSET cn$(cg) = MKI$(0): NEXT: PUT #2, 1
200     GET #2, 1: cv = CVI(cv$)
210     t$(1) = "o codigo:": t$(2) = "a denominacao:": t$(3) = "o nome do fornecedor:"
220     at = 2500: ex(1) = 10: ex(2) = 42: ex(3) = 35: ex(4) = 6: ex(5) = 5: ex(6) = 12: ex(7) = 12: ex(9) = 8
230     M$(1) = CHR$(218): M$(3) = CHR$(191): M$(4) = CHR$(192): M$(5) = CHR$(217)
240     '
250     ' *** MENU ***
260     '
270     CLS : M$(2) = STRING$(14, 196)
280     LOCATE 3, 6: PRINT M$(1) + M$(2) + " CONTROLE DE ESTOQUE - SOFTWARE NCASARI " + M$(2) + M$(3)
290     LOCATE 4, 6: PRINT M$(4) + STRING$(68, 196) + M$(5)
300     LOCATE 7, 8: PRINT "1 - ENTRADA DE ITENS"
310     LOCATE 7, 51: PRINT "LISTAGEM EMITIDA"
320     LOCATE 8, 58: PRINT "POR IMPRESSORA"
330     LOCATE 9, 8: PRINT "2 - SAIDAS E REPOSICOES"
340     LOCATE 10, 48: PRINT "6 - POR MINIMO ATINGIDO"
350     LOCATE 11, 11: PRINT "BUSCA DE ITEM COM OPCAO PARA"
360     LOCATE 12, 11: PRINT "BAIXAR/ALTERAR/CANCELAR:"
370     LOCATE 12, 48: PRINT "7 - POR FORNECEDOR"
380     LOCATE 14, 8: PRINT "3 - POR CODIGO DE PRODUTO"
390     LOCATE 14, 48: PRINT "8 - GERAL DO ARQUIVO"
400     LOCATE 16, 8: PRINT "4 - POR DENOMINACAO"
410     LOCATE 16, 48: PRINT "9 - GERAL, NO FORMATO"
420     LOCATE 18, 8: PRINT "5 - POR FORNECEDOR"
430     LOCATE 17, 52: PRINT "DE LISTA DE PRECOS"
440     FOR cg = 7 TO 18: LOCATE cg, 41: PRINT CHR$(179): NEXT
450     M$(2) = STRING$(5, 196)
460     LOCATE 21, 6: PRINT M$(1) + M$(2) + "  0 - ENCERRA O PROGRAMA " + STRING$(10, 196) + " DIGITE O No DA OPCAO  " + M$(2) + M$(3)
470     LOCATE 22, 6: PRINT M$(4) + STRING$(68, 196) + M$(5)
480     LOCATE 21, 71, 1, 2, 6: r$ = INPUT$(1): b = VAL(r$): IF b = 0 THEN 2530
490     ON b GOTO 520, 1160, 1370, 1370, 1370, 1900, 1900, 1900, 1900
500     GOTO 480
510     '
520     '   *** ENTRADA DE DADOS ***
530     '
540     cr = cr + 1: dc = cr: IF cv > 0 AND cv < 16 THEN dc = CVI(cn$(cv))
550     CLS : LOCATE 1, 21: PRINT "# ENTRADA No:"; dc: LOCATE 1, 40: PRINT "1 - CODIGO:": LOCATE 1, 63: PRINT ":"
560     LOCATE 4, 10: PRINT "2 - DENOMINACAO:": LOCATE 4, 56: PRINT CHR$(179)
570     LOCATE 6, 10: PRINT "3 - FORNECEDOR:"
580     LOCATE 8, 10: PRINT "4 - ESTOQUE:": LOCATE 8, 35: PRINT "5 - MINIMO:"
590     LOCATE 10, 10: PRINT "6 - PRECO CUSTO:"
600     LOCATE 12, 10: PRINT "7 - PRECO VENDA:"
610     LOCATE 14, 10: PRINT "8 - VALOR CUSTO:"
620     LOCATE 16, 10: PRINT "9 - ULTIMA COMPRA:"
630     a$ = "": a = 1: c = 52: f = c: e = 10: GOSUB 890 ' Codigo
640     IF a$ = "" OR LEFT$(a$, 1) = " " THEN 2480
650     IF cr > 1 AND a$ <> "" THEN GOSUB 2560
660     LSET d$(1) = a$: a$ = ""
670     a = 4: c = 27: f = c: e = 42: GOSUB 890         ' Denominacao
680     LSET d$(2) = a$: a$ = ""
690     a = 6: c = 27: f = c: e = 35: GOSUB 890         ' Fornecedor
700     LSET d$(3) = a$: a$ = ""
710     ct = 0: qt = 0: a = 8: c = 25: f = c: e = 6: GOSUB 890  'Estoque
720     GOSUB 2630: IF ct THEN a$ = "": GOTO 710
730     qt = VAL(a$): LSET d$(4) = MKS$(qt): a$ = "": 'qt = 0
740     a = 8: c = 47: f = c: e = 5: GOSUB 890          ' Minimo
750     LSET d$(5) = a$: a$ = ""
760     ct = 0: v = 0: a = 10: c = 27: f = c: e = 12: GOSUB 890' Preco custo
770     GOSUB 2630: IF ct THEN a$ = "": GOTO 760
780     v = VAL(a$): LSET d$(6) = MKD$(v): a$ = ""
790     ct = 0: v = 0: a = 12: c = 27: f = c: e = 12: GOSUB 890' Preco venda
800     GOSUB 2630: IF ct THEN a$ = "": GOTO 790
810     v = VAL(a$): LSET d$(7) = MKD$(v): a$ = "": v = 0
820     'v = CVS(d$(4)) * CVD(d$(6)): LSET d$(8) = MKD$(v)
830     v1 = CVS(d$(4)): v2 = CVD(d$(6)): v = v1 * v2: LSET d$(8) = MKD$(v)
840     GOSUB 2730: LOCATE 14, 24: PRINT c8$: v = 0: c8$ = ""  ' Valor Custo
850     a = 16: c = 29: f = c: e = 8: GOSUB 890         ' Data Ultima Compra
860     LSET d$(9) = a$: a$ = ""
870     GOTO 1060
880     '
890     ' *** CONTROLE DE EXTENSAO DOS DADOS DE ENTRADA ***
900     '
910     LOCATE a, f, 1, 2, 6: r$ = INPUT$(1)
920     IF ASC(r$) = 13 THEN 1020   ' ENTER
930     IF ASC(r$) = 8 THEN f = f - 1: IF f >= c THEN 980 ELSE f = c: GOTO 910' BACKSPACE
940     a$ = a$ + r$
950     IF LEN(a$) > e THEN a$ = LEFT$(a$, e): GOTO 910
960     LOCATE a, c: PRINT a$: f = f + 1
970     GOTO 910
980     ev = LEN(a$)
990     LOCATE a, c: PRINT STRING$(ev, 32)
1000    a$ = LEFT$(a$, ev - 1)
1010    LOCATE a, c: PRINT a$: GOTO 910
1020    RETURN
1030    '
1040    ' *** GRAVACAO DOS DADOS ***
1050    '
1060    LOCATE 20, 21: PRINT " - (R)egistra,  (A)nula  ou  (C)orrige  -";
1070    r$ = INPUT$(1): IF r$ = "R" OR r$ = "r" THEN 1110
1080    IF r$ = "A" OR r$ = "a" THEN cr = cr - 1: FOR cg = 1 TO 9: LSET d$(cg) = "": NEXT: GOTO 1130
1090    IF r$ = "C" OR r$ = "c" THEN 1680
1100    GOTO 1070
1110    IF cv > 0 AND cv < 16 THEN GET #2, 1: PUT #1, CVI(cn$(cv)): LSET cv$ = MKI$(cv - 1): LSET cn$(cv) = MKI$(0): PUT #2, 1: cv = cv - 1: cr = cr - 1: : GOTO 1130
1120    PUT #1, cr
1130    GOSUB 2490: LOCATE 20, 23: PRINT " *** Outro registro: (S)im ou (N)ao *** ";
1140    r$ = INPUT$(1): IF r$ = "S" OR r$ = "s" THEN 520 ELSE GOTO 250
1150    '
1160    ' *** SAIDAS E REPOSICOES ***
1170    '
1180    IF cr < 1 THEN 2510
1190    CLS : LOCATE 10, 20: LINE INPUT "Digite o codigo do Item:"; x$: IF x$ = "" THEN 250
1200    FOR cl = 1 TO cr
1210    GET #1, cl: IF LEFT$(d$(1), LEN(x$)) = x$ THEN 1230 ' Encontrou o registro
1220    NEXT: LOCATE 14, 29: PRINT "ITEM NAO ECONTRADO!": GOTO 2500
1230    LOCATE 1, 20: PRINT "O codigo "; x$; " corresponde ao item: ": GOSUB 2310
1240    LOCATE 20, 23: INPUT "(E)ntrada ou (S)aida do estoque"; x$: IF x$ <> "E" AND x$ <> "e" AND x$ <> "S" AND x$ <> "s" THEN GOSUB 2490: GOTO 1240
1250    GOSUB 2490: ct = 0: LOCATE 20, 25: INPUT "Qual e a quantidade "; a$: GOSUB 2630: IF ct THEN a$ = "": GOTO 1250
1260    qt = VAL(a$): IF x$ = "S" OR x$ = "s" THEN qt = qt * -1
1270    qa = CVS(d$(4)) + qt: LSET d$(4) = MKS$(qa): qt = 0: IF x$ = "S" OR x$ = "s" THEN 1310
1280    ct = 0: LOCATE 21, 21: LINE INPUT "Preco de Custo:"; a$: GOSUB 2630: IF ct THEN a$ = "": GOTO 1280
1290    LSET d$(6) = MKD$(VAL(a$)): a$ = ""
1300    LOCATE 21, 47: LINE INPUT "Data: "; a$: LSET d$(9) = a$: a$ = ""
1310    v = qa * CVD(d$(6)): LSET d$(8) = MKD$(v): qa = 0: v = 0: x$ = "": PUT #1, cl: GOSUB 2310
1320    GOSUB 2490: LOCATE 20, 24, 1, 2, 6: PRINT " (C)ontinua ou (V)olta ao MENU "
1330    r$ = INPUT$(1): IF r$ = "C" OR r$ = "c" THEN 1160
1340    IF r$ = "V" OR r$ = "v" THEN 250
1350    GOTO 1330
1360    '
1370    ' *** BUSCA DE DADOS ***
1380    '
1390    IF cr < 1 THEN 2510
1400    IF b = 3 THEN d = 1 ELSE IF b = 4 THEN d = 2 ELSE IF b = 5 THEN d = 3
1410    IF d < 1 OR d > 3 THEN 250
1420    CLS : LOCATE 7, 32: PRINT "BUSCA DE ITENS"
1430    LOCATE 10, 20: PRINT "Digite "; t$(d): LOCATE 12, 20: LINE INPUT x$: IF x$ = "" THEN 250
1440    PRINT : PRINT , , TAB(34); "AGUARDE!!!"
1450    FOR cl = 1 TO cr
1460    GET #1, cl
1470    IF x$ = LEFT$(d$(d), LEN(x$)) THEN GOSUB 2310: GOTO 1500    ' Achou o arquivo
1480    NEXT
1490    GOSUB 2490: LOCATE 20, 21: PRINT " - - - Atingido o FIM do arquivo! - - -": GOTO 2500
1500    GOSUB 2490: LOCATE 20, 22, 1, 2, 6: PRINT " (A)lterar, (C)ancelar, (S)eguir com busca, (V)oltar ao MENU";
1510    r$ = INPUT$(1): IF r$ = "A" OR r$ = "a" THEN dc = 0: GOTO 1680:
1520    IF r$ = "V" OR r$ = "v" THEN 250
1530    IF r$ = "C" OR r$ = "c" THEN 1580
1540    IF (r$ = "S" OR r$ = "s") AND cl = cr THEN 1490
1550    IF (r$ = "S" OR r$ = "s") AND (d = 1 XOR d = 2 XOR d = 3) THEN 1480
1560    GOTO 1510
1570    '
1580    ' *** CANCELAMENTO DE DADOS ***
1590    '
1600    GOSUB 2490: LOCATE 20, 20: PRINT " - - - Confitma o cancelamento? (S)im ou (N)ao - - -";
1610    r$ = INPUT$(1): IF r$ <> "S" AND r$ <> "s" THEN 1500
1620    LSET d$(1) = "": LSET d$(2) = "": LSET d$(3) = "": LSET d$(4) = MKS$(0): LSET d$(5) = "": LSET d$(6) = MKS$(0): LSET d$(7) = MKS$(0): LSET d$(8) = MKS$(0): LSET d$(9) = "": PUT #1, cl
1630    GET #2, 1: cv = CVI(cv$): cv = cv - 1: IF cv > 15 THEN 1650
1640    LSET cv$ = MKI$(cv): LSET cn$(cv) = MKI$(cl): PUT #2, 1
1650    GOSUB 2490: LOCATE 20, 13: PRINT " - - - Registro cancelado com sucesso! Tecle ENTER para continuar"; : r$ = INPUT$(1):
1660    GOSUB 2310: GOTO 1500
1670    '
1680    ' *** ALTERACAO DE DADOS ***
1690    '
1700    GOSUB 2490: LOCATE 20, 23: PRINT "Qual o numero do item para corrigir "; : al = 0: INPUT al
1710    IF al < 1 OR al > 9 THEN 1700
1720    IF al = 8 THEN GOSUB 2490: LOCATE 20, 21: PRINT "O Item so pode ser corrigido indiretamente atraves dos Itens 4 e 6. ": LOCATE , 21: PRINT "Tecle ENTER para continuar."; : r$ = INPUT$(1): GOTO 1700
1730    GOSUB 2490: LOCATE 20, 27: PRINT "Digite o item "; al; " corrigido."
1740    LOCATE 22, 20: PRINT ":": bf = ex(al): LOCATE 22, bf + 21: PRINT ":"
1750    ct = 0: a = 22: c = 21: f = c: e = ex(al): GOSUB 890: c$ = a$: a$ = ""
1760    IF al = 4 XOR al = 6 XOR al = 7 XOR al = 8 THEN a$ = c$: GOSUB 2630: IF ct THEN 1750 ELSE c$ = a$: a$ = ""
1770    ON al GOTO 1780, 1780, 1780, 1790, 1780, 1800, 1810, 1720, 1780
1780    LSET d$(al) = c$: c$ = "": GOTO 1830
1790    qt = VAL(c$): LSET d$(al) = MKS$(qt): v = qt * CVD(d$(6)): qt = 0: GOTO 1820
1800    v = VAL(c$): LSET d$(al) = MKD$(v): v = CVS(d$(4)) * v: GOTO 1820
1810    v = VAL(c$): LSET d$(al) = MKD$(v): c$ = "": GOTO 1830
1820    LSET d$(8) = MKD$(v): v = 0: c$ = "": GOTO 1830
1830    GOSUB 2310: GOSUB 2490
1840    LOCATE 20, 23: PRINT " - - - Outra alteracao? (S)im ou (N)ao - - -";
1850    r$ = INPUT$(1): IF r$ = "S" OR r$ = "s" THEN 1700
1860    GOSUB 2490: LOCATE 20, 12: PRINT " - - - Alteracao efetuada com sucesso! Tecle ENTER para continuar."; r$ = INPUT$(1)
1870    IF dc THEN 1110
1880    PUT #1, cl: GOTO 1500
1890    '
1900    ' *** EMISSAO DE LISTAGEM
1910    '
2300    '
2310    ' *** EXIBICAO DE DADOS ***
2320    '
2330    IF dc AND b = 1 THEN de = cl: cl = dc
2340    CLS : LOCATE 2, 45: PRINT "REGISTRO No: "; cl: LOCATE 3, 20: PRINT STRING$(50, 205)
2350    LOCATE 4, 20: PRINT "1 - Codigo: "; d$(1)
2360    LOCATE 5, 20: PRINT "2 - Descricao: "; d$(2)
2370    LOCATE 6, 20: PRINT "3 - Fornecedor: "; d$(3)
2380    LOCATE 7, 20: PRINT "4 - Estoque: "; CVS(d$(4));
2385    LOCATE 7, 53: PRINT "5 - Minimo: "; d$(5)
2390    LOCATE 8, 20: PRINT "6 - Preco Custo: "; : v = CVD(d$(6)): GOSUB 2730: PRINT c8$
2400    LOCATE 9, 20: PRINT "7 - Preco Venda: "; : v = CVD(d$(7)): GOSUB 2730: PRINT c8$
2410    LOCATE 10, 20: PRINT "8 - Valor Custo: "; : v = CVD(d$(8)): GOSUB 2730: PRINT c8$
2420    LOCATE 11, 20: PRINT "9 - Data da Ult. Compra:  "; d$(9)
2430    IF dc AND b = 1 THEN cl = de
2440    RETURN
2450    '
2460    ' *** SUB-ROTINAS DIVERSAS ***
2470    '
2480    CLS : cr = cr - 1: LOCATE 10, 22: PRINT "O CODIGO E IMPRESCINDIVEL! DIGITE - O ": r$ = INPUT$(1): FOR cg = 1 TO at: NEXT: GOTO 540
2490    LOCATE 20, 10: PRINT SPACE$(60): LOCATE , 10: PRINT SPACE$(60): LOCATE , 10: PRINT SPACE$(60): RETURN
2500    LOCATE 21, 21: PRINT "Pressione uma tecla para voltar ao MENU."; : r$ = INPUT$(1): GOTO 250
2510    CLS : LOCATE 12, 23: PRINT "AINDA NAO HA REGISTRO NO ARQUIVO!"
2520    FOR cg = 1 TO at: NEXT: r$ = INPUT$(1): GOTO 250
2530    CLOSE : CLS : LOCATE 11, 20: PRINT "NAO SE ESQUECA DE DESLIGAR OS APARELHOS!"
2540    END
2550    '
2560    ' *** CHECAGEM DE DUPLICIDADE DOS ITENS DE ESTOQUE ***
2570    '
2580    FOR cl = 1 TO cr - 1
2590    GET #1, cl
2600    IF a$ = LEFT$(d$(1), LEN(a$)) THEN cr = cr - 1: CLS : LOCATE 10, 20: PRINT "CODIGO JA EXISTE NO BANCO DE DADOS. VERIFIQUE!": GOTO 2500
2610    NEXT: RETURN
2620    '
2630    ' *** CONVERSAO $ BRA/CPU
2640    '
2650    IF a$ = "" THEN RETURN
2660    b$ = "": ev = LEN(a$): FOR ce = 1 TO ev
2670    IF MID$(a$, ce, 1) = "." THEN 2710
2680    IF MID$(a$, ce, 1) = "," THEN MID$(a$, ce, 1) = "."' Troca virgua por ponto
2690    IF ASC(MID$(a$, ce, 1)) < 45 OR ASC(MID$(a$, ce, 1)) > 57 THEN ct = 1: RETURN
2700    b$ = b$ + MID$(a$, ce, 1)
2710    NEXT:   aa$ = b$: b$ = "": RETURN
2720    '
2730    ' *** CONVERSAO R$ / STRING
2740    '
2750    v1 = FIX(v): v2 = ABS(v - v1) + .009: v1$ = STR$(v): v2 = VAL(LEFT$(STR$(v2), 4))
2760    v2$ = MID$(STR$(v1), 2): IF v2 < .01 THEN v3$ = "00": GOTO 2780
2770    v3$ = MID$(v1$, (INSTR(v1$, ".") + 1), 2): IF LEN(v3$) = 1 THEN v3$ = v3$ + "0"
2780    c$ = RIGHT$(SPACE$(11) + v2$ + v3$, 13): c8$ = LEFT$(c$, 2) + "." + MID$(c$, 3, 3) + "." + MID$(c$, 6, 3) + "." + MID$(c$, 9, 3) + "," + MID$(c$, 12, 2): RETURN

