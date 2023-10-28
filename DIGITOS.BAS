DECLARE SUB digitos (casa, cor, digito)
CLS
SCREEN 7
COLOR 14, 1
        c = 13
        l = 120
        t = 50

        d = 729
Inicio:
'desenha quadro
        LINE (10, 25)-(310, 28), 15, BF
        LINE (10, 125)-(310, 128), 15, BF
        LINE (10, 25)-(15, 128), 15, BF
        LINE (310, 25)-(315, 128), 15, BF

WHILE NOT INKEY$ = CHR$(27)
        l$ = TIME$
        'LOCATE 1, 10: PRINT l$
        h1$ = MID$(l$, 1, 1)
        h2$ = MID$(l$, 2, 1)
        m1$ = MID$(l$, 4, 1)
        m2$ = MID$(l$, 5, 1)

        s1$ = MID$(l$, 7, 1)
        s2$ = MID$(l$, 8, 1)

        h1 = VAL(h1$)
        h2 = VAL(h2$)
        m1 = VAL(m1$)
        m2 = VAL(m2$)
        s1 = VAL(s1$)
        s2 = VAL(s2$)

        digitos 1, c, h1
        digitos 2, c, h2
        digitos 3, c, m1
        digitos 4, c, m2
        digitos 5, c, s1
        digitos 6, c, s2

WEND

END

SUB digitos (casa, cor, digito)
 DIM digit(10) AS STRING
  bx = 35
  alt = 40
  larg = (casa * (bx + 10)) - (bx - 20)
FOR h = 0 TO 1
  FOR i = 1 TO 3
  CIRCLE ((bx + 81), (alt + 10) + h * 40), i, cor + 4, , , 1
  CIRCLE ((bx + 68) * 2, (alt + 10) + h * 40), i, cor + 4, , , 1
  NEXT
NEXT
511 x = x + 1
    digit(0) = "1110111"
    digit(1) = "0010010"
    digit(2) = "0111101"
    digit(3) = "0111011"
    digit(4) = "1011010"
    digit(5) = "1101011"
    digit(6) = "1101111"
    digit(7) = "0110010"
    digit(8) = "1111111"
    digit(9) = "1111011"
    z = VAL(MID$(digit(digito), x, 1))
    IF z = 1 THEN z = z + cor
    IF x < 8 THEN GOTO 7 ELSE 80

7  ON x GOTO 10, 20, 30, 40, 50, 60, 70
10
 LINE (larg + 2, alt + 7)-(larg + 6, alt + 29), z, BF
 GOTO 511
20
 LINE (larg + 7, alt + 3)-(larg + 30, alt + 7), z, BF
 GOTO 511
30
 LINE (larg + 31, alt + 8)-(larg + 35, alt + 29), z, BF
 GOTO 511
40
 LINE (larg + 30, alt + 30)-(larg + 7, alt + 34), z, BF
 GOTO 511

50
 LINE (larg + 2, alt + 35)-(larg + 6, alt + 55), z, BF
 GOTO 511
60
 LINE (larg + 31, alt + 35)-(larg + 35, alt + 55), z, BF
 GOTO 511
70
 LINE (larg + 7, alt + 56)-(larg + 30, alt + 60), z, BF
 GOTO 511
80 x = 0
END SUB

