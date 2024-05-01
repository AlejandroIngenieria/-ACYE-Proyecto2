PrintCadena MACRO cadena
    MOV AH, 09h
    LEA DX, cadena
    INT 21h
ENDM 

OpenFile MACRO
    LOCAL ErrorToOpen, ExitOpenFile
    MOV AL, 2
    MOV DX, OFFSET filename + 2
    MOV AH, 3Dh
    INT 21h

    JC ErrorToOpen

    MOV handlerFile, AX
    PrintCadena salto
    PrintCadena exitOpenFileMsg
    JMP ExitOpenFile

    ErrorToOpen:
        MOV errorCode, AL
        ADD errorCode, 48

        PrintCadena salto
        PrintCadena errorOpenFile

        MOV AH, 02h
        MOV DL, errorCode
        INT 21h

    ExitOpenFile:
ENDM

CloseFile MACRO handler
    LOCAL ErrorToClose, ExitCloseFile
    MOV AH, 3Eh
    MOV BX, handler
    INT 21h

    JC ErrorToClose

    PrintCadena salto
    PrintCadena exitCloseFileMsg
    JMP ExitCloseFile

    ErrorToClose:
        MOV errorCode, AL
        ADD errorCode, 48

        PrintCadena salto
        PrintCadena errorCloseFile

        MOV AH, 02h
        MOV DL, errorCode
        INT 21h
    
    ExitCloseFile:
ENDM

ReadCSV MACRO handler, buffer
    LOCAL LeerByte, ErrorReadCSV, ExitReadCSV

    MOV BX, handler
    MOV CX, 1
    MOV DX, OFFSET buffer

    LeerByte:
        MOV AX, 3F00h
        INT 21h

        JC ErrorReadCSV

        INC DX
        MOV SI, DX
        SUB SI, 1

        SUB SI, OFFSET buffer

        CMP buffer[SI], 2Ch
        JNE LeerByte
        
        PUSH BX
        ConvertirNumero
        MOV DX, OFFSET buffer

        PUSH CX
        PUSH DX

        obtenerPosApuntador handler, 1, posApuntador

        POP DX
        POP CX
        POP BX
        MOV AX, posApuntador

        CMP extensionArchivo, AX
        JBE ExitReadCSV
        JMP LeerByte

    ErrorReadCSV:
        MOV errorCode, AL
        ADD errorCode, 48

        PrintCadena salto
        PrintCadena errorReadFile

        MOV AH, 02h
        MOV DL, errorCode
        INT 21h

    ExitReadCSV:
ENDM

ConvertirNumero MACRO
    LOCAL DosDigitosNum, FinConvertirNumero
    XOR AX, AX
    XOR BX, BX

    MOV DI, 0
    CMP SI, 2
    JNE UnDigitoNum

    DosDigitosNum:
        MOV AL, numCSV[DI]
        SUB AL, 48
        MOV BL, 10
        MUL BL
        INC DI

    UnDigitoNum:
        MOV BL, numCSV[DI]
        SUB BL, 48
        ADD AL, BL

    FinConvertirNumero:
        XOR BX, BX
        MOV BX, indexDatos
        MOV bufferDatos[BX], AL
        INC BX
        MOV indexDatos, BX

        MOV BX, numDatos
        INC BX
        MOV numDatos, BX
ENDM

GetSizeFile MACRO handler
    LOCAL ErrorGetSize, ExitGetSize
    obtenerPosApuntador handler, 2, extensionArchivo
    JC ErrorGetSize

    MOV extensionArchivo, AX
    obtenerPosApuntador handler, 0, posApuntador
    JC ErrorGetSize

    PrintCadena salto
    PrintCadena exitSizeFileMsg
    JMP ExitGetSize

    ErrorGetSize:
        MOV errorCode, AL
        ADD errorCode, 48

        PrintCadena salto
        PrintCadena errorSizeFile

        MOV AH, 02h
        MOV DL, errorCode
        INT 21h

    ExitGetSize:
ENDM

obtenerPosApuntador MACRO handler, posActual, bufferPos
    MOV AH, 42h
    MOV AL, posActual
    MOV BX, handler
    MOV CX, 0
    MOV DX, 0
    INT 21h

    MOV bufferPos, AX
ENDM

PedirCadena MACRO buffer
    LEA DX, buffer
    MOV AH, 0Ah
    INT 21h

    XOR BX, BX
    MOV SI, 2
    MOV BL, filename[1]
    ADD SI, BX
    MOV filename[SI], 0
ENDM

OrderData MACRO
    LOCAL for1, for2, Intercambio, terminarFor2
    XOR AX, AX
    XOR CX, CX
    XOR DX, DX

    MOV CX, numDatos
    DEC CX
    MOV DL, 0
    for1:
        PUSH CX

        MOV CX, numDatos
        DEC CX
        SUB CX, DX
        MOV BX, 0
        for2:
            MOV AL, bufferDatos[BX]
            MOV AH, bufferDatos[BX + 1]
            CMP AL, AH
            JA Intercambio
            INC BX
            LOOP for2
            JMP terminarFor2

            Intercambio:
                XCHG AL, AH
                MOV bufferDatos[BX], AL
                MOV bufferDatos[BX + 1], AH
                INC BX

            LOOP for2

        terminarFor2:
            POP CX
            INC DL
            LOOP for1
ENDM

Promedio MACRO
    LOCAL Sumatoria, CicloDecimal, ContinuarProm
    XOR AX, AX
    XOR BX, BX
    XOR CX, CX
    XOR DX, DX

    MOV CX, numDatos
    Sumatoria:
        MOV DL, bufferDatos[BX]
        ADD AX, DX
        INC BX
        MOV DX, 0
        LOOP Sumatoria
    
    MOV DX, 0
    MOV BX, numDatos
    DIV BX
    MOV entero, AX
    MOV decimal, DX
    MOV SI, 0

    CrearCadena entero, cadenaResult

    MOV cadenaResult[SI], 46
    INC SI

    CMP decimal, 0
    JNE CicloDecimal

    MOV cadenaResult[SI], 48
    INC SI
    MOV cadenaResult[SI], 48
    JMP ContinuarProm

    CicloDecimal:
        MOV AX, decimal
        MOV BX, 10
        MOV DX, 0
        MUL BX

        MOV BX, numDatos
        MOV DX, 0
        DIV BX

        MOV decimal, DX
        MOV entero, AX
        CrearCadena entero, cadenaResult
        MOV AL, cantDecimal
        INC AL
        MOV cantDecimal, AL
        CMP AL, 2
        JNE CicloDecimal

    ContinuarProm:
        MOV cantDecimal, 0
        PrintCadena salto
        PrintCadena msgPromedio
        PrintCadena cadenaResult
ENDM

CrearCadena MACRO valor, cadena
    LOCAL CICLO, DIVBASE, SALIRCC, ADDZERO, ADDZERO2

    CICLO:
        MOV DX, 0
        MOV CX, valor
        CMP CX, base
        JB DIVBASE

        MOV BX, base
        MOV AX, valor
        DIV BX
        MOV cadena[SI], AL
        ADD cadena[SI], 48
        INC SI

        MUL BX
        SUB valor, AX

        CMP base, 1
        JE SALIRCC
        
        DIVBASE:
            CMP valor, 0
            JE ADDZERO

            MOV AX, base
            MOV BX, 10
            DIV BX
            MOV base, AX
            JMP CICLO
            
            ADDZERO:
                MOV cadena[SI], 48
                INC SI
    SALIRCC:
ENDM

Maximo MACRO
    XOR AX, AX
    MOV BX, numDatos
    DEC BX
    MOV AL, bufferDatos[BX]
    MOV entero, AX
    MOV SI, 0

    CrearCadena entero, cadenaResult
    MOV cadenaResult[SI], 46
    INC SI
    MOV cadenaResult[SI], 48
    INC SI
    MOV cadenaResult[SI], 48
    INC SI
    MOV cadenaResult[SI], 36

    PrintCadena salto
    PrintCadena msgMaximo
    PrintCadena cadenaResult
ENDM

Minimo MACRO
    XOR AX, AX
    MOV AL, bufferDatos[0]
    MOV entero, AX
    MOV SI, 0

    CrearCadena entero, cadenaResult
    MOV cadenaResult[SI], 46
    INC SI
    MOV cadenaResult[SI], 48
    INC SI
    MOV cadenaResult[SI], 48
    INC SI
    MOV cadenaResult[SI], 36

    PrintCadena salto
    PrintCadena msgMinimo
    PrintCadena cadenaResult
ENDM

Mediana MACRO
    LOCAL CalcPromedio, ExitCalcMediana, CicloDecimal
    XOR AX, AX
    XOR BX, BX
    XOR DX, DX

    MOV AX, numDatos
    MOV BX, 2
    DIV BX

    MOV BX, AX

    CMP DX, 0
    JZ CalcPromedio
    
    XOR DX, DX
    MOV DL, bufferDatos[BX]
    MOV entero, DX
    MOV SI, 0

    CrearCadena entero, cadenaResult

    MOV cadenaResult[SI], 46
    INC SI
    MOV cadenaResult[SI], 48
    INC SI
    MOV cadenaResult[SI], 48
    INC SI
    MOV cadenaResult[SI], 36
    JMP ExitCalcMediana

    CalcPromedio:
        XOR AX, AX
        DEC BX
        ADD AL, bufferDatos[BX]
        ADD AL, bufferDatos[BX + 1]
        MOV DX, 0
        MOV BX, 2
        DIV BX
        MOV entero, AX
        MOV decimal, DX
        MOV SI, 0

        CrearCadena entero, cadenaResult

        MOV cadenaResult[SI], 46
        INC SI

        CMP decimal, 0
        JNE CicloDecimal

        MOV cadenaResult[SI], 48
        INC SI
        MOV cadenaResult[SI], 48
        INC SI
        MOV cadenaResult[SI], 36
        JMP ExitCalcMediana

        CicloDecimal:
            MOV AX, decimal
            MOV BX, 10
            MOV DX, 0
            MUL BX

            MOV BX, 2
            MOV DX, 0
            DIV BX

            MOV decimal, DX
            MOV entero, AX
            CrearCadena entero, cadenaResult
            MOV AL, cantDecimal
            INC AL
            MOV cantDecimal, AL
            CMP AL, 2
            JNE CicloDecimal

    ExitCalcMediana:
        MOV cantDecimal, 0
        PrintCadena salto
        PrintCadena msgMediana
        PrintCadena cadenaResult

ENDM

ContadorDatos MACRO
    XOR AX, AX
    MOV AX, numDatos
    MOV entero, AX
    MOV SI, 0

    CrearCadena entero, cadenaResult

    MOV cadenaResult[SI], 36

    PrintCadena salto
    PrintCadena msgContadorDatos
    PrintCadena cadenaResult
ENDM

BuildTablaFrecuencias MACRO
    LOCAL forDatos, saveFrecuencia, ExitModa
    XOR AX, AX
    XOR BX, BX
    XOR CX, CX
    XOR SI, SI

    MOV CX, numDatos
    MOV AH, bufferDatos[BX]
    forDatos:
        CMP AH, bufferDatos[BX]
        JNE saveFrecuencia

        INC AL
        INC BX
        LOOP forDatos

        MOV tablaFrecuencias[SI], AH
        INC SI
        MOV tablaFrecuencias[SI], AL
        INC SI 

        JMP ExitModa

        saveFrecuencia:
            MOV tablaFrecuencias[SI], AH
            INC SI
            MOV tablaFrecuencias[SI], AL
            INC SI

            MOV AH, numEntradas
            INC AH
            MOV numEntradas, AH

            MOV AH, bufferDatos[BX]
            MOV AL, 0
        
        JMP forDatos

    ExitModa:
ENDM

OrderFrecuencies MACRO
    LOCAL for1, for2, Intercambio, terminarFor2
    XOR AX, AX
    XOR BX, BX
    XOR CX, CX
    XOR DX, DX

    MOV CL, numEntradas
    DEC CX
    MOV DL, 0
    for1:
        PUSH CX

        MOV CL, numEntradas
        DEC CX
        SUB CX, DX
        MOV SI, 0
        for2:
            MOV AH, tablaFrecuencias[SI]
            MOV AL, tablaFrecuencias[SI + 1]
            MOV BH, tablaFrecuencias[SI + 2]
            MOV BL, tablaFrecuencias[SI + 3]

            CMP AL, BL
            JA Intercambio
            ADD SI, 2
            LOOP for2
            JMP terminarFor2

            Intercambio:
                XCHG AX, BX
                MOV tablaFrecuencias[SI], AH
                MOV tablaFrecuencias[SI + 1], AL
                MOV tablaFrecuencias[SI + 2], BH
                MOV tablaFrecuencias[SI + 3], BL
                ADD SI, 2

            LOOP for2
        
        terminarFor2:
            POP CX
            INC DL
            LOOP for1

ENDM

Moda MACRO
    LOCAL CicloModa, ExitCalcModa
    XOR AX, AX
    XOR BX, BX
    MOV AL, numEntradas
    MOV BL, 2
    MUL BL
    MOV DI, AX
    DEC DI

    CicloModa:
        XOR AX, AX
        XOR BX, BX

        MOV AL, tablaFrecuencias[DI] ; ? Frecuencia
        DEC DI
        MOV BL, tablaFrecuencias[DI] ; ? Valor
        DEC DI
        
        PUSH AX
        MOV entero, BX
        MOV SI, 0
        MOV base, 10000
        CrearCadena entero, cadenaResult
        MOV cadenaResult[SI], 36

        PrintCadena salto
        PrintCadena msgModa1
        PrintCadena cadenaResult
        POP AX
        MOV entero, AX
        
        PUSH AX
        MOV SI, 0
        MOV base, 10000

        CrearCadena entero, cadenaResult
        MOV AX, entero
        MOV valor, AX
        
        MOV cadenaResult[SI], 36

        PrintCadena salto
        PrintCadena msgModa2
        PrintCadena cadenaResult

        POP AX
        
        CMP AL, tablaFrecuencias[DI]
        JA ExitCalcModa
        JMP CicloModa

    ExitCalcModa:
ENDM

PrintTablaFrecuencias MACRO
    LOCAL tabla, ExitPrintTabla
    PrintCadena salto
    PrintCadena msgEncabezadoTabla
    PrintCadena salto

    XOR AX, AX
    XOR BX, BX
    MOV AL, numEntradas
    MOV CX, AX
    MOV BL, 2
    MUL BL
    MOV DI, AX
    DEC DI

    tabla:
        PUSH CX
        XOR AX, AX
        XOR BX, BX

        MOV AL, tablaFrecuencias[DI]
        DEC DI
        MOV BL, tablaFrecuencias[DI]  
        DEC DI

        PUSH AX
        MOV entero, BX
        MOV SI, 0
        MOV base, 10000
        CrearCadena entero, cadenaResult
        MOV cadenaResult[SI], 36
        PrintCadena espacios
        PrintCadena cadenaResult

        POP AX
        MOV entero, AX
        
        MOV SI, 0
        MOV base, 10000
        CrearCadena entero, cadenaResult
        MOV cadenaResult[SI], 36
        PrintCadena espacios

        MOV AH, 2
        MOV DL, 124
        INT 21h

        PrintCadena espacios
        PrintCadena cadenaResult
        PrintCadena salto

        POP CX
        DEC CX
        CMP CX, 0
        JE ExitPrintTabla
        JMP tabla

    ExitPrintTabla:
ENDM  

DibujarNumeros MACRO 
    LOCAL Numeros
    MOV fila, 0
    XOR AX, AX
    XOR BX, BX 
    XOR CX, CX
    XOR DX, DX
    MOV AL, numEntradas
    MOV CX, AX
    MOV BL, 2
    MUL BL
    MOV DI, AX
    DEC DI
    MOV dl,TablaFrecuencias[DI]  
    MOV Cx, DX 
    MOV conteoAux, CX 
    Mov lineas, CX
    Mov valor, cx
        
        Numeros:
        mov valor, CX 
        Conversion valor, cadenaV
        
        
         mov ah, 02h
         mov bh, 00h
         mov dh, fila
         mov dl, 0
         int 10h
         
         mov ah, 09h
         lea dx, cadenaV
         int 21h 
         
        
        ADD fila, 1            ; Incrementar la posici�n vertical para la siguiente l�nea
        
        DEC conteoAux             ; Decrementar conteo
        MOV CX, conteoAux 
        MOV base, 1000        
        CMP conteoAux, 0
        JGE Numeros
    
ENDM 
Conversion MACRO valor, cadena
    LOCAL CICLO, DIVBASE, SALIRCC, ADDZERO, ADDZERO2,WipeClean
    
    xor  bx,bx
    XOR SI, SI
    WipeClean:
     mov  [byte ptr cadena + bx], '$'
     inc  bx
     cmp  bx, 6
     jb   WipeClean  
     
    CICLO:
        MOV DX, 0
        MOV CX, valor
        CMP CX, base
        JB DIVBASE

        MOV BX, base
        MOV AX, valor
        DIV BX
        MOV [cadena + SI], AL
        ADD [cadena + SI], 48
        INC SI

        MUL BX
        SUB valor, AX

        CMP base, 1
        JE SALIRCC
        
        DIVBASE:
            CMP valor, 0
            JE ADDZERO

            MOV AX, base
            MOV BX, 10
            DIV BX
            MOV base, AX
            JMP CICLO
            
        ADDZERO:
            MOV [cadena + SI], 48
            INC SI
    SALIRCC:

ENDM 
DibujarLineas MACRO  
    LOCAL ciclo, ciclo2
    XOR AX,AX
    XOR BX,BX
    XOR CX,CX
    XOR DX,DX  
    
     
    
    MOV AH, 0Ch     ;codigo Interrupcion
    MOV AL, 100     ;color del pixel
    MOV DX, filaL   ;Posicion Fila 
    ADD lineas,1
    MOV CX, lineas;REpeticion de ciclo (cantidad de lineas)
    
    Ciclo:
        Push CX
        MOV CX, 20 ; columna donde empezara la linea
        
        Ciclo2:
            INT 10h
            INC CX  
            CMP CX, 320 ;Pinto la linea completa
            JNE Ciclo2 
            
        ADD DX, 8  ;distancia entre linea y linea
        Add filaL, 8
        POP CX
        LOOP ciclo 
     SUB filaL,8
    
    
ENDM 
Limpiar MACRO
    mov ah, 0Fh
    INT 10h
    mov ah, 0
    Int 10h

ENDM  
DibujarBarras MACRO
    Local Ciclo1, CicloF
    XOR AX,AX
    XOR BX,BX
    XOR CX,CX
    XOR DX,DX   
    
    MOV AH, 0Ch ;codigo Interrupcion
    MOV AL, color   ;color del pixel 
    
    MOV DX, filaL  ;Posicion Fila
    
    MOV BX, colum  ; Posicion inicial de la columna
    MOV CX, 10  ; Anchura de la barra
    
    CicloI:
        Push CX
        MOV CX, valor; Altura del la barra
        MOV DX, filaL ; fila donde iniciara la barra en cada ciclo 
        
        CicloF:
            PUSH CX
            MOV CX, BX
            INT 10h
            DEC DX 
            POP CX 
            LOOP CicloF
            
        INC BX
        POP CX
        LOOP CicloI 
        
    
ENDM
GraficaA MACRO
    LOCAL Pintar, salirMacro, AuxPintar
    Mov filaL,0
    MOV colum,25
    DibujarNumeros
    DibujarLineas
    MOV color, 7  
    MOV AX, filaL
    MOV filaN, AL
    XOR AX, AX 
    MOV AL, numEntradas
    MOV CX, AX  
        ;pintar barra
         
    XOR DX, DX 
    MOV DI, 0
    INC DI
    Pintar:
        push CX
        MOV dl,TablaFrecuencias[DI]
        MOV AX, DX
        MOV BX,8 
        mul BL 
        MOV valor, AX
            
        DibujarBarras
            
        ADD color, 10 
        ADD DI,2 
        ADD colum,15 
        XOR DX, DX
        POP CX
        DEC CX
        CMP CX,0
        JNE AuxPintar 
    jmp salirMacro
    
    AuxPintar:
        jmp Pintar
    salirMacro:
   MOV AH, 7   ; INPUT WITHOUT ECHO
   INT 21H
ENDM 
DibujarNumeros2 MACRO 
    LOCAL Numeros
    MOV fila, 0
    XOR AX, AX
    XOR BX, BX 
    XOR CX, CX
    XOR DX, DX
    MOV AL, numEntradas
    MOV CX, AX
    MOV BL, 2
    MUL BL
    MOV DI, AX
    DEC DI
    MOV dl,TablaFrecuencias[DI]  
    MOV Cx, DX 
    MOV conteoAux, CX 
    Mov lineas, CX
    Mov valor, cx
        
        Numeros:
        mov valor, CX 
        Conversion2 valor, cadenaV
        
        
         mov ah, 02h
         mov bh, 00h
         mov dh, fila
         mov dl, 0
         int 10h
         
         mov ah, 09h
         lea dx, cadenaV
         int 21h 
         
        
        ADD fila, 1            ; Incrementar la posici�n vertical para la siguiente l�nea
        
        DEC conteoAux             ; Decrementar conteo
        MOV CX, conteoAux 
        MOV base, 1000        
        CMP conteoAux, 0
        JGE Numeros
    
ENDM 
Conversion2 MACRO valor, cadena
    LOCAL CICLO, DIVBASE, SALIRCC, ADDZERO, ADDZERO2,WipeClean
    
    xor  bx,bx
    XOR SI, SI
    WipeClean:
     mov  [byte ptr cadena + bx], '$'
     inc  bx
     cmp  bx, 6
     jb   WipeClean  
     
    CICLO:
        MOV DX, 0
        MOV CX, valor
        CMP CX, base
        JB DIVBASE

        MOV BX, base
        MOV AX, valor
        DIV BX
        MOV [cadena + SI], AL
        ADD [cadena + SI], 48
        INC SI

        MUL BX
        SUB valor, AX

        CMP base, 1
        JE SALIRCC
        
        DIVBASE:
            CMP valor, 0
            JE ADDZERO

            MOV AX, base
            MOV BX, 10
            DIV BX
            MOV base, AX
            JMP CICLO
            
        ADDZERO:
            MOV [cadena + SI], 48
            INC SI
    SALIRCC:

ENDM 
DibujarLineas2 MACRO  
    LOCAL ciclo, ciclo2
    XOR AX,AX
    XOR BX,BX
    XOR CX,CX
    XOR DX,DX  
    
    MOV filaL,0 
    
    MOV AH, 0Ch     ;codigo Interrupcion
    MOV AL, 100     ;color del pixel
    MOV DX, filaL   ;Posicion Fila 
    ADD lineas,1
    MOV CX, lineas;REpeticion de ciclo (cantidad de lineas)
    
    Ciclo:
        Push CX
        MOV CX, 20 ; columna donde empezara la linea
        
        Ciclo2:
            INT 10h
            INC CX  
            CMP CX, 320 ;Pinto la linea completa
            JNE Ciclo2 
            
        ADD DX, 8  ;distancia entre linea y linea
        Add filaL, 8
        POP CX
        LOOP ciclo 
     SUB filaL,8
    
    
ENDM   
DibujarBarras2 MACRO
    Local CicloI2, CicloF2
    XOR AX,AX
    XOR BX,BX
    XOR CX,CX
    XOR DX,DX   
    
    MOV AH, 0Ch ;codigo Interrupcion
    MOV AL, color   ;color del pixel 
    
    MOV DX, filaL  ;Posicion Fila
    
    MOV BX, colum  ; Posicion inicial de la columna
    MOV CX, 10  ; Anchura de la barra
    
    CicloI2:
        Push CX
        MOV CX, valor; Altura del la barra
        MOV DX, filaL ; fila donde iniciara la barra en cada ciclo 
        
        CicloF2:
            PUSH CX
            MOV CX, BX
            INT 10h
            DEC DX 
            POP CX 
            LOOP CicloF2
            
        INC BX
        POP CX
        LOOP CicloI2 
    
        
    
ENDM

GraficaD MACRO 
    LOCAL pintar2
    Mov filaL,0 
    MOV colum, 25
    DibujarNumeros2
    DibujarLineas2
    MOV color, 7 
    
    MOV AX, filaL
    MOV filaN, AL
    
    
    XOR AX, AX
    XOR BX, BX
    MOV AL, numEntradas
    MOV CX, AX
    MOV BL, 2
    MUL BL
    MOV DI, AX
    DEC DI
    pintar2:
        push CX
        MOV dl,TablaFrecuencias[DI]
        MOV AX, DX
        MOV BX,8 
        mul BL 
        MOV valor, AX
            
        DibujarBarras2
            
        ADD color, 10 
        SUB DI,2 
        ADD colum,15 
        XOR DX, DX
        POP CX
        DEC CX
        CMP CX,0
        JNE pintar2 
  
  
  
  MOV AH, 7   ; INPUT WITHOUT ECHO
  INT 21H

ENDM  

; * CODIGO DE EJEMPLO PARA LA LECTURA DE UN CSV
.MODEL small
.STACK 100h
.DATA
    handlerFile         dw ?
    filename            db 30 dup(32)
    bufferDatos         db 300 dup (?)
    errorCode           db ?
    errorOpenFile       db "Ocurrio Un Error Al Abrir El Archivo - CODE: ", "$"
    errorCloseFile      db "Ocurrio Un Error Al Cerrar El Archivo - CODE: ", "$"
    errorReadFile       db "Ocurrio Un Error Al Leer El CSV - CODE: ", "$"
    errorSizeFile       db "Ocurrio Un Error Obteniendo El Size Del Archivo - CODE: ", "$"
    exitOpenFileMsg     db "El Archivo Se Abrio Correctamente", "$"
    exitCloseFileMsg    db "El Archivo Se Cerro Correctamente", "$"
    exitSizeFileMsg     db "Se Obtuvo La Longitud Correctamente", "$"
    msgToRequestFile    db "Ingrese El Nombre Del Archivo CSV: ", "$"
    msgPromedio         db "El Promedio De Los Datos Es: ", "$"
    msgMaximo           db "El Valor Maximo De Los Datos Es: ", "$"
    msgMinimo           db "El Valor Minimo De Los Datos Es: ", "$"
    msgMediana          db "El Valor De la Mediana De Los Datos Es: ", "$"
    msgContadorDatos    db "El Total De Datos Utilizados Ha Sido De: ", "$"
    msgModa1            db "La Moda De Los Datos Es: ", "$"
    msgModa2            db "Con Una Frecuencia De: ", "$"
    msgEncabezadoTabla  db "-> Valor    -> Frecuencia", "$"
    msgcomando          db "Ingrese el comando: ", "$"
    infoHeader          db 13,10, "ARQUITECTURA DE COMPUTADORES Y ENSAMBLADORES 1 A", 13,10, "PRIMER SEMESTRE 2024", 13,10, "Josue Alejandro Perez Benito", 13,10, "201712602", 13,10, "PROYECTO 2 ACYE",13,10, "$"
    ArchivoLeido        db 13,10, "Archivo leido correctamente","$"
    salto               db 10, 13, "$"
    espacios            db 32, 32, 32, 32, 32, "$"
    numCSV              db 3 dup(?)
    cadenaResult        db 6 dup("$")
    tablaFrecuencias    db 100 dup(?)
    numEntradas         db 1
    indexDatos          dw 0
    extensionArchivo    dw 0
    posApuntador        dw 0
    numDatos            dw 0
    base                dw 10000
    entero              dw ?
    decimal             dw ?
    cantDecimal         db 0  
    fila db 20
    filaL dw 0 
    filaN db 0            
    conteoAux dw 0 
    lineas dw 0
    cadenaV db 6 dup("$")  
    valor dw 0  
    colum dw 25 
    col db 0 
    color db 10  
    comando db  0
.CODE
    MOV AX, @data
    MOV DS, AX

    Main PROC 
        mov ah, 0
        mov al, 13h
        int 10h 
        
       PedirComando: 
        PrintCadena msgcomando 
        MOV AH, 1
        INT 21H 
        MOV BL, AL 
        MOV comando, BL  
        
        cmp comando,61h
        JE CAbrir
        
        cmp comando,70h
        JE AuxCPromedio
        
        cmp comando, 6Dh
        JE AuxComandoM
        
        cmp comando, 63h
        JE AuxCContador
        
        cmp comando, 67h
        JE AuxCGraficas
        
        cmp comando, 6Ch
        JE AuxCLimpiar
        
        cmp comando, 72h
        JE AuxCReporte
        
        cmp comando, 69h
        JE AuxCInfo
        
        cmp comando, 73h
        JE AuxSalir   
        
        jmp PedirComando
        
        AuxCPromedio:
            jmp CPromedio

        AuxComandoM:
            jmp ComandoM
        
        AuxCContador:
            jmp CContador
        
        AuxCGraficas:
            jmp CGraficas
        
        AuxCLimpiar:
            jmp CLimpiar
        
        AuxCReporte:
            jmp CReporte
        
        AuxCInfo:
            jmp CInfo
        
        AuxSalir:
            jmp Salir
                
        CAbrir:
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            PedirCadena filename
            OpenFile
            GetSizeFile handlerFile
            ReadCSV handlerFile, numCSV
            CloseFile handlerFile 
            OrderData 
            BuildTablaFrecuencias
            OrderFrecuencies
            MOV base, 10000 
            PrintCadena ArchivoLeido 
            MOV AH, 7   ; INPUT WITHOUT ECHO
            INT 21H 
            PrintCadena salto
            jmp PedirComando
    
        CPromedio:
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            Promedio
            MOV base, 10000
            MOV AH, 7   ; INPUT WITHOUT ECHO
            INT 21H 
            PrintCadena salto
            jmp PedirComando
        
        ComandoM:
            MOV AH, 1
            INT 21H 
            MOV BL, AL 
            MOV comando, BL
            
            cmp comando,65h
            JE AuxCMediana 
            
            cmp comando,6Fh
            JE AuxCModa
            
            cmp comando,61h
             je AuxCMax
            
            cmp comando,69h
            je AuxCMin   
            MOV AH, 7   ; INPUT WITHOUT ECHO
            INT 21H   
            PrintCadena salto
            jmp PedirComando
            
            AuxCMediana:
                jmp CMediana
            AuxCModa:
                jmp CModa
            AuxCMax:
                jmp CMax
            AuxCMin:
                jmp CMin            

            CMediana:
                MOV AH, 1
                INT 21H
                MOV AH, 1
                INT 21H
                MOV AH, 1
                INT 21H
                MOV AH, 1
                INT 21H
                MOV AH, 1
                INT 21H 
                Mediana
                MOV base, 10000 
                MOV AH, 7   ; INPUT WITHOUT ECHO
                INT 21H 
                PrintCadena salto
                jmp PedirComando 
            
            CModa:
                MOV AH, 1
                INT 21H
                MOV AH, 1
                INT 21H
                Moda
                MOV base, 10000
                MOV AH, 7   ; INPUT WITHOUT ECHO
                INT 21H 
                PrintCadena salto 
                jmp PedirComando
                
            CMax:
                MOV AH, 1
                INT 21H
                Maximo
                MOV base, 10000 
                MOV AH, 7   ; INPUT WITHOUT ECHO
                INT 21H 
                PrintCadena salto
                jmp PedirComando  
                
            CMin:
                MOV AH, 1
                INT 21H
                Minimo
                MOV base, 10000
                MOV AH, 7   ; INPUT WITHOUT ECHO
                INT 21H  
                PrintCadena salto
                jmp PedirComando 
        
        CContador:
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            ContadorDatos
            MOV base, 10000 
            MOV AH, 7   ; INPUT WITHOUT ECHO
            INT 21H  
            PrintCadena salto
            jmp PedirComando 
        
        CGraficas:
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H 
            MOV AH, 1
            INT 21H
            MOV BL, AL 
            MOV comando, BL
            
            cmp comando, 61h
            JE AuxGrafA
            
            cmp comando, 64h
            JE AuxGrafD
            PrintCadena salto
            jmp PedirComando
            
            AuxGrafA:
                jmp GrafA
            AuxGrafD:
                jmp GrafD

            GrafA:
                MOV AH, 1
                INT 21H
                MOV AH, 1
                INT 21H
                Limpiar
                GraficaA 
                PrintCadena salto 
                jmp PedirComando
                
            GrafD:
                MOV AH, 1
                INT 21H
                MOV AH, 1
                INT 21H
                MOV AH, 1
                INT 21H
                Limpiar
                GraficaD 
                PrintCadena salto
                jmp PedirComando 
                    
        CLimpiar: 
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            
            Limpiar  
            PrintCadena salto
            jmp PedirComando
        
        CReporte:
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H 
            PrintTablaFrecuencias
            MOV AH, 7   ; INPUT WITHOUT ECHO
            INT 21H 
            PrintCadena salto
            jmp PedirComando
            
        CInfo:
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H 
            
            PrintCadena infoHeader
            MOV AH, 7   ; INPUT WITHOUT ECHO
            INT 21H 
            PrintCadena salto
            jmp PedirComando
        
        Salir: 
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            MOV AH, 1
            INT 21H
            
        MOV AX, 4C00h
        INT 21h
        
    Main ENDP
END