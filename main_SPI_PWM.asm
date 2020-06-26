#include "C:\Program Files (x86)\Microchip\MPLABX\v5.25\mpasmx\p18f4550.inc"
  LIST p=18F4550	    ;Tipo de microprocesador
 
udata_acs
;Variables
data_to_send RES 1			    ; Reservamos un byte de memoria para enviar datos por SPI
save_data RES 1				    ;reservamos memoria para recibir datos
save_data_1 RES 1				    ;reservamos memoria para recibir datos
save_data_2 RES 1			    ; Reservamos memoria para recibir datos
;Variables del delay 1s
DCounter1 EQU 0X0C
DCounter2 EQU 0X0D
DCounter3 EQU 0X0E
    
Duty_On	    EQU	    0x20	;Vairalbes de ancho de pulso
Duty_Off    EQU	    0x21
    
  CODE 0x00
;---------FRECUENCIA DEL OSCILADOR------------------
;seleccion del oscilador interno como fuente de reloj del CPU
BSF OSCCON,SCS1,0 ;
BSF OSCCON,SCS0,0 ; 
;configuro el PLL para 8MHz de fosc al cpu 
BSF OSCCON, IRCF2,0
BSF OSCCON, IRCF1,0
BSF OSCCON, IRCF0,0
;configuro puerto D como salida
CLRF PORTD	    ;limpia el puerto D
MOVLW B'00000000'   ;mueve 0 a W
MOVWF TRISD,ACCESS  ;mueve W a TRISD 
MOVLW B'00000000'   ;mueve 0 a W
MOVWF LATD,ACCESS   ;mueve W a LATD
;Set CCP1 pin como salida puerto C2 como salida
CLRF TRISC

;-------------------------------------Configuración del SPI al Inicio-------------------------------------
Start_SPI_master:
    ; Limpiamos puertos a utilizar
    CLRF PORTC				    ; Limpiamos puerto C
    CLRF PORTB				    ; Limpiasmo puerto B
    ;B1 SCK
    ;B0 (MOSI)
    ;C7 SDO (MISO)
    ;
    ;Configuración del B1 SCK como salida en el puerto y RB0 SDI(MOSI) como entrada en el puerto 0
    MOVLW 0x01				    ; Pasamos 0x01 al registro W, para poner a pi1 como salida y pin 0 como entrada
    MOVWF TRISB, ACCESS			    ; Movemos lo que está en el registro W a TRISB
    
    ;Configuración del SDO(MISO) como salida en el puerto C7
    CLRF TRISC			            ; Definimos el registro TRISC pin 7 como salida
    CLRF SSPCON1
    
    ;Configuración del Registro SSPCON1
    MOVF SSPCON1,W,ACCESS		    ; Movemos SSPCON1 al registro W
    IORLW b'10100000'			    ; Configuración de control del SPI con registro SSPBUF
    MOVWF SSPCON1,ACCESS		    ; Pasamos lo que está en el registro W a SSPCON1
    
    MOVF SSPCON1, W, ACCESS		    ; Movemos SSPCON1 al registro W
    IORLW b'00000010'			    ; Iniciamos con una frecuencia base 
    MOVWF SSPCON1, ACCESS		    ; Pasamos lo que está en el registro W a SSPCON1
    
    ;Configuración polaridad del reloj
    BCF SSPCON1,4,ACCESS		    ; El estado inactivo para el reloj es un nivel bajo
      
    ;Configuración del Registro SSPSTAT, realizar muestreo al final del tiempo de salida de datos por flanco de bajada
    BSF SSPSTAT,7,ACCESS		    ; Habilitamos muestreo por tiempo de salida de datos
    BCF SSPSTAT,6,ACCESS		    ; Habilitamos flanco de bajada
   
    ;Configuración interrupción SPI en el registro PIE
    BCF PIE1,3,ACCESS			    ; Ponemos el Registro PIE1 la deshabilitación de la interrupción de MSSP
      
    ;Prioridad de interrupción 
    CLRF IPR1				    ; Ponemos el Registro IPR1 como baja prioridad
   
    ;Definir configuración del SPI
    BCF PIR1,3,ACCESS			    ; Ponemos el Registro PIR1 en espera para transmitir o recibir
   
    ;Habilitar Configuración SPI en el pin 5
    BSF SSPCON1,5,ACCESS		    ; Habilita el puerto serie y configura SCK, SDO, SDI y SS
   
   ; RETURN				    ; Retornamos 
   
;--------------PROGRAMA PRINCIPAL----------------------

   ;------INICIA SPI-------
   CALL SPI_transfer
   CALL DELAY
   CALL DELAY
   CALL DELAY
MAIN:
    
   CALL SPI_transfer
   MOVFF save_data,save_data_1
   MOVF save_data_1,W
   MOVWF LATD
   CALL DELAY
   CALL DELAY
   CALL DELAY

   CALL SPI_transfer
   MOVFF save_data,save_data_2
   MOVF save_data_2,W
   MOVWF LATD
   CALL DELAY
   CALL DELAY
   CALL DELAY
   
   ;----INICIA PWM Y SALIDAS EN LOS PUERTO
    MOVF    save_data_1,W
    MOVWF   Duty_On	;Registra el valor para el periodo
    RRNCF   save_data_2,F	;Hace un corrimiento a la derecha aumentando 1
    RRNCF   save_data_2,W
    ANDLW   B'00110000'
    MOVWF   Duty_Off	;Se guarda la conversión más baja en Duty_Off
;Configuración PWM
    MOVLW   B'00001100'
    IORWF   Duty_Off,F	;Se obtienen los valores del Duty_Off
    MOVWF   CCP1CON	;Modo PWM para el CCP1
;Registro PR2
    MOVLW   .255
    MOVWF   PR2		;Periodo se guarda en el PR2
;Anchura del pulso
    MOVF    Duty_On,W	;Guardamos Duty_On -> W
    MOVWF   CCPR1L	;Y se determina concatenando en el registro CCPR1L
;Prescaler
    MOVLW   B'00000111'	;Prescaler 1:16 y frecuencia de 8MHz 
    MOVWF   T2CON
   
   GOTO MAIN    
   
   

SPI_transfer:
    MOVLW B'11111111'
    MOVWF data_to_send; 
    MOVFF data_to_send,SSPBUF		    ; Pasamos los datos a tranferiar al Registro SSPBUF
    
Wait_transfer:
    ;Verificar si se acompleto la transferencia
    MOVLW b'00001000'				    ; Movemos 0x08 al registro W
    CPFSEQ PIR1,0			    ; Comparamos PIR1 = 1, entonces salta
    BRA Wait_transfer			    ; Repetir hasta que la transferencia se complete
    
    ;Limpiamos bandera de la interrupci?n del SPI
    BCF PIR1,3,ACCESS			    ; Ponemos el Registro PIR1 y bajamos bandera
   
    ;Obtenemos los datos del registro y lo guardamos
    MOVFF SSPBUF,save_data		    ; Pasamos los datos del Registro SSPBUF a la direcci?n de memoria save_data
   

    RETURN
    ;GOTO SPI_transfer
    
    DELAY:
    MOVLW 0X5c
    MOVWF DCounter1
    MOVLW 0X26
    MOVWF DCounter2
    MOVLW 0X0b
    MOVWF DCounter3
LOOP:
    DECFSZ DCounter1, 1 ;decrementa 1 y salta si es cero
    GOTO LOOP
    DECFSZ DCounter2, 1
    GOTO LOOP
    DECFSZ DCounter3, 1
    GOTO LOOP
   return
  END
  
  
  Start_SPI_slave:
    ; Limpiamos puertos a utilizar
    CLRF PORTC				    ; Limpiamos puerto C
    CLRF PORTB				    ; Limpiasmo puerto B
    ;B1 SCK
    ;B0 (MOSI)
    ;C7 SDO (MISO)
    ;
    ;Configuración del B1 SCK como salida en el puerto y RB0 SDI(MOSI) como entrada en el puerto 0
    MOVLW B'00000011'				    ;  RB0 as input(SDI) RB1 as INPUT(SCK)
    MOVWF TRISB, ACCESS			    ; Movemos lo que está en el registro W a TRISB
    
    MOVLW B'00010000' ;RA5 as a INPUT(SS')   
    MOVWF TRISA, ACCESS
    ;Configuración del SDO(MISO) como salida en el puerto C7
    CLRF TRISC			            ; Definimos el registro TRISC pin 7 como salida
    CLRF SSPCON1
    ;Configuración del Registro SSPCON1
    MOVF SSPCON1,W,ACCESS		    ; Movemos SSPCON1 al registro W
    IORLW b'10100000'			    ; Configuración de control del SPI con registro SSPBUF
    MOVWF SSPCON1,ACCESS		    ; Pasamos lo que está en el registro W a SSPCON1
    
    MOVF SSPCON1, W, ACCESS		    ; Movemos SSPCON1 al registro W
    IORLW b'00000100'			    ; Iniciamos con una frecuencia base 
    MOVWF SSPCON1, ACCESS		    ; Pasamos lo que está en el registro W a SSPCON1
    
    ;Configuración polaridad del reloj
    BCF SSPCON1,4,ACCESS		    ; El estado inactivo para el reloj es un nivel bajo
      
    ;Configuración del Registro SSPSTAT, realizar muestreo al final del tiempo de salida de datos por flanco de bajada
    BSF SSPSTAT,7,ACCESS		    ; Habilitamos muestreo por tiempo de salida de datos
    BSF SSPSTAT,6,ACCESS		    ; Habilitamos flanco de bajada
   
    ;Configuración interrupción SPI en el registro PIE
    BCF PIE1,3,ACCESS			    ; Ponemos el Registro PIE1 la deshabilitación de la interrupción de MSSP
      
    ;Prioridad de interrupción 
    CLRF IPR1				    ; Ponemos el Registro IPR1 como baja prioridad
   
    ;Definir configuración del SPI
    BCF PIR1,3,ACCESS			    ; Ponemos el Registro PIR1 en espera para transmitir o recibir
   
    ;Habilitar Configuración SPI en el pin 5
    BSF SSPCON1,5,ACCESS		    ; Habilita el puerto serie y configura SCK, SDO, SDI y SS
   
    ; Disable the ADC channel which are on for multiplexed pin when used as an input     
    CLRF ADCON0;			; This is for de-multiplexed the SCL and SDI from analog pins*/
    MOVLW 0X0F
    MOVWF ADCON1,ACCESS;		; This makes all pins as digital I/O */  
   ; RETURN				    ; Retornamos 
   