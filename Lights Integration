;*********************************************************************
;* Lab 2 shell code for students                                     *
;*********************************************************************
;* Summary:                                                          *
;*                                                                   *
;* This code is designed for use with the 2016 hardware for ME305.   *
;* This code accepts two two-byte integers through the debugger and  *
;* uses these value to adjust the timing of two pairs of LEDs        *
;* connected to Port P.                                              *
;*                                                                   *
;*                                                                   * 
;* Author: Austin Conrad,Chandler Jones                              *
;*         Cal Poly University                                       *
;*         October 2019                                              *
;*                                                                   *
;* Revision History:                                                 *
;*      WRM 9/30/2019 14:17                                          *
;*      Format for Word 10/3/19 13:41                                *
;*                                                                   *
;* ToDo:                                                             *
;*      - Turn in Report                                             *
;*********************************************************************
;/-------------------------------------------------------------------\
;| Include all associated files                                      |
;\-------------------------------------------------------------------/
; The following are external files to be included during assembly
;/-------------------------------------------------------------------\
;| External Definitions                                              |
;\-------------------------------------------------------------------/
; All labels that are referenced by the linker need an external
; definition
     
     XDEF main

;/-------------------------------------------------------------------\
;| External References                                               |
;\-------------------------------------------------------------------/
; All labels from other files must have an external reference

     XREF 	ENABLE_MOTOR , DISABLE_MOTOR
     XREF 	STARTUP_MOTOR , UPDATE_MOTOR , CURRENT_MOTOR
     XREF 	STARTUP_PWM , STARTUP_ATD0 , STARTUP_ATD1
     XREF 	OUTDACA , OUTDACB
     XREF 	STARTUP_ENCODER , READ_ENCODER
     XREF 	INITLCD , SETADDR , GETADDR , CURSOR_ON , CURSOR_OFF , DISP_OFF
     XREF 	OUTCHAR , OUTCHAR_AT , OUTSTRING , OUTSTRING_AT
     XREF 	INITKEY , LKEY_FLG , GETCHAR
     XREF 	LCDTEMPLATE , UPDATELCD_L1 , UPDATELCD_L2
     XREF 	LVREF_BUF , LVACT_BUF, LERR_BUF , LEFF_BUF , LKP_BUF, LKI_BUF
     XREF 	Entry , ISR_KEYPAD

;/-------------------------------------------------------------------\
;| Assembler Equates                                                 |
;\-------------------------------------------------------------------/
; Constant values can be equated here

PORTP            EQU $0258      ; Output port for LEDs
DDRP             EQU $025A
DDRT             EQU $0242
G_LED_1          EQU %00001000  ; Green LED output pin for LED pair_1
R_LED_1          EQU %00010000  ; Red LED output pin for LED pair_1
LED_MSK_1      EQU %00011000    ; LED pair_1 (sum of G_LED_1 & R_LED_1)
G_LED_2        EQU %00100000    ; Green LED output pin for LED pair_2
R_LED_2        EQU %01000000 	   ; Red LED output pin for LED pair_2
LED_MSK_2      EQU %01100000    ; LED pair_2 (sum of G_LED_2 & R_LED_2)

;/--------------------------------------------------------------------\
;| Variables in RAM                                                   |
;\--------------------------------------------------------------------/
; The following variables are located in unpaged RAM

DEFAULT_RAM: SECTION

;Label      Type DS     #ofType		
t1state      DS.B         1     ; First State Init
t2state      DS.B         1     ; Second State Init
t3state      DS.B         1     ; Third State Init
t4state      DS.B         1     ; Fourth State Init
t5state      DS.B         1     ; Fifth State Init
DONE_1       DS.B         1     ; Intermediate Variable for LED's_1
DONE_2       DS.B         1     ; Intermediate Variable for LED's_2
TICKS_1      DS.W         1     ; Value of the delay for the First set of LED's
TICKS_2      DS.W         1     ; Value of the Delay for the second set of LED's
COUNT_1      DS.W         1     ; COUNTER for the first LED pair
COUNT_2      DS.W         1     ; COUNTER for the second LED pair

;/-------------------------------------------------------------------\
;| Main Program Code                                                 |
;\-------------------------------------------------------------------/
; This code uses cooperative multitasking for Lab 2 from ME 305

MyCode: 	SECTION

main:  

     clr     t1state            ; Initialize all tasks to state0
     clr     t2state
     clr     t3state
     clr     t4state
     clr     t5state

; Normally no code other than that to clear the state variables and call the tasks
; repeatedly should be in your main program. However, in this lab we will make a
; one-time exception. The following code will set TICKS_1 and TICKS_2 to default values
; and the BGND will allow the user to change these values in the debugger.

     movw     #100, TICKS_1     ; Set default for TICKS_1
     movw     #200, TICKS_2     ; Set default for TICKS_2
     bgnd                       ; Stop in DEBUGGER to allow user to alter TICKS

Top:

     jsr     TASK_1             ; LED_1 Pattern
     jsr     TASK_2             ; Countdown_1
     jsr     TASK_3             ; Delay Task
     jsr     TASK_4             ; LED_1 Pattern
     jsr     TASK_5             ; Countdown_2
     bra     Top




;-------------TASK_1 First LED Pair -----------------------------------

TASK_1:
     ldaa     t1state          ; loading the trigger variable into accumulator A
     beq      t1state0         ; Compare accumulator A to zero (counter is done)
     deca
     beq      t1state1         ; Compare accumulator A to zero (counter is done)
     deca
     beq      t1state2         ; Compare accumulator A to zero (counter is done)
     deca
     beq      t1state3         ; Compare accumulator A to zero (counter is done)
     deca
     beq      t1state4         ; Compare accumulator A to zero (counter is done)
     deca
     beq      t1state5         ; Compare accumulator A to zero (counter is done)
     deca
     beq      t1state6         ; Compare accumulator A to zero (counter is done)
        
t1state0:
     clr      DDRT                    ; Set PORTT to input
     bclr     PORTP,LED_MSK_1         ; Clearing the LED
     bset     DDRP,LED_MSK_1          ; Setting LED_1 to output Pins
     movb     #$01,t1state            ; Setting t1state to 1
     rts
  
t1state1:

     bclr     PORTP,LED_MSK_1          ; Clearing the LED
     tst      DONE_1                   ; Testing DONE_1
     beq      t1s1Exit                 ; Set to the next state              
     ldaa     t1state                  ; Load Accumulator A with value of t1state
     movb     #$02 , t1state           ; Setting t1state to 2

t1s1Exit:
     rts   

t1state2:
     bset     PORTP,G_LED_1            ; Setting Green LED_1
     tst      DONE_1                   ; Testing DONE_1
     beq      t1s2Exit                 ; Set to the next state              
     ldaa     t1state                  ; Load Accumulator A with value of t1state
     movb     #$03,t1state             ; Setting t1state to 3

t1s2Exit:
    rts	

t1state3:
     bclr     PORTP,LED_MSK_1          ; Clearing the LED
     tst      DONE_1                   ; Testing DONE_1
     beq      t1s3Exit                 ; Set to the next state              
     ldaa     t1state                  ; Load Accumulator A with value of t1state
     movb     #$04,t1state             ; Setting t1state to 4

t1s3Exit:
     rts

t1state4:
  
     bset     PORTP,R_LED_1            ; Setting Red LED_1
     tst      DONE_1                   ; Testing DONE_1
     beq      t1s4Exit                 ; Set to the next state              
     ldaa     t1state
     movb     #$05,t1state             ; Setting t1state to 5

t1s4Exit:
     rts

t1state5:

     bclr     PORTP,LED_MSK_1          ; Clearing the LED
     tst      DONE_1                   ; Testing DONE_1
     beq      t1s5Exit                 ; Set to the next state              
     ldaa     t1state
     movb     #$06,t1state             ; Setting t1state to 6

t1s5Exit:
     rts  
  
t1state6:
  
     bset     PORTP,LED_MSK_1          ; Setting Both R&G LED's 1
     tst      DONE_1                   ; Testing DONE_1
     beq      t1s6Exit                 ; Set to the next state              
     ldaa     t1state
     movb     #$01,t1state             ; Setting t1state to 1

t1s6Exit:
     rts                               ; Exit TASK_1

;-------------TASK_2 Countdown_1 --------------------------------------
TASK_2:
     ldaa     t2state                  ; Get current t2state and brance accordingly
     beq      t2state0
     deca
     beq      t2state1
     rts
	
t2state0:                              ; Initialization for Task_2
     clr      DONE_1                   ; Set DONE_1 back to 0
     movw     TICKS_1,COUNT_1          ; Initialize COUNT_1 to Ticks_1
     movb     #$01,t2state             ; Setting t2state to 1 (setting next state)
     rts
	      
t2state1:                              ; Re initialize COUNT_1
     tst      DONE_1                   ; check for need to reinitialize 
     beq      t2s1a                    ; no need to reinitialize
     movw     TICKS_1,COUNT_1          ; reinitialize COUNT_1 to TICKS_1
     clr      DONE_1                   ; clear DONE_1 after reinitialization
         
        
t2s1a:                                 ; decrement COUNT_1
     decw     COUNT_1                  ; if COUNT_1 is not zero, simply return
     bne      exit_t2s1                ; if COUNT_1 is zero, set DONE_1 and return
     movb     #$01, DONE_1

exit_t2s1:
     rts                               ; exit TASK_2
	



	
;-------------TASK_3 Delay 1ms----------------------------------------------

TASK_3: 
	
     ldaa     t3state                  ; Get current t3state and branch accordingly
     beq      t3state0		
     deca                              ; Decrement t3state by 1, store in t3state
     beq      t3state1                 ; Simple branch
     rts                               ; Undefined state - do nothing but return


t3state0:                              ; Initialization for TASK_3
                                       ; No initialization required

     movb     #$01, t3state 	; set next state
     rts

t3state1:
     jsr      DELAY_1ms
     rts                               ; exit TASK_3
	
;-------------TASK_4 First LED Pair ----------------------------------------
TASK_4:
     ldaa     t4state      ; loading the trigger variable into accumulator A
     beq      t4state0     ; Compare accumulator A to zero (counter is done)
     deca
     beq      t4state1     ; Compare accumulator A to zero (counter is done)
     deca
     beq      t4state2     ; Compare accumulator A to zero (counter is done)
     deca
     beq      t4state3     ; Compare accumulator A to zero (counter is done)
     deca
     beq      t4state4     ; Compare accumulator A to zero (counter is done)
     deca
     beq      t4state5     ; Compare accumulator A to zero (counter is done)
     deca
     beq      t4state6     ; Compare accumulator A to zero (counter is done)
        
t4state0:
     clr      DDRT                        ;set PORTT to input
     bclr     PORTP,LED_MSK_2             ;Clearing the LED
     bset     DDRP,LED_MSK_2              ;Setting LED_1 to output Pins
     movb     #$01,t4state                ;Setting t1state to 1
     rts
  
t4state1:

     bclr     PORTP,LED_MSK_2             ;Clearing the LED
     tst      DONE_2                      ;Testing DONE_1
     beq      t4s1Exit                    ;Set to the next state              
     ldaa     t4state
     movb     #$02,t4state                ;Setting t1state to 2

t4s1Exit:
     rts   

t4state2:
     bset     PORTP,G_LED_2               ;Setting Green LED_1
     tst      DONE_2                      ;Testing DONE_1
     beq      t4s2Exit                    ;Set to the next state              
     ldaa     t4state
     movb     #$03,t4state               ;Setting t1state to 3

t4s2Exit:
     rts	

t4state3:
     bclr     PORTP,LED_MSK_2            ;Clearing the LED
     tst      DONE_2                     ;Testing DONE_1
     beq      t4s3Exit                   ;Set to the next state              
     ldaa     t4state
     movb     #$04,t4state               ;setting t1state to 4

t4s3Exit:
     rts

t4state4:
  
     bset     PORTP,R_LED_2              ;Setting Red LED_1
     tst      DONE_2                     ;Testing DONE_1
     beq      t4s4Exit                   ;Set to the next state              
     ldaa     t4state
     movb     #$05,t4state               ;setting t1state to 5

t4s4Exit:
     rts

t4state5:

     bclr     PORTP,LED_MSK_2            ;Clearing the LED
     tst      DONE_2                     ;Testing DONE_1
     beq      t4s5Exit                   ;Set to the next state              
     ldaa     t4state
     movb     #$06,t4state               ;setting t1state to 6

t4s5Exit:
    rts  
  
t4state6:
  
     bset     PORTP,LED_MSK_2            ;Setting Both R&G LED's 1
     tst      DONE_2                     ;Testing DONE_1
     beq      t4s6Exit                   ;Set to the next state              
     ldaa     t4state
     movb     #$01,t4state               ;setting t1state to 1

t4s6Exit:
     rts                                 ; exit TASK_4

;-------------TASK_5 Countdown_2 ----------------------------------------------
TASK_5:
     ldaa     t5state                    ;get current t2state and brance accordingly
     beq      t5state0
     deca
     beq      t5state1
     rts
	
t5state0:                                ; Initialization for Task_2
     clr      DONE_2                     ; 
     movw     TICKS_2,COUNT_2            ; initialize COUNT_1 to Ticks_1
     movb     #$01,t5state               ; Setting t2state to 1 (setting next state)
     rts
	      
t5state1:                              ; Re initialize COUNT_1
     tst     DONE_2                    ; check for need to reinitialize 
     beq     t5s1a                     ; no need to reinitialize
     movw    TICKS_2,COUNT_2           ; reinitialize COUNT_1 to TICKS_1
     clr     DONE_2                    ; clear DONE_1 after reinitialization
        
        
t5s1a:                                 ; decrement COUNT_1
     decw    COUNT_2                   ; if COUNT_1 is not zero, simply return
     bne     exit_t5s1                 ; if COUNT_1 is zero, set DONE_1 and return
     movb    #$01, DONE_2

exit_t5s1:
     rts                               ; exit TASK_5

	

;/-----------------------------------------------------------------------\
;| Subroutines                                                            |
;/-----------------------------------------------------------------------/
; Add subroutines here: 

DELAY_1ms:                             ; Don’t fuck with this!!!
     ldy     #$0584                    ; Load Index Register Y

INNER:                                 ; inside loop
     cpy     #0		          ; Compare index register y
     beq     EXIT                      ; Simple branch
     dey                               ; Decrement Index Register Y
     bra     INNER                     ; Keeps looping through INNER

EXIT:
     rts                               ; exit DELAY_1ms

;/-----------------------------------------------------------------------\
;| Messages                                                              |
;/-----------------------------------------------------------------------/
; Add ASCII messages here:
;/-----------------------------------------------------------------------\
;| Vectors                                                               |
;\------------------------------------- ---------------------------------/
; Add interrupt and reset vectors here:
     ORG      $FFFE                   ; reset vector address
     DC.W     Entry
     ORG      $FFCE                   ; Key Wakeup interrupt vector address [Port J]
     DC.W     ISR_KEYPAD
