;****************************************************************************
;* LAB 3                                                                    *
;****************************************************************************
;* Summary:                                                                 *
;*   This program allows the user to input values to a keypad. These values *
;*   represent the period in milliseconds which will blink LED lights if set*
;*   up properly. To set the first pair, click F1, or the top right button, *
;*   followed by a value (Only magnitudes from 1 to 65535 will be accepted),*  
;*   and finish setting by pressing enter, or the bottom right button.      *
;*   To set the second sequence, press F2, or the second button from the top*
;*   right, and follow the same sequence.                                   *
;*                                                                          *
;*   Author: Chandler Jones                                                 *
;*   Cal Poly University                                                    *
;*   Fall 2018                                                              *
;****************************************************************************

;/--------------------------------------------------------------------------\
;| Include all associated files                                             |
;\--------------------------------------------------------------------------/
; The following are external files to be included during assembly

;/--------------------------------------------------------------------------\
;| External Definitions                                                     |
;\--------------------------------------------------------------------------/
; All labels that are referenced by the linker need an external definition

              XDEF  main


;/--------------------------------------------------------------------------\
;| External References                                                      |
;\--------------------------------------------------------------------------/
; All labels from other files must have an external reference

             XREF  ENABLE_MOTOR, DISABLE_MOTOR
             XREF  STARTUP_MOTOR, UPDATE_MOTOR, CURRENT_MOTOR
             XREF  STARTUP_PWM, STARTUP_ATD0, STARTUP_ATD1
             XREF  OUTDACA, OUTDACB
             XREF  STARTUP_ENCODER, READ_ENCODER
             XREF  INITLCD, SETADDR, GETADDR, CURSOR_ON, CURSOR_OFF, DISP_OFF
             XREF  OUTCHAR, OUTCHAR_AT, OUTSTRING, OUTSTRING_AT
             XREF  INITKEY, LKEY_FLG, GETCHAR
             XREF  LCDTEMPLATE, UPDATELCD_L1, UPDATELCD_L2
             XREF  LVREF_BUF, LVACT_BUF, LERR_BUF,LEFF_BUF, LKP_BUF, LKI_BUF
             XREF  Entry, ISR_KEYPAD
            
;/--------------------------------------------------------------------------\
;| Assembler Equates                                                        |
;\--------------------------------------------------------------------------/
; Constant values can be equated here
PORTP:           EQU   $0258         ; Output port for LEDs
DDRP:            EQU   $025A         ; Direct Data Registry Port P           
DDRT:            EQU   $0242         ; Direct Data Registry Port P                                       
G_LED_1:         EQU   %00001000     ; green LED output pin for LED pair_1
R_LED_1:         EQU   %00010000     ; red LED output pin for LED pair_1
LED_MSK_1:       EQU   %00011000     ; LED output pins for pair_1
G_LED_2:         EQU   %00100000     ; green LED output pin for LED pair_2
R_LED_2:         EQU   %01000000     ; red LED output pin for LED pair_2
LED_MSK_2:       EQU   %01100000     ; LED output pins for pair_2
LCD_address1:       EQU   $00     ; LCD top, first character
LCD_address2:       EQU   $40     ; LCD bottom, first character
LCD_address3:       EQU   $08     ; LCD top, first AVAILABLE digits.
LCD_address4:       EQU   $48     ; LCD bottom, first AVAILABLE digits
LCD_address5:       EQU   $0E     ; LCD first for F1.
LCD_address6:       EQU   $4E     ; LCD first for F2.



;/--------------------------------------------------------------------------\
;| Variables in RAM                                                         |
;\--------------------------------------------------------------------------/
; The following variables are located in unpaged ram

DEFAULT_RAM:  SECTION

; LABEL      TYPE / #ofType
COUNT         DS.B 1              ; # of digits entered 
POINTER       DS.W 1              ; Points to the next available memory
                                  ; location in buffer
BUFFER        DS.B 5              ; Holds entered ASCII digits to be
                                  ; converted to decimal
DPTR:         DS.W 1              ; Contains address of next byte to be read
                                  ; and written for display   
time_1_display:      DS.B 1       ; sets ‘Time 1 =’
time_2_display:      DS.B 1       ; sets ‘Time 2 =’
F1_Display:   DS.B 1              ; '<F1> TO UPDATE LED1 PERIOD'
F2_Display:   DS.B 1              ; ‘<F2> TO UPDATE LED2 PERIOD'
DECHO:        DS.B 1              ; Flag communicating accepted digit
ERROR_1:      DS.B 1              ; ‘Magnitude too large’
ERROR_2:      DS.B 1              ; ‘zero magnitude input not valid’
ERROR_3:      DS.B 1              ; ‘no digits entered’
DBS:          DS.B 1              ; flag for backspace
FIRSTCHAR:    DS.B 1              ; First Char to be displayed flag
KEY_FLG:      DS.B 1              ; flag for key pressed
KEY_BUFF:     DS.B 1              ; Buffer that stores most recent key press
t1state:      DS.B 1              ; State Variables for tasks
t2state:      DS.B 1              
t3state:      DS.B 1              
t4state:      DS.B 1              
t5state:      DS.B 1              
t6state:      DS.B 1              
t7state:      DS.B 1              
t8state:      DS.B 1              
F1:           DS.B 1              ; Flag for F1
F2:           DS.B 1              ; Flag for F2   
ON_1:         DS.B 1              ; Flag for LED pair_1 to be on
ON_2:         DS.B 1              ; Flag for LED pair_2 to be on
DONE_1        DS.B 1              ; Intermediate Variable for LED's_1
DONE_2        DS.B 1              ; Intermediate Variable for LED's_2
TICKS_1       DS.W 1              ; Value of the delay for the First set of
                                  ; LED's
TICKS_2       DS.W 1              ; Value of the delay for the First set of
                                  ; LED's
COUNT_1       DS.W 1              ; time remaining in cycle for LED pair_1
COUNT_2       DS.W 1              ; time remaining in cycle for LED pair_2
D_SBLANK:     DS.B 1              ; Flag for short blank message
D_LBLANK      DS.B 1              ; Flag for long blank message
WAIT          DS.W 1              ; Time remaining on displayed error
                                  ; message
HIDE_CURS     DS.B 1              ; Flag to hide cursor

;/--------------------------------------------------------------------------\
;|  Main Program Code                                                       |
;\--------------------------------------------------------------------------/
MyCode:       SECTION
       
main:  
        clr t1state              ; Initialize all tasks to state0
        clr t2state
        clr t3state
        clr t4state
        clr t5state
        clr t6state
        clr t7state
        clr t8state 
        
loop:   jsr   TASK_1             ; Mastermind
        jsr   TASK_2             ; Display
        jsr   TASK_3             ; Keypad
        jsr   TASK_4             ; LED pair_1 Pattern
        jsr   TASK_5             ; Countdown_1
        jsr   TASK_6             ; LED pair_2 Pattern
        jsr   TASK_7             ; Countdown_2
        jsr   TASK_8             ; Delay 1ms
        bra   loop

;/--------------------------------------------------------------------------\
;|  TASK_1: MASTERMIND                                                      |
;\--------------------------------------------------------------------------/


TASK_1:


        ldaa   t1state              ; Get current t1state and branch 
                                    ; accordingly
        lbeq   t1state0             ; init.
        deca
        lbeq   t1state1             ; Wait for Display.
        deca
        lbeq   t1state2             ; Hub.
        deca
        lbeq   t1state3             ; Digit.
        deca
        lbeq   t1state4             ; Enter
        deca
        lbeq   t1state5	       ; Backspace.
        deca
        lbeq   t1state6             ; F1.
        deca
        lbeq   t1state7             ; F2.
        deca        
        lbeq   t1state8             ; Errors
        rts                         ; Undefined state - do nothing but return        
t1state0:                           ; initialization task1
        clr KEY_FLG
        clr DECHO
        clr ERROR_1
        clr ERROR_2
        clr ERROR_3
        clr F1
        clr F2
        clr D_SBLANK
        clr D_LBLANK
        clr COUNT
        clr DBS
        clr ON_1
        clr ON_2
        clr HIDE_CURS
        movw  #BUFFER, POINTER
        movb  #01,t1state
        movb  #01, time_1_display
        movb  #01,time_2_display
        movb  #01,F1_Display
        movb  #01,F2_Display
        movb  #01,FIRSTCHAR
        movw  #2000, WAIT
        rts                        ; Undefined state - do nothing but return

        
t1state1:                          ; Waits for Display.
        TST time_1_display         ; Test if time_1 has been displayed
        bne NOT_YET                ; branch if time 1 is not zero
        TST time_2_display         ; tests if time 2 has been displayed
        bne NOT_YET                ; branch if time 2 is not zero
        TST D_SBLANK               ; tests if short blank has been displayed
        BNE NOT_YET                ; branch if D_SBLANK is not zero
        clr F1
        clr F2
        TST F1_Display             ; tests if f1 has been displayed
        bne NOT_YET                ; branch if F1_display is not zero
        TST F2_Display             ; tests if f2 has been displayed
        bne NOT_YET
        movb #01,HIDE_CURS         ; Hides cursor once all messages displayed
        movb #02, t1state          ; sets state 1 to state 2
        rts       
NOT_YET:
        rts                        
        
t1state2:                          ; Hub of Mastermind.
        TST KEY_FLG                ; Checks key and branches
        lbeq t1s2exit
        ldab KEY_BUFF
        cmpb #$F1                  ; From Lab 3 handout
        lbeq FLAG_F1
        cmpb #$F2                  ; From Lab 3 handout
        lbeq FLAG_F2
        cmpb #$0A                  ; From Lab 3 handout
        lbeq FLAG_ENTER
        cmpb #$08                  ; From Lab 3 handout
        lbeq FLAG_BS 
        cmpb #$40                  ; From ASCII Table
        lbge t1s2exit                 

FLAG_DIGIT:                       ; Digit key pressed
        movb #03,t1state          ; sets t1state5
        bra t1s2exit

FLAG_ENTER:                       ; Enter key pressed
        movb #04,t1state          ; sets t1state7
        bra t1s2exit
FLAG_BS:                          ; Backspace key pressed
        movb #05,t1state          ; sets t1state5
        bra t1s2exit
FLAG_F1:                          ; F1 key pressed
        movb #06,t1state          ; sets t1state6
        bra t1s2exit
FLAG_F2:                          ; F2 key pressed
        movb #07,t1state          ; sets t1state4
        bra t1s2exit               
t1s2exit:
        clr KEY_FLG
        rts

t1state3:                         ; Digit
        TST F1                    
        bne t1state3a             ; Requires F1 or F2 to be pressed
        TST F2
        bne t1state3a             ; ^^
        movb #02, t1state         ; Back to Hub
        rts

t1state3a:
        ldab COUNT                
        cmpb #$05                 ; can’t have more than 65536 (5dig)
        beq t1state3back
        ldab KEY_BUFF             ; Puts Key buff into buffer
        ldx POINTER               ; display pointer
        stab 0,x                  ; loads buff to memory
        inc COUNT                 ; getting closer to 5 digits
        incw POINTER              ; tells display to move 1 forward
        movb #01, DECHO           ; flag says we know digit is stored
t1state3back:        
        movb #02, t1state         ; Back to Hub
        rts
        
t1state4:                         ; deals with enter 
        TST F1                    ; Exits 
        bne t1state4a
        TST F2
        bne t1state4a
        movb #02, t1state         ; back to hub t1state2
        rts       
t1state4a:
        TST COUNT
        beq ERRORMSG3           ; no digits entered (if 0)
        jsr convertASCII          ; converts ASCII
        rts                               
convertASCII:                     ; conversion subroutine from Homework
                                  ; Ref: Sammy Tran for help with this
        ldd    #$0000             ; loads D with starting result of 0
        pshD                      ; pushes that D
        ldx    #BUFFER            ; Gets X Buff

conversion:   
        ldy #$000A                ; load register Y with Hex $10 
        pulD                      ; pulls contents of D from stack
        EMUL                      ; 16 by 16 bit multiplication D*Y = Y:D
        TSTY                      ; checks register Y for mag too large tasty
        BNE ERRORMSG1            ; values in the upper register, too large 
        pshD                      ; pushes product to d
        clrA                      ; clears A should be 0 anyway
        ldab 0,x                  ; loads next digit from buffer
        subb #$30                 ; subtract hex $30 to produce hex digit
        ADDD 0,SP                 ; adds that digit
        pulY                      ; pulls the multiply 10
        bcs ERRORMSG1            ; Mag too large error
        pshD                      ; pushes addition result from d to stack
        inx                       ; inc to next slot of buffer for next digit
        dec COUNT                 ; keeps track of digits remaining
        bne conversion            ; loops back until count = 0
TestMag:                         ; tests for zero input
        pulx                      ; pulls result from stack into register X
        TSTX                      ; test for v = 1
        BEQ ERRORMSG2           ; zero mag input
        pshx                      ; back to the stack!
        TST F1                    ; Which Sequence
        BNE F1GO
        movw SP,TICKS_2           ; new sequence_2 period
        pulx                      ; pulls new value entered
        clr F2                    ; Flag cleared
        movb #01, ON_2            ; Turn it back on!
        bra t1state4back          ; exit convert
F1GO:
        movw SP,TICKS_1           ; new Sequence_1 period
        pulx                      ; pulls new value
        clr F1                    ; clears the flag
        movb #01, ON_1            ; Turn it back on!
        bra t1state4back
ERRORMSG1:                      ; Magnitude too large error 
        movb #01, ERROR_1 
        bra t1state4error                   
ERRORMSG2:                      ; Zero Magnitude error
        movb #01, ERROR_2
        bra t1state4error                
ERRORMSG3:                      ; No digits error
        movb #01, ERROR_3
        bra t1state4error        
t1state4error:                    ; Error Display handler
        movb #01, D_LBLANK
        movb #08, t1state         ; sets next state
        clr  COUNT                ; resets count for next entry
        movw #BUFFER, POINTER     ; reinitializes pointer
        rts                             
t1state4back:
        movw #BUFFER, POINTER
        movb #02, t1state         ; back to hub
        movb #01, HIDE_CURS
        rts
t1state5:                         ; Deals with backspace.
        TST COUNT                 ; Exits if no digits have been displayed
        beq t1state5back
        movb #01, DBS             ; Lets display know Backspace has been
                                  ; pressed
        dec COUNT                 ; Decs count & pointer
        decw POINTER 
t1state5back:
        movb #02, t1state         ; back to hub
        rts
t1state6:                         ; SETS F1 AND TURNS OFF LED_1
        TST F1                    ; Exits state if F1 has been pressed
        bne t1state6back
        TST F2
        bne t1state6back          ; Exits state if F2 has been pressed
        movb #01, F1              ; Sets f1 flag
        movb #01, D_SBLANK        ; Clears existing digits in top row
        clr  ON_1                 ; Turns off LED_1 
t1state6back:
        movb #02, t1state         ; Sets next state
        rts
t1state7:                         ; Deals with f2
        TST F1                    ; Exits state if f1 or f2 has been pressed
        bne t1state7back
        TST F2
        bne t1state7back
        movb #01, F2              ; Sets f2 flag, digits should be displayed
                                  ;in bottom row
        movb #01, D_SBLANK        ; Clears existing digits in bottom row
        clr  ON_2                 ; Turns off LED2 pair
t1state7back:
        movb #02, t1state         ; back to hub
        rts
        
t1state8:                         ; Waiting for error messages
        TST D_LBLANK              ; Tests each error message flag
        BNE NOT_YET_2
        TST ERROR_1
        BNE NOT_YET_2
        TST ERROR_2
        BNE NOT_YET_2
        TST ERROR_3
        BNE NOT_YET_2
        movb #01, HIDE_CURS       ; Hide the cursor
        decw WAIT                 ; starts wait from 2000
        TSTW WAIT
        BEQ DisplayAgain          ; Jump when timer is done
        rts
NOT_YET_2:
        rts
DisplayAgain:                     ; Displays menu message after wait
        TST F1
        BNE ResetF1               ; Resets F1 if F1 is 1
        movb #01, F2_Display      ; Send Flag for F2 to Display
        bra DONE
ResetF1:
        movb #01, F1_Display      ; Send Flag for F1 to Display
DONE:
        movb #01, D_SBLANK
        movb #01, t1state
        movw #2000, WAIT          ; Reload error message timer 
        rts
       

 
;/--------------------------------------------------------------------------\
;|  TASK_2 DISPLAY:                                                         |
;\--------------------------------------------------------------------------/

TASK_2:                            ; Display feng shui
        ldaa   t2state             ; Get current t2state and branch 
                                   ; accordingly
        lbeq   t2state0
        deca
        lbeq   t2state1
        deca
        lbeq   t2state2
        deca
        lbeq   t2state3
        deca
        lbeq   t2state4
        deca
        lbeq   t2state5
        deca
        lbeq   t2state6
        deca
        lbeq   t2state7
        deca        
        lbeq   t2state8
        deca 
        lbeq   t2state9
        deca 
        lbeq   t2state10
        deca
        lbeq   t2state11
        deca
        lbeq   t2state12
        deca
        lbeq   t2state13
        rts                        
        
t2state0:                          ; Initialization
        jsr INITLCD                ; Turn on LCD
        jsr CURSOR_ON              ; Turn on Cursor
        movb #01, t2state          ; sets t2state0 to t2state1
        rts
                                 
t2state1:                          ; Checks flags & branches
        TST  time_1_display               
        BNE  flag_time_1
        TST  time_2_display
        BNE  flag_time_2        
        TST  F1_Display
        BNE  FLAG_F1_DISPLAY
        TST  F2_Display
        BNE  FLAG_F2_DISPLAY
        TST  D_SBLANK
        BNE  FLAG_SBLANK
        TST  D_LBLANK                
        BNE  FLAG_LBLANK             
        TST  ERROR_1
        BNE  FLAG_ERROR_1
        TST  ERROR_2
        BNE  FLAG_ERROR_2
        TST  ERROR_3
        BNE  FLAG_ERROR_3
        TST  DECHO
        BNE  FLAG_ECHO
        TST  DBS
        BNE  FLAG_BACKSPACE
        TST  HIDE_CURS
        BNE  FLAG_HIDE_CURS
        rts                       
flag_time_1:                       ; displays time 1 screen
        movb #02, t2state
        rts
flag_time_2:                       ;displays time 2 screen
        movb #03, t2state          
        rts       
FLAG_F1_DISPLAY:                   ; Shows display for F1 only
        movb #04, t2state
        rts
FLAG_F2_DISPLAY:                   ; Shows display for F2 only.
        movb #05, t2state
        rts
FLAG_ERROR_1:                      ; Displays error 1.
        movb #06, t2state
        rts
FLAG_ERROR_2:                      ; Displays error 2.
        movb #07, t2state
        rts
FLAG_ERROR_3:                      ; Displays Error 3.
        movb #08, t2state
        rts  
FLAG_ECHO:                         ; Moves cursor forward.
        movb #09, t2state
        rts
FLAG_SBLANK:                       ; short blank
        movb #10, t2state
        rts        
FLAG_BACKSPACE:                    ; backspace
        movb #11, t2state
        rts
FLAG_LBLANK:                       ; long blank
        movb #12, t2state
        rts
FLAG_HIDE_CURS:                    ; hides cursor
        movb #13, t2state
        rts
        
t2state2:                         ; for time 1
        ldaa FIRSTCHAR              
        cmpa #01                  
        bne  t2state2a            ; branch if z flag = 0
        ldaa  #LCD_address1       ; Loads LCD_address1
        ldx   #TIME_1             ; Loads Message to be Displayed
        jsr PUTCHAR_1st           ; Will print first character 
        bra t2state2low
t2state2a:
        jsr PUTCHAR
t2state2low:                      ; Tests if full message is printed
        TST FIRSTCHAR
        beq t2state2exit
        clr time_1_display
        movb #01,t2state          ; back to state1
        movb #01,FIRSTCHAR        ; Resets FIRSTCHAR flag
t2state2exit:
        rts                       

t2state3:                         ; Display Time 2 message
        ldaa FIRSTCHAR
        cmpa #01                  ; Tests first character
        bne  t2state3a
        ldaa #LCD_address2        ; Loads bottom first char
        ldx  #TIME_2              ; Loads Message to be Displayed
        jsr PUTCHAR_1st           ; print first character
        bra t2state3low
t2state3a:
        jsr PUTCHAR
t2state3low:                      ; tests for full print
        TST FIRSTCHAR
        beq t2state3exit
        clr time_2_display
        movb #01,t2state          ; back to state 1
        movb #01,FIRSTCHAR        ; sets first char flag
t2state3exit:
        rts

t2state4:                         ; Shows F1
        ldaa FIRSTCHAR
        cmpa #01                  ; Tests First Char
        bne  t2state4a
        ldaa  #LCD_address5       ; Load first char for f1
        ldx   #F_1                ; Loads F1 Message
        jsr PUTCHAR_1st           ; Print first character
        bra t2state4exit
t2state4a:
        jsr PUTCHAR
        
t2state4low:                      ; checks for full print
        TST FIRSTCHAR
        beq t2state4exit          ; exit
        clr F1_Display
        movb #01,t2state          ; back to state 1
        movb #01,FIRSTCHAR        ; sets first char flag
t2state4exit:        
        rts

t2state5:                         ; Shows F2 
        ldaa FIRSTCHAR
        cmpa #01                  ; Tests First char
        bne  t2state5a            ; exit
        ldaa  #LCD_address6       ; Load first char for f2
        ldx   #F_2                ; Loads F2
        jsr PUTCHAR_1st           ; print first character
        bra t2state5low
t2state5a:
        jsr PUTCHAR
t2state5low:                      ; checks for full print
        TST FIRSTCHAR
        beq t2state5exit          ; exits
        clr F2_Display            
        movb #01,t2state          ; back to state 1
        movb #01,FIRSTCHAR        ; set first char flag
t2state5exit:        
        rts                               

t2state6:                         ; Zero Mag Error
        ldaa FIRSTCHAR
        cmpa #01                  ; checks for first char
        bne t2state6a             ; to putchar
        TST F1                    ; Top or bottom?
        bne TOP_ROW_6             ; load error 1 on top
        ldaa #LCD_address4        ; Loads bottom first available
        ldx  #ERROR_MSG_1             ; Loads Error 1 message
        jsr PUTCHAR_1st           ; print first char
        bra t2state6low
TOP_ROW_6:
        ldaa #LCD_address3        ; load top first available
        ldx #ERROR_MSG_1              ; Load error 1 message zero mag
        jsr PUTCHAR_1st           ; print first char
        bra t2state6low
t2state6a:
        jsr PUTCHAR
t2state6low:                      ; Tests for full print
        TST FIRSTCHAR
        beq t2state6exit          ; exit
        clr ERROR_1               ; clear error flag
        movb #01, t2state         ; back to state 1
        movb #01, FIRSTCHAR       ; Resets FIRSTCHAR flag
t2state6exit:
        rts


t2state7:                         ; Magnitude Too Large error
        ldaa FIRSTCHAR
        cmpa #01                  ; checks if first char
        bne t2state7a             ; to putchar
        TST F1                    ; Top or bottom?
        bne TOP_ROW_7             ; error on top
        ldaa #LCD_address4        ; Load bottom row first available
        ldx  #ERROR_MSG_2             ; Loads error 2 mag too large on bottom
        jsr PUTCHAR_1st           ; print first char
        bra t2state7low           ; test full print
TOP_ROW_7:                        ; error to top row
        ldaa #LCD_address3        ; Loads top row first available
        ldx #ERROR_MSG_2              ; load error 2 mag too large
        jsr PUTCHAR_1st           ; print first char
        bra t2state7low           ; test full print
t2state7a:
        jsr PUTCHAR               ; add next char
t2state7low:                      ; tests for full print
        TST FIRSTCHAR
        beq t2state7exit          ; exits
        clr ERROR_2               ; clears error flg
        movb #01, t2state         ; back to state 1
        movb #01, FIRSTCHAR       ; sets firstchar flag
t2state7exit:
        rts
        
t2state8:                         ; No Digits error
        ldaa FIRSTCHAR
        cmpa #01                  ; check if first char needs to be printed
        bne t2state8a             ; to putchar
        TST F1                    ; Top or bottom?
        bne TOP_ROW_8            ; Top
        ldaa #LCD_address4       ; Loads bottom first slot
        ldx  #ERROR_MSG_3            ; Loads error 3 no digits
        jsr PUTCHAR_1st          ; print first char
        bra t2state8low          ; test for full print
TOP_ROW_8:
        ldaa #LCD_address3        ; Loads address top first available
        ldx #ERROR_MSG_3              ; loads error 3 no digits
        jsr PUTCHAR_1st           ; print first char
        bra t2state8low           ; tests for full print
t2state8a:
        jsr PUTCHAR               ; throw on the next character
t2state8low:                      ; Tests for full print
        TST FIRSTCHAR
        beq t2state8exit          ; exit
        clr ERROR_3               ; clear error flag
        movb #01, t2state         ; back to state 1
        movb #01, FIRSTCHAR         ; sets first char flag
t2state8exit:
        rts
t2state9:                         ; add digit pressed
        ldx POINTER
        ldab -1, x                ; Loads B with digit pressed High Low
        jsr OUTCHAR               ; Display digit on screen
        clr DECHO                 ; Clear for mastermind
        movb #01, t2state         ; back to state 1
        rts
     
t2state10:                        ; Short Blank Message
        ldaa FIRSTCHAR
        cmpa #01                  ; checks for first char
        bne t2state10a            ;  putchar
        TST F1                    ; Top or bottom?
        bne TOP_ROW_10            ; Top
        ldaa #LCD_address4        ; Loads bottom first available
        ldx #SBLANK               ; Loads short blank, 6 spaces
        jsr PUTCHAR_1st           ; prints first char
        bra t2state10low          ; test for full print
TOP_ROW_10:
        ldaa #LCD_address3        ; Loads Top first available
        ldx #SBLANK               ; loads short blank, 6 spaces
        jsr PUTCHAR_1st           ; prints first character
        bra t2state10low          ; test for full print on bottom
t2state10a
        jsr PUTCHAR               ; prints next char
t2state10low:                     ; Tests for full print
        TST FIRSTCHAR
        beq t2state10exit         ; exits
        clr D_SBLANK              ; clear short blank flag
        movb #01, t2state         ; back to state 1
        movb #01, FIRSTCHAR       ; sets first char flag
        jsr GETADDR               ; Loads address to A
        deca                      ; Decrements A 6 times
        deca
        deca
        deca
        deca
        deca
        jsr SETADDR               ; Sets address for digits to be entered (6)
t2state10exit:
        rts

t2state11:                        ; Perform backspace 
        ldaa DBS
        cmpa #02
        beq  t2state11a           ; Branches for space
        cmpa #03 
        beq  t2state11b           ; Branches if second backspace
        ldab #$08                 ; backspace in ASCII language
        jsr OUTCHAR               ; send backspace command
        inc DBS                   ; Increment DBS flag
        bra t2state11done
t2state11a:
        ldab #$20                 ; space in ASCII
        jsr OUTCHAR               ; Send space to display
        inc DBS                   ; Increment DBS flag
        bra t2state11done         ; exit
t2state11b
        ldab #$08                 ; backspace in ASCII
        jsr OUTCHAR               ; Perform second backspace command
        clr DBS                   ; Clears Backspace flag
        movb #01, t2state         ; Back to state 1       
t2state11done:
        rts

PUTCHAR_1st: 
        stx DPTR                  ; Stores first character read & write
        jsr SETADDR               ; Sets address to write to
        clr FIRSTCHAR             ; clear first char flag 
PUTCHAR:
        ldx DPTR                  ; Loads x with next character to read&write
        ldab 0,x                  ; Loads b with character
        beq done_printing             ; Leaves once ASCII Null is read
        inx
        stx DPTR                  ; Stores location of next character to read
        jsr OUTCHAR               ; Displays character
        rts
done_printing:
        movb #$01,FIRSTCHAR       ; sets first char flag
        rts 

t2state12:                        ; prints long blank
        ldaa FIRSTCHAR
        cmpa #01                  ; checks first char flag
        bne t2state12a
        TST F1                    ; Top or Bottom?
        bne TOP_ROW_12            ;
        ldaa #LCD_address4        ; Loads bottom first address
        ldx #LBLANK               ; Loads long blank message
        jsr PUTCHAR_1st           ; First character of message needs to be 
                                  ; printed
        bra t2state12low
TOP_ROW_12:
        ldaa #LCD_address3        ; Loads Appropriate LCD address.
        ldx #LBLANK               ; Loads Message to be Displayed
        jsr PUTCHAR_1st           ; print first char
        bra t2state12low          ; test for full print
t2state12a:
        jsr PUTCHAR               ; print next char
t2state12low:                     ; test for full print
        TST FIRSTCHAR
        beq t2state12exit         ; exit
        clr D_LBLANK
        movb #01, t2state         ; back to state 1
        movb #01, FIRSTCHAR       ; Resets FIRSTCHAR flag
t2state12exit:
        rts
        
t2state13:                        ; Hides Cursor
        ldaa #$30                 
        jsr SETADDR               ; Sets cursor location
        movb #01, t2state         ; back to state 1
        clr HIDE_CURS             ; clears flag
        rts

;/--------------------------------------------------------------------------\
;|  TASK_3: KEYPAD                                                          |
;\--------------------------------------------------------------------------/
TASK_3:
        ldaa  t3state            ; Get current t3state and branch accordingly
        beq   t3state0           ; init keypad
        deca
        lbeq   t3state1          ; Gets key, stores, and reports to 
                                 ; mastermind       
        deca
        lbeq   t3state2          ; waits for recognition from mastermind
        rts                      ; return to stack

t3state0:                        ; Init keypad
        jsr INITKEY              ; Turns on the keypad
        movb #$01, t3state       ; Sets next state
        rts

t3state1:                        ; Waits for key
        TST LKEY_FLG             ; Tests if a key has been pressed
        beq t3state1exit         ; Exit if no key has been pressed
        jsr GETCHAR              ; Get Key that has been pressed
        stab KEY_BUFF            ; Store pressed key in KEY_BUFF
        movb #$01, KEY_FLG       ; Sets flag for mastermind
        movb #$02, t3state       ; Sets t3state1 to t3state2
t3state1exit:
        rts
        
t3state2:                        ; Waiting for acknowledgement
        TST KEY_FLG              
        bne t3state2exit         ; Exits if not acknowledged by mastermind
        movb #$01, t3state       ; Sets t3state 2 back to state 1 once                                 
                                 ; mastermind has recognized press
t3state2exit:
        rts                      ; exits
               
;/--------------------------------------------------------------------------\
;|  TASK_4 PATTERN_1:                                                       |
;\--------------------------------------------------------------------------/
TASK_4:  
        ldaa  t4state                  ; Get current t4state and branch 
                                       ; accordingly
        beq   t4state0                 ; init pattern 1
        deca
        beq   t4state1                 ; tests if mastermind has set on,
                                       ; does the pattern
        deca
        beq   t4state2                 ; green
        deca
        beq   t4state3                 ; none
        deca
        beq   t4state4                 ; red
        deca
        beq   t4state5                 ; none
        deca                           
        lbeq   t4state6                ; both
        deca
        lbeq   t4state7                ; none  
        rts                            

t4state0:                              ; init TASK_4
        bclr  PORTP, LED_MSK_1         ; turns off LEDs at Init
        bset  DDRP, LED_MSK_1          ; set LED_MSK_1 pins as PORTS outputs
        movb  #$01, t4state            ; t4state0 to t4state1
        
exit_t4s0:
        rts

t4state1:                              ; wait
        TST ON_1                       ; Tests if mastermind has set ON_1
        BEQ t4s1off                    ; Exits because LEDs should off
        movb #02, t4state              ; Sets t4state1 to t4state 2
t4s1off:
        rts

t4state2:                              ; Turns on Green
        TST   ON_1
        BEQ   t4s2off                  ; Exits if flag is 0
        bset  PORTP, G_LED_1           ; Setting Green LED_1
        tst   DONE_1                   ; Testing DONE_1
        beq   exitst4s2                ; if not done, return
        movb  #$03, t4state            ; sets t4state2 to t4state3
exitst4s2:
        rts
t4s2off:
        bclr PORTP, LED_MSK_1          ; Clears red and green LED_1
        movb #$01, t4state             ; sets t4state2 back to t4state1
        rts

t4state3:                              ; Turn off Green and Red
        TST   ON_1
        BEQ   t4s3off                  ; Exits if flag is 0
        bclr  PORTP, LED_MSK_1         ; Clears LED
        tst   DONE_1                   ; checks done flag
        beq   exitst4s3                ; if not done, return
        movb  #$04, t4state            ; sets t4state3 to t4state4
exitst4s3:
        rts                            
t4s3off:
        bclr PORTP, LED_MSK_1          ; Turns off LEDs
        movb #$01, t4state             ; sets t4state3 back to t4state1
        rts

t4state4:                              ; Sets Red LED_1
        TST   ON_1
        BEQ   t4s4off                  ; Exits if flag is 0
        bset  PORTP, R_LED_1           ; Turns on Red LED_1
        tst   DONE_1                   ; check done flag
        beq   exitst4s4                ; if not done, return
        movb  #$05, t4state            ; sets t4state4 to t4state5
exitst4s4:    
        rts
t4s4off:
        bclr PORTP, LED_MSK_1          ; Turns off LEDs
        movb #$01, t4state             ; sets t4state 4 back to t4state1
        rts    

t4state5:                              ; Turns off Red and Green
        TST   ON_1
        BEQ   t4s5off                  ; Exits if flag is 0
        bclr  PORTP, LED_MSK_1         ; set state4 pattern on LEDs
        tst   DONE_1                   ; check done flag
        beq   exitst4s5                ; if not done, return
        movb  #$06, t4state            ; sets t4state5 to t4state6
exitst4s5:
        rts
t4s5off:
        bclr PORTP, LED_MSK_1          ; Turns off LEDs
        movb #$01, t4state             ; sets t4state5 back to t4state1
        rts    

t4state6:                              ; Turns on Green and Red
        TST   ON_1
        BEQ   t4s6off                  ; Exits if flag is 0
        bset  PORTP, LED_MSK_1         ; set red and green on for LED_1
        tst   DONE_1                   ; check done flag
        beq   exitst4s6                ; if not done, return
        movb  #$07, t4state            ; sets t4state6 to t4state7
exitst4s6:    
        rts
t4s6off:
        bclr PORTP, LED_MSK_1          ; Turns off LEDs
        movb #$01, t4state             ; sets t4state6 back to t4state1
        rts

t4state7:                              ; Turns off Red and Green
        TST   ON_1
        BEQ   t4s7off                  ; Exits if flag is 0
        bclr  PORTP, LED_MSK_1         ; Turns off Red and Green
        tst   DONE_1                   ; check done flag
        beq   exitst4s7                ; if not done, return
        movb  #$02, t4state            ; sets t4state7 to t4state8
exitst4s7:
        rts
t4s7off:
        bclr PORTP, LED_MSK_1          ; Turns off LEDs
        movb #$01, t4state             ; sets t4state7 back to t4state1
        rts                            


;/--------------------------------------------------------------------------\
;|  TASK_5 TIMING_1:                                                        |
;\--------------------------------------------------------------------------/
TASK_5: 
        ldaa  t5state                  ; Get current t5state and branch  
                                       ; accordingly
        beq   t5state0                 ; init
        deca
        beq   t5state1                 ; Wait for ON flag
        deca
        beq   t5state2                 ; 
        rts                            
t5state0:                              ; initialization for TASK_5
        clr   DONE_1                   ; Set DONE_1 to 0
        clrw  TICKS_1                  ; Set TICKS_1 to 0
        movb  #$01, t5state            ; set t5state0 to t5state 1
        rts

t5state1:                              ; Wait for keypad entry
        TST   ON_1                     ; Tests if ON flag has been set
        BEQ   t5s1off                  ; exit it hasn’t
        movb  #02, t5state             ; it has, move to t5state2
        movw  TICKS_1, COUNT_1         ; Initializes COUNT_1 to TICKS_1
t5s1off:
        rts                            ; drop to rts
        
t5state2:                              ; Re initialize COUNT_1
        TST   ON_1                     ; should this be on
        BEQ   t5s2off                  ; exits 
        tst   DONE_1                   ; check for need to reinitialize
        beq   t5s2a                    ; no need to reintialize;
        movw  TICKS_1, COUNT_1         ; reinitialize COUNT_1 to TICKS_1
        clr   DONE_1                   ; clear DONE_1 after reinitialization
t5s2a:  
        decw  COUNT_1                  ; decrement COUNT_1
        bne   exit_t5s2a               ; if COUNT_1 is not zero, simply  
                                       ; return
        movb  #$01, DONE_1             ; if COUNT_1 is zero, set DONE_1 and   
                                       ; return
exit_t5s2a:
        rts
t5s2off:
        movb  #$01, t5state            ; Returns to wait state if LEDs are 
                                       ; set to off 
        rts                            ; exit TASK_5
      
;/--------------------------------------------------------------------------\
;|  TASK_6 PATTERN_2:                                                       |
;\--------------------------------------------------------------------------/
TASK_6:  ldaa  t6state                  ; get current t6state and branch 
                                        ; accordingly
        beq   t6state0                  ; initialize
        deca
        beq   t6state1                  ; tests if mastermind has set on
                                        ; does the pattern
        deca
        beq   t6state2                  ; Green
        deca
        beq   t6state3                  ; Both off
        deca
        beq   t6state4                  ; Red
        deca
        beq   t6state5                  ; Both off
        deca
        lbeq   t6state6                 ; Both on
        deca
        lbeq   t6state7                ; Both off
        rts                            ; undefined state - do nothing but 
                                       ; return

t6state0:                              
        bclr  PORTP, LED_MSK_2         ; Turn both lights off
        bset  DDRP, LED_MSK_2          ; set LED_MSK_2 pins as PORTS outputs
        movb  #$01, t6state            ; back to state 1
exit_t6s0:        
        rts

t6state1:
        TST ON_2                       ; Tests if LEDs should be On
        BEQ t6s1off                    ; Exits if Mastermind has not set LEDs
        movb #02, t6state              ; move to state 2
t6s1off:
        rts

t6state2:                              ; Green on
        TST   ON_2                     ; should this be on
        BEQ   t6s2off                  ; Exits if should be off
        bset  PORTP, G_LED_2           ; Turns green on
        tst   DONE_2                   ; Testing Done_2
        beq   exit_t6s2                ; exit
        movb  #$03, t6state            ; set to state3

exit_t6s2:
        rts
t6s2off:                               ; exit ON set off
        bclr PORTP, LED_MSK_2          ; Clears LEDs
        movb #$01, t6state             ; back to state 1
        rts

t6state3:                              ; Clears LEDS
        TST   ON_2                     ; Should this be on
        BEQ   t6s3off                  ; No, exit
        bclr  PORTP, G_LED_2           ; Turn Green on
        tst   DONE_2                   ; Test Done 2
        beq   exit_t6s3                ; Exit
        movb  #$04, t6state            ; Set to state 4
exit_t6s3:
        rts
t6s3off:                               ; exit ON set Off
        bclr PORTP, LED_MSK_2          ; Clear LEDs
        movb #$01, t6state             ; back to state 1
        rts

t6state4:                              ; Sets Red LED
        TST   ON_2                     ; Should this be on?
        BEQ   t6s4off                  ; No, exit
        bset  PORTP, R_LED_2           ; Turn Red on
        tst   DONE_2                   ; Test Done_2
        beq   exit_t6s4                ; exit
        movb  #$05, t6state            ; Set to state 5
exit_t6s4:    
        rts
t6s4off:
        bclr PORTP, LED_MSK_2          ; Turns off LEDs
        movb #$01, t6state             ; Returns to Wait State since ON_2=0
        rts    

t6state5:                              ; not G, not R
        TST   ON_2
        BEQ   t6s5off                  ; Exits if LEDs' flag is 0
        bclr  PORTP, R_LED_2           ; set state4 pattern on LEDs
        tst   DONE_2                   ; check TASK_6 done flag
        beq   exit_t6s5                ; if not done, return
        movb  #$06, t6state            ; if done, set next state
exit_t6s5:
        rts
t6s5off:
        bclr PORTP, LED_MSK_2          ; Turns off LEDs
        movb #$01, t6state             ; Returns to Wait State since ON_2=0
        rts     

t6state6:                              ; G, R
        TST   ON_2
        BEQ   t6s6off                  ; Exits if LEDs' flag is 0
        bset  PORTP, LED_MSK_2         ; set state5 pattern on LEDs
        tst   DONE_2                   ; check TASK_6 done flag
        beq   exit_t6s6                ; if not done, return
        movb  #$07, t6state            ; if done, set next state
exit_t6s6:    
        rts
t6s6off:
        bclr PORTP, LED_MSK_2          ; Turns off LEDs
        movb #$01, t6state             ; Returns to Wait State since ON_2=0
        rts 

t6state7:                              ; not G, not R
        TST   ON_2
        BEQ   t6s7off                  ; Exits if LEDs' flag is 0
        bclr  PORTP, LED_MSK_2         ; set state6 pattern on LEDs
        tst   DONE_2                   ; check TASK_6 done flag
        beq   exit_t6s7                ; if not done, return
        movb  #$02, t6state            ; if done, set next state
exit_t6s7:
        rts                            ; exit TASK_6
t6s7off:
        bclr PORTP, LED_MSK_2          ; Turns off LEDs
        movb #$01, t6state             ; Returns to Wait State since ON_2=0
        rts 

;/------------------------------------------------------------------------------------\
;|  TASK_7 TIMING_2:                                                                 |
;\------------------------------------------------------------------------------------/
TASK_7:  ldaa  t7state                  ; get current t7state and branch 
                                        ; accordingly
        beq   t7state0
        deca
        beq   t7state1
        deca
        beq   t7state2
        rts                            ; undefined state - do nothing but return

t7state0:                              ; initialization for TASK_7
        clrw   TICKS_2                 
        clr    DONE_2                  
        movb   #$01, t7state           ; set next state
        rts

t7state1:                              ; Waiting for Period to be Entered
        TST   ON_2                     ; Tests if ON flag has been set
        BEQ   t7s1off
        movb  #02, t7state             ; Sets next state
        movw  TICKS_2, COUNT_2         ; Initializes COUNT_2 to TICKS_2
t7s1off:
        rts 
        
t7state2:                              ; (re)initialize COUNT_2
        TST   ON_2
        BEQ   t7s2off                  ; Branches if LEDs are set to be off
        tst   DONE_2                   ; check for need to reinitialize
        beq   t7state2a                ; no need to reinitialize;
        movw  TICKS_2, COUNT_2         ; reinitialize COUNT_2 to TICKS_2
        clr   DONE_2                   ; clear DONE_2 after reinitialization
t7state2a:  
        decw  COUNT_2                  ; decrement COUNT_2
        bne   exit_t7s2a               ; if COUNT_2 is not zero, simply 
                                       ; return
        movb  #$01, DONE_2             ; if COUNT_2 is zero, set DONE_2 and 
                                       ; return
exit_t7s2a:
        rts
t7s2off:
        movb #01, t7state              ; Returns to wait state if LEDs are 
                                       ; set to off
        rts                            ; exit TASK_7

;/--------------------------------------------------------------------------\
;|  TASK_8 DELAY 1ms:                                                       |
;\--------------------------------------------------------------------------/
TASK_8:  ldaa  t8state                 ; get current t8state and branch
                                       ; accordingly
        beq   t8state0
        deca                           ; dec t8state by 1, store in t8state
        beq   t8state1                 ; simple branch
        rts                            ; undefined state - do nothing but
                                       ; return

t8state0:                              ; initialization for TASK_8
                                       ; no initialization required
        movb  #$01, t8state            ; set next state
        rts

t8state1:
        jsr   DELAY_1ms
        rts                            ; exit TASK_8

;/--------------------------------------------------------------------------\
;| Subroutines                                                              |
;\--------------------------------------------------------------------------/
; General purpose subroutines go here
DELAY_1ms:
        ldy   #$0584
INNER:                                 ; inside loop
        cpy   #0
        beq   EXIT
        dey
        bra   INNER
EXIT:
        rts                            ; exit DELAY_1ms
 
;/--------------------------------------------------------------------------\
;| ASCII Messages and Constant Data                                         |
;\--------------------------------------------------------------------------/
; Any constants can be defined here
TIME_1:  DC.B  'TIME1 =',$00
TIME_2:  DC.B  'TIME2 =',$00
F_1:     DC.B  '<F1> TO UPDATE LED1 PERIOD',$00
F_2:     DC.B  '<F2> TO UPDATE LED2 PERIOD',$00
ERROR_MSG_1: DC.B  '       MAGNITUDE TOO LARGE',$00
ERROR_MSG_2: DC.B  'ZERO MAGNITUDE INPUT NOT VALID',$00
ERROR_MSG_3: DC.B  '       NO DIGITS ENTERED',$00 
SBLANK:   DC.B  '      ' , $00
LBLANK:  DC.B  '                                ' , $00
;/--------------------------------------------------------------------------\
;| Vectors                                                                  |
;\--------------------------------------------------------------------------/
; Add interrupt and reset vectors here
        ORG   $FFFE                    ; reset vector address
        DC.W  Entry
        ORG   $FFCE                    ; Key Wakeup interrupt vector address
                                       ; [Port J]
        DC.W  ISR_KEYPAD
