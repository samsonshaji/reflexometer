               THUMB             ; Thumb instruction set
                AREA          My_code, CODE, READONLY
                EXPORT        __MAIN
                        ENTRY  
__MAIN

; The following lines are similar to Lab-1 but use a defined address to make it easier.
; They just turn off all LEDs
                        LDR               R10, =LED_BASE_ADR            ; R10 is a permenant pointer to the base address for the LEDs, offset of 0x20 and 0x40 for the ports

                        MOV         R3, #0xB0000000         ; Turn off three LEDs on port 1  
                        STR         R3, [r10, #0x20]
                        MOV         R3, #0x0000007C
                        STR         R3, [R10, #0x40]  ; Turn off five LEDs on port 2

; This line is very important in your main program
; Initializes R11 to a 16-bit non-zero value and NOTHING else can write to R11 !!
                       
                        MOV                     R11, #0xABCD                  ; Init the random number generator with a non-zero number
                                    MOV32                   R3, #65535
                                    
GEN                       BL                    RandomNum
                                    CMP                           R3, R11
                                    BGT                           GEN
                                    
                                    MOV                           R5, R11
                                    MOV                           R3, #156
                                    MUL                           R11, R3                             ; Multiply by 157
                                    LSR                           R11, #7                             ; Divide by 128, roughly multiplies by 1.22
                                    
                        MOV               R3, #20000                    ; preparing to scale
                                    
                        ADD               R0, R11, R3                   ; scale R11 and put into R0
                       
                        BL                DELAY
                       
                        MOV               R3, #0x40                           ; 0100 0000 to turn on pin .29
                        BL                DISPLAY_NUM                   
                       
                        MOV               R6, #0
                        LDR               R11, =FIO2PIN                       ; address of the button
POLL              
                        LDR               R9, [R11]
                        LSR               R9, #10                             ; shift right by 10 to get the 10 bit to check if it is 0
                                    
                                          TST               R9, #1                              ; check if 10th bit is 0 (pressed)
                        BEQ               INIT                                ; if 10th bit is 0, go to INIT
                       
                        MOV               R0, #1                              ; in delay subroutine, R0 times 100 us
                        BL                DELAY
                                            ADD               R6, #1                              ; increment counter (check recation time)
                                            B                         POLL                                    ; Poll after delay
                       

INIT
                        MOV               R7, #4
                       
                        PUSH                {R6}
LOOP
                        AND               R3, R6, #0xFF                       ; get the first 8 bits from R6 into R3                
                        BL                DISPLAY_NUM
                       
                        MOV               R0, #20000                    ; delay by 2 secs
                        BL                DELAY
                       
                        LSR               R6, #8                              ; get the next 8 bits
                       
                        SUBS              R7, #1                                    ; decrement because we already displayed first 8 bits
                        BNE               LOOP
                       
                        POP               {R6}
                       
                        MOV               R0, #50000
                        BL                DELAY
                       
                        B                 INIT
           
                 
SIMPLE_COUNTER          STMFD               R13!, {R14}
                        MOV               R3, #0
                       
LOOP2                   BL                DISPLAY_NUM

                        MOV               R0, #1000
                        BL                DELAY
                       
                        CMP             R3, #0xFF
                        BEQ             DONE
                        ADD               R3, #1
                        B               LOOP2

DONE                    MOV               R3, #0
                        B                 LOOP2

                        LDMFD       R13!,{R15}

;
; Display the number in R3 onto the 8 LEDs
DISPLAY_NUM       STMFD       R13!,{R1, R2, R14}

; Usefull commaands:  RBIT (reverse bits), BFC (bit field clear), LSR & LSL to shift bits left and right, ORR & AND and EOR for bitwise operations

                        MOV               R2, #0xC0                     ; first two
                        MOV               R1, #0x20                     ; third bit
                        AND               R2, R3                              ; first two in r2
                        AND               R1, R3                              ; third in r1
                        LSL               R2, #1                        ; shift first two left by 1 (p30 doesnt exist)
                        ADD               R2, R1                        ; add two bits with the third bit to make it fourth bit (ab0c)
                        LSR               R2, #5                              ; shift right 5 to make c lsb
                        EOR               R2, #0xD                      ; complement all bits (active low)
                        RBIT        R2, R2                                    ; reverse bits to align them with memory address
                        STR               R2, [R10, #0x20]        ; offset by 20 to get address of first three pins
                       
                        MOV               R1, #0x1F                     ;
                        AND               R1, R3                        ; get the rest 5 bits
                        EOR               R1, #0x1F                     ; complement
                        RBIT        R1, R1                                    ; reverse
                        LSR               R1, #25                       ; align them
                        STR               R1, [R10, #0x40]        

                        LDMFD       R13!,{R1, R2, R15}

;
; R11 holds a 16-bit random number via a pseudo-random sequence as per the Linear feedback shift register (Fibonacci) on WikiPedia
; R11 holds a non-zero 16-bit number.  If a zero is fed in the pseudo-random sequence will stay stuck at 0
; Take as many bits of R11 as you need.  If you take the lowest 4 bits then you get a number between 1 and 15.
;   If you take bits 5..1 you'll get a number between 0 and 15 (assuming you right shift by 1 bit).
;
; R11 MUST be initialized to a non-zero 16-bit value at the start of the program OR ELSE!
; R11 can be read anywhere in the code but must only be written to by this subroutine
RandomNum         STMFD       R13!,{R1, R2, R3, R14}

                        AND               R1, R11, #0x8000
                        AND               R2, R11, #0x2000
                        LSL               R2, #2
                        EOR               R3, R1, R2
                        AND               R1, R11, #0x1000
                        LSL               R1, #3
                        EOR               R3, R3, R1
                        AND               R1, R11, #0x0400
                        LSL               R1, #5
                        EOR               R3, R3, R1        ; the new bit to go into the LSB is present
                        LSR               R3, #15
                        LSL               R11, #1
                        ORR               R11, R11, R3
                        
                        LDMFD       R13!,{R1, R2, R3, R15}

;
;           Delay 0.1ms (100us) * R0 times
;           aim for better than 10% accuracy
;               The formula to determine the number of loop cycles is equal to Clock speed x Delay time / (#clock cycles)
;               where clock speed = 4MHz and if you use the BNE or other conditional branch command, the #clock cycles =
;               2 if you take the branch, and 1 if you don't.

DELAY             STMFD       R13!,{R2, R14}
MultipleDelay     CMP               R0, #0            ; test R0 to see if it's 0 - set Zero flag so you can use BEQ, BNE

                  BEQ               exitDelay
                  MOV32             R2, #0x85         ;Value for 100us delay
                 
innerDelay
                  SUBS        R2, #1
                  BNE               innerDelay
           
                  SUB               R0, #1
                  B                 MultipleDelay
exitDelay         LDMFD       R13!,{R2, R15}; branch to the address in the Link Register.  Ie return to the caller
                       

LED_BASE_ADR                  EQU   0x2009c000        ; Base address of the memory that controls the LEDs
PINSEL3                 EQU   0x4002c00c        ; Address of Pin Select Register 3 for P1[31:16]
PINSEL4                 EQU   0x4002c010        ; Address of Pin Select Register 4 for P2[15:0]
FIO2PIN                 EQU         0x2009c054
;     Usefull GPIO Registers
;     FIODIR  - register to set individual pins as input or output
;     FIOPIN  - register to read and write pins
;     FIOSET  - register to set I/O pins to 1 by writing a 1
;     FIOCLR  - register to clr I/O pins to 0 by writing a 1

                        ALIGN

                        END

