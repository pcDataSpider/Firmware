con

MAXPERIOD = 100    


var
byte    wavCog            

obj
    pwm      : "AsmPwm"             

var
        long          outPin

Dat
        'pwm data
        pwmExec         long 0[2]
        pwmData         long 0[2]

        W1              long 50[MAXPERIOD] 
        W2              long 50[MAXPERIOD]

        Multiple1       long 0
        Period1         long 0
        Multiple2       long 0
        Period2         long 0
        
        DigOnT          long 0
        DigOffT         long 0

        DigitalPin          long 0

dat
        'sin wave
        Sin         long 500,531,563,594,624,655,684,713,741,768,794,819,842,864,885,905,922,938,952,965,976,984,991,996,999,1000,999,996,991,984,976,965,952,938,922,905,885,864,842,819,794,768,741,713,684,655,624,594,563,531,500,469,437,406,376,345,316,287,259,232,206,181,158,136,115,95,78,62,48,35,24,16,9,4,1,0,1,4,9,16,24,35,48,62,78,95,115,136,158,181,206,232,259,287,316,345,376,406,437,469,500
     '   Saw         long 0,20,40,...1000,980,960,...,0
        Saw         long 500[100]
pub start(Data1, Data2, PwmPeriod)

Init(Data1, Data2)    
pwm.setPeriod( PwmPeriod )
wavCog := cognew(@WavEntry,0)
return wavCog

pub Stop
  if wavCog
    cogstop(wavCog)
pub Init(Data1, Data2)
{ sets up data for cogs, storing addresses of key data}
                 
       
pPwmDuty1 := Data1 + 16
pPwmDuty2 := Data2 + 16
pW1 := @W1  
pW2 := @W2 
pDigPin := @DigitalPin 
pDigOnT := @DigOnT
pDigOffT := @DigOffT
  
pMult1 := @Multiple1
pPeriod1 := @Period1
pMult2 := @Multiple2  
pPeriod2 := @Period2

'maxpos1 := @W1 + (MAXPERIOD * 4)
'maxpos2 := @W2 + (MAXPERIOD * 4)

               

pub ChngDig(period, duty, pins) | ontime,offtime


ontime  := ((period*duty)/1000) 
offtime := (period-ontime)

DigPin := pins
DigOnT := ontime
DigOffT := offtime



pub ChngWav(wave, period, amp, pwmN)   | i,n
      
i := period/100
       
n:=0
repeat 100

  if Wave==1     
    val:=Sin[n]
  elseif Wave==2 
    val:=Saw[n]
    
  if pwmN==1
    W1[n]:=pwm.ChangePwm((val*amp)/1000)
    Multiple1:=i  
    Period1:=period
  else
    W2[n]:=pwm.ChangePwm((val*amp)/1000)   
    Multiple2:=i 
    Period2:=period 
  n++
dat
org 0

WavEntry

              mov       pos1,   pW1
              rdlong    DigPin, pDigPin
             ' or        dira,   DigPin 
              mov       startT, cnt
              add       startT, LoopDur   
                              
:loop                                   
              rdlong    digon,   pDigOnT 
              rdlong    digoff,  pDigOffT
              rdlong    DigPin, pDigPin
                

              
              rdlong    per1,  pPeriod1 wz
              rdlong    mult1,  pMult1    
        if_z  mov       m1,    #2
              rdlong    per2,  pPeriod2 wz    
        if_z  mov       m2,    #2
              rdlong    mult2,  pMult2
                                    

              sub       dign,   #1              wz, wc
if_z_or_c     mov       dign,   digon
if_z_or_c     add       dign,   digoff 
if_z_or_c     or        dira,   DigPin 
              cmp       dign,   digoff           wz, wc
if_c_or_z     andn      outa,   digpin   
if_nc_and_nz  or        outa,   digpin

                                  

              sub       n1,     #1              wz, wc
if_z_or_c     mov       n1,     per1   
if_z_or_c     mov       pos1,   pW1  
                                  
              sub       m1,     #1              wz, wc 
if_z_or_c     mov       m1,     mult1  
        if_z  rdlong    val,    pos1 
        if_z  wrlong    val,    pPwmDuty1
        if_z  add       pos1,   #4

        

              sub       n2,     #1              wz, wc
if_z_or_c     mov       n2,     per2   
if_z_or_c     mov       pos2,   pW2  
                                  
              sub       m2,     #1              wz, wc 
if_z_or_c     mov       m2,     mult2  
        if_z  rdlong    val,    pos2 
        if_z  wrlong    val,    pPwmDuty2
        if_z  add       pos2,   #4
                                      
              waitcnt   startT, LoopDur    
            

              jmp       #:loop     

pW1     long  0
pW2     long  0  
pMult1  long  0 
pMult2  long  0 
pPeriod1 long 0
pPeriod2 long 0
pDigOnT long  0
pDigOffT long 0
pDigPin long  0
pPwmDuty1 long 0
pPwmDuty2 long 0 
              
dign    long  0
digon   long  0
digoff  long  0
digpin  long  0 
      
n1      long  0
m1      long  0
pos1    long  0   
per1    long  0 
mult1   long  0 
'maxpos1 long  0

m2      long  0
n2      long  0
pos2    long  0
per2    long  0
mult2   long  0 
'maxpos2 long  0
             
startT  long  0
val     long 0


LoopDur long  10000
'ZERO    long  0
'LOW16   long    %00000000000000001111111111111111
'n1mask  long    %00000000000000001111111111111111
'n2mask  long    %11111111111111110000000000000000
'digonmask long  %00000000000000001111111111111111
'digoffmask long %11111111111111110000000000000000
   