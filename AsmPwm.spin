con

  cminOn  = 4
  cminOff = 16
            '(2-6)
var
byte    pwmCog            

obj               
  Com   :       "FullDuplexSerial_rr004" 
'dat
'        outPin          long    0  
'        onTime          long    0
'        offTime         long    0
'        changed         long    0
'        period          long    0
'        duty            long    0
var
        long          outPin
        long          onTime
        long          offTime
        long          changed
        long          times 
        long          period
        long          duty  
pub Main  | duty2, inc
  Init
  outPin:=%010
  period:=1015 
  'Com.Start(31,30,%0000,BAUD)    
  'changePwn(4) 3-> ... .. 4989

  '3 = 0
  'duty = 3 - 1005
  
  'changePwn(3)   
  changePwmAsm(500)
  Start( outPin, period)

  duty2 := 0
  inc := 1
  repeat            
    'Com.str(String(" ON:"))
    'Com.dec(onTime)
    'Com.str(String(" OFF:"))
    'Com.dec(offTime) 
    'Com.str(String(" DUTY:"))
    'Com.dec(duty2)     
    'Com.str(String(" PERIOD:"))
    'Com.dec(period)
    'Com.tx(13)
    duty2:=duty2 + inc 
    changePwmAsm(duty2) 
    if duty2=>period
      inc:=-1
    if duty=<0
      inc:=+1

pub Init
{ sets up data for cogs, storing addresses of key data}

pOnTime:= @onTime
pOffTime:= @offTime  
pChanged:=@changed
pTimes:=@times 
pPeriod:=@period
pDuty:=@duty 

pub Start( pins, tperiod )          
{ starts this PWM in a new cog
    pins = bitmask for output pins
    tperiod total cycles per period}

Init
outPin := pins
period := tperiod       
  
pwmCog := cognew(@pwmEntry,@outPin)
return pwmCog

pub Stop
  if pwmCog
    cogstop(pwmCog)
pub setPeriod(newperiod)
  period := newperiod
pub getChngAddr : addr
  addr:=@ChngDuty
pub getDataAddr  : addr
  addr:=@outPin
pub changePwmAsm( duty2 ) 
cognew(@ChngDuty,duty2)
pub changePwm( duty2 )
  { calculate on/off time for given duty
    min = 4
    max = period-27 }
    
  On := (period * duty2)/period 
  if On < cminOn                  ' duty too low 
    OnTime:=0   
    OffTime:=period-cminOn-cminOff+1
  elseif On > period-cminOff      ' duty too high
    OffTime:=0
    OnTime:=period-cminOff-cminOn+1
    
  elseif  On == cminOn            ' duty at min
    OffTime:=period-cminOff-cminOn
    OnTime:=1
  elseif On == period-cminOff     ' duty at max
    OffTime:=1 
    OnTime:=period-cminOff-cminOn '
    
  else                            ' normal
    OnTime:=On-cminOn    
    OffTime:=period-On-cminOff
  changed := 1
  times := (OffTime<<16) | OnTime

  return times

dat
{changes duty}
org 0
ChngDuty
              mov       lDuty,   par  
              rdlong    lPeriod, pPeriod
              wrlong    lDuty,  pDuty           ' save new duty value    

              mov       maxT,    lPeriod
              sub       maxT,    minOff
              sub       maxT,    minOn

              mov       maxDuty, lPeriod
              sub       maxDuty, minOff
                                   
              cmp       lDuty,   minOn           wz, wc 
        if_z  mov       lonTime, #1 
              nop                      ' <--- WHY?!
        if_z  mov       lOffTime,MaxT
        if_z  jmp       #:done   

        if_c  mov       lonTime, #0
        if_c  mov       lofftime,maxT
        if_c  add       loffTime,#1
        if_c  jmp       #:done  
                                    
              
              cmp       lDuty,   maxDuty         wz, wc
        if_z  mov       lonTime, maxT
        if_z  mov       loffTime,#1 
        if_z  jmp       #:done

if_nz_and_nc  mov       lonTime, maxT
if_nz_and_nc  add       lonTime, #1
if_nz_and_nc  mov       loffTime,#0
if_nz_and_nc  jmp       #:done

              mov       lOnTime, lDuty
              sub       lOnTime, minOn

              mov       lOffTime,lperiod
              sub       lOffTime,lDuty
              sub       lOffTime,minOff 
              jmp       #:done
     
:done
              mov       lTimes, lOffTime
              shl       lTimes, #16
              or        lTimes, lOnTime
              wrlong    lTimes, pTimes
              
              wrlong    lOnTime, pOnTime
              wrlong    lOffTime,pOffTime 
              mov       tmp,     #1       
              wrlong    tmp,     pChanged

              
              cogid    MyID    'get the ID of the cog I'm running in  
              cogstop  MyID    'use the ID to terminate myself

MyID    long  0

maxT    long  0
maxDuty long  0

minOn   long  4
minOff  long  11
                             
   
pOnTime long  4
pOffTime long 8
pChanged long 12
pTimes long   16  
pPeriod long  20 
pDuty   long  24  


lPeriod long  0
lDuty   long  0
lOnTime long  0
lOffTime long 0
lChanged long 0
lTimes  long  0  
                      

dat
         
              org       0
pwmEntry      ' entry point
              mov       tmp,    par
              mov       pMask,  tmp  
              add       pOnT,   tmp
              add       pOffT,  tmp 
              add       pChange,tmp
              add       pTiming,tmp   
                            

              call      #reload       
              or        dira,   Mask







              
                        
loop          ' main pwm loop   

              mov       T,      onT             wz  '13   '1 instruction = 4 cycles                                                    
              or        outa,   Mask                '1
:on     if_nz djnz      T,      #:on                '2                                   
                                      
              mov       T,      offT            wz  '3
              andn      outa,   Mask                '1
:off    if_nz djnz      T,      #:off               '2


              rdlong    Timing, pTiming             '6-9
              mov       OnT,    Timing              '7
              and       OnT,    OnTMask             '8
              mov       OffT,   Timing              '9
              and       OffT,   OffTMask            '10
              shr       OffT,   #16                 '11
             
        '      rdlong    Change, pChange         wz 
        'if_nz call      #reload     

              jmp       #loop                        '12








              
reload        ' reload on/off times, pinmask, reset change 
              mov       tmp,    #0  
              rdlong    Mask,   pMask
              rdlong    OnT,    pOnT 
              rdlong    OffT,   pOffT         
                    
              or        dira,   Mask
              
              wrlong    tmp,    pChange
reload_ret    ret   
 

                      

              on        long    0
              off       long    0
              T         long    0 
              tmp       long    0

                                    
              OnTMask   long    %00000000000000001111111111111111  
              OffTMask  long    %11111111111111110000000000000000  
              
              Mask      long    0
              OnT       long    0
              OffT      long    0
              Change    long    0
              Timing    long    0
              
              pMask     long    0
              pOnT      long    4
              pOffT     long    8 
              pChange   long    12
              pTiming   long    16
                      