Con
  { ==[ CLOCK SET ]== }       
  _CLKMODE      = XTAL1 + PLL16X
  _XINFREQ      = 5_000_000                          ' 5MHz Crystal

  cDigitalPin   = 16   ' first digital pin#
  cEvtLoopLen   = 350 
DAT
EvtCog        long      0
NextIns       long      0
CurTimer      long      0
Triggers      long      0

DAT 'test code vars
        Debug         long      0
        'Digitals data
        DigDir          long %00001111                  ' bitmask for channel directions (MSB set when changed)
        DigOut          long %00000000                  ' state of output channels 
        DigIn           long 0                  ' state of input channels (MSB set when changed)

{obj                   
  Msg       : "Messager"
  ExecCB    : "MessageHandler"
var                
  long Stack2[50]
  long MSGCOG 
pub main
  MSGCOG:=Msg.start
  if MSGCOG==0
    repeat
      Msg.str(string("Failed to open Com")) ' lol, should this happen com wont work anyways... 
    Msg.stop
    return 0

  init(@debug, @DigDir, @DigOut, @DigIn, 0, 0)
  start
  cognew(readLoop, @stack2)
  waitms(5000)
  SetTimer(0, $03000000)
  AddEvent(3,1,$1FF,42)
  AddEvent(0,1,0,20)
  AddEvent(3,0,$1FF,0) 
pri ReadLoop | n, m, mask, curVal, curTime, pushed, ldbg1, ldbg2, ldbg3, ldbg4, duty, inc
' test code ...
  duty:=0                                                 
  inc:=1


  repeat
    'test digitals
    curVal:=DigIn~
    if curVal                                                       
      Msg.Str(String("."))
    ' test debugmsg
    curVal:=debug~
    if curVal
      Msg.Str(String("!"))
      Msg.dec(curVal)          
}
pub init( MessageAddr, DigDirAddr, DigOutAddr, DigInAddr, AIChannelsAddr, AIChangeAddr)

NextIns := @EventLoopEnd

pMessage :=     MessageAddr
pDigDir  :=     DigDirAddr
pDigOut  :=     DigOutAddr
pDigIn   :=     DigInAddr
pAIChannels :=  AIChannelsAddr
pAIChange   :=  AIChangeAddr
pTriggers := @Triggers

pub start
  EvtCog  := CogNew(@EventCog, 0)
pub stop
  CogStop(EvtCog)
pub reset
  CogStop(EvtCog)
  ' reset all memory      
  NextIns:= @EventLoopEnd
  repeat cEvtLoopLen
    long[NextIns]:=0
    NextIns += 4         
  NextIns:= @EventLoopEnd
  long[NextIns] := JMPLOOP          
  EvtCog  := CogNew(@EventCog, 0)
PUB SetTimer(Timer, Delay)

Timers[(Timer*2)+1]:=Delay
CogStop(EvtCog)
EvtCog := CogNew(@EventCog, 0 )

PUB trigger(triggerID)
repeat until triggers==0
triggers := triggerID
PUB AddEvent(ConditionType,  ConditionParam, ActionType, ActionParam)|instructions,InsPtr,ins,ActionRet


case ActionType    
  0: 'TimerEnable                              
    ActionType := (@EnableTimer - @EventCog)/4
    ActionRet  := (@EnableTimer_ret - @EventCog)/4
    ActionParam:= (ActionParam * 2) + (@Timers - @EventCog)/4 
  1: 'Message
    ActionType := (@Notify - @EventCog)/4
    ActionRet  := (@Notify_ret - @EventCog)/4
  2: 'AIStart                               
    ActionType := (@AIStart - @EventCog)/4
    ActionRet  := (@AIStart_ret - @EventCog)/4
  3: 'AIStop                             
    ActionType := (@AIStop - @EventCog)/4
    ActionRet  := (@AIStop_ret - @EventCog)/4
  4: 'DOHigh                             
    ActionType := (@DOHigh - @EventCog)/4
    ActionRet  := (@DOHigh_ret - @EventCog)/4
  5: 'DOLow                             
    ActionType := (@DOLow - @EventCog)/4
    ActionRet  := (@DOLow_ret - @EventCog)/4         
case ConditionType                       
  0: 'TimerExpire             
    instructions := @TimerExpire
    ConditionParam := (ConditionParam * 2) + (@Timers - @EventCog)/4 
  1: 'Always!
    instructions := @Always
  2: 'OnChange
    instructions := @OnChange    
  3: 'OnRising              
    instructions := @OnRising  
  4: 'OnFalling             
    instructions := @OnFalling
  5: 'WhileHigh             
    instructions := @WhileHigh
  6: 'WhileLow             
    instructions := @WhileLow
  7: 'OnTrigger
    instructions := @OnTrigger                             

' copy instructions into memory
CogStop( EvtCog )
insPtr:=NextIns              
if ConditionType == -1                       
  ins:=long[insPtr-8]                                        
  long[insPtr] := (ins&$FFFFFE00)|(ActionParam & $1FF) 
  insPtr += 4
  ins:=long[insPtr-8]                                   
  long[insPtr] := (ins&$FFFC0000)|(ActionRet<<9)|(ActionType)
  insPtr += 4
else
  repeat                 
    ins := long[instructions] 
    if ins == 0
      quit                                               
    elseif (ins & $1FF) == 0                                        
      long[insPtr] := (ins&$FFFC0000)|(ActionRet<<9)|(ActionType)
    elseif (ins & $1FF) == 1
      long[insPtr] := (ins&$FFFFFE00)|(ActionParam & $1FF)         
    elseif (ins & $1FF) == 2
      long[insPtr] := (ins&$FFFFFE00)|(ConditionParam & $1FF)
    elseif (ins & $3FE00) == (2<<9)
      long[insPtr] := (ins&$FFFC01FF)|((ConditionParam & $1FF)<<9)
    else
      long[insPtr] := ins
    instructions += 4
    insPtr += 4
long[insPtr] := JMPLOOP
NextIns := insPtr          
EvtCog := CogNew(@EventCog, 0 )

pub WaitMS(MS)                
  waitcnt(((clkfreq/1000) * MS) +cnt) 'wait for a specified number of MS
DAT
org 0             
Placeholder   ret
ActPar        ret
CondPar       ret
TimerVal      ret
long    $FFAAFFAA
long    $AAFFAAFF
   
JMPLOOP       jmp       #EventLoop


              
Always        MOV       r1,     #ActPar
              jmpret    Placeholder,#Placeholder
              long      0
OnChange      
              ' pin change event
              MOV       r2,     #CondPar
              AND       r2,     changedIns           WZ
              MOV       r1,     #ActPar
       if_nz  jmpret    Placeholder,#Placeholder
              long      0       
OnRising                        
              ' pin high event
              MOV       r2,     #CondPar               
              AND       r2,     changedIns          WZ
              AND       r2,     inputs              WZ
              MOV       r1,     #ActPar
       if_nz  jmpret    Placeholder,#Placeholder
              long      0       
OnFalling                         
              ' pin low event
              MOV       r1,     inputs
              XOR       r1,     Neg1
              MOV       r2,     #CondPar               
              AND       r2,     changedIns          WZ
              AND       r2,     r1                  WZ  
              MOV       r1,     #ActPar
       if_nz  jmpret    Placeholder,#Placeholder
              long      0 
WhileHigh
              MOV       r2,     #CondPar
              AND       r2,     inputs              WZ
              MOV       r1,     #ActPar
       if_nz  jmpret    Placeholder,#Placeholder
              long      0       
WhileLow                        
              MOV       r2,     #CondPar
              AND       r2,     inputs              WZ
              MOV       r1,     #ActPar
        if_z  jmpret    Placeholder,#Placeholder
              long      0       
TimerExpire
              mov       r2,     CondPar
              cmp       r2,     Zero            WZ
              sub       r2,     cnt             WC      ' test if curTime>expireTime
         if_c cmp       halfCycle,r2            WC      ' r2 should be Negative diff between curTime and expireTime, so we use UNSIGNED greater-than to check if diff is LESS than halfCycle (halfCycle=$80000001)
  if_nz_and_c MOV       CondPar,Zero
              MOV       r1,     #ActPar
  if_nz_and_c jmpret    Placeholder,#Placeholder                
              long      0
OnTrigger                                 
              mov       r2,     #CondPar    
              cmp       r2,     TriggerVal      WZ
              mov       r1,     #ActPar
        if_z  jmpret    Placeholder, #Placeholder
               
DAT
org 0
EventCog
jmp #EventLoopStart
long 0 [3]
'define some registers. everything must be defined before the code, not after
inputs        long      0       ' state of input pins 
chanOut       long      0       ' state of output pins
eventChanOut  long      0       ' state of output pins, due to events
changedIns    long      0       ' pins that have changed
prevIns       long      0       ' previous state of input pins
chanDir       long      0       ' channel directions
                  

r1            long      0
r2            long      0
' these are general purpose registers, they can be changed.
outMask       long      0       ' pin mask for outputs
inMask        long      0       ' pin mask for inputs    
PinMask       long      %00000000111111110000000000000000

pMessage      long      0                         
pDigDir       long      0
pDigOut       long      0
pDigIn        long      0                                
pAIChannels   long      0       ' pointer to active channels
pAIChange     long      0       ' pointer to changed channels
pTriggers     long      0
MSB           long      $80000000
Neg1          long      -1      
halfCycle     long      $80000001
Zero          long      0

TriggerVal    long      0       ' external triggers
Timers        long      0[16]    ' Each timer has 2 longs        
                          
' defined actions:

Notify
        wrlong r1,     pMessage
Notify_ret ret    

AIStart
        mov    r2,    #1                        ' get bitmask of ch idx
        shl    r2,    r1                        '
        rdlong r1,     pAIChannels              ' read current AI states
        or     r1,     r2                       ' add idx to current active channels
        wrlong r1,     pAIChannels              ' write new AI states
        wrlong r2,    pAIChange                 ' notify changes
AIStart_ret ret

AIStop                      
        mov    r2,      #1                      ' get bitmask of ch idx
        shl    r2,      r1                      '                       
        xor    r2,      Neg1                  ' negate bitmask
        rdlong r1,      pAIChannels             ' get current AI states 
        and    r1,      r2                      ' stop idx channel
        wrlong r1,      pAIChannels             ' write new AI states
        xor    r2,      Neg1                  ' get original bitmask
        wrlong r2,      pAIChange               ' notify changes
AIStop_ret ret

EnableTimer                         
        movd  :enable,  r1         
        add   r1,       #1         
        movs  :delay,   r1        
        mov   r2,       cnt
:delay  ADD   r2,       Timers
        add   r2,       #1      'ensure r2!=0 without modifying flags with a cmp
:enable MOV   Timers ,r2
EnableTimer_ret ret

DOHigh
        shl   r1,       #cDigitalPin
        or    eventChanOut,     r1
DOHigh_ret ret

DOLow
        shl   r1,       #cDigitalPin
        xor   r1,       Neg1
        and   eventChanOut,     r1
DOLow_ret ret            
           
                                   
readInput                            
              mov       prevIns,inputs         ' save state 
              or        inputs,  MSB             ' set MSB       
              wrlong    inputs,  pDigIn          ' write new value              
readInput_ret ret

setPinDir
              or        chanDir,   MSB             ' set MSB
              xor       chanDir,   MSB             ' clear MSB 
              wrlong    chanDir,   pDigDir         ' save w/o MSB
               
              shl       chanDir,   #cDigitalPin    ' adjust from channel# -> pin#
              mov       dira,   chanDir            ' set new pin directions      
              ' save inMask and outMask
              mov       outMask,chanDir
              xor       chanDir,Neg1
              mov       inMask, chanDir
              and       inMask, PinMask                                                       
setPinDir_ret ret
' Loop starts:
EventLoopStart
' set pin directions, etc
        rdlong ChanDir, pDigDir                 WZ
        call #setPinDir
EventLoop
              'read dig inputs
              mov       inputs,  ina             ' read all pins
              and       inputs,  inMask          ' only check input pins
              shr       inputs,  #cDigitalPin
              'look at changed pins
              mov       changedIns, inputs
              xor       changedIns, prevIns     WZ
        if_nz call      #readInput              ' notify other cogs on change            
              'set digital outputs
              rdlong    chanOut,pDigOut         WZ                      
              shl       chanOut,#cDigitalPin    ' read state of output pins
              and       chanOut,outMask         ' only set output pins  
              or        chanOut,eventChanOut    ' or with event information
              mov       outa,   chanOut
              'load external triggers                  
              rdlong    TriggerVal, pTriggers  WZ
        if_nz mov       r1, #0
        if_nz wrlong    r1,     pTriggers
              
EventLoopEnd  jmp       #EventLoop 

long    $00000000[cEvtLoopLen]

jmp     #EventLoopEnd   ' if code falls through to here, break something so we notice
fit 496                                   