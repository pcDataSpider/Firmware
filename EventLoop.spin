Con
  { ==[ CLOCK SET ]== }       
  _CLKMODE      = XTAL1 + PLL16X
  _XINFREQ      = 5_000_000                          ' 5MHz Crystal

  cDigitalPin   = 16   ' first digital pin# 
DAT
EvtCog        long      0
NextIns       long      0

DAT 'test code vars
        Debug         long      0
        'Digitals data
        DigDir          long %00001111                  ' bitmask for channel directions (MSB set when changed)
        DigOut          long %00000000                  ' state of output channels 
        DigIn           long 0                  ' state of input channels (MSB set when changed)
{
obj                   
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

  start(@debug, @DigDir, @DigOut, @DigIn, 0, 0)
  cognew(readLoop, @stack2)
  waitms(5000)
  AddEvent(2,1,$1FF,42)
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
pub start( MessageAddr, DigDirAddr, DigOutAddr, DigInAddr, AIChannelsAddr, AIChangeAddr)

NextIns := @EventLoopEnd

pMessage :=     MessageAddr
pDigDir  :=     DigDirAddr
pDigOut  :=     DigOutAddr
pDigIn   :=     DigInAddr
pAIChannels :=  AIChannelsAddr
pAIChange   :=  AIChangeAddr

EvtCog  := CogNew(@EventCog, 0)

pub stop
  CogStop(EvtCog)
PUB AddEvent(ConditionType, ActionType, ConditionParam, ActionParam)|instructions,InsPtr,ins,lastIns,last2Ins,ActionRet

case ActionType
  0:
    ActionType := 0
    ActionRet  := 0
  1: 'Message
    ActionType := (@Notify - @EventCog)/4
    ActionRet  := (@Notify_ret - @EventCog)/4
  2: 'AIStart                               
    ActionType := (@AIStart - @EventCog)/4
    ActionRet  := (@AIStart_ret - @EventCog)/4
  3: 'AIStop                             
    ActionType := (@AIStop - @EventCog)/4
    ActionRet  := (@AIStop_ret - @EventCog)/4
case ConditionType
  0: 'Always!
    instructions := @Always
  1: 'OnChange
    instructions := @OnChange    
  2: 'OnRising              
    instructions := @OnRising  
  3: 'OnFalling             
    instructions := @OnFalling
  4: 'OnFalling             
    instructions := @WhileHigh
  5: 'OnFalling             
    instructions := @WhileLow
' copy instructions into memory
cogstop( EvtCog )
insPtr:=NextIns
repeat                 
  ins := long[instructions]           
  if ins == 0
    quit
  elseif (ins & $1FF) == 0
    long[insPtr] := ins|(ConditionParam & $1FF)
  else
    long[insPtr] := ins
  last2Ins := lastIns
  lastIns := ins                
  instructions += 4
  insPtr += 4                                  
long[insPtr-8]:= last2Ins|(ActionParam & $1FF) 
long[insPtr-4]:= lastIns |(ActionRet<<9) |(ActionType)
long[insPtr] := JMPLOOP
NextIns := insPtr          
EvtCog := cognew(@EventCog, 0 )

pub WaitMS(MS)                
  waitcnt(((clkfreq/1000) * MS) +cnt) 'wait for a specified number of MS
DAT
org 0
Placeholder   ret
long    $FFAAFFAA
long    $AAFFAAFF
   
JMPLOOP       jmp       #EventLoop



Always        MOV       r1,    #$000
              jmpret    Placeholder,#Placeholder
              long      0
OnChange      
              ' pin change event
              MOV       r2,     #$000
              AND       r2, changedIns           WZ
              MOV       r1,    #$000
       if_nz  jmpret    Placeholder,#Placeholder
              long      0       
OnRising                        
              ' pin high event
              MOV       r2,     #$000               
              AND       r2, changedIns          WZ
              AND       r2, inputs              WZ
              MOV       r1,    #$000
       if_nz  jmpret    Placeholder,#Placeholder
              long      0       
OnFalling                         
              ' pin low event
              MOV       r1, inputs
              XOR       r1, Neg1
              MOV       r2,     #$000               
              AND       r2, changedIns          WZ
              AND       r2, r1                  WZ  
              MOV       r1,    #$000
       if_nz  jmpret    Placeholder,#Placeholder
              long      0 
WhileHigh
              MOV       r2,     #0000
              AND       r2, inputs              WZ
              MOV       r1,    #$000
       if_nz  jmpret    Placeholder,#Placeholder
              long      0       
WhileLow                        
              MOV       r2,     #0000
              AND       r2, inputs              WZ
              MOV       r1,    #$000
        if_z  jmpret    Placeholder,#Placeholder
              long      0       
              
DAT
org 0
EventCog
jmp #EventLoopStart
'define some registers. ORDER MATTERS
' these registers are used in generated code, and their position
' should NOT be changed.
inputs        long      0       ' state of input pins 
chanOut       long      0       ' state of output pins 
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
MSB           long      $80000000
Neg1        long      -1              
                          
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
              and       chanOut,outMask        ' only set output pins  
              mov       outa,   chanOut 
              
EventLoopEnd  jmp       #EventLoop 

long    $00000000[400]

jmp     #EventLoopEnd   ' if code falls through to here, break something so we notice
fit 496                                   