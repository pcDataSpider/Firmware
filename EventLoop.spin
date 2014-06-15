Con
  cDigitalPin   = 16   ' first digital pin# 
DAT
NextIns       long      0
pub main

NextIns := @EventCog-@EventLoopEnd+1
PUB AddEvent(ConditionType, ActionType, ConditionParam, ActionParam)|instructions,ins,lastIns,last2Ins

case ActionType
  0:
  1:
  2:
  3:
case ConditionType
  0: 'Always!
    instructions := @Always
  1: 'OnChange
    instructions := @OnChange    
  2: 'OnRising              
    instructions := @OnRising
  3: 'OnFalling             
    instructions := @OnFalling
' copy instructions into memory
repeat
  ins := long[instructions++]           
  if long[NextIns] == 0
    quit
  elseif long[NextIns] & $1FF
    long[NextIns] := ins|ConditionParam
  else
    long[NextIns] := ins
  last2Ins := lastIns
  lastIns := ins
long[NextIns-4]:=lastIns|ActionType
long[NextIns-8]:= last2Ins|ActionParam 
long[NextInS] := JMPLOOP | @EventLoop

DAT
org 0
LabelLulz
long $FFFFFFFF
TestLab     NOP         ' pin change event
TestLab_ret ret
r3 long 0
              MOV       r3,    #$000
              jmp      #TestLab

DAT
JMPLOOP       long      $5C7C0000

Always        long      $A0FC2000
              long      $5CFC0000

OnChange                 
              long      $867C0600
              long      $A0FC2000
              long      $5CE80000
              long      0         
OnRising      
              long      $857C0600
              long      $A0BC2001
              long      $62FC0200    
              long      $A0FC0200
              long      $5CC40000
              long      0
OnFalling     
              long      $857C0600
              long      $A0BC2001
              long      $62FC0200    
              long      $A0FC0200
              long      $5CC80000
              long      0
WhileHigh
              long      $A0BC2001 
              long      $62FC0200    
              long      $A0FC0200
              long      $5CE80000
              long      0
WhileLow
              long      $A0BC2001
              long      $62FC0200    
              long      $A0FC0200
              long      $5CD40000
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
                         
' these are general purpose registers, they can be changed.
outMask       long      0       ' pin mask for outputs
inMask        long      0       ' pin mask for inputs    
PinMask       long      %00000000111111110000000000000000
                         
pDigDir       long      0
pDigOut       long      0
pDigIn        long      0                                
pAIChannels     long      0       ' pointer to active channels
pAIChange       long      0       ' pointer to changed channels
MSB           long      $80000000
Neg1_2        long      -1              

r1            long      0
r2            long      0
' defined actions:
Action 
        mov   r2,     r1
        and   r2,      #$FF
        shr   r1,      #8
                                   
        cmp   r2,     #01     WZ
 if_z   call  #AIStart
        cmp   r2,     #02     WZ
 if_z   call  #AIStop
 
Action_ret ret
AIStart
        mov    r2,    #1                      ' get bitmask of ch idx
        shl    r2,    r1                     '
        rdlong r1,     pAIChannels               ' read current AI states
        or     r1,     r2                    ' add idx to current active channels
        wrlong r1,     pAIChannels               ' write new AI states
        wrlong r2,    pAIChange                 ' notify changes
AIStart_ret ret

AIStop                      
        mov    r2,      #1                      ' get bitmask of ch idx
        shl    r2,      r1                     '                       
        xor    r2,      r2                    ' negate bitmask
        rdlong r1,      pAIChannels               ' get current AI states 
        and    r1,      r2                    ' stop idx channel
        wrlong r1,      pAIChannels               ' write new AI states
        xor    r2,      r2                    ' get original bitmask
        wrlong r2,      pAIChange                 ' notify changes
AIStop_ret ret
DebugAction
Debug_ret ret      

                                   
readInput                            
              mov       prevIns,inputs         ' save state 
              shr       inputs,  #cDigitalPin    ' shift back so that pin# -> channel# 
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
              xor       chanDir,Neg1_2
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
              long  $DDDDDDDD 
              long  $DDDDDDDD

              'Events:
              ' loop through all our events
long    $00000000[400]
fit 496
              long  $FFFFFFFF
              long  $EFBEADDE
              ' pin change event
              CMP       changedIns, #$000           WZ
              MOV       r1,    #$000
        if_z  call      #Action
                               
              long  $AAAAAAAA
              ' pin high event (rising edge)
              CMP       changedIns, #00         WC
              mov       r1,    inputs          
              and       r1,    #00            WZ
              mov       r1,    #00
if_nz_and_nc  call      #ACTION
              long  $BBBBBBBB
              ' pin low event  (falling edge)
              CMP       changedIns, #0         WC
              mov       r1,    inputs
              and       r1,    #0            WZ 
              mov       r1,    #00
if_z_and_nc   call      #ACTION
              long  $CCCCCCCC
              ' while high                        
              mov r1,  inputs
              and r1,  #00                    WZ
              mov r1,    #00
        if_z  call      #ACTION
              'while low       
              long  $DDDDDDDD
              mov r1,  inputs
              and r1,  #00                    WZ 
              mov r1,    #00
        if_nz call      #ACTION
