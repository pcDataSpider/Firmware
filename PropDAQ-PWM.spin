Con

  { ==[ CLOCK SET ]== }       
  _CLKMODE      = XTAL1 + PLL16X
  _XINFREQ      = 5_000_000                          ' 5MHz Crystal
              
  nChannels     = 14   ' cant exceed 32.
  nAnalogI      = 4 
  nAnalogO      = 2 
  nDigitals     = 8

  cDigitalPin   = 16   ' first digital pin#                              
                      
  EX_DAT_LEN = 50
  btnMask       = %10000000 
con
  nKeys         = 10
obj
  Msg       : "Messager"
  ExecCB    : "MessageHandler"
  Stk       : "Stack Length"           'Include Stack Length Object
  pwm1      : "AsmPwm"                                  
  pwm2      : "AsmPwm"   

var        
  long Stack[400]  
  byte RDCOG   
  byte ADCCOG  
  byte DIGCOG            

dat                                           
        ExData          long    0[EX_DAT_LEN]
dat
        org 0                                                                
        version         long    11   
        model           long    7  
        nchans          long    nChannels
        push            long    0
        'freq            long    1015            ' period in instructions
        set             long    0   
        start           long    0
        stop            long    0             
        'read            long    0
dat     ' channel related data
        org 0
        ' these values define different channels
        Change          long 0                          ' bitmask for DAC channels whose parameters have changed. 1=change 0=no change
        Channels        long %0000000000                ' bitmask for active channels. 1=on 0=off  
        Rates           long 800000[nAnalogI]          ' sample rates for channels in clocks per sample
 
        ' next two values are set by the ADC loop 
        lastVal         long 120[nAnalogI]             ' last value read / value set 
        tstamp          long 0[nAnalogI]               ' time stamp of the last value read

        'pwm data
        pwmExec         long 0[nAnalogO]
        pwmData         long 0[nAnalogO]

        DigDir          long %00001111                  ' bitmask for channel directions (MSB set when changed)
        DigOut          long %00000110                  ' state of output channels 
        DigIn           long 0                  ' state of input channels (MSB set when changed)
                                
        dbg             long 0 
        dbg2            long 0  
        dbg3            long 0 
        dbg4            long 0
dat     ' name table
        NameTable       byte    "version",0,"model",0,"nchannels",0,"push",0,"set",0,"start",0,"stop",0,"dir",0,"type",0,"wav",0
                                                                          '    <set:chan#, val>
dat     ' message strings
        PushMsg         byte    "push",0        'static string for 'push' response   
pub Main
    
  Stk.Init(@Stack, 80)                  'Initialize reserved Stack space (reserved below)
                                           
  ' init callback data
  Table       := @version   
  pChangeCB   := @Change  
  pChannelsCB := @Channels  
  pRatesCB    := @Rates
  pPwmExecCB  := @pwmExec 
  pPwmDatCB   := @pwmData
  pDigInCB    := @DigIn 
  pDigOutCB   := @DigOut 
  pDigDirCB   := @DigDir

  ' get PWM data                    
  pwmExec[0]:= pwm1.getChngAddr   
  pwmExec[1]:= pwm2.getChngAddr 
  pwmData[0]:= pwm1.getDataAddr 
  pwmData[1]:= pwm2.getDataAddr 
                        

' init data for ADC reader 
  pChange   := @Change
  pChannels := @channels 
  pRates    := @Rates       
  plastVal  := @lastVal
  ptstamp   := @tstamp    
  pDbg      := @dbg    
  pDbg2     := @dbg2   
  pDbg3     := @dbg3
  pDbg4     := @dbg4  
  cs:= %00010000 ' pin bitmasks   '   %00010000
  din:=%00100000                 '   %00100000
  clk:=%01000000                 '   %01000000


  
  ' init data for Digital Cog
  pDigIn  := @DigIn
  pDigOut := @DigOut
  pDigDir := @DigDir
  pDbgX   := @Dbg





                                            
  ' start message cog  
  ExecCB.InitData(pwmData[0], pwmData[1], 1015)
  if Msg.start(@Callback, @NameTable, nKeys)==0
    repeat
      Msg.str(string("<Failed:com>")) ' lol, should this happen com wont work anyways... 
    Msg.stop
    return 0
              
   
  ' create locks 
  LockID:=LockNew
  if LockID==-1
    repeat                            
      Msg.Str(String("<Failed:locks>"))
    Msg.stop 
    return 0
  LockIDCB:=LockID 'share lock with callback code.   
                   
  'Msg.Pause                     ' pause for user to start it!
                   
  pwm1.start( %0111, 1015)
  pwm1.changePwmAsm(000) 
  pwm2.start( %1000, 1015)
  pwm2.changePwmAsm(000)   
  ADCCOG:=cognew( @DACLoop, 0 )+1                       ' start ADC   
  DIGCOG:=cognew( @DigitalLoop, 0)+1                    ' start Digital Cog                
  RDCOG:= cognew( ReadLoop, @Stack )+1                  ' start readloop  

  
pub ReadLoop | n, m, mask, curVal, curTime, pushed, ldbg1, ldbg2, ldbg3, ldbg4, duty, inc


  duty:=0
  inc:=1


  repeat
    ' check buffer.
    Msg.checkKeys    
    ' check push-button
    if not ina & btnMask and not pushed
      pushed~~
      
      Msg.sendKey(String("push"),4,@ExData,0,1)         
      'Msg.sendRepeat( @PushMsg,4, 1 ) 
    if pushed and ina & btnMask
      pushed~     
                                 
    repeat until not lockSet(LockID)
    n:=0
    mask:=1                 
    repeat nAnalogI        
      'if Channels&mask and lastVal[n]
      if lastVal[n]  
        sendVal( tstamp[n], n, lastVal[n]&$FFFF )
        lastVal[n]:=0
      n++
      mask<<=1
      
    lockClr(LockID)

    curVal:=DigIn~
    if curVal                                                       
      sendDig(cnt,curVal&$7FFFFFFF) 

pub sendDig( t,v ) 
 ' send a message   
  ExData[0]:=v   
    ExData[1]:=t 
 ' send a message
  Msg.sendKey(String("d"),1,@ExData,2,1)                        
pub sendVal( t, c, v)
  ExData[0]:=c               '                       chan    value
    ExData[1]:=v             '   00000000,00000000, 000000 00,00000000
  ExData[2]:=t    
 ' send a message
  Msg.sendKey(String("p"),1,@ExData,3,0)  
pub WaitMS(MS)                
  waitcnt(((clkfreq/1000) * MS) +cnt) 'wait for a specified number of MS

DAT
org 0
DigitalLoop
        'set pin directions, start all pins as input 
        rdlong chanDir, pDigDir                 WZ
        call  #setPinDir

:loop   'jmp #:loop
        ' test inputs
              mov       input,  ina             ' read all pins
              and       input,  inMask          ' only check input pins
              cmp       input,  prevInput       WZ
        if_nz call      #readInput            
              rdlong    chanDir,pDigDir
              and       chanDir,MSB             WZ, NR       
        if_nz call      #setPinDir
              rdlong    chanOut,pDigOut         WZ                      
              shl       chanOut,#cDigitalPin    ' read state of output pins
              and       chanOut,outMask        ' only set output pins  
              mov       outa,   chanOut 
                                                 
              jmp   #:loop
' --------------------                              <name:content,rst,rst,rst>
                                                    
readInput                            
              'shr       prevInput,  #cDigitalPin    ' shift back so that pin# -> channel# 
              'wrlong    prevInput,  pDbgX          ' write new value        
              mov       prevInput,input         ' save state 
              shr       input,  #cDigitalPin    ' shift back so that pin# -> channel# 
              or        input,  MSB             ' set MSB       
              wrlong    input,  pDigIn          ' write new value      
              
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


outMask       long      0       ' pin mask for outputs
inMask        long      0       ' pin mask for inputs    
PinMask       long      %00000000111111110000000000000000
'PinMask       long      %00000000000000000111111110000000

input         long      0       ' state of input pins 
chanOut       long      0       'state of output pins 
chanDir       long      0       ' channel directions   
prevInput     long      0       ' previous state of input pins

                                
pDigIn        long      0       ' state of inputs (non-zero when changed)                 
pDigOut       long      0       ' state of outputs 
pDigDir       long      0       ' direction of channels ( MSB set when changed, cleared after read)
pDbgX          long      0


MSB           long      $80000000
Neg1_2        long      -1              


        
DAT
                                                                                                                                                     
org 0 'Reset assembly pointer
DACLoop
        ' reset pin directions
        or    dira,     clk     
        or    dira,     cs  
        or    dira,     din
        or    dira,     #%100000010   
        or    dira,     chAct
        ' update all channels
        mov   curCnt,   cnt
        wrlong MaxLong, pChange 
        call  #ChUpdate
        mov   lastCnt,  #0  
                          
MainLoop
        ' --- update channels if nessesary                                                  
        rdlong    changed,  pChange         WZ    ' read activated channels
        if_nz call      #ChUpdate     
              ' ------ reset all the counters and isntructions
              mov       cmask,  #1                      ' reset t1  
              mov       curCh,  #0                      ' reset t2
              mov       count,  #nAnalogI               ' set channel counter 
              movd   :tcomp,    #DLine  
              movd   :tset,     #DLine
              movs   :tset,     #Delay           
              ' ------ get new clock cycle and store old one,           
              mov       lastCnt,curCnt
              mov       curCnt, cnt
              ' ------ test for clock overflow.   
              cmp       curCnt, Lastcnt         WC 
if_c          mov       CkRoll, chAct  
:chnloop                    
              ' ------- test if channel is active. 
              test      chAct,  cmask           WC      ' test if channel is active
        if_nc jmp       #:chnloop_end                   ' if not activated, skip.
              ' ------- test overflow conditions  
              test      CkRoll, cmask           WC    
              test      DlRoll, cmask           WZ      
if_nc_and_z   jmp       #:tcomp                         ' normal. no overflow      
     ' --- no cnt overflow dline overflow       
if_nc_and_nz  jmp       #:chnloop_end                                
     ' --- cnt overflow not dline overflow 
if_c_and_z    jmp       #:tset                                              
     ' --- cnt overflow and dline overflow.                         
if_c_and_nz   andn      DlRoll, cmask                   ' clear DlRoll
if_c_and_nz   andn      CkRoll, cmask                   ' clear CKRoll
if_c_and_nz   jmp       #:tcomp                         ' proceed! ( not reall usefull considering the next instruction... )
:tcomp        cmp       vxm,  curCnt            WC 
        if_nc jmp       #:chnloop_end
:tset         add       vxm,  vxm               WC
        if_c  or        DlRoll, cmask               
              call      #Act    
              '  setup for next loop
:chnloop_end  shl       cmask,  #1                      ' prepare for next channel
              add       curCh,  #1
              add       :tcomp, hex200
              add       :tset,  hex201   
              djnz      count,  #:chnloop

              
jmp #MainLoop 'loop back
                
'************************************
DbgWait                        

              mov       tx4,    #0
        if_c  or        tx4,    #%10
        if_nz or        tx4,    #%01  
              wrlong    tx4, pdbg2   
              wrlong    curCnt, pdbg3    
              wrlong    cmask, pdbg 

:wait         rdlong    tmp,    pdbg            WZ
        if_nz jmp       #:wait

        ' restore z flag
              test      tx4,    #%01            WZ 
                             
DbgWait_ret       ret              
'************************************

Lock          ' save C flag         
              mov       tmp,    #0           
        if_c  mov       tmp,    #1
              lockset   LockID                  WC
        if_c  jmp       #Lock
              ' restore C flag
              cmp       tmp,    #2              WC    
Lock_ret      ret

Clear     
              lockclr   LockID
Clear_ret     ret
'************************************
ChUpdate
              call      #Lock 
              ' -- reread bitmasks
              rdlong    changed,pChange
              rdlong    chAct,  pChannels    
              ' -- reset instructions
              movd      :get,   #Delay        
              movd      :set,   #Dline         
              movd      :add,   #Dline         
              movs      :add,   #Delay
              ' -- reset counters
              mov       cmask,  #1
              mov       pOff,   #0
              mov       count,  #nAnalogI 
              ' loop for each channel
:loop
              ' -- read in new rate and reset deadline for this channel                       
              mov       tmp,    pRates
              add       tmp,    pOff  
:get          rdlong    vxm,    tmp
:set          mov       vxm,    curCnt                  ' save current time as new deadline
:add          add       vxm,    vxm           WC                  ' add first offset
:loop_end     ' -- increment all counters and instructions
              shl       cmask,  #1
              add       pOff,   #4
                 
              add       :set,   hex200                  ' update destination addr for next loop                 
              add       :get,   hex200                  ' update destination addr for next loop                 
              add       :add,   hex201                  ' update destination addr for next loop
              djnz      count,  #:loop
              ' -- clear changed bit mask. 
              mov       changed,#0
              wrlong    changed,pChange    
              call      #Clear
ChUpdate_ret  ret   
'************************************
Act
              'xor       outa,       cmask
              call      #ReadADC
              ' force non-zero number
      '        cmp       tmpval,#0             wz
      '  if_z  mov       tmpval,#1
              or        tmpval,valSetBit
              
              mov       ts,     cnt             ' cnt only works as source operand.
              mov       tx1,    curCh           ' get address for lastVal
              shl       tx1,    #2
              add       tx1,    plastVal
              mov       tx2,    curCh           ' get address for tstamp
              shl       tx2,    #2
              add       tx2,    ptstamp
              
              call      #Lock                   ' take out lock for shared mem
              wrlong    tmpval, tx1             ' write the value of the ADC to main mem
              wrlong    ts,     tx2             ' write the time stamp as well.
              call      #Clear                  ' return lock
              
Act_ret       ret


ReadADC                                                        
              or        outa,   cs              ' cs high 
              or        dira,   din             ' din high  
              andn      outa,   clk             ' clk low 
         ' start com, send out 5 bits.                
              mov       ADCcount,#5
              mov       tmpval, #$18            ' setup for single mode, with start bit
              or        tmpval, curCh     
              andn      outa,   cs              ' low cs
        
:sendbit
              test      tmpval, #$10    wc      'update DIN/DOUT
              muxc      outa,   din
              shl       tmpval, #1          
              call      #ClkPulse 
              djnz      ADCcount,#:sendbit    
        
              andn      dira,   din           
              call      #ClkPulse ' dummy 
              call      #ClkPulse ' skip null bit

              mov       ADCcount,#12
              xor       tmpVal, tmpval
:readbit                  
              test      din,    ina     WC
              rcl       tmpval, #1              ' store bit for output  
              call      #ClkPulse
              djnz      ADCcount,#:readbit

        
              or        outa,   cs              ' reset cs high.
        
ReadADC_ret   ret

ClkPulse        
              nop 
              nop             
              or       outa,    clk     
              nop
              nop                  
              nop
              nop                
              andn      outa,   clk  
              nop
              nop                 
ClkPulse_ret  ret             
                                
                       'P876543210
chAct         long      0       ' bit mask of activated channels
DLine         long      0[nAnalogI]            ' next deadline for each channel
Delay         long      0[nAnalogI]            ' how much of a delay between deadlines for each channel

changed       long      0       ' bit mask of channels that have changed recently 
count         long      0       ' counter for number of channels   
cmask         long      0       ' bitmask of current channel      
curCh         long      0       ' current channel    
ADCcount      long      0       ' counter for ADC loop
pOff          long      0       ' used in chUpdate and Act while changing the offsets into the arrays,    
' clock control
curCnt        long      0       ' curent clock count
lastCnt       long      0       ' last clock count
DlRoll        long      0       ' mask of channels whose deadline overflowed
CkRoll        long      0       ' mask of channels whose clock overflowed. 

                          
count2        long      0 
cmask2        long      0  

' tmp vars
tmpval        long      0       ' returned value from ReadADC routine.              
ts            long      0       ' used to capture the timestamp while the ADC reads
tmp           long      0       ' temp var, assumed to be invalid after ever 'call'
tx1           long      0       ' temp var, used in Act
tx2           long      0       ' temp var, used in Act
vxm           long      0       ' dummy name for dynamic operands (never actually used, operand overwriten)

' constants larger than $1FF. 
crate         long      $3FF
hex200        long      $200
hex201        long      $201                    
maxlong       long      $FFFFFFFF             
valSetBit     long      $10000  ' bit set when a value is read from ADC 
' values are initialized before cog start.
pChange       long      0
pChannels     long      0
pRates        long      0   
plastval      long      0
ptstamp       long      0
                          
pDbg          long      0    
pDbg2         long      0   
pDbg3         long      0
pDbg4         long      0 
tx4           long      0
tx5           long      0

cs            long      0
clk           long      0
din           long      0

LockID        long      0
fit


dat
Callbackdbg
              org       0
              mov       pLockAddr, par
              mov       exit,   #1
              wrlong    exit,   pLockAddr 
              cogid     MyID    'get the ID of the cog I'm running in  
              cogstop   MyID    'use the ID to terminate myself
               
Callback      
{ does stuff }
              org       0
              ' PAR is the address of pLockAddr.
              mov       pLockAddr,  PAR ' retrieves data locations from passed parameter
              add       pNameAddr,  PAR  
              add       pNameSize,  PAR  
              add       pNameNum ,  PAR 
              add       pNameIdx ,  PAR 
              add       pValAddr,   PAR   
              add       pValSize,   PAR
              add       pValNum,    PAR   
              add       pRetData,   PAR   
              add       pExData,    PAR           
              rdlong    Idx,        pNameNum
              mov       exit,       #0      
              ' get previous value out of the table 
              mov       Data,       Table 
              add       Data,       Idx   ' add 4 times, to increase by longs instead of bytes. ( Data += Idx * 4 )
              add       Data,       Idx
              add       Data,       Idx
              add       Data,       Idx
              rdlong    retv,       Data
              ' test if this val is a number,empty,or string
              rdlong    nArgs,       pValNum
              cmp       nArgs,       #0          WZ      ' true if val is empty      
        if_nz call      #writeNum
              jmp       #check
' ***********************************************
              
Check         ' test for certain keys, in order to act upon changes   
              'call      #LockCB
              call      #Blink
                                                       
              cmp       Idx,    #5              WZ      ' test for "start" 
        if_z  call      #StartChannel                                               
              cmp       Idx,    #6              WZ      ' test for "stop" 
        if_z  call      #StopChannel                               
              cmp       Idx,    #4              WZ      ' test for "set" 
        if_z  call      #SetChan                                          
              cmp       Idx,    #7              WZ      ' test for "dir"
        if_z  call      #SetChanDir                     
              cmp       Idx,    #8              WZ      ' test for "type"
        if_z  call      #TestChanType                   
                                                                                            
              call      #ClearCB
           '  finish -----------     
              jmp       #Done 

Done         
              wrlong    retv,   pRetData
              cmp       exit,   #0              WZ
        if_z  mov       exit,   #1            
              wrlong    exit,   pLockAddr
              cogid     MyID    'get the ID of the cog I'm running in  
              cogstop   MyID    'use the ID to terminate myself

' **************************** SUBROUTINES ***************************
writeNum      rdlong    tmp1,   pExData    
              wrlong    tmp1,   Data
'              mov       exit,   #2
writeNum_ret  ret                   
' -------------------------------

LockCB          ' save C flag
'              mov       tmp1,   #0           
'        if_c  mov       tmp1,   #1
              mov       dira,   #3
              mov       outa,   #3
              call      #Blink
              lockset   LockIDCB                WC
        if_c  jmp       #LockCB
              ' restore C flag
'              cmp       tmp1,   #2              WC  
LockCB_ret    ret

ClearCB     
              lockclr   LockIDCB
ClearCB_ret   ret        
' -------------------------------
getChn        mov       tmp1,   #8                      ' load offset into data table to find channel                           
              add       tmp1,   Table                   ' find address of channel   
              rdlong    tmp1,   tmp1                    ' read channel
              mov       tmp2,   #1
              shl       tmp2,   tmp1                    ' get bit mask for channel

getChn_ret    ret       
' -------------------------------
getIdx        mov       tmp1,   pNameIdx
              rdlong    tmp1,   tmp1
              cmp       tmp1,   Neg1            WZ
        if_z  mov       tmp1,   #0
              
getIdx_ret    ret         
' -------------------------------
SetADCChange          
              call      #getChn                         ' returns the value of channel in tmp1 with a bitmask for channel in tmp2
              rdlong    tmp1, pChangeCB              ' read current changed value
              or        tmp1, tmp2                   ' or it with current channel
              wrlong    tmp1, pChangeCB              ' write it back
              ' dummy test..
              'mov       tmp1,   #$1FF
              'wrlong    tmp1,   pChangeCB
SetADCChange_ret ret      
' -------------------------------
ChangeRate               
                        nop
                        nop
                        nop     

              cmp       nArgs,  #1              WC      ' test if there were arguments. C flag used later  
              rdlong    tmp3,   pExData                 ' load first value of key into memory
                                    
              'call      #getChn
              call      #getIdx                         ' tmp1 = channel
              
              mov       tmp2,   pRatesCB                ' load Rate Table Address
              shl       tmp1,   #2                      ' tmp1 = channel * 4   
              add       tmp2,   tmp1                    ' tmp2 = @Rate[channel]
                 
:loadrate     rdlong    retv,   tmp2   
:write  if_nc wrlong    tmp3,   tmp2                    ' write new rate into the Rates table if there were any args
                           
ChangeRate_ret          ret
                          
' -------------------------------
StartChannel
          
              cmp       nArgs,  #1              WC      ' test if there were arguments. C flag used later
if_c          jmp       #:end

              rdlong    tmp2,   pExData         WZ      ' read first arg as a bitmask. test if 0. 
              rdlong    tmp1,   pChannelsCB             ' read current active channels  
              or        tmp1,   tmp2                    ' or it with current channel   
              wrlong    tmp1,   pChannelsCB             ' write it back
:markchange
              rdlong    tmp1, pChangeCB              ' read current changed value
              or        tmp1, tmp2                   ' or it with current channel
              wrlong    tmp1, pChangeCB              ' write it back
:end          rdlong    retv,   pChannelsCB             ' load pChannelsCB into the return value.                
StartChannel_ret        ret
             
' -------------------------------
StopChannel    

              cmp       nArgs,  #1              WC      ' test if there were arguments. C flag used later
if_c          jmp       #:end

if_nc         rdlong    tmp2,   pExData         WZ      ' read first arg as a bitmask. test if 0.
if_c_or_z     call      #getChn
              rdlong    tmp1,   pChannelsCB             ' read current active channels
              andn      tmp1,   tmp2                    ' and it with inverse of current channel 
              wrlong    tmp1,   pChannelsCB             ' write it back
:markchange
              rdlong    tmp1, pChangeCB              ' read current changed value
              or        tmp1, tmp2                   ' or it with current channel
              wrlong    tmp1, pChangeCB              ' write it back   
:end          rdlong    retv,   pChannelsCB             
StopChannel_ret         ret
TestChanType
              cmp       nArgs,  #1              WZ      ' test if there were arguments.
        if_nz jmp       #TestChanType_ret
              rdlong    tmp1,   pExData 
              mov       tmp3,   #nAnalogI
              mov       tmp4,   #4
              add       tmp4,   pExData
              mov       exit,   #3
              mov       retv,   #2
              
              cmp       tmp1,   tmp3            wc
        if_c  mov       tmp2,   #0
        if_c  wrlong    tmp2,   tmp4 
        if_c  jmp       #TestChanType_ret  
              add       tmp3,   #nAnalogO         
              cmp       tmp1,   tmp3            wc 
        if_c  mov       tmp2,   #1
        if_c  wrlong    tmp2,   tmp4 
        if_c  jmp       #TestChanType_ret
              add       tmp3,   #nDigitals
              cmp       tmp1,   tmp3            wc 
        if_c  mov       tmp2,   #2
        if_c  wrlong    tmp2,   tmp4  
        if_c  jmp       #TestChanType_ret

TestChanType_ret        ret
' -------------------------------
SetChanDir
              cmp       nArgs,  #0              WZ      ' test if there were arguments 
              rdlong    retv,   pDigDirCB
              or        retv,   MSBCB
              xor       retv,   MSBCB           ' clear MSB
              'mov       retv,   #56
                 
        if_z  jmp       #SetChanDir_ret
              cmp       nArgs,  #1              WZ
        if_z  jmp       #:setAllDir
              cmp       nArgs,  #2              WZ
        if_z  jmp       #:setChanDir
              jmp       #SetChanDir_ret

:setChanDir 
              rdlong    tmp1,   pExData
              mov       tmp2,   #4
              add       tmp2,   pExData
              rdlong    tmp2,   tmp2
              ' tmp1 = chan 9 -> n
              ' tmp2 = new dir (zero or non-zero)
              cmp       tmp2,   #0              WZ
                                      
              mov       tmp3,   retv         
              'tmp3 = current chan dirs
              mov       tmp4,   #1
              shl       tmp4,   tmp1
              shr       tmp4,   #nAnalogI
              shr       tmp4,   #nAnalogO  
              ' tmp4 = 1 << (0 -> nDigitals)  
        if_nz or        tmp3,   tmp4
        if_z  xor       tmp4,   tmp4
        if_z  and       tmp3,   tmp4
              ' tmp3 = new chan dirs
              jmp #:write 
:setAllDir
              rdlong    tmp3,   pExData
              ' tmp3 = new chan dirs                    

:write        or        tmp3,   MSBCB           ' set MSB to indicate change
              wrlong    tmp3,   pDigDirCB 
SetChanDir_ret          ret 
' -------------------------------
SetChan   

              cmp       nArgs,  #0              WZ      ' test if there were arguments. C flag used later
        if_z  mov       exit,   #4
        if_z  jmp       #SetChan_ret 
              cmp       nArgs,  #1              WZ      ' test if there was only one argument
        if_z  mov       exit,   #3
        if_z  mov       retv,   #2
             
              rdlong    tmp1,   pExData                 ' load first value of key into memory (chn index)
              mov       tmp2,   #4
              add       tmp2,   pExData                 ' load second value of key into memory (new duty)
              rdlong    tmp2,   tmp2
                                           
              mov       tmp3,   #nAnalogI
              cmp       tmp1,   tmp3            wc 
        if_c  jmp       #:SetAnalogI     
              add       tmp3,   #nAnalogO
              cmp       tmp1,   tmp3            wc 
        if_c  jmp       #:SetAnalogO 
              add       tmp3,   #nDigitals
              cmp       tmp1,   tmp3            wc 
        if_c  jmp       #:SetDigitalO 

:SetAnalogI
              call      #GetSampRate                
if_nz         call      #ChngSampRate
              jmp       #SetChan_ret   
:SetAnalogO
              call      #getDuty
if_nz         call      #ChngDuty
              jmp       #SetChan_ret  
:SetDigitalO
              call      #getDigitalOut
if_nz         call      #SetDigitalOut
              jmp       #SetChan_ret
       
SetChan_ret          ret
                  
' -------------------------------


GetSampRate   ' must leave Z flag, tmp1, and tmp2 intact  
              'tmp1 = chan 0->nAnalogI
              'retv = store rate   
:loadaddr     mov       tmp4,   tmp1
              mov       tmp3,   pRatesCB                ' load Rate Table Address
              shl       tmp1,   #2                      ' tmp1 = channel * 4   
              add       tmp3,   tmp1                    ' tmp3 = @Rate[channel]   
                 
:loadrate     rdlong    tmp1,   tmp3

:saverate     mov       tmp3,   #4
              add       tmp3,   pExData                 
              wrlong    tmp1,   tmp3                    ' write rate to second value
                            
              mov       tmp1,   tmp4 
              'tmp1 = channel 8->9 
GetSampRate_ret         ret
        
' -------------------------------      
GetDuty       ' must leave Z flag, tmp1, and tmp2 intact 
              ' tmp1 = channel 8->9  
              ' retv = store duty
:loadaddr     mov       tmp4,   tmp1            'save
              sub       tmp1,   #nAnalogI       'index from 0
              shl       tmp1,   #2              ' x4
              add       tmp1,   pPwmDatCB       ' calculate table address       
              rdlong    tmp1,   tmp1            ' load real address
              'tmp1 = pData           
                                
              mov       tmp3,   tmp1
              add       tmp3,   #20
:loadduty     rdlong    tmp1,   tmp3            ' save new duty value
:saveduty     mov       tmp3,   #4
              add       tmp3,   pExData         
              wrlong    tmp1,   tmp3            ' write duty to second value
                                 
              mov       tmp1,   tmp4 
              'tmp1 = channel 8->9 
GetDuty_ret   ret
           
' -------------------------------
GetDigitalOut ' must leave Z flag, tmp1, and tmp2 intact 
              ' tmp1 = channel 9 -> n (irrelevant for digitals, sens all pins anyways)
                                              
              mov       tmp4,   tmp1            'save  
              rdlong    tmp3,   pDigOutCB
              'shr       tmp3,   #cDigitalPin
              mov       tmp1,   #4
              add       tmp1,   pExData
              wrlong    tmp3,   tmp1
              mov       tmp1,   tmp4
              ' tmp1 = channel 9 -> n
GetDigitalOut_ret       ret
              
 
' -------------------------------
              
              


ChngSampRate
              'tmp1 = chan 0->nAnalogI
              'tmp2 = new rate 
              mov       tmp3,   pRatesCB                ' load Rate Table Address
              shl       tmp1,   #2                      ' tmp1 = channel * 4   
              add       tmp3,   tmp1                    ' tmp3 = @Rate[channel]
                 
':loadrate     rdlong    retv,   tmp3   
:write        wrlong    tmp2,   tmp3                    ' write new rate into the Rates table if there were any args  

:markchange   shr       tmp1,   #2                      ' shift back to 0->nAnalogI
              mov       tmp2,   #1
              shl       tmp2,   tmp1                    ' make bitmask of channel
              rdlong    tmp1,   pChangeCB               ' read current changed value
              or        tmp1,   tmp2                    ' or it with current channel
              wrlong    tmp1,   pChangeCB               ' write it back
               
ChngSampRate_ret        ret
   
' -------------------------------

ChngDuty
              ' tmp2 = new duty
              ' tmp1 = channel 8->9
              sub       tmp1,   #nAnalogI       'index from 0
              shl       tmp1,   #2              ' x4
              add       tmp1,   pPwmDatCB       ' calculate table address       
              rdlong    tmp1,   tmp1            ' load real address
              'tmp1 = pData
                                
              mov       tmp3,   tmp1
              add       tmp3,   #24
              wrlong    tmp2,   tmp3            ' save new duty value
              
              mov       tmp3,   tmp1
              add       tmp3,   #20
              'tmp3 = pPeriod
              rdlong    tmp3,   tmp3
              'tmp3 = period
                                
                             
                                                                        

              mov       maxT,    tmp3
              sub       maxT,    minOff
              sub       maxT,    minOn

              mov       maxDuty, tmp3
              sub       maxDuty, minOff
                                   
              cmp       tmp2,   minOn           wz, wc 
        if_z  mov       onTime, #1 
              nop                      ' <--- WHY?!
        if_z  mov       OffTime,MaxT
        if_z  jmp       #:done   

        if_c  mov       onTime, #0
        if_c  mov       offtime,maxT
        if_c  add       offTime,#1
        if_c  jmp       #:done  
                                    
              
              cmp       tmp2,   maxDuty         wz, wc
        if_z  mov       onTime, maxT
        if_z  mov       offTime,#1 
        if_z  jmp       #:done

if_nz_and_nc  mov       onTime, maxT
if_nz_and_nc  add       onTime, #1
if_nz_and_nc  mov       offTime,#0
if_nz_and_nc  jmp       #:done

              mov       OnTime, tmp2
              sub       OnTime, minOn

              mov       OffTime,tmp3
              sub       OffTime,tmp2
              sub       OffTime,minOff 
              jmp       #:done
     
:done

              mov       Times,  OffTime
              shl       Times,  #16
              or        Times,  OnTime  
              mov       tmp3,   tmp1
              add       tmp3,   #16
              wrlong    Times,  tmp3            ' save Times

              mov       tmp3,   tmp1
              add       tmp3,   #4
              wrlong    OnTime, tmp3            ' save OnTime  
              mov       tmp3,   tmp1
              add       tmp3,   #8
              wrlong    OffTime,tmp3            ' save OffTime
              mov       tmp3,   tmp1
              add       tmp3,   #12
              mov       tmp2,   #1
              wrlong    tmp2,   tmp3              ' set changed
                                       
ChngDuty_ret  ret
' -------------------------------
SetDigitalOut 'tmp1 = chan 6 -> n 
              'tmp2 = new level ( zero, nonzero, or bitmask if MSB is set )

              mov       tmp4,   tmp2   
              and       tmp4,   MSBCB
              cmp       tmp4,   #0              WZ      ' test MSB 

'              jmp       #:setchan 
        if_z  jmp       #:setchan
        if_nz jmp       #:setall
        
:setall       'shl       tmp2,   #cDigitalPin
              wrlong    tmp2,   pDigOutCB
              jmp       SetDigitalOut_ret
                   
:setchan               
              sub       tmp1,   #nAnalogI
              sub       tmp1,   #nAnalogO
              mov       tmp3,   #1
              shl       tmp3,   tmp1       
              ' tmp3 = 1 << (0 -> nDigitals)

                                    
              rdlong    tmp4,   pDigOutCB
              'mov       tmp1,   Neg1
              'xor       tmp3,   tmp1    
              or        tmp4,   tmp3            ' clear selected bit (xxxxx & 11011 = xx0xx)
              xor       tmp4,   tmp3
              'xor       tmp3,   tmp1                                          
              cmp       tmp2,   #0              WZ                            
        if_nz or        tmp4,   tmp3                 
              wrlong    tmp4,   pDigOutCB  
              
              jmp       SetDigitalOut_ret  
                                     
SetDigitalOut_ret       ret
' **********************
Blink       

              mov       dira,   PinCB
:loop                             
              mov       TimeCB,   cnt
              add       TimeCB,   #9
              xor       outa,   PinCB
              mov       tmp1,   DelayCB
              call      #WaitD
              'xor       outa,   PinCB   
              'waitcnt   TimeCB,   DelayCB     
              
Blink_ret     ret

WaitD         djnz tmp1,        #WaitD
WaitD_ret     ret
                          
 

Neg1          long -1
MSBCB         long  $80000000    
PinCB         long  |<1
DelayCB       long  1000000
TimeCB        long 0   
              
                                                                                                
MyID          long 0

pLockAddr     long 0  
pNameAddr     long 4 
pNameSize     long 8   
pNameNum      long 12   
pNameIdx      long 16    
pValAddr      long 20 
pValSize      long 24
pValNum       long 28   
pRetData      long 32
pExData       long 36
                     
Table         long 0 
Idx           long 0
Data          long 0
nArgs         long 0
                       
pChangeCB     long 0
pChannelsCB   long 0
pRatesCB      long 0
pOffsetsCB    long 0

pPwmExecCB    long 0
pPwmDatCB     long 0
LockIDCB      long 0

           
pDigInCB      long 0
pDigOutCB     long 0
pDigDirCB     long 0    

tmp1          long 0 
tmp2          long 0
tmp3          long 0
tmp4          long 0
tmp5          long 0
exit          long 0
retv          long 0

' PWM vars
               
maxDuty long  0
maxT    long  0
minOn   long  4
minOff  long  11 
OnTime  long  0
OffTime long  0
Times   long  0  