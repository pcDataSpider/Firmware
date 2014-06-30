Con

  { ==[ CLOCK SET ]== }       
  _CLKMODE      = XTAL1 + PLL16X
  _XINFREQ      = 5_000_000                          ' 5MHz Crystal
              
  nChannels     = 14   ' cant exceed 32.
  nAnalogI      = 4    
  nAnalogO      = 2 
  nDigitals     = 8

  cDigitalPin   = 16   ' first digital pin#                              

  SyncDelay = 80000000
  INITAVG = 1 ' initial value for nAvg. number of ADC reads to average per sample
  EX_DAT_LEN = 50   
  ADCBUFFERLEN = 200    ' number of values a buffer will hold
  ADCBUFFERBYTELEN = ADCBUFFERLEN * 2 * 4     ' number of bytes in a buffer
  ERR           = 200  ' largest possible variance in tstamp of ADC data, and ideal sample rate, in clock cycles.
  btnMask       = %10000000

  DPIN1 = 16
  DPIN2 = 17
  DPIN3 = 18
  DPIN4 = 19        
obj
  Msg       : "Messager"
  ExecCB    : "MessageHandler"
  Stk       : "Stack Length"           'Include Stack Length Object
  pwm1      : "AsmPwm"                                  
  pwm2      : "AsmPwm"
  Events    : "EventLoop"
 ' Streams   : "StreamData"  

var              
  long Stack[800] 
  long Stack2[800]  
  byte RDCOG   
  byte ADCCOG  
  byte DIGCOG
  byte DATCOG
  byte MSGCOG
  word bitsLeft              

dat                                           
        ExData          long    0[EX_DAT_LEN]
dat     ' channel related data
        org 0
        ' these values define different channels
        Change          long 0                          ' bitmask for ADC channels whose parameters have changed. 1=change 0=no change
        Channels        long %0000000000                ' bitmask for active channels. 1=on 0=off  
        Rates           long 80000[nAnalogI]            ' sample rates for channels in clocks per sample
        chanFreqs       long 0                          ' Bitmask for each ADC channel mode (high/low freq)
        clearBuffer     long 0                          ' bitmask for ADC channels that need to clear their buffer.  
        nAvg            long INITAVG                    ' number of ADC reads to average for one sample
   
        ' ADC buffer and buffer data
        ADCBuffers      long 0[nAnalogI*ADCBUFFERLEN*2] ' time stamp of the last value read
        curADCIdx       long 0[nAnalogI]                ' current position into the buffer of next val to read
        lastTstamp      long 0[nAnalogI]                ' last tStamp from the ADC
        streamRate      long 0[nAnalogI]                ' the samplerate for the current stream
        'pwm data
        pwmExec         long 0[nAnalogO]
        pwmData         long 0[nAnalogO]
        'stream data
        curStream       byte                            ' current OPEN stream index.

        'Digitals data
        DigDir          long %00001111                  ' bitmask for channel directions (MSB set when changed)
        DigOut          long %00000000                  ' state of output channels 
        DigIn           long 0                  ' state of input channels (MSB set when changed)

        EvtMsg          long 0
        'Sync deadlines
        nextSync        long 0
        lastSync        long 0

        'Debug names                        
        dbg1            long 0 
        dbg2            long 0  
        dbg3            long 0 
        dbg4            long 0
pub Main | n, i
    
  Stk.Init(@Stack, 80)                  'Initialize reserved Stack space (reserved below)
  i:=0
  repeat nAnalogI  
    ADCBuffPos[i] := @ADCBuffers + (i * ADCBUFFERLEN*2)*4   
    ADCBuffMax[i] := @ADCBuffers + ((i+1) * ADCBUFFERLEN*2)*4
    curADCIdx[i] := i * ADCBUFFERLEN*2
    i++         

  ' get PWM data                    
  pwmExec[0]:= pwm1.getChngAddr   
  pwmExec[1]:= pwm2.getChngAddr 
  pwmData[0]:= pwm1.getDataAddr 
  pwmData[1]:= pwm2.getDataAddr 
                        

' init data for ADC reader 
  pChange   := @Change
  pChannels := @channels 
  pRates    := @Rates
  pnAvg     := @nAvg                   
  pDbg1     := @dbg1   
  pDbg2     := @dbg2   
  pDbg3     := @dbg3
  pDbg4     := @dbg4
  ADClen    := (ADCBUFFERLEN*2*4)
  cs:= %00010000 ' pin bitmasks   '   %00010000
  din:=%00100000                 '   %00100000
  clk:=%01000000                 '   %01000000


  
  ' init data for Digital Cog
  'pDigIn  := @DigIn
  'pDigOut := @DigOut
  'pDigDir := @DigDir
  'pDbgX   := @Dbg1



                                       
  ExecCB.InitData(nAnalogI, nAnalogO, @nAvg, @pwmData, @pwmExec, 1015, @Change, @Channels, @Rates, @DigOut, @DigIn, @DigDir, @ChanFreqs, @clearBuffer, @EvtMsg)        
  'Streams.Init                                        

  MSGCOG:=Msg.start
  if MSGCOG==0
    repeat
      Msg.str(string("Failed to open Com")) ' lol, should this happen com wont work anyways... 
    Msg.stop
    return 0
              
   
  ' create locks 
  LockID:=LockNew
  if LockID==-1
    repeat                            
      Msg.Str(String("<Failed:locks>"))
    Msg.stop 
    return 0
                
  'Msg.Pause                     ' pause for user to start it!

  lastSync:=cnt
  nextSync:=lastSync+syncDelay
                           
  'pwm1.start( %0111, 1015) 
  'pwm1.changePwmAsm(000) 
  'pwm2.start( %1000, 1015)
  'pwm2.changePwmAsm(000)                                            
  ExecCB.startEvents
  ExecCB.startWave
  'DIGCOG:=cognew( @EventCog, 0)+1                    ' start Digital Cog                
  RDCOG:= cognew( ReadLoop, @Stack )+1                  ' start readloop (handles all supervisory code)
  DATCOG:=cognew( DataLoop, @Stack2 )+1                 ' start ADC AND the data loop (handles processing data)                                                                      

  WAITMS(10000)

pub DataLoop | i, n, tmpByte,sendData, ch, beg, valCount, val, time, lastTime,AvgCnt, addr, len, curRate, bitsLeft2,chk
  ' wait until a cog is available to start the ADC.
    

     ' start ADC 
  repeat  until ADCCOG > 0 
    ADCCOG:=cognew(@ADCloop, 0 )+1                       ' start ADC
  ExecCB.setADCCOG(ADCCOG, @ADCloop) 

  ch:=0
  repeat nAnalogI  
    curADCIdx[ch] := ch * ADCBUFFERLEN*2
    ch++
    
  repeat
    ch := 0
    repeat nAnalogI       
      'read in as much data as possible from this channel
      curRate:=streamRate[ch] 
      lastTime:=lastTstamp[ch]
      beg:=curADCIdx[ch]
        
      i:=beg
      valCount:=0
      chk:=9999 'chk should be reset when a new packet starts                         
      if clearBuffer&(1<<ch)<>0
        repeat while ADCBuffers[ i+1 ] <> 0
          valCount+=1          
          ' move to the next long
          lastTime:=time          
          i+=2                   
          if i => (ch+1) * ADCBUFFERLEN*2             
            i:= ch * ADCBUFFERLEN*2   
            longfill( @ADCBuffers + (beg*4), 0, valCount*2)
            beg:=i
            valCount:=0 
            
        longfill( @ADCBuffers + (beg*4), 0, valCount*2) 
        clearBuffer&=(!(1<<ch))
        
      repeat while ADCBuffers[ i+1 ] <> 0
          val:=ADCBuffers[ i ]
          AvgCnt:=val>>25
          val:=(val&$00FFFFFF) / AvgCnt  
          time:= ADCBuffers[ i + 1 ]
  
          if not ChanFreqs & (1<<ch)
            'LowFreq mode..
            'make sure no streams are open
            CloseStream(curstream+1,chk,lastTime)
            SendPoint(time, val | (ch<<12) )
          else
            ' test if this is the first byte in the stream.
            if !isStreamOpen(ch)   
              streamRate[ch]:= Rates[ch]*nAvg
              curRate:=streamRate[ch] 
              bitsLeft:=openStream(ch)
              chk:=0
              lastTime:=time-curRate 'reset lastTime

              'transmit data  ( add curRate and time to stream. both 32bit values.
              chk:=sendBits(curRate, 32, chk)
              chk:=sendBits(time, 32, chk)
              
              beg:=i 'rest beginning to point here
              valCount:=0 'no data yet. 
            ' test for non consecutive values
            tmpByte:=time-lastTime
            if (tmpByte>curRate+ERR or tmpByte<curRate-ERR) 'time to send. Either buffer is full, or reached a non-consecutive value
              longfill( @ADCBuffers + (beg*4), 0, valCount*2) 'clear out values we have read in. 
              closeStream(ch,chk,lastTime)
              beg:=i
              valCount:=0  
              next ' just bail out of this loop iteration

            {  
            'add this value and move on.
            if bitsLeft 'if there are bits already, read in 4 bits, send, then read the rest
              tmp:= bitsLeft | val>>8 'send 4 MSB's (12-4=8)
              Msg.txData(tmp)  
              chk:=Msg.checksum(chk,tmp)
              Msg.txData(val)         'send 8 LSB's
              chk:=Msg.checksum(chk,val)
              bitsLeft:=0
            else
              tmp:=val>>4             'send 8 MSB's (12-8=4)
              Msg.txData(tmp)  
              chk:=Msg.checksum(chk,tmp)
              bitsLeft:=((val)<<4)|%100000000  'add 4 LSB's to bitsLeft, with added higher order bits  
            }
          chk:=sendBits(val, 12, chk)
          valCount+=1          
          ' move to the next long
          lastTime:=time          
          i+=2                   
          if i => (ch+1) * ADCBUFFERLEN*2             
            i:= ch * ADCBUFFERLEN*2   
            quit 'give up time to OTHER channels when the buffer loops around.  
                          

      longfill( @ADCBuffers + (beg*4), 0, valCount*2) 
      curADCIdx[ch]:=i
      lastTstamp[ch]:=lastTime
      'close stream if it is currently open.
      closeStream(ch,chk,lastTime)
      ch++
      

pri sendBits(sendData, N, chk) | tmpByte
{{ sendData = value to send
 N=number of bits in sendData (must be multiple of 4)
 chk = current checksum}}

repeat until N<8
  if bitsLeft 
    N:=N-4 'only work in increments of 4 bits to make things simpler 
    tmpByte:= bitsLeft | sendData>>N 'add 4 bits to bitsLeft
    chk:=Msg.txData(tmpByte,chk)  
    bitsLeft:=0  
  else
    N:=N-8 'send a whole byte
    tmpByte:=sendData>>N   
    chk:=Msg.txData(tmpByte,chk)
if N>0       'add the last 4 bits
  bitsLeft:=((sendData&15)<<4)| %1000000000 'add higher bits to signify it has data   
return chk 
pri openStream(ch) | blah
if curStream
  closeStream(curStream-1,0,0) 'close a bad stream. log an error or something
  ExData[0]:=9999  
  Msg.debugmsg(String("Closing bad stream?"))
  Msg.sendControl(1,@ExData,1)
if ch>3
  ch:=3  
curStream:=ch+1               
Msg.Lock    'take lock for the duration that this stream is open. (will close at the end of a stream packet)
bitsLeft := ((%1000 | ch)<<4)  |%1000000000 'add higher bits to signify it has data   
return bitsLeft
pri closeStream(ch,chk,lastVal) | blah
if curStream<>ch+1 'only close if already open. otherwise we could clear a lock we didnt take.
  return                            
chk:=sendBits(lastVal, 32, chk)
if bitsLeft
  chk:=Msg.txData(bitsLeft,chk)
Msg.txEOP             
Msg.char(chk) 
Msg.Clear
curStream:=0
return
pri isStreamOpen(ch)
if curStream == ch+1
  return true
else
  return false
pub ReadLoop | n, m, mask, curVal, curTime, pushed, ldbg1, ldbg2, ldbg3, ldbg4, duty, inc


  duty:=0                                                 
  inc:=1


  repeat
    ' check buffer.
    Msg.checkKeys            
    'test for hanshake timing
    CurCnt:=cnt
    if CurCnt>nextSync and (nextSync>lastSync or CurCnt<lastSync)
      sendSync
    'test digitals
    curVal:=DigIn~
    if curVal                                                       
      sendDig(cnt,curVal&$7FFFFFFF)
    curVal:=EvtMsg~
    if curVal
      Msg.sendControl(curVal,@ExData,0)

    
pub sendSync
  ExData[0]:=cnt
  if nextSync < lastSync
    ExData[1]:=1
    Msg.sendControl(13,@ExData,2)
  else
    Msg.sendControl(13,@ExData,1)
  lastSync:=nextSync
  nextSync:=nextSync+SyncDelay
pub sendDig( t,v ) 
 ' send a message   
  ExData[0]:=v   
    ExData[1]:=t 
 ' send a message
  Msg.sendControl(10,@ExData,2)                        

pub sendPoint(t,v )   
 ' send a message   
  ExData[0]:=v   
  ExData[1]:=t     
  ExData[2]:=t 
 ' send a message
  Msg.sendControl(12,@ExData,3)          
pub WaitMS(MS)                
  waitcnt(((clkfreq/1000) * MS) +cnt) 'wait for a specified number of MS

pub TogglePin(Pin)
 {{toggles the debugging pin to check with a scope/LED}} 
 !outa[Pin]
{
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
' --------------------                              
                                                    
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


 }       
DAT
                                                                                                                                                     
org 0 'Reset assembly pointer
ADCLoop
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
              wrlong    cmask, pdbg1

:wait         rdlong    tmp,    pdbg1           WZ
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
              ' -- reread nAvg
              rdlong    ADCnAvg,    pnAvg           WZ
        if_z  mov       ADCnAvg,    #1  
              ' -- reset instructions
              movd      :get,   #Delay        
              movd      :set,   #Dline         
              movd      :add,   #Dline         
              movs      :add,   #Delay
              movd      :resetSum, #ADCAvg
              movd      :resetAvg, #ADCAvgCnt  
              ' -- reset counters
              mov       cmask,  #1
              mov       pOff,   #0
              mov       count,  #nAnalogI 
              ' loop for each channel
:loop
              ' -- read in new rate and reset deadline + averages for this channel                       
              mov       tmp,    pRates
              add       tmp,    pOff  
:get          rdlong    vxm,    tmp
:set          mov       vxm,    curCnt                  ' save current time as new deadline
:add          add       vxm,    vxm           WC                  ' add first offset
              mov       tmp,    #0
:resetSum     mov       vxm,    tmp
              mov       tmp,    ADCnAvg
:resetAvg     mov       vxm,    tmp

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
              call      #ReadADC
:avgval
              mov tx2,  curCh
              mov tx1,  tx2                             
              add tx1,  #ADCAvg
              add tx2,  #ADCAvgCnt
              movd      :addval,tx1
              movd      :inccnt,tx2   
              
:addval       add       vxm,    tmpval          ' add this value into the average
:inccnt       sub       vxm,    #1         WZ   ' add 1 to the current avg count.
if_nz         jmp       #:done                  ' jump if not enough values to average
                                      
              movd      :resetsum,tx1
              movd      :resetcnt,tx2
              movd      :addAvgCnt, tx1
              movd      :wrVal, tx1  
:resetcnt     mov       vxm,    ADCnAvg
              mov       tx3,    ADCnAvg
              shl       tx3,    #25
:addAvgCnt    or        vxm,    tx3

:saveval
              mov       ts,     cnt             ' tmp var needed, as cnt only works as source operand
              cmp       ts,     #0              WZ' make sure timestamp is not zero
        if_z  mov       ts,     #1
              mov       tx2,    curCh           ' start calculating address into current buffer..

              mov       tx1,    tx2
              add       tx2,    #ADCBuffPos              ' add 4 to get the current offset
              movs      :rdPos, tx2
              movd      :wrPos, tx2  
              add       tx1,    #ADCBuffMax              ' add 4 to get the current offset
              movs      :rdMax, tx1
                                      
:rdPos        mov       tx2,    vxm             ' read in the current position in the buffer
:rdMax        mov       tx1,    vxm             ' read in the maximum position of the current buffer
              rdlong    tx3,    tx2             WZ' read in the next value to tset for zero      
:wrVal  if_z  wrlong    vxm, tx2             ' write ADC data(the avg sum) into the buffer
        if_z  add       tx2,    #4
        if_z  wrlong    ts,     tx2             ' write timestamp into the buffer
        if_z  add       tx2,    #4
        if_z  cmp       tx2,    tx1             WC
 if_z_and_nc  sub       tx2,    ADCLen
                     
:wrPos        mov       vxm,    tx2    
:resetsum     mov       vxm,    #0

:done    
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
              
ADCBuffPos    long      0[nAnalogI]
ADCBuffMax    long      0[nAnalogI]

ADCAvg        long      0[nAnalogI]  ' sum of samples so far
ADCAvgCnt     long      INITAVG[nAnalogI]  ' number of samples averaged so far (counts down)
ADCnAvg       long      INITAVG      ' number of ADC reads to average per sample

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
tx3           long      0       ' temp var, used in Act

vxm           long      0       ' dummy name for dynamic operands (never actually used, operand overwriten)

' constants larger than $1FF. 
crate         long      $3FF
hex200        long      $200
hex201        long      $201                    
maxlong       long      $FFFFFFFF             
valSetBit     long      $10000  ' bit set when a value is read from ADC
ADCLen        long      $0      ' the number of bytes in the global buffer        
' values are initialized before cog start.
pChange       long      0
pChannels     long      0
pRates        long      0
pnAvg         long      0
                          
pDbg1         long      0    
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


 