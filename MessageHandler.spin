Con
                              
  VERSION       = 200 
  nAnalogI_CON = 18 'used to setup an array for calculateBandwidth. Higher is fine.

  MAX_BANDWIDTH = 17000  'maximum bandiwdth in bytes per second. 
  MIN_BANDWIDTH = 100 'minimum bandwidth a channel must use to change to HighFreq mode. (mul. by num. samples avg.) Ensures low frequencies use lowfreq mode.

  'MIN_ADC_PERIOD = 1500 ' minimum period for ADC to handle properly
  MIN_ADC_PERIOD = 1 ' ignore super fast periods
  
  cminOn  = 4           ' constants for PWM duty calculations
  cminOff = 16                   

  DPIN1 = 16 'blue
  DPIN2 = 17
  DPIN3 = 18
  DPIN4 = 19 'green

       
obj
  wav           : "WaveGen"
  Events        : "EventLoop"

var
        byte WAVCOG         

dat
        nAnalogI        long    0
        nAnalogO        long    0
        nAvg            long    0
        pnAvg           long    0
        ADCCOG          long    0
        pADCloop        long    0         
        pPwmData        long    0  
        pPwmExec        long    0
        pwmPeriod       long    0

        pChange         long 0                ' bitmask for ADC channels whose parameters have changed. 1=change 0=no change
        pChannels       long 0                ' bitmask for active channels. 1=on 0=off  
        pRates          long 0                ' sample rates for channels in clocks per sample
        pDigOut         long 0
        pDigDir         long 0
        pChanFreqs      long 0
        pClearBuffer    long 0                  ' bitmask of ADC channels that need to clear their remaining data buffer.

        rateArray     long 0[nAnalogI_CON]     ' array to store rates for channels during CalculateBandwidth.
pub startWave
  {{ Object Instance specific Init method }}
  WAVCOG:=wav.start(  long[pPwmData+0], long[pPwmData+4], pwmPeriod )
  wav.chngDig(000, 00, %00000000000011110000000000000000)
  wav.chngWav(1, 0, 1000, 1)
  wav.chngWav(1, 0, 1000, 2)
  return WAVCOG
pub startEvents
  Events.start
pub InitData(nAnalogI2, nAnalogO2,nAvgAddr, pwmDataAddr, pwmExecAddr, period, ChangeAddr, ChannelsAddr, RatesAddr, DigOutAddr, DigInAddr, DigDirAddr, ChanFreqsAddr, ClearBufferAddr, EvtMsgAddr)
  {{ Defines data that is shared among all object instances using DAT blocks }}

  nAnalogI:=nAnalogI2
  nAnalogO:=nAnalogO2
  pnAvg:=nAvgAddr      
  nAvg:=long[nAvgAddr]   
  pPwmData:=pwmDataAddr
  pPwmExec:=pwmExecAddr
  pwmPeriod:=period
  pChange:=ChangeAddr
  pChannels:=ChannelsAddr
  pRates:=RatesAddr
  pDigOut:=DigOutAddr
  pDigDir:=DigDirAddr
  pChanFreqs:=ChanFreqsAddr
  pClearBuffer:=ClearBufferAddr

  Events.init(EvtMsgAddr, DigDirAddr, DigOutAddr, DigInAddr, ChannelsAddr, ChangeAddr)

pub setADCCOG(ADCCOGvar, ADCloopaddr)
  ADCCOG:=ADCCOGvar            
  pADCloop:=ADCloopaddr
  
pub Exec(NameNum, pExData, ValNum) : retV |chn,offset,val,sendRate
  {{ Handle parsed messages }}
  dira[DPIN1]~~
  dira[DPIN2]~~
  dira[DPIN3]~~
  dira[DPIN4]~~
  
  togglepin(DPIN1)  
  waitms(100)     
  togglepin(DPIN1)
  waitms(100)     
  
  retV:=-1
  case NameNum
    0: ' talk message. recieving this is a mistake
    1: ' over message. host is done talking, switch back to half-duplex
    2: ' "Bad"  resend waiting buffer starting with packet indicated { packet# }
    3: ' "version" message. send the current version 
      long[pExData+0] := 1    
      long[pExData+4] := VERSION    
      retV:=3
    4: ' "Start"  { channels to start as bitmask }
      ClearBuffers(long[pExData+0])                                             
      long[pChannels] := long[pChannels] | long[pExData+0]
      long[pChange]   |= long[pExData+0]
                                                       
      retV:=1    
      long[pExData+0] := 3     
      long[pExData+4]:=CalculateBandwidth 
      long[pExData+8]:=long[pChanFreqs]     
      long[pExData+12]:=long[pnAvg]      
    5: ' "Stop"  { channels to stop as bitmask }
      long[pChannels] := long[pChannels] & (!long[pExData+0]) 
      long[pChange]   |= long[pExData+0]
        retV:=1    
        long[pExData+0] := 3      
        long[pExData+4]:=CalculateBandwidth 
        long[pExData+8]:=long[pChanFreqs]     
        long[pExData+12]:=long[pnAvg]      
    6: ' "Set" {channel #, new value}
      chn := long[pExData+0]
      offset := chn * 4
      val := long[pExData+4]
      if chn < nAnalogI    ' set analog input channel
        if val<1 'can't handle zero
          val:=1

        {
        if (val/nAvg) < MIN_ADC_PERIOD
          nAvg:=val/MIN_ADC_PERIOD
          if nAvg < 1
            nAvg:=1
          ' write changes to shared mem    
          long[pnAvg]:=nAvg  
          long[pChange]|=255
        }
          
        long[pRates + offset] := val/nAvg
        long[pChange]  |= 1<<chn                                            
        ClearBuffers(1<<chn)                                             
        retV:=1    
        long[pExData+0] := 4     
        long[pExData+4]:=CalculateBandwidth 
        long[pExData+8]:=long[pChanFreqs]       
        long[pExData+12]:=long[pnAvg]            
        long[pExData+16]:=long[pRates + offset]
      elseif chn < nAnalogO+nAnalogI ' set analog output channel
        changePwm(chn, val)               
      else                 ' set digital channel
        long[pDigOut]:= $80000000 | val  
        
        
    7: ' "Direction" recieving this is a mistake. {bitmask of directions}
    8: ' "Query" {channel to querry}
      'send all information on requested channels.
      chn:=long[pExData+0]        
      retV:=9
      long[pExData+0] := 3 
      long[pExData+4] := chn  
      if chn < nAnalogI
        offset:=chn*4
        long[pExData+8] := long[pRates+offset] * nAvg
        long[pExData+12]:= long[pChannels] 
      elseif chn < nAnalogI + nAnalogO  'test analog outputs
        offset:=(chn-nAnalogI)*4
        long[pExData+8] := long[long[pPwmData+offset] + 24]
        long[pExData+12]:= long[long[pPwmData+offset] + 20]
      else        
        long[pExData+8] := long[pDigOut] & 2147483647
        long[pExData+12]:= long[pDigDir] 
      
        
    9: ' "Info" recieving this is a mistake.{channel#, value, start/stop/direction}
    10: ' "Digital" recieving this is a mistake. {pinout bitmask, timestamp}
    11: ' wav message  {wavetype, period, duty/amp, pins/pwm#}
      if Long[pExData + 0] == 0
       wav.chngDig( Long[pExData + 4], Long[pExData + 8],Long[pExData + 12])
      else
        wav.chngWav( Long[pExData + 0],Long[pExData + 4], Long[pExData + 8], Long[pExData + 12])        
    12: ' point message {value, timestamp}
    13: ' sync message {timestamp, rollovers} 
    14: 'avg message. changes the sample averaging. {new nAvg}
      val:=long[pExData + 0] ' get new average 
      if val < 1 ' can't average less than 1 point at a time!
        val:=1
      ' recalculate rate for a new nAverage value
      chn:=0    
      repeat until chn == nAnalogI       
        sendRate:=long[pRates +  (chn*4)] * nAvg
        {if sendRate/val < MIN_ADC_PERIOD and val <> 1 
          val:= sendRate/MIN_ADC_PERIOD
          if val < 1
            val:=1
          retV:=1    
          long[pExData+0]:= 4     
          long[pExData+4]:=CalculateBandwidth 
          long[pExData+8]:=long[pChanFreqs]      
          long[pExData+12]:=long[pnAvg]      
          long[pExData+16]:=42
          chn:=0
        else}                             
          long[pRates +  (chn*4)]:= sendRate / val
          chn+=1        
      ' write changes to shared mem                    
      nAvg:=val            
      long[pnAvg]:=nAvg 
      CalculateBandwidth 
      long[pChange]|=$FF

                                                   
        retV:=1    
        long[pExData+0] := 1                    
        long[pExData+4]:=long[pnAvg] 
      
    15: ' setEventTimer message SetTimer(Timer#, Delay)                                
      Events.setTimer(long[pExData + 0],long[pExData + 4])
    16: ' addEvent message  AddEvent(Condition, ConditionParam, Action, ActionParam)                               
      Events.AddEvent(long[pExData + 0],long[pExData + 4],long[pExData + 8],long[pExData + 12])
      offset:=16
      repeat (ValNum-4)/2
          Events.AddEvent(-1,0,long[pExData + offset],long[pExData + offset+4])
'          Events.AddEvent(long[pExData + 0],long[pExData + 4],long[pExData + offset],long[pExData + offset+4])
          offset+=8          
    17: ' resetEvents message
      Events.Reset
    18: ' enableTimer message
      Events.trigger(long[pExData + 0])
  return retV
pub ClearBuffers(chans) | oldChannels
{{Waits for the channels specified in a bitmask to clear any data in their buffers.
Calling this will temporarily stop any reads from going on for the specified channel.}}

' first we need to temporarily stop this channel.
oldChannels := long[pChannels]
long[pChannels] := long[pChannels] & (!chans) 
long[pChange]   |= chans

'now let the DataLoop know to empty those buffers
long[pClearBuffer] := chans

'wait until all buffers are cleared again.
repeat until long[pClearBuffer]==0

' now lets restore any channels that were running.
long[pChannels] := oldChannels
long[pChange] |= chans

pub ChangePwm(ch, val) |offset, On, OnTime, OffTime, period
  { calculate on/off time for given duty
    min = 4
    max = period-27 }

    offset:=(ch-4)*4
    period:=long[long[pPwmData+offset]+20]
    
    
  On := (period * val)/period 
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
                             
  long[long[pPwmData+offset]+4]:=OnTime 
  long[long[pPwmData+offset]+8]:=OffTime
  
  long[long[pPwmData+offset]+12]:=1                    
  long[long[pPwmData+offset]+16]:=(OffTime<<16) | OnTime  
              

  

pub CalculateBandwidth | retV, totalBandwidth, ch, ChanFreqs, rate, LargestRate, LargestCh
{{ Calculates the approxuimate bandwidth for transmitting all the ADC channels.
 Adjusts low/high frequency channels to use different protocols according to bandwidth requirments
 Calling this function sets the global pChanFreqs variable}}
  
  totalBandwidth:=0 ' total bandwidth used so far.
  ch:=0
  retV:=0
  ChanFreqs:=0 ' holds information on what protocol each channel uses. bitmask, 0=low freq, 1= high freq.
  ' assumes every channel will use low frequency method, until MAX_BANDWIDTH is reached.
  ' The goal is to use as much bandwidth as possible, WITHOUT going over MAX_BANDWIDTH.
  ' MAX_BANDWIDTH should allow some room for other comunications.

  ' calculate initial bandwidth for ALL lowFreq mode.
  repeat nAnalogI     
    if not long[pChannels] & (1<<ch)
      rate:=0           
    else
      rate:=long[pRates +  (ch*4)] * nAvg
      rate:= (CLKFREQ/rate)  * (3 + 32 + 32 + 1) 
    rateArray[ch]:=rate 
    'rate is Bytes per Second    
    totalBandwidth+=rate 
    ch+=1

retV:=totalbandwidth


  'incrementally change channels to HighFreq mode if bandwidth is too high
  if totalBandwidth > MAX_BANDWIDTH
    ' switch higher freqeuncy channels to HighFreq mode.
    repeat nAnalogI   ' don't change more than nAnalogI channels.
      ' find largest sample rate.
      LargestRate:=0
      LargestCh:=-1
      ch:=0
      repeat nAnalogI       ' find the fastest LOW FREQ channel.
        if (rateArray[ch] > LargestRate)
            LargestRate:=rateArray[ch]
            largestCh:=ch
        ch++
      if LargestCh == -1 or LargestRate == 0 ' all channels already in highfreq mode. exit
        ChanFreqs:=255  ' set all channels to high freq mode for max speed
        quit
      ' set the largest channel to HighFreq mode and recalculate bandwidth.
      rate := long[pRates+(largestCh*4)] * nAvg
      rate := ((CLKFREQ/rate)/100) * (10+120+1) 'calculate new rate.
      totalBandwidth -= largestRate
      totalBandwidth += rate
      ChanFreqs |= 1<<largestCh
      rateArray[largestCh]:=0
      retV:=totalBandwidth
       
      if totalBandwidth =< MAX_BANDWIDTH
        quit
                            
  long[pChanFreqs]:=ChanFreqs



return retV

pub TogglePin(Pin)
 {{toggles the debugging pin to check with a scope/LED}} 
 !outa[Pin]
pub WaitMS(MS)                
  waitcnt(((clkfreq/1000) * MS) +cnt) 'wait for a specified number of MS 