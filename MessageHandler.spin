Con


  nChannels     = 14   ' cant exceed 32.
  nAnalogI      = 4 
  nAnalogO      = 2 
  nDigitals     = 8

  cDigitalPin   = 16   ' first digital pin#                              
                 
  btnMask       = %10000000 

       
obj
  wav       : "WaveGen"
           

dat                              
        pwmData1        long    0
        pwmData2        long    0
        pwmPeriod       long    0
pub Init
  {{ Object Instance specific Init method }}
  wav.start( pwmData1, pwmData2, pwmPeriod )
  wav.chngDig(000, 00, %00000000000011110000000000000000)
  wav.chngWav(1, 0, 1000, 1)
  wav.chngWav(1, 0, 1000, 2)
pub InitData(data1, data2, period)
  {{ Defines data that is shared among all object instances using DAT blocks }}
  pwmData1:=data1
  pwmData2:=data2
  pwmPeriod:=period
pub Exec(NameAddr, NameSize, NameNum, pExData, ValNum) : retV
  {{ Handle parsed messages }}
  if NameNum == 10 ' wav message  <wav:wavetype, period, duty/amp, pins/pwm#>      
    if ValNum == 4
      if Long[pExData + 0] == 0
       wav.chngDig( Long[pExData + 4], Long[pExData + 8],Long[pExData + 12])
      else
        wav.chngWav( Long[pExData + 0],Long[pExData + 4], Long[pExData + 8], Long[pExData + 12])
    return -1         
  return 0                                            