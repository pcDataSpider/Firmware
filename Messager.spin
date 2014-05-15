con           
 ' _clkmode = xtal1 + pll16x
 ' _xinfreq = 5_000_000
con
  'BAUD = 600
  'BAUD = 9600
  'BAUD = 57600
  BAUD = 115200       
  MAX_BUFFER = 1000              ' maximum length of the main buffer in bytes
  EX_DAT_LEN = 50               ' lenth of ExData
  MAX_WAITING = 10            ' max allowed sent messages to be waiting for timeout
  WAITING_SIZE = 10             ' size(in bytes) that one message's info consumes
  DEBUG = 0
  BUFDUMP = 0
  MSG_SIZE = 20
  MAX_MSG = 30                             '<XXX................>

             
                                          '<XXXp:19,#0000,#0000>
                      
  TIMEOUT = 80000000
  'TIMEOUT = 800000
  '<!zd:XX,#zzzz>
  
  EOP = 124
  ESC = 96     
obj                               
'  Com   :       "FullDuplexSerial"     
  Com   :       "FullDuplexSerial_rr004" 
  ExecCB:       "MessageHandler"  
  Q     :       "Queue"
  Name  :       "Queue"
  Val   :       "Queue" 
  Tmp   :       "Queue"
  Tmp2  :       "Queue" 
  Waiting    :  "Queue"
  WaitingMsg :  "Queue"   

dat 'global shared data                                    

                    
 
   Locked     long 0
   LastPkt    long 0 
   NameNum    long 0 
   ValNum     long 0  
   RetData    long 0
   ExData     long 0[EX_DAT_LEN]
          

   newID         long 0
   
   LockID     byte 0
   ComCog     byte 0            
   Buffer     byte 0[MAX_BUFFER]
   PacketBuffer byte 0[MAX_MSG]
   SendBuffer byte 0[MAX_MSG] 


   Tout       long 0 'next timeout for repeating messages
   ToutTaken  long 0 'cnt when Tout was taken, used for clk overflow
   WaitingInfo long 0[MAX_WAITING * WAITING_SIZE] ' buffer for waiting repeating message info
   WaitingBuf byte 0[MAX_WAITING * MSG_SIZE] ' buffer used to store repeating messages
   eIDBuf     long 0[128]  

pub main         

  
  ComCog:=Com.Start(31,30,%0000,BAUD)       
  if  ComCog== 0
    repeat                   
      Com.str(String ("MCOG:"))
      Com.dec(ComCog)
    return 0

  repeat
    Com.str(String (" BadDownload "))

pub Start
 
  Q.init(@Buffer,MAX_BUFFER)
  Q.insert(EOP)
  Q.insert(" ")     
       
  ComCog:=Com.Start(31,30,%0000,BAUD)       
  if  ComCog== 0
    repeat                   
      Com.str(String ("MCOG:"))
      Com.dec(ComCog)
    return 0
           
  LockID:=LockNew
  if LockID==-1
    repeat                  
      Com.str(String ("MCOG:"))
      Com.dec(ComCog)  
      Com.str(String ("BADLOCK:"))
    return 0
    
  ExecCB.Init   
                                       
  return ComCog
pub Stop
  Com.Stop 
pub checkKeys
    if Read > 0 '(reads in the RxBuffer from Ser. Com)
      Parse ' parses the buffer for keys.

pub echoQ | n
n:=Q.getHead
lock
repeat Q.getSize
  Com.tx(Q.peek(n++))
clear
pub echoQesc | n
n:=Q.getHead
lock
repeat Q.getSize
  if Q.peek(n) == ESC or Q.peek(n) == EOP
    Com.tx(ESC)
  Com.tx(Q.peek(n++))
clear
pub echoTmp | n
n:=Tmp.getHead
lock
repeat Tmp.getSize
  Com.tx(Tmp.peek(n))
  n:=Tmp.inc(n)
clear
pub echoname | n
n:=Name.getHead
lock
repeat Name.getSize
  Com.tx(Name.peek(n++))
clear
pub echoval | n
n:=Val.getHead
lock
repeat Val.getSize
  Com.tx(Val.peek(n++))
clear

pub Parse | beg,end,numChars,c,state, exec,  r, n, lastc,chk , sum, escaped
{{ Parses the buffer for keys. }}
' state determines what we are looking for next.


  if  Q.isEmpty               
    return 0
  end:=0
                        
  repeat until end==-1
    ' match a key. 
     '0 searching for EOP of last packet
     '1 gather checksum      
     '2 gather packet data

     
    lastc:=0
    sum:=0  
    beg:=0
    end:=0
    state:=0
    escaped:=0
    
    Tmp2.initWithData(@Buffer,MAX_BUFFER,Q.getHead, Q.getTail, 1, 1) 
    Tmp.init(@PacketBuffer,MAX_MSG)
                      
    repeat
    
      if Tmp2.isEmpty
        end:=-1                              
        quit
                       
      c:=Tmp2.get
      case state
        0: 'searching for start of packet (end of last packet)
          if not escaped and c==EOP
            state:=1
          else
            Com.tx("!")
            Com.tx("!")
            Com.tx("!")
            Com.tx(EOP)
            Com.tx(0)
            'Com.tx("(")
        1: 'previous packet chksum   
          state:=2             
          beg:=Tmp2.getHead  
          'Com.tx("#")
        2: 'collect packet data
          if not escaped and c==EOP 
            'Com.tx(")")        
            state:=3
          elseif escaped or (c<>ESC and c<>EOP)
              Tmp.insert(c)
              'Com.tx(c)
            sum:=checksum(sum,c)
            'sum<-=1
            'sum+=c
        3: 'character after EOP is chksum  
          end:=Tmp2.dec(Tmp2.dec(Tmp2.getHead))
          chk:=c         
          quit             
            
      if c==ESC and not escaped
        escaped:=1
      else
        escaped:=0
      lastc:=c
                  
    if end==-1                     
      quit ' no more keys!!                
    ' test checksum  
    if sum <> chk
      ExData[0]:=sum
      ExData[1]:=chk
      sendControl(2,@ExData, 2)
    else               
      ' parse key.                  
      if Tmp.peekNext & %10000000
        'stream packet  (PropDAQ does not understand stream packets. this msg is a mistake.
        parseStream    
      else
        'control packet
        parseControl
        r:=ExecCB.exec(NameNum, @ExData, ValNum)
        if not (r & 1<<31)   
          sendControl( r, @ExData+4, ExData[0] )
          
    if BUFDUMP>0    
      Com.str(String("<dump:'"))   
    repeat Q.getDist(Q.getHead,end)  'flush out things from Q. 
      if BUFDUMP > 0
        c := Q.get
        if c == EOP or c == ESC
          Com.tx(ESC)
        Com.tx(c)     
      else
        Q.get
       
    if BUFDUMP>0   
      Com.str(String("'>"))
      Com.tx(EOP)

pri parseStream | n 
  Com.str(String("Can't Read Stream Packets!"))
  NameNum:=-1 'indicates error with control packets
  return
pri parseControl | n, c, state, curVal
  {{ Parses the Tmp buffer for control packets. fills nameNum, exData, and valNum}}
    ' match a key. 
     '0 gets msg ID
     '1 gets packet #   
     '2 reads in msd parameters


    curVal:=0 
    state:=0
    ValNum:=0
    NameNum:=-1
    n:=0
           
      if Tmp.isEmpty
        NameNum:=-1                 
        Com.str(String("Bad Control Msg!|"))                            
        return
    repeat   
      if Tmp.isEmpty
        quit
                       
      c:=Tmp.get
      case state
        0: 'first byte: the message ID#
          NameNum:=c
          state:=1
          
        1: 'second byte: packet #
          LastPkt:=c  
          state:=2
        2: 'collect packet data
          curVal <<= 8
          curVal += c
          n++
          if n==4
            exData[valNum]:=curVal
            curVal:=0
            n:=0
            valNum++
  return
pub sendControl(nameID, params, paramLen) | n, m, i, v, bytev, chkSum, pID
{{ sends a control packet with nameID, and a long arrary of params, of length paramLen
    nameID - Control Message ID
    params - Long array of message parameters
    paramLen - number of longs in params}}

  'com.tx("%")
  'com.dec(nameID)
  'com.tx("#")
  'com.dec(paramLen)
  'n:=0
  'repeat paramlen
  '  com.tx(".")
  '  com.dec(long[params+n*4])
  n:=0
  chkSum:=0
  'sendBuffer[n++] := 0 ' placeholder for checksum
  if nameID == EOP ' make sure not to transmit the EOP within the packet
    sendBuffer[n++] := ESC 
    chkSum := checksum(chksum,ESC)
  sendBuffer[n++] := nameID  
  chkSum := checksum(chksum,nameID)
  pID:=nextID
  if PID == EOP ' make sure not to transmit the EOP within the packet
    sendBuffer[n++] := ESC 
    chkSum := checksum(chksum,ESC)
  sendBuffer[n++] := pID
  chkSum := checksum(chksum,PID)
 ' bytemove( @sendBuffer+n, params, paramLen*4 )

  m:=0 ' move the long aray, params, into a byte array. (this involves inverting the byte order of each long)
  i:=0
  repeat paramLen
    v := long[params+(m)]
    m+=4
    i:=8
    repeat 4             
      bytev := (v >> (32 - i))&$FF
      i+=8   
      if bytev == EOP or bytev == ESC ' make sure not to transmit the EOP within data
        sendBuffer[n++] := ESC 
        chkSum := checksum(chksum,ESC)
      sendBuffer[n++] := bytev 
      chkSum:= checksum(chksum,bytev)                        
   
 
  sendBuffer[n++] := EOP 
  sendBuffer[n++] := chkSum

          
  lock
  m:=0
  repeat n
    com.tx(sendBuffer[m++])  
  clear
pub checksum(chksum, value)
  return  ((((chksum<<1) | (chksum>>7)) & 255) + value) & 255
pub txEOP
  Com.tx(EOP)
pub txESC
  Com.tx(ESC)
pub txData(c) 'sends a character over serial port, avoiding EOP and ESC characters
  if c == EOP or c == ESC
    Com.tx(ESC)
  Com.tx(c)
pub char( c )  ' sends a char over the serial port 
  Com.tx(c)  
pub str( s ) 
  Com.str(s) 
pub dec( d ) 
  Com.dec(d)
pub bin(v)  
  Com.tx((v&$FF000000)>>24)   
  Com.tx((v&$00FF0000)>>16)   
  Com.tx((v&$0000FF00)>>8)   
  Com.tx((v&$000000FF)>>0)  
pub pause | c
{{ Pause until a character is recieved. }} 
  repeat
    c:=Com.rxcheck  
  until c<>-1  
pub lock
  repeat until not lockSet(LockID) 
pub clear
  lockClr(LockID)
pub nextID
  newID:=(newID+1) & $FF
  if newID==0
    newID:=(newID+1) & $FF 
  return (newID)

pri Read | c, bytrd
{{ reads in all characters in the rx buffer }}  
  bytrd:=0
      'BlinkLED
  repeat   
    if Q.isFull==1
      ExData[0]:=Q.gethead
      ExData[1]:=Q.getTail
      com.str(String("!full!|"))
      c:=-1 ' Full. done with loop. 
    else ' otherwise, read in next char.  
      c:=Com.rxcheck
      if c<>-1             
     
        Q.insert(c)
        bytrd++   
  until c==-1
  if  bytrd > 0 
    if BUFDUMP > 0
      lock  
      Com.str( String("<buffer:'") )
      clear
      echoQesc
      lock
      Com.str( String("'>") )
      Com.tx(EOP)
      clear  

  return bytrd                         
pub WaitMS(MS)                
  waitcnt(((clkfreq/1000) * MS) +cnt) 'wait for a specified number of MS        