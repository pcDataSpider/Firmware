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

 
   somevar    long 0
 
   Locked     long 0
   NameAddr   long 0 
   NameSize   long 0
   NameNum    long 0
   NameIdx    long 0
   ValAddr    long 0 
   ValSize    long 0
   ValNum     long 0  
   RetData    long 0
   ExData     long 0[EX_DAT_LEN]
          

   newID         long 0
   
   LockID     byte 0
   ComCog     byte 0            
   Buffer     byte 0[MAX_BUFFER]
   SendBuffer byte 0[MAX_MSG] 
   CallBack   long 0    
   NameTable  long 0   
   NameCount  long 0

   Tout       long 0 'next timeout for repeating messages
   ToutTaken  long 0 'cnt when Tout was taken, used for clk overflow
   WaitingInfo long 0[MAX_WAITING * WAITING_SIZE] ' buffer for waiting repeating message info
   WaitingBuf byte 0[MAX_WAITING * MSG_SIZE] ' buffer used to store repeating messages
   eIDBuf     long 0[128]  

dat  ' predefined strings. 
              '  size,string
   NotFound   byte 8,"NotFound"



pub main         

  
  ComCog:=Com.Start(31,30,%0000,BAUD)       
  if  ComCog== 0
    repeat                   
      Com.str(String ("MCOG:"))
      Com.dec(ComCog)
    return 0

  repeat
    Com.str(String (" BadDownload "))

pub Start(callbackaddr, nametableaddr, nkeys)
  Callback:=callbackaddr
  NameTable:=nametableaddr
  NameCount:=nkeys
              
  Q.init(@Buffer,MAX_BUFFER)  
  ExecCB.Init           
       
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
pri storeVal | v, digit, num
  if DEBUG 
    Com.str(String("<store:" ))
    echoVal
    Com.tx(">")
  if Val.getSize == 0  ' val is empty..  
    return ' nothing to do. this is not a value.
  else          

    if Val.peekNext == ","      'get rid of leading comma before storing.
      Val.get
    v:=0
    num:=0
    case Val.peekNext
      "#":
        Val.get                 ' throw away the #      
        repeat 4
          v<<=8
          v+=Val.get
        exData[valNum]:=v
          
      "'":
        Val.get                 ' throw away the open quote. use close quote as string terminator 
        exData[valNum]:=ValAddr 
      other:
      
        ValAddr:=Val.getAddr + Val.getHead 'start of Val. 
        repeat until Val.isEmpty  
          digit:=Val.get - 48          ' get digit in numeric form
          if digit<0 or digit>9    ' if digit is not a number, store this addr, and move to next val
            num:=1
            quit
          else  
            v*=10              'otherwise its a number...
            v+=digit
        if num == 0
          exData[valNum]:=v
        else
          exData[valNum]:=ValAddr  
    valNum++
    ' empty Val
    repeat until Val.isEmpty
      Val.get



pub Parse | beg,end,numChars,c,state, exec,  m, n
{{ Parses the buffer for keys. }}
' state determines what we are looking for next.
' 0 : looking for open paren
' 1 : looking for name. ends with : or >
' 2 : looking for val. .... unknown what yet. lots of cases..
' 3 : extract a number.     
' 4 : extract a string.
' 5 : extract a digest.
  if  Q.isEmpty               
    return 0
  end:=0
  Tmp2.initWithData(@Buffer,MAX_BUFFER,Q.getHead, Q.getTail, 1, 1) 
 { repeat until end == -1
    repeat until Tmp2.isEmpty
      tmp2.get
    end:=-1
  }                            
  repeat until end==-1

   ' match a key. 
    m:=1       
    beg:=0
    end:=0
    state:=0
    ValNum:=0
     
    repeat while end==0
    
      if Tmp2.isEmpty
        end:=-1                              
        quit            
      c:=Tmp2.get
      case state
        0:
          if c=="<"       
            state:=1  
            NumChars:=0 
            beg:=Tmp2.getHead
            Name.initWithData(@Buffer, MAX_BUFFER, Tmp2.getHead, Tmp2.getHead, 0, 1)
        1:
          if c==":"
            'sum+= c*m++     
            state:=2
            Val.initWithData(@Buffer, MAX_BUFFER, Tmp2.getHead, Tmp2.getHead, 0, 1)
          
          elseif c==">" 
            end:=Tmp2.getHead
          elseif c=="<"    
            state:=1   
            NumChars:=0 
            beg:=Tmp2.getHead
            Name.initWithData(@Buffer, MAX_BUFFER, Tmp2.getHead, Tmp2.getHead, 0, 1)
            Val.initWithData(@Buffer, MAX_BUFFER, Tmp2.getHead, Tmp2.getHead, 0, 1)
          else 
            'sum+= c*m++
            name.insert(c)   
        2:    
          if c==">"                            
            storeVal 
            end:=Tmp2.getHead
          elseif c=="<"   
            state:=1
            beg:=Tmp2.getHead
            Name.initWithData(@Buffer, MAX_BUFFER, Tmp2.getHead, Tmp2.getHead, 0, 1)
            Val.initWithData(@Buffer, MAX_BUFFER, Tmp2.getHead, Tmp2.getHead, 0, 1)  
          elseif c=="#" 
            'sum+= c*m++  
            state:=3
            numchars:=0   
          elseif c=="'" 
            'sum+= c*m++  
            state:=4 
          elseif c==","
            'sum+= c*m++   
            storeVal
          else 
            'sum+= c*m++       
          val.insert(c)         ' always insert into val. 
        3:         
          'sum+= c*m++  
          numChars++  
          if numChars==4
            state:=2     
          val.insert(c)
        4: 
          'sum+= c*m++            
          if c=="'"
            state:=2  
          val.insert(c)

                 
     
                                        
    if end==-1                            
      quit ' no more keys!!                
                
    ' parse key.              
    Tmp.initWithData(@Buffer,MAX_BUFFER,beg,end, 1, 1)  
    execCallback   
          
    if BUFDUMP>0    
      Com.str(String("<dump:'"))   
    repeat Q.getDist(Q.getHead,end)  'flush out things from Q. 
      if BUFDUMP > 0
        Com.tx(Q.get)
      else
        Q.get
       
    if BUFDUMP>0   
      Com.str(String("'>"))


pub sendKey(keyname,NameLen,Vals,ValsLen, flags) | n, m, i, x, sum2, eID2, type, cval, r
  {{
  keyname - Byte array for name
  NameLen - length of keyname
  Vals - array of longs, packed as ...type,val,type,val,type,val...
  ValsLen - number of packed values, len(Vals/2) 
  flags - 0Bit set for echo, Bit1 set for extra value types, Bit2 set for ack, bit3 set for a msgID of 0 }}
               
  n:=0  
    
  bytemove( @sendBuffer+n, keyname, NameLen)
  n+=NameLen  

  if ValsLen > 0  
    sendBuffer[n++] := ":"

  m:=0
  repeat ValsLen
  
    if flags & %10       
      type:=Long[Vals+m] 
      m:=m+4
    else                                       ' 1000
      type:=1                                  ' #_ _ _ _
    cval:=Long[Vals+m]
    m:=m+4    
      
    if type == 1  
      if cval<10000 and cval>-10000
          '' Print a decimal number 
        x := cval == NEGX                                                            'Check for max negative
        if cval < 0
          cval := ||(cval+x)                                                        'If negative, make positive; adjust for max negative
          sendBuffer[n++] := ("-")                                                                     'and output sign
        i := 1000                                                            'Initialize divisor

        r~ 
        repeat 4                                                                     'Loop for 4 digits
          if cval => i                                                               
            sendBuffer[n++] := (cval / i + "0" + x*(i == 1)) & $FF                     'If non-zero digit, output digit; adjust for max negative
            cval //= i                                                                 'and digit from value
            r~~                                                                  'flag non-zero found
          elseif r or i == 1
            sendBuffer[n++] := ("0")                                                                   'If zero digit (or only digit) output it
          i /= 10   

      else  
        sendBuffer[n++]:="#"
        sendBuffer[n++]:=((cval&$FF000000)>>24)   
        sendBuffer[n++]:=((cval&$00FF0000)>>16)   
        sendBuffer[n++]:=((cval&$0000FF00)>>8)   
        sendBuffer[n++]:=((cval&$000000FF)>>0) 
    if type == 2
      ' string type           
      sendBuffer[n++]:="'"
      i:=0
      repeat
        if byte[cval+i] == 0
          quit
        else
          sendBuffer[n++]:=byte[cval+i++] 
      sendBuffer[n++]:="'" 
    if (flags & %10 and m/8 <> ValsLen) or (!(flags & %10) and m/4 <> ValsLen)                                   
      sendBuffer[n++]:=","                               



  send(@sendBuffer, n)
                                       
                             

pri send( msg, len) | n, m
         
  n:=0
  lock 
  com.tx("<")
  repeat len
    com.tx( byte[msg+n] ) 
    if WaitingMsg.isFull
      WaitingMsg.get
    WaitingMsg.insert( byte[msg+n++] )  

  com.tx(">")    
  clear
        
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
  repeat while newID==0
    newID:=(newID+1) & $FF 
  return (newID)
pri hashName     : n | TableIdx, Pos
n := 0     
Pos:=Name.getHead      
TableIdx := NameTable

if Name.isEmpty
  return 0                                
      
repeat NameCount                        
  n++   

  repeat
    ' skip echo symbols
    'if Name.peek( Pos ) == "@" or Name.peek( Pos ) == "!"
    '  Pos:=Name.inc(Pos)
    '  Pos:=Name.inc(Pos)
    '  next        
    ' ceck if this is the key 
    if Byte[TableIdx] <> Name.peek( Pos )
      quit

    ' test for end of buffer
    if Name.inc(Pos) == Name.getTail or name.peek( Name.inc(Pos) )=="|"  
      if Byte[TableIdx+1] == 0  ' test for end of entry  
        return    ' MATCH!
      else                                       
        quit      ' no match.
     
    TableIdx++
    Pos:=Name.inc(Pos) 
  ' Key does not match, check next entry...        
  repeat until Byte[TableIdx++] == 0
  Pos:=Name.getHead
           
n:=0 
pri execCallback : COG |digit,number,n,dbg
{{ Executes the callback function. blocks until callback is complete.}}
                  
'setup data before call.     
NameAddr:=Name.getAddr+(Name.getHead)
NameSize:=Name.getSize    
ValAddr:=Val.getAddr  ' case when val is not a number, and is not empty
ValSize:=Val.getSize  ' size of the ValAddr string, 0 if all numbers.        
Locked~
COG~                      
' convert from string to int.
' number represents index into nametable key was found, starting at 1
NameNum:=hashName
'NameIdx:=getNameIdx ' returns the index in numeric form ( <...|IDX:...> )
  RetData := ExecCB.exec(NameAddr, NameSize, NameNum, @ExData, ValNum)
   
  if RetData <> 0  
    if RetData <> -1 
      SendKey(Name.getAddr+Name.getHead,Name.getSize, @ExData, RetData, 4)
    return
                                                    '          ID
                                                     
  if NameNum == 0                                    '   <- <XX ì!   start>
    Locked:=0                                        '   -> <XX j@ì  start:32>                                
    RetData:=0  
  else  
    NameNum--
    repeat
      COG:= cognew(callback, @Locked)+1
    until COG
    if DEBUG
      SendKey(String("execval"), 7, @ExData, 4, 0)                  
    repeat until Locked

     
  if Locked==0 
    SendKey(String("NotFound"),8, @ExData, 0, 1)                                    
  elseif Locked==1  ' normal     
    if ValNum==0     
      ExData[0]:=RetData
      SendKey(Name.getAddr+Name.getHead,Name.getSize, @ExData, 1, 0)
  elseif Locked==2  ' send retData  
      ExData[0]:=RetData
      SendKey(Name.getAddr+Name.getHead,Name.getSize, @ExData, 1, 0) 
  elseif Locked==3 
    SendKey(Name.getAddr+Name.getHead,Name.getSize, @ExData, RetData, 4)
  else                    
    Com.str(String("<?>"))
                                    
    
    
  if DEBUG  
    Com.str(String("<Ret:"))  
    Com.dec(RetData)   
    Com.tx(",")         
    Com.dec(Locked)  
    Com.tx(">")       

pri Read | c, bytrd
{{ reads in all characters in the rx buffer }}  
  bytrd:=0
      'BlinkLED
  repeat   
    if Q.isFull==1
      ExData[0]:=Q.gethead
      ExData[1]:=Q.getTail
      sendKey(String("Full"), 4, @ExData, 2, 1)
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
      echoQ  
      Com.str( String("'>") )
      clear  

  return bytrd                         
pub WaitMS(MS)                
  waitcnt(((clkfreq/1000) * MS) +cnt) 'wait for a specified number of MS        