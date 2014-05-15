var
   ' only one copy of this data!! to define more queue's, copy/pasta??
   byte LockID        ' lock to prevent read/writes
   'Buffer long 1[SIZE]  ' the buffer
   long Buffer        ' address to the current buffer
   byte Full        ' 0=normal 1=full
   long Head        ' points to first byte to READ
   long Tail        ' points to first FREE byte
   long size
   byte ReadOnly

pub init(addr,size2)
 {{ initializes the queue with addr as the underlying array
 and size2 as the length of the array
 }}
  Buffer:=addr
  Size:=size2
  Head:=0
  Tail:=0
  Full:=0
  ReadOnly:=0   

pub initWithData(addr,size2, start, end, startFull, isRO)
{{ initializes the queue with addr as the underlying array
 and size2 as the length of the array, with data already stored
    start     - head pointer into addr array
    end       - tail pointer into addr array
    startFull - 1 if queue is already full
    isRO      - 1 if queue should be read only
 }}
  Buffer:=addr
  Size:=size2
  Head:=start
  Tail:=end      
  ReadOnly:=isRO
       
  if Head == Tail
    if startFull
      Full:=1
    else
      Full:=0  
  else
    Full:=0
  
  
pub insert(val)
{{ inserts val into the queue}}
  if isFull ' fail if buffer is full.
    return 1
  ' add val into the queue
  'Buffer[Tail]:=val
  if not ReadOnly
    Byte[Buffer][Tail]:=val
  Tail++

  
  'rollover and test for overflow
  if Tail => SIZE 
    Tail:=0
    
  if Head==Tail
    Full:=1
  else
    Full:=0

  return 0   ' return success!!

pub replace(val, idx)
{{ replace the value at idx with val}}

  repeat while idx=>size
    idx-=Size
  Byte[Buffer][idx]:=val    

pub get : val
{{ reads and removes the next character from the buffer. if the buffer is empty, zero will be returned. use variable Full to test for this.}}
  ' if the buffer is empty, too bad! 
    if isEmpty  
      return 0

  ' read value in
  val:=Byte[Buffer][Head++]

  'rollover and check for underflow
  if Head=>Size
    Head:=0
  
  Full:=0

pub peekNext : val
{{ reads but does not remove the next character in the buffer. if buffer is empty, then returns 0. }}
  if isEmpty
    return 0
    
  val:=peek(Head)
pub peek(idx) : val
{{ reads but does not removes a character from the buffer. }}
  repeat while idx => Size  
    idx-=Size
  val:=Byte[Buffer][idx]

pub inc(val)
{{ increments a pointer in this buffer, accounting for rollover}}
   val++     
  'rollover
  if val=>Size
    val:=0
  return val
pub dec(val)
{{ decrements a pointer in this buffer, accounting for rollover}}
   val--   
  'rollover
  if val==0
    val:=Size
  return val      
pub isFull : tmp  
  if Head==Tail and Full
    return 1
  else
    return 0
pub isEmpty : tmp            
  if Head==Tail and NOT Full
    return 1
  else
    return 0
pub getTail : tmp
  tmp:=Tail
pub getHead : tmp
  tmp:=Head
pub getAddr : tmp
  tmp:=Buffer
pub getBufferSize : tmp 
  tmp:=Size 
pub getDist(h,t) : tmp
  {{ returns the distance in the buffer between a head and tail position.
  usfull for finding how many chars to delete, etc.

  if head and tail are the same, returns the full size of the buffer }}
  
  if t>h
    tmp:=t-h
  else
    tmp:= (SIZE - h)+t

pub getSize : tmp 
  if isEmpty
    tmp:=0
  else
    tmp:=getDist(Head,Tail)
pub getFree : tmp
  if isEmpty
    tmp:=Size
  elseif isFull
    tmp:=0
  else
    tmp:=Size-getSize
  
pub toString(newAddr) : i
  {{ return the data in the queue in zero-terminated-string form, stored in newAddr}}
  i:=0
  repeat until isEmpty
    newAddr[i]:=get
    i++
  newAddr[i]:=0
pub fromString(addr, isRO) | idx
  {{ inits a new buffer from a string }}
  Buffer:=addr
  Size:=0

  idx:=0
  repeat until Byte[Buffer][idx++] == 0
    Size++
  Head:=0
  Tail:=0
  Full:=1      
  ReadOnly:=isRO


pub Flush
  Head := 0
  Tail := 0