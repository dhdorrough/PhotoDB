{WookieWare Home Defense Series cautiously presents
 String matching routines, Public Domain effective immediately.
 Please bestow credit in any distributed software or source.
 (Yes, they're tested. No, I don't claim to have written them in half
  an hour.)                                             }

 { The assembly routines are much faster but probably haven't
   been compilable since Turbo Pascal 4- DHD }

unit StrCompareX;

interface
  const
    STRCOMPAREMAX = 100;

  type
    str255 = string[255];

function similar100(st1,st2: string): word;
function bickell2(const s1,s2:string):integer; {not quite, but similar}

implementation


// Unused; commenting out to avoid compiler hint. BEW 3/5/2012
// const seeds: string='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';



function similar100(st1,st2: string): word;
{This one started the whole thing. Loosely based on an algorithm called
 SIMILAR.ASM  written by John W. Ratcliff and David E. Metzener
 (See Dr. Dobb's Journal)
 only a lot more understandable. Returns percentage match. Pretty slow
 compared to the ASM versions. Case sensitive.
 Ron Nossaman Sept. 30 1994 }
var score:integer;

   procedure compare(const s1,s2:string);
     var s1l, s1r, s2l, s2r, looker: integer;
   begin { compare }
      s1l    := 1;
      s1r    := length(s1);

      s2l    := 1;
      s2r    := length(s2);
      looker := s2l;
   {increment s1, sweep s2}
      repeat
         if (s1l > s1r) or (looker > s2r) then exit;  // dhd to handle empty strings
         if s1[s1l] = s2[looker] then
           begin             {got a match}
              inc(s1l);      {next position on everything}
              inc(looker);
              s2l := looker;   {pull up starting position marker}
              inc(score);
           end
         else
           inc(looker); {no match, continue sweep}

         if looker>s2r then    {looker swept past end of string}
           begin
              looker := s2l;     {restore looker to last unmatched position}
              if s2l > s2r then
                s1l := s1r;
              inc(s1l);        {next char in first string for matching}
           end;
      until s1l > s1r;
   end;  { compare }

begin { similar100 }
   score  := 0;
   compare(st1,st2);
   compare(st2,st1);
   result := (score * STRCOMPAREMAX) div (length(st1)+length(st2));
end;  { similar100 }



(*
Function Match(const s1:str255; const s2:str255):word;
{Uncle Ron's algorithm to compare two strings, returns percentage match}
{Case sensitive}
{Ron Nossaman Oct2, 1994}
begin
   asm
      LES DI,[S2]
      LDS SI,[S1]
      Xor dx,dx        {zero score}
      xor ax,ax
      cmp [si],al       {is byte1 a zero?}
      je @strerr       {yes, BAIL}
      cmp [di],al
      jne @docmp
@strerr:
      jmp @millertime        {BAIL}
      { ;neither strings zero length, do it}
@docmp:
      cld
      Xor ax,ax
      mov al,[di]       {get length S2}
      mov cx,ax         {save in cx}
      add ax,di
      mov bx,ax         {bx=pointer last byte S2}
      inc di            {di=pointer first byte S2}
      Xor ax,ax
      mov al,[si]
      push ax
      add ax,cx
      mov cx,ax         {total length both strings}
      pop ax
      add ax,si         {ax=pointer last byte S1}
      inc si            {si=pointer first byte S1}
      {ax=lastchar s1}
      {bx=lastchar s2}
      {si=firstchar s1}
      {di=firstchar s2}

      push cx           {save 'total' characters}
      push bx           {save s2 end}
      push ax           {save s1 end}

      mov cx,0          {indicator of first pass through compare}
      jmp @compare
@round2:
      LES DI,[S1]     {swap string beginnings}
      LDS SI,[S2]
      inc si
      inc di
      pop bx          {s2 end swapped}
      pop ax          {s1 end swapped}
                      {'total' still on stack}
      mov cx,1          {pass 2 indicator}

@compare:
      push cx     {save pass indicator}
      mov cx,di   {let keeper remember starting point}
@workloop:
      push ax       {save eos pointers to free up registers}
      push bx
      xor ax,ax
      mov al,[si]
      mov bx,ax
      mov al,[di]
      cmp ax,bx     {are chars equal?}
      jne @nomatch  {no, pass on}
      inc si        {yes, increment both string position pointers}
      inc di
      mov cx,di     {keeper remembers new starting position}
      inc dx        {score}
      jmp @progress
@nomatch:
      inc di    {no match, try next char in second string}
@progress:
      pop bx        {restore end of string pointers}
      pop ax
      cmp di,bx     {is string 2 used up without match?}
      jle @nofix    {nope, go on}
      mov di,cx     {restore last unmatched position}
      cmp di,bx     {is string2 matched to the end?}
      jle @nofix2   {no, go try next letter of string1}
      mov si,ax     {yes, nothing left to compare, cancel further search}
@nofix2:
      inc si        {next char string1}
@nofix:
      cmp si,ax     {done yet?}
      jle @workloop {nope, hiho}
      pop cx        {retreive pass indicator}
      cmp cx,0      {0=pass1}
      je @round2    {go back for pass 2}
      mov ax,dx     {score}
      mov cx,STRCOMPAREMAX
      mul cx
      pop cx      {get 'total' characters}
      div cx
@millertime:
      mov @result,ax
   end;
end;
*)

(*
Function Match2(const s1:str255; const s2:str255):word;
{Uncle Ron's algorithm to compare two strings, returns percentage match}
{a tad smaller, faster. Still Case sensitive}
{Ron Nossaman Oct 4, 1994}
begin
   asm
      les di,[s2]
      lds si,[s1]
      xor ax,ax
      mov al,[si]
      cmp al,0
      je @nolength
      mov cx,ax        {cx= length of string1}
      mov al,[di]
      cmp al,0
      jne @docmp       {ax= length of string2}
@nolength:
      jmp @millertime        {BAIL}

@docmp:       { ;neither strings zero length, do it}
      cld
      mov dx,ax         {save length(s2)}
      add ax,di
      mov bx,ax         {bx= pointer last char s2}
      inc di            {di= pointer first char s2}
      mov ax,dx         {retreive length(s2)}
      add ax,cx         {+length(s1)}
      push ax           {save total length both strings until final scoring}
      mov ax,cx         {length(s1)}
      add ax,si         {ax=pointer last char s1}
      inc si            {si=pointer first char s1}
      Xor dx,dx         {zero score}


      {cast:}           {ax=lastchar s1}
                        {bx=lastchar s2}
                        {si=firstchar s1}
                        {di=firstchar s2}
                        {dx=accumulated score}
                        {cx=temporary position marker during compare}


      mov cx,0          {indicator of first pass through compare}
      jmp @compare
@round2:
      les di,[s1]     {swap string beginnings}
      lds si,[s2]
      inc si
      inc di
      xchg ax,bx      {swap s1 and s2 end pointers}
                      {'total' still on stack}
      mov cx,1          {pass 2 indicator}

@compare:
      push cx     {save pass indicator}
      mov cx,di   {let keeper remember starting point}
@workloop:
      push ax       {save eos pointer to free up ax register}
      mov al,[si]
      mov ah,al
      mov al,[di]
      cmp al,ah     {are chars equal?}
      jne @nomatch  {no, pass on}
      inc si        {yes, increment both string position pointers}
      inc di
      mov cx,di     {keeper remembers new starting position}
      inc dx        {score}
      jmp @progress
@nomatch:
      inc di    {no match, try next char in second string}
@progress:
      pop ax       {restore end of string pointer}
      cmp di,bx     {is string 2 used up without match?}
      jle @nofix    {nope, go on}
      mov di,cx     {restore last unmatched position}
      cmp di,bx     {is string2 matched to the end?}
      jle @nofix2   {no, go try next letter of string1}
      mov si,ax     {yes, nothing left to compare, cancel further search}
@nofix2:
      inc si        {next char string1}
@nofix:
      cmp si,ax     {done yet?}
      jle @workloop {nope, hiho}
      pop cx        {retreive pass indicator}
      cmp cx,0      {0=pass1}
      je @round2    {go back for pass 2}
      mov ax,dx     {score}
      mov cx,STRCOMPAREMAX
      mul cx
      pop cx      {get 'total' characters}
      div cx
@millertime:
      mov @result,ax
   end;
end;
*)




(*
Function Match3(const s1: str255; const s2:str255; case_sensitive: boolean):word;
{Uncle Ron's algorithm to compare two strings, returns percentage match}
{Case sensitive/not switch    Most versatile, speed comparison varies}
{Ron Nossaman Oct 29, 1994}
begin
   asm
      push ds
      les di,[s2]
      lds si,[s1]
      xor ax,ax
      SEGDS mov al,[si]
      cmp al,0
      je @nolength
      mov cx,ax        {cx= length of string1}
      SEGES mov al,[di]
      cmp al,0
      jne @docmp       {ax= length of string2}
@nolength:
      jmp @millertime        {BAIL}

@docmp:       { ;neither strings zero length, do it}
      cld
      mov dx,ax         {save length(s2)}
      add ax,di
      mov bx,ax         {bx= pointer last char s2}
      inc di            {di= pointer first char s2}
      mov ax,dx         {retreive length(s2)}
      add ax,cx         {+length(s1)}
      push ax           {save total length both strings until final scoring}
      mov ax,cx         {length(s1)}
      add ax,si         {ax=pointer last char s1}
      inc si            {si=pointer first char s1}
      Xor dx,dx         {zero score}


      {cast:}           {ax=lastchar s1}
                        {bx=lastchar s2}
                        {si=firstchar s1}
                        {di=firstchar s2}
                        {dx=accumulated score}
                        {cx=temporary position marker during compare}


      mov cx,0          {indicator flag of first pass through compare}
                   {cheap dodge, since you can't call & ret in T.P. asm}
      jmp @compare
@round2:
      les di,[s1]     {swap string beginnings}
      lds si,[s2]
      inc si
      inc di
      xchg ax,bx      {swap s1 and s2 end pointers}
                      {'total' still on stack}
      mov cx,1          {pass 2 indicator}

@compare:
      push cx     {save pass indicator}
      mov cx,di   {let keeper remember starting point}
@workloop:
      push ax       {save eos pointer to free up ax register}
      SEGDS mov al,[si]
      cmp case_sensitive,0
      jnz @CaseOK1
      cmp al,'Z'
      jg  @CaseOK1
      cmp al,'A'
      jl  @CaseOK1
      or al,$20
@CaseOK1:
      mov ah,al
      SEGES mov al,[di]
      cmp case_sensitive,0
      jnz @CaseOK2
      cmp al,'Z'
      jg  @CaseOK2
      cmp al,'A'
      jl  @CaseOK2
      or al,$20
@CaseOK2:
      cmp al,ah     {are chars equal?}
      jne @nomatch  {no, pass on}
      inc si        {yes, increment both string position pointers}
      inc di
      mov cx,di     {keeper remembers new starting position}
      inc dx        {score}
      jmp @progress
@nomatch:
      inc di    {no match, try next char in second string}
@progress:
      pop ax       {restore end of string pointer}
      cmp di,bx     {is string 2 used up without match?}
      jle @nofix    {nope, go on}
      mov di,cx     {restore last unmatched position}
      cmp di,bx     {is string 2 matched to the end?}
      jle @nofix2   {no, go try next letter of string1}
      mov si,ax     {yes, nothing left to compare, cancel further search}
@nofix2:
      inc si        {next char string1}
@nofix:
      cmp si,ax     {done yet?}
      jle @workloop {nope, hiho}
      pop cx        {retreive pass indicator}
      cmp cx,0      {0=pass1}
      je @round2    {go back for pass 2}
      mov ax,dx     {score}
      mov cx,STRCOMPAREMAX
      mul cx
      pop cx      {get 'total' characters}
      div cx
@millertime:
      mov @result,ax
      pop ds
   end;
end;
*)



function bickell2(const s1, s2:string): integer; {not quite, but similar}
const
   weight:array[ord('a')..ord('{')]of byte=(
        3,6,5,4,3,5,5,4,3,8,7,4,5,3,3,5,7,4,3,3,4,6,5,8,8,9,0);
     (* a b c d e f g h i j k l m n o p q r s t u v w x y z { *)
var sort1,sort2:string;
    i,bick1,bick2:integer;
    b1,b2:array[ord('a')..ord('{')]of byte;

begin { bickell2 }
   sort1 := s1;
   sort2 := s2;
   for i := 1 to length(sort1) do
     if (sort1[i] < 'a') or (sort1[i] > 'z') then
       begin
          case sort1[i] of
           'A'..'Z':sort1[i] := char(ord(sort1[i])or 32);
            else sort1[i] := '{';
          end;
       end;

   for i := 1 to length(sort2) do if (sort2[i]<'a')or(sort2[i]>'z') then
     begin
        case sort2[i] of
         'A'..'Z':sort2[i] := char(ord(sort2[i])or 32);
          else sort2[i] := '{';
        end;
     end;
   fillchar(b1, sizeof(b1), 0);
   fillchar(b2, sizeof(b2), 0);

  { weed out duplicates, sort}
   for i := 1 to length(sort1) do
     b1[ord(sort1[i])] := weight[ord(sort1[i])];

   for i := 1 to length(sort2) do
     b2[ord(sort2[i])] := weight[ord(sort2[i])];

  {get total for comparison}
   bick1 := 0;
   for i := ord('a') to ord('{') do
     bick1 := bick1+b1[i]+b2[i];

  {add up all letters common to both words}
   bick2 := 0;
   for i := ord('a') to ord('{') do
     if b1[i]<>0 then
       if (b1[i]=b2[i]) then
         bick2 := bick2+b1[i]+b2[i];

  {figure match}
   bickell2 := (bick2*STRCOMPAREMAX)div bick1;
end;


END.


