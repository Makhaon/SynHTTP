{==============================================================================|
| Project : Ararat Synapse                                       | 003.012.008 |
|==============================================================================|
| Content: buffer wrapper layer
|==============================================================================|
| Copyright (c)1999-2014, Lukas Gebauer                                        |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c) 1999-2012.               |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s): Radek Cervinka, delphi.cz                                    |
|                 Ondrej Pokorny, kluug.net
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(NextGen and Unicode buffer layer)}

unit synabyte;
{$i jedi.inc}

interface
uses
  sysutils;
{$IFDEF NEXTGEN}
    {$ZEROBASEDSTRINGS OFF}
{$ENDIF}
type
{$IFDEF UNICODE}
  TSynaByte = byte;
  TSynaBytes = record
  private
    FBytes: TBytes;
    FRefCheck: string;

    function GetChars(const Index: NativeInt): Char;
    procedure SetChars(const Index: NativeInt; const Value: Char);
    function AGetLength: NativeInt;
    procedure ASetLength(const Value: NativeInt);

    procedure UpdateTerminator;
    procedure CheckCow;
    procedure Unique;
  public
    class operator Implicit(const V1: String): TSynaBytes;
    class operator Implicit(const V1: RawByteString): TSynaBytes;
    class operator Implicit(const V1: TSynaBytes): String;
    class operator Implicit(const V1: Char): TSynaBytes;
    class operator Explicit(const V1: TBytes): TSynaBytes;


    class operator Add(const V1, V2: TSynaBytes): TSynaBytes;

    class operator Equal(const V1, V2: TSynaBytes): Boolean;
    class operator NotEqual(const V1, V2: TSynaBytes): Boolean;

    function Clone: TSynaBytes;
    procedure Delete(Start, Count: Integer);
    function Data: Pointer;


    property Chars[const Index: NativeInt]: Char read GetChars write SetChars; default;
    property Length: NativeInt read AGetLength write ASetLength;
    property Bytes:TBytes read FBytes;
  end;

//  procedure SetLength(var s: TSynaBytes; Count:Integer); overload;

{$ELSE}
  TSynaBytes = AnsiString;
  TSynaByte = AnsiChar;
{$ENDIF}

{$IFNDEF DELPHI12_UP}
  TBytes = Array of Byte;

  function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
{$ENDIF}

  function StringOf(const bytes: TSynaBytes):string; overload;
  function StringOf(const bytes: TBytes):string; overload;
  function StringOf(const bytes: PByte): String; overload;

  procedure DeleteInternal (var s: TSynaBytes; Start, Count: Integer);

implementation

{$IFDEF UNICODE}

function IsBytesEquals(const Bytes1, Bytes2: TBytes; const Len1, Len2: NativeInt): Boolean;
var
  i: NativeInt;
begin
  if Len1 <> Len2 then
    Exit(False);

  for i := 0 to Len1 - 1 do
    if Bytes1[i] <> Bytes2[i] then
      Exit(False);

  Result := True;
end;

class operator TSynaBytes.Implicit(const V1: String): TSynaBytes;
begin
  Result.FBytes := TEncoding.Default.GetBytes(V1);
  Result.Length := System.Length(Result.FBytes);
end;
 
class operator TSynaBytes.Add(const V1, V2: TSynaBytes): TSynaBytes;
begin
  Result.Length := V1.Length + V2.Length;
  if V1.Length > 0 then
    Move(V1.FBytes[0], Result.FBytes[0], V1.Length);
  if V2.Length > 0 then
    Move(V2.FBytes[0], Result.FBytes[V1.Length], V2.Length);
end;

procedure TSynaBytes.CheckCow;
  function RefCount: Integer;
  var
    xStrPtr: ^Integer;
  begin
    //get reference count of FStrBuffer, correct results on 32bit, 64bit and also mobile
    xStrPtr := Pointer(PChar(FRefCheck));
    Dec(xStrPtr, 2);
    Result := xStrPtr^;
  end;

begin
  if RefCount <> 1 then
  begin
    Unique;
  end;
  FRefCheck := '!';
end;

function TSynaBytes.Clone: TSynaBytes;
begin
  Result.Length := Self.Length;
  Move(FBytes[0], Result.FBytes[0], Self.Length);
end;

function TSynaBytes.Data: Pointer;
begin
  Result := @FBytes[0];
end;

// zero based
procedure TSynaBytes.Delete(Start, Count: Integer);
begin
  if Count <= 0 then
    Exit;
  CheckCow;
  if Length - Count <= 0 then
  begin
    Length := 0;
    Exit;
  end;
  if (Start >= 0) then
  begin
    Move(FBytes[Start + Count], FBytes[Start], Length - Count);
    Length := Length - Count;
  end;
end;

class operator TSynaBytes.Equal(const V1, V2: TSynaBytes): Boolean;
begin
  Result := IsBytesEquals(V1.FBytes, V2.FBytes, V1.Length, V2.Length);
end;

class operator TSynaBytes.Explicit(const V1: TBytes): TSynaBytes;
begin
  Result.FBytes := Copy(V1);
  Result.Length := System.Length(V1);
end;

function TSynaBytes.GetChars(const Index: NativeInt): Char;
begin
  Result := Char(FBytes[Index]);
end;

function TSynaBytes.AGetLength: NativeInt;
begin
  Result := System.Length(FBytes);

  if Result > 0 then
    Result := Result - 1;  // Null Terminator
end;

class operator TSynaBytes.Implicit(const V1: Char): TSynaBytes;
begin
  Result.FBytes := TEncoding.Default.GetBytes(V1);
  Result.Length := System.Length(Result.FBytes);
end;

class operator TSynaBytes.Implicit(const V1: RawByteString): TSynaBytes;
var
  I: Integer;
begin
  Result.Length := System.Length(V1);
  for I := 1 to System.Length(V1) do
    Result.FBytes[I-1] := Byte(V1[I]);//warning: null-terminated strings!
end;

class operator TSynaBytes.Implicit(const V1: TSynaBytes): String;
var
  {$IFDEF MSWINDOWS}
  S: RawByteString;
  {$ELSE}
  I: Integer;
  C: PWord;
  {$ENDIF}
begin
  SetLength(Result, V1.Length);
  if V1.Length > 0 then
  begin
    //���������, 4873
  {$IFDEF MSWINDOWS}
    SetLength(s, V1.Length);
    Move(V1.FBytes[0], s[1], V1.Length);
    Result := string(s);
  //��� ���������, 7592
  {$ELSE}
    C := PWord(PWideChar(Result));
    for I := 0 to V1.Length-1 do
    begin
      C^ := V1.FBytes[I];
      Inc(C);
    end;
  {$ENDIF}
  end;
end;

class operator TSynaBytes.NotEqual(const V1, V2: TSynaBytes): Boolean;
begin
  Result := not IsBytesEquals(V1.FBytes, V2.FBytes, V1.Length, V2.Length);
end;

procedure TSynaBytes.SetChars(const Index: NativeInt; const Value: Char);
begin
  CheckCow;
  FBytes[Index] := Byte(Value);
end;

procedure TSynaBytes.Unique;
var
  b:TBytes;
begin
  SetLength(b, Self.Length + 1);
  Move(FBytes[0], b[0], Self.Length);
  FBytes := b;
end;

procedure TSynaBytes.UpdateTerminator;
begin
  if System.Length(FBytes) > 0 then
    FBytes[System.Length(FBytes) - 1] := 0;
end;

procedure TSynaBytes.ASetLength(const Value: NativeInt);
begin
  System.SetLength(FBytes, Value + 1); // +1, null terminator
  Self.UpdateTerminator();
end;
{$ENDIF}

function StringOf(const bytes: TSynaBytes):string;
begin
  Result := bytes;
end;

function StringOf(const bytes: TBytes):string;
{$IFDEF UNICODE}
var
  I: Integer;
  C: PWord;
begin
  SetLength(Result, Length(bytes));
  if Length(bytes) > 0 then
  begin
    C := PWord(PWideChar(Result));
    for I := 0 to Length(bytes)-1 do
    begin
      C^ := bytes[I];
      Inc(C);
    end;
  end;
{$ELSE}
begin
  SetLength(Result, Length(bytes));
  if Length(bytes) > 0 then
   Move(bytes[0], result[1], Length(bytes));
{$ENDIF}
end;

function StringOf(const bytes: PByte):string;
var
  count: Integer;
  buf: PByte;
{$IFDEF UNICODE}
  I: Integer;
  C: PWord;
{$ENDIF}
begin
  Count := 0;
  buf := bytes;
  while buf^<>0 do
  begin
   inc(count);
   inc(buf);
  end;
{$IFDEF UNICODE}
  SetLength(Result, count);
  if count > 0 then
  begin
    C := PWord(PWideChar(Result));
    for I := 0 to count-1 do
    begin
      C^ := bytes[I];
      Inc(C);
    end;
  end;
{$ELSE}
  SetLength(Result, count);
  Move(bytes^, result[1], count);
{$ENDIF}
end;

procedure DeleteInternal (var s: TSynaBytes; Start, Count: Integer);
begin
{$IFDEF UNICODE}
  s.Delete(Start - 1, Count);
{$ELSE}
  Delete(s, Start , Count);
{$ENDIF}
end;

{$IFNDEF DELPHI12_UP}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

end.
