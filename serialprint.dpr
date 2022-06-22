program serialprint;

{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  StrUtils;

function WriteFileToSerial(
  sFileName: string;
  sPortName: string;
  dwBaudRate: DWORD;
  bByteSize: Byte;
  bParity: Byte;
  bStopBits: Byte;
  bDtrRts: Boolean
): Boolean;
const
  iInputBufferSize = 256;
  iOutputBufferSize = 256;
var
  cFileName: array [0..255] of Char;
  FDCB: TDCB;
  ctTimeouts: TCommTimeouts;
  hPort, hFile: THandle;
  dwBytesRead, dwBytesWritten: DWORD;
  buffer: array[1..iInputBufferSize] of AnsiChar;
begin
  StrPCopy(cFileName, '\\.\' + sPortName);
  hPort := CreateFile(
    cFileName,
    GENERIC_READ or GENERIC_WRITE,
    0,
    NIL,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
  Result := False;
  try
    if hPort = INVALID_HANDLE_VALUE then Exit;
    if SetupComm(hPort, iInputBufferSize, iOutputBufferSize) = False then Exit;
    if GetCommState(hPort, FDCB) = False then Exit;
    with FDCB do
    begin
      BaudRate := dwBaudRate;
      ByteSize := bByteSize;
      Parity := bParity;
      StopBits := bStopBits;
      Flags := Flags or 1;
    end;
    if SetCommState(hPort, FDCB) = False then Exit;
    with ctTimeouts do
    begin
      ReadIntervalTimeout := MAXDWORD;
      ReadTotalTimeoutMultiplier := 0;
      ReadTotalTimeoutConstant := 0;
      WriteTotalTimeoutMultiplier := 0;
      WriteTotalTimeoutConstant := 0;
    end;
    SetCommTimeouts(hPort, ctTimeouts);
    if bDtrRts then
    begin
      EscapeCommFunction(hPort, SETDTR);
      EscapeCommFunction(hPort, SETRTS);
    end else begin
      EscapeCommFunction(hPort, CLRDTR);
      EscapeCommFunction(hPort, CLRRTS);
    end;
    StrPCopy(cFileName, sFileName);
    hFile := CreateFile(
      cFileName,
      GENERIC_READ,
      0,
      NIL,
      OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL,
      0
    );
    try
      WriteFile(hPort, buffer, 1, dwBytesWritten, nil);
      while True do
      begin
        if not ReadFile(hFile, buffer, SizeOf(buffer), dwBytesRead, nil) then
          break;
        if dwBytesRead = 0 then
          break;
        WriteFile(hPort, buffer, dwBytesRead, dwBytesWritten, nil);
      end;
      // cut page ESC @ GS V 1
      // https://reference.epson-biz.com/modules/ref_escpos/index.php?content_id=87
      StrPCopy(PAnsiChar(@buffer), #10#10#10#10#10#27 + '@' + #29 + 'V' + #1);
      WriteFile(hPort, buffer, 10, dwBytesWritten, nil);
      Result := True;
    finally
      CloseHandle(hFile);
    end;
  finally
    CloseHandle(hPort);
  end;
end;

function SplitString(var arr: array of string; str_src: string): Integer;
var
  delimiter, s: string;
  current_position: Integer;
  prev_position: Integer;
begin
  delimiter := ' ';
  Result := 0;
  current_position := -1;
  while True do
  begin
    prev_position := current_position;
    current_position := PosEx(delimiter, str_src, current_position + 1);
    if current_position = 0 then  // last item
    begin
      s := Copy(
        str_src,
        prev_position + 1,
        Length(str_src) - prev_position
      );
      arr[Result] := s;
      Inc(Result);
      break;
    end;
    if prev_position = -1 then
    begin
      s := Copy(
        str_src,
        0,
        current_position - 1
      );
    end else begin
      s := Copy(
        str_src,
        prev_position + 1,
        current_position - prev_position - 1
      );
    end;
    arr[Result] := s;
    Inc(Result);
  end;
end;

procedure main();
var
  txtConfigFile: TextFile;
  sConfigFileName: string;
  sConfigLine: string;
  asFields: array [0..5] of string;
  sFileName: string;
begin
  sConfigFileName := ChangeFileExt(ParamStr(0), '.config');
  if ParamCount = 0 then
    sFileName := ExtractFilePath(ParamStr(0)) + 'data.txt'
  else
    sFileName := ParamStr(ParamCount);
  WriteLn('SC MED LAD SRL');
  WriteLn('Config file: ' + sConfigFileName);
  WriteLn('Data file: ' + sFileName);

  AssignFile(txtConfigFile, sConfigFileName);
  Reset(txtConfigFile);
  try
    while not EOF(txtConfigFile) do
    begin
      ReadLn(txtConfigFile, sConfigLine);
      if sConfigLine = '' then
        break;

      if SplitString(asFields, sConfigLine) = 6 then
      begin
        WriteLn('Writing ' + sFileName + ' to ' + sConfigLine);
        WriteFileToSerial(
          sFileName,
          asFields[0],
          StrToInt(asFields[1]),
          StrToInt(asFields[2]),
          StrToInt(asFields[3]),
          StrToInt(asFields[4]),
          StrToInt(asFields[5]) = 1
        );
      end;
    end;
  finally
    CloseFile(txtConfigFile);
  end;
end;

begin
  main();
end.
