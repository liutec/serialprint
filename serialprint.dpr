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
  bDtrRts: Boolean;
  sNumber: string;
  bDebug: Boolean
): Boolean;
const
  dwInputBufferSize = 512;
  dwOutputBufferSize = 512;
  dwLineBufferSize = 1024;
var
  cFileName: array [0..255] of Char;
  FDCB: TDCB;
  ctTimeouts: TCommTimeouts;
  hPort, hFile: THandle;
  dwBytesRead, dwBytesToWrite, dwBytesWritten: DWORD;
  readBuffer: array[0..dwInputBufferSize-1] of AnsiChar;
  writeBuffer: array[0..dwOutputBufferSize-1] of AnsiChar;
  lineBuffer: array[0..dwLineBufferSize-1] of AnsiChar;
  i, k: DWORD;
  d, n: Integer;
  dwLastReadBufferOffset: DWORD;
  dwLineBufferChunkSize: DWORD;
  dwLineBufferOffset: DWORD;
  dwReadBufferOffset: DWORD;
  bFailed: Boolean;
  bEOL: Boolean;
  bEOF: Boolean;
begin
  StrPCopy(cFileName, '\\.\' + sPortName);
  if bDebug then
    hPort := GetStdHandle(STD_OUTPUT_HANDLE)
  else begin
    hPort := CreateFile(
      cFileName,
      GENERIC_READ or GENERIC_WRITE,
      0,
      NIL,
      OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL,
      0
    );
  end;
  Result := False;
  try
    if not bDebug then
    begin
      if hPort = INVALID_HANDLE_VALUE then Exit;
      if SetupComm(hPort, dwInputBufferSize, dwOutputBufferSize) = False then Exit;
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
        WriteTotalTimeoutMultiplier := 5;
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
      bFailed := False;
      dwLineBufferOffset := 0;
      repeat
        if ReadFile(hFile, readBuffer, SizeOf(readBuffer), dwBytesRead, nil) then
          bEOF := dwBytesRead = 0
        else begin
          bEOF := True;
          dwBytesRead := 0;
        end;
        if dwBytesRead > 0 then
          dwLastReadBufferOffset := dwBytesRead - 1
        else
          dwLastReadBufferOffset := 0;
        dwReadBufferOffset := 0;
        i := 0;
        repeat
          if bEOF then
          begin
            dwBytesToWrite := dwLineBufferOffset;
            bEOL := True;
          end else begin
            bEOL := Ord(readBuffer[i]) = 10;
            if bEOL or (i = dwLastReadBufferOffset) then
            begin
              dwLineBufferChunkSize := i - dwReadBufferOffset + 1;
              dwBytesToWrite := dwLineBufferOffset + dwLineBufferChunkSize;
              CopyMemory(
                @lineBuffer[dwLineBufferOffset],
                @readBuffer[dwReadBufferOffset],
                dwLineBufferChunkSize
              );
              dwReadBufferOffset := i + 1;
              if not bEOL then
                dwLineBufferOffset := dwLineBufferOffset + dwLineBufferChunkSize;
            end else
              dwBytesToWrite := 0;
          end;
          if bEOL and (dwBytesToWrite > 0) then
          begin
            k := Pos('COMANDA BON : ', string(lineBuffer));
            if k > 0 then
            begin
              k := k + 13;
              n := 0;
              while (Ord(lineBuffer[k]) >= Ord('0')) and (Ord(lineBuffer[k]) <= Ord('9')) do
              begin
                Inc(k);
                Inc(n);
              end;
              d := Length(sNumber) - n;
              if d <> 0 then
              begin
                CopyMemory(
                  @lineBuffer[k + DWORD(d)],
                  @lineBuffer[k],
                  dwBytesToWrite - k
                );
              end;
              CopyMemory(
                @lineBuffer[k - DWORD(n)],
                @sNumber[1],
                Length(sNumber)
              );
              dwBytesToWrite := dwBytesToWrite + DWORD(d);
            end else begin
              k := Pos('powered by SOFTOK.RO', string(lineBuffer));
              if k > 0 then
              begin
                StrPCopy(PAnsiChar(@lineBuffer), '       LA O IDEE SRL'#13#10);
                dwBytesToWrite := 22;
              end;
            end;
            WriteFile(hPort, lineBuffer, dwBytesToWrite, dwBytesWritten, nil);
            if dwBytesToWrite <> dwBytesWritten then
            begin
              WriteLn('ERROR: failed writting to ' + sPortName + '. Check connection.');
              bFailed := True;
              break;
            end;
            dwLineBufferOffset := 0
          end;
          Inc(i);
        until i > dwLastReadBufferOffset;
      until bEOF;
      if not bFailed then
      begin
        // cut page ESC @ GS V 1
        // https://reference.epson-biz.com/modules/ref_escpos/index.php?content_id=87
        StrPCopy(PAnsiChar(@writeBuffer), #10#10#10#10#10#27 + '@' + #29 + 'V' + #1);
        WriteFile(hPort, writeBuffer, 10, dwBytesWritten, nil);
        Result := True;
      end else
        Result := False;
    finally
      CloseHandle(hFile);
    end;
  finally
    if not bDebug then
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

function ReadLastDailyNumber(sFileName: string): Integer;
var
  fHandle: TextFile;
  sNumber: string;
begin
  AssignFile(fHandle, sFileName);
  try
    try
      Reset(fHandle);
      ReadLn(fHandle, sNumber);
      ReadLastDailyNumber := StrToInt(sNumber);
    except
      on E: EConvertError do begin
        ReadLastDailyNumber := 0;
      end;
    end;
  finally
    CloseFile(fHandle);
  end;
end;

procedure WriteLastDailyNumber(sFileName: string; sNumber: string);
var
  fHandle: TextFile;
begin
  AssignFile(fHandle, sFileName);
  try
    ReWrite(fHandle);
    WriteLn(fHandle, sNumber);
  finally
    CloseFile(fHandle);
  end;
end;

procedure main();
var
  i: Integer;
  txtConfigFile: TextFile;
  sConfigFileName: string;
  sConfigLine: string;
  asFields: array [0..5] of string;
  sFileName: string;
  sDailyNumberFileName: string;
  iNumber: Integer;
  sNumber: string;
  bDebug: Boolean;
begin
  sConfigFileName := ChangeFileExt(ParamStr(0), '.config');
  sDailyNumberFileName := ExtractFilePath(ParamStr(0)) +
    FormatDateTime('yyyy-MM-dd', Now()) + '.txt';
  iNumber := ReadLastDailyNumber(sDailyNumberFileName) + 1;
  sNumber := IntToStr(iNumber);
  bDebug := False;
  if ParamCount = 0 then
    sFileName := ExtractFilePath(ParamStr(0)) + 'data.txt'
  else begin
    for i := 1 to ParamCount do
      if ParamStr(i) = '-d' then
      begin
        bDebug := True;
        break;
      end;
    sFileName := ParamStr(ParamCount);
  end;
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
        WriteLn(#13#10'Writing ' + sFileName + ' to ' + sConfigLine);
        WriteFileToSerial(
          sFileName,
          asFields[0],
          StrToInt(asFields[1]),
          StrToInt(asFields[2]),
          StrToInt(asFields[3]),
          StrToInt(asFields[4]),
          StrToInt(asFields[5]) = 1,
          sNumber,
          bDebug
        );
      end;
    end;
  finally
    CloseFile(txtConfigFile);
  end;
  WriteLastDailyNumber(sDailyNumberFileName, sNumber);
end;

begin
  main();
end.
