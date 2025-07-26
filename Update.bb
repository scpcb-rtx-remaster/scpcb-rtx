; ID: 1816
; Author: Andres
; Date: 2006-09-18 11:05:21
; Title: Remote File Engine
; Description: Easy way to download and read remote files via HTTP/FTP

Global UserAgent$ = "SCPCB"	; What ever you'd like to name your application
Global ResponseDelay% = 2000		; How many millisecs to wait for HTTP response
Global NewLine$ = Chr(13) + Chr(10)	; Line break may differ
Const RFDebugMode% = False				; Debug mode

Type transporter
	Field id%
	Field code%
	Field filename$
	Field protocol$
End Type

Function OpenRemoteFile%(path$, port% = 80, variables$ = "", httpheader$ = "")
	Local protocol$ = Lower(Sector$(path$, ":", 0))
	Local host$ = Sector$(path$, "/", 2)
	Local ip$ = "www." + Sector$(host$, ".", Sectors%(host$, ".") - 1) + "." + Sector$(host$, ".", Sectors%(host$, "."))
	path$ = "/" + Sector$(path$, "/", 3, True)
	Local header%, bank%, tim%, occ%
	
	
	Local Stream% = OpenTCPStream(host$, port%)
	If Not Stream% Then Stream% = OpenTCPStream(ip$, port%)
	
	If Stream
		Select protocol$
			Case "http", "https" ; ---------------------------------------------------------------------------------------------------------------------------------------
				; Send request
				
				If Not Len(variables$) Then
					WriteLine(Stream, "GET " + path$ + " HTTP/1.1")
				Else
					WriteLine(Stream, "POST " + path$ + " HTTP/1.1")
				EndIf
				WriteLine(Stream, "Host: " + host$)
				WriteLine(Stream, "User-Agent: " + UserAgent$)
				If Len(variables$) Then
					WriteLine(Stream, "Content-Type: application/x-www-form-urlencoded")
					WriteLine(Stream, "Content-Length: " + Len(variables$))
				EndIf
				If Len(httpheader$) Then WriteLine(Stream, httpheader$)
				WriteLine(Stream, "Connection: Close")
				WriteLine(Stream, "")
				If Len(variables$) Then WriteLine(Stream, variables$)
				
				; Debug mode
				If RFDebugMode% Then
					If Not Len(variables$) Then DebugLog ">>> GET " + path$ + " HTTP/1.1" Else DebugLog ">>> POST " + path$ + " HTTP/1.1"
					DebugLog ">>> Host: " + host$
					DebugLog ">>> User-Agent: " + UserAgent$
					If Len(variables$) Then
						DebugLog ">>> Content-Type: application/x-www-form-urlencoded"
						DebugLog ">>> Content-Length: " + Len(variables$)
					EndIf
					If Len(httpheader$) Then DebugLog ">>> " + httpheader$
					DebugLog ">>> Connection: Close"
					DebugLog ">>> "
					If Len(variables$) Then DebugLog ">>> " + variables$
				EndIf
				
				; Wait for response
				tim = MilliSecs()
				Repeat
				Until (MilliSecs() - tim) => ResponseDelay% Or ReadAvail(Stream)
				
				txt$ = ReadLine$(Stream%)
				If RFDebugMode Then DebugLog "<<< " + txt$
				If Sector$(txt$, " ", 0) = "HTTP/1.1" Or Sector$(txt$, " ", 0) = "HTTP/1.0"
					code% = Int Sector$(txt$, " ", 1)
					If (code% => 300) Then
						CloseTCPStream Stream%
						Return False
					EndIf
				EndIf
				
				occ% = 0
				header% = CreateBank(0)
				bank% = CreateBank(12); Bank Int, Stream Int, Size Int
				
				PokeInt(bank%, 0, header%)
				PokeInt(bank%, 4, Stream%)
				PokeInt(bank%, 8, -1)
				
				this.transporter = New transporter
				this\id% = bank%
				this\code% = code%
				this\filename$ = Sector$(path$, "/", Sectors%(path$, "/"))
				this\protocol$ = "http"
				
				; Received HTTP Header
				Repeat
					txt$ = ReadRemoteLine$(bank%)
					value$ = Mid$(Sector$(txt$, ":", 1, True), 2)
					
					If RFDebugMode Then DebugLog "<<< " + txt$
					
					Select Sector$(txt$, ":", 0)
						Case ""
							Return bank%
						Case "Content-Disposition"
							For i = 0 To Sectors(value$, ";")
								subtxt$ = Trim$(Sector$(value$, ";", i))
								subvalue$ = Sector$(subtxt$, "=", 1)
								
								Select Sector$(subtxt$, "=", 0)
									Case "filename"
										this\filename$ = subvalue$
										DebugLog "[" + this\filename$ + "]"
								End Select
							Next
						Case "Content-Length"
							PokeInt bank%, 8, Int(value$)
					End Select
				Forever
			Case "ftp" ; ---------------------------------------------------------------------------------------------------------------------------------------
				header% = CreateBank(1)
				bank% = CreateBank(16); Bank Int, Stream Int, Size Int
				
				PokeInt(bank%, 0, header%)
				PokeInt(bank%, 4, Stream%)
				PokeInt(bank%, 8, -1)
				PokeInt(bank%, 12, Stream%)
				
				this.transporter = New transporter
				this\id% = bank%
				this\code% = "0"
				this\filename$ = Sector$(path$, "/", Sectors(path$, "/"))
				this\protocol$ = "ftp"
				
				; Wait for response
				tim = MilliSecs()
				Repeat
				Until (MilliSecs() - tim) => ResponseDelay% Or ReadAvail(Stream)
				
				Repeat
					If ReadAvail(Stream%)
						txt$ = ReadLine(Stream%)
						code% = Int Sector$(txt$, " ", 0)
						value$ = Mid$(Sector$(txt$, " ", 1, True), 1)
						If RFDebugMode Then DebugLog "<<< " + txt$
					Else
						txt$ = "":code% = 0:value$ = "":cmd$ = ""
					EndIf
					
					If Not ReadAvail(Stream%)
						Select code%
							Case 220
								cmd$ = "USER " + variables$
							Case 331
								cmd$ = "PASS " + httpheader$
							Case 230
								cmd$ = "SIZE " + path$
							Case 213
								PokeInt(bank%, 8, Int value$)
								cmd$ = "PASV"
							Case 227
								; Connect to PASV mode
								txt$ = Sector(txt$, "(", 1)
								host$ = Sector(txt$, ",", 0) + "." + Sector(txt$, ",", 1) + "." + Sector(txt$, ",", 2) + "." + Sector(txt$, ",", 3)
								port% = (Int Sector(txt$, ",", 4)) * 256 + (Int Left$(Sector(txt$, ",", 5), Len(Sector(txt$, ",", 5)) - 1))
								pasv_stream% = OpenTCPStream(host$, port%)
								
								; Update stream
								PokeInt(bank%, 4, pasv_stream%)
								
								cmd$ = "RETR " + path$
							Case 150
								Return bank%
						End Select
						
						; ERROR
						If code% => 400
							If Stream% Then CloseTCPStream Stream%
							If pasv_stream% Then CloseTCPStream pasv_stream%
							If header% Then FreeBank header%
							If bank% Then FreeBank bank%
							Return 0
						EndIf
					EndIf
					
					If Len(cmd$) Then
						If RFDebugMode% Then DebugLog ">>> " + cmd$
						WriteLine Stream%, cmd$
					EndIf
				Forever
				
				Return bank%
		End Select
	EndIf
End Function

Function CloseRemoteFile(bank%)
	If PeekInt(bank%, 0) Then FreeBank PeekInt(bank%, 0)
	If PeekInt(bank%, 4) Then CloseTCPStream PeekInt(bank%, 4)
	If BankSize(bank%) = 18 Then If PeekInt(bank%, 12)
		WriteLine PeekInt(bank%, 12), "BYE"
		If RFDebugMode% Then DebugLog ">>> BYE"
		CloseTCPStream PeekInt(bank%, 12)
	EndIf
	FreeBank bank%
	
	For this.transporter = Each transporter
		If this\id% = bank%
			Delete this
			Exit
		EndIf
	Next
End Function

Function EORF(bank%)
	If Not PeekInt(bank%, 4)
		If BankSize(PeekInt(bank%, 0)) = 0 Then Return True
	Else
		If Eof(PeekInt(bank%, 4)) Then Return True
	EndIf
End Function

Function RemoteFileSize%(bank%)
	Return PeekInt(bank%, 8)
End Function

Function RemoteFileName$(bank%)
	For this.transporter = Each transporter
		If this\id% = bank% Return this\filename$
		Next
End Function

Function RemoteFileCode%(bank%)
	For this.transporter = Each transporter
		If this\id% = bank% Return this\code%
		Next
End Function

Function RemoteFileProtocol$(bank%)
	For this.transporter = Each transporter
		If this\id% = bank% Return this\protocol$
		Next
End Function

Function ReadRemoteLine$(bank%)
	Local avail%, rbank% = PeekInt(bank%, 0), stream% = PeekInt(bank%, 4)
	
	; Update bank
	UpdateRemoteFile(bank%)
	
	; Read line
	txt$ = ""
	For i = 0 To BankSize(rbank%) - 1
		char% = PeekByte(rbank%, i)
		txt$ = txt$ + Chr(char%)
		If Right$(txt$, Len(NewLine$)) = NewLine$ Then
			Exit
		ElseIf Right$(txt$,1) = Chr(10) Then
			Exit
		EndIf
	Next
	
	If Right$(txt$, 2) = NewLine$ Then
		txt$ = Mid$(txt$, 1, Len(txt$) - Len(NewLine$))
	ElseIf Right$(txt$, 1) = Right$(NewLine$, 1) Then
		txt$ = Mid$(txt$, 1, Len(txt$) - (Len(NewLine$) - 1))
	EndIf
	
	; Resize bank
	size% = BankSize(rbank%) - (Len(txt$) + Len(NewLine$))
	If size% < 0 Then size% = 0
	ResizeRemoteBank(bank%, size%)
	
	Return txt$
End Function

Function ReadRemoteString$(bank%)
	Local rbank% = PeekInt(bank%, 0)
	UpdateRemoteFile(bank%)
	
	a% = PeekInt(rbank%, 0)
	
	txt$ = ""
	For i = 0 To a% - 1
		txt$ = txt$ + Chr(PeekByte(rbank%, 4 + i))
	Next
	
	ResizeRemoteBank(bank%, BankSize(rbank%) - (4 + a%))
	Return txt$
End Function

Function ReadRemoteInt%(bank%)
	Local rbank% = PeekInt(bank%, 0)
	UpdateRemoteFile(bank%)
	
	a% = PeekInt(rbank%, 0)
	ResizeRemoteBank(bank%, BankSize(rbank%) - 4)
	Return a%
End Function

Function ReadRemoteShort%(bank%)
	Local rbank% = PeekInt(bank%, 0)
	UpdateRemoteFile(bank%)
	
	a% = PeekShort(rbank%, 0)
	ResizeRemoteBank(bank%, BankSize(rbank%) - 2)
	Return a%
End Function

Function ReadRemoteByte%(bank%)
	Local rbank% = PeekInt(bank%, 0)
	UpdateRemoteFile(bank%)
	
	a% = PeekByte(rbank%, 0)
	ResizeRemoteBank(bank%, BankSize(rbank%) - 1)
	Return a%
End Function

Function WriteRemoteBytes(bank%, file%, offset%, count%)
	Local rbank% = PeekInt(bank%, 0)
	UpdateRemoteFile(bank%)
	
	Local N% = WriteBytes(rbank%, file%, offset%, count%)
	ResizeRemoteBank(bank%, BankSize(rbank%) - N%)
	
	Return N%
End Function

Function RemoteReadAvail(bank%)
	UpdateRemoteFile(bank%)
	received% = BankSize(PeekInt(bank%, 0))
	If PeekInt(bank%, 4) Then waiting% = ReadAvail(PeekInt(bank%, 4)) Else waiting% = 0
	Return received% + waiting%
End Function

Function UpdateRemoteFile(bank%)
	Local rbank% = PeekInt(bank%, 0), stream% = PeekInt(bank%, 4)
	
	If stream% And rbank%
		avail% = ReadAvail(stream%)
		offset% = BankSize(rbank%)
		
		ResizeBank(rbank%, offset% + avail%)
		ReadBytes(rbank%, stream%, offset%, avail%)
		
		If Eof(stream%) Then
			CloseTCPStream stream%
			PokeInt bank%, 4, 0
		EndIf
	EndIf
End Function

Function ResizeRemoteBank(bank%, size%)
	Local rbank% = PeekInt(bank%, 0), start% = BankSize(rbank%) - size%
	
	If BankSize(rbank%)
		CopyBank(rbank%, start%, rbank%, 0, size%)
		ResizeBank(rbank%, size%)
	EndIf
End Function

Function Sector$(txt$, separator$, Sector%, toend% = False)
	Local result$ = "", occ
	For i = 1 To Len(txt$)
		If Mid$(txt$, i, 1) = separator$
			occ = occ + 1
			If toend% And occ% > Sector% Then result$ = result$ + Mid$(txt$, i, 1)
		Else
			If occ => Sector Then result$ = result$ + Mid$(txt$, i, 1)
		EndIf
		If Not toend% Then If occ > Sector Then Exit
	Next
	Return result$
End Function

Function Sectors%(txt$, needle$)
	occ% = 0
	For i = 1 To Len(txt$) Step 1
		If Instr(txt$, needle$, i)
			occ% = occ% + 1
			i = Instr(txt$, needle$, i)
		Else
			Exit
		EndIf
	Next
	Return occ%
End Function




;=============================================================================================


;link$      - The link. You may enter the link just like you enter it
;             in your browser. Very tolerant. No http:// required.
;savepath$  - The path where the file should be saved
;savefile$  - The filename of the saved file. When given "", it will
;             be named like the file in the link$.

Const DOWNLOAD_SIZE% = 4096*2

Function Download(link$, savepath$ = "", savefile$ = "", latest$="")
;Strip protocol and return false if not "http"
	inst = Instr(link$, "://")
	If inst Then
		If Lower(Trim(Left(link$, inst - 1))) <> "http" Then Return False
		link$ = Right(link$, Len(link$) - inst - 2)
	EndIf
	
;Seperate host from link
	inst = Instr(link$, "/")
	If inst = 0 Then Return False
	host$ = Trim(Left(link$, inst - 1))
	link$ = Trim(Right(link$, Len(link$) - inst + 1))
	
;Seperate path and file from the link
	For i = Len(link$) To 1 Step -1
		If Mid(link$, i, 1) = "/" Then
			link_path$ = Trim(Left(link$, i))
			link_file$ = Right(link$, Len(link$) - i)
			Exit
		EndIf
	Next
	If link_file$ = "" Then Return False
	If savefile$ = "" Then savefile$ = link_file$
	
;Open TCP stream
	tcp = OpenTCPStream(host$, 80)
	If tcp = 0 Then Return False
	WriteLine tcp, "GET " + link_path$ + link_file$ + " HTTP/1.1" + Chr(13) + Chr(10) + "Host: " + host$ + Chr(13) + Chr(10) + "User-Agent: Download_Function_By_DevilsChild" + Chr(13) + Chr(10)
	
;Download file
	l$ = ReadLine(tcp)
	inst1 = Instr(l$, " ")
	inst2 = Instr(l$, " ", inst1 + 1)
	num = Mid(l$, inst1, inst2 - inst1)
	Select num
		Case 200
			conlen = -1
			chunk = False
			
			Repeat
				l$ = Trim(ReadLine(tcp))
				If l$ = "" Then Exit
				
				inst = Instr(l$, ":")
				l1$ = Trim(Left(l$, inst - 1))
				l2$ = Trim(Right(l$, Len(l$) - inst))
				Select Lower(l1$)
					Case "content-length"
						conlen = l2$
					Case "transfer-encoding"
						If Lower(l2$) = "chunked" Then chunk = True
				End Select
			Forever
			
			If conlen = 0 Then
				file = WriteFile(savepath$ + savefile$)
				CloseFile file
				CloseTCPStream tcp
				Return False ;file doesn't exist ;True
			ElseIf conlen > 0 Then
				file = WriteFile(savepath$ + savefile$)
				bnk = CreateBank(DOWNLOAD_SIZE)
				pos = 0
				Repeat
					avail = conlen - pos
					If avail > DOWNLOAD_SIZE Then
						ReadBytes bnk, tcp, 0, DOWNLOAD_SIZE
						WriteBytes bnk, file, 0, DOWNLOAD_SIZE
						pos = pos + DOWNLOAD_SIZE
						
						;draw the progress bar
						
						SetBuffer BackBuffer()
						Cls
						
						Text 5,5,"Downloading "+latest
						
						Color 255,255,255
						Text 5,165,Str(Floor(((Float(pos)/1024.0)/1024.0)*100.0)/100.0)+"MB out of "+Str(Floor(((Float(conlen)/1024.0)/1024.0)*100.0)/100.0)+"MB downloaded"
						
						Rect 14,198,614,18,False
						For i=0 To Int((Float(pos)/Float(conlen))*61.0)-1
							DrawImage(BlinkMeterIMG, (i*10)+17, 200)
						Next
						
						Text 320,230,Int((Float(pos)/Float(conlen))*100.0)+"%",True,False
						
						If DrawButton2(270,400,100,20,"Cancel",False) Then
							CloseTCPStream(tcp)
							FreeBank bnk
							CloseFile file
							Return -1
						EndIf
						
						Flip False
						
					Else
						ReadBytes bnk, tcp, 0, avail
						WriteBytes bnk, file, 0, avail
						Exit
					EndIf
				Forever
				FreeBank bnk
				CloseFile file
				CloseTCPStream tcp
				Return True
			ElseIf chunk Then
				file = WriteFile(savepath$ + savefile$)
				bnk = CreateBank(DOWNLOAD_SIZE)
				
				Repeat
					l$ = Trim(Upper(ReadLine(tcp)))
					ln = 0
					For i = 1 To Len(l$)
						ln = 16 * ln + Instr("123456789ABCDEF", Mid$(l$, i, 1))
					Next
					If ln = 0 Then Exit
					
					If BankSize(bnk) < ln Then ResizeBank bnk, ln
					ReadBytes bnk, tcp, 0, ln
					WriteBytes bnk, file, 0, ln
					ReadShort(tcp)
				Forever
				
				FreeBank bnk
				CloseFile file
				CloseTCPStream tcp
				Return True
			Else
				CloseTCPStream tcp
				Return False
			EndIf
		Case 301, 302
			Repeat
				l$ = Trim(ReadLine(tcp))
				If l$ = "" Then Exit
				
				inst = Instr(l$, ":")
				l1$ = Trim(Left(l$, inst - 1))
				l2$ = Trim(Right(l$, Len(l$) - inst))
				Select Lower(l1$)
					Case "location"
						CloseTCPStream tcp
						Return Download(l2$, savepath$, savefile$)
				End Select
			Forever
		Default
			CloseTCPStream tcp
			Return False
	End Select
End Function

Global UpdaterBG

Type ChangeLogLines
	Field txt$
End Type

Global UpdaterIMG
Global LinesAmount% = 0

Function GetSelfEXEName$() ;use this to make the new version delete the old exe
	Local lpFileName% = CreateBank(256)
	Local str_len% = api_GetModuleFileName(0,lpFileName,BankSize(lpFileName))
	
	Local exe_name$=""
	
	If str_len > 0 Then
		For grab_exe_name = 0 To str_len-1
			exe_name$ = exe_name + Chr(PeekByte(lpFileName,grab_exe_name))
		Next
	EndIf
	
	FreeBank lpFileName
	
	Return exe_name
End Function












;~IDEal Editor Parameters:
;~F#B#12#C4#D6#DE#E2#E8#EE#F4#110#11F#128#131#13A#144#14B#15C#165#173#18E
;~C#Blitz3D