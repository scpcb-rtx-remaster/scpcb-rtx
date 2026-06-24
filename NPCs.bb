;[Block]
Global Curr173.NPCs, Curr106.NPCs, Curr096.NPCs, Curr5131.NPCs, CurrD9341.NPCs
Const NPCtype173% = 1, NPCtypeOldMan% = 2, NPCtypeGuard% = 3, NPCtypeD% = 4, NPCtypeD9341% = 5
Const NPCtype372% = 6, NPCtypeApache% = 7, NPCtypeMTF% = 8, NPCtype096 = 9
Const NPCtype049% = 10, NPCtypeZombie% = 11, NPCtype5131% = 12, NPCtypeTentacle% = 13
Const NPCtype860% = 14, NPCtype939% = 15, NPCtype066% = 16, NPCtypePdPlane% = 17
Const NPCtype966% = 18, NPCtype1048a = 19, NPCtype1499% = 20, NPCtype008% = 21, NPCtypeClerk% = 22
Const NPCtypeD2% = 23, NPCtypeD3% = 24, NPCtypeS1% = 25, NPCtypeS2% = 26, NPCtypeJ% = 27
Const NPCtypeV% = 28, NPCtypeG% = 29, NPCtypeC% = 30, NPCtypeB1% = 31, NPCtypeB2% = 32, NPCtypeNazi% = 33
Const D9341F_KEY1% = 1, D9341F_KEY2% = 2, D9341F_OMNI% = 4, D9341F_LOCKDOWN% = 8
Const D9341F_008% = 16, D9341F_RCCOFF% = 32, D9341F_079% = 64, D9341F_RCCON% = 128, D9341F_GATEB% = 256, D9341F_TESTROOMCLOSE% = 512
Const D9341F_914DROP% = 1024, D9341F_914APPROACH% = 2048, D9341F_914STARTED% = 4096, D9341F_914OUTPUT% = 8192, D9341F_914OMNISPAWN% = 16384
Const D9341F_SERVERS% = 32768

Const NPCBones$ = "Data\NPCBones.ini"

Type NPCs
	Field obj%, obj2%, obj3%, obj4%, Collider%
	Field NPCtype%, ID%
	Field DropSpeed#, Gravity%
	Field State#, State2#, State3#, PrevState%
	Field MakingNoise%
	
	Field Frame#
	
	Field Angle#
	Field Sound%, SoundChn%, SoundTimer#
	Field Sound2%, SoundChn2%
	
	Field Speed#, CurrSpeed#
	
	Field texture$
	
	Field Idle#
	
	Field Reload#
	
	Field LastSeen%, LastDist#
	
	Field PrevX#, PrevY#, PrevZ#
	
	Field Target.NPCs, TargetID%
	Field EnemyX#, EnemyY#, EnemyZ#
	
	Field Path.WayPoints[20], PathStatus%, PathTimer#, PathLocation%
	
	Field NVX#,NVY#,NVZ#,NVName$
	
	Field GravityMult# = 1.0
	Field MaxGravity# = 0.2
	
	Field MTFVariant%
	Field MTFLeader.NPCs
	Field IsDead%
	Field BlinkTimer# = 1.0
	Field IgnorePlayer%
	
	Field ManipulateBone%
	Field ManipulationType%
	Field BoneToManipulate$
	Field BonePitch#
	Field BoneYaw#
	Field BoneRoll#
	Field NPCNameInSection$
	Field InFacility% = True
	Field CanUseElevator% = False
	Field CurrElevator.ElevatorObj
	Field DoorTarget.Doors
	Field LastDoor.Doors
	Field TeslaRoom.Rooms
	Field DoorRetry%, DoorWaitTimer#
	Field GoalTimer#, GoalX#, GoalZ#
	Field HP%
	Field PathX#,PathZ#
	Field Model$
	Field ModelScaleX#,ModelScaleY#,ModelScaleZ#
	Field HideFromNVG
	Field TextureID%=-1
	Field CollRadius#
	Field IdleTimer#
	Field SoundChn_IsStream%,SoundChn2_IsStream%
	Field FallingPickDistance#
	Field SafeRoom.Rooms
	Field SafeX#, SafeY#, SafeZ#
	Field BoundsTimer#
	Field FollowTimer#, FollowX#, FollowZ#
	Field WanderTimer#
End Type

Function CreateNPC.NPCs(NPCtype%, x#, y#, z#)
	Local n.NPCs = New NPCs, n2.NPCs
	Local temp#, i%, diff1, bump1, spec1
	Local sf, b, t1
	
	n\NPCtype = NPCtype
	n\GravityMult = 1.0
	n\MaxGravity = 0.2
	n\CollRadius = 0.2
	n\FallingPickDistance = 10
	Select NPCtype
		Case NPCtype173
			;[Block]
			n\NVName = I_Loc\NPC_173
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.23, 0.32
			EntityType n\Collider, HIT_PLAYER
			n\Gravity = True
			
			;inward3d
			If SCP173Model$ = "Remastered" Or "remastered" Or "remastered scp-173" Or "inward3d" Or "Inward3D" Or "Inward3d" Or "Inward 3D" Or "inward 3d scp-173" Or "inward3d scp-173" Or "inward scp-173" Or "173_2" Or "Wiki Accurate" Or "wiki accurate scp-173" Then 
				Loaded173Model$ = "Inward3D"
				n\obj = LoadMesh_Strict("GFX\npcs\173_2.b3d")
			EndIf
			
			If SCP173Model$ = "Izumi Kato" Or "izumikato" Or "untitled2004" Or "untitled 2004" Or "untitled 2004 scp-173" Then
				IzumiKato$ = "True"
				Loaded173Model$ = "Inward3D"
				n\obj = LoadMesh_Strict("GFX\npcs\173_2.b3d")
				IzumiKatoTex = Not IzumiKatoTex
				If IzumiKatoTex Then
					Local tex3 = LoadTexture_Strict("GFX\npcs\IzumiKatotexture.jpg", 1)
					EntityTexture Curr173\obj, tex3, 0, 0
					FreeTexture tex3
				EndIf 
			EndIf

			If SCP173Model$ = "Regalis" Or "regalis" Or "regalis scp-173" Or "og scp-173" Or "original scp-173" Or "vanilla" Or "vanilla scp-173" Or "undertow scp-173" Or "old scp-173" Then
				Loaded173Model$ = "Regalis"
				n\obj = LoadMesh_Strict("GFX\npcs\173Regalis.b3d")

				;On Halloween set jack-o-latern texture.
				If (Left(CurrentDate(), 7) = "31 Oct ") Then
					HalloweenTex = True
					Local texFestive = LoadTexture_Strict("GFX\npcs\173h.pt", 1)
					EntityTexture n\obj, texFestive, 0, 0
					FreeTexture texFestive
				EndIf
			ElseIf SCP173Model$ = "Peanut" Or "peanut" Or "peanut mode" Or "peanutmode" Or "peanut173" Or "Peanut" Or "Peanut mode" Or "Peanutmode" Or "Peanut173" Or "nut" Or "Nut"
			    Loaded173Model$ = "Peanut"
				n\obj = LoadMesh_Strict("GFX\npcs\173Peanut.b3d")
			Else 
				Loaded173Model$ = "Inward3D"
				n\obj = LoadMesh_Strict("GFX\npcs\173_2.b3d")
			EndIf
			
			temp# = (GetModdedINIFloat("DATA\NPCs.ini", "SCP-173", "scale") / MeshDepth(n\obj))			
			ScaleEntity n\obj, temp,temp,temp
			
			n\Speed = (GetModdedINIFloat("DATA\NPCs.ini", "SCP-173", "speed") / 100.0)
			
			n\obj2 = LoadMesh_Strict("GFX\173box.b3d")
			ScaleEntity n\obj2, RoomScale, RoomScale, RoomScale
			HideEntity n\obj2
			
			n\CollRadius = 0.32
			;[End Block]
		Case NPCtypeOldMan
			;[Block]
			n\NVName = I_Loc\NPC_106
			n\Collider = CreatePivot()
			n\GravityMult = 0.0
			n\MaxGravity = 0.0
			EntityRadius n\Collider, 0.2
			EntityType n\Collider, HIT_PLAYER
			n\obj = LoadAnimMesh_Strict("GFX\npcs\106_2.b3d")
			
			temp# = (GetModdedINIFloat("DATA\NPCs.ini", "SCP-106", "scale") / 2.2)		
			ScaleEntity n\obj, temp, temp, temp
			
			Local OldManEyes% = LoadTexture_Strict("GFX\npcs\oldmaneyes.jpg")
			
			n\Speed = (GetModdedINIFloat("DATA\NPCs.ini", "SCP-106", "speed") / 100.0)
			
			n\obj2 = CreateSprite()
			ScaleSprite(n\obj2, 0.03, 0.03)
			EntityTexture(n\obj2, OldManEyes)
			EntityBlend (n\obj2, 3)
			EntityFX(n\obj2, 1 + 8)
			SpriteViewMode(n\obj2, 2)
			
			FreeTexture OldManEyes%
			;[End Block]
		Case NPCtypeGuard
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.2
			;EntityRadius Collider, 0.15, 0.30
			EntityType n\Collider, HIT_PLAYER
			n\obj = CopyEntity(GuardObj) ;LoadAnimMesh_Strict("GFX\npcs\mtf.b3d")
			
			n\Speed = (GetModdedINIFloat("DATA\NPCs.ini", "Guard", "speed") / 100.0)
			temp# = (GetModdedINIFloat("DATA\NPCs.ini", "Guard", "scale") / 2.5)
			
			ScaleEntity n\obj, temp, temp, temp
			
			MeshCullBox (n\obj, -MeshWidth(GuardObj), -MeshHeight(GuardObj), -MeshDepth(GuardObj), MeshWidth(GuardObj)*2, MeshHeight(GuardObj)*2, MeshDepth(GuardObj)*2)
			;[End Block]
		Case NPCtypeMTF
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.2
			;EntityRadius Collider, 0.15, 0.30
			EntityType n\Collider, HIT_PLAYER
			;EntityPickMode n\Collider, 1
			n\obj = CopyEntity(MTFObj) ;LoadAnimMesh_Strict("GFX\npcs\mtf.b3d")
			
			n\Speed = (GetModdedINIFloat("DATA\NPCs.ini", "MTF", "speed") / 100.0)
			
			temp# = (GetModdedINIFloat("DATA\NPCs.ini", "MTF", "scale") / 2.5)
			
			ScaleEntity n\obj, temp, temp, temp
			
			MeshCullBox (n\obj, -MeshWidth(MTFObj), -MeshHeight(MTFObj), -MeshDepth(MTFObj), MeshWidth(MTFObj)*2, MeshHeight(MTFObj)*2, MeshDepth(MTFObj)*2) 
			
			If MTFSFX(0)=0 Then
				MTFSFX(0)=LoadSound_Strict("SFX\Character\MTF\ClassD1.ogg")
				MTFSFX(1)=LoadSound_Strict("SFX\Character\MTF\ClassD2.ogg")
				MTFSFX(2)=LoadSound_Strict("SFX\Character\MTF\ClassD3.ogg")			
				MTFSFX(3)=LoadSound_Strict("SFX\Character\MTF\ClassD4.ogg")
				;MTFSFX(4)=LoadSound_Strict("SFX\Character\MTF\Tesla0.ogg")
				MTFSFX(5)=LoadSound_Strict("SFX\Character\MTF\Beep.ogg")
				MTFSFX(6)=LoadSound_Strict("SFX\Character\MTF\Breath.ogg")
			EndIf
			If MTFrooms[6]=Null Then 
				For r.Rooms = Each Rooms
					Select Lower(r\RoomTemplate\Name)
						Case "room106"
							MTFrooms[0]=r
						Case "roompj"
							MTFrooms[1]=r	
						Case "room079"
							MTFrooms[2]=r	
						Case "room2poffices"
							MTFrooms[3]=r	
						Case "914"
							MTFrooms[4]=r	
						Case "coffin"
							MTFrooms[5]=r	
						Case "start"
							MTFrooms[6]=r
					End Select
				Next			
			EndIf
			;[End Block]
		Case NPCtypeD
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.32
			EntityType n\Collider, HIT_PLAYER
			
			n\obj = CopyEntity(ClassDObj)
			
			temp# = 0.5 / MeshWidth(n\obj)
			ScaleEntity n\obj, temp, temp, temp
			
			n\Speed = 2.0 / 100
			
			MeshCullBox (n\obj, -MeshWidth(ClassDObj), -MeshHeight(ClassDObj), -MeshDepth(ClassDObj)*4, MeshWidth(ClassDObj)*2, MeshHeight(ClassDObj)*2, MeshDepth(ClassDObj)*8)
			
			n\CollRadius = 0.32
			;[End Block]
		Case NPCtypeD2
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.32
			EntityType n\Collider, HIT_PLAYER
			
			n\obj = CopyEntity(ClassD2Obj)
			
			temp# = 0.5 / MeshWidth(n\obj)
			ScaleEntity n\obj, temp, temp, temp
			
			n\Speed = 2.0 / 100
			
			MeshCullBox (n\obj, -MeshWidth(ClassD2Obj), -MeshHeight(ClassD2Obj), -MeshDepth(ClassD2Obj)*4, MeshWidth(ClassD2Obj)*2, MeshHeight(ClassD2Obj)*2, MeshDepth(ClassD2Obj)*8)
			
			n\CollRadius = 0.32
			;[End Block]
		Case NPCtypeD3
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.32
			EntityType n\Collider, HIT_PLAYER
			
			n\obj = CopyEntity(ClassD3Obj)
			
			temp# = 0.5 / MeshWidth(n\obj)
			ScaleEntity n\obj, temp, temp, temp
			
			n\Speed = 2.0 / 100
			
			MeshCullBox (n\obj, -MeshWidth(ClassD3Obj), -MeshHeight(ClassD3Obj), -MeshDepth(ClassD3Obj)*4, MeshWidth(ClassD3Obj)*2, MeshHeight(ClassD3Obj)*2, MeshDepth(CClassD3Obj)*8)
			
			n\CollRadius = 0.32
			;[End Block]
		Case NPCtypeJ
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.32
			EntityType n\Collider, HIT_PLAYER
			
			n\obj = CopyEntity(JanitorObj)
			
			temp# = 0.5 / MeshWidth(n\obj)
			ScaleEntity n\obj, temp, temp, temp
			
			n\Speed = 2.0 / 100
			
			MeshCullBox (n\obj, -MeshWidth(JanitorObj), -MeshHeight(JanitorObj), -MeshDepth(JanitorObj)*4, MeshWidth(JanitorObj)*2, MeshHeight(JanitorObj)*2, MeshDepth(JanitorObj)*8)
			
			n\CollRadius = 0.32
			;[End Block]
		Case NPCtypeS1
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.32
			EntityType n\Collider, HIT_PLAYER
			
			n\obj = CopyEntity(Scientist1Obj)
			
			temp# = 0.5 / MeshWidth(n\obj)
			ScaleEntity n\obj, temp, temp, temp
			
			n\Speed = 2.0 / 100
			
			MeshCullBox (n\obj, -MeshWidth(Scientist1Obj), -MeshHeight(Scientist1Obj), -MeshDepth(Scientist1Obj)*4, MeshWidth(Scientist1Obj)*2, MeshHeight(Scientist1Obj)*2, MeshDepth(Scientist1Obj)*8)
			
			n\CollRadius = 0.32
			;[End Block]
		Case NPCtypeS2
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.32
			EntityType n\Collider, HIT_PLAYER
			
			n\obj = CopyEntity(Scientist2Obj)
			
			temp# = 0.5 / MeshWidth(n\obj)
			ScaleEntity n\obj, temp, temp, temp
			
			n\Speed = 2.0 / 100
			
			MeshCullBox (n\obj, -MeshWidth(Scientist2Obj), -MeshHeight(Scientist2Obj), -MeshDepth(Scientist2Obj)*4, MeshWidth(Scientist2Obj)*2, MeshHeight(Scientist2Obj)*2, MeshDepth(Scientist2Obj)*8)
			
			n\CollRadius = 0.32
			;[End Block]
		Case NPCtypeD9341
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.32
			EntityType n\Collider, HIT_PLAYER
			
			n\obj = CopyEntity(D9341Obj)
			
			temp# = 0.5 / MeshWidth(n\obj)
			ScaleEntity n\obj, temp, temp, temp
			
			n\Speed = 2.0 / 100
			
			MeshCullBox (n\obj, -MeshWidth(ClassDObj), -MeshHeight(ClassDObj), -MeshDepth(ClassDObj)*4, MeshWidth(ClassDObj)*2, MeshHeight(ClassDObj)*2, MeshDepth(ClassDObj)*8)
			
			n\CollRadius = 0.32
			;[End Block]
		Case NPCtypeC
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.32
			EntityType n\Collider, HIT_PLAYER
			
			n\obj = CopyEntity(CorpseObj)
			
			temp# = 0.5 / MeshWidth(n\obj)
			ScaleEntity n\obj, temp, temp, temp
			
			n\Speed = 2.0 / 100
			
			MeshCullBox (n\obj, -MeshWidth(CorpseObj), -MeshHeight(CorpseObj), -MeshDepth(CorpseObj)*4, MeshWidth(CorpseObj)*2, MeshHeight(CorpseObj)*2, MeshDepth(CorpseObj)*8)
			
			n\CollRadius = 0.32
			n\IsDead = True
			EntityType n\Collider, HIT_DEAD
			SetNPCFrame(n, 80)
			;[End Block]
		Case NPCtypeV
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.32
			EntityType n\Collider, HIT_PLAYER
			
			n\obj = CopyEntity(Victim106Obj)
			
			temp# = 0.5 / MeshWidth(n\obj)
			ScaleEntity n\obj, temp, temp, temp
			
			n\Speed = 2.0 / 100
			
			MeshCullBox (n\obj, -MeshWidth(Victim106Obj), -MeshHeight(Victim106Obj), -MeshDepth(Victim106Obj)*4, MeshWidth(Victim106Obj)*2, MeshHeight(Victim106Obj)*2, MeshDepth(Victim106Obj)*8)
			
			n\CollRadius = 0.32
			n\IsDead = True
			EntityType n\Collider, HIT_DEAD
			SetNPCFrame(n, 1)
			;[End Block]
		Case NPCtypeB2
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.32
			EntityType n\Collider, HIT_PLAYER
			
			n\obj = CopyEntity(Body2Obj)
			
			temp# = 0.5 / MeshWidth(n\obj)
			ScaleEntity n\obj, temp, temp, temp
			
			n\Speed = 2.0 / 100
			
			MeshCullBox (n\obj, -MeshWidth(Body2Obj), -MeshHeight(Body2Obj), -MeshDepth(Body2Obj)*4, MeshWidth(Body2Obj)*2, MeshHeight(Body2Obj)*2, MeshDepth(Body2Obj)*8)
			
			n\CollRadius = 0.32
			n\State = 3
			n\IsDead = True
			EntityType n\Collider, HIT_DEAD
			SetNPCFrame(n, 40)
			;[End Block]
		Case NPCtypeB1
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.32
			EntityType n\Collider, HIT_PLAYER
			
			n\obj = CopyEntity(Body1Obj)
			
			temp# = 0.5 / MeshWidth(n\obj)
			ScaleEntity n\obj, temp, temp, temp
			
			n\Speed = 2.0 / 100
			
			MeshCullBox (n\obj, -MeshWidth(Body1Obj), -MeshHeight(Body1Obj), -MeshDepth(Body1Obj)*4, MeshWidth(Body1Obj)*2, MeshHeight(Body1Obj)*2, MeshDepth(Body1Obj)*8)
			
			n\CollRadius = 0.32
			n\State = 3
			n\IsDead = True
			EntityType n\Collider, HIT_DEAD
			SetNPCFrame(n, 80)
			;[End Block]
		Case NPCtypeG
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.32
			EntityType n\Collider, HIT_PLAYER
			
			n\obj = CopyEntity(GonzalesObj)
			
			temp# = 0.5 / MeshWidth(n\obj)
			ScaleEntity n\obj, temp, temp, temp
			
			n\Speed = 2.0 / 100
			
			MeshCullBox (n\obj, -MeshWidth(GonzalesObj), -MeshHeight(GonzalesObj), -MeshDepth(GonzalesObj)*4, MeshWidth(GonzalesObj)*2, MeshHeight(GonzalesObj)*2, MeshDepth(GonzalesObj)*8)
			
			n\CollRadius = 0.32
			n\State = 3
			n\IsDead = True
			EntityType n\Collider, HIT_DEAD
			SetNPCFrame(n, 80)
			;[End Block]
		Case NPCtypeNazi
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.32
			EntityType n\Collider, HIT_PLAYER
			
			n\obj = LoadAnimMesh_Strict("GFX\npcs\naziofficer.b3d")
			
			temp# = 0.5 / MeshWidth(n\obj)
			ScaleEntity n\obj, temp, temp, temp
			
			n\Speed = 2.0 / 100
			
			MeshCullBox (n\obj, -MeshWidth(n\obj), -MeshHeight(n\obj), -MeshDepth(n\obj)*4, MeshWidth(n\obj)*2, MeshHeight(n\obj)*2, MeshDepth(n\obj)*8)
			
			n\CollRadius = 0.32
			;[End Block]
		Case NPCtype372
			;[Block]
			n\NVName = I_Loc\NPC_372
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.2
			n\obj = LoadAnimMesh_Strict("GFX\npcs\372.b3d")
			
			temp# = 0.35 / MeshWidth(n\obj)
			ScaleEntity n\obj, temp, temp, temp
			;[End Block]
		Case NPCtype5131
			;[Block]
			n\NVName = I_Loc\NPC_5131
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.2
			n\obj = LoadAnimMesh_Strict("GFX\npcs\bll.b3d")
			
			n\obj2 = CopyEntity (n\obj)
			EntityAlpha n\obj2, 0.6
			
			temp# = 1.8 / MeshWidth(n\obj)
			ScaleEntity n\obj, temp, temp, temp
			ScaleEntity n\obj2, temp, temp, temp
			;[End Block]
		Case NPCtype096
			;[Block]
			n\NVName = I_Loc\NPC_096
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.26
			EntityType n\Collider, HIT_PLAYER
			n\obj = LoadAnimMesh_Strict("GFX\npcs\scp096.b3d")
			n\obj2 = CreatePivot(FindChild(n\obj, GetModdedINIString(NPCBones, "SCP-096", "face_bonename")))
			
			n\Speed = (GetModdedINIFloat("DATA\NPCs.ini", "SCP-096", "speed") / 100.0)
			
			temp# = (GetModdedINIFloat("DATA\NPCs.ini", "SCP-096", "scale") / 3.0)
			ScaleEntity n\obj, temp, temp, temp	
			
			MeshCullBox (n\obj, -MeshWidth(n\obj)*2, -MeshHeight(n\obj)*2, -MeshDepth(n\obj)*2, MeshWidth(n\obj)*2, MeshHeight(n\obj)*4, MeshDepth(n\obj)*4)
			
			n\CollRadius = 0.26
			;[End Block]
		Case NPCtype049
			;[Block]
			n\NVName = I_Loc\NPC_049
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.2
			EntityType n\Collider, HIT_PLAYER
			;n\obj = LoadAnimMesh_Strict("GFX\npcs\scp-049.b3d")
			n\obj = CopyEntity(NPC049OBJ)
			
			n\Speed = (GetModdedINIFloat("DATA\NPCs.ini", "SCP-049", "speed") / 100.0)
			
			temp# = GetModdedINIFloat("DATA\NPCs.ini", "SCP-049", "scale")
			ScaleEntity n\obj, temp, temp, temp	
			
			n\Sound = LoadSound_Strict("SFX\Horror\Horror12.ogg")
			
			If HorrorSFX(13)=0 Then HorrorSFX(13)=LoadSound_Strict("SFX\Horror\Horror13.ogg")
			
			n\CanUseElevator = True
			;[End Block]
		Case NPCtypeZombie
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.2
			EntityType n\Collider, HIT_PLAYER
			
			;For n2.NPCs = Each NPCs
			;	If n\NPCtype = n2\NPCtype And n<>n2 Then
			;		n\obj = CopyEntity (n2\obj)
			;		Exit
			;	EndIf
			;Next
			
			If n\obj = 0 Then 
				;n\obj = LoadAnimMesh_Strict("GFX\npcs\zombie1.b3d")
				n\obj = CopyEntity(NPC0492OBJ)
				
				temp# = (GetModdedINIFloat("DATA\NPCs.ini", "SCP-049-2", "scale") / 2.5)
				ScaleEntity n\obj, temp, temp, temp
				
				MeshCullBox (n\obj, -MeshWidth(n\obj), -MeshHeight(n\obj), -MeshDepth(n\obj)*1.5, MeshWidth(n\obj)*2, MeshHeight(n\obj)*2, MeshDepth(n\obj)*3)
			EndIf
			
			n\Speed = (GetModdedINIFloat("DATA\NPCs.ini", "SCP-049-2", "speed") / 100.0)
			
			SetAnimTime(n\obj, 107)
			
			n\Sound = LoadSound_Strict("SFX\SCP\049\0492Breath.ogg")
			
			n\HP = 100
			;[End Block]
		Case NPCtypeApache
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\GravityMult = 0.0
			n\MaxGravity = 0.0
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.2
			n\obj = CopyEntity(ApacheObj);LoadAnimMesh_Strict("GFX\apache.b3d")
			
			n\obj2 = CopyEntity(ApacheRotorObj);LoadAnimMesh_Strict("GFX\apacherotor.b3d",n\obj)
			EntityParent n\obj2,n\obj
			
			For i = -1 To 1 Step 2
				Local rotor2 = CopyEntity(n\obj2,n\obj2)
				RotateEntity rotor2,0,4.0*i,0
				EntityAlpha rotor2, 0.5
			Next
			
			n\obj3 = LoadAnimMesh_Strict("GFX\apacherotor2.b3d",n\obj)
			PositionEntity n\obj3, 0.0, 2.15, -5.48
			
			EntityType n\Collider, HIT_APACHE
			EntityRadius n\Collider, 3.0
			
			For i = -1 To 1 Step 2
				Local Light1 = CreateLight(2,n\obj)
				;room\LightDist[i] = range
				LightRange(Light1,2.0)
				LightColor(Light1,255,255,255)
				PositionEntity(Light1, 1.65*i, 1.17, -0.25)
				
				Local lightsprite = CreateSprite(n\obj)
				PositionEntity(lightsprite, 1.65*i, 1.17, 0, -0.25)
				ScaleSprite(lightsprite, 0.13, 0.13)
				EntityTexture(lightsprite, LightSpriteTex(0))
				EntityBlend (lightsprite, 3)
				EntityFX lightsprite, 1+8				
			Next
			
			temp# = 0.6
			ScaleEntity n\obj, temp, temp, temp
			;[End Block]
		Case NPCtypeTentacle
			;[Block]
			n\NVName = I_Loc\NPC_Unidentified
			
			n\Collider = CreatePivot()
			
			For n2.NPCs = Each NPCs
				If n\NPCtype = n2\NPCtype And n<>n2 Then
					n\obj = CopyEntity (n2\obj)
					Exit
				EndIf
			Next
			
			If n\obj = 0 Then 
				n\obj = LoadAnimMesh_Strict("GFX\NPCs\035tentacle.b3d")
				ScaleEntity n\obj, 0.065,0.065,0.065
			EndIf
			
			SetAnimTime n\obj, 283
			;[End Block]
		Case NPCtype860
			;[Block]
			n\NVName = I_Loc\NPC_Unidentified
			
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.25
			EntityType n\Collider, HIT_PLAYER
			n\obj = LoadAnimMesh_Strict("GFX\npcs\forestmonster.b3d")
			
			EntityFX(n\obj, 1)
			
			tex = LoadTexture_Strict("GFX\npcs\860_eyes.png",1+2)
			
			n\obj2 = CreateSprite()
			ScaleSprite(n\obj2, 0.1, 0.1)
			EntityTexture(n\obj2, tex)
			FreeTexture tex
			
			EntityFX(n\obj2, 1 + 8)
			EntityBlend(n\obj2, 3)
			SpriteViewMode(n\obj2, 2)
			
			n\Speed = (GetModdedINIFloat("DATA\NPCs.ini", "forestmonster", "speed") / 100.0)
			
			temp# = (GetModdedINIFloat("DATA\NPCs.ini", "forestmonster", "scale") / 20.0)
			ScaleEntity n\obj, temp, temp, temp	
			
			MeshCullBox (n\obj, -MeshWidth(n\obj)*2, -MeshHeight(n\obj)*2, -MeshDepth(n\obj)*2, MeshWidth(n\obj)*2, MeshHeight(n\obj)*4, MeshDepth(n\obj)*4)
			
			n\CollRadius = 0.25
			;[End Block]
		Case NPCtype939
			;[Block]
			;i = 53
			;For n2.NPCs = Each NPCs
			;	If (n\NPCtype = n2\NPCtype) And (n<>n2) Then i=i+36
			;Next
			;n\NVName = "SCP-939-"+i
			Local amount939% = 0
			For n2.NPCs = Each NPCs
				If (n\NPCtype = n2\NPCtype) And (n<>n2)
					amount939% = amount939% + 1
				EndIf
			Next
			If amount939% = 0 Then i = 53
			If amount939% = 1 Then i = 89
			If amount939% = 2 Then i = 96
			n\NVName = Format(I_Loc\NPC_939, i)
			
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.3
			EntityType n\Collider, HIT_PLAYER
			For n2.NPCs = Each NPCs
				If n\NPCtype = n2\NPCtype And n<>n2 Then
					n\obj = CopyEntity (n2\obj)
					Exit
				EndIf
			Next
			
			If n\obj = 0 Then 
				n\obj = LoadAnimMesh_Strict("GFX\NPCs\scp-939.b3d")
				temp# = GetModdedINIFloat("DATA\NPCs.ini", "SCP-939", "scale")/2.5
				ScaleEntity n\obj, temp, temp, temp		
			EndIf
			
			n\Speed = (GetModdedINIFloat("DATA\NPCs.ini", "SCP-939", "speed") / 100.0)
			
			n\CollRadius = 0.3
			;[End Block]
		Case NPCtype066
			;[Block]
			n\NVName = I_Loc\NPC_066
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.2
			EntityType n\Collider, HIT_PLAYER
			
			n\obj = LoadAnimMesh_Strict("GFX\NPCs\scp-066.b3d")
			temp# = GetModdedINIFloat("DATA\NPCs.ini", "SCP-066", "scale")/2.5
			ScaleEntity n\obj, temp, temp, temp		
			
			n\Speed = (GetModdedINIFloat("DATA\NPCs.ini", "SCP-066", "speed") / 100.0)
			;[End Block]
		Case NPCtype966
			;[Block]
			i = 1
			For n2.NPCs = Each NPCs
				If (n\NPCtype = n2\NPCtype) And (n<>n2) Then i=i+1
			Next
			n\NVName = Format(I_Loc\NPC_966, i)
			
			n\Collider = CreatePivot()
			EntityRadius n\Collider,0.2
			
			For n2.NPCs = Each NPCs
				If (n\NPCtype = n2\NPCtype) And (n<>n2) Then
					n\obj = CopyEntity (n2\obj)
					Exit
				EndIf
			Next
			
			If n\obj = 0 Then 
				n\obj = LoadAnimMesh_Strict("GFX\npcs\scp-966.b3d")
			EndIf
			
			EntityFX n\obj,1
			
			temp# = GetModdedINIFloat("DATA\NPCs.ini", "SCP-966", "scale")/40.0
			ScaleEntity n\obj, temp, temp, temp		
			
			;EntityColor n\obj,Rnd(0,50),0,Rnd(50,100)
			
			SetAnimTime n\obj,15.0
			
			EntityType n\Collider,HIT_PLAYER
			
			n\Speed = (GetModdedINIFloat("DATA\NPCs.ini", "SCP-966", "speed") / 100.0)
			;[End Block]
		Case NPCtype1048a
			;[Block]
			;n\NVName = I_Loc\NPC_1048a
			n\obj =	LoadAnimMesh_Strict("GFX\npcs\scp-1048a.b3d")
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.1
			EntityType n\Collider, HIT_PLAYER
			RotateEntity(n\obj, -90.0, Rnd(0.0, 360.0), 0.0)
			ScaleEntity n\obj, 0.05,0.05,0.05
			SetAnimTime(n\obj, 2)
			
			n\Sound = LoadSound_Strict("SFX\SCP\1048A\Shriek.ogg")
			n\Sound2 = LoadSound_Strict("SFX\SCP\1048A\Growth.ogg")
			;[End Block]
		Case NPCtype1499
			;[Block]
			n\NVName = I_Loc\NPC_Unidentified
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.2
			EntityType n\Collider, HIT_PLAYER
			For n2.NPCs = Each NPCs
				If (n\NPCtype = n2\NPCtype) And (n<>n2) Then
					n\obj = CopyEntity (n2\obj)
					Exit
				EndIf
			Next
			
			If n\obj = 0 Then 
				n\obj = LoadAnimMesh_Strict("GFX\npcs\1499-1.b3d")
			EndIf
			
			n\Speed = (GetModdedINIFloat("DATA\NPCs.ini", "SCP-1499-1", "speed") / 100.0) * Rnd(0.9,1.1)
			temp# = (GetModdedINIFloat("DATA\NPCs.ini", "SCP-1499-1", "scale") / 4.0) * Rnd(0.8,1.0)
			
			ScaleEntity n\obj, temp, temp, temp
			
			EntityFX n\obj,1
			
			EntityAutoFade n\obj,HideDistance*2.5,HideDistance*2.95
			;[End Block]
		Case NPCtype008
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.2
			EntityType n\Collider, HIT_PLAYER
			
			n\obj = LoadAnimMesh_Strict("GFX\npcs\zombiesurgeon.b3d")
			
			temp# = 0.5 / MeshWidth(n\obj)
			ScaleEntity n\obj, temp, temp, temp
			
			n\Speed = 2.0 / 100
			
			MeshCullBox (n\obj, -MeshWidth(n\obj), -MeshHeight(n\obj), -MeshDepth(n\obj)*4, MeshWidth(n\obj)*2, MeshHeight(n\obj)*2, MeshDepth(n\obj)*8)
			
			SetNPCFrame n,11
			
			n\Sound = LoadSound_Strict("SFX\SCP\049\0492Breath.ogg")
			
			n\HP = 120
			;[End Block]
		Case NPCtypeClerk
			;[Block]
			n\NVName = I_Loc\NPC_Human
			n\Collider = CreatePivot()
			EntityRadius n\Collider, 0.32
			EntityType n\Collider, HIT_PLAYER
			
			n\obj = CopyEntity(ClerkOBJ)
			
			temp# = 0.5 / MeshWidth(n\obj)
			ScaleEntity n\obj, temp, temp, temp
			
			n\Speed = 2.0 / 100
			
			MeshCullBox (n\obj, -MeshWidth(ClerkOBJ), -MeshHeight(ClerkOBJ), -MeshDepth(ClerkOBJ)*4.5, MeshWidth(ClerkOBJ)*2, MeshHeight(ClerkOBJ)*2, MeshDepth(ClerkOBJ)*9)
			
			n\CollRadius = 0.32
			;[End Block]
	End Select
	
	PositionEntity(n\Collider, x, y, z, True)
	PositionEntity(n\obj, x, y, z, True)
	
	ResetEntity(n\Collider)
	
	n\ID = 0
	n\ID = FindFreeNPCID()
	
	DebugLog ("Created NPC "+n\NVName+" (ID: "+n\ID+")")
	
	NPCSpeedChange(n)
	
	Return n
End Function

Function RemoveNPC(n.NPCs)
	
	If n=Null Then Return
	
	If n\obj2 <> 0 Then 
		FreeEntity n\obj2
		n\obj2 = 0
	EndIf
	If n\obj3 <> 0 Then 
		FreeEntity n\obj3
		n\obj3 = 0
	EndIf
	If n\obj4 <> 0 Then 
		FreeEntity n\obj4
		n\obj4 = 0
	EndIf
	
	If n\SoundChn <> 0 Then
		If (Not n\SoundChn_IsStream)
			StopChannel(n\SoundChn)
		Else
			StopStream_Strict(n\SoundChn) : n\SoundChn_IsStream = False
		EndIf
		n\SoundChn = 0
	EndIf
	
	If n\SoundChn2 <> 0 Then
		If (Not n\SoundChn2_IsStream)
			StopChannel(n\SoundChn2)
		Else
			StopStream_Strict(n\SoundChn2) : n\SoundChn2_IsStream = False
		EndIf
		n\SoundChn2 = 0
	EndIf
	
	If n\Sound<>0 Then FreeSound_Strict n\Sound : n\Sound = 0
	If n\Sound2<>0 Then FreeSound_Strict n\Sound2 : n\Sound2 = 0
	
	FreeEntity(n\obj) : n\obj = 0
	FreeEntity(n\Collider) : n\Collider = 0	
	
	Delete n
End Function


Function UpdateNPCs()
	CatchErrors("Uncaught (UpdateNPCs)")
	Local n.NPCs, n2.NPCs, d.Doors, de.Decals, r.Rooms, eo.ElevatorObj, eo2.ElevatorObj
	Local i%, dist#, dist2#, angle#, x#, y#, z#, prevFrame#, PlayerSeeAble%, RN$
	
	Local target
	
	For n.NPCs = Each NPCs
		;A variable to determine if the NPC is in the facility or not
		n\InFacility = CheckForNPCInFacility(n)
		
		Select n\NPCtype
			Case NPCtype173
				;[Block]
				
				If Curr173\Idle <> 3 Then
					dist# = EntityDistance(n\Collider, Collider)		
					
					n\State3 = 1
					
					If n\Idle < 2 Then
						If n\IdleTimer > 0.1
							n\Idle = 1
							n\IdleTimer = Max(n\IdleTimer-FPSfactor,0.1)
						ElseIf n\IdleTimer = 0.1
							n\Idle = 0
							n\IdleTimer = 0
						EndIf
						
						PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) - 0.32, EntityZ(n\Collider))
						RotateEntity (n\obj, 0, EntityYaw(n\Collider)-180, 0)
						
						If n\Idle = False Then
							Local temp% = False
							Local move% = True
							If dist < 15 Then
								If dist < 10.0 Then 
									If EntityVisible(n\Collider, Collider) Then
										temp = True
										n\EnemyX = EntityX(Collider, True)
										n\EnemyY = EntityY(Collider, True)
										n\EnemyZ = EntityZ(Collider, True)
									EndIf
								EndIf										
								
								Local SoundVol# = Max(Min((Distance(EntityX(n\Collider), EntityZ(n\Collider), n\PrevX, n\PrevZ) * 2.5), 1.0), 0.0)
								n\SoundChn = LoopSound2(StoneDragSFX, n\SoundChn, Camera, n\Collider, 10.0, n\State)
								
								n\PrevX = EntityX(n\Collider)
								n\PrevZ = EntityZ(n\Collider)				
								
								If (BlinkTimer < - 16 Or BlinkTimer > - 6) And (IsNVGBlinking=False) Then
									If EntityInView(n\obj, Camera) Then move = False
								EndIf
							EndIf
							
							If NoTarget Then move = True
							
							;player is looking at it -> doesn't move
							If move=False Then
								BlurVolume = Max(Max(Min((4.0 - dist) / 6.0, 0.9), 0.1), BlurVolume)
								CurrCameraZoom = Max(CurrCameraZoom, (Sin(Float(MilliSecs())/20.0)+1.0)*15.0*Max((3.5-dist)/3.5,0.0))								
								
								If dist < 3.5 And MilliSecs() - n\LastSeen > 60000 And temp Then
									PlaySound_Strict(HorrorSFX(Rand(3,4)))
									
									n\LastSeen = MilliSecs()
								EndIf
								
								If dist < 1.5 And Rand(700) = 1 Then PlaySound2(Scp173SFX(Rand(0, 2)), Camera, n\obj)
								
								If dist < 1.5 And n\LastDist > 2.0 And temp Then
									CurrCameraZoom = 40.0
									HeartBeatRate = Max(HeartBeatRate, 140)
									HeartBeatVolume = 0.5
									
									Select Rand(5)
										Case 1
											PlaySound_Strict(HorrorSFX(1))
										Case 2
											PlaySound_Strict(HorrorSFX(2))
										Case 3
											PlaySound_Strict(HorrorSFX(9))
										Case 4
											PlaySound_Strict(HorrorSFX(10))
										Case 5
											PlaySound_Strict(HorrorSFX(14))
									End Select
								EndIf									
									
								n\LastDist = dist
								
								n\State = Max(0, n\State - FPSfactor / 20)
							Else 
								;more than 6 room lengths away from the player -> teleport to a room closer to the player
								If dist > 50 Then
									If Rand(70)=1 Then
										If PlayerRoom\RoomTemplate\Name <> "exit1" And PlayerRoom\RoomTemplate\Name <> "gatea" And PlayerRoom\RoomTemplate\Name <> "pocketdimension" Then
											For w.waypoints = Each WayPoints
												If w\door=Null And Rand(5)=1 Then
													x = Abs(EntityX(Collider)-EntityX(w\obj,True))
													If x < 25.0 And x > 15.0 Then
														z = Abs(EntityZ(Collider)-EntityZ(w\obj,True))
														If z < 25 And z > 15.0 Then
															DebugLog "MOVING 173 TO "+w\room\roomtemplate\name
															PositionEntity n\Collider, EntityX(w\obj,True), EntityY(w\obj,True)+0.25,EntityZ(w\obj,True)
															ResetEntity n\Collider
															Exit
														EndIf
													EndIf
														
												EndIf
											Next
										EndIf
									EndIf
								ElseIf dist > HideDistance*0.8 ;3-6 rooms away from the player -> move randomly from waypoint to another
									If Rand(70)=1 Then TeleportCloser(n)
								Else ;less than 3 rooms away -> actively move towards the player
									n\State = CurveValue(SoundVol, n\State, 3)
									
									;try to open doors
									If Rand(20) = 1 Then
										For d.Doors = Each Doors
											If (Not d\locked) And d\open = False And d\Code = "" And d\KeyCard=0 Then
												For i% = 0 To 1
													If d\buttons[i] <> 0 Then
														If Abs(EntityX(n\Collider) - EntityX(d\buttons[i])) < 0.5 Then
															If Abs(EntityZ(n\Collider) - EntityZ(d\buttons[i])) < 0.5 Then
																If (d\openstate >= 180 Or d\openstate <= 0) Then
																	pvt = CreatePivot()
																	PositionEntity pvt, EntityX(n\Collider), EntityY(n\Collider) + 0.5, EntityZ(n\Collider)
																	PointEntity pvt, d\buttons[i]
																	MoveEntity pvt, 0, 0, n\Speed * 0.6
																	
																	If EntityPick(pvt, 0.5) = d\buttons[i] Then 
																		PlaySound_Strict (LoadTempSound("SFX\Door\DoorOpen173.ogg"))
																		UseDoor(d,False)
																	EndIf
																	
																	FreeEntity pvt
																EndIf
															EndIf
														EndIf
													EndIf
												Next
											EndIf
										Next
									EndIf
									
									If NoTarget
										temp = False
										n\EnemyX = 0
										n\EnemyY = 0
										n\EnemyZ = 0
									EndIf
									
									;player is not looking and is visible from 173's position -> attack
									If temp Then 				
										If dist < 0.65 Then
											If KillTimer >= 0 And (Not GodMode) Then
												
												Select PlayerRoom\RoomTemplate\Name
													Case "lockroom", "lockroom_ez", "room2closets", "coffin"
														DeathMSG = I_Loc\DeathMessage_173Camera
													Case "173"
														DeathMSG = I_Loc\DeathMessage_173Intro
													Case "room2doors"
														DeathMSG = I_Loc\DeathMessage_173Doors
													Default 
														DeathMSG = I_Loc\DeathMessage_173
												End Select
												
												If (Not GodMode) Then n\Idle = True
												PlaySound_Strict(NeckSnapSFX(Rand(0,2)))
												If Rand(2) = 1 Then 
													TurnEntity(Camera, 0, Rand(80,100), 0)
												Else
													TurnEntity(Camera, 0, Rand(-100,-80), 0)
												EndIf
												Kill()
												
											EndIf
										Else
											PointEntity(n\Collider, Collider)
											RotateEntity n\Collider, 0, EntityYaw(n\Collider), EntityRoll(n\Collider)
											;MoveEntity(n\Collider, 0, 0, n\Speed * FPSfactor)
											TranslateEntity n\Collider,Cos(EntityYaw(n\Collider)+90.0)*n\Speed*FPSfactor,0.0,Sin(EntityYaw(n\Collider)+90.0)*n\Speed*FPSfactor
										EndIf
										
									Else ;player is not visible -> move to the location where he was last seen							
										If n\EnemyX <> 0 Then						
											If Distance(EntityX(n\Collider), EntityZ(n\Collider), n\EnemyX, n\EnemyZ) > 0.5 Then
												AlignToVector(n\Collider, n\EnemyX-EntityX(n\Collider), 0, n\EnemyZ-EntityZ(n\Collider), 3)
												MoveEntity(n\Collider, 0, 0, n\Speed * FPSfactor)
												If Rand(500) = 1 Then n\EnemyX = 0 : n\EnemyY = 0 : n\EnemyZ = 0
											Else
												n\EnemyX = 0 : n\EnemyY = 0 : n\EnemyZ = 0
											End If
										Else
											If Rand(400)=1 Then RotateEntity (n\Collider, 0, Rnd(360), 10)
											TranslateEntity n\Collider,Cos(EntityYaw(n\Collider)+90.0)*n\Speed*FPSfactor,0.0,Sin(EntityYaw(n\Collider)+90.0)*n\Speed*FPSfactor
											
										End If
									EndIf
									
								EndIf ; less than 2 rooms away from the player
								
							EndIf
							
						EndIf ;idle = false
						
						PositionEntity(n\Collider, EntityX(n\Collider), Min(EntityY(n\Collider),0.35), EntityZ(n\Collider))
						
					Else ;idle = 2
						
						If n\Target <> Null Then
							Local tmp = False
							If dist > HideDistance*0.7
								If EntityVisible(n\obj,Collider)=False
									tmp = True
								EndIf
							EndIf
							If (Not tmp)
								PointEntity n\obj, n\Target\Collider
								RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),10.0), 0, True								
								dist = EntityDistance(n\Collider, n\Target\Collider)
								;MoveEntity n\Collider, 0, 0, 0.008*FPSfactor*Max(Min((dist*2-1.0)*0.5,1.0),-0.5)
								MoveEntity n\Collider, 0, 0, 0.016*FPSfactor*Max(Min((dist*2-1.0)*0.5,1.0),-0.5)
								n\GravityMult = 1.0
							Else
								PositionEntity n\Collider,EntityX(n\Target\Collider),EntityY(n\Target\Collider)+0.3,EntityZ(n\Target\Collider)
								ResetEntity n\Collider
								n\DropSpeed = 0
								n\GravityMult = 0.0
								;PointEntity n\Collider, n\Target\Collider
								;RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),10.0), 0, True
								;dist = EntityDistance(n\Collider, n\Target\Collider)
								;MoveEntity n\Collider, 0, 0, dist-0.6
							EndIf
							
							;For r.Rooms = Each Rooms
							;	If r\RoomTemplate\Name = "start" Then
							;		If Distance(EntityX(n\Collider),EntityZ(n\Collider),EntityX(r\obj,True)+1024*RoomScale,EntityZ(r\obj,True)+384*RoomScale)<1.6 Then
							;			n\Idle = 3
							;			n\Target = Null
							;		EndIf
							;		Exit
							;	EndIf
							;Next
						EndIf
						
						PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) + 0.05 + Sin(MilliSecs()*0.08)*0.02, EntityZ(n\Collider))
						RotateEntity (n\obj, 0, EntityYaw(n\Collider)-180, 0)
						
						ShowEntity n\obj2
						
						PositionEntity(n\obj2, EntityX(n\Collider), EntityY(n\Collider) - 0.05 + Sin(MilliSecs()*0.08)*0.02, EntityZ(n\Collider))
						RotateEntity (n\obj2, 0, EntityYaw(n\Collider)-180, 0)
					EndIf
				EndIf
				
				;[End block]
			Case NPCtypeOldMan ;------------------------------------------------------------------------------------------------------------------
				;[Block]
				If Contained106 Then
					n\Idle = True
					HideEntity n\obj
					HideEntity n\obj2
					PositionEntity n\obj, 0,500.0,0, True
				Else
					
					dist = EntityDistance(n\Collider, Collider)
					
					Local spawn106% = True
					;checking if 106 is allowed to spawn
					If PlayerRoom\RoomTemplate\Name$ = "dimension1499" Then spawn106% = False
					For e.Events = Each Events
						If e\EventName = "room860"
							If e\EventState = 1
								spawn106% = False
							EndIf
							Exit
						EndIf
					Next
					If PlayerRoom\RoomTemplate\Name$ = "room049" And EntityY(Collider) <= -2848*RoomScale Then
						spawn106% = False
					EndIf
					;GateA event has been triggered - don't make 106 disapper!
					;The reason why this is a seperate For loop is because we need to make sure that room860 would not be able to overwrite the "spawn106%" variable
					For e.events = Each Events
						If e\EventName = "gatea"
							If e\EventState <> 0
								spawn106% = True
								If PlayerRoom\RoomTemplate\Name$ = "dimension1499" Then
									n\Idle = True
								Else
									n\Idle = False
								EndIf
							EndIf
							Exit
						EndIf
					Next
					If (Not spawn106%) And n\State <= 0 Then
						n\State = Rand(22000, 27000)
						PositionEntity n\Collider,0,500,0
					EndIf
					
					If (Not n\Idle) And spawn106%
						If n\State <= 0 Then	;attacking	
							If EntityY(n\Collider) < EntityY(Collider) - 20.0 - 0.55 Then
								If Not PlayerRoom\RoomTemplate\DisableDecals Then
									de.Decals = CreateDecal(0, EntityX(Collider), 0.01, EntityZ(Collider), 90, Rand(360), 0)
									de\Size = 0.05 : de\SizeChange = 0.001 : EntityAlpha(de\obj, 0.8) : UpdateDecals
								EndIf
								
								n\PrevY = EntityY(Collider)
								
								SetAnimTime n\obj, 110
								
								If PlayerRoom\RoomTemplate\Name <> "coffin"
									PositionEntity(n\Collider, EntityX(Collider), EntityY(Collider) - 15, EntityZ(Collider))
								EndIf
								
								PlaySound_Strict(DecaySFX(0))
							End If
							
							If Rand(500) = 1 Then PlaySound2(OldManSFX(Rand(0, 2)), Camera, n\Collider)
							n\SoundChn = LoopSound2(OldManSFX(4), n\SoundChn, Camera, n\Collider, 8.0, 0.8)
							
							If n\State > - 10 Then
								ShouldPlay = 66
								If n\Frame<259 Then
									PositionEntity n\Collider, EntityX(n\Collider), n\PrevY-0.15, EntityZ(n\Collider)
									PointEntity n\obj, Collider
									RotateEntity (n\Collider, 0, CurveValue(EntityYaw(n\obj),EntityYaw(n\Collider),100.0), 0, True)
									
									AnimateNPC(n, 110, 259, 0.15, False)
								Else
									n\State = -10
								EndIf
							Else
								If PlayerRoom\RoomTemplate\Name <> "gatea" Then ShouldPlay = 10
								
								Local Visible% = False
								If dist < 8.0 Then
									Visible% = EntityVisible(n\Collider, Collider)
								EndIf
								
								If NoTarget Then Visible = False
								
								If Visible Then
									If PlayerRoom\RoomTemplate\Name <> "gatea" Then n\PathTimer = 0
									If EntityInView(n\Collider, Camera) Then
										GiveAchievement(Achv106)
										
										;Achievements(Achv106) = True
										
										BlurVolume = Max(Max(Min((4.0 - dist) / 6.0, 0.9), 0.1), BlurVolume)
										CurrCameraZoom = Max(CurrCameraZoom, (Sin(Float(MilliSecs())/20.0)+1.0) * 20.0 * Max((4.0-dist)/4.0,0))
										
										If MilliSecs() - n\LastSeen > 60000 Then 
											CurrCameraZoom = 40
											PlaySound_Strict(HorrorSFX(6))
											n\LastSeen = MilliSecs()
										EndIf
									EndIf
								Else
									n\State=n\State-FPSfactor
								End If
								
								If dist > 0.8 Then
									If (dist > 25.0 Or PlayerRoom\RoomTemplate\Name = "pocketdimension" Or Visible Or n\PathStatus <> 1) And PlayerRoom\RoomTemplate\Name <> "gatea" Then 
										
										If (dist > 40 Or PlayerRoom\RoomTemplate\Name = "pocketdimension") Then
											TranslateEntity n\Collider, 0, ((EntityY(Collider) - 0.14) - EntityY(n\Collider)) / 50.0, 0
										EndIf
										
										n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,10.0)
										
										PointEntity n\obj, Collider
										RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj), EntityYaw(n\Collider), 10.0), 0
										
										If KillTimer >= 0 Then
											prevFrame# = n\Frame
											AnimateNPC(n, 284, 333, n\CurrSpeed*43)
											;Animate2(n\obj, AnimTime(n\obj), 284, 333, n\CurrSpeed*43)
											
											If prevFrame =< 286 And n\Frame>286 Then
												PlaySound2(Step2SFX(Rand(0,2)),Camera, n\Collider, 6.0, Rnd(0.8,1.0))	
											ElseIf prevFrame=<311 And n\Frame>311.0 
												PlaySound2(Step2SFX(Rand(0,2)),Camera, n\Collider, 6.0, Rnd(0.8,1.0))
											EndIf
										Else 
											n\CurrSpeed = 0
										EndIf
										
										n\PathTimer = Max(n\PathTimer-FPSfactor,0)
										If n\PathTimer =< 0 Then
											n\PathStatus = FindPath (n, EntityX(Collider,True), EntityY(Collider,True), EntityZ(Collider,True))
											n\PathTimer = 70*10
										EndIf
									Else 
										If n\PathTimer <= 0 Then
											n\PathStatus = FindPath (n, EntityX(Collider,True), EntityY(Collider,True), EntityZ(Collider,True))
											n\PathTimer = 70*10
											n\CurrSpeed = 0
										Else
											n\PathTimer = Max(n\PathTimer-FPSfactor,0)
											
											If n\PathStatus = 2 Then
												n\CurrSpeed = 0
											ElseIf n\PathStatus = 1
												While n\Path[n\PathLocation]=Null
													If n\PathLocation > 19 Then 
														n\PathLocation = 0 : n\PathStatus = 0
														Exit
													Else
														n\PathLocation = n\PathLocation + 1
													EndIf
												Wend
												
												If n\Path[n\PathLocation]<>Null Then 
													TranslateEntity n\Collider, 0, ((EntityY(n\Path[n\PathLocation]\obj,True) - 0.11) - EntityY(n\Collider)) / 50.0, 0
													
													PointEntity n\obj, n\Path[n\PathLocation]\obj
													
													dist2# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
													
													RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj), EntityYaw(n\Collider), Min(20.0,dist2*10.0)), 0
													n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,10.0)
													
													prevFrame# = AnimTime(n\obj)
													AnimateNPC(n, 284, 333, n\CurrSpeed*43)
													;Animate2(n\obj, AnimTime(n\obj), 284, 333, n\CurrSpeed*43)
													If prevFrame =< 286 And n\Frame>286 Then
														PlaySound2(Step2SFX(Rand(0,2)),Camera, n\Collider, 6.0, Rnd(0.8,1.0))	
													ElseIf prevFrame=<311 And n\Frame>311.0 
														PlaySound2(Step2SFX(Rand(0,2)),Camera, n\Collider, 6.0, Rnd(0.8,1.0))
													EndIf
													
													If dist2 < 0.2 Then n\PathLocation = n\PathLocation + 1
												EndIf
											ElseIf n\PathStatus = 0
												If n\State3=0 Then AnimateNPC(n, 334, 494, 0.3)
												;Animate2(n\obj, AnimTime(n\obj), 334, 494, 0.3)
												n\CurrSpeed = CurveValue(0,n\CurrSpeed,10.0)
											EndIf
										EndIf
										
									EndIf
									
								ElseIf PlayerRoom\RoomTemplate\Name <> "gatea" And (Not NoTarget) ;dist < 0.8
									
									If dist > 0.5 Then 
										n\CurrSpeed = CurveValue(n\Speed * 2.5,n\CurrSpeed,10.0)
									Else
										n\CurrSpeed = 0
									EndIf
									AnimateNPC(n, 105, 110, 0.15, False)
									;Animate2(n\obj, AnimTime(n\obj), 105, 110, 0.15, False)
									;If Floor(AnimTime(n\obj)) = 43 Then SetAnimTime(n\obj, 43)
									
									If KillTimer >= 0 And FallTimer >= 0 Then
										PointEntity n\obj, Collider
										RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj), EntityYaw(n\Collider), 10.0), 0										
										
										If Ceil(n\Frame) = 110 And (Not GodMode) Then
											PlaySound_Strict(DamageSFX(1))
											PlaySound_Strict(HorrorSFX(5))											
											If PlayerRoom\RoomTemplate\Name = "pocketdimension" Then
												DeathMSG = I_Loc\DeathMessage_106Pd
												Kill()
											Else
												PlaySound_Strict(OldManSFX(3))
												FallTimer = Min(-1, FallTimer)
												PositionEntity(Head, EntityX(Camera, True), EntityY(Camera, True), EntityZ(Camera, True), True)
												ResetEntity (Head)
												RotateEntity(Head, 0, EntityYaw(Camera) + Rand(-45, 45), 0)
											EndIf
										EndIf
									EndIf
									
								EndIf
								
							EndIf 
							
							MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
							
							If n\State <= Rand(-3500, -3000) Then 
								If Not EntityInView(n\obj,Camera) And dist > 5 Then
									n\State = Rand(22000, 27000)
									PositionEntity n\Collider,0,500,0
								EndIf
							EndIf
							
							If FallTimer < -250.0 Then
								MoveToPocketDimension()
								n\State = 250 ;make 106 idle for a while
							EndIf
							
							If n\Reload = 0 Then
                                If dist > 10 And PlayerRoom\RoomTemplate\Name <> "pocketdimension" And PlayerRoom\RoomTemplate\Name <> "gatea" And n\State <-5 Then ;timer idea by Juanjpro
                                    If (Not EntityInView(n\obj,Camera))
                                        TurnEntity Collider,0,180,0
                                        pick = EntityPick(Collider,5)
                                        TurnEntity Collider,0,180,0
                                        If pick<>0
											TeleportEntity(n\Collider,PickedX(),PickedY(),PickedZ(),n\CollRadius)
                                            PointEntity(n\Collider,Collider)
                                            RotateEntity(n\Collider,0,EntityYaw(n\Collider),0)
                                            MoveEntity(n\Collider,0,0,-2)
                                            PlaySound2(OldManSFX(3),Camera,n\Collider)
											n\SoundChn2 = PlaySound2(OldManSFX(6+Rand(0,2)),Camera,n\Collider)
                                            n\PathTimer = 0
                                            n\Reload = (70*10.0)/(SelectedDifficulty\otherFactors+1)
                                            DebugLog "Teleported 106 (Distance: "+EntityDistance(n\Collider,Collider)+")"
                                        EndIf
                                    EndIf
                                EndIf
                            EndIf
                            n\Reload = Max(0, n\Reload - FPSfactor)
                            DebugLog "106 in... "+n\Reload 
							
							UpdateSoundOrigin(n\SoundChn2,Camera,n\Collider)
						Else ;idling outside the map
							n\CurrSpeed = 0
							MoveEntity n\Collider, 0, ((EntityY(Collider) - 30) - EntityY(n\Collider)) / 200.0, 0
							n\DropSpeed = 0
							n\Frame = 110
							;SetAnimTime n\obj, 110.0
							
							If (Not PlayerRoom\RoomTemplate\DisableDecals) Then
								If PlayerRoom\RoomTemplate\Name <> "gatea"
									If (SelectedDifficulty\aggressiveNPCs) Then
										n\State=n\State-FPSfactor*2
									Else
										n\State=n\State-FPSfactor
									EndIf
								EndIf
							EndIf
						End If
						
						ResetEntity(n\Collider)
						n\DropSpeed = 0
						PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) - 0.15, EntityZ(n\Collider))
						
						RotateEntity n\obj, 0, EntityYaw(n\Collider), 0
						
						PositionEntity(n\obj2, EntityX(n\obj), EntityY(n\obj) , EntityZ(n\obj))
						RotateEntity(n\obj2, 0, EntityYaw(n\Collider) - 180, 0)
						MoveEntity(n\obj2, 0, 8.6 * 0.11, -1.5 * 0.11)
						
						If PlayerRoom\RoomTemplate\Name = "pocketdimension" Or PlayerRoom\RoomTemplate\Name = "gatea" Then
							HideEntity n\obj2
						Else
							If dist < CameraFogFar*LightVolume*0.6 Then
								HideEntity n\obj2
							Else
								ShowEntity n\obj2
								EntityAlpha (n\obj2, Min(dist-CameraFogFar*LightVolume*0.6,1.0))
							EndIf
						EndIf						
					Else
						HideEntity n\obj2
					EndIf
					
				EndIf
				
				;[End Block]
			Case NPCtype096
				;[Block]
				dist = EntityDistance(Collider, n\Collider)
				
				Select n\State
					Case 0
						;[Block]
						If dist<8.0 Then
							GiveAchievement(Achv096)
							;If n\Sound = 0 Then
							;	n\Sound = LoadSound_Strict("SFX\Music\096.ogg")
							;Else
							;	n\SoundChn = LoopSound2(n\Sound, n\SoundChn, Camera, n\Collider, 8.0, 1.0)
							;EndIf
							If n\SoundChn = 0
								n\SoundChn = StreamSound_Strict("SFX\Music\096.ogg",0)
								n\SoundChn_IsStream = True
							EndIf
							UpdateStreamSoundOrigin(n\SoundChn,Camera,n\Collider,8.0,1.0)
							
							If n\State3 = -1
								AnimateNPC(n,936,1263,0.1,False)
								If n\Frame=>1262.9
									n\State = 5
									n\State3 = 0
									n\Frame = 312
								EndIf
							Else
								AnimateNPC(n,936,1263,0.1)
								If n\State3 < 70*6
									n\State3=n\State3+FPSfactor
								Else
									If Rand(1,5)=1
										n\State3 = -1
									Else
										n\State3=70*(Rand(0,3))
									EndIf
								EndIf
							EndIf
							;AnimateNPC(n, 1085,1412, 0.1) ;sitting
							
							If dist < (CameraFogFar * LightVolume) And Sees096Face(n) Then
								PlaySound_Strict LoadTempSound("SFX\SCP\096\Triggered.ogg")
								
								CurrCameraZoom = 10
								
								n\Frame = 194
								;n\Frame = 307
								If n\SoundChn <> 0 Then
									StopStream_Strict(n\SoundChn) : n\SoundChn = 0 : n\SoundChn_IsStream = False
								EndIf
								n\Sound = 0
								n\State = 1
								n\State3 = 0
							EndIf
						EndIf
						;[End Block]
					Case 4
						;[Block]
						; Can't save if targeting the player.
						CanSave = n\Target<>Null
						
						CurrCameraZoom = CurveValue(Max(CurrCameraZoom, (Sin(Float(MilliSecs())/20.0)+1.0) * 10.0),CurrCameraZoom,8.0)
						
						If n\Target = Null Then 
							;If n\Sound = 0 Then
							;	n\Sound = LoadSound_Strict("SFX\SCP\096\Scream.ogg")
							;Else
							;	n\SoundChn = LoopSound2(n\Sound, n\SoundChn, Camera, n\Collider, 7.5, 1.0)
							;EndIf
							If n\SoundChn = 0
								n\SoundChn = StreamSound_Strict("SFX\SCP\096\Scream.ogg",0)
								n\SoundChn_IsStream = True
							EndIf
							UpdateStreamSoundOrigin(n\SoundChn,Camera,n\Collider,7.5,1.0)
							
							;If n\Sound2 = 0 Then
							;	n\Sound2 = LoadSound_Strict("SFX\Music\096Chase.ogg")
							;Else
							;	If n\SoundChn2 = 0 Then
							;		n\SoundChn2 = PlaySound_Strict (n\Sound2)
							;	Else
							;		If (Not ChannelPlaying(n\SoundChn2)) Then n\SoundChn2 = PlaySound_Strict(n\Sound2)
							;		ChannelVolume(n\SoundChn2, Min(Max(8.0-dist,0.6),1.0)*SFXVolume#)
							;	EndIf
							;EndIf
							If n\SoundChn2 = 0
								n\SoundChn2 = StreamSound_Strict("SFX\Music\096Chase.ogg",0)
								n\SoundChn2_IsStream = True
							Else
								SetStreamVolume_Strict(n\SoundChn2,Min(Max(8.0-dist,0.6),1.0)*SFXVolume#)
							EndIf
						EndIf
						
						If NoTarget And n\Target = Null Then n\State = 5
						
						If KillTimer =>0 Then
							
							If MilliSecs() > n\State3 Then
								n\LastSeen=0
								If n\Target=Null Then
									If EntityVisible(Collider, n\Collider) Then n\LastSeen=1
								Else
									If EntityVisible(n\Target\Collider, n\Collider) Then n\LastSeen=1
								EndIf
								n\State3=MilliSecs()+3000
							EndIf
							
							If n\LastSeen=1 Then
								n\PathTimer=Max(70*3, n\PathTimer)
								n\PathStatus=0
								
								If n\Target<> Null Then dist = EntityDistance(n\Target\Collider, n\Collider)
								
								If dist < 2.8 Or n\Frame<150 Then 
									If n\Frame>193 Then n\Frame = 2.0 ;go to the start of the jump animation
									
									AnimateNPC(n, 2, 193, 0.7)
									
									If dist > 1.0 Then 
										n\CurrSpeed = CurveValue(n\Speed*2.0,n\CurrSpeed,15.0)
									Else
										n\CurrSpeed = 0
										
										If n\Target=Null Then
											If (Not GodMode) Then 
												PlaySound_Strict DamageSFX(4)
												
												pvt = CreatePivot()
												CameraShake = 30
												BlurTimer = 2000
												DeathMSG = I_Loc\DeathMessage_096
												Kill()
												KillAnim = 1
												For i = 0 To 6
													PositionEntity pvt, EntityX(Collider)+Rnd(-0.1,0.1),EntityY(Collider)-0.05,EntityZ(Collider)+Rnd(-0.1,0.1)
													TurnEntity pvt, 90, 0, 0
													EntityPick(pvt,0.3)
													
													de.Decals = CreateDecal(Rand(15,16), PickedX(), PickedY()+0.005, PickedZ(), 90, Rand(360), 0)
													de\Size = Rnd(0.2,0.6) : EntityAlpha(de\obj, 1.0) : ScaleSprite de\obj, de\Size, de\Size
												Next
												FreeEntity pvt
											EndIf
										EndIf				
									EndIf
									
									If n\Target=Null Then
										PointEntity n\Collider, Collider
									Else
										PointEntity n\Collider, n\Target\Collider
									EndIf
									
								Else
									If n\Target=Null Then 
										PointEntity n\obj, Collider
									Else
										PointEntity n\obj, n\Target\Collider
									EndIf
									
									RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj), EntityYaw(n\Collider), 5.0), 0
									
									;1000
									If n\Frame>847 Then n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,20.0)
									
									If n\Frame<906 Then ;1058
										AnimateNPC(n,737,906,n\Speed*8,False)
										;AnimateNPC(n, 892,1058, n\Speed*8, False)
									Else
										AnimateNPC(n,907,935,n\CurrSpeed*8)
										;AnimateNPC(n, 1059,1084, n\CurrSpeed*8)	
									EndIf
								EndIf
								
								RotateEntity n\Collider, 0, EntityYaw(n\Collider), 0, True
								MoveEntity n\Collider, 0,0,n\CurrSpeed*FPSfactor
								
							Else
								If n\PathStatus = 1 Then
									
									If n\Path[n\PathLocation]=Null Then 
										If n\PathLocation > 19 Then 
											n\PathLocation = 0 : n\PathStatus = 0
										Else
											n\PathLocation = n\PathLocation + 1
										EndIf
									Else
										PointEntity n\obj, n\Path[n\PathLocation]\obj
										
										RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj), EntityYaw(n\Collider), 5.0), 0
										
										;1000
										If n\Frame>847 Then n\CurrSpeed = CurveValue(n\Speed*1.5,n\CurrSpeed,15.0)
										MoveEntity n\Collider, 0,0,n\CurrSpeed*FPSfactor
										
										If n\Frame<906 Then ;1058
											AnimateNPC(n,737,906,n\Speed*8,False)
											;AnimateNPC(n, 892,1058, n\Speed*8, False)
										Else
											AnimateNPC(n,907,935,n\CurrSpeed*8)
											;AnimateNPC(n, 1059,1084, n\CurrSpeed*8)	
										EndIf
										
										dist2# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
										If dist2 < 0.8 Then ;0.4
											If n\Path[n\PathLocation]\door <> Null Then
												If n\Path[n\PathLocation]\door\open = False Then
													n\Path[n\PathLocation]\door\open = True
													n\Path[n\PathLocation]\door\fastopen = 1
													PlaySound2(OpenDoorFastSFX, Camera, n\Path[n\PathLocation]\door\obj)
												EndIf
											EndIf							
											If dist2 < 0.7 Then n\PathLocation = n\PathLocation + 1 ;0.2
										EndIf 
									EndIf
									
								Else
									;AnimateNPC(n, 892,972, 0.2)
									AnimateNPC(n,737,822,0.2)
									
									n\PathTimer = Max(0, n\PathTimer-FPSfactor)
									If n\PathTimer=<0 Then
										If n\Target<>Null Then
											n\PathStatus = FindPath(n, EntityX(n\Target\Collider),EntityY(n\Target\Collider)+0.2,EntityZ(n\Target\Collider))	
										Else
											n\PathStatus = FindPath(n, EntityX(Collider),EntityY(Collider)+0.2,EntityZ(Collider))	
										EndIf
										n\PathTimer = 70*5
									EndIf
								EndIf
							EndIf
							
							If dist > 32.0 Or EntityY(n\Collider)<-50 Then
								If Rand(50)=1 Then TeleportCloser(n)
							EndIf
						Else ;play the eating animation if killtimer < 0 
							AnimateNPC(n, Min(27,AnimTime(n\obj)), 193, 0.5)
							
							;Animate2(n\obj, AnimTime(n\obj), Min(27,AnimTime(n\obj)), 193, 0.5)
						EndIf
						
						
						;[End Block]
					Case 1,2,3
						; Can't save if targeting the player.
						CanSave = n\Target<>Null

						;[Block]
						;If n\Sound = 0 Then
						;	n\Sound = LoadSound_Strict("SFX\Music\096Angered.ogg")
						;Else
						;	n\SoundChn = LoopSound2(n\Sound, n\SoundChn, Camera, n\Collider, 10.0, 1.0)
						;EndIf
						If n\SoundChn = 0
							n\SoundChn = StreamSound_Strict("SFX\Music\096Angered.ogg",0)
							n\SoundChn_IsStream = True
						EndIf
						UpdateStreamSoundOrigin(n\SoundChn,Camera,n\Collider,10.0,1.0)
						
						If n\State=1 Then ; get up
							If n\Frame<312
								AnimateNPC(n,193,311,0.3,False)
								If n\Frame > 310.9 Then n\State = 2 : n\Frame = 737
							ElseIf n\Frame>=312 And n\Frame<=422
								AnimateNPC(n,312,422,0.3,False)
								If n\Frame > 421.9 Then n\Frame = 677
							Else
								AnimateNPC(n,677,736,0.3,False)
								If n\Frame > 735.9 Then n\State = 2 : n\Frame = 737
							EndIf
							;If n\Frame>1085 Then
							;	AnimateNPC(n, 1085, 1412, 0.3,False)
							;	If n\Frame> 1411.9 Then n\Frame = 307
							;Else
							;	AnimateNPC(n, 307, 424, 0.3, False)
							;	If n\Frame > 423.9 Then n\State = 2 : n\Frame = 892
							;EndIf
						ElseIf n\State=2
							AnimateNPC(n,677,737,0.2,False)
							If n\Frame=>737 Then n\State=3 : n\State2=0
							;AnimateNPC(n, 833, 972, 0.3, False)
							;If n\Frame=>972 Then n\State = 3 : n\State2=0
						ElseIf n\State=3
							n\State2 = n\State2+FPSfactor
							If n\State2 > 70*18 Then
								AnimateNPC(n,823,847,n\Speed*8,False)
								;AnimateNPC(n, 973, 1001, 0.5, False)
								If n\Frame>846.9 Then ;1000.9 
									n\State = 4
									If n\SoundChn <> 0 Then
										StopStream_Strict(n\SoundChn) : n\SoundChn = 0 : n\SoundChn_IsStream = False
									EndIf
								EndIf
							Else
								AnimateNPC(n,737,822,0.3)
								;AnimateNPC(n, 892,978, 0.3)
							EndIf
						EndIf
						;[End Block]
					Case 5
						;[Block]
						If dist < 16.0 Then 
							
							If dist < 4.0 Then
								GiveAchievement(Achv096)
							EndIf
							
							;If n\Sound = 0 Then
							;	n\Sound = LoadSound_Strict("SFX\Music\096.ogg")
							;Else
							;	n\SoundChn = LoopSound2(n\Sound, n\SoundChn, Camera, n\Collider, 14.0, 1.0)
							;EndIf
							If n\SoundChn = 0
								n\SoundChn = StreamSound_Strict("SFX\Music\096.ogg",0)
								n\SoundChn_IsStream = True
							EndIf
							UpdateStreamSoundOrigin(n\SoundChn,Camera,n\Collider,14.0,1.0)
							
							If n\Frame>=422
								n\State2=n\State2+FPSfactor
								If n\State2>1000 Then ;walking around
									If n\State2>1600 Then n\State2=Rand(0,500) ;: n\Frame = 1457 ;1652
									
									;1652
									If n\Frame<1382 Then ;idle to walk
										n\CurrSpeed = CurveValue(n\Speed*0.1,n\CurrSpeed,5.0)
										AnimateNPC(n,1369,1382,n\CurrSpeed*45,False)
										;AnimateNPC(n, 1638,1652, n\CurrSpeed*45,False)
									Else
										n\CurrSpeed = CurveValue(n\Speed*0.1,n\CurrSpeed,5.0)
										AnimateNPC(n,1383,1456,n\CurrSpeed*45)
										;AnimateNPC(n, 1653,1724, n\CurrSpeed*45) ;walk
									EndIf
									
									If MilliSecs() > n\State3 Then
										n\LastSeen=0
										If EntityVisible(Collider, n\Collider) Then 
											n\LastSeen=1
										Else
											HideEntity n\Collider
											EntityPick(n\Collider, 1.5)
											If PickedEntity() <> 0 Then
												n\Angle = EntityYaw(n\Collider)+Rnd(80,110)
											EndIf
											ShowEntity n\Collider
										EndIf
										n\State3=MilliSecs()+3000
									EndIf
									
									If n\LastSeen Then 
										PointEntity n\obj, Collider
										RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),130.0),0
										If dist < 1.5 Then n\State2=0
									Else
										RotateEntity n\Collider, 0, CurveAngle(n\Angle,EntityYaw(n\Collider),50.0),0
									EndIf
								Else
									;1638
									If n\Frame>472 Then ;walk to idle
										n\CurrSpeed = CurveValue(n\Speed*0.05,n\CurrSpeed,8.0)
										AnimateNPC(n,1383,1469,n\CurrSpeed*45,False)
										If n\Frame=>1468.9 Then n\Frame=423
										;AnimateNPC(n, 1652, 1638, -n\CurrSpeed*45,False)
									Else ;idle
										n\CurrSpeed = CurveValue(0,n\CurrSpeed,4.0)	
										AnimateNPC(n,423,471,0.2)
										;AnimateNPC(n, 585, 633, 0.2) ;idle
									EndIf
								EndIf
								
								MoveEntity n\Collider,0,0,n\CurrSpeed*FPSfactor
							Else
								AnimateNPC(n,312,422,0.3,False)
							EndIf
							
							If dist < (CameraFogFar * LightVolume) And Sees096Face(n) Then
								PlaySound_Strict LoadTempSound("SFX\SCP\096\Triggered.ogg")
								
								CurrCameraZoom = 10
								
								If n\Frame >= 422
									n\Frame = 677 ;833
								EndIf
								If n\SoundChn <> 0 Then
									StopStream_Strict(n\SoundChn) : n\SoundChn = 0 : n\SoundChn_IsStream = False
								EndIf
								n\Sound = 0
								n\State = 2
							EndIf
						EndIf
						;[End Block]
				End Select
				
				;ResetEntity(n\Collider)
				PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider)-0.03, EntityZ(n\Collider)) ;-0.07
				
				RotateEntity n\obj, EntityPitch(n\Collider), EntityYaw(n\Collider), 0
				;[End Block]
			Case NPCtype049
				;[Block]
				;n\state = the "main state" of the NPC
				;n\state2 = attacks the player when the value is above 0.0
				;n\state3 = timer for updating the path again
				
				prevFrame# = n\Frame
				
				dist  = EntityDistance(Collider, n\Collider)
				
				n\BlinkTimer# = 1.0
				
				If n\Idle > 0.1
					If PlayerRoom\RoomTemplate\Name$ <> "room049"
						n\Idle = Max(n\Idle-(1+SelectedDifficulty\aggressiveNPCs)*FPSfactor,0.1)
					EndIf
					n\DropSpeed = 0
					If ChannelPlaying(n\SoundChn) Then StopChannel(n\SoundChn)
					If ChannelPlaying(n\SoundChn2) Then StopChannel(n\SoundChn2)
					PositionEntity n\Collider,0,-500,0
					ResetEntity n\Collider
				Else
					If n\Idle = 0.1 Then
						If PlayerInReachableRoom() Then
							For i = 0 To 3
								If PlayerRoom\Adjacent[i]<>Null Then
									For j = 0 To 3
										If PlayerRoom\Adjacent[i]\Adjacent[j]<>Null Then
											If PlayerRoom\Adjacent[i]\Adjacent[j]<>PlayerRoom Then
												TeleportEntity(n\Collider,PlayerRoom\Adjacent[i]\Adjacent[j]\x,0.5,PlayerRoom\Adjacent[i]\Adjacent[j]\z,n\CollRadius,True)
												Exit
											EndIf
										EndIf
									Next
									Exit
								EndIf
							Next
							n\Idle = 0.0
							DebugLog "SCP-049 not idle"
						EndIf
					EndIf
					
					Select n\State
						Case 0 ;nothing (used for events)
						Case 1 ;looking around before getting active
							;[Block]
							If n\Frame=>538 Then
								AnimateNPC(n, 659, 538, -0.45, False)
								If n\Frame > 537.9 Then n\Frame = 37
								
								;Animate2(n\obj, AnimTime(n\obj), 659, 538, -0.45, False)
								;If AnimTime(n\obj)=538 Then SetAnimTime(n\obj, 37)
							Else
								AnimateNPC(n, 37, 269, 0.7, False)
								If n\Frame>268.9 Then n\State = 2
								
								;Animate2(n\obj, AnimTime(n\obj), 37, 269, 0.7, False)
								;If AnimTime(n\obj)=269 Then n\State = 2
							EndIf
							;[End Block]
						Case 2 ;being active
							;[Block]
							If (dist < HideDistance*2) And (Not n\Idle) And PlayerInReachableRoom(True) Then
								n\SoundChn = LoopSound2(n\Sound, n\SoundChn, Camera, n\Collider)
								PlayerSeeAble% = MeNPCSeesPlayer(n)
								If PlayerSeeAble%=True Or n\State2>0 Then ;Player is visible for 049's sight - attacking
									GiveAchievement(Achv049)
									
									;Playing a sound after detecting the player
									If n\PrevState <= 1 And ChannelPlaying(n\SoundChn2)=False
										If n\Sound2 <> 0 Then FreeSound_Strict(n\Sound2)
										n\Sound2 = LoadSound_Strict("SFX\SCP\049\Spotted"+Rand(1,7)+".ogg")
										n\SoundChn2 = LoopSound2(n\Sound2,n\SoundChn2,Camera,n\obj)
										n\PrevState = 2
									EndIf
									n\PathStatus = 0
									n\PathTimer# = 0.0
									n\PathLocation = 0
									If PlayerSeeAble%=True Then n\State2 = 70*2
									
									PointEntity n\obj,Collider
									RotateEntity n\Collider,0,CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),10.0),0
									
									If dist < 0.5 Then
										If WearingHazmat>0 Then
											BlurTimer = BlurTimer+FPSfactor*2.5
											If BlurTimer>250 And BlurTimer-FPSfactor*2.5 <= 250 And n\PrevState<>3 Then
												If n\SoundChn2 <> 0 Then StopChannel(n\SoundChn2)
												n\SoundChn2 = PlaySound_Strict(LoadTempSound("SFX\SCP\049\TakeOffHazmat.ogg"))
												n\PrevState=3
											ElseIf BlurTimer => 500
												For i = 0 To MaxItemAmount-1
													If Inventory(i)<>Null Then
														If Inventory(i)\itemtemplate\group = "hazmat" And WearingHazmat<3 Then
															If Inventory(i)\state2 < 3 Then
																Inventory(i)\state2 = Inventory(i)\state2 + 1
																BlurTimer = 260.0
																CameraShake = 2.0
															Else
																RemoveItem(Inventory(i))
																WearingHazmat = False
															EndIf
															Exit
														EndIf
													EndIf
												Next
											EndIf
										ElseIf Wearing714 Then
											BlurTimer = BlurTimer+FPSfactor*2.5
											If BlurTimer>250 And BlurTimer-FPSfactor*2.5 <= 250 And n\PrevState<>3 Then
												If n\SoundChn2 <> 0 Then StopChannel(n\SoundChn2)
												n\SoundChn2 = PlaySound_Strict(LoadTempSound("SFX\SCP\049\714Equipped.ogg"))
												n\PrevState=3
											ElseIf BlurTimer => 500
												Wearing714=False
											EndIf
										Else
											CurrCameraZoom = 20.0
											BlurTimer = 500.0
											
											If (Not GodMode) Then
												If PlayerRoom\RoomTemplate\Name$ = "room049"
													DeathMSG = I_Loc\DeathMessage_049Chamber
													For e.events = Each Events
														If e\EventName = "room049" Then e\EventState=-1 : Exit
													Next
												Else
													DeathMSG = I_Loc\DeathMessage_049
													Kill() : KillAnim = 0
												EndIf
												PlaySound_Strict HorrorSFX(13)
												If n\Sound2 <> 0 Then FreeSound_Strict(n\Sound2)
												n\Sound2 = LoadSound_Strict("SFX\SCP\049\Kidnap"+Rand(1,2)+".ogg")
												n\SoundChn2 = LoopSound2(n\Sound2,n\SoundChn2,Camera,n\obj)
												n\State = 3
											EndIf										
										EndIf
									Else
										n\CurrSpeed = CurveValue(n\Speed, n\CurrSpeed, 20.0)
										MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor	
										
										If n\PrevState = 3 Then n\PrevState = 2
										
										If dist < 3.0 Then
											AnimateNPC(n, Max(Min(AnimTime(n\obj),428.0),387), 463.0, n\CurrSpeed*38)
										Else
											If n\Frame>428.0 Then
												AnimateNPC(n, Min(AnimTime(n\obj),463.0), 498.0, n\CurrSpeed*38,False)
												If n\Frame>497.9 Then n\Frame = 358
											Else
												AnimateNPC(n, Max(Min(AnimTime(n\obj),358.0),346), 393.0, n\CurrSpeed*38)
											EndIf
										EndIf
									EndIf
								Else ;Finding a path to the player
									If n\PathStatus = 1 ;Path to player found
										While n\Path[n\PathLocation]=Null
											If n\PathLocation > 19
												n\PathLocation = 0 : n\PathStatus = 0 : Exit
											Else
												n\PathLocation = n\PathLocation + 1
											EndIf
										Wend
										If n\Path[n\PathLocation]<>Null Then
											;closes doors behind him
											If n\PathLocation>0 Then
												If n\Path[n\PathLocation-1] <> Null Then
													d = n\Path[n\PathLocation-1]\door
													If d <> Null Then
														If (Not d\IsElevatorDoor) Then
															If EntityDistance(n\Path[n\PathLocation-1]\obj,n\Collider)>0.3 Then
																If (d\MTFClose) And (d\open) And (d\buttons[0]<>0 Or d\buttons[1]<>0) And (d\openstate <= 0 Lor d\openstate >= 180) Then
																	UseDoor(n\Path[n\PathLocation-1]\door, False)
																EndIf
															EndIf
														EndIf
													EndIf
												EndIf
											EndIf
											
											n\CurrSpeed = CurveValue(n\Speed, n\CurrSpeed, 20.0)
											PointEntity n\obj,n\Path[n\PathLocation]\obj
											RotateEntity n\Collider,0,CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),10.0),0
											MoveEntity n\Collider,0,0,n\CurrSpeed*FPSfactor
											
											;opens doors in front of him
											dist2# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
											If dist2 < 0.6 Then
												temp = True
												If n\Path[n\PathLocation]\door <> Null Then
													If (Not n\Path[n\PathLocation]\door\IsElevatorDoor)
														If (n\Path[n\PathLocation]\door\locked Or n\Path[n\PathLocation]\door\KeyCard<>0 Or n\Path[n\PathLocation]\door\Code<>"") And (Not n\Path[n\PathLocation]\door\open) Then
															temp = False
														Else
															If n\Path[n\PathLocation]\door\open = False And (n\Path[n\PathLocation]\door\buttons[0]<>0 Or n\Path[n\PathLocation]\door\buttons[1]<>0) Then
																UseDoor(n\Path[n\PathLocation]\door, False)
															EndIf
														EndIf
													EndIf
												EndIf
												If dist2#<0.2 And temp
													n\PathLocation = n\PathLocation + 1
												ElseIf dist2#<0.5 And (Not temp)
													;Breaking up the path when the door in front of 049 cannot be operated by himself
													n\PathStatus = 0
													n\PathTimer# = 0.0
												EndIf
											EndIf
											
											AnimateNPC(n, Max(Min(AnimTime(n\obj),358.0),346), 393.0, n\CurrSpeed*38)
											
											;Playing a sound if he hears the player
											If n\PrevState = 0 And ChannelPlaying(n\SoundChn2)=False
												If n\Sound2 <> 0 Then FreeSound_Strict(n\Sound2)
												If Rand(30)=1
													n\Sound2 = LoadSound_Strict("SFX\SCP\049\Searching7.ogg")
												Else
													n\Sound2 = LoadSound_Strict("SFX\SCP\049\Searching"+Rand(1,6)+".ogg")
												EndIf
												n\SoundChn2 = LoopSound2(n\Sound2,n\SoundChn2,Camera,n\obj)
												n\PrevState = 1
											EndIf
											
											;Resetting the "PrevState" value randomly, to make 049 talking randomly 
											If Rand(600)=1 And ChannelPlaying(n\SoundChn2)=False Then n\PrevState = 0
											
											If n\PrevState > 1 Then n\PrevState = 1
										EndIf
									Else ;No Path to the player found - stands still and tries to find a path
										;[Block]
										n\PathTimer# = n\PathTimer# + FPSfactor
										If n\PathTimer# > 70*(5-(2*SelectedDifficulty\aggressiveNPCs)) Then
											n\PathStatus = FindPath(n, EntityX(Collider),EntityY(Collider),EntityZ(Collider))
											n\PathTimer# = 0.0
											n\State3 = 0
											
											;Attempt to find a room (the Playerroom or one of it's adjacent rooms) for 049 to go to but select the one closest to him
											If n\PathStatus <> 1 Then
												Local closestdist# = EntityDistance(PlayerRoom\obj,n\Collider)
												Local closestRoom.Rooms = PlayerRoom
												Local currdist# = 0.0
												For i = 0 To 3
													If PlayerRoom\Adjacent[i]<>Null Then
														currdist = EntityDistance(PlayerRoom\Adjacent[i]\obj,n\Collider)
														If currdist < closestdist Then
															closestdist = currdist
															closestRoom = PlayerRoom\Adjacent[i]
														EndIf
													EndIf
												Next
												n\PathStatus = FindPath(n,EntityX(closestRoom\obj),0.5,EntityZ(closestRoom\obj))
												DebugLog "Find path for 049 in another room (pathstatus: "+n\PathStatus+")"
											EndIf
											
											;Making 3 attempts at finding a path
											While Int(n\State3) < 3
												;Breaking up the path if no "real" path has been found (only 1 waypoint and it is too close)
												If n\PathStatus = 1 Then
													If n\Path[1]<>Null Then
														If n\Path[2]=Null And EntityDistance(n\Path[1]\obj,n\Collider)<0.4 Then
															n\PathLocation = 0
															n\PathStatus = 0
															DebugLog "Breaking up path for 049 because no waypoint number 2 has been found and waypoint number 1 is too close."
														EndIf
													EndIf
													If n\Path[0]<>Null And n\Path[1]=Null Then
														n\PathLocation = 0
														n\PathStatus = 0
														DebugLog "Breaking up path for 049 because no waypoint number 1 has been found."
													EndIf
												EndIf
												
												;No path could still be found, just make 049 go to a room (further away than the very first attempt)
												If n\PathStatus <> 1 Then
													closestdist# = 100.0 ;Prevent the PlayerRoom to be considered the closest, so 049 wouldn't try to find a path there
													closestRoom.Rooms = PlayerRoom
													currdist# = 0.0
													For i = 0 To 3
														If PlayerRoom\Adjacent[i]<>Null Then
															currdist = EntityDistance(PlayerRoom\Adjacent[i]\obj,n\Collider)
															If currdist < closestdist Then
																closestdist = currdist
																For j = 0 To 3
																	If PlayerRoom\Adjacent[i]\Adjacent[j]<>Null Then
																		If PlayerRoom\Adjacent[i]\Adjacent[j]<>PlayerRoom Then
																			closestRoom = PlayerRoom\Adjacent[i]\Adjacent[j]
																			Exit
																		EndIf
																	EndIf
																Next
															EndIf
														EndIf
													Next
													n\PathStatus = FindPath(n,EntityX(closestRoom\obj),0.5,EntityZ(closestRoom\obj))
													DebugLog "Find path for 049 in another further away room (pathstatus: "+n\PathStatus+")"
												EndIf
												
												;Making 049 skip waypoints for doors he can't interact with, but only if the actual path is behind him
												If n\PathStatus = 1 Then
													If n\Path[1]<>Null Then
														If n\Path[1]\door<>Null Then
															If (n\Path[1]\door\locked Or n\Path[1]\door\KeyCard<>0 Or n\Path[1]\door\Code<>"") And (Not n\Path[1]\door\open) Then
																Repeat
																	If n\PathLocation > 19
																		n\PathLocation = 0 : n\PathStatus = 0 : Exit
																	Else
																		n\PathLocation = n\PathLocation + 1
																	EndIf
																	If n\Path[n\PathLocation]<>Null Then
																		If Abs(DeltaYaw(n\Collider,n\Path[n\PathLocation]\obj))>(45.0-Abs(DeltaYaw(n\Collider,n\Path[1]\obj))) Then
																			DebugLog "Skip until waypoint number "+n\PathLocation
																			n\State3 = 3
																			Exit
																		EndIf
																	EndIf
																Forever
															Else
																n\State3 = 3
															EndIf
														Else
															n\State3 = 3
														EndIf
													EndIf
												EndIf
												n\State3 = n\State3 + 1
											Wend
										EndIf
										AnimateNPC(n, 269, 345, 0.2)
										;[End Block]
									EndIf
								EndIf
								
								If n\CurrSpeed > 0.005 Then
									If (prevFrame < 361 And n\Frame=>361) Or (prevFrame < 377 And n\Frame=>377) Then
										PlaySound2(StepSFX(3,0,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.8,1.0))						
									ElseIf (prevFrame < 431 And n\Frame=>431) Or (prevFrame < 447 And n\Frame=>447) Then
										PlaySound2(StepSFX(3,0,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.8,1.0))
									EndIf
								EndIf
								
								UpdateSoundOrigin(n\SoundChn2,Camera,n\obj)
							ElseIf (Not n\Idle)
								If ChannelPlaying(n\SoundChn) Then
									StopChannel(n\SoundChn)
								EndIf
								If PlayerInReachableRoom(True) And InFacility=1 Then ;Player is in a room where SCP-049 can teleport to
									If Rand(1,3-SelectedDifficulty\otherFactors)=1 Then
										TeleportCloser(n)
										DebugLog "SCP-049 teleported closer due to distance"
									Else
										n\Idle = 60*70
										DebugLog "SCP-049 is now idle"
									EndIf
								EndIf
							EndIf
							;[End Block]
						Case 3 ;The player was killed by SCP-049
							;[Block]
							AnimateNPC(n, 537, 660, 0.7, False)
							
							;Animate2(n\obj, AnimTime(n\obj), 537, 660, 0.7, False)
							PositionEntity n\Collider, CurveValue(EntityX(Collider),EntityX(n\Collider),20.0),EntityY(n\Collider),CurveValue(EntityZ(Collider),EntityZ(n\Collider),20.0)
							RotateEntity n\Collider, 0, CurveAngle(EntityYaw(Collider)-180.0,EntityYaw(n\Collider),40), 0
							;[End Block]
						Case 4 ;Standing on catwalk in room4
							;[Block]
							If dist < 8.0 Then
								AnimateNPC(n, 18, 19, 0.05)
								
								;Animate2(n\obj, AnimTime(n\obj), 18, 19, 0.05)
								PointEntity n\obj, Collider	
								RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj), EntityYaw(n\Collider), 45.0), 0
								
								n\State3 = 1
							ElseIf dist > HideDistance*0.8 And n\State3 > 0 Then
								n\State = 2
								n\State3 = 0
								n\PathStatus = 0
								n\PathTimer = 0.0
								n\PathLocation = 0
								n\State2 = 0
								n\PrevState = 0
								
								For r.Rooms = Each Rooms
									If EntityDistance(r\obj,n\Collider)<4.0 Then
										TeleportEntity(n\Collider,EntityX(r\obj),0.1,EntityZ(r\obj),n\CollRadius,True)
										Exit
									EndIf
								Next
							EndIf
							;[End Block]
						Case 5 ;used for "room2sl"
							;[Block]
							n\SoundChn = LoopSound2(n\Sound, n\SoundChn, Camera, n\Collider)
							PlayerSeeAble% = MeNPCSeesPlayer(n,True)
							If PlayerSeeAble% = True
								n\State = 2
								n\PathStatus = 0
								n\PathLocation = 0
								n\PathTimer = 0
								n\State3 = 0
								n\State2 = 70*2
								n\PrevState = 0
								PlaySound_Strict LoadTempSound("SFX\Room\Room2SL049Spawn.ogg")
							ElseIf PlayerSeeAble% = 2 And n\State3 > 0.0
								n\PathStatus = FindPath(n,EntityX(Collider),EntityY(Collider),EntityZ(Collider))
							Else
								If n\State3 = 6.0
									If EntityDistance(n\Collider,Collider)>HideDistance
										n\State = 2
										n\PathStatus = 0
										n\PathLocation = 0
										n\PathTimer = 0
										n\State3 = 0
										n\PrevState = 0
									Else
										If n\PathStatus <> 1 Then n\PathStatus = FindPath(n,EntityX(Collider),EntityY(Collider),EntityZ(Collider))
									EndIf
								EndIf
								
								If n\PathStatus = 1
									If n\Path[n\PathLocation]=Null
										If n\PathLocation > 19 Then
											n\PathLocation = 0 : n\PathStatus = 0
										Else
											n\PathLocation = n\PathLocation + 1
										EndIf
									Else
										n\CurrSpeed = CurveValue(n\Speed, n\CurrSpeed, 20.0)
										PointEntity n\obj,n\Path[n\PathLocation]\obj
										RotateEntity n\Collider,0,CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),10.0),0
										MoveEntity n\Collider,0,0,n\CurrSpeed*FPSfactor
										
										;closes doors behind him
										If n\PathLocation>0 Then
											If n\Path[n\PathLocation-1] <> Null
												d = n\Path[n\PathLocation-1]\door
												If d <> Null Then
													If d\KeyCard=0
														If EntityDistance(n\Path[n\PathLocation-1]\obj,n\Collider)>0.3
															If d\open And (d\openstate <= 0 Lor d\openstate >= 180) Then UseDoor(d, False)
														EndIf
													EndIf
												EndIf
											EndIf
										EndIf
										
										;opens doors in front of him
										dist2# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
										If dist2 < 0.6 Then
											If n\Path[n\PathLocation]\door <> Null Then
												If n\Path[n\PathLocation]\door\open = False Then UseDoor(n\Path[n\PathLocation]\door, False)
											EndIf
										EndIf
										
										If dist2#<0.2
											n\PathLocation = n\PathLocation + 1
										EndIf
										
										AnimateNPC(n, Max(Min(AnimTime(n\obj),358.0),346), 393.0, n\CurrSpeed*38)
									EndIf
								Else
									Select n\PrevState
										Case 0
											AnimateNPC(n, 269, 345, 0.2)
										Case 1
											AnimateNPC(n, 661, 891, 0.4, False)
										Case 2
											AnimateNPC(n, 892, 1119, 0.4, False)
									End Select
								EndIf
							EndIf
							
							If PlayerRoom\RoomTemplate\Name = "room2sl" Then
								ShouldPlay = 20
							EndIf
							
							If n\CurrSpeed > 0.005 Then
								If (prevFrame < 361 And n\Frame=>361) Or (prevFrame < 377 And n\Frame=>377) Then
									PlaySound2(StepSFX(3,0,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.8,1.0))						
								ElseIf (prevFrame < 431 And n\Frame=>431) Or (prevFrame < 447 And n\Frame=>447)
									PlaySound2(StepSFX(3,0,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.8,1.0))
								EndIf
							EndIf
							
							UpdateSoundOrigin(n\SoundChn2,Camera,n\obj)
							;[End Block]
					End Select
				EndIf
				
				PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider)-0.22, EntityZ(n\Collider))
				
				RotateEntity n\obj, 0, EntityYaw(n\Collider), 0
				
				n\LastSeen = Max(n\LastSeen-FPSfactor,0)
				
				n\State2 = Max(n\State2-FPSfactor,0)
				
				;[End Block]
			Case NPCtypeZombie
				;[Block]
				
				If Abs(EntityY(Collider)-EntityY(n\Collider))<4.0 Then
					
					prevFrame# = n\Frame
					
					If (Not n\IsDead)
						Select n\State
							Case 0
								;[Block]
								AnimateNPC(n, 719, 777, 0.2, False)
								
								If n\Frame=777 Then
									If Rand(700)=1 Then
										If EntityDistance(Collider, n\Collider)<5.0 Then
											n\Frame = 719
										EndIf
									EndIf
								EndIf
								;[End Block]
							Case 1 ;stands up
								;[Block]
								If n\Frame=>682 Then 
									AnimateNPC(n, 926, 935, 0.3, False)
									If n\Frame = 935 Then n\State = 2
									
									;Animate2(n\obj, AnimTime(n\obj), 926, 935, 0.3, False)
									;If AnimTime(n\obj)=935 Then n\State = 2
								Else
									AnimateNPC(n, 155, 682, 1.5, False)
									;Animate2(n\obj, AnimTime(n\obj), 155, 682, 1.5, False)
								EndIf
								;[End Block]
							Case 2 ;following the player
								;[Block]
								If n\State3 < 0 Then ;check if the player is visible every three seconds
									If EntityDistance(Collider, n\Collider)<5.0 Then 
										If EntityVisible(Collider, n\Collider) Then n\State2 = 70*5
									EndIf
									n\State3=70*3
								Else
									n\State3=n\State3-FPSfactor
								EndIf
								
								If n\State2 > 0 And (Not NoTarget) Then ;player is visible -> attack
									n\SoundChn = LoopSound2(n\Sound, n\SoundChn, Camera, n\Collider, 6.0, 0.6)
									
									n\PathStatus = 0
									
									dist = EntityDistance(Collider, n\Collider)
									
									PointEntity n\obj, Collider
									RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj), EntityYaw(n\Collider), 30.0), 0
									
									If dist < 0.7 Then 
										n\State = 3
										If Rand(2)=1 Then
											n\Frame = 2
											;SetAnimTime n\obj, 2
										Else
											n\Frame = 66
											;SetAnimTime n\obj, 66
										EndIf
									Else
										n\CurrSpeed = CurveValue(n\Speed, n\CurrSpeed, 20.0)
										MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
										
										AnimateNPC(n, 936, 1017, n\CurrSpeed*60)
										
										;Animate2(n\obj, AnimTime(n\obj), 936, 1017, n\CurrSpeed*60)
										
									EndIf
									
									n\State2=n\State2-FPSfactor
								Else
									If n\PathStatus = 1 Then ;path found
										If n\Path[n\PathLocation]=Null Then 
											If n\PathLocation > 19 Then 
												n\PathLocation = 0 : n\PathStatus = 0
											Else
												n\PathLocation = n\PathLocation + 1
											EndIf
										Else
											PointEntity n\obj, n\Path[n\PathLocation]\obj
											
											RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj), EntityYaw(n\Collider), 30.0), 0
											n\CurrSpeed = CurveValue(n\Speed, n\CurrSpeed, 20.0)
											MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
											
											AnimateNPC(n, 936, 1017, n\CurrSpeed*60)
											;Animate2(n\obj, AnimTime(n\obj), 936, 1017, n\CurrSpeed*60)
											
											If EntityDistance(n\Collider,n\Path[n\PathLocation]\obj) < 0.2 Then
												n\PathLocation = n\PathLocation + 1
											EndIf 
										EndIf
									Else ;no path to the player, stands still
										n\CurrSpeed = 0
										AnimateNPC(n, 778, 926, 0.1)
										;Animate2(n\obj, AnimTime(n\obj), 778, 926, 0.1)
										
										n\PathTimer = n\PathTimer-FPSfactor
										If n\PathTimer =< 0 Then
											n\PathStatus = FindPath(n, EntityX(Collider),EntityY(Collider)+0.1,EntityZ(Collider))
											n\PathTimer = n\PathTimer+70*5
										EndIf
									EndIf
								EndIf
								
								;65, 80, 93, 109, 123
								If n\CurrSpeed > 0.005 Then
									If (prevFrame < 977 And n\Frame=>977) Or (prevFrame > 1010 And n\Frame<940) Then
										;PlaySound2(StepSFX(0,0,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.3,0.5))
										PlaySound2(StepSFX(2,0,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.3,0.5))
									EndIf
								EndIf
								;[End Block]
							Case 3
								;[Block]
								If NoTarget Then n\State = 2
								If n\Frame < 66 Then
									AnimateNPC(n, 2, 65, 0.7, False)
									
									;Animate2(n\obj, AnimTime(n\obj), 2, 65, 0.7, False)
									If prevFrame < 23 And n\Frame=>23 Then
										If EntityDistance(n\Collider,Collider)<1.1
											If (Abs(DeltaYaw(n\Collider,Collider))<=60.0)
												PlaySound_Strict DamageSFX(Rand(5,8))
												Injuries = Injuries+Rnd(0.4,1.0)
												DeathMSG = I_Loc\DeathMessage_0492
											EndIf
										EndIf
									ElseIf n\Frame=65 Then
										n\State = 2
									EndIf							
								Else
									AnimateNPC(n, 66, 132, 0.7, False)
									;Animate2(n\obj, AnimTime(n\obj), 66, 132, 0.7, False)
									If prevFrame < 90 And n\Frame=>90 Then
										If EntityDistance(n\Collider,Collider)<1.1
											If (Abs(DeltaYaw(n\Collider,Collider))<=60.0)
												PlaySound_Strict DamageSFX(Rand(5,8))
												Injuries = Injuries+Rnd(0.4,1.0)
												DeathMSG = I_Loc\DeathMessage_0492
											EndIf
										EndIf
									ElseIf n\Frame=132 Then
										n\State = 2
									EndIf		
								EndIf
								;[End Block]
						End Select
					Else
						AnimateNPC(n, 133, 157, 0.5, False)
					EndIf
					
					PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) - 0.2, EntityZ(n\Collider))
					
					RotateEntity n\obj, -90, EntityYaw(n\Collider), 0
				EndIf
				
				;[End Block]
			Case NPCtypeGuard ;------------------------------------------------------------------------------------------------------------------
				;[Block]
				prevFrame# = n\Frame
				
				n\BoneToManipulate = ""
				;n\BoneToManipulate2 = ""
				n\ManipulateBone = False
				n\ManipulationType = 0
				n\NPCNameInSection = "Guard"
				
				Select n\State
					Case 1 ;aims and shoots at the player
						;[Block]
						If n\Frame < 39 Or (n\Frame > 76 And n\Frame < 245) Or (n\Frame > 248 And n\Frame < 302) Or n\Frame > 344
							AnimateNPC(n,345,357,0.2,False)
							If n\Frame >= 356 Then SetNPCFrame(n,302)
						EndIf
						;Animate2(n\obj, AnimTime(n\obj), 1539, 1553, 0.2, False)
						
						If KillTimer => 0 Then
							dist = EntityDistance(n\Collider,Collider)
							Local ShootAccuracy# = 0.4+0.5*SelectedDifficulty\aggressiveNPCs
							Local DetectDistance# = 11.0
							
							;If at Gate B increase his distance so that he can shoot the player from a distance after they are spotted.
							If PlayerRoom\RoomTemplate\Name = "exit1" Then
								DetectDistance = 21.0
								ShootAccuracy = 0.0
								If Rand(1,8-SelectedDifficulty\aggressiveNPCs*4)<2 Then ShootAccuracy = 0.03
								
								;increase accuracy if the player is going slow
								ShootAccuracy = ShootAccuracy + (0.5 - CurrSpeed*20)
							EndIf
							
							If dist < DetectDistance Then
								pvt% = CreatePivot()
								PositionEntity(pvt, EntityX(n\Collider), EntityY(n\Collider), EntityZ(n\Collider))
								PointEntity(pvt, Collider)
								RotateEntity(pvt, Min(EntityPitch(pvt), 20), EntityYaw(pvt), 0)
								
								RotateEntity(n\Collider, CurveAngle(EntityPitch(pvt), EntityPitch(n\Collider), 10), CurveAngle(EntityYaw(pvt), EntityYaw(n\Collider), 10), 0, True)
								
								PositionEntity(pvt, EntityX(n\Collider), EntityY(n\Collider)+0.8, EntityZ(n\Collider))
								PointEntity(pvt, Collider)
								RotateEntity(pvt, Min(EntityPitch(pvt), 40), EntityYaw(n\Collider), 0)
								
								If n\Reload = 0 ;And n\Frame>1550 Then
									DebugLog "entitypick"
									EntityPick(pvt, dist)
									If PickedEntity() = Collider Or n\State3=1 Then
										Local instaKillPlayer% = False
										
										If PlayerRoom\RoomTemplate\Name = "start" Then 
											DeathMSG = I_Loc\DeathMessage_GuardIntro
											instaKillPlayer = True
										ElseIf PlayerRoom\RoomTemplate\Name = "exit1" Then
											DeathMSG = I_Loc\DeathMessage_GuardGateB
										Else
											DeathMSG = ""
										EndIf
										
										PlaySound2(GunshotSFX, Camera, n\Collider, 35)
										
										RotateEntity(pvt, EntityPitch(n\Collider), EntityYaw(n\Collider), 0, True)
										PositionEntity(pvt, EntityX(n\obj), EntityY(n\obj), EntityZ(n\obj))
										MoveEntity (pvt,0.8*0.079, 10.75*0.079, 6.9*0.079)
										
										PointEntity pvt, Collider
										Shoot(EntityX(pvt), EntityY(pvt), EntityZ(pvt), ShootAccuracy, False, instaKillPlayer)
										n\Reload = 7
									Else
										n\CurrSpeed = n\Speed
									End If
								EndIf
								
								If n\Reload > 0 And n\Reload <= 7
									AnimateNPC(n,245,248,0.35,True)
								Else
									If n\Frame < 302
										AnimateNPC(n,302,344,0.35,True)
									EndIf
								EndIf
								
								FreeEntity(pvt)
							Else
								AnimateNPC(n,302,344,0.35,True)
							EndIf
							
							n\ManipulateBone = True
							
							If n\State2 = 10 Then ;Hacky way of applying spine pitch to specific guards.
								n\BoneToManipulate = "Chest"
								n\ManipulationType = 3
							Else
								n\BoneToManipulate = "Chest"
								n\ManipulationType = 0
							EndIf
						Else
							n\State = 0
						EndIf
						;[End Block]
					Case 2 ;shoots
						;[Block]
						AnimateNPC(n,245,248,0.35,True)
						;DebugLog "shoot"
						;Animate2(n\obj, AnimTime(n\obj), 1539, 1553, 0.35, False)
						If n\Reload = 0 ;And n\Frame > 1545 Then 
							PlaySound2(GunshotSFX, Camera, n\Collider, 20)
							p.Particles = CreateParticle(EntityX(n\obj, True), EntityY(n\obj, True), EntityZ(n\obj, True), 1, 0.2, 0.0, 5)
							PositionEntity(p\pvt, EntityX(n\obj), EntityY(n\obj), EntityZ(n\obj))
							RotateEntity(p\pvt, EntityPitch(n\Collider), EntityYaw(n\Collider), 0, True)
							MoveEntity (p\pvt,0.8*0.079, 10.75*0.079, 6.9*0.079)
							n\Reload = 7
						End If
						;[End Block]
					Case 3 ;follows a path
						;[Block]
						If n\PathStatus = 2 Then
							n\State = 0
							n\CurrSpeed = 0
						ElseIf n\PathStatus = 1
							If n\Path[n\PathLocation]=Null Then 
								If n\PathLocation > 19 Then 
									n\PathLocation = 0 : n\PathStatus = 0
								Else
									n\PathLocation = n\PathLocation + 1
								EndIf
							Else
								PointEntity n\obj, n\Path[n\PathLocation]\obj
								
								RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj), EntityYaw(n\Collider), 20.0), 0
								
								AnimateNPC(n,1,38,n\CurrSpeed*40)
								;Animate2(n\obj, AnimTime(n\obj), 1614, 1641, n\CurrSpeed*30)
								n\CurrSpeed = CurveValue(n\Speed*0.7, n\CurrSpeed, 20.0)
								
								MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
								
								If EntityDistance(n\Collider,n\Path[n\PathLocation]\obj) < 0.2 Then
									n\PathLocation = n\PathLocation + 1
								EndIf 
							EndIf
						Else
							n\CurrSpeed = 0
							n\State = 4
						EndIf
						;[End Block]
					Case 4
						;[Block]
						AnimateNPC(n,77,201,0.2)
						;Animate2(n\obj, AnimTime(n\obj), 923, 1354, 0.2)
						
						If Rand(400) = 1 Then n\Angle = Rnd(-180, 180)
						
						RotateEntity(n\Collider, 0, CurveAngle(n\Angle + Sin(MilliSecs() / 50) * 2, EntityYaw(n\Collider), 150.0), 0, True)
						
						dist# = EntityDistance(n\Collider, Collider)
						If dist < 15.0 Then
							
							If WrapAngle(EntityYaw(n\Collider)-DeltaYaw(n\Collider, Collider))<90 Then
								If EntityVisible(n\Collider,Collider) Then n\State = 1
							EndIf
							
						EndIf
						
						;[End Block]
					Case 5 ;following a target
						;[Block]
						
						RotateEntity n\Collider, 0, CurveAngle(VectorYaw(n\EnemyX-EntityX(n\Collider), 0, n\EnemyZ-EntityZ(n\Collider))+n\Angle, EntityYaw(n\Collider), 20.0), 0
						
						dist# = Distance(EntityX(n\Collider),EntityZ(n\Collider),n\EnemyX,n\EnemyZ)
						
						AnimateNPC(n,1,38,n\CurrSpeed*40)
						
						If dist > 2.0 Or dist < 1.0  Then
							n\CurrSpeed = CurveValue(n\Speed*Sgn(dist-1.5)*0.75, n\CurrSpeed, 10.0)
						Else
							n\CurrSpeed = CurveValue(0, n\CurrSpeed, 10.0)
						EndIf
						
						MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
						;[End Block]
					Case 7
						;[Block]
						AnimateNPC(n,77,201,0.2)
						;Animate2(n\obj, AnimTime(n\obj), 923, 1354, 0.2)
						;[End Block]
					Case 8
						
					Case 9
						;[Block]
						AnimateNPC(n,77,201,0.2)
						n\BoneToManipulate = "head"
						n\ManipulateBone = True
						n\ManipulationType = 0
						n\Angle = EntityYaw(n\Collider)
						;[End Block]
					Case 10
						;[Block]
						AnimateNPC(n, 1, 38, n\CurrSpeed*40)
						
						n\CurrSpeed = CurveValue(n\Speed*0.7, n\CurrSpeed, 20.0)
						
						MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
						;[End Block]
					Case 11
						;[Block]
						If n\Frame < 39 Or (n\Frame > 76 And n\Frame < 245) Or (n\Frame > 248 And n\Frame < 302) Or n\Frame > 344
							AnimateNPC(n,345,357,0.2,False)
							If n\Frame >= 356 Then SetNPCFrame(n,302)
						EndIf
						
						If KillTimer => 0 Then
							dist = EntityDistance(n\Collider,Collider)
							
							Local SearchPlayer% = False
							If dist < 11.0
								If EntityVisible(n\Collider,Collider)
									SearchPlayer = True
								EndIf
							EndIf
							
							If SearchPlayer
								pvt% = CreatePivot()
								PositionEntity(pvt, EntityX(n\Collider), EntityY(n\Collider), EntityZ(n\Collider))
								PointEntity(pvt, Collider)
								RotateEntity(pvt, Min(EntityPitch(pvt), 20), EntityYaw(pvt), 0)
								
								RotateEntity(n\Collider, CurveAngle(EntityPitch(pvt), EntityPitch(n\Collider), 10), CurveAngle(EntityYaw(pvt), EntityYaw(n\Collider), 10), 0, True)
								
								PositionEntity(pvt, EntityX(n\Collider), EntityY(n\Collider)+0.8, EntityZ(n\Collider))
								PointEntity(pvt, Collider)
								RotateEntity(pvt, Min(EntityPitch(pvt), 40), EntityYaw(n\Collider), 0)
								
								If n\Reload = 0
									DebugLog "entitypick"
									EntityPick(pvt, dist)
									If PickedEntity() = Collider Or n\State3=1 Then
										instaKillPlayer% = False
										
										DeathMSG = ""
										
										PlaySound2(GunshotSFX, Camera, n\Collider, 35)
										
										RotateEntity(pvt, EntityPitch(n\Collider), EntityYaw(n\Collider), 0, True)
										PositionEntity(pvt, EntityX(n\obj), EntityY(n\obj), EntityZ(n\obj))
										MoveEntity (pvt,0.8*0.079, 10.75*0.079, 6.9*0.079)
										
										PointEntity pvt, Collider
										Shoot(EntityX(pvt), EntityY(pvt), EntityZ(pvt), 1.0, False, instaKillPlayer)
										n\Reload = 7
									Else
										n\CurrSpeed = n\Speed
									End If
								EndIf
								
								If n\Reload > 0 And n\Reload <= 7
									AnimateNPC(n,245,248,0.35,True)
								Else
									If n\Frame < 302
										AnimateNPC(n,302,344,0.35,True)
									EndIf
								EndIf
								
								FreeEntity(pvt)
							Else
								If n\PathStatus = 1
									If n\Path[n\PathLocation]=Null Then 
										If n\PathLocation > 19 Then 
											n\PathLocation = 0 : n\PathStatus = 0
										Else
											n\PathLocation = n\PathLocation + 1
										EndIf
									Else
										AnimateNPC(n,39,76,n\CurrSpeed*40)
										n\CurrSpeed = CurveValue(n\Speed*0.7, n\CurrSpeed, 20.0)
										MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
										
										PointEntity n\obj, n\Path[n\PathLocation]\obj
										
										RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj), EntityYaw(n\Collider), 20.0), 0
										
										If EntityDistance(n\Collider,n\Path[n\PathLocation]\obj) < 0.2 Then
											n\PathLocation = n\PathLocation + 1
										EndIf
									EndIf
								Else
									If n\PathTimer = 0 Then n\PathStatus = FindPath(n,EntityX(Collider),EntityY(Collider)+0.5,EntityZ(Collider))
									
									Local wayPointCloseToPlayer.WayPoints
									wayPointCloseToPlayer = Null
									
									For wp.WayPoints = Each WayPoints
										If EntityDistance(wp\obj,Collider)<2.0
											wayPointCloseToPlayer = wp
											Exit
										EndIf
									Next
									
									If wayPointCloseToPlayer<>Null
										n\PathTimer = 1
										If EntityVisible(wayPointCloseToPlayer\obj,n\Collider)
											If Abs(DeltaYaw(n\Collider,wayPointCloseToPlayer\obj))>0
												PointEntity n\obj, wayPointCloseToPlayer\obj
												RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj), EntityYaw(n\Collider), 20.0), 0
											EndIf
										EndIf
									Else
										n\PathTimer = 0
									EndIf
									
									If n\PathTimer = 1
										AnimateNPC(n,39,76,n\CurrSpeed*40)
										n\CurrSpeed = CurveValue(n\Speed*0.7, n\CurrSpeed, 20.0)
										MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
									EndIf
								EndIf
								
								If prevFrame < 43 And n\Frame=>43 Then
									PlaySound2(StepSFX(2,0,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.5,0.7))						
								ElseIf prevFrame < 61 And n\Frame=>61
									PlaySound2(StepSFX(2,0,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.5,0.7))
								EndIf
							EndIf
							
						Else
							n\State = 0
						EndIf
						;[End Block]
					Case 12
						;[Block]
						If n\Frame < 39 Or (n\Frame > 76 And n\Frame < 245) Or (n\Frame > 248 And n\Frame < 302) Or n\Frame > 344
							AnimateNPC(n,345,357,0.2,False)
							If n\Frame >= 356 Then SetNPCFrame(n,302)
						EndIf
						If n\Frame < 345
							AnimateNPC(n,302,344,0.35,True)
						EndIf
						
						pvt% = CreatePivot()
						PositionEntity(pvt, EntityX(n\Collider), EntityY(n\Collider), EntityZ(n\Collider))
						If n\State2 = 1.0
							PointEntity(pvt, Collider)
						Else
							RotateEntity pvt,0,n\Angle,0
						EndIf
						RotateEntity(pvt, Min(EntityPitch(pvt), 20), EntityYaw(pvt), 0)
						
						RotateEntity(n\Collider, CurveAngle(EntityPitch(pvt), EntityPitch(n\Collider), 10), CurveAngle(EntityYaw(pvt), EntityYaw(n\Collider), 10), 0, True)
						
						PositionEntity(pvt, EntityX(n\Collider), EntityY(n\Collider)+0.8, EntityZ(n\Collider))
						If n\State2 = 1.0
							PointEntity(pvt, Collider)
							n\ManipulateBone = True
							n\BoneToManipulate = "Chest"
							n\ManipulationType = 0
						Else
							RotateEntity pvt,0,n\Angle,0
						EndIf
						RotateEntity(pvt, Min(EntityPitch(pvt), 40), EntityYaw(n\Collider), 0)
						
						FreeEntity(pvt)
						
						UpdateSoundOrigin(n\SoundChn,Camera,n\Collider,20)
						;[End Block]
					Case 13
						;[Block]
						AnimateNPC(n,202,244,0.35,True)
						;[End Block]
					Case 14
						;[Block]
						If n\PathStatus = 2 Then
							n\State = 13
							n\CurrSpeed = 0
						ElseIf n\PathStatus = 1
							If n\Path[n\PathLocation]=Null Then 
								If n\PathLocation > 19 Then 
									n\PathLocation = 0 : n\PathStatus = 0
								Else
									n\PathLocation = n\PathLocation + 1
								EndIf
							Else
								PointEntity n\obj, n\Path[n\PathLocation]\obj
								
								RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj), EntityYaw(n\Collider), 20.0), 0
								
								AnimateNPC(n,39,76,n\CurrSpeed*40)
								n\CurrSpeed = CurveValue(n\Speed*0.7, n\CurrSpeed, 20.0)
								
								MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
								
								If EntityDistance(n\Collider,n\Path[n\PathLocation]\obj) < 0.2 Then
									n\PathLocation = n\PathLocation + 1
								EndIf 
							EndIf
						Else
							n\CurrSpeed = 0
							n\State = 13
						EndIf
						
						If prevFrame < 43 And n\Frame=>43 Then
							PlaySound2(StepSFX(2,0,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.5,0.7))						
						ElseIf prevFrame < 61 And n\Frame=>61
							PlaySound2(StepSFX(2,0,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.5,0.7))
						EndIf
						;[End Block]
					Default
						;[Block]
						If Rand(400) = 1 Then n\PrevState = Rnd(-30, 30)
						n\PathStatus = 0
						AnimateNPC(n,77,201,0.2)
						
						RotateEntity(n\Collider, 0, CurveAngle(n\Angle + n\PrevState + Sin(MilliSecs() / 50) * 2, EntityYaw(n\Collider), 50), 0, True)
						;[End Block]
				End Select
				
				If n\CurrSpeed > 0.01 Then
					If prevFrame < 5 And n\Frame>=5
						PlaySound2(StepSFX(2,0,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.5,0.7))						
					ElseIf prevFrame < 23 And n\Frame>=23
						PlaySound2(StepSFX(2,0,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.5,0.7))						
					EndIf
				EndIf
				
				If n\Frame > 286.5 And n\Frame < 288.5
					n\IsDead = True
				EndIf
				If AnimTime(n\obj) > 286.5 And AnimTime(n\obj) < 288.5 Then
					n\IsDead = True
				EndIf
				
				n\Reload = Max(0, n\Reload - FPSfactor)
				;RotateEntity(n\Collider, 0, EntityYaw(n\Collider), 0, True)
				PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) - 0.2, EntityZ(n\Collider))
				
				;RotateEntity n\obj, EntityPitch(n\Collider)-90, EntityYaw(n\Collider), 0
				RotateEntity n\obj, 0, EntityYaw(n\Collider)+180, 0
				;[End Block]
			Case NPCtypeMTF ;------------------------------------------------------------------------------------------------------------------
				;[Block]
				UpdateMTFUnit(n)
				
				;[End Block]
			Case NPCtypeD9341
			;[Block]
				If n = CurrD9341 And n\State < 100 Then
					UpdateD9341BodyNPC(n)
				Else
					UpdateD9341GameNPC(n)
				EndIf
				;[End Block]
		Case NPCtypeD,NPCtypeClerk,NPCtypeJ,NPCtypeD2,NPCtypeD3,NPCtypeS1,NPCtypeS2,NPCtypeG,NPCtypeB1,NPCtypeB2 	;------------------------------------------------------------------------------------------------------------------
				;[Block]
				If n\NPCtype = NPCtypeClerk And n\State >= 100 Then
					UpdateClerkTeslaNPC(n)
				Else
					RotateEntity(n\Collider, 0, EntityYaw(n\Collider), EntityRoll(n\Collider), True)
				
				prevFrame = AnimTime(n\obj)
				
				Select n\State
					Case 0 ;idle
						n\CurrSpeed = CurveValue(0.0, n\CurrSpeed, 5.0)
						Animate2(n\obj, AnimTime(n\obj), 210, 235, 0.1)
					Case 1 ;walking
						If n\State2 = 1.0
							n\CurrSpeed = CurveValue(n\Speed*0.7, n\CurrSpeed, 20.0)
						Else
							n\CurrSpeed = CurveValue(0.015, n\CurrSpeed, 5.0)
						EndIf
						Animate2(n\obj, AnimTime(n\obj), 236, 260, n\CurrSpeed * 18)
					Case 2 ;running
						n\CurrSpeed = CurveValue(0.03, n\CurrSpeed, 5.0)
						Animate2(n\obj, AnimTime(n\obj), 301, 319, n\CurrSpeed * 18)
				End Select
				
				If n\State2 <> 2
					If n\State = 1
						If n\CurrSpeed > 0.01 Then
							If prevFrame < 244 And AnimTime(n\obj)=>244 Then
								PlaySound2(StepSFX(GetStepSound(n\Collider),0,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.3,0.5))						
							ElseIf prevFrame < 256 And AnimTime(n\obj)=>256
								PlaySound2(StepSFX(GetStepSound(n\Collider),0,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.3,0.5))
							EndIf
						EndIf
					ElseIf n\State = 2
						If n\CurrSpeed > 0.01 Then
							If prevFrame < 309 And AnimTime(n\obj)=>309
								PlaySound2(StepSFX(GetStepSound(n\Collider),1,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.3,0.5))
							ElseIf prevFrame =< 319 And AnimTime(n\obj)=<301
								PlaySound2(StepSFX(GetStepSound(n\Collider),1,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.3,0.5))
							EndIf
						EndIf
					EndIf
				EndIf
				
				If n\Frame = 19 Or n\Frame = 60
					n\IsDead = True
				EndIf
				If AnimTime(n\obj)=19 Or AnimTime(n\obj)=60
					n\IsDead = True
				EndIf
				
				MoveEntity(n\Collider, 0, 0, n\CurrSpeed * FPSfactor)
				
				PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) - 0.32, EntityZ(n\Collider))
				
				RotateEntity n\obj, EntityPitch(n\Collider), EntityYaw(n\Collider)-180.0, 0
				EndIf
				;[End Block]
			Case NPCtypeV
				;[Block]
				Update106VictimNPC(n)
				;[End Block]
			Case NPCtypeNazi
				;[Block]
				UpdateNaziOfficerNPC(n)
				;[End Block]
			Case NPCtypeC 
				;[Block]
				Update970CorpseNPC(n)
				;[End Block]
			Case NPCtype5131
				;[Block]
				;If KeyHit(48) Then n\Idle = True : n\State2 = 0
				
				If PlayerRoom\RoomTemplate\Name <> "pocketdimension" Then 
					If n\Idle Then
						HideEntity(n\obj)
						HideEntity(n\obj2)
						If Rand(200)=1 Then
							For w.WayPoints = Each WayPoints
								If w\room<>PlayerRoom Then
									x = Abs(EntityX(Collider)-EntityX(w\obj,True))
									If x>3 And x < 9 Then
										z = Abs(EntityZ(Collider)-EntityZ(w\obj,True))
										If z>3 And z < 9 Then
											PositionEntity(n\Collider, EntityX(w\obj,True), EntityY(w\obj,True), EntityZ(w\obj,True))
											PositionEntity(n\obj, EntityX(w\obj,True), EntityY(w\obj,True), EntityZ(w\obj,True))
											ResetEntity n\Collider
											ShowEntity(n\obj)
											ShowEntity(n\obj2)
											
											n\LastSeen = 0
											
											n\Path[0]=w
											
											n\Idle = False
											n\State2 = Rand(15,20)*70
											n\State = Max(Rand(-1,2),0)
											n\PrevState = Rand(0,1)
											Exit
										EndIf
									EndIf
								EndIf
							Next
						End If
					Else
						dist = EntityDistance(Collider, n\Collider)
						
						;use the prev-values to do a "twitching" effect
						n\PrevX = CurveValue(0.0, n\PrevX, 10.0)
						n\PrevZ = CurveValue(0.0, n\PrevZ, 10.0)
						
						If Rand(100)=1 Then
							If Rand(5)=1 Then
								n\PrevX = (EntityX(Collider)-EntityX(n\Collider))*0.9
								n\PrevZ = (EntityZ(Collider)-EntityZ(n\Collider))*0.9
							Else
								n\PrevX = Rnd(0.1,0.5)
								n\PrevZ = Rnd(0.1,0.5)						
							EndIf
						EndIf
						
						temp = Rnd(-1.0,1.0)
						PositionEntity n\obj2, EntityX(n\Collider)+n\PrevX*temp, EntityY(n\Collider) - 0.2 + Sin((MilliSecs()/8-45) Mod 360)*0.05, EntityZ(n\Collider)+n\PrevZ*temp
						RotateEntity n\obj2, 0, EntityYaw(n\obj), 0
						If (Floor(AnimTime(n\obj2))<>Floor(n\Frame)) Then SetAnimTime n\obj2, n\Frame
						
						If n\State = 0 Then
							If n\PrevState=0
								AnimateNPC(n,2,74,0.2)
							Else
								AnimateNPC(n,75,124,0.2)
							EndIf
							;AnimateNPC(n, 229, 299, 0.2)
							
							If n\LastSeen Then 	
								PointEntity n\obj2, Collider
								RotateEntity n\obj, 0, CurveAngle(EntityYaw(n\obj2),EntityYaw(n\obj),40), 0
								If dist < 4 Then n\State = Rand(1,2)
							Else
								If dist < 6 And Rand(5)=1 Then
									If EntityInView(n\Collider,Camera) Then
										If EntityVisible(Collider, n\Collider) Then
											n\LastSeen = 1
											PlaySound_Strict LoadTempSound("SFX\SCP\513\Bell"+Rand(2,3)+".ogg")
										EndIf
									EndIf
								EndIf								
							EndIf
							
						Else
							If n\Path[0]=Null Then
								
								;move towards a waypoint that is:
								;1. max 8 units away from 513-1
								;2. further away from the player than 513-1's current position 
								For w.WayPoints = Each WayPoints
									x = Abs(EntityX(n\Collider,True)-EntityX(w\obj,True))
									If x < 8.0 And x > 1.0 Then
										z = Abs(EntityZ(n\Collider,True)-EntityZ(w\obj,True))
										If z < 8.0 And z > 1.0 Then
											If EntityDistance(Collider, w\obj) > dist Then
												n\Path[0]=w
												Exit
											EndIf
										EndIf
									EndIf
								Next
								
								;no suitable path found -> 513-1 simply disappears
								If n\Path[0] = Null Then
									n\Idle = True
									n\State2 = 0
								EndIf
							Else
								
								If EntityDistance(n\Collider, n\Path[0]\obj) > 1.0 Then
									PointEntity n\obj, n\Path[0]\obj
									RotateEntity n\Collider, CurveAngle(EntityPitch(n\obj),EntityPitch(n\Collider),15.0), CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),15.0), 0, True
									n\CurrSpeed = CurveValue(0.05*Max((7.0-dist)/7.0,0.0),n\CurrSpeed,15.0)
									MoveEntity n\Collider, 0,0,n\CurrSpeed*FPSfactor
									If Rand(200)=1 Then MoveEntity n\Collider, 0, 0, 0.5
									RotateEntity n\Collider, 0, EntityYaw(n\Collider), 0, True
								Else
									For i = 0 To 4
										If n\Path[0]\connected[i] <> Null Then
											If EntityDistance(Collider, n\Path[0]\connected[i]\obj) > dist Then
												
												If n\LastSeen = 0 Then 
													If EntityInView(n\Collider,Camera) Then
														If EntityVisible(Collider, n\Collider) Then
															n\LastSeen = 1
															PlaySound_Strict LoadTempSound("SFX\SCP\513\Bell"+Rand(2,3)+".ogg")
														EndIf
													EndIf
												EndIf
												
												n\Path[0]=n\Path[0]\connected[i]
												Exit
											EndIf
										EndIf
									Next
									
									If n\Path[0]=Null Then n\State2 = 0
								EndIf
							EndIf
						EndIf
						
						PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) - 0.2 + Sin((MilliSecs()/8) Mod 360)*0.1, EntityZ(n\Collider))
						
						Select n\State 
							Case 1
								If n\PrevState=0
									AnimateNPC(n,125,194,n\CurrSpeed*20)
								Else
									AnimateNPC(n,195,264,n\CurrSpeed*20)
								EndIf
								;AnimateNPC(n, 458, 527, n\CurrSpeed*20)
								RotateEntity n\obj, 0, EntityYaw(n\Collider), 0 
							Case 2
								If n\PrevState=0
									AnimateNPC(n,2,74,0.2)
								Else
									AnimateNPC(n,75,124,0.2)
								EndIf
								;AnimateNPC(n, 229, 299, 0.2)
								RotateEntity n\obj, 0, EntityYaw(n\Collider), 0						
						End Select
						
						If n\State2 > 0 Then
							If dist < 4.0 Then n\State2 = n\State2-FPSfactor*4
							n\State2 = n\State2-FPSfactor
						Else
							n\Path[0]=Null
							n\Idle = True
							n\State2=0
						EndIf
						
					End If
					
				EndIf
				
				n\DropSpeed = 0
				ResetEntity(n\Collider)						
				;[End Block]
			Case NPCtype372 ;------------------------------------------------------------------------------------------------------------------
				;[Block]
				RN$ = PlayerRoom\RoomTemplate\Name
				If RN$ <> "pocketdimension" And RN$ <> "dimension1499" Then 
					If n\Idle Then
						HideEntity(n\obj)
						If Rand(50) = 1 And (BlinkTimer < -5 And BlinkTimer > -15) Then
							ShowEntity(n\obj)
							angle# = EntityYaw(Collider)+Rnd(-90,90)
							
							dist = Rnd(1.5, 2.0)
							PositionEntity(n\Collider, EntityX(Collider) + Sin(angle) * dist, EntityY(Collider)+0.2, EntityZ(Collider) + Cos(angle) * dist)
							n\Idle = False
							n\State = Rand(20, 60)
							
							If Rand(300)=1 Then PlaySound2(RustleSFX(Rand(0,2)),Camera, n\Collider, 8, Rnd(0.0,0.2))
						End If
					EndIf
					
					If (Not n\Idle) Then
						PositionEntity(n\obj, EntityX(n\Collider) + Rnd(-0.005, 0.005), EntityY(n\Collider)+0.3+0.1*Sin(MilliSecs()/2), EntityZ(n\Collider) + Rnd(-0.005, 0.005))
						RotateEntity n\obj, 0, EntityYaw(n\Collider), ((MilliSecs()/5) Mod 360)
						
						AnimateNPC(n, 32, 113, 0.4)
						;Animate2(n\obj, AnimTime(n\obj), 32, 113, 0.4)
						
						If EntityInView(n\obj, Camera) And (BlinkTimer < - 16 Or BlinkTimer > - 6) Then
							GiveAchievement(Achv372)
							
							If Rand(30)=1 Then 
								If (Not ChannelPlaying(n\SoundChn)) Then
									If EntityVisible(Camera, n\obj) Then 
										n\SoundChn = PlaySound2(RustleSFX(Rand(0,2)),Camera, n\obj, 8, 0.3)
									EndIf
								EndIf
							EndIf
							
							temp = CreatePivot()
							PositionEntity temp, EntityX(Collider), EntityY(Collider), EntityZ(Collider)
							PointEntity temp, n\Collider
							
							angle =  WrapAngle(EntityYaw(Collider)-EntityYaw(temp))
							If angle < 180 Then
								RotateEntity n\Collider, 0, EntityYaw(Collider)-80, 0		
							Else
								RotateEntity n\Collider, 0, EntityYaw(Collider)+80, 0
							EndIf
							FreeEntity temp
							
							MoveEntity n\Collider, 0, 0, 0.03*FPSfactor
							
							n\State = n\State-FPSfactor
						EndIf
						n\State=n\State-(FPSfactor/80.0)
						If n\State <= 0 Then
							n\Idle = True
							PositionEntity(n\Collider, 0, 500, 0)
						EndIf	
					End If
					
				EndIf
				
				n\DropSpeed = 0
				ResetEntity(n\Collider)						
				;[End Block]
			Case NPCtypeApache ;------------------------------------------------------------------------------------------------------------------
				;[Block]
				dist = EntityDistance(Collider, n\Collider)
				If dist<60.0 Then 
					If PlayerRoom\RoomTemplate\Name = "exit1" Then 
						dist2 = Max(Min(EntityDistance(n\Collider, PlayerRoom\Objects[3])/(8000.0*RoomScale),1.0),0.0)
					Else 
						dist2 = 1.0
					EndIf
					
					n\SoundChn = LoopSound2(ApacheSFX, n\SoundChn, Camera, n\Collider, 25.0, dist2)
				EndIf
				
				n\DropSpeed = 0
				
				Select n\State
					Case 0,1
						TurnEntity(n\obj2,0,20.0*FPSfactor,0)
						TurnEntity(n\obj3,20.0*FPSfactor,0,0)
						
						If n\State=1 And (Not NoTarget) Then
							If Abs(EntityX(Collider)-EntityX(n\Collider))< 30.0 Then
								If Abs(EntityZ(Collider)-EntityZ(n\Collider))<30.0 Then
									If Abs(EntityY(Collider)-EntityY(n\Collider))<20.0 Then
										If Rand(20)=1 Then 
											If EntityVisible(Collider, n\Collider) Then
												n\State = 2
												PlaySound2(AlarmSFX(2), Camera, n\Collider, 50, 1.0)
											EndIf
										EndIf									
									EndIf
								EndIf
							EndIf							
						EndIf
					Case 2,3 ;player located -> attack
						
						If n\State = 2 Then 
							target = Collider
						ElseIf n\State = 3
							target=CreatePivot()
							PositionEntity target, n\EnemyX, n\EnemyY, n\EnemyZ, True
						EndIf
						
						If NoTarget And n\State = 2 Then n\State = 1
						
						TurnEntity(n\obj2,0,20.0*FPSfactor,0)
						TurnEntity(n\obj3,20.0*FPSfactor,0,0)
						
						If Abs(EntityX(target)-EntityX(n\Collider)) < 55.0 Then
							If Abs(EntityZ(target)-EntityZ(n\Collider)) < 55.0 Then
								If Abs(EntityY(target)-EntityY(n\Collider))< 20.0 Then
									PointEntity n\obj, target
									RotateEntity n\Collider, CurveAngle(Min(WrapAngle(EntityPitch(n\obj)),40.0),EntityPitch(n\Collider),40.0), CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),90.0), EntityRoll(n\Collider), True
									PositionEntity(n\Collider, EntityX(n\Collider), CurveValue(EntityY(target)+8.0,EntityY(n\Collider),70.0), EntityZ(n\Collider))
									
									dist# = Distance(EntityX(target),EntityZ(target),EntityX(n\Collider),EntityZ(n\Collider))
									
									n\CurrSpeed = CurveValue(Min(dist-6.5,6.5)*0.008, n\CurrSpeed, 50.0)
									
									;If Distance(EntityX(Collider),EntityZ(Collider),EntityX(n\collider),EntityZ(n\collider)) > 6.5 Then
									;	n\currspeed = CurveValue(0.08,n\currspeed,50.0)
									;Else
									;	n\currspeed = CurveValue(0.0,n\currspeed,30.0)
									;EndIf
									MoveEntity n\Collider, 0,0,n\CurrSpeed*FPSfactor
									
									
									If n\PathTimer = 0 Then
										n\PathStatus = EntityVisible(n\Collider,target)
										n\PathTimer = Rand(100,200)
									Else
										n\PathTimer = Min(n\PathTimer-FPSfactor,0.0)
									EndIf
									
									If n\PathStatus = 1 Then ;player visible
										RotateEntity n\Collider, EntityPitch(n\Collider), EntityYaw(n\Collider), CurveAngle(0, EntityRoll(n\Collider),40), True
										
										If n\Reload =< 0 Then
											If dist<20.0 Then
												pvt = CreatePivot()
												
												PositionEntity pvt, EntityX(n\Collider),EntityY(n\Collider), EntityZ(n\Collider)
												RotateEntity pvt, EntityPitch(n\Collider), EntityYaw(n\Collider),EntityRoll(n\Collider)
												MoveEntity pvt, 0, 8.87*(0.21/9.0), 8.87*(1.7/9.0) ;2.3
												PointEntity pvt, target
												
												If WrapAngle(EntityYaw(pvt)-EntityYaw(n\Collider))<10 Then
													PlaySound2(Gunshot2SFX, Camera, n\Collider, 20)
													
													Local deathGate$
													If PlayerRoom\RoomTemplate\Name = "exit1" Then
														deathGate = I_Loc\DeathMessage_ApacheGateB
													Else
														deathGate = I_Loc\DeathMessage_ApacheGateA
													EndIf
													DeathMSG = Format(I_Loc\DeathMessage_Apache, deathGate)
													
													Shoot( EntityX(pvt),EntityY(pvt), EntityZ(pvt),((10/dist)*(1/dist))*(n\State=2),(n\State=2))
													
													n\Reload = 5
												EndIf
												
												FreeEntity pvt
											EndIf
										EndIf
									Else 
										RotateEntity n\Collider, EntityPitch(n\Collider), EntityYaw(n\Collider), CurveAngle(-20, EntityRoll(n\Collider),40), True
									EndIf
									MoveEntity n\Collider, -EntityRoll(n\Collider)*0.002,0,0
									
									n\Reload=n\Reload-FPSfactor
									
									
								EndIf
							EndIf
						EndIf		
						
						If n\State = 3 Then FreeEntity target
					Case 4 ;crash
						If n\State2 < 300 Then
							
							TurnEntity(n\obj2,0,20.0*FPSfactor,0)
							TurnEntity(n\obj3,20.0*FPSfactor,0,0)
							
							TurnEntity n\Collider,0,-FPSfactor*7,0;Sin(MilliSecs()/40)*FPSfactor
							n\State2=n\State2+FPSfactor*0.3
							
							target=CreatePivot()
							PositionEntity target, n\EnemyX, n\EnemyY, n\EnemyZ, True
							
							PointEntity n\obj, target
							MoveEntity n\obj, 0,0,FPSfactor*0.001*n\State2
							PositionEntity(n\Collider, EntityX(n\obj), EntityY(n\obj), EntityZ(n\obj))
							
							If EntityDistance(n\obj, target) <0.3 Then
								;If TempSound2 <> 0 Then FreeSound_Strict TempSound2 : TempSound2 = 0
								;TempSound2 = LoadSound_Strict("SFX\Character\Apache\Crash"+Rand(1,2)+".ogg")
								CameraShake = Max(CameraShake, 3.0)
								;PlaySound_Strict TempSound2
								PlaySound_Strict LoadTempSound("SFX\Character\Apache\Crash"+Rand(1,2)+".ogg")
								n\State = 5
							EndIf
							
							FreeEntity target
						EndIf
				End Select
				
				PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider), EntityZ(n\Collider))
				RotateEntity n\obj, EntityPitch(n\Collider), EntityYaw(n\Collider), EntityRoll(n\Collider), True
				;[End Block]
			Case NPCtypeTentacle
				;[Block]
				dist = EntityDistance(n\Collider,Collider)
				
				If dist < HideDistance
					
					Select n\State 
						Case 0 ;spawn
							
							If n\Frame>283 Then
								HeartBeatVolume = Max(CurveValue(1.0, HeartBeatVolume, 50),HeartBeatVolume)
								HeartBeatRate = Max(CurveValue(130, HeartBeatRate, 100),HeartBeatRate)
								
								PointEntity n\obj, Collider
								RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),25.0), 0
								
								AnimateNPC(n, 283, 389, 0.3, False)
								;Animate2(n\obj, AnimTime(n\obj), 283, 389, 0.3, False)
								
								If n\Frame>388 Then
									n\State = 1
									FreeSound_Strict n\Sound2 : n\Sound2 = 0
								EndIf
							Else
								If dist < 2.5 Then 
									SetNPCFrame(n, 284)
									n\Sound2 = LoadSound_Strict("SFX\Room\035Chamber\TentacleSpawn.ogg")
									PlaySound_Strict(n\Sound2)
								EndIf
							EndIf
							;spawn 283,389
							;attack 2, 32
							;idle 33, 174
						Case 1 ;idle
							If n\Sound2=0 Then
								FreeSound_Strict n\Sound2 : n\Sound2=0
								n\Sound2 = LoadSound_Strict("SFX\Room\035Chamber\TentacleIdle.ogg")
							EndIf
							n\SoundChn2 = LoopSound2(n\Sound2,n\SoundChn2,Camera,n\Collider)
							
							If dist < 1.8 Then 
								If Abs(DeltaYaw(n\Collider, Collider))<20 Then 
									n\State = 2
									If n\Sound<>0 Then FreeSound_Strict n\Sound : n\Sound = 0 
									;If n\Sound2<>0 Then FreeSound_Strict n\Sound2 : n\Sound2 = 0
								EndIf
							EndIf
							
							PointEntity n\obj, Collider
							RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),25.0), 0
							
							AnimateNPC(n, 33, 174, 0.3, True)
							;Animate2(n\obj, AnimTime(n\obj), 33, 174, 0.3, True)
						Case 2
							
							;finish the idle animation before playing the attack animation
							If n\Frame>33 And n\Frame<174 Then
								AnimateNPC(n, 33, 174, 2.0, False)
								;Animate2(n\obj, AnimTime(n\obj), 33, 174, 2.0, False)
							Else
								PointEntity n\obj, Collider
								RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),10.0), 0							
								
								If n\Frame>33 Then 
									;SetAnimTime(n\obj,2)
									If n\Sound2<>0 Then FreeSound_Strict n\Sound2 : n\Sound2 = 0
									n\Frame = 2
									n\Sound = LoadSound_Strict("SFX\Room\035Chamber\TentacleAttack"+Rand(1,2)+".ogg")
									PlaySound_Strict(n\Sound)
								EndIf
								AnimateNPC(n, 2, 32, 0.3, False)
								;Animate2(n\obj, AnimTime(n\obj), 2, 32, 0.3, False)
								
								If n\Frame>=5 And n\Frame<6 Then
									If dist < 1.8 Then
										If Abs(DeltaYaw(n\Collider, Collider))<20 Then 
											If WearingHazmat Then
												Injuries = Injuries+Rnd(0.5)
												PlaySound_Strict(LoadTempSound("SFX\General\BodyFall.ogg"))
											Else
												BlurTimer = 100
												Injuries = Injuries+Rnd(1.0,1.5)
												PlaySound_Strict DamageSFX(Rand(2,3))
												
												If Injuries > 3.0 Then
													If PlayerRoom\RoomTemplate\Name = "room2offices" Then
														DeathMSG = I_Loc\DeathMessage_035TentacleOffice
													Else
														DeathMSG = I_Loc\DeathMessage_035Tentacle
													EndIf
													Kill()
												EndIf
											EndIf
											
										EndIf
									EndIf
									
									n\Frame = 6
									;SetAnimTime(n\obj, 6)
								ElseIf n\Frame=32
									n\State = 1
									n\Frame = 173
									;SetAnimTime(n\obj, 173)
								EndIf
							EndIf
							
					End Select
					
				EndIf
				
				PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider), EntityZ(n\Collider))
				RotateEntity n\obj, EntityPitch(n\Collider)-90, EntityYaw(n\Collider)-180, EntityRoll(n\Collider), True
				
				n\DropSpeed = 0
				
				ResetEntity n\Collider
				;[End Block]
			Case NPCtype860
				;[Block]
				If PlayerRoom\RoomTemplate\Name = "room860" Then
					Local fr.Forest=PlayerRoom\fr;Object.Forest(e\room\Objects[1])
					
					dist = EntityDistance(Collider,n\Collider)
					
					If ForestNPC<>0
						If ForestNPCData[2]=1
							ShowEntity ForestNPC
							If n\State<>1
								If (BlinkTimer<-8 And BlinkTimer >-12) Or (Not EntityInView(ForestNPC,Camera))
									ForestNPCData[2]=0
									HideEntity ForestNPC
								EndIf
							EndIf
							If ForestNPCData[1]=0.0
								If Rand(200)=1
									ForestNPCData[1]=FPSfactor
									EntityTexture ForestNPC,ForestNPCTex,ForestNPCData[0]+1
								EndIf
							ElseIf ForestNPCData[1]>0.0 And ForestNPCData[1]<5.0
								ForestNPCData[1]=Min(ForestNPCData[1]+FPSfactor,5.0)
							Else
								ForestNPCData[1]=0
								EntityTexture ForestNPC,ForestNPCTex,ForestNPCData[0]
							EndIf
						Else
							HideEntity ForestNPC
						EndIf
					EndIf
					
					Select n\State
						Case 0 ;idle (hidden)
							
							HideEntity n\Collider
							HideEntity n\obj
							HideEntity n\obj2
							
							n\State2 = 0
							PositionEntity(n\Collider, 0, -100, 0)
						Case 1 ;appears briefly behind the trees
							n\DropSpeed = 0
							
							If EntityY(n\Collider)<= -100 Then
								;transform the position of the player to the local coordinates of the forest
								TFormPoint(EntityX(Collider),EntityY(Collider),EntityZ(Collider),0,fr\Forest_Pivot)
								
								;calculate the indices of the forest cell the player is in
								x = Floor((TFormedX()+6.0)/12.0)
								z = Floor((TFormedZ()+6.0)/12.0)
								
								;step through nearby cells
								For x2 = Max(x-1,1) To Min(x+1,gridsize) Step 2
									For z2 = Max(z-1,1) To Min(z+1,gridsize) Step 2
										;choose an empty cell (not on the path)
										If fr\grid[(z2*gridsize)+x2]=0 Then
											;spawn the monster between the empty cell and the cell the player is in
											TFormPoint(((x2+x)/2)*12.0,0,((z2+z)/2)*12.0,fr\Forest_Pivot,0)
											
											PositionEntity n\Collider, TFormedX(), EntityY(fr\Forest_Pivot,True)+2.3, TFormedZ()
											
											;in view -> nope, keep searching for a more suitable cell
											If EntityInView(n\Collider, Camera) Then
												PositionEntity n\Collider, 0, -110, 0
												DebugLog("spawned monster in view -> hide")
											Else ; not in view -> all good
												DebugLog("spawned monster successfully")
												
												x2 = gridsize
												Exit												
											EndIf
										EndIf
									Next
								Next
								
								If EntityY(n\Collider)> -100 Then
									PlaySound2(Step2SFX(Rand(3,5)), Camera, n\Collider, 15.0, 0.5)
									
									If ForestNPCData[2]<>1 Then ForestNPCData[2]=0
									
									Select Rand(3)
										Case 1
											PointEntity n\Collider, Collider
											n\Frame = 2
											;SetAnimTime(n\obj, 2)
										Case 2
											PointEntity n\Collider, Collider
											n\Frame = 201
											;SetAnimTime(n\obj, 201)
										Case 3
											PointEntity n\Collider, Collider
											TurnEntity n\Collider, 0, 90, 0
											n\Frame = 299
											;SetAnimTime(n\obj, 299)
									End Select
									
									n\State2 = 0
								EndIf
							Else
								
								ShowEntity n\obj
								ShowEntity n\Collider
								
								PositionEntity n\Collider, EntityX(n\Collider), EntityY(fr\Forest_Pivot,True)+2.3, EntityZ(n\Collider)
								
								;[TODO]
								If ForestNPC<>0
									If ForestNPCData[2]=0
										Local docchance% = 0
										Local docamount% = 0
										For i = 0 To MaxItemAmount-1
											If Inventory(i)<>Null
												If Instr(Inventory(i)\itemtemplate\name, "log") = 1 Then
													;860,850,830,800
													docamount% = docamount% + 1
													docchance = docchance + 10*docamount%
												EndIf
											EndIf
										Next
										
										If Rand(1,860-docchance)=1
											ShowEntity ForestNPC
											ForestNPCData[2]=1
											If Rand(2)=1
												ForestNPCData[0]=0
											Else
												ForestNPCData[0]=2
											EndIf
											ForestNPCData[1]=0
											PositionEntity ForestNPC,EntityX(n\Collider),EntityY(n\Collider)+0.5,EntityZ(n\Collider)
											RotateEntity ForestNPC,0,EntityYaw(n\Collider),0
											MoveEntity ForestNPC,0.75,0,0
											RotateEntity ForestNPC,0,0,0
											EntityTexture ForestNPC,ForestNPCTex,ForestNPCData[0]
										Else
											ForestNPCData[2]=2
										EndIf
									EndIf
								EndIf
								
								If n\State2 = 0 Then ;don't start moving until the player is looking
									If EntityInView(n\Collider, Camera) Then 
										n\State2 = 1
										If Rand(8)=1 Then
											PlaySound2(LoadTempSound("SFX\SCP\860\Cancer"+Rand(0,2)+".ogg"), Camera, n\Collider, 20.0)
										EndIf										
									EndIf
								Else
									If n\Frame<=199 Then
										AnimateNPC(n, 2, 199, 0.5,False)
										If n\Frame=199 Then n\Frame = 298 : PlaySound2(Step2SFX(Rand(3,5)), Camera, n\Collider, 15.0)
									ElseIf n\Frame <= 297
										PointEntity n\Collider, Collider
										
										AnimateNPC(n, 200, 297, 0.5, False)
										If n\Frame=297 Then n\Frame=298 : PlaySound2(Step2SFX(Rand(3,5)), Camera, n\Collider, 15.0)
									Else
										angle = CurveAngle(point_direction(EntityX(n\Collider),EntityZ(n\Collider),EntityX(Collider),EntityZ(Collider)),EntityYaw(n\Collider)+90,20.0)
										
										RotateEntity n\Collider, 0, angle-90, 0, True
										
										AnimateNPC(n, 298, 316, n\CurrSpeed*10)
										
										;Animate2(n\obj, AnimTime(n\obj), 298, 316, n\CurrSpeed*10)
										
										n\CurrSpeed = CurveValue(n\Speed, n\CurrSpeed, 10.0)
										MoveEntity n\Collider, 0,0,n\CurrSpeed*FPSfactor
										
										If dist>15.0 Then
											PositionEntity n\Collider, 0,-110,0
											n\State = 0
											n\State2 = 0
										EndIf
									EndIf									
								EndIf
								
							EndIf
							
							ResetEntity n\Collider
						Case 2 ;appears on the path and starts to walk towards the player
							ShowEntity n\obj
							ShowEntity n\Collider
							
							prevFrame = n\Frame
							
							If EntityY(n\Collider)<= -100 Then
								;transform the position of the player to the local coordinates of the forest
								TFormPoint(EntityX(Collider),EntityY(Collider),EntityZ(Collider),0,fr\Forest_Pivot)
								
								;calculate the indices of the forest cell the player is in
								x = Floor((TFormedX()+6.0)/12.0)
								z = Floor((TFormedZ()+6.0)/12.0)
								
								For x2 = Max(x-1,1) To Min(x+1,gridsize)
									For z2 = Max(z-1,1) To Min(z+1,gridsize)
										;find a nearby cell that's on the path and NOT the cell the player is in
										If fr\grid[(z2*gridsize)+x2]>0 And (x2<>x Or z2<>z) And (x2=x Or z2=z) Then
											
											;transform the position of the cell back to world coordinates
											TFormPoint(x2*12.0, 0,z2*12.0, fr\Forest_Pivot,0)
											
											PositionEntity n\Collider, TFormedX(), EntityY(fr\Forest_Pivot,True)+1.0,TFormedZ()
											
											DebugLog(TFormedX()+", "+TFormedZ())
											
											If EntityInView(n\Collider, Camera) Then
												BlinkTimer=-10
											Else
												x2 = gridsize
												Exit
											EndIf
										EndIf
									Next
								Next
							Else
								angle = CurveAngle(Find860Angle(n, fr),EntityYaw(n\Collider)+90,80.0)
								
								RotateEntity n\Collider, 0, angle-90, 0, True
								
								n\CurrSpeed = CurveValue(n\Speed*0.3, n\CurrSpeed, 50.0)
								MoveEntity n\Collider, 0,0,n\CurrSpeed*FPSfactor
								
								AnimateNPC(n, 494, 569, n\CurrSpeed*25)
								
								If n\State2 = 0 Then
									If dist<8.0 Then
										If EntityInView(n\Collider,Camera) Then
											PlaySound_Strict LoadTempSound("SFX\SCP\860\Chase"+Rand(1,2)+".ogg")
											
											PlaySound2(LoadTempSound("SFX\SCP\860\Cancer"+Rand(0,2)+".ogg"), Camera, n\Collider)	
											n\State2 = 1
										EndIf										
									EndIf
								EndIf
								
								If CurrSpeed > 0.03 Then ;the player is running
									n\State3 = n\State3 + FPSfactor
									If Rnd(5000)<n\State3 Then
										temp = True
										If n\SoundChn <> 0 Then
											If ChannelPlaying (n\SoundChn) Then temp = False
										EndIf
										If temp Then
											n\SoundChn = PlaySound2(LoadTempSound("SFX\SCP\860\Cancer"+Rand(0,2)+".ogg"), Camera, n\Collider)
										EndIf
									EndIf
								Else
									n\State3 = Max(n\State3 - FPSfactor,0)
								EndIf
								
								If dist<4.5 Or n\State3 > Rnd(200,250) Then
									n\SoundChn = PlaySound2(LoadTempSound("SFX\SCP\860\Cancer"+Rand(3,5)+".ogg"), Camera, n\Collider)
									n\State = 3
								EndIf
								
								If dist > 20.0 Then
									n\State = 0
									n\State2 = 0
									PositionEntity n\Collider, 0,-110,0
								EndIf
							EndIf
							
							;535, 568
							If (prevFrame < 533 And n\Frame=>533) Or (prevFrame > 568 And n\Frame<2) Then
								PlaySound2(Step2SFX(Rand(3,5)), Camera, n\Collider, 15.0, 0.6)
							EndIf
							
						Case 3 ;runs towards the player and attacks
							ShowEntity n\obj
							ShowEntity n\Collider
							
							prevFrame = n\Frame
							
							angle = CurveAngle(Find860Angle(n, fr),EntityYaw(n\Collider)+90,40.0)
							
							RotateEntity n\Collider, 0, angle-90, 0, True
							
							If n\Sound = 0 Then n\Sound = LoadSound_Strict("SFX\General\Slash1.ogg")
							If n\Sound2 = 0 Then n\Sound2 = LoadSound_Strict("SFX\General\Slash2.ogg")
							
							;if close enough to attack OR already attacking, play the attack anim
							If (dist<1.1 Or (n\Frame>451 And n\Frame<493) Or KillTimer < 0) Then
								DeathMSG = ""
								
								n\CurrSpeed = CurveValue(0.0, n\CurrSpeed, 5.0)
								
								AnimateNPC(n, 451,493, 0.5, False)
								
								;Animate2(n\obj, AnimTime(n\obj), 451,493, 0.5, False)
								If (prevFrame < 461 And n\Frame=>461) Then 
									If KillTimer => 0 Then Kill() : KillAnim = 0
									PlaySound_Strict(n\Sound)
								EndIf
								If (prevFrame < 476 And n\Frame=>476) Then PlaySound_Strict(n\Sound2)
								If (prevFrame < 486 And n\Frame=>486) Then PlaySound_Strict(n\Sound2)
							Else
								n\CurrSpeed = CurveValue(n\Speed*0.8, n\CurrSpeed, 10.0)
								
								AnimateNPC(n, 298, 316, n\CurrSpeed*10)
								;Animate2(n\obj, AnimTime(n\obj), 298, 316, n\CurrSpeed*10)
								
								If (prevFrame < 307 And n\Frame=>307) Then
									PlaySound2(Step2SFX(Rand(3,5)), Camera, n\Collider, 10.0)
								EndIf
							EndIf
							
							MoveEntity n\Collider, 0,0,n\CurrSpeed*FPSfactor
					End Select
					
					If n\State <> 0 Then
						RotateEntity n\Collider, 0, EntityYaw(n\Collider), 0, True	
						
						PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider)-0.1, EntityZ(n\Collider))
						RotateEntity n\obj, EntityPitch(n\Collider)-90, EntityYaw(n\Collider), EntityRoll(n\Collider), True
						
						If dist > 8.0 Then
							ShowEntity n\obj2
							EntityAlpha n\obj2, Min(dist-8.0,1.0)
							
							PositionEntity(n\obj2, EntityX(n\obj), EntityY(n\obj) , EntityZ(n\obj))
							RotateEntity(n\obj2, 0, EntityYaw(n\Collider) - 180, 0)
							MoveEntity(n\obj2, 0, 30.0*0.025, -33.0*0.025)
							
							;render distance is set to 8.5 inside the forest,
							;so we need to cheat a bit to make the eyes visible if they're further than that
							pvt = CreatePivot()
							PositionEntity pvt, EntityX(Camera),EntityY(Camera),EntityZ(Camera)
							PointEntity pvt, n\obj2
							MoveEntity pvt, 0,0,8.0
							PositionEntity n\obj2, EntityX(pvt),EntityY(pvt),EntityZ(pvt)
							FreeEntity pvt
						Else
							HideEntity n\obj2
						EndIf
					EndIf
				EndIf
				;[End Block] 
			Case NPCtype939
				;[Block]
				
				If PlayerRoom\RoomTemplate\Name <> "room3storage"
					n\State = 66
				EndIf
				
				;state is set to 66 in the room3storage-event if player isn't inside the room
				If n\State < 66 Then 
					Select n\State
						Case 0
							AnimateNPC(n, 290,405,0.1)
							
							;Animate2(n\obj,AnimTime(n\obj),290,405,0.1)
						Case 1
							
							If n\Frame=>644 And n\Frame<683 Then ;finish the walking animation
								;n\CurrSpeed = CurveValue(n\Speed*0.2, n\CurrSpeed, 10.0)
								n\CurrSpeed = CurveValue(n\Speed*0.05, n\CurrSpeed, 10.0)
								AnimateNPC(n, 644,683,28*n\CurrSpeed*4,False)
								If n\Frame=>682 Then n\Frame =175
								
								;Animate2(n\obj,AnimTime(n\obj),644,683,28*n\CurrSpeed,False)
								;If AnimTime(n\obj)=683 Then SetAnimTime(n\obj,175)
							Else
								n\CurrSpeed = CurveValue(0, n\CurrSpeed, 5.0)
								AnimateNPC(n, 175,297,0.22,False)
								If n\Frame=>296 Then n\State = 2
								
								;Animate2(n\obj,AnimTime(n\obj),175,297,0.22,False)
								;If AnimTime(n\obj)=297 Then n\State = 2
							EndIf
							
							n\LastSeen = 0
							
							MoveEntity n\Collider, 0,0,n\CurrSpeed*FPSfactor						
							
						Case 2
							n\State2 = Max(n\State2, (n\PrevState-3))
							
							dist = EntityDistance(n\Collider, PlayerRoom\Objects[n\State2])
							
							n\CurrSpeed = CurveValue(n\Speed*0.3*Min(dist,1.0), n\CurrSpeed, 10.0)
							MoveEntity n\Collider, 0,0,n\CurrSpeed*FPSfactor 
							
							prevFrame = n\Frame
							AnimateNPC(n, 644,683,28*n\CurrSpeed) ;walk
							
							;prevFrame = AnimTime(n\obj)
							;Animate2(n\obj,AnimTime(n\obj),644,683,28*n\CurrSpeed) ;walk
							
							If (prevFrame<664 And n\Frame=>664) Or (prevFrame>673 And n\Frame<654) Then
								PlaySound2(StepSFX(4, 0, Rand(0,3)), Camera, n\Collider, 12.0)
								If Rand(10)=1 Then
									temp = False
									If n\SoundChn = 0 Then 
										temp = True
									ElseIf Not ChannelPlaying(n\SoundChn)
										temp = True
									EndIf
									If temp Then
										If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
										n\Sound = LoadSound_Strict("SFX\SCP\939\"+(n\ID Mod 3)+"Lure"+Rand(1,10)+".ogg")
										n\SoundChn = PlaySound2(n\Sound, Camera, n\Collider)
									EndIf
								EndIf
							EndIf
							
							PointEntity n\obj, PlayerRoom\Objects[n\State2]
							RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),20.0), 0
							
							If dist<0.4 Then
								n\State2 = n\State2 + 1
								If n\State2 > n\PrevState Then n\State2 = (n\PrevState-3)
								n\State = 1
							EndIf
							
						Case 3
							If EntityVisible(Collider, n\Collider) Then
								If n\Sound2 = 0 Then n\Sound2 = LoadSound_Strict("SFX\General\Slash1.ogg")
								
								n\EnemyX = EntityX(Collider)
								n\EnemyZ = EntityZ(Collider)
								n\LastSeen = 10*7
							EndIf
							
							If n\LastSeen > 0 And (Not NoTarget) Then
								prevFrame = n\Frame
								
								If (n\Frame=>18.0 And n\Frame<68.0) Then
									n\CurrSpeed = CurveValue(0, n\CurrSpeed, 5.0)
									AnimateNPC(n, 18,68,0.5,True)
									;Animate2(n\obj,AnimTime(n\obj),18,68,0.5,True)
									
									;hasn't hit
									temp = False
									
									If prevFrame < 24 And n\Frame>=24 Then
										temp = True
									ElseIf prevFrame < 57 And n\Frame>=57
										temp = True
									EndIf
									
									If temp Then
										If Distance(n\EnemyX, n\EnemyZ, EntityX(n\Collider), EntityZ(n\Collider))<1.5 Then
											PlaySound_Strict n\Sound2
											Injuries = Injuries + Rnd(1.5, 2.5)-WearingVest*0.5
											BlurTimer = 500		
										Else
											n\Frame	 = 449
											;SetAnimTime(n\obj, 449)
										EndIf
									EndIf
									
									If Injuries>4.0 Then 
										DeathMSG=I_Loc\DeathMessage_939
										Kill()
										If (Not GodMode) Then n\State = 5
									EndIf								
								Else
									If n\LastSeen = 10*7 Then 
										n\CurrSpeed = CurveValue(n\Speed, n\CurrSpeed, 20.0)
										
										AnimateNPC(n, 449,464,6*n\CurrSpeed) ;run
										;Animate2(n\obj,AnimTime(n\obj),449,464,6*n\CurrSpeed) ;run
										
										If (prevFrame<452 And n\Frame=>452) Or (prevFrame<459 And n\Frame=>459) Then
											PlaySound2(StepSFX(1, 1, Rand(0,7)), Camera, n\Collider, 12.0)
										EndIf										
										
										If Distance(n\EnemyX, n\EnemyZ, EntityX(n\Collider), EntityZ(n\Collider))<1.1 Then ;player is visible
											n\Frame = 18
											;SetAnimTime(n\obj, 18.0)
										EndIf
									Else
										n\CurrSpeed = CurveValue(0, n\CurrSpeed, 5.0)
										AnimateNPC(n, 175,297,5*n\CurrSpeed,True)	
										;Animate2(n\obj,AnimTime(n\obj),175,297,5*n\CurrSpeed,True)
									EndIf
									
								EndIf
								
								angle = VectorYaw(n\EnemyX-EntityX(n\Collider), 0.0, n\EnemyZ-EntityZ(n\Collider))
								RotateEntity n\Collider, 0, CurveAngle(angle,EntityYaw(n\Collider),15.0), 0									
								
								MoveEntity n\Collider, 0,0,n\CurrSpeed*FPSfactor							
								
								n\LastSeen = n\LastSeen - FPSfactor
							Else
								n\State = 2
							EndIf
							
						;Animate2(n\obj,AnimTime(n\obj),406,437,0.1) ;leap
						Case 5
							If n\Frame<68 Then
								AnimateNPC(n, 18,68,0.5,False) ;finish the attack animation
								
								;Animate2(n\obj,AnimTime(n\obj),18,68,0.5,False) ;finish the attack animation
							Else
								AnimateNPC(n, 464,473,0.5,False) ;attack to idle
								
								;Animate2(n\obj,AnimTime(n\obj),464,473,0.5,False) ;attack to idle
							EndIf
							
					End Select
					
					If n\State < 3 And (Not NoTarget) And (Not n\IgnorePlayer) Then
						dist = EntityDistance(n\Collider, Collider)
						
						If dist < 4.0 Then dist = dist - EntityVisible(Collider, n\Collider)
						If PlayerSoundVolume*1.2>dist Or dist < 1.5 Then
							If n\State3 = 0 Then
								If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
								n\Sound = LoadSound_Strict("SFX\SCP\939\"+(n\ID Mod 3)+"Attack"+Rand(1,3)+".ogg")
								n\SoundChn = PlaySound2(n\Sound, Camera, n\Collider)										
								
								PlaySound_Strict(LoadTempSound("SFX\SCP\939\attack.ogg"))
								n\State3 = 1
							EndIf
							
							n\State = 3
						ElseIf PlayerSoundVolume*1.6>dist
							If n\State<>1 And n\Reload <= 0 Then
								If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
								n\Sound = LoadSound_Strict("SFX\SCP\939\"+(n\ID Mod 3)+"Alert"+Rand(1,3)+".ogg")
								n\SoundChn = PlaySound2(n\Sound, Camera, n\Collider)	
								
								n\Frame = 175
								n\Reload = 70 * 3
								;SetAnimTime(n\obj, 175)	
							EndIf
							
							n\State = 1
							
						EndIf
						
						n\Reload = n\Reload - FPSfactor
						
					EndIf				
					
					RotateEntity n\Collider, 0, EntityYaw(n\Collider), 0, True	
					
					PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider)-0.28, EntityZ(n\Collider))
					RotateEntity n\obj, EntityPitch(n\Collider)-90, EntityYaw(n\Collider), EntityRoll(n\Collider), True					
				EndIf
				;[End Block]
			Case NPCtype066
				;[Block]
				dist = Distance(EntityX(Collider),EntityZ(Collider),EntityX(n\Collider),EntityZ(n\Collider))
				
				Select n\State
					Case 0 
						;idle: moves around randomly from waypoint to another if the player is far enough
						;starts staring at the player when the player is close enough
						
						If dist > 20.0 Then
							AnimateNPC(n, 451, 612, 0.2, True)
							;Animate2(n\obj, AnimTime(n\obj), 451, 612, 0.2, True)
							
							If n\State2 < MilliSecs() Then
								For w.waypoints = Each WayPoints
									If w\door = Null Then
										If Abs(EntityX(w\obj,True)-EntityX(n\Collider))<4.0 Then
											If Abs(EntityZ(w\obj,True)-EntityZ(n\Collider))<4.0 Then
												PositionEntity n\Collider, EntityX(w\obj,True), EntityY(w\obj,True)+0.3, EntityZ(w\obj,True)
												ResetEntity n\Collider
												Exit
											EndIf
										EndIf
									EndIf
								Next
								n\State2 = MilliSecs()+5000
							EndIf
						ElseIf dist < 8.0
							n\LastDist = Rnd(1.0, 2.5)
							n\State = 1
						EndIf
					Case 1 ;staring at the player
						
						If n\Frame<451 Then
							angle = WrapAngle(CurveAngle(DeltaYaw(n\Collider, Collider)-180, (AnimTime(n\obj)-2.0)/1.2445, 15.0))
							;0->360 = 2->450
							SetNPCFrame(n,angle*1.2445+2.0)
							
							;SetAnimTime(n\obj, angle*1.2445+2.0)							
						Else
							AnimateNPC(n, 636, 646, 0.4, False)
							If n\Frame = 646 Then SetNPCFrame(n,2)
							;Animate2(n\obj, AnimTime(n\obj), 636, 646, 0.4, False)
							;If AnimTime(n\obj)=646 Then SetAnimTime (n\obj, 2)
						EndIf
						dist = Distance(EntityX(Collider),EntityZ(Collider),EntityX(n\Collider),EntityZ(n\Collider))
						
						If Rand(700)=1 Then PlaySound2(LoadTempSound("SFX\SCP\066\Eric"+Rand(1,3)+".ogg"),Camera, n\Collider, 8.0)
						
						If dist < 1.0+n\LastDist Then
							GiveAchievement(Achv066)
							n\State = Rand(2,3)
						EndIf
					Case 2 ;roll towards the player and make a sound, and then escape	
						If n\Frame < 647 Then 
							angle = CurveAngle(0, (AnimTime(n\obj)-2.0)/1.2445, 5.0)
							
							If angle < 5 Or angle > 355 Then 
								SetNPCFrame(n,647)
							Else
								SetNPCFrame(n,angle*1.2445+2.0)
							EndIf
							;SetAnimTime(n\obj, angle*1.2445+2.0)
							;If angle < 5 Or angle > 355 Then SetAnimTime(n\obj, 647)
						Else
							If n\Frame=683 Then 
								If n\State2 = 0 Then
									If Rand(2)=1 Then
										PlaySound2(LoadTempSound("SFX\SCP\066\Eric"+Rand(1,3)+".ogg"),Camera, n\Collider, 8.0)
									Else
										PlaySound2(LoadTempSound("SFX\SCP\066\Notes"+Rand(1,6)+".ogg"), Camera, n\Collider, 8.0)
									EndIf									
									
									Select Rand(1,6)
										Case 1
											If n\Sound2=0 Then n\Sound2=LoadSound_Strict("SFX\SCP\066\Beethoven.ogg")
											n\SoundChn2 = PlaySound2(n\Sound2, Camera, n\Collider)
											DeafTimer# = 70*(45+(15*SelectedDifficulty\aggressiveNPCs))
											DeafPlayer = True
											CameraShake = 10.0
										Case 2
											n\State3 = Rand(700,1400)
										Case 3
											For d.Doors = Each Doors
												If d\locked = False And d\KeyCard = 0 And d\Code = "" Then
													If Abs(EntityX(d\frameobj)-EntityX(n\Collider))<16.0 Then
														If Abs(EntityZ(d\frameobj)-EntityZ(n\Collider))<16.0 Then
															UseDoor(d, False)
														EndIf
													EndIf
												EndIf
											Next
										Case 4
											If PlayerRoom\RoomTemplate\DisableDecals = False Then
												CameraShake = 5.0
												de.Decals = CreateDecal(1, EntityX(n\Collider), 0.01, EntityZ(n\Collider), 90, Rand(360), 0)
												de\Size = 0.3 : UpdateDecals
												PlaySound_Strict(LoadTempSound("SFX\General\BodyFall.ogg"))
												If Distance(EntityX(Collider),EntityZ(Collider),EntityX(n\Collider),EntityZ(n\Collider))<0.8 Then
													Injuries = Injuries + Rnd(0.3,0.5)
												EndIf
											EndIf
									End Select
								EndIf
								
								n\State2 = n\State2+FPSfactor
								If n\State2>70 Then 
									n\State = 3
									n\State2 = 0
								EndIf
							Else
								n\CurrSpeed = CurveValue(n\Speed*1.5, n\CurrSpeed, 10.0)
								PointEntity n\obj, Collider
								;angle = CurveAngle(EntityYaw(n\obj), EntityYaw(n\Collider), 10);1.0/Max(n\CurrSpeed,0.0001))
								RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj)-180, EntityYaw(n\Collider), 10), 0
								
								AnimateNPC(n, 647, 683, n\CurrSpeed*25, False)
								;Animate2(n\obj, AnimTime(n\obj), 647, 683, n\CurrSpeed*25, False)
								
								MoveEntity n\Collider, 0,0,-n\CurrSpeed*FPSfactor
								
							EndIf
						EndIf
					Case 3
						PointEntity n\obj, Collider
						angle = CurveAngle(EntityYaw(n\obj)+n\Angle-180, EntityYaw(n\Collider), 10);1.0/Max(n\CurrSpeed,0.0001))
						RotateEntity n\Collider, 0, angle, 0
						
						n\CurrSpeed = CurveValue(n\Speed, n\CurrSpeed, 10.0)
						MoveEntity n\Collider, 0,0,n\CurrSpeed*FPSfactor
						
						;Animate2(n\obj, AnimTime(n\obj), 684, 647, -n\CurrSpeed*25)
						
						If Rand(100)=1 Then n\Angle = Rnd(-20,20)
						
						n\State2 = n\State2 + FPSfactor
						If n\State2>250 Then 
							AnimateNPC(n, 684, 647, -n\CurrSpeed*25, False)
							;Animate2(n\obj, AnimTime(n\obj), 684, 647, -n\CurrSpeed*25, False)
							If n\Frame=647 Then 
								n\State = 0
								n\State2=0
							EndIf
						Else
							AnimateNPC(n, 684, 647, -n\CurrSpeed*25)
							
							;Animate2(n\obj, AnimTime(n\obj), 684, 647, -n\CurrSpeed*25)
						EndIf
						
				End Select
				
				If n\State > 1 Then
					If n\Sound = 0 Then n\Sound = LoadSound_Strict("SFX\SCP\066\Rolling.ogg")
					If n\SoundChn<>0 Then
						If ChannelPlaying(n\SoundChn) Then
							n\SoundChn = LoopSound2(n\Sound, n\SoundChn, Camera, n\Collider, 20)
						EndIf
					Else
						n\SoundChn = PlaySound2(n\Sound, Camera, n\Collider, 20)
					EndIf					
				EndIf
				
				;If n\SoundChn2<>0 Then
				;	If ChannelPlaying(n\SoundChn2) Then
				;		n\SoundChn2 = LoopSound2(n\Sound2, n\SoundChn2, Camera, n\Collider, 20)
				;		BlurTimer = Max((5.0-dist)*300,0)
				;	EndIf
				;EndIf
				
				
				If n\State3 > 0 Then
					n\State3 = n\State3-FPSfactor
					LightVolume = TempLightVolume-TempLightVolume*Min(Max(n\State3/500,0.01),0.6)
					HeartBeatRate = Max(HeartBeatRate, 130)
					HeartBeatVolume = Max(HeartBeatVolume,Min(n\State3/1000,1.0))
				EndIf
				
				If ChannelPlaying(n\SoundChn2)
					BlurTimer = Max((5.0-dist)*300,0)
				EndIf
				UpdateSoundOrigin(n\SoundChn2,Camera,n\Collider,20,1.0,False)
				
				PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) - 0.2, EntityZ(n\Collider))
				
				RotateEntity n\obj, EntityPitch(n\Collider)-90, EntityYaw(n\Collider), 0
				;[End Block]
			Case NPCtype966
				;[Block]
				dist = EntityDistance(n\Collider,Collider)
				
				If (dist<HideDistance) Then
					
					;n\state = the "general" state (idle/wander/attack/echo etc)
					;n\state2 = timer for doing raycasts
					
					prevFrame = n\Frame
					
					If n\Sound > 0 Then
						temp = 0.5
						;the ambient sound gets louder when the npcs are attacking
						If n\State > 0 Then temp = 1.0	
						
						n\SoundChn = LoopSound2(n\Sound, n\SoundChn, Camera, Camera, 10.0,temp)
					EndIf
					
					temp = Rnd(-1.0,1.0)
					PositionEntity n\obj,EntityX(n\Collider,True),EntityY(n\Collider,True)-0.2,EntityZ(n\Collider,True)
					RotateEntity n\obj,-90.0,EntityYaw(n\Collider),0.0
					
					If (WearingNightVision=0) Then
						HideEntity n\obj
						If dist<1 And n\Reload <= 0 Then
							Msg = I_Loc\Message_966Presence[Rand(6)]
                            n\Reload = 20*70
							MsgTimer=8*70
						EndIf
						n\Reload = n\Reload - FPSfactor
						
					Else
						ShowEntity n\obj
					EndIf
					
					If n\State3>5*70 Then
						;n\State = 1
						If n\State3<1000.0 Then
							For n2.NPCs = Each NPCs	
								If n2\NPCtype = n\NPCtype Then n2\State3=1000.0 
							Next
						EndIf
						
						n\State = Max(n\State,8)
						n\State3 = 1000.0					
						
					EndIf
					
					If Stamina<10 Then 
						n\State3=n\State3+FPSfactor
					Else If n\State3 < 900.0
						n\State3=Max(n\State3-FPSfactor*0.2,0.0)
					EndIf
					
					If n\State <> 10
						n\LastSeen = 0
					EndIf
					
					Select n\State
						Case 0 ;idle, standing
							;If n\Frame>2300.0 Then
							If n\Frame>556.0
								;AnimateNPC(n, 2391, 2416, 1.0, False)	
								;If n\Frame>2415.0 Then SetNPCFrame(n, 201)
								AnimateNPC(n, 628, 652, 0.25, False)
								If n\Frame>651.0 Then SetNPCFrame(n, 2)
							Else
								;AnimateNPC(n, 201, 1015, 1.0, False)
								AnimateNPC(n, 2, 214, 0.25, False)
								
								;echo/stare/walk around periodically
								;If n\Frame>1014.0 Then
								If n\Frame>213.0
									If Rand(3)=1 And dist<4 And (Not NoTarget) Then
										n\State = Rand(1,4)
									Else
										n\State = Rand(5,6)								
									EndIf
								EndIf
								
								;echo if player gets close
								If dist<2.0 And (Not NoTarget) Then 
									If WearingNightVision<> 0 Then GiveAchievement(Achv966)

									n\State=Rand(1,4)
								EndIf 							
							EndIf
							
							n\CurrSpeed = CurveValue(0.0, n\CurrSpeed, 10.0)
							
							MoveEntity n\Collider,0,0,n\CurrSpeed
							
						Case 1,2 ;echo
;							If n\State=1 Then
;								AnimateNPC(n, 1015, 1180, 1.0, False)
;								If n\Frame > 1179.0 Then n\State = 0
;							Else
;								AnimateNPC(n, 1180, 1379, 1.0, False)
;								If n\Frame > 1378.0 Then n\State = 0
;							EndIf
							AnimateNPC(n, 214, 257, 0.25, False)
							If n\Frame > 256.0 Then n\State = 0
							
							;If n\Frame>1029.0 And prevFrame<=1029.0 Or n\Frame>1203.0 And prevFrame<=1203.0 Then
							If n\Frame>228.0 And prevFrame<=228.0
								PlaySound2(LoadTempSound("SFX\SCP\966\Echo"+Rand(1,3)+".ogg"), Camera, n\Collider)
							EndIf
							
							angle = VectorYaw(EntityX(Collider)-EntityX(n\Collider),0,EntityZ(Collider)-EntityZ(n\Collider))
							RotateEntity n\Collider,0.0,CurveAngle(angle,EntityYaw(n\Collider),20.0),0.0
							
							If n\State3<900 Then
								BlurTimer = ((Sin(MilliSecs()/50)+1.0)*200)/dist
								
								If (Not Wearing714) And (WearingGasMask<3) And (WearingHazmat<3) And dist<16 Then
									If WearingNightVision<> 0 Then GiveAchievement(Achv966)

									If StaminaEffect<1.5 Then
										Msg = I_Loc\Message_966Sleep[Rand(4)]
										MsgTimer = 7*70
									EndIf
									
									BlinkEffect = Max(BlinkEffect, 1.5)
									BlinkEffectTimer = 1000
									
									StaminaEffect = 2.0
									StaminaEffectTimer = 1000
								EndIf							
							EndIf
							
						Case 3,4 ;stare at player
;							If n\State=3 Then
;								AnimateNPC(n, 1379.0, 1692.0, 1.0, False)
;								
;								If n\Frame>1691.0 Then n\State = 0
;							Else
;								AnimateNPC(n, 1692.0, 2156.0, 1.0, False)
;								
;								If n\Frame>2155.0 Then n\State = 0
;							EndIf
							If n\State=3
								AnimateNPC(n, 257, 332, 0.25, False)
								If n\Frame > 331.0 Then n\State = 0
							Else
								AnimateNPC(n, 332, 457, 0.25, False)
								If n\Frame > 456.0 Then n\State = 0
							EndIf
							
							;If n\Frame>1393.0 And prevFrame<=1393.0 Or n\Frame>1589.0 And prevFrame<=1589.0 Or n\Frame>2000.0 And prevFrame<=2000.0 Then
							If (n\Frame>271.0 And prevFrame<=271.0) Or (n\Frame>314.0 And prevFrame<=314.0) Or (n\Frame>301.0 And prevFrame<=301.0)
								PlaySound2(LoadTempSound("SFX\SCP\966\Idle"+Rand(1,3)+".ogg"), Camera, n\Collider)
							EndIf
							
							angle = VectorYaw(EntityX(Collider)-EntityX(n\Collider),0,EntityZ(Collider)-EntityZ(n\Collider))
							RotateEntity n\Collider,0.0,CurveAngle(angle,EntityYaw(n\Collider),20.0),0.0
						Case 5,6,8 ;walking or chasing
							;If n\Frame<2343.0 Then
							If n\Frame>213.0 And n\Frame<580.0 ;start walking
								;AnimateNPC(n, 2319, 2343, 0.5, False)
								AnimateNPC(n, 556, 580, 0.25, False)
							Else
								;AnimateNPC(n, 2343, 2391, n\CurrSpeed*25.0)
								AnimateNPC(n, 580, 628, n\CurrSpeed*25.0)
								
								;chasing the player
								If n\State = 8 And dist<32 And (Not NoTarget) Then
									If n\PathTimer <= 0 Then
										n\PathStatus = FindPath (n, EntityX(Collider,True), EntityY(Collider,True), EntityZ(Collider,True))
										n\PathTimer = 40*10
										n\CurrSpeed = 0
									EndIf
									n\PathTimer = Max(n\PathTimer-FPSfactor,0)
									
									If (Not EntityVisible(n\Collider,Collider)) Then
										If n\PathStatus = 2 Then
											n\CurrSpeed = 0
											;SetNPCFrame(n,201)
											SetNPCFrame(n,2)
										ElseIf n\PathStatus = 1
											If n\Path[n\PathLocation]=Null Then 
												If n\PathLocation > 19 Then 
													n\PathLocation = 0 : n\PathStatus = 0
												Else
													n\PathLocation = n\PathLocation + 1
												EndIf
											Else
												n\Angle = VectorYaw(EntityX(n\Path[n\PathLocation]\obj,True)-EntityX(n\Collider),0,EntityZ(n\Path[n\PathLocation]\obj,True)-EntityZ(n\Collider))
												;RotateEntity n\Collider,0.0,CurveAngle(angle,EntityYaw(n\Collider),10.0),0.0
												
												dist2 = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
												
												temp = True
												If dist2 < 0.8 Then 
													If n\Path[n\PathLocation]\door<>Null Then
														If (Not n\Path[n\PathLocation]\door\IsElevatorDoor)
															If (n\Path[n\PathLocation]\door\locked Or n\Path[n\PathLocation]\door\KeyCard<>0 Or n\Path[n\PathLocation]\door\Code<>"") And (Not n\Path[n\PathLocation]\door\open) Then
																temp = False
															Else
																d.Doors = n\Path[n\PathLocation]\door
																If d\open = False And (d\buttons[0]<>0 Lor d\buttons[1]<>0) And (d\openstate <= 0 Lor d\openstate >= 180) Then
																	UseDoor(n\Path[n\PathLocation]\door, False)
																EndIf
															EndIf
														EndIf
													EndIf
													If dist2 < 0.3 Then n\PathLocation = n\PathLocation + 1
												EndIf
												
												If temp = False Then
													n\PathStatus = 0
													n\PathLocation = 0
													n\PathTimer = 40*10
												EndIf
											EndIf
											
											n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,10.0)
										ElseIf n\PathStatus = 0
											n\CurrSpeed = CurveValue(0,n\CurrSpeed,10.0)
										EndIf
									Else
										If WearingNightVision<> 0 Then GiveAchievement(Achv966)

										n\Angle = VectorYaw(EntityX(Collider)-EntityX(n\Collider),0,EntityZ(Collider)-EntityZ(n\Collider))
										n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,10.0)
										
										If dist<1.0 Then n\State=10
										
									EndIf
								Else
									If MilliSecs() > n\State2 And dist<16.0 Then
										HideEntity n\Collider
										EntityPick(n\Collider, 1.5)
										If PickedEntity() <> 0 And (Not NoTarget) Then
											If WearingNightVision<> 0 Then GiveAchievement(Achv966)
											n\Angle = EntityYaw(n\Collider)+Rnd(80,110)
										EndIf
										ShowEntity n\Collider
										
										n\State2=MilliSecs()+1000
										
										If Rand(5)=1 Then n\State=0
									EndIf	
									
									n\CurrSpeed = CurveValue(n\Speed*0.5, n\CurrSpeed, 20.0)
									
								EndIf
								
								If (prevFrame < 604 And n\Frame=>604) Or (prevFrame < 627 And n\Frame=>627) Then
                                    PlaySound2(StepSFX(4,0,Rand(0,3)),Camera, n\Collider, 7.0, Rnd(0.5,0.7))
                                EndIf
								
								RotateEntity n\Collider, 0, CurveAngle(n\Angle,EntityYaw(n\Collider),10.0),0
								
								MoveEntity n\Collider,0,0,n\CurrSpeed*FPSfactor
							EndIf
						Case 10 ;attack
							If n\LastSeen=0
								PlaySound2(LoadTempSound("SFX\SCP\966\Echo"+Rand(1,3)+".ogg"), Camera, n\Collider)
								n\LastSeen = 1
							EndIf
							
							;If n\Frame>2300.0 Then
							If n\Frame>556.0
								;AnimateNPC(n, 2391, 2416, 1.0, False
								AnimateNPC(n, 628, 652, 0.25, False)
								;If n\Frame>2415.0 Then
								If n\Frame>651.0
									Select Rand(3)
										Case 1
											;SetNPCFrame(n, 2160)
											SetNPCFrame(n, 458)
										Case 2
											;SetNPCFrame(n, 2192)
											SetNPCFrame(n, 488)
										Case 3
											;SetNPCFrame(n, 2221)
											SetNPCFrame(n, 518)
									End Select
									
								EndIf
							Else
;								If n\Frame <= 2191 Then
;									AnimateNPC(n, 2160, 2191, 0.3, False)
;									If n\Frame > 2190 Then n\State = 8
;								ElseIf n\Frame <= 2220
;									AnimateNPC(n, 2192, 2220, 0.3, False)
;									If n\Frame > 2219 Then n\State = 8
;								ElseIf n\Frame <= 2260
;									AnimateNPC(n, 2221, 2260, 0.3, False)
;									If n\Frame > 2259 Then n\State = 8
;								EndIf
								
								If n\Frame <= 487
									AnimateNPC(n, 458, 487, 0.3, False)
									If n\Frame > 486.0 Then n\State = 8
								ElseIf n\Frame <= 517
									AnimateNPC(n, 488, 517, 0.3, False)
									If n\Frame > 516.0 Then n\State = 8
								ElseIf n\Frame <= 557
									AnimateNPC(n, 518, 557, 0.3, False)
									If n\Frame > 556.0 Then n\State = 8
								EndIf
							EndIf
							
							If dist<1.0 Then
								;If n\Frame>2173.0 And prevFrame<=2173.0 Or n\Frame>2203.0 And prevFrame<=2203.0 Or n\Frame>2227.0 And prevFrame<=2227.0 Then
								If (n\Frame>470.0 And prevFrame<=470.0) Or (n\Frame>500.0 And prevFrame<=500.0) Or (n\Frame>527.0 And prevFrame<=527.0)
									PlaySound2(LoadTempSound("SFX\General\Slash"+Rand(1,2)+".ogg"), Camera, n\Collider)
									Injuries = Injuries + Rnd(0.5,1.0)								
								EndIf	
							EndIf
							
							n\Angle = VectorYaw(EntityX(Collider)-EntityX(n\Collider),0,EntityZ(Collider)-EntityZ(n\Collider))
							RotateEntity n\Collider, 0, CurveAngle(n\Angle,EntityYaw(n\Collider),30.0),0
							
					End Select
				Else
					HideEntity n\obj
					If (Rand(600)=1) Then
						TeleportCloser(n)
					EndIf
				EndIf
				
				;[End Block]
			Case NPCtype1048a
				;[Block]

				PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) - 0.1, EntityZ(n\Collider))

				If n\State3 > 0 And EntityDistance(Collider, n\obj) >= 4 Then
					n\State3 = n\State3 + FPSfactor

					If n\State3>70*25 And (n\SoundChn = 0 Lor (Not ChannelPlaying(n\SoundChn))) Then
						RemoveNPC(n)
						Return
					EndIf
				EndIf

				Select n\State
					Case 0
						AnimateNPC(n, 2.0, 395.0, 1.0)
							
						If (Not NoTarget) And EntityDistance(Collider, n\obj)<2.5 Then n\State = 1
					
					Case 1
						prevFrame# = n\Frame
						AnimateNPC(n, 2.0, 647.0, 1.0, False)
						
						If n\SoundChn = 0 And n\Frame>400.0 Then n\SoundChn = PlaySound_Strict(n\Sound)
						
						Local volume# = Max(1.0 - Abs(prevFrame - 600.0)/100.0, 0.0)
						
						BlurTimer = volume*1000.0
						CameraShake = volume*10.0
						
						PointEntity(n\obj, Collider)
						RotateEntity(n\obj, -90.0, EntityYaw(n\obj), 0.0)
						
						If (prevFrame>646.0) Then
							If (EntityDistance(Collider, n\obj) < 4) Then
								n\State = 2
								PlaySound_Strict n\Sound2
								
								Msg = I_Loc\Message_1048a1
								MsgTimer = 70.0 * 3.0
							Else
								n\State3 = 70*30
							EndIf
						EndIf

					Case 2
						n\State2 = n\State2 + FPSfactor
						
						CanSave = False

						BlurTimer = n\State2*2.0
						
						If (n\State2>250.0 And n\State2-FPSfactor <= 250.0) Then
							Msg = I_Loc\Message_1048a2[Rand(3)]
							MsgTimer = 70.0 * 3.0
						Else If (n\State2>600.0 And n\State2-FPSfactor <= 600.0)
							Msg = I_Loc\Message_1048a3[Rand(4)]
							MsgTimer = 70.0 * 5.0
						EndIf
						
						If (n\State2>70*15) Then
							DeathMSG = I_Loc\DeathMessage_1048a
							Kill()
							n\State = 3
						EndIf

				End Select
				;[End block]
			Case NPCtype1499
				;[Block]
				;n\State: Current State of the NPC
				;n\State2: A second state variable (dependend on the current NPC's n\State)
				;n\State3: Determines if the NPC will always be aggressive against the player
				;n\PrevState: Determines the type/behaviour of the NPC
				;	0 = Normal / Citizen
				;	1 = Stair guard / Guard next to king
				;	2 = King
				;	3 = Front guard
				
				prevFrame# = n\Frame
				
				If (Not n\Idle) And EntityDistance(n\Collider,Collider)<HideDistance*3 Then
					If n\PrevState = 0 Then
						If n\State = 0 Or n\State = 2 Then
							For n2.NPCs = Each NPCs
								If n2\NPCtype = n\NPCtype And n2 <> n Then
									If n2\State <> 0 And n2\State <> 2 Then
										If n2\PrevState = 0 Then
											n\State = 1
											n\State2 = 0
											Exit
										EndIf
									EndIf
								EndIf
							Next
						EndIf
					EndIf
					
					Select n\State
						Case 0
							;[Block]
							If n\PrevState=0 Then
								If n\CurrSpeed = 0.0 Then
									If n\Reload=0 Then
										If n\State2 < 500.0*Rnd(1,3) Then
											n\CurrSpeed = 0.0
											n\State2 = n\State2 + FPSfactor
										Else
											If n\CurrSpeed = 0.0 Then n\CurrSpeed = n\CurrSpeed + 0.0001
										EndIf
									Else
										If n\State2 < 1500 Then
											n\CurrSpeed = 0.0
											n\State2 = n\State2 + FPSfactor
										Else
											If n\Target <> Null Then
												If n\Target\Target <> Null Then
													n\Target\Target = Null
												EndIf
												n\Target\Reload = 0
												n\Target\Angle = n\Target\Angle+Rnd(-45,45)
												n\Target = Null
												n\Reload = 0
												n\Angle = n\Angle+Rnd(-45,45)
											EndIf
											If n\CurrSpeed = 0.0 Then n\CurrSpeed = n\CurrSpeed + 0.0001
										EndIf
									EndIf
								Else
									If n\Target<>Null Then
										n\State2 = 0.0
									EndIf
									
									If n\State2 < 10000.0*Rnd(1,3)
										n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,10.0)
										n\State2 = n\State2 + FPSfactor
									Else
										n\CurrSpeed = CurveValue(0.0,n\CurrSpeed,50.0)
									EndIf
									
									RotateEntity n\Collider,0,CurveAngle(n\Angle,EntityYaw(n\Collider),10.0),0
									
									If n\Target=Null Then
										If Rand(200) = 1 Then n\Angle = n\Angle + Rnd(-45,45)
										
										HideEntity n\Collider
										EntityPick(n\Collider, 1.5)
										If PickedEntity() <> 0 Then
											n\Angle = EntityYaw(n\Collider)+Rnd(80,110)
										EndIf
										ShowEntity n\Collider
									Else
										n\Angle = EntityYaw(n\Collider) + DeltaYaw(n\Collider,n\Target\Collider)
										If EntityDistance(n\Collider,n\Target\Collider)<1.5 Then
											n\CurrSpeed = 0.0
											n\Target\CurrSpeed = 0.0
											n\Reload = 1
											n\Target\Reload = 1
											temp = Rand(0,2)
											If temp=0 Then
												SetNPCFrame(n,296)
											ElseIf temp=1 Then
												SetNPCFrame(n,856)
											Else
												SetNPCFrame(n,905)
											EndIf
											temp = Rand(0,2)
											If temp=0 Then
												SetNPCFrame(n\Target,296)
											ElseIf temp=1 Then
												SetNPCFrame(n\Target,856)
											Else
												SetNPCFrame(n\Target,905)
											EndIf
										EndIf
									EndIf
								EndIf
							Else
								RotateEntity n\Collider,0,CurveAngle(n\Angle,EntityYaw(n\Collider),10.0),0
							EndIf
							
							If n\CurrSpeed = 0.0
								If n\Reload = 0 And n\PrevState<>2 Then
									AnimateNPC(n,296,320,0.2)
								ElseIf n\Reload = 0 And n\PrevState=2 Then
									;509-533
									;534-601
									If n\Frame <= 532.5 Then
										AnimateNPC(n,509,533,0.2,False)
									ElseIf n\Frame > 533.5 And n\Frame <= 600.5 Then
										AnimateNPC(n,534,601,0.2,False)
									Else
										temp = Rand(0,1)
										If temp=0 Then
											SetNPCFrame(n,509)
										Else
											SetNPCFrame(n,534)
										EndIf
									EndIf
									;SetNPCFrame(n,855)
								Else
									If n\PrevState=2 Then
										AnimateNPC(n,713,855,0.2,False)
										If n\Frame > 833.5 Then
											PointEntity n\obj,Collider
											RotateEntity n\Collider,0,CurveAngle(n\Angle,EntityYaw(n\Collider),10.0),0
										EndIf
									ElseIf n\PrevState=1 Then
										AnimateNPC(n,602,712,0.2,False)
										If n\Frame > 711.5 Then
											n\Reload = 0
										EndIf
									Else
										If n\Frame <= 319.5 Then
											AnimateNPC(n,296,320,0.2,False)
										;856-904
										ElseIf n\Frame > 320.5 And  n\Frame < 903.5 Then
											AnimateNPC(n,856,904,0.2,False)
										;905-953
										ElseIf n\Frame > 904.5 And n\Frame < 952.5 Then
											AnimateNPC(n,905,953,0.2,False)
										Else
											temp = Rand(0,2)
											If temp=0 Then
												SetNPCFrame(n,296)
											ElseIf temp=1 Then
												SetNPCFrame(n,856)
											Else
												SetNPCFrame(n,905)
											EndIf
										EndIf
									EndIf
								EndIf
							Else
								If (n\ID Mod 2 = 0) Then
									AnimateNPC(n,1,62,(n\CurrSpeed*28))
								Else
									AnimateNPC(n,100,167,(n\CurrSpeed*28))
								EndIf
								If n\PrevState = 0 Then
									If n\Target = Null Then
										If Rand(1,1200)=1 Then
											For n2.NPCs = Each NPCs
												If n2<>n Then
													If n2\NPCtype=n\NPCtype Then
														If n2\Target = Null Then
															If n2\PrevState=0 Then
																If EntityDistance(n\Collider,n2\Collider)<20.0 Then
																	n\Target = n2
																	n2\Target = n
																	n\Target\CurrSpeed = n\Target\CurrSpeed + 0.0001
																	Exit
																EndIf
															EndIf
														EndIf
													EndIf
												EndIf
											Next
										EndIf
									EndIf
								EndIf
							EndIf
							
							;randomly play the "screaming animation" and revert back to state 0
							If n\Target=Null And n\PrevState=0 Then
								If (Rand(5000)=1) Then
									n\State = 2
									n\State2 = 0
									
									If Not ChannelPlaying(n\SoundChn) Then
										dist = EntityDistance(n\Collider,Collider)
										If (dist < 20.0) Then
											If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
											n\Sound = LoadSound_Strict("SFX\SCP\1499\Idle"+Rand(1,4)+".ogg")
											n\SoundChn = PlaySound2(n\Sound, Camera, n\Collider, 20.0)
										EndIf
									EndIf
								EndIf
								
								If (n\ID Mod 2 = 0) And (Not NoTarget) Then
									dist = EntityDistance(n\Collider,Collider)
									If dist < 10.0 Then
										If EntityVisible(n\Collider,Collider) Then
											;play the "screaming animation"
											n\State = 2
											If dist < 5.0 Then
												If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
												n\Sound = LoadSound_Strict("SFX\SCP\1499\Triggered.ogg")
												n\SoundChn = PlaySound2(n\Sound, Camera, n\Collider,20.0)
												
												n\State2 = 1 ;if player is too close, switch to attack after screaming
												
												For n2.NPCs = Each NPCs
													;If n2\NPCtype = n\NPCtype And n2 <> n And (n\ID Mod 2 = 0) Then
													If n2\NPCtype = n\NPCtype And n2 <> n
														If n2\PrevState = 0 Then
															n2\State = 1
															n2\State2 = 0
														EndIf
													EndIf
												Next
											Else
												n\State2 = 0 ;otherwise keep idling
											EndIf
											
											n\Frame = 203
										EndIf
									EndIf
								EndIf
							ElseIf n\PrevState=1 Then
								dist = EntityDistance(n\Collider,Collider)
								If (Not NoTarget) Then
									If dist < 4.0 Then
										If EntityVisible(n\Collider,Collider) Then
											If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
											n\Sound = LoadSound_Strict("SFX\SCP\1499\Triggered.ogg")
											n\SoundChn = PlaySound2(n\Sound, Camera, n\Collider,20.0)
											
											n\State = 1
											
											n\Frame = 203
										EndIf
									EndIf
								EndIf
							EndIf
							;[End Block]
						Case 1 ;attacking the player
							;[Block]
							If NoTarget Then n\State = 0
							
							If PlayerRoom\RoomTemplate\Name = "dimension1499" And n\PrevState=0 Then
								ShouldPlay = 19
							EndIf
							
							PointEntity n\obj,Collider
							RotateEntity n\Collider,0,CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),20.0),0
							
							dist = EntityDistance(n\Collider,Collider)
							
							If n\State2 = 0.0
								If n\PrevState=1 Then
									n\CurrSpeed = CurveValue(n\Speed*2.0,n\CurrSpeed,10.0)
									If n\Target<>Null Then
										n\Target\State = 1
									EndIf
								Else
									n\CurrSpeed = CurveValue(n\Speed*1.75,n\CurrSpeed,10.0)
								EndIf
								
								If (n\ID Mod 2 = 0) Then
									AnimateNPC(n,1,62,(n\CurrSpeed*28))
								Else
									AnimateNPC(n,100,167,(n\CurrSpeed*28))
								EndIf
							EndIf
							
							If dist < 0.75
								If (n\ID Mod 2 = 0) Or n\State3 = 1 Or n\PrevState=1 Or n\PrevState=3 Or n\PrevState=4 Then
									n\State2 = Rand(1,2)
									n\State = 3
									If n\State2 = 1
										SetNPCFrame(n,63)
									Else
										SetNPCFrame(n,168)
									EndIf
								Else
									n\State = 4
								EndIf
							EndIf
							;[End Block]
						Case 2 ;play the "screaming animation" and switch to n\state2 after it's finished
							;[Block]
							n\CurrSpeed = 0.0
							AnimateNPC(n,203,295,0.1,False)
							
							If n\Frame > 294.0 Then
								n\State = n\State2
							EndIf
							;[End Block]
						Case 3 ;slashing at the player
							;[Block]
							n\CurrSpeed = CurveValue(0.0,n\CurrSpeed,5.0)
							dist = EntityDistance(n\Collider,Collider)
							If n\State2 = 1
								AnimateNPC(n,63,100,0.6,False)
								If prevFrame < 89 And n\Frame=>89
									If dist > 0.85 Or Abs(DeltaYaw(n\Collider,Collider))>60.0
										;Miss
									Else
										Injuries = Injuries + Rnd(0.75,1.5)
										PlaySound2(LoadTempSound("SFX\General\Slash"+Rand(1,2)+".ogg"), Camera, n\Collider)
										If Injuries > 10.0
											Kill()
											If PlayerRoom\RoomTemplate\Name$ = "dimension1499"
												DeathMSG = I_Loc\DeathMessage_14991Dimension
											Else
												DeathMSG = I_Loc\DeathMessage_14991
											EndIf
										EndIf
									EndIf
								ElseIf n\Frame => 99
									n\State2 = 0.0
									n\State = 1
								EndIf
							Else
								AnimateNPC(n,168,202,0.6,False)
								If prevFrame < 189 And n\Frame=>189
									If dist > 0.85 Or Abs(DeltaYaw(n\Collider,Collider))>60.0
										;Miss
									Else
										Injuries = Injuries + Rnd(0.75,1.5)
										PlaySound2(LoadTempSound("SFX\General\Slash"+Rand(1,2)+".ogg"), Camera, n\Collider)
										If Injuries > 10.0
											Kill()
											If PlayerRoom\RoomTemplate\Name$ = "dimension1499"
												DeathMSG = I_Loc\DeathMessage_14991Dimension
											Else
												DeathMSG = I_Loc\DeathMessage_14991
											EndIf
										EndIf
									EndIf
								ElseIf n\Frame => 201
									n\State2 = 0.0
									n\State = 1
								EndIf
							EndIf
							;[End Block]
						Case 4 ;standing in front of the player
							;[Block]
							dist = EntityDistance(n\Collider,Collider)
							n\CurrSpeed = CurveValue(0.0,n\CurrSpeed,5.0)
							AnimateNPC(n,296,320,0.2)
							
							PointEntity n\obj,Collider
							RotateEntity n\Collider,0,CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),20.0),0
							
							If dist > 0.85
								n\State = 1
							EndIf
							;[End Block]
					End Select
					
					UpdateSoundOrigin(n\SoundChn,Camera,n\Collider,20.0)
					
					MoveEntity n\Collider,0,0,n\CurrSpeed*FPSfactor
					
					RotateEntity n\obj,0,EntityYaw(n\Collider)-180,0
					PositionEntity n\obj,EntityX(n\Collider),EntityY(n\Collider)-0.2,EntityZ(n\Collider)
					
					ShowEntity n\obj
				Else
					HideEntity n\obj
				EndIf
				
				;[End Block]
			Case NPCtype008
				;[Block]
				;n\State: Main State
				;n\State2: A timer used for the player detection
				;n\State3: A timer for making the NPC idle (if the player escapes during that time)
				
				If (Not n\IsDead)
					If n\State = 0
						EntityType n\Collider,HIT_DEAD
					Else
						EntityType n\Collider,HIT_PLAYER
					EndIf
					
					prevFrame = n\Frame
					
					n\BlinkTimer = 1
					
					Select n\State
						Case 0 ;Lying next to the wall
							SetNPCFrame(n,11)
						Case 1 ;Standing up
							AnimateNPC(n,11,32,0.1,False)
							If n\Frame => 29
								n\State = 2
							EndIf
						Case 2 ;Being active
							PlayerSeeAble = MeNPCSeesPlayer(n)
							If PlayerSeeAble=1 Or n\State2 > 0.0
								If PlayerSeeAble=1
									n\State2 = 70*2
								Else
									n\State2 = Max(n\State2-FPSfactor,0)
								EndIf
								PointEntity n\obj, Collider
								RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj), EntityYaw(n\Collider), 20.0), 0
								
								AnimateNPC(n, 64, 93, n\CurrSpeed*30)
								n\CurrSpeed = CurveValue(n\Speed*0.7, n\CurrSpeed, 20.0)
								MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
								
								If EntityDistance(n\Collider,Collider)<1.0
									If (Abs(DeltaYaw(n\Collider,Collider))<=60.0)
										n\State = 3
									EndIf
								EndIf
								
								n\PathTimer = 0
								n\PathStatus = 0
								n\PathLocation = 0
								n\State3 = 0
							Else
								If n\PathStatus = 1
									If n\Path[n\PathLocation]=Null Then 
										If n\PathLocation > 19 Then 
											n\PathLocation = 0 : n\PathStatus = 0
										Else
											n\PathLocation = n\PathLocation + 1
										EndIf
									Else
										PointEntity n\obj, n\Path[n\PathLocation]\obj
										RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj), EntityYaw(n\Collider), 20.0), 0
										
										AnimateNPC(n, 64, 93, n\CurrSpeed*30)
										n\CurrSpeed = CurveValue(n\Speed*0.7, n\CurrSpeed, 20.0)
										MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
										
										;opens doors in front of him
										dist2# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
										If dist2 < 0.6 Then
											temp = True
											If n\Path[n\PathLocation]\door <> Null Then
												If (Not n\Path[n\PathLocation]\door\IsElevatorDoor)
													If n\Path[n\PathLocation]\door\locked Or n\Path[n\PathLocation]\door\KeyCard>0 Or n\Path[n\PathLocation]\door\Code<>"" Then
														temp = False
													Else
														If n\Path[n\PathLocation]\door\open = False Then UseDoor(n\Path[n\PathLocation]\door, False)
													EndIf
												EndIf
											EndIf
											If dist2#<0.2 And temp
												n\PathLocation = n\PathLocation + 1
											ElseIf dist2#<0.5 And (Not temp)
												n\PathStatus = 0
												n\PathTimer# = 0.0
											EndIf
										EndIf
										
										;If Rand(100)=3
										;	n\PathStatus = FindPath(n,EntityX(Collider),EntityY(Collider),EntityZ(Collider))
										;EndIf
									EndIf
								Else
									AnimateNPC(n, 323, 344, 0.2, True)
									n\CurrSpeed = 0
									If n\PathTimer < 70*5
										n\PathTimer = n\PathTimer + Rnd(1,2+(2*SelectedDifficulty\aggressiveNPCs))*FPSfactor
									Else
										n\PathStatus = FindPath(n,EntityX(Collider),EntityY(Collider),EntityZ(Collider))
										n\PathTimer = 0
									EndIf
								EndIf
								
								If EntityDistance(n\Collider,Collider)>HideDistance
									If n\State3 < 70*(15+(10*SelectedDifficulty\aggressiveNPCs))
										n\State3 = n\State3+FPSfactor
									Else
										DebugLog "SCP-008-1 IDLE"
										n\State3 = 70*(6*60)
										n\State = 4
									EndIf
								EndIf
							EndIf
							
							If n\CurrSpeed > 0.005 Then
								If (prevFrame < 80 And n\Frame=>80) Or (prevFrame > 92 And n\Frame<65)
									PlaySound2(StepSFX(0,0,Rand(0,7)),Camera, n\Collider, 8.0, Rnd(0.3,0.5))
								EndIf
							EndIf
							
							n\SoundChn = LoopSound2(n\Sound,n\SoundChn,Camera,n\Collider)
						Case 3 ;Attacking
							AnimateNPC(n, 126, 165, 0.4, False)
							If (n\Frame => 146 And prevFrame < 146)
								If EntityDistance(n\Collider,Collider)<1.1
									If (Abs(DeltaYaw(n\Collider,Collider))<=60.0)
										PlaySound_Strict DamageSFX(Rand(5,8))
										Injuries = Injuries+Rnd(0.4,1.0)
										Infect = Infect + (1+(1*SelectedDifficulty\aggressiveNPCs))
										DeathMSG = I_Loc\DeathMessage_008zombie
									EndIf
								EndIf
							ElseIf n\Frame => 164
								If EntityDistance(n\Collider,Collider)<1.1
									If (Abs(DeltaYaw(n\Collider,Collider))<=60.0)
										SetNPCFrame(n,126)
									Else
										n\State = 2
									EndIf
								Else
									n\State = 2
								EndIf
							EndIf
						Case 4 ;Idling
							HideEntity n\obj
							HideEntity n\Collider
							n\DropSpeed = 0
							PositionEntity n\Collider,0,500,0
							ResetEntity n\Collider
							If n\Idle > 0
								n\Idle = Max(n\Idle-(1+(1*SelectedDifficulty\aggressiveNPCs))*FPSfactor,0)
							Else
								If PlayerInReachableRoom() ;Player is in a room where SCP-008-1 can teleport to
									If Rand(50-(20*SelectedDifficulty\aggressiveNPCs))=1
										ShowEntity n\Collider
										ShowEntity n\obj
										For w.waypoints = Each WayPoints
											If w\door=Null And w\room\dist < HideDistance And Rand(3)=1 Then
												If EntityDistance(w\room\obj,n\Collider)<EntityDistance(Collider,n\Collider)
													x = Abs(EntityX(n\Collider)-EntityX(w\obj,True))
													If x < 12.0 And x > 4.0 Then
														z = Abs(EntityZ(n\Collider)-EntityZ(w\obj,True))
														If z < 12 And z > 4.0 Then
															If w\room\dist > 4
																DebugLog "MOVING 008-1 TO "+w\room\roomtemplate\name
																PositionEntity n\Collider, EntityX(w\obj,True), EntityY(w\obj,True)+0.25,EntityZ(w\obj,True)
																ResetEntity n\Collider
																n\PathStatus = 0
																n\PathTimer# = 0.0
																n\PathLocation = 0
																Exit
															EndIf
														EndIf
													EndIf
												EndIf
											EndIf
										Next
										n\State = 2
										n\State3 = 0
									EndIf
								EndIf
							EndIf
					End Select
				Else
					If n\SoundChn <> 0
						StopChannel n\SoundChn
						n\SoundChn = 0
						FreeSound_Strict n\Sound
						n\Sound = 0
					EndIf
					AnimateNPC(n, 344, 363, 0.5, False)
				EndIf
				
				RotateEntity n\obj,0,EntityYaw(n\Collider)-180,0
				PositionEntity n\obj,EntityX(n\Collider),EntityY(n\Collider)-0.2,EntityZ(n\Collider)
				;[End Block]
		End Select
		
		If n\IsDead
			EntityType n\Collider,HIT_DEAD
		EndIf
		
		Local gravityDist = Distance(EntityX(Collider),EntityZ(Collider),EntityX(n\Collider),EntityZ(n\Collider))
		
		If gravityDist<HideDistance*0.7 Or n\NPCtype = NPCtype1499 Then
			If n\InFacility = InFacility
				TranslateEntity n\Collider, 0, n\DropSpeed, 0
				
				Local CollidedFloor% = False
				For i% = 1 To CountCollisions(n\Collider)
					If CollisionY(n\Collider, i) < EntityY(n\Collider) - 0.01 Then CollidedFloor = True : Exit
				Next
				
				If CollidedFloor = True Then
					n\DropSpeed# = 0
				Else
					If ShouldEntitiesFall
;						If n\FallingPickDistance>0
;							Local pick = LinePick(EntityX(n\Collider),EntityY(n\Collider),EntityZ(n\Collider),0,-n\FallingPickDistance,0)
;							If pick
;								n\DropSpeed# = Max(n\DropSpeed - 0.005*FPSfactor*n\GravityMult,-n\MaxGravity)
;							Else
;								n\DropSpeed# = 0
;							EndIf
;						Else
;							n\DropSpeed# = Max(n\DropSpeed - 0.005*FPSfactor*n\GravityMult,-n\MaxGravity)
;						EndIf
						Local UpdateGravity% = False
						Local MaxX#,MinX#,MaxZ#,MinZ#
						If n\InFacility=1
							If PlayerRoom\RoomTemplate\Name$ <> "173"
								For e.Events = Each Events
									If e\EventName = "room860"
										If e\EventState = 1.0
											UpdateGravity = True
											Exit
										EndIf
									EndIf
								Next
							Else
								UpdateGravity = True
							EndIf
							If (Not UpdateGravity)
								For r.Rooms = Each Rooms
									If IsInRoom(r, EntityX(n\Collider), EntityZ(n\Collider))
										If r=PlayerRoom
											UpdateGravity = True
											Exit
										EndIf
										If IsRoomAdjacent(PlayerRoom,r)
											UpdateGravity = True
											Exit
										EndIf
										For i=0 To 3
											If (IsRoomAdjacent(PlayerRoom\Adjacent[i],r))
												UpdateGravity = True
												Exit
											EndIf
										Next
									EndIf
								Next
							EndIf
						Else
							UpdateGravity = True
						EndIf
						If UpdateGravity
							n\DropSpeed# = Max(n\DropSpeed - 0.005*FPSfactor*n\GravityMult,-n\MaxGravity)
						Else
							If n\FallingPickDistance>0
								n\DropSpeed = 0.0
							Else
								n\DropSpeed# = Max(n\DropSpeed - 0.005*FPSfactor*n\GravityMult,-n\MaxGravity)
							EndIf
						EndIf
					Else
						n\DropSpeed# = 0.0
					EndIf
				EndIf
			Else
				n\DropSpeed = 0
			EndIf
		Else
			n\DropSpeed = 0
		EndIf
		
		CatchErrors(Chr(34)+n\NVName+Chr(34)+" NPC")
		
	Next
	
	If MTF_CameraCheckTimer>0.0 And MTF_CameraCheckTimer<70*90
		MTF_CameraCheckTimer=MTF_CameraCheckTimer+FPSfactor
	ElseIf MTF_CameraCheckTimer>=70*90
		MTF_CameraCheckTimer=0.0
		If (Not PlayerDetected)
			If MTF_CameraCheckDetected
				PlayAnnouncement("SFX\Character\MTF\AnnouncCameraFound"+Rand(1,2)+".ogg")
				PlayerDetected=True
				MTF_CameraCheckTimer=70*60
			Else
				PlayAnnouncement("SFX\Character\MTF\AnnouncCameraNoFound.ogg")
			EndIf
		EndIf
		MTF_CameraCheckDetected=False
		If MTF_CameraCheckTimer=0.0
			PlayerDetected=False
		EndIf
	EndIf
	
End Function

Function TeleportCloser(n.NPCs)
	Local closestDist# = 0
	Local closestWaypoint.WayPoints
	Local w.WayPoints
	
	Local xtemp#, ztemp#
	
	For w.WayPoints = Each WayPoints
		If w\door = Null Then
			xtemp = Abs(EntityX(w\obj,True)-EntityX(n\Collider,True))
			If xtemp < 10.0 And xtemp > 1.0 Then 
				ztemp = Abs(EntityZ(w\obj,True)-EntityZ(n\Collider,True))
				If ztemp < 10.0 And ztemp > 1.0 Then
					If (EntityDistance(Collider, w\obj)>16-(8*SelectedDifficulty\aggressiveNPCs)) Then
						;teleports to the nearby waypoint that takes it closest to the player
						Local newDist# = EntityDistance(Collider, w\obj)
						If (newDist < closestDist Or closestWaypoint = Null) Then
							closestDist = newDist	
							closestWaypoint = w
						EndIf						
					EndIf
				EndIf
			EndIf
		EndIf
	Next
	
	Local shouldTeleport% = False
	If (closestWaypoint<>Null) Then
		If n\InFacility <> 1 Or SelectedDifficulty\aggressiveNPCs Then
			shouldTeleport = True
		ElseIf EntityY(closestWaypoint\obj,True)<=7.0 And EntityY(closestWaypoint\obj,True)>=-10.0 Then
			shouldTeleport = True
		EndIf
		
		If shouldTeleport Then
			PositionEntity n\Collider, EntityX(closestWaypoint\obj,True), EntityY(closestWaypoint\obj,True)+0.15, EntityZ(closestWaypoint\obj,True), True
			ResetEntity n\Collider
			n\PathStatus = 0
			n\PathTimer# = 0.0
			n\PathLocation = 0
		EndIf
	EndIf
	
End Function

Function OtherNPCSeesMeNPC%(me.NPCs,other.NPCs)
	If other\BlinkTimer<=0.0 Then Return False
	
	If EntityDistance(other\Collider,me\Collider)<6.0 Then
		If Abs(DeltaYaw(other\Collider,me\Collider))<60.0 Then
			Return True
		EndIf
	EndIf
	Return False
End Function

Function MeNPCSeesPlayer%(me.NPCs,disablesoundoncrouch%=False)
	;Return values:
		;False (=0): Player is not detected anyhow
		;True (=1): Player is detected by vision
		;2: Player is detected by emitting a sound
		;3: Player is detected by a camera (only for MTF Units!)
		;4: Player is detected through glass
	
	If NoTarget Then Return False
	
	If (Not PlayerDetected) Or me\NPCtype <> NPCtypeMTF
		If me\BlinkTimer<=0.0 Then Return False
		If EntityDistance(Collider,me\Collider)>(8.0-CrouchState+PlayerSoundVolume) Then Return False
		
		;spots the player if he's either in view or making a loud sound
		If PlayerSoundVolume>1.0
			If (Abs(DeltaYaw(me\Collider,Collider))>60.0) And EntityVisible(me\Collider,Collider)
				Return 1
			ElseIf (Not EntityVisible(me\Collider,Collider))
				If disablesoundoncrouch% And Crouch%
					Return False
				Else
					Return 2
				EndIf
			EndIf
		Else
			If (Abs(DeltaYaw(me\Collider,Collider))>60.0) Then Return False
		EndIf
		Return EntityVisible(me\Collider,Collider)
	Else
		If EntityDistance(Collider,me\Collider)>(8.0-CrouchState+PlayerSoundVolume) Then Return 3
		If EntityVisible(me\Collider, Camera) Then Return True
		
		;spots the player if he's either in view or making a loud sound
		If PlayerSoundVolume>1.0 Then Return 2
		Return 3
	EndIf
	
End Function

Function Sees096Face%(n.NPCs)
	Local angle# = WrapAngle(DeltaYaw(n\Collider, Collider));-EntityYaw(n\Collider,True))
	Return (Not NoTarget) And (angle < 55 Lor angle > 360-55) And EntityVisible(Camera, n\obj2) And EntityInView(n\obj2, Camera) And (BlinkTimer < - 16 Lor BlinkTimer > - 6) And (Not IsNVGBlinking)
End Function

Function TeleportMTFGroup(n.NPCs)
	Local n2.NPCs
	
	If n\MTFLeader <> Null Then Return
	
	TeleportCloser(n)
	
	For n2 = Each NPCs
		If n2\NPCtype = NPCtypeMTF
			If n2\MTFLeader <> Null
				PositionEntity n2\Collider,EntityX(n2\MTFLeader\Collider),EntityY(n2\MTFLeader\Collider)+0.1,EntityZ(n2\MTFLeader\Collider)
			EndIf
		EndIf
	Next
	
	DebugLog "Teleported MTF Group (dist:"+EntityDistance(n\Collider,Collider)+")"
	
End Function

Function UpdateMTFUnit(n.NPCs)
	;[Block]
	
	If n\NPCtype<>NPCtypeMTF Then
		Local realType$ = ""
		Select n\NPCtype
			Case NPCtype173
                realType = "173"
			Case NPCtypeOldMan
                realType = "106"
			Case NPCtypeGuard
                realType = "guard"
			Case NPCtypeD
                realType = "d"
			Case NPCtype372
                realType = "372"
			Case NPCtypeApache
                realType = "apache"
			Case NPCtype096
                realType = "096"
			Case NPCtype049
                realType = "049"
			Case NPCtypeZombie
                realType = "zombie"
			Case NPCtype5131
                realType = "513-1"
			Case NPCtypeTentacle
                realType = "tentacle"
			Case NPCtype860
                realType = "860"
			Case NPCtype939
                realType = "939"
			Case NPCtype066
                realType = "066"
			Case NPCtypePdPlane
                realType = "PDPlane"
			Case NPCtype966
                realType = "966"
			Case NPCtype1048a
                realType = "1048-A"
			Case NPCtype1499
				realType = "1499-1"
		End Select
		RuntimeErrorExt "Called UpdateMTFUnit on "+realType
	EndIf
	;[End Block]
	
	Local x#,y#,z#
	Local r.Rooms
	Local prevDist#,newDist#
	Local n2.NPCs
	
	Local p.Particles, target, dist#, dist2#
	
	If n\IsDead Then
		n\BlinkTimer = -1.0
		SetNPCFrame(n, 532)
		If ChannelPlaying(n\SoundChn2) Then
			StopChannel(n\SoundChn2)
		EndIf
		Return
	EndIf
	
	n\MaxGravity = 0.03
	
	n\BlinkTimer = n\BlinkTimer - FPSfactor
	If n\BlinkTimer<=-5.0 Then 
		;only play the "blinking" sound clip if searching/containing 173
		If n\State = 2
			If OtherNPCSeesMeNPC(Curr173,n)
				PlayMTFSound(LoadTempSound("SFX\Character\MTF\173\BLINKING.ogg"),n)
			EndIf
		EndIf
		n\BlinkTimer = 70.0*Rnd(10.0,15.0)
	EndIf	
	
	n\Reload = n\Reload - FPSfactor
	
	Local prevFrame# = n\Frame
	
	n\BoneToManipulate = ""
	;n\BoneToManipulate2 = ""
	n\ManipulateBone = False
	n\ManipulationType = 0
	n\NPCNameInSection = "MTF"
	
	If Int(n\State) <> 1 Then n\PrevState = 0
	
	n\SoundChn2 = LoopSound2(MTFSFX(6),n\SoundChn2,Camera,n\Collider)
	
	If n\Idle>0.0 Then
		FinishWalking(n,488,522,0.015*26)
		n\Idle=n\Idle-FPSfactor
		If n\Idle<=0.0 Then n\Idle = 0.0
	Else
		Select Int(n\State) ;what is this MTF doing
			Case 0 ;wandering around
                ;[Block]
                n\Speed = 0.015
                If n\PathTimer<=0.0 Then ;update path
					If n\MTFLeader<>Null Then ;i'll follow the leader
						n\PathStatus = FindPath(n,EntityX(n\MTFLeader\Collider,True),EntityY(n\MTFLeader\Collider,True)+0.1,EntityZ(n\MTFLeader\Collider,True)) ;whatever you say boss
					Else ;i am the leader
						If Curr173\Idle<>2
							For r = Each Rooms
								If ((Abs(r\x-EntityX(n\Collider,True))>12.0) Or (Abs(r\z-EntityZ(n\Collider,True))>12.0)) And (Rand(1,Max(4-Int(Abs(r\z-EntityZ(n\Collider,True)/8.0)),2))=1) Then
									x = r\x
									y = 0.1
									z = r\z
									DebugLog r\RoomTemplate\Name
									Exit
								EndIf
							Next
						Else
							Local tmp = False
							If EntityDistance(n\Collider,Curr173\Collider)>4.0
								If (Not EntityVisible(n\Collider,Curr173\Collider))
									tmp = True
								EndIf
							EndIf
							
							If (Not tmp)
								For r = Each Rooms
									If r\RoomTemplate\Name$ = "start"
										Local foundChamber% = False
										Local pvt% = CreatePivot()
										PositionEntity pvt%,EntityX(r\obj,True)+4736*RoomScale,0.5,EntityZ(r\obj,True)+1692*RoomScale
										
										If Distance(EntityX(pvt%),EntityZ(pvt%),EntityX(n\Collider),EntityZ(n\Collider))<3.5
											foundChamber% = True
											DebugLog Distance(EntityX(pvt%),EntityZ(pvt%),EntityX(n\Collider),EntityZ(n\Collider))
										EndIf
										
										If Curr173\Idle = 3 And Distance(EntityX(pvt%),EntityZ(pvt%),EntityX(n\Collider),EntityZ(n\Collider)) > 4.0
											If r\RoomDoors[1]\open = True Then UseDoor(r\RoomDoors[1],False)
										EndIf
										
										FreeEntity pvt%
										
										If Distance(EntityX(n\Collider),EntityZ(n\Collider),EntityX(r\obj,True)+4736*RoomScale,EntityZ(r\obj,True)+1692*RoomScale)>1.6 And (Not foundChamber)
											x = EntityX(r\obj,True)+4736*RoomScale
											y = 0.1
											z = EntityZ(r\obj,True)+1692*RoomScale
											DebugLog "Move to 173's chamber"
											Exit
										ElseIf Distance(EntityX(n\Collider),EntityZ(n\Collider),EntityX(r\obj,True)+4736*RoomScale,EntityZ(r\obj,True)+1692*RoomScale)>1.6 And foundChamber
											n\PathX = EntityX(r\obj,True)+4736*RoomScale
											n\PathZ = EntityZ(r\obj,True)+1692*RoomScale
											DebugLog "Move inside 173's chamber"
											Exit
										Else
											Curr173\Idle = 3
											Curr173\Target = Null
											Curr173\IsDead = True
											If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
											n\Sound = LoadSound_Strict("SFX\Character\MTF\173\Cont"+Rand(1,4)+".ogg")
											PlayMTFSound(n\Sound, n)
											PlayAnnouncement("SFX\Character\MTF\Announc173Contain.ogg")
											DebugLog "173 contained"
											Exit
										EndIf
									EndIf
								Next
							Else
								x = EntityX(Curr173\Collider)
								y = 0.1
								z = EntityZ(Curr173\Collider)
								DebugLog "Going back to 173's cage"
							EndIf
						EndIf
						If n\PathX=0 Then n\PathStatus = FindPath(n,x,y,z) ;we're going to this room for no particular reason
					EndIf
					If n\PathStatus = 1 Then
						While n\Path[n\PathLocation]=Null
							If n\PathLocation>19 Then Exit
							n\PathLocation=n\PathLocation+1
						Wend
						If n\PathLocation<19 Then
							If (n\Path[n\PathLocation]<>Null) And (n\Path[n\PathLocation+1]<>Null) Then
								If (n\Path[n\PathLocation]\door=Null) Then
									If Abs(DeltaYaw(n\Collider,n\Path[n\PathLocation]\obj))>Abs(DeltaYaw(n\Collider,n\Path[n\PathLocation+1]\obj)) Then
										n\PathLocation=n\PathLocation+1
									EndIf
								EndIf
							EndIf
						EndIf
					EndIf
					n\PathTimer = 70.0 * Rnd(6.0,10.0) ;search again after 6-10 seconds
                ElseIf (n\PathTimer<=70.0 * 2.5) And (n\MTFLeader=Null) Then
					n\PathTimer=n\PathTimer-FPSfactor
					n\CurrSpeed = 0.0
					If Rand(1,35)=1 Then
						RotateEntity n\Collider,0.0,Rnd(360.0),0.0,True
					EndIf
					FinishWalking(n,488,522,n\Speed*26)
					n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
					RotateEntity n\obj,-90.0,n\Angle,0.0,True
                Else
					If n\PathStatus=2 Then
						n\PathTimer=n\PathTimer-(FPSfactor*2.0) ;timer goes down fast
						n\CurrSpeed = 0.0
						If Rand(1,35)=1 Then
							RotateEntity n\Collider,0.0,Rnd(360.0),0.0,True
						EndIf
						FinishWalking(n,488,522,n\Speed*26)
						n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
						RotateEntity n\obj,-90.0,n\Angle,0.0,True
					ElseIf n\PathStatus=1 Then
						If n\Path[n\PathLocation]=Null Then
							If n\PathLocation > 19 Then
								n\PathLocation = 0 : n\PathStatus = 0
							Else
								n\PathLocation = n\PathLocation + 1
							EndIf
						Else
							prevDist# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
							
							PointEntity n\Collider,n\Path[n\PathLocation]\obj
							RotateEntity n\Collider,0.0,EntityYaw(n\Collider,True),0.0,True
							
							n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
							
							RotateEntity n\obj,-90.0,n\Angle,0.0,True
							
							n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,20.0)
							;MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
							
							TranslateEntity n\Collider, Cos(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, 0, Sin(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, True
							AnimateNPC(n,488, 522, n\CurrSpeed*26)
							
							newDist# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
							
							If (newDist<1.0 And n\Path[n\PathLocation]\door<>Null) Then
								;open the door and make it automatically close after 5 seconds
								If (Not n\Path[n\PathLocation]\door\open)
									Local sound = 0
									If n\Path[n\PathLocation]\door\dir = 1 Then sound = 0 Else sound=Rand(0, 2)
									PlaySound2(OpenDoorSFX(n\Path[n\PathLocation]\door\dir,sound),Camera,n\Path[n\PathLocation]\door\obj)
									PlayMTFSound(MTFSFX(5),n)
								EndIf
								n\Path[n\PathLocation]\door\open = True
								If n\Path[n\PathLocation]\door\MTFClose
									n\Path[n\PathLocation]\door\timerstate = 70.0*5.0
								EndIf
							EndIf
                            
							If (newDist<0.2) Or ((prevDist<newDist) And (prevDist<1.0)) Then
								n\PathLocation=n\PathLocation+1
							EndIf
						EndIf
						n\PathTimer=n\PathTimer-FPSfactor ;timer goes down slow
					ElseIf n\PathX#<>0.0
						pvt = CreatePivot()
						PositionEntity pvt,n\PathX#,0.5,n\PathZ#
						
						PointEntity n\Collider,pvt
						RotateEntity n\Collider,0.0,EntityYaw(n\Collider,True),0.0,True
						n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
						RotateEntity n\obj,-90.0,n\Angle,0.0,True
						
						n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,20.0)
						TranslateEntity n\Collider, Cos(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, 0, Sin(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, True
						AnimateNPC(n,488, 522, n\CurrSpeed*26)
						
						If Distance(EntityX(n\Collider),EntityZ(n\Collider),n\PathX#,n\PathZ#)<0.2
							n\PathX# = 0.0
							n\PathZ# = 0.0
							n\PathTimer = 70.0 * Rnd(6.0,10.0)
						EndIf
						
						FreeEntity pvt
					Else
						n\PathTimer=n\PathTimer-(FPSfactor*2.0) ;timer goes down fast
						If n\MTFLeader = Null Then
							If Rand(1,35)=1 Then
								RotateEntity n\Collider,0.0,Rnd(360.0),0.0,True
							EndIf
							FinishWalking(n,488,522,n\Speed*26)
							n\CurrSpeed = 0.0
						ElseIf EntityDistance(n\Collider,n\MTFLeader\Collider)>1.0 Then
							PointEntity n\Collider,n\MTFLeader\Collider
							RotateEntity n\Collider,0.0,EntityYaw(n\Collider,True),0.0,True
							
							n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,20.0)
							TranslateEntity n\Collider, Cos(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, 0, Sin(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, True
							AnimateNPC(n,488, 522, n\CurrSpeed*26)
						Else
							If Rand(1,35)=1 Then
								RotateEntity n\Collider,0.0,Rnd(360.0),0.0,True
							EndIf
							FinishWalking(n,488,522,n\Speed*26)
							n\CurrSpeed = 0.0
						EndIf
						n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
						RotateEntity n\obj,-90.0,n\Angle,0.0,True
					EndIf
                EndIf
                
				Local temp = MeNPCSeesPlayer(n)
				
				If NoTarget Then temp = False
				
                If temp>False Then
					If n\LastSeen > 0 And n\LastSeen < 70*15 Then
						If temp < 2
							If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
							n\Sound = LoadSound_Strict("SFX\Character\MTF\ThereHeIs"+Rand(1,6)+".ogg")
							PlayMTFSound(n\Sound, n)
						EndIf
					Else
						If temp = True
							If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
							n\Sound = LoadSound_Strict("SFX\Character\MTF\Stop"+Rand(1,6)+".ogg")
							PlayMTFSound(n\Sound, n)
						ElseIf temp = 2
							;If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
							;n\Sound = MTFSFX(Rand(0,3))
							;PlayMTFSound(n\Sound, n)
							PlayMTFSound(MTFSFX(Rand(0,3)),n)
						EndIf
					EndIf
					
					n\LastSeen = (70*Rnd(30,40))
					n\LastDist = 1
					
					n\State = 1
					n\EnemyX = EntityX(Collider,True)
					n\EnemyY = EntityY(Collider,True)
					n\EnemyZ = EntityZ(Collider,True)
					n\State2 = 70.0*(15.0*temp) ;give up after 15 seconds (30 seconds if detected by loud noise, over camera: 45)
					DebugLog "player spotted :"+n\State2
					n\PathTimer=0.0
					n\PathStatus=0
					n\Reload = 200-(100*SelectedDifficulty\aggressiveNPCs)
					
					;If EntityDistance(n\Collider,Collider)>HideDistance*0.7
					;	TeleportMTFGroup(n)
					;EndIf
                EndIf
				
				;B3D doesn't do short-circuit evaluation, so this retarded nesting is an optimization
                If Curr173\Idle<2 Then
					Local SoundVol173# = Max(Min((Distance(EntityX(Curr173\Collider), EntityZ(Curr173\Collider), Curr173\PrevX, Curr173\PrevZ) * 2.5), 1.0), 0.0)
					If OtherNPCSeesMeNPC(Curr173,n) Or (SoundVol173#>0.0 And EntityDistance(n\Collider,Curr173\Collider)<6.0) Then
						If EntityVisible(n\Collider,Curr173\Collider) Or SoundVol173#>0.0 Then							
							n\State = 2
							n\EnemyX = EntityX(Curr173\Collider,True)
							n\EnemyY = EntityY(Curr173\Collider,True)
							n\EnemyZ = EntityZ(Curr173\Collider,True)
							n\State2 = 70.0*15.0 ;give up after 15 seconds
							n\State3 = 0.0
							n\PathTimer=0.0
							n\PathStatus=0
							DebugLog "173 spotted :"+n\State2
							If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
							n\Sound = LoadSound_Strict("SFX\Character\MTF\173\Spotted"+Rand(1,2)+".ogg")
							PlayMTFSound(n\Sound, n)
						EndIf
					EndIf
				EndIf
				
				If Curr106\State <= 0
					If OtherNPCSeesMeNPC(Curr106,n) Or EntityDistance(n\Collider,Curr106\Collider)<3.0 Then
						If EntityVisible(n\Collider,Curr106\Collider) Then
							n\State = 4
							n\EnemyX = EntityX(Curr106\Collider,True)
							n\EnemyY = EntityY(Curr106\Collider,True)
							n\EnemyZ = EntityZ(Curr106\Collider,True)
							n\State2 = 70*15.0
							n\State3 = 0.0
							n\PathTimer = 0.0
							n\PathStatus = 0
							n\Target = Curr106
							DebugLog "106 spotted :"+n\State2
							;If n\MTFLeader=Null
								If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
								n\Sound = LoadSound_Strict("SFX\Character\MTF\106\Spotted"+Rand(1,3)+".ogg")
								PlayMTFSound(n\Sound, n)
							;EndIf
						EndIf
					EndIf
				EndIf
				
				If Curr096 <> Null
					If OtherNPCSeesMeNPC(Curr096,n) Then
						If EntityVisible(n\Collider,Curr096\Collider) Then
							n\State = 8
							n\EnemyX = EntityX(Curr096\Collider,True)
							n\EnemyY = EntityY(Curr096\Collider,True)
							n\EnemyZ = EntityZ(Curr096\Collider,True)
							n\State2 = 70*15.0
							n\State3 = 0.0
							n\PathTimer = 0.0
							n\PathStatus = 0
							DebugLog "096 spotted :"+n\State2
							;If n\MTFLeader=Null
								If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
								n\Sound = LoadSound_Strict("SFX\Character\MTF\096\Spotted"+Rand(1,2)+".ogg")
								PlayMTFSound(n\Sound, n)
							;EndIf
						EndIf
					EndIf
				EndIf
				
				For n2.NPCs = Each NPCs
					If n2\NPCtype = NPCtype049
						If OtherNPCSeesMeNPC(n2,n) Then
							If EntityVisible(n\Collider,n2\Collider)
								n\State = 4
								n\EnemyX = EntityX(n2\Collider,True)
								n\EnemyY = EntityY(n2\Collider,True)
								n\EnemyZ = EntityZ(n2\Collider,True)
								n\State2 = 70*15.0
								n\State3 = 0.0
								n\PathTimer = 0.0
								n\PathStatus = 0
								n\Target = n2
								DebugLog "049 spotted :"+n\State2
								If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
								n\Sound = LoadSound_Strict("SFX\Character\MTF\049\Spotted"+Rand(1,5)+".ogg")
								PlayMTFSound(n\Sound, n)
								Exit
							EndIf
						EndIf
					ElseIf n2\NPCtype = NPCtypeZombie And n2\IsDead = False
						If OtherNPCSeesMeNPC(n2,n) Then
							If EntityVisible(n\Collider,n2\Collider)
								n\State = 9
								n\EnemyX = EntityX(n2\Collider,True)
								n\EnemyY = EntityY(n2\Collider,True)
								n\EnemyZ = EntityZ(n2\Collider,True)
								n\State2 = 70*15.0
								n\State3 = 0.0
								n\PathTimer = 0.0
								n\PathStatus = 0
								n\Target = n2
								n\Reload = 70*5
								DebugLog "049-2 spotted :"+n\State2
								If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
								n\Sound = LoadSound_Strict("SFX\Character\MTF\049\Player0492_1.ogg")
								PlayMTFSound(n\Sound, n)
								Exit
							EndIf
						EndIf
					ElseIf n2\NPCtype = NPCtype008 And n2\IsDead = False
						If OtherNPCSeesMeNPC(n2,n) Then
							If EntityVisible(n\Collider,n2\Collider)
								n\State = 9
								n\EnemyX = EntityX(n2\Collider,True)
								n\EnemyY = EntityY(n2\Collider,True)
								n\EnemyZ = EntityZ(n2\Collider,True)
								n\State2 = 70*15.0
								n\State3 = 0.0
								n\PathTimer = 0.0
								n\PathStatus = 0
								n\Target = n2
								n\Reload = 70*5
								DebugLog "008 spotted :"+n\State2
								;If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
								;n\Sound = LoadSound_Strict("SFX\Character\MTF\049\Player0492_1.ogg")
								;PlayMTFSound(n\Sound, n)
								Exit
							EndIf
						EndIf
					EndIf
				Next
                ;[End Block]
			Case 1 ;searching for player
                ;[Block]
                n\Speed = 0.015
                n\State2=n\State2-FPSfactor
                If MeNPCSeesPlayer(n) = True Then
					
					;if close enough, start shooting at the player
					If playerDist < 4.0 Then
						
						Local angle# = VectorYaw(EntityX(Collider)-EntityX(n\Collider),0,EntityZ(Collider)-EntityZ(n\Collider))
						
						RotateEntity(n\Collider, 0, CurveAngle(angle, EntityYaw(n\Collider), 10.0), 0, True)
						n\Angle = EntityYaw(n\Collider)
						
						If n\Reload =< 0 And KillTimer = 0 Then
							If EntityVisible(n\Collider, Camera) Then
								angle# = WrapAngle(angle - EntityYaw(n\Collider))
								If angle < 5 Or angle > 355 Then 
									prev% = KillTimer
									
									PlaySound2(GunshotSFX, Camera, n\Collider, 15)
									
									pvt% = CreatePivot()
									
									RotateEntity(pvt, EntityPitch(n\Collider), EntityYaw(n\Collider), 0, True)
									PositionEntity(pvt, EntityX(n\obj), EntityY(n\obj), EntityZ(n\obj))
									MoveEntity (pvt,0.8*0.079, 10.75*0.079, 6.9*0.079)
									
									Shoot(EntityX(pvt),EntityY(pvt),EntityZ(pvt),5.0/playerDist, False)
									n\Reload = 7
									
									FreeEntity(pvt)
									
									DeathMSG=I_Loc\DeathMessage_NtfBloodloss
									
									;player killed -> "target terminated"
									If prev => 0 And KillTimer < 0 Then
										DeathMSG=I_Loc\DeathMessage_Ntf
										PlayMTFSound(LoadTempSound("SFX\Character\MTF\Targetterminated"+Rand(1,4)+".ogg"),n)
									EndIf
								EndIf	
							EndIf
						EndIf
						
						For n2.NPCs = Each NPCs
							If n2\NPCtype = NPCtypeMTF And n2 <> n
								If n2\State = 0
									If EntityDistance(n\Collider,n2\Collider)<6.0
										n\PrevState = 1
										n2\LastSeen = (70*Rnd(30,40))
										n2\LastDist = 1
										
										n2\State = 1
										n2\EnemyX = EntityX(Collider,True)
										n2\EnemyY = EntityY(Collider,True)
										n2\EnemyZ = EntityZ(Collider,True)
										n2\State2 = n\State2
										n2\PathTimer=0.0
										n2\PathStatus=0
										n2\Reload = 200-(100*SelectedDifficulty\aggressiveNPCs)
										n2\PrevState = 0
									EndIf
								EndIf
							EndIf
						Next
						
						If n\PrevState = 1
							SetNPCFrame(n,423)
							n\PrevState = 2
						ElseIf n\PrevState=2
							If n\Frame>200
								n\CurrSpeed = CurveValue(0, n\CurrSpeed, 20.0)
								AnimateNPC(n, 423, 463, 0.4, False)
								If n\Frame>462.9 Then n\Frame = 78
							Else
								AnimateNPC(n, 78, 193, 0.2, False)
								n\CurrSpeed = CurveValue(0, n\CurrSpeed, 20.0)
							EndIf
						Else
							If n\Frame>958 Then
								AnimateNPC(n, 1374, 1383, 0.3, False)
								n\CurrSpeed = CurveValue(0, n\CurrSpeed, 20.0)
								If n\Frame>1382.9 Then n\Frame = 78
							Else
								AnimateNPC(n, 78, 193, 0.2, False)
								n\CurrSpeed = CurveValue(0, n\CurrSpeed, 20.0)
							EndIf
						EndIf
					Else
						PositionEntity n\obj,n\EnemyX,n\EnemyY,n\EnemyZ,True
						PointEntity n\Collider,n\obj
						RotateEntity n\Collider,0.0,EntityYaw(n\Collider,True),0.0,True
						n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
						RotateEntity n\obj,-90.0,n\Angle,0.0,True
						
						n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,20.0)
						TranslateEntity n\Collider, Cos(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, 0, Sin(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, True
						AnimateNPC(n,488, 522, n\CurrSpeed*26)
					EndIf
                Else
					n\LastSeen = n\LastSeen - FPSfactor
					
					;n\Reload = 200-(100*SelectedDifficulty\aggressiveNPCs)
					If n\Reload <= 7
						n\Reload = 7
					EndIf
					
					If n\PathTimer<=0.0 Then ;update path
						n\PathStatus = FindPath(n,n\EnemyX,n\EnemyY+0.1,n\EnemyZ)
						n\PathTimer = 70.0 * Rnd(6.0,10.0) ;search again after 6 seconds
					ElseIf n\PathTimer<=70.0 * 2.5 Then
						n\PathTimer=n\PathTimer-FPSfactor
						n\CurrSpeed = 0.0
						If Rand(1,35)=1 Then
							RotateEntity n\Collider,0.0,Rnd(360.0),0.0,True
						EndIf
						FinishWalking(n,488,522,n\Speed*26)
						n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
						RotateEntity n\obj,-90.0,n\Angle,0.0,True
					Else
						If n\PathStatus=2 Then
							n\PathTimer=n\PathTimer-(FPSfactor*2.0) ;timer goes down fast
							n\CurrSpeed = 0.0
							If Rand(1,35)=1 Then
								RotateEntity n\Collider,0.0,Rnd(360.0),0.0,True
							EndIf
							FinishWalking(n,488,522,n\Speed*26)
							n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
							RotateEntity n\obj,-90.0,n\Angle,0.0,True
						ElseIf n\PathStatus=1 Then
							If n\Path[n\PathLocation]=Null Then
								If n\PathLocation > 19 Then
									n\PathLocation = 0 : n\PathStatus = 0
								Else
									n\PathLocation = n\PathLocation + 1
								EndIf
							Else
								prevDist# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
								
								PointEntity n\Collider,n\Path[n\PathLocation]\obj
								RotateEntity n\Collider,0.0,EntityYaw(n\Collider,True),0.0,True
								n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
								RotateEntity n\obj,-90.0,n\Angle,0.0,True
								
								n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,20.0)
								
								TranslateEntity n\Collider, Cos(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, 0, Sin(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, True
								AnimateNPC(n,488, 522, n\CurrSpeed*26)
								
								newDist# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
								
								If (newDist<1.0 And n\Path[n\PathLocation]\door<>Null) Then
									;open the door and make it automatically close after 5 seconds
									If (Not n\Path[n\PathLocation]\door\open)
										sound = 0
										If n\Path[n\PathLocation]\door\dir = 1 Then sound = 0 Else sound=Rand(0, 2)
										PlaySound2(OpenDoorSFX(n\Path[n\PathLocation]\door\dir,sound),Camera,n\Path[n\PathLocation]\door\obj)
										PlayMTFSound(MTFSFX(5),n)
									EndIf
									n\Path[n\PathLocation]\door\open = True
									If n\Path[n\PathLocation]\door\MTFClose
										n\Path[n\PathLocation]\door\timerstate = 70.0*5.0
									EndIf
								EndIf
								
								If (newDist<0.2) Or ((prevDist<newDist) And (prevDist<1.0)) Then
									n\PathLocation=n\PathLocation+1
								EndIf
							EndIf
							n\PathTimer=n\PathTimer-FPSfactor ;timer goes down slow
						Else
							PositionEntity n\obj,n\EnemyX,n\EnemyY,n\EnemyZ,True
							If (Distance(EntityX(n\Collider,True),EntityZ(n\Collider,True),n\EnemyX,n\EnemyZ)<0.2) Or (Not EntityVisible(n\obj,n\Collider)) Then
								If Rand(1,35)=1 Then
									RotateEntity n\Collider,0.0,Rnd(360.0),0.0,True
								EndIf
								FinishWalking(n,488,522,n\Speed*26)
								If Rand(1,35)=1 Then
									For wp.Waypoints = Each WayPoints
										If (Rand(1,3)=1) Then
											If (EntityDistance(wp\obj,n\Collider)<6.0) Then
												n\EnemyX = EntityX(wp\obj,True)
												n\EnemyY = EntityY(wp\obj,True)
												n\EnemyZ = EntityZ(wp\obj,True)
												n\PathTimer = 0.0
												Exit
											EndIf											
										EndIf
									Next
								EndIf
								n\PathTimer=n\PathTimer-FPSfactor ;timer goes down slow
							Else
								PointEntity n\Collider,n\obj
								RotateEntity n\Collider,0.0,EntityYaw(n\Collider,True),0.0,True
								n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
								RotateEntity n\obj,-90.0,n\Angle,0.0,True
								
								n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,20.0)
								TranslateEntity n\Collider, Cos(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, 0, Sin(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, True
								AnimateNPC(n,488, 522, n\CurrSpeed*26)
							EndIf
						EndIf
					EndIf
					
					If n\MTFLeader=Null And n\LastSeen<70*30 And n\LastSeen+FPSfactor=>70*30 Then
						If Rand(2)=1 Then 
							PlayMTFSound(LoadTempSound("SFX\Character\MTF\Searching"+Rand(1,6)+".ogg"),n)
						EndIf
					EndIf
					
					;If EntityDistance(n\Collider,Collider)>HideDistance*0.7
					;	TeleportMTFGroup(n)
					;EndIf
                EndIf
                
                If n\State2<=0.0 And n\State2+FPSfactor >0.0 Then
					If n\MTFLeader = Null Then
						DebugLog "targetlost: "+n\State2
						PlayMTFSound(LoadTempSound("SFX\Character\MTF\Targetlost"+Rand(1,3)+".ogg"),n)
						If MTF_CameraCheckTimer=0.0
							If Rand(15-(7*SelectedDifficulty\aggressiveNPCs))=1 ;Maybe change this to another chance - ENDSHN
								PlayAnnouncement("SFX\Character\MTF\AnnouncCameraCheck.ogg")
								MTF_CameraCheckTimer = FPSfactor
							EndIf
						EndIf
					EndIf
					n\State = 0
                EndIf
                
				;B3D doesn't do short-circuit evaluation, so this retarded nesting is an optimization
                If Curr173\Idle<2 Then
					SoundVol173# = Max(Min((Distance(EntityX(Curr173\Collider), EntityZ(Curr173\Collider), Curr173\PrevX, Curr173\PrevZ) * 2.5), 1.0), 0.0)
					If OtherNPCSeesMeNPC(Curr173,n) Or (SoundVol173#>0.0 And EntityDistance(n\Collider,Curr173\Collider)<6.0) Then
						If EntityVisible(n\Collider,Curr173\Collider) Or SoundVol173#>0.0 Then	
							n\State = 2
							n\EnemyX = EntityX(Curr173\Collider,True)
							n\EnemyY = EntityY(Curr173\Collider,True)
							n\EnemyZ = EntityZ(Curr173\Collider,True)
							n\State2 = 70.0*15.0 ;give up after 15 seconds
							DebugLog "173 spotted :"+n\State2
							If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
							n\Sound = LoadSound_Strict("SFX\Character\MTF\173\Spotted3.ogg")
							PlayMTFSound(n\Sound, n)
							n\State3 = 0.0
							n\PathTimer=0.0
							n\PathStatus=0
						EndIf
					EndIf
				EndIf
				
				If Curr106\State <= 0
					If OtherNPCSeesMeNPC(Curr106,n) Or EntityDistance(n\Collider,Curr106\Collider)<3.0 Then
						If EntityVisible(n\Collider,Curr106\Collider) Then
							n\State = 4
							n\EnemyX = EntityX(Curr106\Collider,True)
							n\EnemyY = EntityY(Curr106\Collider,True)
							n\EnemyZ = EntityZ(Curr106\Collider,True)
							n\State2 = 70*15.0
							n\State3 = 0.0
							n\PathTimer = 0.0
							n\PathStatus = 0
							n\Target = Curr106
							DebugLog "106 spotted :"+n\State2
							If n\MTFLeader=Null
								If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
								n\Sound = LoadSound_Strict("SFX\Character\MTF\106\Spotted4.ogg")
								PlayMTFSound(n\Sound, n)
							EndIf
						EndIf
					EndIf
				EndIf
				
				If Curr096 <> Null
					If OtherNPCSeesMeNPC(Curr096,n) Then
						If EntityVisible(n\Collider,Curr096\Collider) Then
							n\State = 8
							n\EnemyX = EntityX(Curr096\Collider,True)
							n\EnemyY = EntityY(Curr096\Collider,True)
							n\EnemyZ = EntityZ(Curr096\Collider,True)
							n\State2 = 70*15.0
							n\State3 = 0.0
							n\PathTimer = 0.0
							n\PathStatus = 0
							DebugLog "096 spotted :"+n\State2
							If n\MTFLeader=Null
								If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
								n\Sound = LoadSound_Strict("SFX\Character\MTF\096\Spotted"+Rand(1,2)+".ogg")
								PlayMTFSound(n\Sound, n)
							EndIf
						EndIf
					EndIf
				EndIf
				
				For n2.NPCs = Each NPCs
					If n2\NPCtype = NPCtype049
						If OtherNPCSeesMeNPC(n2,n) Then
							If EntityVisible(n\Collider,n2\Collider)
								n\State = 4
								n\EnemyX = EntityX(n2\Collider,True)
								n\EnemyY = EntityY(n2\Collider,True)
								n\EnemyZ = EntityZ(n2\Collider,True)
								n\State2 = 70*15.0
								n\State3 = 0.0
								n\PathTimer = 0.0
								n\PathStatus = 0
								n\Target = n2
								DebugLog "049 spotted :"+n\State2
								If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
								n\Sound = LoadSound_Strict("SFX\Character\MTF\049\Spotted"+Rand(1,5)+".ogg")
								PlayMTFSound(n\Sound, n)
								Exit
							EndIf
						EndIf
					ElseIf n2\NPCtype = NPCtypeZombie And n2\IsDead = False
						If OtherNPCSeesMeNPC(n2,n) Then
							If EntityVisible(n\Collider,n2\Collider)
								n\State = 9
								n\EnemyX = EntityX(n2\Collider,True)
								n\EnemyY = EntityY(n2\Collider,True)
								n\EnemyZ = EntityZ(n2\Collider,True)
								n\State2 = 70*15.0
								n\State3 = 0.0
								n\PathTimer = 0.0
								n\PathStatus = 0
								n\Target = n2
								n\Reload = 70*5
								DebugLog "049-2 spotted :"+n\State2
								;If n\MTFLeader=Null
									If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
									n\Sound = LoadSound_Strict("SFX\Character\MTF\049\Player0492_1.ogg")
									PlayMTFSound(n\Sound, n)
								;EndIf
								Exit
							EndIf
						EndIf
					EndIf
				Next
				
                ;DebugLog Distance(EntityX(n\Collider,True),EntityZ(n\Collider,True),n\EnemyX,n\EnemyZ)
                
                ;[End Block]
			Case 2 ;searching for/looking at 173
                ;[Block]
                If Curr173\Idle = 2 Then
					n\State = 0
                Else
					For n2.NPCs = Each NPCs
						If n2<>n Then
							If n2\NPCtype = NPCtypeMTF Then
								n2\State = 2
							EndIf
						EndIf
					Next
					
					Local curr173Dist# = Distance(EntityX(n\Collider,True),EntityZ(n\Collider,True),EntityX(Curr173\Collider,True),EntityZ(Curr173\Collider,True))
					
					If curr173Dist<5.0 Then
						If Curr173\Idle <> 2 Then Curr173\Idle = True
						n\State2 = 70.0*15.0
						n\PathTimer = 0.0
						Local tempDist# = 1.0
						If n\MTFLeader<>Null Then tempDist = 2.0
						If curr173Dist<tempDist Then
							If n\MTFLeader = Null Then
								n\State3=n\State3+FPSfactor
								DebugLog "CONTAINING 173: "+n\State3
								;If n\State3>=70.0*10.0 Then
								If n\State3>=70.0*15.0 Then
									Curr173\Idle = 2
									If n\MTFLeader = Null Then Curr173\Target = n
									If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
									n\Sound = LoadSound_Strict("SFX\Character\MTF\173\Box"+Rand(1,3)+".ogg")
									PlayMTFSound(n\Sound, n)
								EndIf
							EndIf
							PositionEntity n\obj,EntityX(Curr173\Collider,True),EntityY(Curr173\Collider,True),EntityZ(Curr173\Collider,True),True
							PointEntity n\Collider,n\obj
							RotateEntity n\Collider,0.0,EntityYaw(n\Collider,True),0.0,True
							n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
							FinishWalking(n,488,522,n\Speed*26)
							RotateEntity n\obj,-90.0,n\Angle,0.0,True
						Else
							PositionEntity n\obj,EntityX(Curr173\Collider,True),EntityY(Curr173\Collider,True),EntityZ(Curr173\Collider,True),True
							PointEntity n\Collider,n\obj
							RotateEntity n\Collider,0.0,EntityYaw(n\Collider,True),0.0,True
							n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
							RotateEntity n\obj,-90.0,n\Angle,0.0,True
							
							n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,20.0)
							TranslateEntity n\Collider, Cos(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, 0, Sin(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, True
							AnimateNPC(n,488, 522, n\CurrSpeed*26)
						EndIf
					Else
						If Curr173\Idle <> 2 Then Curr173\Idle = False
						If n\PathTimer<=0.0 Then ;update path
							n\PathStatus = FindPath(n,EntityX(Curr173\Collider,True),EntityY(Curr173\Collider,True)+0.1,EntityZ(Curr173\Collider,True))
							n\PathTimer = 70.0 * Rnd(6.0,10.0) ;search again after 6 seconds
						ElseIf n\PathTimer<=70.0 * 2.5 Then
							n\PathTimer=n\PathTimer-FPSfactor
							n\CurrSpeed = 0.0
							If Rand(1,35)=1 Then
								RotateEntity n\Collider,0.0,Rnd(360.0),0.0,True
							EndIf
							FinishWalking(n,488,522,n\Speed*26)
							n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
							RotateEntity n\obj,-90.0,n\Angle,0.0,True
						Else
							If n\PathStatus=2 Then
								n\PathTimer=n\PathTimer-(FPSfactor*2.0) ;timer goes down fast
								n\CurrSpeed = 0.0
								If Rand(1,35)=1 Then
									RotateEntity n\Collider,0.0,Rnd(360.0),0.0,True
								EndIf
								FinishWalking(n,488,522,n\Speed*26)
								n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
								RotateEntity n\obj,-90.0,n\Angle,0.0,True
							ElseIf n\PathStatus=1 Then
								If n\Path[n\PathLocation]=Null Then
									If n\PathLocation > 19 Then
										n\PathLocation = 0 : n\PathStatus = 0
									Else
										n\PathLocation = n\PathLocation + 1
									EndIf
								Else
									prevDist# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
									
									PointEntity n\Collider,n\Path[n\PathLocation]\obj
									RotateEntity n\Collider,0.0,EntityYaw(n\Collider,True),0.0,True
									n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
									RotateEntity n\obj,-90.0,n\Angle,0.0,True
									
									n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,20.0)
									
									TranslateEntity n\Collider, Cos(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, 0, Sin(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, True
									AnimateNPC(n,488, 522, n\CurrSpeed*26)
									
									newDist# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
									
									If (newDist<1.0 And n\Path[n\PathLocation]\door<>Null) Then
										;open the door and make it automatically close after 5 seconds
										If (Not n\Path[n\PathLocation]\door\open)
											sound = 0
											If n\Path[n\PathLocation]\door\dir = 1 Then sound = 0 Else sound=Rand(0, 2)
											PlaySound2(OpenDoorSFX(n\Path[n\PathLocation]\door\dir,sound),Camera,n\Path[n\PathLocation]\door\obj)
											PlayMTFSound(MTFSFX(5),n)
										EndIf
										n\Path[n\PathLocation]\door\open = True
										If n\Path[n\PathLocation]\door\MTFClose
											n\Path[n\PathLocation]\door\timerstate = 70.0*5.0
										EndIf
									EndIf
									
									If (newDist<0.2) Or ((prevDist<newDist) And (prevDist<1.0)) Then
										n\PathLocation=n\PathLocation+1
									EndIf
								EndIf
								n\PathTimer=n\PathTimer-FPSfactor ;timer goes down slow
							Else
								n\PathTimer=n\PathTimer-(FPSfactor*2.0) ;timer goes down fast
								n\CurrSpeed = 0.0
								If Rand(1,35)=1 Then
									RotateEntity n\Collider,0.0,Rnd(360.0),0.0,True
								EndIf
								FinishWalking(n,488,522,n\Speed*26)
								n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
								RotateEntity n\obj,-90.0,n\Angle,0.0,True
							EndIf
						EndIf
					EndIf
                EndIf
                ;[End Block]
			Case 3 ;following a path
				;[Block]
				
				n\Angle = CurveValue(0,n\Angle,40.0)
				
				If n\PathStatus = 2 Then
					n\State = 5
					n\CurrSpeed = 0
				ElseIf n\PathStatus = 1
					If n\Path[n\PathLocation]=Null Then 
						If n\PathLocation > 19 Then 
							n\PathLocation = 0
							n\PathStatus = 0
							If n\LastSeen > 0 Then n\State = 5
						Else
							n\PathLocation = n\PathLocation + 1
						EndIf
					Else
						If n\Path[n\PathLocation]\door <> Null Then
							If n\Path[n\PathLocation]\door\open = False Then
								n\Path[n\PathLocation]\door\open = True
								n\Path[n\PathLocation]\door\timerstate = 8.0*70.0
								PlayMTFSound(MTFSFX(5),n)
							EndIf
						EndIf
						
						If dist < HideDistance*0.7 Then 
							dist2# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj) 
							
							;If Rand(5)=1 Then 
							;	For n2.NPCs = Each NPCs
							;		If n2\NPCtype = n\NPCtype And n2<>n Then
							;			If EntityDistance(n\Collider, n2\Collider)<2 Then
							;				n\Idle = 150
							;			EndIf
							;		EndIf
							;	Next
							;EndIf
							
							PointEntity n\obj, n\Path[n\PathLocation]\obj
							
							RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj), EntityYaw(n\Collider), 10.0), 0
							If n\Idle = 0 Then
								n\CurrSpeed = CurveValue(n\Speed*Max(Min(dist2,1.0),0.1), n\CurrSpeed, 20.0)
								MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
								
								;If dist2 < (0.25+((n\Path[Min(n\PathLocation+1,19)]=Null)*0.3 * (n\ID Mod 3))) Then
								If EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)<0.5
									n\PathLocation = n\PathLocation + 1
								EndIf
							EndIf
						Else
							If Rand(20)=1 Then 
								PositionEntity n\Collider, EntityX(n\Path[n\PathLocation]\obj,True),EntityY(n\Path[n\PathLocation]\obj,True)+0.25,EntityZ(n\Path[n\PathLocation]\obj,True),True
								n\PathLocation = n\PathLocation + 1
								ResetEntity n\Collider
							EndIf
						EndIf
						
					EndIf
				Else
					n\CurrSpeed = 0
					n\State = 5
				EndIf
				
				
				If n\Idle = 0 And n\PathStatus = 1 Then
					If dist < HideDistance Then
						If n\Frame>959 Then
							AnimateNPC(n, 1376, 1383, 0.2, False)
							If n\Frame >1382.9 Then n\Frame = 488
						Else
							AnimateNPC(n, 488, 522, n\CurrSpeed*30)
						EndIf
					EndIf
				Else
					If dist < HideDistance Then
						If n\LastSeen > 0 Then 
							AnimateNPC(n, 78, 312, 0.2, True)
						Else
							If n\Frame<962 Then
								If n\Frame>487 Then n\Frame = 463
								AnimateNPC(n, 463, 487, 0.3, False)
								If n\Frame>486.9 Then n\Frame = 962
							Else
								AnimateNPC(n, 962, 1259, 0.3)
							EndIf
						EndIf
					EndIf
					
					n\CurrSpeed = CurveValue(0, n\CurrSpeed, 20.0)
				EndIf
				
				n\Angle = EntityYaw(n\Collider)
				;[End Block]
			Case 4 ;SCP-106/049 detected
				;[Block]
				n\Speed = 0.03
                n\State2=n\State2-FPSfactor
				If n\State2 > 0.0
					If OtherNPCSeesMeNPC(n\Target,n)
						n\State2 = 70*15
					EndIf
					
					If EntityDistance(n\Target\Collider,n\Collider)>HideDistance
						If n\State2 > 70
							n\State2 = 70
						EndIf
					EndIf
					
					If EntityDistance(n\Target\Collider,n\Collider)<3.0 And n\State3 >= 0.0
						n\State3 = 70*5
					EndIf
					
					If n\State3 > 0.0
						n\PathStatus = 0
						n\PathLocation = 0
						n\Speed = 0.02
						PointEntity n\Collider,n\Target\Collider
						RotateEntity n\Collider,0.0,EntityYaw(n\Collider,True),0.0,True
						n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
						RotateEntity n\obj,-90.0,n\Angle,0.0,True
						n\CurrSpeed = CurveValue(-n\Speed,n\CurrSpeed,20.0)
						TranslateEntity n\Collider, Cos(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, 0, Sin(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, True
						AnimateNPC(n,522, 488, n\CurrSpeed*26)
						
						n\PathTimer = 1.0
						
						n\State3=Max(n\State3-FPSfactor,0)
						
						HideEntity n\Collider
						TurnEntity n\Collider,0,180,0
						EntityPick(n\Collider, 1.0)
						If PickedEntity() <> 0 Then
							n\State3 = -70*2
						EndIf
						ShowEntity n\Collider
						TurnEntity n\Collider,0,180,0
					ElseIf n\State3 < 0.0
						n\State3 = Min(n\State3+FPSfactor,0)
					EndIf
					
					If n\PathTimer<=0.0 Then
						If n\MTFLeader<>Null Then
							n\PathStatus = FindPath(n,EntityX(n\MTFLeader\Collider,True),EntityY(n\MTFLeader\Collider,True)+0.1,EntityZ(n\MTFLeader\Collider,True))
						Else
							For r = Each Rooms
								If ((Abs(r\x-EntityX(n\Collider,True))>12.0) Or (Abs(r\z-EntityZ(n\Collider,True))>12.0)) And (Rand(1,Max(4-Int(Abs(r\z-EntityZ(n\Collider,True)/8.0)),2))=1) Then
									If EntityDistance(r\obj,n\Target\Collider)>6.0
										x = r\x
										y = 0.1
										z = r\z
										DebugLog r\RoomTemplate\Name
										Exit
									EndIf
								EndIf
							Next
							n\PathStatus = FindPath(n,x,y,z)
						EndIf
						If n\PathStatus = 1 Then
							While n\Path[n\PathLocation]=Null
								If n\PathLocation>19 Then Exit
								n\PathLocation=n\PathLocation+1
							Wend
							If n\PathLocation<19 Then
								If (n\Path[n\PathLocation]<>Null) And (n\Path[n\PathLocation+1]<>Null) Then
									If (n\Path[n\PathLocation]\door=Null) Then
										If Abs(DeltaYaw(n\Collider,n\Path[n\PathLocation]\obj))>Abs(DeltaYaw(n\Collider,n\Path[n\PathLocation+1]\obj)) Then
											n\PathLocation=n\PathLocation+1
										EndIf
									EndIf
								EndIf
							EndIf
						EndIf
						n\PathTimer = 70*10
					Else
						If n\PathStatus=1 Then
							If n\Path[n\PathLocation]=Null Then
								If n\PathLocation > 19 Then
									n\PathLocation = 0 : n\PathStatus = 0
								Else
									n\PathLocation = n\PathLocation + 1
								EndIf
							Else
								prevDist# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
								
								PointEntity n\Collider,n\Path[n\PathLocation]\obj
								RotateEntity n\Collider,0.0,EntityYaw(n\Collider,True),0.0,True
								n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
								RotateEntity n\obj,-90.0,n\Angle,0.0,True
								
								n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,20.0)
								TranslateEntity n\Collider, Cos(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, 0, Sin(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, True
								AnimateNPC(n,488, 522, n\CurrSpeed*26) ;Placeholder (until running animation has been implemented)
								
								newDist# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
								
								If (newDist<2.0 And n\Path[n\PathLocation]\door<>Null) Then
									If (Not n\Path[n\PathLocation]\door\open)
										sound = 0
										If n\Path[n\PathLocation]\door\dir = 1 Then sound = 0 Else sound=Rand(0, 2)
										PlaySound2(OpenDoorSFX(n\Path[n\PathLocation]\door\dir,sound),Camera,n\Path[n\PathLocation]\door\obj)
										PlayMTFSound(MTFSFX(5),n)
									EndIf
									n\Path[n\PathLocation]\door\open = True
									If n\Path[n\PathLocation]\door\MTFClose
										n\Path[n\PathLocation]\door\timerstate = 70.0*5.0
									EndIf
								EndIf
								
								If (newDist<0.2) Or ((prevDist<newDist) And (prevDist<1.0)) Then
									n\PathLocation=n\PathLocation+1
								EndIf
							EndIf
							n\PathTimer=n\PathTimer-FPSfactor
						Else
							n\PathTimer=0.0
						EndIf
					EndIf
				Else
					n\State = 0
				EndIf
				;[End Block]
			Case 5 ;looking at some other target than the player
				;[Block]
				target=CreatePivot()
				PositionEntity target, n\EnemyX, n\EnemyY, n\EnemyZ, True
				
				If dist<HideDistance Then
					AnimateNPC(n, 346, 351, 0.2, False)
				EndIf
				
				If Abs(EntityX(target)-EntityX(n\Collider)) < 55.0 And Abs(EntityZ(target)-EntityZ(n\Collider)) < 55.0 And Abs(EntityY(target)-EntityY(n\Collider))< 20.0 Then
					
					PointEntity n\obj, target
					RotateEntity n\Collider, 0, CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),30.0), 0, True
					
					If n\PathTimer = 0 Then
						n\PathStatus = EntityVisible(n\Collider,target)
						n\PathTimer = Rand(100,200)
					Else
						n\PathTimer = Min(n\PathTimer-FPSfactor,0.0)
					EndIf
					
					If n\PathStatus = 1 And n\Reload =< 0 Then
						dist# = Distance(EntityX(target),EntityZ(target),EntityX(n\Collider),EntityZ(n\Collider))
						
						;If dist<20.0 Then
						;	pvt = CreatePivot()
						;	
						;	PositionEntity pvt, EntityX(n\obj),EntityY(n\obj), EntityZ(n\obj)
						;	RotateEntity pvt, EntityPitch(n\Collider), EntityYaw(n\Collider),0
						;	MoveEntity (pvt,0.8*0.079, 10.75*0.079, 6.9*0.079)
						;	
						;	If WrapAngle(EntityYaw(pvt)-EntityYaw(n\Collider))<5 Then
						;		PlaySound2(GunshotSFX, Camera, n\Collider, 20)
						;		p.Particles = CreateParticle(EntityX(n\obj, True), EntityY(n\obj, True), EntityZ(n\obj, True), 1, 0.2, 0.0, 5)
						;		PositionEntity(p\pvt, EntityX(pvt), EntityY(pvt), EntityZ(pvt))
						;		
						;		n\Reload = 7
						;	EndIf
						;	
						;	FreeEntity pvt
						;EndIf
					EndIf
				EndIf		
				
				FreeEntity target
				
				n\Angle = EntityYaw(n\Collider)
				;[End Block]
			Case 6 ;seeing the player as a 049-2 instance
				;[Block]
				
				PointEntity n\obj,Collider
				RotateEntity n\Collider,0,CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),20.0),0
				n\Angle = EntityYaw(n\Collider)
				
				AnimateNPC(n, 346, 351, 0.2, False)
				
				If n\Reload =< 0 And KillTimer = 0 Then
					If EntityVisible(n\Collider, Collider) Then
						;angle# = WrapAngle(angle - EntityYaw(n\Collider))
						;If angle < 5 Or angle > 355 Then
						If (Abs(DeltaYaw(n\Collider,Collider))<50.0)
							;prev% = KillTimer
							
							PlaySound2(GunshotSFX, Camera, n\Collider, 15)
							
							pvt% = CreatePivot()
							
							RotateEntity(pvt, EntityPitch(n\Collider), EntityYaw(n\Collider), 0, True)
							PositionEntity(pvt, EntityX(n\obj), EntityY(n\obj), EntityZ(n\obj))
							MoveEntity (pvt,0.8*0.079, 10.75*0.079, 6.9*0.079)
							
							Shoot(EntityX(pvt),EntityY(pvt),EntityZ(pvt),0.9, False)
							n\Reload = 7
							
							FreeEntity(pvt)
							
							;If prev => 0 And KillTimer < 0 Then
								;DeathMSG=I_Loc\DeathMessage_Ntf
								;If n\MTFLeader = Null Then PlayMTFSound(LoadTempSound("SFX\Character\MTF\049\Player0492_2.ogg"),n)
							;EndIf
						EndIf	
					EndIf
				EndIf
				
				;[End Block]
			Case 7 ;just shooting
				;[Block]
				AnimateNPC(n, 346, 351, 0.2, False)
				
				RotateEntity n\Collider,0,CurveAngle(n\State2,EntityYaw(n\Collider),20),0
				n\Angle = EntityYaw(n\Collider)
				
				If n\Reload =< 0 
					LightVolume = TempLightVolume*1.2
					PlaySound2(GunshotSFX, Camera, n\Collider, 20)
					
					pvt% = CreatePivot()
					
					RotateEntity(pvt, EntityPitch(n\Collider), EntityYaw(n\Collider), 0, True)
					PositionEntity(pvt, EntityX(n\obj), EntityY(n\obj), EntityZ(n\obj))
					MoveEntity (pvt,0.8*0.079, 10.75*0.079, 6.9*0.079)
					
					p.Particles = CreateParticle(EntityX(pvt), EntityY(pvt), EntityZ(pvt), 1, Rnd(0.08,0.1), 0.0, 5)
					TurnEntity p\obj, 0,0,Rnd(360)
					p\Achange = -0.15
					
					FreeEntity(pvt)
					n\Reload = 7
				End If
				;[End Block]
			Case 8 ;SCP-096 spotted
				;[Block]
				n\Speed = 0.015
				n\BoneToManipulate = "head"
				n\ManipulateBone = True
				n\ManipulationType = 2
                If n\PathTimer<=0.0 Then ;update path
					If n\MTFLeader<>Null Then ;i'll follow the leader
						n\PathStatus = FindPath(n,EntityX(n\MTFLeader\Collider,True),EntityY(n\MTFLeader\Collider,True)+0.1,EntityZ(n\MTFLeader\Collider,True)) ;whatever you say boss
					Else ;i am the leader
						For r = Each Rooms
							If ((Abs(r\x-EntityX(n\Collider,True))>12.0) Or (Abs(r\z-EntityZ(n\Collider,True))>12.0)) And (Rand(1,Max(4-Int(Abs(r\z-EntityZ(n\Collider,True)/8.0)),2))=1) Then
								x = r\x
								y = 0.1
								z = r\z
								DebugLog r\RoomTemplate\Name
								Exit
							EndIf
						Next
						n\PathStatus = FindPath(n,x,y,z) ;we're going to this room for no particular reason
					EndIf
					If n\PathStatus = 1 Then
						While n\Path[n\PathLocation]=Null
							If n\PathLocation>19 Then Exit
							n\PathLocation=n\PathLocation+1
						Wend
						If n\PathLocation<19 Then
							If (n\Path[n\PathLocation]<>Null) And (n\Path[n\PathLocation+1]<>Null) Then
								If (n\Path[n\PathLocation]\door=Null) Then
									If Abs(DeltaYaw(n\Collider,n\Path[n\PathLocation]\obj))>Abs(DeltaYaw(n\Collider,n\Path[n\PathLocation+1]\obj)) Then
										n\PathLocation=n\PathLocation+1
									EndIf
								EndIf
							EndIf
						EndIf
					EndIf
					n\PathTimer = 70.0 * Rnd(6.0,10.0) ;search again after 6-10 seconds
                ElseIf (n\PathTimer<=70.0 * 2.5) And (n\MTFLeader=Null) Then
					n\PathTimer=n\PathTimer-FPSfactor
					n\CurrSpeed = 0.0
					;If Rand(1,35)=1 Then
					;	RotateEntity n\Collider,0.0,Rnd(360.0),0.0,True
					;EndIf
					FinishWalking(n,488,522,n\Speed*26)
					n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
					RotateEntity n\obj,-90.0,n\Angle,0.0,True
                Else
					If n\PathStatus=2 Then
						n\PathTimer=n\PathTimer-(FPSfactor*2.0) ;timer goes down fast
						n\CurrSpeed = 0.0
						;If Rand(1,35)=1 Then
						;	RotateEntity n\Collider,0.0,Rnd(360.0),0.0,True
						;EndIf
						FinishWalking(n,488,522,n\Speed*26)
						n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
						RotateEntity n\obj,-90.0,n\Angle,0.0,True
					ElseIf n\PathStatus=1 Then
						If n\Path[n\PathLocation]=Null Then
							If n\PathLocation > 19 Then
								n\PathLocation = 0 : n\PathStatus = 0
							Else
								n\PathLocation = n\PathLocation + 1
							EndIf
						Else
							prevDist# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
							
							PointEntity n\Collider,n\Path[n\PathLocation]\obj
							RotateEntity n\Collider,0.0,EntityYaw(n\Collider,True),0.0,True
							n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
							RotateEntity n\obj,-90.0,n\Angle,0.0,True
							
							n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,20.0)
							;MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
							TranslateEntity n\Collider, Cos(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, 0, Sin(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, True
							AnimateNPC(n,488, 522, n\CurrSpeed*26)
							
							newDist# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
							
							If (newDist<1.0 And n\Path[n\PathLocation]\door<>Null) Then
								;open the door and make it automatically close after 5 seconds
								If (Not n\Path[n\PathLocation]\door\open)
									sound = 0
									If n\Path[n\PathLocation]\door\dir = 1 Then sound = 0 Else sound=Rand(0, 2)
									PlaySound2(OpenDoorSFX(n\Path[n\PathLocation]\door\dir,sound),Camera,n\Path[n\PathLocation]\door\obj)
									PlayMTFSound(MTFSFX(5),n)
								EndIf
								n\Path[n\PathLocation]\door\open = True
								If n\Path[n\PathLocation]\door\MTFClose
									n\Path[n\PathLocation]\door\timerstate = 70.0*5.0
								EndIf
							EndIf
                            
							If (newDist<0.2) Or ((prevDist<newDist) And (prevDist<1.0)) Then
								n\PathLocation=n\PathLocation+1
							EndIf
						EndIf
						n\PathTimer=n\PathTimer-FPSfactor ;timer goes down slow
					Else
						n\PathTimer=n\PathTimer-(FPSfactor*2.0) ;timer goes down fast
						If n\MTFLeader = Null Then
							;If Rand(1,35)=1 Then
							;	RotateEntity n\Collider,0.0,Rnd(360.0),0.0,True
							;EndIf
							FinishWalking(n,488,522,n\Speed*26)
							n\CurrSpeed = 0.0
						ElseIf EntityDistance(n\Collider,n\MTFLeader\Collider)>1.0 Then
							PointEntity n\Collider,n\MTFLeader\Collider
							RotateEntity n\Collider,0.0,EntityYaw(n\Collider,True),0.0,True
							
							n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,20.0)
							TranslateEntity n\Collider, Cos(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, 0, Sin(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, True
							AnimateNPC(n,488, 522, n\CurrSpeed*26)
						Else
							;If Rand(1,35)=1 Then
							;	RotateEntity n\Collider,0.0,Rnd(360.0),0.0,True
							;EndIf
							FinishWalking(n,488,522,n\Speed*26)
							n\CurrSpeed = 0.0
						EndIf
						n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
						RotateEntity n\obj,-90.0,n\Angle,0.0,True
					EndIf
                EndIf
				
				If (Not EntityVisible(n\Collider,Curr096\Collider)) Or EntityDistance(n\Collider,Curr096\Collider)>6.0
					n\State = 0
				EndIf
				;[End Block]
			Case 9 ;SCP-049-2/008 spotted
				;[Block]
				If EntityVisible(n\Collider, n\Target\Collider) Then
					PointEntity n\obj,n\Target\Collider
					RotateEntity n\Collider,0,CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),20.0),0
					n\Angle = EntityYaw(n\Collider)
					
					If EntityDistance(n\Target\Collider,n\Collider)<1.3
						n\State3 = 70*2
					EndIf
					
					If n\State3 > 0.0
						n\PathStatus = 0
						n\PathLocation = 0
						n\Speed = 0.02
						n\CurrSpeed = CurveValue(-n\Speed,n\CurrSpeed,20.0)
						TranslateEntity n\Collider, Cos(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, 0, Sin(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, True
						AnimateNPC(n,522, 488, n\CurrSpeed*26)
						
						n\PathTimer = 1.0
						
						n\State3=Max(n\State3-FPSfactor,0)
					Else
						n\State3 = 0
						AnimateNPC(n, 346, 351, 0.2, False)
					EndIf
					If n\Reload =< 0 And n\Target\IsDead = False Then
						;angle# = WrapAngle(angle - EntityYaw(n\Collider))
						;If angle < 5 Or angle > 355 Then
						If (Abs(DeltaYaw(n\Collider,n\Target\Collider))<50.0)
							;prev% = KillTimer
							
							PlaySound2(GunshotSFX, Camera, n\Collider, 15)
							
							pvt% = CreatePivot()
							
							RotateEntity(pvt, EntityPitch(n\Collider), EntityYaw(n\Collider), 0, True)
							PositionEntity(pvt, EntityX(n\obj), EntityY(n\obj), EntityZ(n\obj))
							MoveEntity (pvt,0.8*0.079, 10.75*0.079, 6.9*0.079)
							
							p.Particles = CreateParticle(EntityX(pvt), EntityY(pvt), EntityZ(pvt), 1, Rnd(0.08,0.1), 0.0, 5)
							TurnEntity p\obj, 0,0,Rnd(360)
							p\Achange = -0.15
							If n\Target\HP > 0
								n\Target\HP = Max(n\Target\HP-Rand(5,10),0)
							Else
								If (Not n\Target\IsDead)
									If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
									If n\Target\NPCtype = NPCtypeZombie
										n\Sound = LoadSound_Strict("SFX\Character\MTF\049\Player0492_2.ogg")
										PlayMTFSound(n\Sound, n)
									Else
										;Still needs to be added! (for 008)
									EndIf
								EndIf
								SetNPCFrame(n\Target,133)
								n\Target\IsDead = True
								n\Target = Null
								n\State = 0
								Return
							EndIf
							n\Reload = 7
							
							FreeEntity(pvt)
						EndIf	
					EndIf
					n\PathStatus = 0
				Else
					If n\PathTimer<=0.0 Then
						n\PathStatus = FindPath(n,EntityX(n\Target\Collider),EntityY(n\Target\Collider),EntityZ(n\Target\Collider))
						If n\PathStatus = 1 Then
							While n\Path[n\PathLocation]=Null
								If n\PathLocation>19 Then Exit
								n\PathLocation=n\PathLocation+1
							Wend
							If n\PathLocation<19 Then
								If (n\Path[n\PathLocation]<>Null) And (n\Path[n\PathLocation+1]<>Null) Then
									If (n\Path[n\PathLocation]\door=Null) Then
										If Abs(DeltaYaw(n\Collider,n\Path[n\PathLocation]\obj))>Abs(DeltaYaw(n\Collider,n\Path[n\PathLocation+1]\obj)) Then
											n\PathLocation=n\PathLocation+1
										EndIf
									EndIf
								EndIf
							EndIf
						EndIf
						n\PathTimer = 70*10
					Else
						If n\PathStatus=1 Then
							If n\Path[n\PathLocation]=Null Then
								If n\PathLocation > 19 Then
									n\PathLocation = 0 : n\PathStatus = 0
								Else
									n\PathLocation = n\PathLocation + 1
								EndIf
							Else
								prevDist# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
								
								PointEntity n\Collider,n\Path[n\PathLocation]\obj
								RotateEntity n\Collider,0.0,EntityYaw(n\Collider,True),0.0,True
								n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
								RotateEntity n\obj,-90.0,n\Angle,0.0,True
								
								n\CurrSpeed = CurveValue(n\Speed,n\CurrSpeed,20.0)
								TranslateEntity n\Collider, Cos(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, 0, Sin(EntityYaw(n\Collider,True)+90.0)*n\CurrSpeed * FPSfactor, True
								AnimateNPC(n,488, 522, n\CurrSpeed*26)
								
								newDist# = EntityDistance(n\Collider,n\Path[n\PathLocation]\obj)
								
								If (newDist<1.0 And n\Path[n\PathLocation]\door<>Null) Then
									If (Not n\Path[n\PathLocation]\door\open)
										sound = 0
										If n\Path[n\PathLocation]\door\dir = 1 Then sound = 0 Else sound=Rand(0, 2)
										PlaySound2(OpenDoorSFX(n\Path[n\PathLocation]\door\dir,sound),Camera,n\Path[n\PathLocation]\door\obj)
										PlayMTFSound(MTFSFX(5),n)
									EndIf
									n\Path[n\PathLocation]\door\open = True
									If n\Path[n\PathLocation]\door\MTFClose
										n\Path[n\PathLocation]\door\timerstate = 70.0*5.0
									EndIf
								EndIf
								
								If (newDist<0.2) Or ((prevDist<newDist) And (prevDist<1.0)) Then
									n\PathLocation=n\PathLocation+1
								EndIf
							EndIf
							n\PathTimer=n\PathTimer-FPSfactor
						Else
							n\PathTimer=0.0
						EndIf
					EndIf
				EndIf
				
				If n\Target\IsDead = True Then
					n\Target = Null
					n\State = 0
				EndIf
				
				;[End Block]
		End Select
		
		If n\CurrSpeed > 0.01 Then
			If prevFrame > 500 And n\Frame<495
				PlaySound2(StepSFX(2,0,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.5,0.7))
			ElseIf prevFrame < 505 And n\Frame=>505
				PlaySound2(StepSFX(2,0,Rand(0,2)),Camera, n\Collider, 8.0, Rnd(0.5,0.7))
			EndIf
		EndIf
		
		If NoTarget And n\State = 1 Then n\State = 0
		
		If n\State <> 3 And n\State <> 5 And n\State <> 6 And n\State <> 7
			If n\MTFLeader<>Null Then
				If EntityDistance(n\Collider,n\MTFLeader\Collider)<0.7 Then
					PointEntity n\Collider,n\MTFLeader\Collider
					RotateEntity n\Collider,0.0,EntityYaw(n\Collider,True),0.0,True
					n\Angle = CurveAngle(EntityYaw(n\Collider,True),n\Angle,20.0)
					
					TranslateEntity n\Collider, Cos(EntityYaw(n\Collider,True)-45)* 0.01 * FPSfactor, 0, Sin(EntityYaw(n\Collider,True)-45)* 0.01 * FPSfactor, True
				EndIf
			Else
				For n2.NPCs = Each NPCs
					If n2<>n And n2\IsDead=False Then
						If Abs(DeltaYaw(n\Collider,n2\Collider))<80.0 Then
							If EntityDistance(n\Collider,n2\Collider)<0.7 Then							
								TranslateEntity n2\Collider, Cos(EntityYaw(n\Collider,True)+90)* 0.01 * FPSfactor, 0, Sin(EntityYaw(n\Collider,True)+90)* 0.01 * FPSfactor, True
							EndIf
						EndIf
					EndIf
				Next
			EndIf
		EndIf
		
		;teleport back to the facility if fell through the floor
		If n\State <> 6 And n\State <> 7
			If (EntityY(n\Collider) < -10.0) Then
				TeleportCloser(n)
			EndIf
		EndIf
		
		RotateEntity n\obj,-90.0,n\Angle,0.0,True
		
		PositionEntity n\obj,EntityX(n\Collider,True),EntityY(n\Collider,True)-0.15,EntityZ(n\Collider,True),True
		
	EndIf
End Function

Function Shoot(x#, y#, z#, hitProb# = 1.0, particles% = True, instaKill% = False)
	
	;muzzle flash
	Local p.Particles = CreateParticle(x,y,z, 1, Rnd(0.08,0.1), 0.0, 5)
	TurnEntity p\obj, 0,0,Rnd(360)
	p\Achange = -0.15
	
	LightVolume = TempLightVolume*1.2
	
	If (Not GodMode) Then 
		
		If instaKill Then Kill() : PlaySound_Strict BullethitSFX : Return
		
		If Rnd(1.0) =< hitProb Then
			TurnEntity Camera, Rnd(-3,3), Rnd(-3,3), 0
			
			Local ShotMessageUpdate$
			If WearingVest>0 Then
				If WearingVest = 1 Then
					Select Rand(8)
						Case 1,2,3,4,5
							BlurTimer = 500
							Stamina = 0
							ShotMessageUpdate = I_Loc\MessageShot_ChestVest
							Injuries = Injuries + Rnd(0.1,0.5)
						Case 6
							BlurTimer = 500
							ShotMessageUpdate = I_Loc\MessageShot_Leftleg
							Injuries = Injuries + Rnd(0.8,1.2)
						Case 7
							BlurTimer = 500
							ShotMessageUpdate = I_Loc\MessageShot_Rightleg
							Injuries = Injuries + Rnd(0.8,1.2)
						Case 8
							BlurTimer = 500
							Stamina = 0
							ShotMessageUpdate = I_Loc\MessageShot_Neck
							Injuries = Injuries + Rnd(1.2,1.6)
					End Select	
				Else
					If Rand(10)=1 Then
						BlurTimer = 500
						Stamina = Stamina - 1
						ShotMessageUpdate = I_Loc\MessageShot_ChestFinevestHeavy
						Injuries = Injuries + Rnd(0.8,1.1)
					Else
						ShotMessageUpdate = I_Loc\MessageShot_ChestFinevestLight
						Injuries = Injuries + Rnd(0.1,0.5)
					EndIf
				EndIf
				
				If Injuries >= 5
					If Rand(3) = 1 Then Kill()
				EndIf
			Else
				Select Rand(6)
					Case 1
						Kill()
					Case 2
						BlurTimer = 500
						ShotMessageUpdate = I_Loc\MessageShot_Leftleg
						Injuries = Injuries + Rnd(0.8,1.2)
					Case 3
						BlurTimer = 500
						ShotMessageUpdate = I_Loc\MessageShot_Rightleg
						Injuries = Injuries + Rnd(0.8,1.2)
					Case 4
						BlurTimer = 500
						ShotMessageUpdate = I_Loc\MessageShot_Rightshoulder
						Injuries = Injuries + Rnd(0.8,1.2)	
					Case 5
						BlurTimer = 500
						ShotMessageUpdate = I_Loc\MessageShot_Leftshoulder
						Injuries = Injuries + Rnd(0.8,1.2)	
					Case 6
						BlurTimer = 500
						ShotMessageUpdate = I_Loc\MessageShot_Chest
						Injuries = Injuries + Rnd(2.5,4.0)
				End Select
			EndIf
			
			;Only updates the message if it's been more than two seconds.
			If (MsgTimer < 64*4) Then
				Msg = ShotMessageUpdate
				MsgTimer = 70*6
			EndIf

			Injuries = Min(Injuries, 4.0)
			
			;Kill()
			PlaySound_Strict BullethitSFX
		ElseIf particles And ParticleAmount>0
			pvt = CreatePivot()
			PositionEntity pvt, EntityX(Collider),(EntityY(Collider)+EntityY(Camera))/2,EntityZ(Collider)
			PointEntity pvt, p\obj
			TurnEntity pvt, 0, 180, 0
			
			EntityPick(pvt, 2.5)
			
			If PickedEntity() <> 0 Then 
				PlaySound2(Gunshot3SFX, Camera, pvt, 0.4, Rnd(0.8,1.0))
				
				If particles Then 
					;dust/smoke particles
					p.Particles = CreateParticle(PickedX(),PickedY(),PickedZ(), 0, 0.03, 0, 80)
					p\speed = 0.001
					p\SizeChange = 0.003
					p\A = 0.8
					p\Achange = -0.01
					RotateEntity p\pvt, EntityPitch(pvt)-180, EntityYaw(pvt),0
					
					For i = 0 To Rand(2,3)
						p.Particles = CreateParticle(PickedX(),PickedY(),PickedZ(), 0, 0.006, 0.003, 80)
						p\speed = 0.02
						p\A = 0.8
						p\Achange = -0.01
						RotateEntity p\pvt, EntityPitch(pvt)+Rnd(170,190), EntityYaw(pvt)+Rnd(-10,10),0	
					Next
					
					;bullet hole decal
					Local de.Decals = CreateDecal(Rand(13,14), PickedX(),PickedY(),PickedZ(), 0,0,0)
					AlignToVector de\obj,-PickedNX(),-PickedNY(),-PickedNZ(),3
					MoveEntity de\obj, 0,0,-0.001
					EntityFX de\obj, 1
					de\lifetime = 70*20
					EntityBlend de\obj, 2
					de\Size = Rnd(0.028,0.034)
					ScaleSprite de\obj, de\Size, de\Size
				EndIf				
			EndIf
			FreeEntity pvt
			
		EndIf
		
	EndIf
	
End Function

Function PlayMTFSound(sound%, n.NPCs)
	If n <> Null Then
		n\SoundChn = PlaySound2(sound, Camera, n\Collider, 8.0)	
	EndIf
	
	If SelectedItem <> Null Then
		If SelectedItem\state2 = 3 And SelectedItem\state > 0 Then 
			Select SelectedItem\itemtemplate\name 
				Case "radio","fineradio","18vradio"
					If sound<>MTFSFX(5) Or (Not ChannelPlaying(RadioCHN(3)))
						If RadioCHN(3)<> 0 Then StopChannel RadioCHN(3)
						RadioCHN(3) = PlaySound_Strict (sound)
					EndIf
			End Select
		EndIf
	EndIf 
End Function

Function MoveToPocketDimension()
	Local r.Rooms
	
	For r.Rooms = Each Rooms
		If r\RoomTemplate\Name = "pocketdimension" Then
			FallTimer = 0
			UpdateDoors()
			UpdateRooms()
			ShowEntity Collider
			PlaySound_Strict(Use914SFX)
			PlaySound_Strict(OldManSFX(5))
			PositionEntity(Collider, EntityX(r\obj),0.8,EntityZ(r\obj))
			DropSpeed = 0
			ResetEntity Collider
			
			BlinkTimer = -10
			
			Injuries = Injuries+0.5
			
			PlayerRoom = r
			
			Return
		EndIf
	Next		
End Function

Function FindFreeNPCID%()
	Local id% = 1
	While (True)
		Local taken% = False
		For n2.NPCs = Each NPCs
			If n2\ID = id Then
				taken = True
				Exit
			EndIf
		Next
		If (Not taken) Then
			Return id
		EndIf
		id = id + 1
	Wend
End Function

Function ForceSetNPCID(n.NPCs, newID%)
	n\ID = newID
	
	For n2.NPCs = Each NPCs
		If n2 <> n And n2\ID = newID Then
			n2\id = FindFreeNPCID()
		EndIf
	Next
End Function

Function Find860Angle(n.NPCs, fr.Forest)
	TFormPoint(EntityX(Collider),EntityY(Collider),EntityZ(Collider),0,fr\Forest_Pivot)
	Local playerx = Floor((TFormedX()+6.0)/12.0)
	Local playerz = Floor((TFormedZ()+6.0)/12.0)
	
	TFormPoint(EntityX(n\Collider),EntityY(n\Collider),EntityZ(n\Collider),0,fr\Forest_Pivot)
	Local x# = (TFormedX()+6.0)/12.0
	Local z# = (TFormedZ()+6.0)/12.0
	
	Local xt = Floor(x), zt = Floor(z)
	
	Local x2,z2
	If xt<>playerx Or zt<>playerz Then ;the monster is not on the same tile as the player
		For x2 = Max(xt-1,0) To Min(xt+1,gridsize-1)
			For z2 = Max(zt-1,0) To Min(zt+1,gridsize-1)
				If fr\grid[(z2*gridsize)+x2]>0 And (x2<>xt Or z2<>zt) And (x2=xt Or z2=zt) Then
					
					;tile (x2,z2) is closer to the player than the monsters current tile
					If (Abs(playerx-x2)+Abs(playerz-z2))<(Abs(playerx-xt)+Abs(playerz-zt)) Then
						;calculate the position of the tile in world coordinates
						TFormPoint(x2*12.0,0,z2*12.0,fr\Forest_Pivot,0)
						
						Return point_direction(EntityX(n\Collider),EntityZ(n\Collider),TFormedX(),TFormedZ())+180
					EndIf
					
				EndIf
			Next
		Next
	Else
		Return point_direction(EntityX(n\Collider),EntityZ(n\Collider),EntityX(Collider),EntityZ(Collider))+180
	EndIf		
End Function

Function ConsoleNPCPlayerVisible%(n.NPCs, maxDist#)
	Local dist# = EntityDistance(n\Collider, Collider)
	
	If dist > maxDist Then Return False
	If (Not EntityVisible(n\Collider, Collider)) Then Return False
	
	Return True
End Function

Function ConsoleNPCPlayerClearShot%(n.NPCs, maxDist#)
	Local dist# = EntityDistance(n\Collider, Collider)
	Local pvt%
	Local clearShot%
	
	If dist > maxDist Then Return False
	
	If EntityVisible(n\Collider, Collider) Then Return True
	
	pvt = CreatePivot()
	PositionEntity(pvt, EntityX(n\Collider), EntityY(n\Collider) + 0.8, EntityZ(n\Collider))
	PointEntity(pvt, Collider)
	EntityPick(pvt, dist + 0.25)
	clearShot = (PickedEntity() = Collider)
	FreeEntity(pvt)
	
	Return clearShot
End Function

Function TurnConsoleNPCToPlayer(n.NPCs, smooth# = 20.0)
	Local pvt% = CreatePivot()
	
	PositionEntity(pvt, EntityX(n\Collider), EntityY(n\Collider), EntityZ(n\Collider))
	PointEntity(pvt, Collider)
	RotateEntity(n\Collider, 0, CurveAngle(EntityYaw(pvt), EntityYaw(n\Collider), smooth), 0, True)
	FreeEntity(pvt)
End Function

Function ConsoleNPCMapPickY#(x#, startY#, z#, yDelta#, fallback#)
	Local pick%
	Local i%
	Local curY#
	Local endY#
	Local remaining#
	
	curY = startY
	endY = startY + yDelta
	remaining = yDelta
	
	For i = 0 To 8
		If Abs(remaining) < 0.05 Then Exit
		pick = LinePick(x, curY, z, 0, remaining, 0)
		If pick <> 0 Then
			If GetEntityType(pick) = HIT_MAP Then Return PickedY()
			If yDelta < 0 Then
				curY = PickedY() - 0.25
			Else
				curY = PickedY() + 0.25
			EndIf
			remaining = endY - curY
		Else
			Exit
		EndIf
	Next
	
	Return fallback
End Function

Function ConsoleNPCFindFloorY#(x#, startY#, z#, fallback#)
	Return ConsoleNPCMapPickY(x, startY, z, -30.0, fallback)
End Function

Function ConsoleNPCFindCeilingY#(x#, startY#, z#, fallback#)
	Return ConsoleNPCMapPickY(x, startY, z, 12.0, fallback)
End Function


Function ConsoleTeslaRoom%(r.Rooms)
	Local rn$
	
	If r = Null Then Return False
	If r\RoomTemplate = Null Then Return False
	
	rn = Lower(r\RoomTemplate\Name)
	If Instr(rn, "room2tesla") > 0 Then Return True
	
	Return False
End Function

Function ConsoleTeslaRoomDisabled%(r.Rooms)
	Local e.Events
	
	If r = Null Then Return False
	
	For e.Events = Each Events
		If e\room = r Then
			If e\EventName = "room2tesla" Then
				If e\EventState2 > 70 * 3.5 And e\EventState2 < 70 * 90 Then Return True
			EndIf
		EndIf
	Next
	
	Return False
End Function

Function ConsoleClerkEnableTeslaRoom(r.Rooms)
	Local e.Events
	
	If r = Null Then Return
	
	For e.Events = Each Events
		If e\room = r Then
			If e\EventName = "room2tesla" Then
				e\EventState2 = 0
				e\EventState3 = 0
			EndIf
		EndIf
	Next
End Function

Function ConsoleClerkPathScore#(n.NPCs, targetX#, targetZ#)
	Local i%
	Local score#
	Local lastX#
	Local lastZ#
	Local wpX#
	Local wpZ#
	
	lastX = EntityX(n\Collider, True)
	lastZ = EntityZ(n\Collider, True)
	
	For i = 0 To 19
		If n\Path[i] <> Null Then
			wpX = EntityX(n\Path[i]\obj, True)
			wpZ = EntityZ(n\Path[i]\obj, True)
			score = score + Distance(lastX, lastZ, wpX, wpZ)
			lastX = wpX
			lastZ = wpZ
		EndIf
	Next
	
	score = score + Distance(lastX, lastZ, targetX, targetZ)
	Return score
End Function

Function FindNearestConsoleTeslaRoom.Rooms(n.NPCs)
	Local r.Rooms
	Local bestEnabled.Rooms
	Local bestDisabled.Rooms
	Local fallbackEnabled.Rooms
	Local fallbackDisabled.Rooms
	Local dist#
	Local fallbackEnabledDist# = 1000000.0
	Local fallbackDisabledDist# = 1000000.0
	Local bestEnabledDist# = 1000000.0
	Local bestDisabledDist# = 1000000.0
	Local target%
	Local pathStatus%
	Local pathScore#
	Local foundEnabledPath%
	Local foundDisabledPath%
	Local disabled%
	Local i%
	Local targetX#
	Local targetY#
	Local targetZ#
	
	For r.Rooms = Each Rooms
		If ConsoleTeslaRoom(r) Then
			target = r\Objects[2]
			If target = 0 Then target = r\obj
			If target <> 0 Then
				targetX = EntityX(target, True)
				targetY = EntityY(target, True)
				targetZ = EntityZ(target, True)
				dist = EntityDistance(n\Collider, target)
				disabled = ConsoleTeslaRoomDisabled(r)
				
				If disabled Then
					If dist < fallbackDisabledDist Then
						fallbackDisabledDist = dist
						fallbackDisabled = r
					EndIf
				Else
					If dist < fallbackEnabledDist Then
						fallbackEnabledDist = dist
						fallbackEnabled = r
					EndIf
				EndIf
				
				pathStatus = FindPath(n, targetX, targetY + 0.1, targetZ)
				If pathStatus = 1 Then
					pathScore = ConsoleClerkPathScore(n, targetX, targetZ)
					If disabled Then
						If (Not foundDisabledPath) Or pathScore < bestDisabledDist Then
							bestDisabledDist = pathScore
							bestDisabled = r
							foundDisabledPath = True
						EndIf
					Else
						If (Not foundEnabledPath) Or pathScore < bestEnabledDist Then
							bestEnabledDist = pathScore
							bestEnabled = r
							foundEnabledPath = True
						EndIf
					EndIf
				EndIf
			EndIf
		EndIf
	Next
	
	n\PathStatus = 0
	n\PathLocation = 0
	For i = 0 To 19
		n\Path[i] = Null
	Next
	
	If foundEnabledPath Then Return bestEnabled
	If foundDisabledPath Then Return bestDisabled
	If fallbackEnabled <> Null Then Return fallbackEnabled
	Return fallbackDisabled
End Function



Function ConsoleClerkSkipPathJitter(n.NPCs)
	Local i%
	Local nextIndex%
	Local curr.WayPoints
	Local nextp.WayPoints
	Local distCurr#
	Local distNext#
	Local cx#
	Local cz#
	
	While n\PathLocation <= 19
		curr = n\Path[n\PathLocation]
		If curr = Null Then
			n\PathLocation = n\PathLocation + 1
		Else
			cx = EntityX(n\Collider, True)
			cz = EntityZ(n\Collider, True)
			distCurr = Distance(cx, cz, EntityX(curr\obj, True), EntityZ(curr\obj, True))
			
			If distCurr < 0.35 Then
				n\PathLocation = n\PathLocation + 1
			Else
				nextp = Null
				nextIndex = -1
				For i = n\PathLocation + 1 To 19
					If n\Path[i] <> Null Then
						nextp = n\Path[i]
						nextIndex = i
						Exit
					EndIf
				Next
				
				If nextp <> Null Then
					distNext = Distance(cx, cz, EntityX(nextp\obj, True), EntityZ(nextp\obj, True))
					If distNext + 0.15 < distCurr Then
						If EntityVisible(n\Collider, nextp\obj) Then
							n\PathLocation = nextIndex
						Else
							Return
						EndIf
					Else
						Return
					EndIf
				Else
					Return
				EndIf
			EndIf
		EndIf
	Wend
End Function

Function ConsoleHumanHasBlockingNPC%(n.NPCs, checkDist#)
	Local n2.NPCs
	
	For n2.NPCs = Each NPCs
		If n2 <> n Then
			If n2\Collider <> 0 Then
				If (Not n2\IsDead) Then
					If EntityDistance(n\Collider, n2\Collider) < checkDist Then
						If Abs(DeltaYaw(n\Collider, n2\Collider)) < 55.0 Then Return True
					EndIf
				EndIf
			EndIf
		EndIf
	Next
	
	Return False
End Function

Function D9341AvoidBlockingNPC%(n.NPCs, checkDist#)
	Local n2.NPCs
	Local yaw#
	Local sidestep#
	
	For n2.NPCs = Each NPCs
		If n2 <> n Then
			If n2\NPCtype <> NPCtype1048a Then
				If n2\Collider <> 0 Then
					If (Not n2\IsDead) Then
						If EntityDistance(n\Collider, n2\Collider) < checkDist Then
							yaw = DeltaYaw(n\Collider, n2\Collider)
							If Abs(yaw) < 65.0 Then
								sidestep = 0.014
								If yaw < 0 Then
									MoveEntity n\Collider, sidestep * FPSfactor, 0, 0
								Else
									MoveEntity n\Collider, -sidestep * FPSfactor, 0, 0
								EndIf
								If n2\NPCtype <> NPCtype049 Then
									n\PathStatus = 0
									n\PathTimer = 0
									n\PathLocation = 0
								EndIf
								Return True
							EndIf
						EndIf
					EndIf
				EndIf
			EndIf
		EndIf
	Next
	
	Return False
End Function

Function D93411048CenterEntity%()
	Local n2.NPCs
	Local e.Events
	
	For n2.NPCs = Each NPCs
		If n2\NPCtype = NPCtype1048a Then
			If n2\Collider <> 0 And n2\IsDead = False Then Return n2\Collider
		EndIf
	Next
	For e.Events = Each Events
		If Lower(e\EventName) = "1048a" Then
			If e\room <> Null Then Return e\room\obj
		EndIf
	Next
	Return 0
End Function

Function D9341StartClickFollow%(n.NPCs)
	If n = Null Then Return False
	If MouseHit1 = False Then Return False
	If Collider = 0 Then Return False
	If EntityDistance(n\Collider, Collider) > 1.15 Then Return False
	
	If n\State = 190 Then
		n\FollowTimer = 0
		n\State = 191
		n\State2 = 0
		n\State3 = 0
		n\CurrSpeed = 0
		n\WanderTimer = 0
		D9341ClearPath(n)
		Return True
	EndIf
	
	If n\State <> 101 And n\State <> 180 Then Return False
	n\Idle = 101
	n\FollowTimer = 70 * 5.4
	n\FollowX = EntityX(Collider, True)
	n\FollowZ = EntityZ(Collider, True)
	n\EnemyX = n\FollowX
	n\EnemyY = EntityY(Collider, True)
	n\EnemyZ = n\FollowZ
	n\WanderTimer = 0
	n\GoalTimer = 0
	D9341ClearPath(n)
	n\PathX = 0
	n\PathZ = 0
	n\State = 190
	n\State2 = 0
	n\State3 = 0
	n\CurrSpeed = 0
	Return True
End Function

Function D9341NudgeTowardPoint(n.NPCs, x#, z#, amount#)
	Local dx#
	Local dz#
	Local dist#
	Local oldYaw#
	Local targetPvt%
	Local lookPvt%
	
	If n = Null Then Return
	If amount <= 0 Then Return
	If amount > 0.09 Then amount = 0.09
	dx = x - EntityX(n\Collider, True)
	dz = z - EntityZ(n\Collider, True)
	dist = Sqr(dx * dx + dz * dz)
	If dist < 0.05 Then Return
	
	oldYaw = EntityYaw(n\Collider, True)
	targetPvt = CreatePivot()
	lookPvt = CreatePivot()
	PositionEntity targetPvt, x, EntityY(n\Collider, True), z, True
	PositionEntity lookPvt, EntityX(n\Collider, True), EntityY(n\Collider, True), EntityZ(n\Collider, True), True
	PointEntity lookPvt, targetPvt
	RotateEntity n\Collider, 0, EntityYaw(lookPvt, True), 0, True
	MoveEntity n\Collider, 0, 0, amount
	RotateEntity n\Collider, 0, oldYaw, 0, True
	FreeEntity lookPvt
	FreeEntity targetPvt
End Function

Function ConsoleClerkForceDoorOpen(d.Doors)
	If d = Null Then Return
	If d\obj = 0 Then Return
	If d\IsElevatorDoor > 0 Then Return
	
	d\dist = 0
	If d\fastopen < 1 Then d\fastopen = 1
	
	d\locked = False
	If d\KeyCard > 0 Then d\KeyCard = 0
	
	If d\LinkedDoor <> Null Then
		d\LinkedDoor\dist = 0
		If d\LinkedDoor\fastopen < 1 Then d\LinkedDoor\fastopen = 1
		d\LinkedDoor\locked = False
		If d\LinkedDoor\KeyCard > 0 Then d\LinkedDoor\KeyCard = 0
	EndIf
	
	If d\open = False Then
		d\open = True
		d\timerstate = d\timer
		If d\LinkedDoor <> Null Then
			d\LinkedDoor\open = True
			d\LinkedDoor\timerstate = d\LinkedDoor\timer
		EndIf
	EndIf
End Function

Function ConsoleClerkDoorReady%(d.Doors)
	If d = Null Then Return True
	If d\open = False Then Return False
	If d\openstate < 150.0 Then Return False
	If d\LinkedDoor <> Null Then
		If d\LinkedDoor\open = False Then Return False
		If d\LinkedDoor\openstate < 150.0 Then Return False
	EndIf
	Return True
End Function

Function ConsoleClerkOpenNearbyDoors(n.NPCs)
	Local d.Doors
	
	For d.Doors = Each Doors
		If d\obj <> 0 Then
			If EntityDistance(n\Collider, d\obj) < 1.85 Then ConsoleClerkForceDoorOpen(d)
		EndIf
	Next
End Function

Function ConsoleClerkNullifyTeslaClerkEvent(r.Rooms, n.NPCs)
	Local e.Events
	
	If r = Null Then Return
	
	ConsoleClerkEnableTeslaRoom(r)
	
	For e.Events = Each Events
		If e\EventName = "room2tesla" And e\room = r Then
			If e\room\NPC[0] <> Null Then
				If e\room\NPC[0] <> n Then
					If e\room\NPC[0]\NPCtype = NPCtypeClerk And (Not e\room\NPC[0]\IsDead) Then
						RemoveNPC(e\room\NPC[0])
					EndIf
				EndIf
				e\room\NPC[0] = Null
			EndIf
			e\EventStr = "done"
			Exit
		EndIf
	Next
End Function

Function ConsoleClerkTriggerNearestTesla(n.NPCs)
	Local r.Rooms
	Local e.Events
	Local target%
	Local dist#
	Local bestDist# = 1000000.0
	Local best.Rooms
	
	For r.Rooms = Each Rooms
		If ConsoleTeslaRoom(r) Then
			target = r\Objects[2]
			If target = 0 Then target = r\obj
			If target <> 0 Then
				dist = EntityDistance(n\Collider, target)
				If dist < bestDist Then
					bestDist = dist
					best = r
				EndIf
			EndIf
		EndIf
	Next
	
	If best <> Null Then
		ConsoleClerkEnableTeslaRoom(best)
		If best\Objects[3] <> 0 Then
			PlaySound2(TeslaActivateSFX, Camera, best\Objects[3], 4.0, 0.5)
			HideEntity best\Objects[4]
		Else
			PlaySound_Strict TeslaActivateSFX
		EndIf
		
		For e.Events = Each Events
			If e\room = best Then
				If e\EventName = "room2tesla" Then
					e\EventState2 = 0
					e\EventState3 = 0
					e\EventState = 1
					Exit
				EndIf
			EndIf
		Next
	EndIf
End Function

Function UpdateClerkTeslaNPC(n.NPCs)
	Local r.Rooms
	Local d.Doors
	Local dist#
	Local dist2#
	Local prevFrame#
	Local blocked%
	Local targetPvt%
	Local lookPvt%
	Local targetSpeed#
	Local turnDelta#
	Local wpX#
	Local wpZ#
	Local prevDist#
	Local newDist#
	Local pathDoor.Doors
	Local doorDist#
	Local doorWait%
	
	RotateEntity(n\Collider, 0, EntityYaw(n\Collider), EntityRoll(n\Collider), True)
	prevFrame = AnimTime(n\obj)
	
	If n\State = 101 Or n\State = 102 Then ConsoleClerkOpenNearbyDoors(n)
	
	Select n\State
		Case 100
			r = FindNearestConsoleTeslaRoom(n)
			If r = Null Then
				n\State = 0
				n\CurrSpeed = CurveValue(0.0, n\CurrSpeed, 5.0)
				Animate2(n\obj, AnimTime(n\obj), 210, 235, 0.1)
			Else
				If r\Objects[2] <> 0 Then
					n\EnemyX = EntityX(r\Objects[2], True)
					n\EnemyY = EntityY(r\Objects[2], True)
					n\EnemyZ = EntityZ(r\Objects[2], True)
				Else
					n\EnemyX = EntityX(r\obj, True)
					n\EnemyY = EntityY(r\obj, True)
					n\EnemyZ = EntityZ(r\obj, True)
				EndIf
				
				; Prevent the chosen tesla room from spawning its normal scripted clerk.
				ConsoleClerkNullifyTeslaClerkEvent(r, n)
				
				n\PathStatus = 0
				n\PathTimer = 0
				n\PathLocation = 0
				n\State2 = 0
				n\State3 = 0
				n\PrevX = EntityX(n\Collider, True)
				n\PrevZ = EntityZ(n\Collider, True)
				n\State = 101
			EndIf
		Case 101
			; Run through the waypoint path until the clerk is close enough to make a direct dash into the tesla field.
			dist = Distance(EntityX(n\Collider), EntityZ(n\Collider), n\EnemyX, n\EnemyZ)
			
			If dist < 1.95 Then
				n\State = 102
				n\State2 = 0
				n\State3 = 0
				n\PathStatus = 0
				n\PathLocation = 0
			Else
				If n\PathStatus <> 1 Then
					n\PathStatus = FindPath(n, n\EnemyX, n\EnemyY + 0.1, n\EnemyZ)
					n\PathTimer = 0
				EndIf
				
				If n\PathStatus = 0 Then
					n\State = 102
					n\State2 = 0
					n\State3 = 0
					n\PathLocation = 0
				ElseIf n\PathStatus = 1 Then
					While n\PathLocation <= 19
						If n\Path[n\PathLocation] = Null Then
							n\PathLocation = n\PathLocation + 1
						Else
							Exit
						EndIf
					Wend
					
					If n\PathLocation > 19 Then
						n\State = 102
						n\State2 = 0
						n\State3 = 0
					Else
						wpX = EntityX(n\Path[n\PathLocation]\obj, True)
						wpZ = EntityZ(n\Path[n\PathLocation]\obj, True)
						doorWait = False
						
						pathDoor = n\Path[n\PathLocation]\door
						If n\PathLocation > 0 Then
							If n\Path[n\PathLocation - 1] <> Null Then
								d = n\Path[n\PathLocation - 1]\door
								If d <> Null Then ConsoleClerkForceDoorOpen(d)
							EndIf
						EndIf
						
						If pathDoor <> Null Then
							ConsoleClerkForceDoorOpen(pathDoor)
							If pathDoor\frameobj <> 0 Then
								doorDist = Distance(EntityX(n\Collider), EntityZ(n\Collider), EntityX(pathDoor\frameobj, True), EntityZ(pathDoor\frameobj, True))
							Else
								doorDist = Distance(EntityX(n\Collider), EntityZ(n\Collider), wpX, wpZ)
							EndIf
							
							If doorDist < 1.55 Then
								If ConsoleClerkDoorReady(pathDoor) = False Then doorWait = True
							EndIf
						EndIf
						
						prevDist = Distance(EntityX(n\Collider), EntityZ(n\Collider), wpX, wpZ)
						
						blocked = ConsoleHumanHasBlockingNPC(n, 0.72)
						If blocked Or doorWait Then
							targetSpeed = 0.0
						Else
							targetSpeed = 0.038
						EndIf
						
						targetPvt = CreatePivot()
						lookPvt = CreatePivot()
						PositionEntity targetPvt, wpX, EntityY(n\Collider, True), wpZ, True
						PositionEntity lookPvt, EntityX(n\Collider, True), EntityY(n\Collider, True), EntityZ(n\Collider, True), True
						PointEntity lookPvt, targetPvt
						turnDelta = Abs(angleDist(EntityYaw(lookPvt, True), EntityYaw(n\Collider, True)))
						RotateEntity n\Collider, 0.0, CurveAngle(EntityYaw(lookPvt, True), EntityYaw(n\Collider, True), 5.5), 0.0, True
						FreeEntity lookPvt
						FreeEntity targetPvt
						
						; Slow down through hard corners so the route looks less robotic and
						; so she does not overshoot a waypoint during a sharp turn.
						If turnDelta > 65.0 Then targetSpeed = targetSpeed * 0.70
						If turnDelta > 105.0 Then targetSpeed = targetSpeed * 0.55
						
						n\CurrSpeed = CurveValue(targetSpeed, n\CurrSpeed, 12.0)
						MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
						newDist = Distance(EntityX(n\Collider), EntityZ(n\Collider), wpX, wpZ)
						
						If doorWait Then
							n\State3 = 0
							n\PathTimer = 0
							n\PrevX = EntityX(n\Collider, True)
							n\PrevZ = EntityZ(n\Collider, True)
						ElseIf targetSpeed > 0.0 Then
							If n\PathTimer <= 0.0 Then n\PathTimer = prevDist
							If newDist < n\PathTimer - (0.008 * FPSfactor) Then
								n\State3 = 0
							ElseIf Distance(EntityX(n\Collider, True), EntityZ(n\Collider, True), n\PrevX, n\PrevZ) < 0.004 * FPSfactor Then
								n\State3 = n\State3 + FPSfactor
							ElseIf newDist + 0.02 >= n\PathTimer Then
								n\State3 = n\State3 + FPSfactor
							Else
								n\State3 = 0
							EndIf
							n\PathTimer = newDist
							n\PrevX = EntityX(n\Collider, True)
							n\PrevZ = EntityZ(n\Collider, True)
							
							If n\State3 > 70 * 1.10 Then
								ConsoleClerkOpenNearbyDoors(n)
								If newDist < 1.35 And n\PathLocation < 19 Then
									n\PathLocation = n\PathLocation + 1
									n\PathTimer = 0
									n\State3 = 0
								Else
									n\PathStatus = 0
									n\PathTimer = 0
									n\PathLocation = 0
									n\State3 = 0
									If dist < 3.0 Then
										n\State = 102
										n\State2 = 0
									EndIf
								EndIf
							EndIf
						EndIf
						
						If doorWait = False Then
							If (newDist < 0.32) Or ((prevDist < newDist) And (prevDist < 0.90)) Then
								n\PathLocation = n\PathLocation + 1
								n\PathTimer = 0
							EndIf
						EndIf
					EndIf
				Else
					n\CurrSpeed = CurveValue(0.0, n\CurrSpeed, 10.0)
					n\State2 = n\State2 + FPSfactor
					If n\State2 > 70 * 3 Then
						n\State = 100
						n\State2 = 0
					EndIf
				EndIf
			EndIf
		Case 102
			dist = Distance(EntityX(n\Collider), EntityZ(n\Collider), n\EnemyX, n\EnemyZ)
			
			If n\State2 = 0 Then
				ConsoleClerkTriggerNearestTesla(n)
				n\State2 = 1
				n\State3 = 0
			Else
				n\State3 = n\State3 + FPSfactor
			EndIf
			
			If dist < 0.26 Then
				PositionEntity n\Collider, n\EnemyX, EntityY(n\Collider), n\EnemyZ, True
				n\State = 110
				n\State2 = 0
				n\State3 = 0
				n\CurrSpeed = 0.0
				n\PathStatus = 0
				n\PathLocation = 0
			Else
				targetPvt = CreatePivot()
				lookPvt = CreatePivot()
				PositionEntity targetPvt, n\EnemyX, EntityY(n\Collider), n\EnemyZ, True
				PositionEntity lookPvt, EntityX(n\Collider, True), EntityY(n\Collider, True), EntityZ(n\Collider, True), True
				PointEntity lookPvt, targetPvt
				turnDelta = Abs(angleDist(EntityYaw(lookPvt, True), EntityYaw(n\Collider, True)))
				RotateEntity n\Collider, 0.0, CurveAngle(EntityYaw(lookPvt, True), EntityYaw(n\Collider, True), 5.0), 0.0, True
				FreeEntity lookPvt
				FreeEntity targetPvt
				
				targetSpeed = 0.040
				If turnDelta > 70.0 Then targetSpeed = 0.032
				n\CurrSpeed = CurveValue(targetSpeed, n\CurrSpeed, 10.0)
				MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
			EndIf
		Case 110
			; Tesla death animation copied from the room event, but owned by this NPC so it works without e\room\NPC[0].
			n\CurrSpeed = CurveValue(0.0, n\CurrSpeed, 5.0)
			AnimateNPC(n, 41, 60, 0.5, False)
			If n\Frame >= 60 Or AnimTime(n\obj) >= 60 Then
				n\IsDead = True
				EntityType n\Collider, HIT_DEAD
				SetNPCFrame(n, 57)
				n\State = 111
			EndIf
		Case 111
			n\CurrSpeed = 0.0
			n\IsDead = True
			EntityType n\Collider, HIT_DEAD
			AnimateNPC(n, 57, 60, 0.5, False)
	End Select
	
	If n\State = 101 Or n\State = 102 Then
		If n\CurrSpeed > 0.01 Then
			Animate2(n\obj, AnimTime(n\obj), 301, 319, n\CurrSpeed * 18.0)
			If prevFrame < 309 And AnimTime(n\obj) => 309 Then
				PlaySound2(StepSFX(GetStepSound(n\Collider), 1, Rand(0, 2)), Camera, n\Collider, 8.0, Rnd(0.3, 0.5))
			ElseIf prevFrame =< 319 And AnimTime(n\obj) =< 301 Then
				PlaySound2(StepSFX(GetStepSound(n\Collider), 1, Rand(0, 2)), Camera, n\Collider, 8.0, Rnd(0.3, 0.5))
			EndIf
		Else
			Animate2(n\obj, AnimTime(n\obj), 210, 235, 0.1)
		EndIf
	EndIf
	
	PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) - 0.32, EntityZ(n\Collider))
	RotateEntity n\obj, EntityPitch(n\Collider), EntityYaw(n\Collider) - 180.0, 0
End Function

Function D9341HasFlag%(n.NPCs, flag%)
	Return ((Int(n\Reload) And flag) <> 0)
End Function

Function D9341AddFlag(n.NPCs, flag%)
	n\Reload = Int(n\Reload) Or flag
End Function

Function D9341RemoveFlag(n.NPCs, flag%)
	If D9341HasFlag(n, flag) Then n\Reload = Int(n\Reload) - flag
End Function

Function D9341ClearPath(n.NPCs)
	Local i%
	n\PathStatus = 0
	n\PathTimer = 0
	n\PathLocation = 0
	For i = 0 To 19
		n\Path[i] = Null
	Next
End Function

Function D9341ResumeObjective(n.NPCs)
	If n = Null Then Return
	n\State = 100
	n\State2 = 0
	n\State3 = 0
	n\Idle = 101
	n\CurrSpeed = 0
	n\FollowTimer = 0
	n\WanderTimer = 0
	n\GoalTimer = 0
	n\Angle = 0
	n\DoorTarget = Null
	n\LastDoor = Null
	n\DoorRetry = 0
	n\DoorWaitTimer = 0
	n\PathX = 0
	n\PathZ = 0
	D9341ClearPath(n)
	D9341ChooseNextObjective(n)
End Function

Function D9341PathScore#(n.NPCs, targetX#, targetZ#)
	Local i%
	Local score#
	Local lastX#
	Local lastZ#
	Local wpX#
	Local wpZ#
	
	lastX = EntityX(n\Collider, True)
	lastZ = EntityZ(n\Collider, True)
	
	For i = 0 To 19
		If n\Path[i] <> Null Then
			wpX = EntityX(n\Path[i]\obj, True)
			wpZ = EntityZ(n\Path[i]\obj, True)
			score = score + Distance(lastX, lastZ, wpX, wpZ)
			lastX = wpX
			lastZ = wpZ
		EndIf
	Next
	
	score = score + Distance(lastX, lastZ, targetX, targetZ)
	Return score
End Function

Function D9341DirectTargetVisible%(n.NPCs, x#, y#, z#)
	Local pvt%
	Local result%
	
	pvt = CreatePivot()
	PositionEntity pvt, x, y, z, True
	result = EntityVisible(n\Collider, pvt)
	FreeEntity pvt
	Return result
End Function

Function D9341FindNearestRoom.Rooms(n.NPCs, roomName$)
	Local r.Rooms
	Local best.Rooms
	Local fallback.Rooms
	Local dist#
	Local fallbackDist# = 1000000.0
	Local bestScore# = 1000000.0
	Local score#
	Local status%
	Local rn$
	Local targetName$ = Lower(roomName)
	Local tx#
	Local ty#
	Local tz#
	
	For r.Rooms = Each Rooms
		If r\RoomTemplate <> Null Then
			rn = Lower(r\RoomTemplate\Name)
			If rn = targetName Then
				tx = EntityX(r\obj, True)
				ty = EntityY(r\obj, True)
				tz = EntityZ(r\obj, True)
				dist = EntityDistance(n\Collider, r\obj)
				If dist < fallbackDist Then
					fallbackDist = dist
					fallback = r
				EndIf
				status = FindPath(n, tx, ty + 0.1, tz)
				If status = 1 Then
					score = D9341PathScore(n, tx, tz)
					If score < bestScore Then
						bestScore = score
						best = r
					EndIf
				ElseIf status = 0 And D9341DirectTargetVisible(n, tx, ty, tz) Then
					If dist < bestScore Then
						bestScore = dist
						best = r
					EndIf
				EndIf
		EndIf
	EndIf
	Next
	
	D9341ClearPath(n)
	If best <> Null Then Return best
	Return fallback
End Function

Function D9341NearestRoomByName.Rooms(n.NPCs, roomName$)
	Local r.Rooms
	Local best.Rooms
	Local dist#
	Local bestDist# = 1000000.0
	Local rn$
	Local targetName$ = Lower(roomName)
	
	For r.Rooms = Each Rooms
		If r\RoomTemplate <> Null Then
			rn = Lower(r\RoomTemplate\Name)
			If rn = targetName Then
				dist = EntityDistance(n\Collider, r\obj)
				If dist < bestDist Then
					bestDist = dist
					best = r
				EndIf
			EndIf
		EndIf
	Next
	
	Return best
End Function


Function D9341FindGateRoom.Rooms(n.NPCs)
	Local r.Rooms
	
	r = D9341FindNearestRoom(n, "gateb")
	If r <> Null Then Return r
	r = D9341FindNearestRoom(n, "gatebentrance")
	If r <> Null Then Return r
	r = D9341FindNearestRoom(n, "gateaentrance")
	If r <> Null Then Return r
	Return D9341FindNearestRoom(n, "gatea")
End Function

Function D9341FindNearestItem.Items(n.NPCs, itemName$)
	Local it.Items
	Local best.Items
	Local fallback.Items
	Local dist#
	Local fallbackDist# = 1000000.0
	Local bestScore# = 1000000.0
	Local score#
	Local status%
	Local targetName$ = Lower(itemName)
	Local tx#
	Local ty#
	Local tz#
	
	For it.Items = Each Items
		If it\collider <> 0 Then
			If it\Picked = False Then
				If it\itemtemplate <> Null Then
					If Lower(it\itemtemplate\name) = targetName Then
						tx = EntityX(it\collider, True)
						ty = EntityY(it\collider, True)
						tz = EntityZ(it\collider, True)
						dist = EntityDistance(n\Collider, it\collider)
						If dist < fallbackDist Then
							fallbackDist = dist
							fallback = it
						EndIf
						status = FindPath(n, tx, ty + 0.1, tz)
						If status = 1 Then
							score = D9341PathScore(n, tx, tz)
							If score < bestScore Then
								bestScore = score
								best = it
							EndIf
						ElseIf status = 0 And D9341DirectTargetVisible(n, tx, ty, tz) Then
							If dist < bestScore Then
								bestScore = dist
								best = it
							EndIf
						EndIf
					EndIf
				EndIf
			EndIf
		EndIf
	Next
	
	D9341ClearPath(n)
	If best <> Null Then Return best
	Return fallback
End Function


Function D9341SetTarget(n.NPCs, x#, y#, z#)
	n\EnemyX = x
	n\EnemyY = y
	n\EnemyZ = z
	n\PathStatus = 0
	n\PathTimer = 0
	n\PathLocation = 0
	n\PathX = 0
	n\PathZ = 0
	n\State = 101
	n\State2 = 0
	n\State3 = 0
	n\Angle = 0
	n\GoalTimer = 0
	n\WanderTimer = 0
	n\GoalX = EntityX(n\Collider, True)
	n\GoalZ = EntityZ(n\Collider, True)
	n\PrevX = EntityX(n\Collider, True)
	n\PrevZ = EntityZ(n\Collider, True)
End Function


Function D9341SetRoomTarget%(n.NPCs, r.Rooms, objIndex%)
	Local target%
	
	If r = Null Then Return False
	If objIndex >= 0 Then target = r\Objects[objIndex]
	If target = 0 Then target = r\obj
	If target = 0 Then Return False
	D9341SetTarget(n, EntityX(target, True), EntityY(target, True), EntityZ(target, True))
	Return True
End Function

Function D9341SetItemTarget%(n.NPCs, it.Items)
	If it = Null Then Return False
	If it\collider = 0 Then Return False
	D9341SetTarget(n, EntityX(it\collider, True), EntityY(it\collider, True), EntityZ(it\collider, True))
	Return True
End Function

Function D9341LCZCheckpointLocked%()
	Local e.Events
	
	For e.Events = Each Events
		If e\EventName = "room2sl" Then
			If e\EventState3 <> 0 Then Return True
		EndIf
	Next
	
	Return False
End Function

Function D9341KeyLevel%(n.NPCs)
	If n = Null Then Return 0
	If D9341HasFlag(n, D9341F_OMNI) Then Return 6
	If D9341HasFlag(n, D9341F_KEY2) Then Return 2
	If D9341HasFlag(n, D9341F_KEY1) Then Return 1
	Return 0
End Function

Function D9341DoorInRoom%(d.Doors, roomName$)
	If d = Null Then Return False
	If d\room <> Null Then
		If d\room\RoomTemplate <> Null Then
			If Lower(d\room\RoomTemplate\Name) = Lower(roomName) Then Return True
		EndIf
	EndIf
	If d\LinkedDoor <> Null Then
		If d\LinkedDoor\room <> Null Then
			If d\LinkedDoor\room\RoomTemplate <> Null Then
				If Lower(d\LinkedDoor\room\RoomTemplate\Name) = Lower(roomName) Then Return True
			EndIf
		EndIf
	EndIf
	Return False
End Function

Function D9341DoorUsable%(n.NPCs, d.Doors)
	If d = Null Then Return False
	If d\obj = 0 Then Return False
	If D9341DoorInRoom(d, "start") Then Return False
	If d\IsElevatorDoor > 0 Then Return False
	If d\KeyCard < 0 Then Return False
	If d\locked Then
		If Int(n\PrevState) = 40 And D9341DoorInRoom(d, "room2sl") Then
			d\locked = False
		Else
			Return False
		EndIf
	EndIf
	If d\KeyCard > D9341KeyLevel(n) Then Return False
	If d\LinkedDoor <> Null Then
		If d\LinkedDoor\KeyCard > D9341KeyLevel(n) Then Return False
		If d\LinkedDoor\locked Then
			If Int(n\PrevState) = 40 And D9341DoorInRoom(d\LinkedDoor, "room2sl") Then
				d\LinkedDoor\locked = False
			Else
				Return False
			EndIf
		EndIf
	EndIf
	Return True
End Function

Function D9341OpenDoorNow%(n.NPCs, d.Doors)
	Local sound%
	Local dir%
	
	If D9341DoorUsable(n, d) = False Then Return False
	
	d\dist = 0
	If d\fastopen < 1 Then d\fastopen = 1
	If d\open = False Then
		D9341PlayDoorUseSound(n, d)
		d\open = True
		d\timerstate = d\timer
		If d\obj <> 0 Then
			sound = Rand(0, 2)
			dir = Min(d\dir, 3)
			d\SoundCHN = PlaySound2(OpenDoorSFX(dir, sound), Camera, d\obj)
		EndIf
		If d\LinkedDoor <> Null Then
			d\LinkedDoor\dist = 0
			If d\LinkedDoor\fastopen < 1 Then d\LinkedDoor\fastopen = 1
			If D9341DoorUsable(n, d\LinkedDoor) Then
				d\LinkedDoor\open = True
				d\LinkedDoor\timerstate = d\LinkedDoor\timer
				If d\LinkedDoor\obj <> 0 Then
					dir = Min(d\LinkedDoor\dir, 3)
					d\LinkedDoor\SoundCHN = PlaySound2(OpenDoorSFX(dir, sound), Camera, d\LinkedDoor\obj)
				EndIf
			EndIf
		EndIf
	EndIf
	Return True
End Function

Function D9341DoorReady%(d.Doors)
	If d = Null Then Return True
	If d\open = False Then Return False
	If d\openstate < 150.0 Then Return False
	If d\LinkedDoor <> Null Then
		If d\LinkedDoor\open = False Then Return False
		If d\LinkedDoor\openstate < 150.0 Then Return False
	EndIf
	Return True
End Function

Function D9341DoorKeycard%(d.Doors)
	If d = Null Then Return False
	If d\KeyCard > 0 Then Return True
	If d\LinkedDoor <> Null Then
		If d\LinkedDoor\KeyCard > 0 Then Return True
	EndIf
	Return False
End Function

Function D9341DoorButton%(n.NPCs, d.Doors)
	Local i%
	Local b%
	Local dist#
	Local best%
	Local fallback%
	Local bestDist# = 1000000.0
	Local fallbackDist# = 1000000.0
	
	If d = Null Then Return 0
	For i = 0 To 1
		b = d\buttons[i]
		If b <> 0 Then
			dist = EntityDistance(n\Collider, b)
			If EntityVisible(n\Collider, b) Then
				If dist < bestDist Then
					bestDist = dist
					best = b
				EndIf
			EndIf
			If dist < fallbackDist Then
				fallbackDist = dist
				fallback = b
			EndIf
		EndIf
	Next
	If d\LinkedDoor <> Null Then
		For i = 0 To 1
			b = d\LinkedDoor\buttons[i]
			If b <> 0 Then
				dist = EntityDistance(n\Collider, b)
				If EntityVisible(n\Collider, b) Then
					If dist < bestDist Then
						bestDist = dist
						best = b
					EndIf
				EndIf
				If dist < fallbackDist Then
					fallbackDist = dist
					fallback = b
				EndIf
			EndIf
		Next
	EndIf
	If best <> 0 Then Return best
	Return fallback
End Function

Function D9341DoorRegularButton%(n.NPCs, d.Doors)
	Local i%
	Local b%
	Local dist#
	Local best%
	Local bestDist# = 1000000.0
	
	If d = Null Then Return 0
	For i = 0 To 1
		b = d\buttons[i]
		If b <> 0 Then
			dist = EntityDistance(n\Collider, b)
			If dist < bestDist Then
				bestDist = dist
				best = b
			EndIf
		EndIf
	Next
	If d\LinkedDoor <> Null Then
		For i = 0 To 1
			b = d\LinkedDoor\buttons[i]
			If b <> 0 Then
				dist = EntityDistance(n\Collider, b)
				If dist < bestDist Then
					bestDist = dist
					best = b
				EndIf
			EndIf
		Next
	EndIf
	Return best
End Function

Function D9341PlayDoorUseSound(n.NPCs, d.Doors)
	Local b%
	
	b = D9341DoorButton(n, d)
	If D9341DoorKeycard(d) Then
		If b <> 0 Then
			PlaySound2(KeyCardSFX1, Camera, b, 20.0, 1.0)
		Else
			PlaySound_Strict KeyCardSFX1
		EndIf
	Else
		If b <> 0 Then
			PlaySound2(ButtonSFX, Camera, b, 20.0, 1.0)
		Else
			PlaySound_Strict ButtonSFX
		EndIf
	EndIf
End Function

Function D9341CloseDoorNow(d.Doors)
	Local sound%
	Local dir%
	
	If d = Null Then Return
	If D9341DoorInRoom(d, "start") Then Return
	d\dist = 0
	If d\open Then
		d\open = False
		sound = Rand(0, 2)
		If d\obj <> 0 Then
			dir = Min(d\dir, 3)
			d\SoundCHN = PlaySound2(CloseDoorSFX(dir, sound), Camera, d\obj)
		EndIf
	EndIf
	If d\LinkedDoor <> Null Then
		d\LinkedDoor\dist = 0
		If d\LinkedDoor\open Then
			d\LinkedDoor\open = False
			If d\LinkedDoor\obj <> 0 Then
				dir = Min(d\LinkedDoor\dir, 3)
				d\LinkedDoor\SoundCHN = PlaySound2(CloseDoorSFX(dir, sound), Camera, d\LinkedDoor\obj)
			EndIf
		EndIf
	EndIf
End Function


Function D9341ClosePathDoorBehind(n.NPCs, d.Doors)
	Local doorDist#
	
	If d = Null Then Return
	If (Not NoTarget) And EntityDistance(n\Collider, Collider) < 3.0 Then Return
	If d\obj = 0 Then Return
	If d\IsElevatorDoor > 0 Then Return
	If D9341DoorKeycard(d) Then Return
	If d\frameobj <> 0 Then
		doorDist = EntityDistance(n\Collider, d\frameobj)
	Else
		doorDist = EntityDistance(n\Collider, d\obj)
	EndIf
	If doorDist < 0.65 Or doorDist > 3.00 Then Return
	If d\open Then D9341CloseDoorNow(d)
End Function

Function D9341CloseNearestDoorAt(n.NPCs, x#, z#)
	Local d.Doors
	Local i%
	Local b%
	Local dist#
	Local npcDist#
	Local best.Doors
	Local bestDist# = 1000000.0
	Local bestNPCDist# = 1000000.0
	
	For d.Doors = Each Doors
		If d\obj <> 0 Then
			If d\IsElevatorDoor = 0 Then
				If D9341DoorKeycard(d) = False Then
					For i = 0 To 1
						b = d\buttons[i]
						If b <> 0 Then
							dist = Distance(EntityX(b, True), EntityZ(b, True), x, z)
							npcDist = EntityDistance(n\Collider, b)
							If dist < bestDist And npcDist < 2.65 Then
								bestDist = dist
								bestNPCDist = npcDist
								best = d
							EndIf
						EndIf
					Next
					If d\LinkedDoor <> Null Then
						For i = 0 To 1
							b = d\LinkedDoor\buttons[i]
							If b <> 0 Then
								dist = Distance(EntityX(b, True), EntityZ(b, True), x, z)
								npcDist = EntityDistance(n\Collider, b)
								If dist < bestDist And npcDist < 2.65 Then
									bestDist = dist
									bestNPCDist = npcDist
									best = d
								EndIf
							EndIf
						Next
					EndIf
				EndIf
			EndIf
		EndIf
	Next
	
	If best <> Null Then
		If bestDist < 1.55 And bestNPCDist < 2.65 Then D9341CloseDoorNow(best)
	EndIf
End Function

Function D9341StartCloseBehind%(n.NPCs, d.Doors, lookX#, lookZ#)
	Local b%
	Local doorDist#
	Local sideYaw#
	
	If d = Null Then Return False
	If (Not NoTarget) And EntityDistance(n\Collider, Collider) < 3.0 Then Return False
	If d\IsElevatorDoor > 0 Then Return False
	If D9341DoorKeycard(d) Then Return False
	If d\open = False Then Return False
	b = D9341DoorButton(n, d)
	If b = 0 Then Return False
	If d\frameobj <> 0 Then
		doorDist = EntityDistance(n\Collider, d\frameobj)
	Else
		doorDist = EntityDistance(n\Collider, d\obj)
	EndIf
	If doorDist < 0.35 Or doorDist > 2.05 Then Return False
	If EntityDistance(n\Collider, b) > 3.35 Then Return False
	If D9341DoorAhead(n, d, 1.55) Then Return False
	
	n\Idle = n\State
	n\State = 150
	n\State2 = 0
	n\State3 = -1
	sideYaw = DeltaYaw(n\Collider, b)
	If sideYaw > 0 Then n\State3 = 1
	n\DoorTarget = d
	n\PrevX = lookX
	n\PrevZ = lookZ
	n\PrevY = Distance(EntityX(n\Collider), EntityZ(n\Collider), EntityX(b, True), EntityZ(b, True))
	n\PathX = EntityX(b, True)
	n\PathZ = EntityZ(b, True)
	n\CurrSpeed = -0.006
	Return True
End Function

Function D9341DoorAhead%(n.NPCs, d.Doors, maxDist#)
	Local yaw#
	Local dist#
	
	If n = Null Then Return False
	If d = Null Then Return False
	If d\frameobj <> 0 Then
		dist = EntityDistance(n\Collider, d\frameobj)
		yaw = Abs(DeltaYaw(n\Collider, d\frameobj))
	ElseIf d\obj <> 0 Then
		dist = EntityDistance(n\Collider, d\obj)
		yaw = Abs(DeltaYaw(n\Collider, d\obj))
	Else
		Return False
	EndIf
	If dist < maxDist And yaw < 82.0 Then Return True
	Return False
End Function

Function D9341KeepRoom2SLAccessOpen(n.NPCs)
	Local e.Events
	Local i%
	Local d.Doors
	Local dist#

	If n = Null Then Return
	If Int(n\PrevState) <> 40 Then Return

	For e.Events = Each Events
		If e\EventName = "room2sl" Then
			If e\room <> Null Then
				For i = 0 To 1
					d = e\room\RoomDoors[i]
					If d <> Null Then
						d\locked = False
						d\dist = 0
						If d\fastopen < 1 Then d\fastopen = 1
						If d\LinkedDoor <> Null Then
							d\LinkedDoor\locked = False
							d\LinkedDoor\dist = 0
							If d\LinkedDoor\fastopen < 1 Then d\LinkedDoor\fastopen = 1
						EndIf
						If d\frameobj <> 0 Then
							dist = EntityDistance(n\Collider, d\frameobj)
						Else
							dist = EntityDistance(n\Collider, d\obj)
						EndIf
						If dist < 1.45 And d\open = False Then D9341OpenDoorNow(n, d)
					EndIf
				Next
			EndIf
			Exit
		EndIf
	Next
End Function

Function D9341RouteToLockdownSwitch%(n.NPCs)
	Local r.Rooms
	
	If n = Null Then Return False
	If D9341LCZCheckpointLocked() = False Then Return False
	r = D9341FindNearestRoom(n, "room2sl")
	If r = Null Then Return False
	
	D9341RemoveFlag(n, D9341F_LOCKDOWN)
	n\PrevState = 40
	If r\Levers[0] <> 0 Then
		D9341SetTarget(n, EntityX(r\Levers[0], True), EntityY(r\Levers[0], True), EntityZ(r\Levers[0], True))
	Else
		D9341SetRoomTarget(n, r, 0)
	EndIf
	n\DoorRetry = 0
	n\DoorWaitTimer = 0
	n\LastDoor = Null
	Return True
End Function

Function D9341SetRoom2SLTarget%(n.NPCs, r.Rooms)
	Local target%
	
	If n = Null Then Return False
	If r = Null Then Return False
	If r\Objects[7] <> 0 Then
		If EntityY(n\Collider, True) < EntityY(r\obj, True) + 360.0 * RoomScale Then
			If EntityDistance(n\Collider, r\Objects[7]) > 0.95 Then target = r\Objects[7]
		EndIf
	EndIf
	If target = 0 Then
		If r\Objects[15] <> 0 Then
			If EntityDistance(n\Collider, r\Objects[15]) > 0.95 Then target = r\Objects[15]
		EndIf
	EndIf
	If target = 0 Then
		If r\Objects[16] <> 0 Then
			If EntityDistance(n\Collider, r\Objects[16]) > 0.95 Then target = r\Objects[16]
		EndIf
	EndIf
	If target = 0 Then
		If r\Objects[17] <> 0 Then
			If EntityDistance(n\Collider, r\Objects[17]) > 0.95 Then target = r\Objects[17]
		EndIf
	EndIf
	If target = 0 Then target = r\Levers[0]
	If target = 0 Then Return D9341SetRoomTarget(n, r, 0)
	D9341SetTarget(n, EntityX(target, True), EntityY(target, True), EntityZ(target, True))
	Return True
End Function

Function D9341StartPlayerBypass%(n.NPCs)
	Local dist#
	Local yaw#
	
	If n = Null Then Return False
	If n\State <> 101 Then Return False
	If Collider = 0 Then Return False
	If n\BlinkTimer > 0 Then Return False
	dist = EntityDistance(n\Collider, Collider)
	If dist > 0.62 Then Return False
	yaw = Abs(DeltaYaw(n\Collider, Collider))
	If yaw > 42.0 Then Return False
	
	n\Idle = n\State
	n\State = 160
	n\State2 = 0
	n\State3 = 1
	If EntityX(Collider, True) < EntityX(n\Collider, True) Then n\State3 = -1
	n\SoundTimer = 0
	n\CurrSpeed = 0
	n\PathStatus = 0
	n\PathTimer = 0
	n\PathLocation = 0
	n\PathX = 0
	n\PathZ = 0
	n\BlinkTimer = 70 * Rnd(2.0, 4.0)
	Return True
End Function

Function D9341CloseRoomDoors(r.Rooms)
	Local i%
	
	If r = Null Then Return
	For i = 0 To 3
		If r\RoomDoors[i] <> Null Then D9341CloseDoorNow(r\RoomDoors[i])
	Next
End Function

Function D9341CloseTestroomAfterLeaving(n.NPCs)
	Local r.Rooms
	Local rn$
	Local dist#
	
	If D9341HasFlag(n, D9341F_TESTROOMCLOSE) = False Then Return
	For r.Rooms = Each Rooms
		If r\RoomTemplate <> Null Then
			rn = Lower(r\RoomTemplate\Name)
			If rn = "room2testroom" Or rn = "room2testroom2" Then
				dist = EntityDistance(n\Collider, r\obj)
				If dist > 14.0 And dist < 28.0 Then
					D9341CloseRoomDoors(r)
					D9341RemoveFlag(n, D9341F_TESTROOMCLOSE)
					Return
				EndIf
			EndIf
		EndIf
	Next
End Function

Function D9341OpenRoomDoors(n.NPCs, r.Rooms)
	Local i%
	
	If r = Null Then Return
	For i = 0 To 3
		If r\RoomDoors[i] <> Null Then D9341OpenDoorNow(n, r\RoomDoors[i])
	Next
End Function

Function D9341BadPathRoom%(w.WayPoints)
	Return False
End Function

Function D9341TurnTowardPoint(n.NPCs, x#, z#, curve#)
	Local targetPvt%
	Local lookPvt%
	
	targetPvt = CreatePivot()
	lookPvt = CreatePivot()
	PositionEntity targetPvt, x, EntityY(n\Collider, True), z, True
	PositionEntity lookPvt, EntityX(n\Collider, True), EntityY(n\Collider, True), EntityZ(n\Collider, True), True
	PointEntity lookPvt, targetPvt
	RotateEntity n\Collider, 0, CurveAngle(EntityYaw(lookPvt, True), EntityYaw(n\Collider, True), curve), 0, True
	FreeEntity lookPvt
	FreeEntity targetPvt
End Function

Function D9341NearestTeslaRoom.Rooms(n.NPCs, maxDist#)
	Local r.Rooms
	Local best.Rooms
	Local target%
	Local dist#
	Local bestDist# = 1000000.0
	
	For r.Rooms = Each Rooms
		If ConsoleTeslaRoom(r) Then
			target = r\Objects[2]
			If target = 0 Then target = r\Objects[3]
			If target = 0 Then target = r\obj
			If target <> 0 Then
				dist = EntityDistance(n\Collider, target)
				If dist < maxDist And dist < bestDist Then
					bestDist = dist
					best = r
				EndIf
			EndIf
		EndIf
	Next
	
	Return best
End Function

Function D9341TeslaExitObject%(n.NPCs, r.Rooms)
	Local d5#
	Local d6#
	Local t5#
	Local t6#
	
	If r = Null Then Return 0
	If r\Objects[5] = 0 Then Return r\Objects[6]
	If r\Objects[6] = 0 Then Return r\Objects[5]
	d5 = EntityDistance(n\Collider, r\Objects[5])
	d6 = EntityDistance(n\Collider, r\Objects[6])
	t5 = Distance(EntityX(r\Objects[5], True), EntityZ(r\Objects[5], True), n\EnemyX, n\EnemyZ)
	t6 = Distance(EntityX(r\Objects[6], True), EntityZ(r\Objects[6], True), n\EnemyX, n\EnemyZ)
	If Abs(t5 - t6) > 0.25 Then
		If t5 < t6 Then Return r\Objects[5]
		Return r\Objects[6]
	EndIf
	If d5 > d6 Then Return r\Objects[5]
	Return r\Objects[6]
End Function

Function D9341TriggerTeslaRoom(r.Rooms)
	Local e.Events
	
	If r = Null Then Return
	ConsoleClerkEnableTeslaRoom(r)
	If r\Objects[3] <> 0 Then
		ShowEntity r\Objects[3]
		PlaySound2(TeslaActivateSFX, Camera, r\Objects[3], 4.0, 0.5)
		If r\Objects[4] <> 0 Then HideEntity r\Objects[4]
	Else
		PlaySound_Strict TeslaActivateSFX
	EndIf
	For e.Events = Each Events
		If e\room = r Then
			If e\EventName = "room2tesla" Then
				e\EventState2 = 0
				e\EventState3 = 0
				e\EventState = 1
				If e\SoundCHN <> 0 Then StopChannel(e\SoundCHN)
				If r\Objects[3] <> 0 Then e\SoundCHN = PlaySound2(TeslaActivateSFX, Camera, r\Objects[3], 4.0, 0.5)
				Exit
			EndIf
		EndIf
	Next
End Function

Function D9341ResetTeslaRoom(r.Rooms)
	Local e.Events
	
	If r = Null Then Return
	If r\Objects[3] <> 0 Then HideEntity r\Objects[3]
	If r\Objects[4] <> 0 Then HideEntity r\Objects[4]
	For e.Events = Each Events
		If e\room = r Then
			If e\EventName = "room2tesla" Then
				e\EventState = 0
				e\EventState2 = Max(e\EventState2, 70 * 3.5)
				e\EventState3 = Max(e\EventState3, 140)
				If e\SoundCHN <> 0 Then StopChannel(e\SoundCHN) : e\SoundCHN = 0
				Exit
			EndIf
		EndIf
	Next
End Function

Function D9341MaybeStartTeslaReaction%(n.NPCs)
	Local r.Rooms
	Local exitObj%
	Local latchObj%
	
	If n\State <> 101 Then Return False
	If n\LastDist > 0 Then Return False
	If n\TeslaRoom <> Null Then
		latchObj = n\TeslaRoom\Objects[2]
		If latchObj = 0 Then latchObj = n\TeslaRoom\Objects[3]
		If latchObj = 0 Then latchObj = n\TeslaRoom\obj
		If latchObj <> 0 Then
			If EntityDistance(n\Collider, latchObj) > 3.8 Then n\TeslaRoom = Null
		Else
			n\TeslaRoom = Null
		EndIf
	EndIf
	r = D9341NearestTeslaRoom(n, 1.55)
	If r = Null Then Return False
	If n\TeslaRoom = r Then Return False
	If r\Objects[2] = 0 Then Return False
	exitObj = D9341TeslaExitObject(n, r)
	If exitObj = 0 Then Return False
	n\TeslaRoom = r
	n\Idle = n\State
	n\State = 140
	n\State2 = 0
	n\State3 = 0
	n\Angle = 0
	n\PathX = EntityX(exitObj, True)
	n\PathZ = EntityZ(exitObj, True)
	n\PrevX = EntityX(r\Objects[2], True)
	n\PrevZ = EntityZ(r\Objects[2], True)
	n\CurrSpeed = 0
	Return True
End Function

Function D9341MaybeStartServerRoom%(n.NPCs)
	Local r.Rooms
	
	If n = Null Then Return False
	If n\State <> 101 Then Return False
	If D9341HasFlag(n, D9341F_SERVERS) Then Return False
	r = D9341NearestRoomByName(n, "room2servers")
	If r = Null Then Return False
	If EntityDistance(n\Collider, r\obj) > 6.0 Then Return False
	
	n\Idle = n\State
	n\State = 170
	n\State2 = 0
	n\State3 = 0
	n\PrevX = n\EnemyX
	n\PrevY = 0
	n\PrevZ = n\EnemyZ
	n\Angle = n\EnemyY
	n\PathStatus = 0
	n\PathTimer = 0
	n\PathLocation = 0
	Return True
End Function

Function D9341StartStuckWander%(n.NPCs)
	If n = Null Then Return False
	If n\State <> 101 Then Return False
	If Int(n\PrevState) <> 0 Then
		n\GoalTimer = 0
		n\State3 = 0
		n\PathX = 0
		n\PathZ = 0
		D9341ClearPath(n)
		Return True
	EndIf
	
	n\Idle = n\State
	n\State = 180
	n\State2 = 0
	n\State3 = 1
	If Rand(0, 1) = 0 Then n\State3 = -1
	n\Angle = WrapAngle(EntityYaw(n\Collider, True) + Rnd(-135.0, 135.0))
	n\CurrSpeed = 0
	n\PathStatus = 0
	n\PathTimer = 0
	n\PathLocation = 0
	n\PathX = 0
	n\PathZ = 0
	Return True
End Function

Function D9341ProbeBlocked%(n.NPCs, yaw#, checkDist#, height#)
	Local pvt%
	Local hit%
	
	If n = Null Then Return False
	pvt = CreatePivot()
	PositionEntity pvt, EntityX(n\Collider, True), EntityY(n\Collider, True) + height, EntityZ(n\Collider, True), True
	RotateEntity pvt, 0, yaw, 0, True
	HideEntity n\Collider
	EntityPick pvt, checkDist
	hit = PickedEntity()
	ShowEntity n\Collider
	FreeEntity pvt
	If hit <> 0 Then Return True
	Return False
End Function

Function D9341ForwardBlocked%(n.NPCs, yaw#, checkDist#)
	If n = Null Then Return False
	If D9341ProbeBlocked(n, yaw, checkDist, 0.38) Then Return True
	Return False
End Function

Function D9341LowStepAhead%(n.NPCs, yaw#, checkDist#)
	If n = Null Then Return False
	If D9341ProbeBlocked(n, yaw, checkDist, -0.16) = False Then Return False
	If D9341ProbeBlocked(n, yaw, checkDist, 0.30) Then Return False
	Return True
End Function

Function D9341SteerAroundWall%(n.NPCs, targetX#, targetZ#)
	Local yaw#
	Local targetYaw#
	Local leftYaw#
	Local rightYaw#
	Local leftBlocked%
	Local rightBlocked%
	Local chosenYaw#
	
	If n = Null Then Return False
	yaw = EntityYaw(n\Collider, True)
	If D9341ForwardBlocked(n, yaw, 0.52) = False Then Return False
	
	targetYaw = VectorYaw(targetX - EntityX(n\Collider, True), 0, targetZ - EntityZ(n\Collider, True))
	leftYaw = WrapAngle(yaw - 58.0)
	rightYaw = WrapAngle(yaw + 58.0)
	leftBlocked = D9341ForwardBlocked(n, leftYaw, 0.48)
	rightBlocked = D9341ForwardBlocked(n, rightYaw, 0.48)
	
	If leftBlocked And rightBlocked Then
		chosenYaw = WrapAngle(yaw + 145.0)
	ElseIf leftBlocked Then
		chosenYaw = rightYaw
	ElseIf rightBlocked Then
		chosenYaw = leftYaw
	ElseIf Abs(angleDist(leftYaw, targetYaw)) < Abs(angleDist(rightYaw, targetYaw)) Then
		chosenYaw = leftYaw
	Else
		chosenYaw = rightYaw
	EndIf
	
	RotateEntity n\Collider, 0, CurveAngle(chosenYaw, yaw, 3.0), 0, True
	Return True
End Function

Function D9341CanLiftNudge%(n.NPCs)
	If n = Null Then Return False
	If D9341LowStepAhead(n, EntityYaw(n\Collider, True), 0.42) Then Return True
	Return False
End Function

Function D9341LiftNudgeToward%(n.NPCs, x#, z#)
	Local oldYaw#
	Local targetYaw#
	Local lowBlocked%
	
	If n = Null Then Return False
	targetYaw = VectorYaw(x - EntityX(n\Collider, True), 0, z - EntityZ(n\Collider, True))
	If D9341ProbeBlocked(n, targetYaw, 0.44, 0.30) Then Return False
	lowBlocked = D9341ProbeBlocked(n, targetYaw, 0.44, -0.16)
	If lowBlocked = False And n\State3 < 70 * 0.35 Then Return False
	oldYaw = EntityYaw(n\Collider, True)
	RotateEntity n\Collider, 0, targetYaw, 0, True
	TranslateEntity n\Collider, 0, 0.14, 0, True
	MoveEntity n\Collider, 0, 0, 0.12
	RotateEntity n\Collider, 0, oldYaw, 0, True
	n\DropSpeed = 0
	ResetEntity n\Collider
	Return True
End Function

Function D9341SetRandomRoomTarget%(n.NPCs)
	Local r.Rooms
	Local pick.Rooms
	Local rn$
	Local attempts%
	
	If n = Null Then Return False
	For attempts = 0 To 18
		For r.Rooms = Each Rooms
			If r\RoomTemplate <> Null Then
				rn = Lower(r\RoomTemplate\Name)
				If rn <> "gatea" And rn <> "exit1" And rn <> "pocketdimension" And rn <> "dimension1499" Then
					If Rand(1, 9) = 1 Then
						pick = r
						Exit
					EndIf
				EndIf
			EndIf
		Next
		If pick <> Null Then Exit
	Next
	
	If pick = Null Then Return False
	n\PrevState = 0
	Return D9341SetRoomTarget(n, pick, 0)
End Function

Function D9341SkipCurrentObjective(n.NPCs)
	If n = Null Then Return
	
	Select Int(n\PrevState)
		Case 10
			D9341AddFlag(n, D9341F_KEY1)
		Case 20, 25
			D9341AddFlag(n, D9341F_KEY1)
			D9341AddFlag(n, D9341F_KEY2)
		Case 30, 31
			D9341AddFlag(n, D9341F_OMNI)
		Case 40
			D9341AddFlag(n, D9341F_LOCKDOWN)
		Case 50
			D9341AddFlag(n, D9341F_008)
		Case 60
			D9341AddFlag(n, D9341F_RCCOFF)
		Case 70
			D9341AddFlag(n, D9341F_079)
		Case 80
			D9341AddFlag(n, D9341F_RCCON)
		Default
			If D9341SetRandomRoomTarget(n) Then Return
	End Select
	
	n\GoalTimer = 0
	D9341ChooseNextObjective(n)
End Function

Function D9341UpdateIdleWatchdog(n.NPCs)
	Local moved#
	
	If n = Null Then Return
	If n\State <> 101 And n\State <> 102 Then
		n\GoalTimer = 0
		n\GoalX = EntityX(n\Collider, True)
		n\GoalZ = EntityZ(n\Collider, True)
		Return
	EndIf
	If Int(n\PrevState) <> 0 Then
		n\GoalTimer = 0
		n\GoalX = EntityX(n\Collider, True)
		n\GoalZ = EntityZ(n\Collider, True)
		Return
	EndIf
	
	If n\GoalTimer <= 0 Then
		n\GoalX = EntityX(n\Collider, True)
		n\GoalZ = EntityZ(n\Collider, True)
	EndIf
	moved = Distance(EntityX(n\Collider, True), EntityZ(n\Collider, True), n\GoalX, n\GoalZ)
	If moved > 0.11 Then
		n\GoalTimer = 0
		n\GoalX = EntityX(n\Collider, True)
		n\GoalZ = EntityZ(n\Collider, True)
		Return
	EndIf
	
	n\GoalTimer = n\GoalTimer + FPSfactor
	If n\State = 101 Then
		If n\GoalTimer > 70 * 4.0 Then
			n\GoalTimer = 0
			If D9341StartStuckWander(n) = False Then D9341SetRandomRoomTarget(n)
		EndIf
	ElseIf n\GoalTimer > 70 * 4.0 Then
		n\GoalTimer = 0
		D9341ResumeObjective(n)
	EndIf
End Function

Function D9341Remove914InputKey2(r.Rooms)
	Local it.Items
	
	If r = Null Then Return
	If r\Objects[2] = 0 Then Return
	For it.Items = Each Items
		If it\collider <> 0 Then
			If it\Picked = False Then
				If it\itemtemplate <> Null Then
					If Lower(it\itemtemplate\name) = "key2" Then
						If Distance(EntityX(it\collider), EntityZ(it\collider), EntityX(r\Objects[2], True), EntityZ(r\Objects[2], True)) < 180.0 * RoomScale Then
							RemoveItem(it)
							Return
						EndIf
					EndIf
				EndIf
			EndIf
		EndIf
	Next
End Function

Function D9341Remove914OutputOmni(r.Rooms)
	Local it.Items
	
	If r = Null Then Return
	If r\Objects[3] = 0 Then Return
	For it.Items = Each Items
		If it\collider <> 0 Then
			If it\Picked = False Then
				If it\itemtemplate <> Null Then
					If Lower(it\itemtemplate\name) = "key6" Then
						If Distance(EntityX(it\collider), EntityZ(it\collider), EntityX(r\Objects[3], True), EntityZ(r\Objects[3], True)) < 260.0 * RoomScale Then
							RemoveItem(it)
							Return
						EndIf
					EndIf
				EndIf
			EndIf
		EndIf
	Next
End Function

Function D9341Set914OutputWaitTarget%(n.NPCs, r.Rooms)
	Local d.Doors
	Local pvt%
	Local x#
	Local y#
	Local z#
	Local vx#
	Local vz#
	Local dist#
	
	If n = Null Then Return False
	If r = Null Then Return False
	
	d = r\RoomDoors[1]
	If d <> Null Then
		If d\frameobj <> 0 Then
			pvt = CreatePivot()
			PositionEntity pvt, EntityX(d\frameobj, True), EntityY(r\obj, True), EntityZ(d\frameobj, True), True
			RotateEntity pvt, 0, EntityYaw(d\frameobj, True), 0, True
			MoveEntity pvt, 0.34, 0, 0.68
			x = EntityX(pvt, True)
			y = EntityY(r\obj, True)
			z = EntityZ(pvt, True)
			FreeEntity pvt
			D9341SetTarget(n, x, y, z)
			Return True
		ElseIf d\obj <> 0 Then
			pvt = CreatePivot()
			PositionEntity pvt, EntityX(d\obj, True), EntityY(r\obj, True), EntityZ(d\obj, True), True
			RotateEntity pvt, 0, EntityYaw(d\obj, True), 0, True
			MoveEntity pvt, 0.34, 0, 0.68
			x = EntityX(pvt, True)
			y = EntityY(r\obj, True)
			z = EntityZ(pvt, True)
			FreeEntity pvt
			D9341SetTarget(n, x, y, z)
			Return True
		EndIf
	EndIf
	
	If r\Objects[3] = 0 Then Return False
	
	x = EntityX(r\Objects[3], True)
	y = EntityY(r\Objects[3], True)
	z = EntityZ(r\Objects[3], True)
	vx = EntityX(r\obj, True) - x
	vz = EntityZ(r\obj, True) - z
	dist = Sqr(vx * vx + vz * vz)
	If dist < 0.01 Then Return D9341SetRoomTarget(n, r, 3)
	x = x + (vx / dist) * 1.35
	z = z + (vz / dist) * 1.35
	D9341SetTarget(n, x, y, z)
	Return True
End Function

Function D9341ForceRoomDoorsOpen(r.Rooms)
	Local i%
	
	If r = Null Then Return
	For i = 0 To 3
		If r\RoomDoors[i] <> Null Then
			r\RoomDoors[i]\dist = 0
			If r\RoomDoors[i]\open = False And r\RoomDoors[i]\locked = False And r\RoomDoors[i]\KeyCard <= 0 Then r\RoomDoors[i]\open = True
		EndIf
	Next
End Function

Function D9341ChooseNextObjective(n.NPCs)
	Local it.Items
	Local r.Rooms
	
	If (Not D9341HasFlag(n, D9341F_KEY1)) And (Not D9341HasFlag(n, D9341F_KEY2)) And (Not D9341HasFlag(n, D9341F_OMNI)) Then
		it = D9341FindNearestItem(n, "key1")
		If it <> Null Then
			n\PrevState = 10
			D9341SetItemTarget(n, it)
			Return
		EndIf
		it = D9341FindNearestItem(n, "key2")
		If it <> Null Then
			n\PrevState = 20
			D9341SetItemTarget(n, it)
			Return
		EndIf
		n\PrevState = 25
		n\State = 102
		n\State2 = 0
		n\State3 = 0
		Return
	EndIf
	
	If (Not D9341HasFlag(n, D9341F_KEY2)) And (Not D9341HasFlag(n, D9341F_OMNI)) Then
		it = D9341FindNearestItem(n, "key2")
		If it <> Null Then
			n\PrevState = 20
			D9341SetItemTarget(n, it)
			Return
		EndIf
		r = D9341FindNearestRoom(n, "room2testroom2")
		If r = Null Then r = D9341FindNearestRoom(n, "room2testroom")
		If r <> Null Then
			n\PrevState = 20
			D9341SetRoomTarget(n, r, 0)
			Return
		EndIf
		D9341AddFlag(n, D9341F_KEY2)
	EndIf
	
	If (Not D9341HasFlag(n, D9341F_OMNI)) Then
		r = D9341FindNearestRoom(n, "914")
		If r <> Null Then
			n\PrevState = 30
			D9341SetRoomTarget(n, r, 2)
			Return
		EndIf
		D9341AddFlag(n, D9341F_OMNI)
	EndIf
	
	If D9341LCZCheckpointLocked() And (Not D9341HasFlag(n, D9341F_LOCKDOWN)) Then
		r = D9341FindNearestRoom(n, "room2sl")
		If r <> Null Then
			n\PrevState = 40
			If r\Levers[0] <> 0 Then
				D9341SetTarget(n, EntityX(r\Levers[0], True), EntityY(r\Levers[0], True), EntityZ(r\Levers[0], True))
			Else
				D9341SetRoomTarget(n, r, 0)
			EndIf
			Return
		EndIf
		D9341AddFlag(n, D9341F_LOCKDOWN)
	EndIf
	
	If Not D9341HasFlag(n, D9341F_008) Then
		r = D9341FindNearestRoom(n, "008")
		If r <> Null Then
			n\PrevState = 50
			If r\RoomDoors[0] <> Null Then r\RoomDoors[0]\locked = False : r\RoomDoors[0]\open = True
			If r\RoomDoors[1] <> Null Then r\RoomDoors[1]\locked = False : r\RoomDoors[1]\open = True
			If r\Objects[1] <> 0 Then
				D9341SetRoomTarget(n, r, 1)
			Else
				D9341SetRoomTarget(n, r, 0)
			EndIf
			Return
		EndIf
		D9341AddFlag(n, D9341F_008)
	EndIf
	
	If RemoteDoorOn And (Not D9341HasFlag(n, D9341F_RCCOFF)) Then
		r = D9341FindNearestRoom(n, "room2ccont")
		If r <> Null Then
			n\PrevState = 60
			D9341SetRoomTarget(n, r, 5)
			Return
		EndIf
		RemoteDoorOn = False
		D9341AddFlag(n, D9341F_RCCOFF)
	EndIf
	
	If Not D9341HasFlag(n, D9341F_079) Then
		r = D9341FindNearestRoom(n, "room079")
		If r <> Null Then
			n\PrevState = 70
			D9341SetRoomTarget(n, r, 0)
			Return
		EndIf
		D9341AddFlag(n, D9341F_079)
	EndIf
	
	If Not D9341HasFlag(n, D9341F_RCCON) Then
		r = D9341FindNearestRoom(n, "room2ccont")
		If r <> Null Then
			n\PrevState = 80
			D9341SetRoomTarget(n, r, 5)
			Return
		EndIf
		RemoteDoorOn = True
		D9341AddFlag(n, D9341F_RCCON)
	EndIf
	
	If Not D9341HasFlag(n, D9341F_GATEB) Then
		r = D9341FindGateRoom(n)
		If r <> Null Then
			n\PrevState = 90
			D9341SetRoomTarget(n, r, 0)
			Return
		EndIf
		D9341AddFlag(n, D9341F_GATEB)
	EndIf
	
	n\State = 0
	n\CurrSpeed = 0
End Function

Function D9341InsideRoomName%(n.NPCs, roomName$)
	Local r.Rooms
	Local rn$
	
	If n = Null Then Return False
	For r.Rooms = Each Rooms
		If r\RoomTemplate <> Null Then
			rn = Lower(r\RoomTemplate\Name)
			If rn = Lower(roomName) Then
				If IsInRoom(r, EntityX(n\Collider, True), EntityZ(n\Collider, True)) Then Return True
			EndIf
		EndIf
	Next
	Return False
End Function

Function D9341FollowTargetRoom2SL%(n.NPCs, finalDist#)
	Local dist#
	Local prevDist#
	Local newDist#
	Local movedDist#
	Local wpX#
	Local wpY#
	Local wpZ#
	Local targetPvt%
	Local lookPvt%
	Local targetSpeed#
	Local turnDelta#
	Local d.Doors
	Local pathDoor.Doors
	Local doorDist#
	Local doorWait%
	Local sprinting%
	Local directMode%
	Local blocked%
	Local button%
	Local buttonDist#
	Local keyDoor%
	Local rn$
	Local r.Rooms
	Local hazard%
	Local hazardDist#
	Local pitX#
	Local pitZ#
	
	dist = Distance(EntityX(n\Collider), EntityZ(n\Collider), n\EnemyX, n\EnemyZ)
	If dist < finalDist Then Return True
	
	If D9341StartPlayerBypass(n) Then Return False
	
	If D9341MaybeStartTeslaReaction(n) Then Return False
	
	If n\PathStatus <> 1 Then
		n\PathStatus = FindPath(n, n\EnemyX, n\EnemyY + 0.1, n\EnemyZ)
		n\PathTimer = 0
		n\PathLocation = 0
		n\PathX = 0
		n\PathZ = 0
	EndIf
	
	If n\PathStatus = 1 Then
		While n\PathLocation <= 19
			If n\Path[n\PathLocation] = Null Then
				n\PathLocation = n\PathLocation + 1
			Else
				Exit
			EndIf
		Wend
		
		If n\PathLocation > 19 Then
			directMode = True
		Else
			wpX = EntityX(n\Path[n\PathLocation]\obj, True)
			wpY = EntityY(n\Path[n\PathLocation]\obj, True)
			wpZ = EntityZ(n\Path[n\PathLocation]\obj, True)
			If n\Path[n\PathLocation]\room <> Null Then
				If n\Path[n\PathLocation]\room\RoomTemplate <> Null Then
					rn = Lower(n\Path[n\PathLocation]\room\RoomTemplate\Name)
					If Instr(rn, "pit") > 0 Then
						pitX = EntityX(n\Path[n\PathLocation]\room\obj, True)
						pitZ = EntityZ(n\Path[n\PathLocation]\room\obj, True)
						If Distance(EntityX(n\Collider), EntityZ(n\Collider), pitX, pitZ) < 2.35 Then
							If Abs(EntityX(n\Collider) - pitX) > Abs(EntityZ(n\Collider) - pitZ) Then
								If EntityX(n\Collider) < pitX Then wpX = Min(wpX, pitX - 1.05) Else wpX = Max(wpX, pitX + 1.05)
							Else
								If EntityZ(n\Collider) < pitZ Then wpZ = Min(wpZ, pitZ - 1.05) Else wpZ = Max(wpZ, pitZ + 1.05)
							EndIf
						EndIf
					EndIf
				EndIf
			EndIf
			pathDoor = n\Path[n\PathLocation]\door
			If n\PathLocation > 0 Then
				If n\Path[n\PathLocation - 1] <> Null Then d = n\Path[n\PathLocation - 1]\door
			EndIf
			If pathDoor = Null Then pathDoor = d
			If pathDoor <> Null Then
				If n\LastDoor <> pathDoor Then
					n\LastDoor = pathDoor
					n\DoorRetry = 0
					n\DoorWaitTimer = 0
				EndIf
				If pathDoor\frameobj <> 0 Then
					doorDist = Distance(EntityX(n\Collider), EntityZ(n\Collider), EntityX(pathDoor\frameobj, True), EntityZ(pathDoor\frameobj, True))
				Else
					doorDist = Distance(EntityX(n\Collider), EntityZ(n\Collider), wpX, wpZ)
				EndIf
				keyDoor = D9341DoorKeycard(pathDoor)
				button = D9341DoorButton(n, pathDoor)
				If button <> 0 Then buttonDist = EntityDistance(n\Collider, button) Else buttonDist = 1000.0
				If keyDoor And pathDoor\open = False And button <> 0 Then
					wpX = EntityX(button, True)
					wpY = EntityY(button, True)
					wpZ = EntityZ(button, True)
					If buttonDist < 0.90 Then
						D9341TurnTowardPoint(n, EntityX(button, True), EntityZ(button, True), 5.0)
						n\CurrSpeed = CurveValue(0, n\CurrSpeed, 8.0)
						If D9341DoorUsable(n, pathDoor) Then
							If n\Angle < 1 Then
								n\Angle = 1
								n\State2 = 0
							EndIf
							n\State2 = n\State2 + FPSfactor
							If n\State2 > 70 * 0.85 Then
								D9341OpenDoorNow(n, pathDoor)
								n\Angle = 2
								n\State2 = 0
							EndIf
						Else
							n\PathStatus = 0
						EndIf
						Return False
					EndIf
				EndIf
				If doorDist < 0.78 Then
					If pathDoor\open = False Then
						If D9341DoorUsable(n, pathDoor) And (keyDoor = False Or button = 0) Then D9341OpenDoorNow(n, pathDoor)
					EndIf
					If D9341DoorReady(pathDoor) = False Then doorWait = True
				EndIf
				If keyDoor And pathDoor\open And D9341DoorReady(pathDoor) = False Then doorWait = True
				If D9341DoorReady(pathDoor) Then
					n\Angle = 0
					n\State2 = 0
					n\DoorWaitTimer = 0
				EndIf
				If keyDoor And n\DoorRetry >= 2 And D9341DoorReady(pathDoor) Then
					If pathDoor\LinkedDoor <> Null Then
						If pathDoor\LinkedDoor\frameobj <> 0 Then
							wpX = EntityX(pathDoor\LinkedDoor\frameobj, True)
							wpY = EntityY(pathDoor\LinkedDoor\frameobj, True)
							wpZ = EntityZ(pathDoor\LinkedDoor\frameobj, True)
							pathDoor = pathDoor\LinkedDoor
							doorWait = False
						EndIf
					EndIf
				EndIf
				If n\DoorRetry = 1 Then
					button = D9341DoorButton(n, pathDoor)
					If button <> 0 Then
						wpX = EntityX(button, True)
						wpY = EntityY(button, True)
						wpZ = EntityZ(button, True)
						buttonDist = EntityDistance(n\Collider, button)
						doorWait = False
						If buttonDist < 0.95 Then
							D9341TurnTowardPoint(n, EntityX(button, True), EntityZ(button, True), 5.0)
							PlaySound2(ButtonSFX, Camera, button, 12.0, 0.8)
							If D9341DoorUsable(n, pathDoor) Then
								If pathDoor\open = False Then D9341OpenDoorNow(n, pathDoor)
								pathDoor\dist = 0
								If pathDoor\fastopen < 1 Then pathDoor\fastopen = 1
								If pathDoor\LinkedDoor <> Null Then
									pathDoor\LinkedDoor\dist = 0
									If pathDoor\LinkedDoor\fastopen < 1 Then pathDoor\LinkedDoor\fastopen = 1
								EndIf
							EndIf
							n\DoorRetry = 2
							n\DoorWaitTimer = 0
							Return False
						EndIf
					Else
						n\DoorRetry = 2
						n\DoorWaitTimer = 0
					EndIf
				EndIf
				If doorWait Then
					n\DoorWaitTimer = n\DoorWaitTimer + FPSfactor
					If n\DoorRetry = 0 And n\DoorWaitTimer > 70 * 5.0 Then
						n\DoorRetry = 1
						n\DoorWaitTimer = 0
					ElseIf n\DoorRetry >= 2 And n\DoorWaitTimer > 70 * 5.0 Then
						If keyDoor And pathDoor\LinkedDoor <> Null Then
							n\DoorRetry = 1
							n\DoorWaitTimer = 0
							n\State3 = 0
							Return False
						EndIf
						n\PathStatus = 0
						n\PathTimer = 0
						n\PathLocation = 0
						n\PathX = 0
						n\PathZ = 0
						n\DoorRetry = 0
						n\DoorWaitTimer = 0
						n\LastDoor = Null
						Return False
					EndIf
				ElseIf n\DoorRetry = 0 Then
					n\DoorWaitTimer = 0
				EndIf
			Else
				n\LastDoor = Null
				n\DoorRetry = 0
				n\DoorWaitTimer = 0
			EndIf
		EndIf
	Else
		If D9341DirectTargetVisible(n, n\EnemyX, n\EnemyY + 0.25, n\EnemyZ) Or dist < 2.25 Then
			directMode = True
		Else
			n\CurrSpeed = CurveValue(0, n\CurrSpeed, 8.0)
			n\State3 = n\State3 + FPSfactor
			If n\State3 > 70 * 3.5 Then
				n\PathStatus = 0
				n\PathTimer = 0
				n\PathLocation = 0
				n\State3 = 0
			EndIf
			Return False
		EndIf
	EndIf
	
	If directMode Then
		wpX = n\EnemyX
		wpY = n\EnemyY
		wpZ = n\EnemyZ
	EndIf
	
	For r.Rooms = Each Rooms
		If r\RoomTemplate <> Null Then
			rn = Lower(r\RoomTemplate\Name)
			If Instr(rn, "pit") > 0 Then
				hazard = r\obj
				If r\Objects[7] <> 0 Then hazard = r\Objects[7]
				If hazard <> 0 Then
					pitX = EntityX(hazard, True)
					pitZ = EntityZ(hazard, True)
					hazardDist = Distance(EntityX(n\Collider), EntityZ(n\Collider), pitX, pitZ)
					If hazardDist < 2.25 Or Distance(wpX, wpZ, pitX, pitZ) < 1.35 Then
						If Abs(EntityX(n\Collider) - pitX) > Abs(EntityZ(n\Collider) - pitZ) Then
							If EntityX(n\Collider) < pitX Then wpX = Min(wpX, pitX - 1.65) Else wpX = Max(wpX, pitX + 1.65)
						Else
							If EntityZ(n\Collider) < pitZ Then wpZ = Min(wpZ, pitZ - 1.65) Else wpZ = Max(wpZ, pitZ + 1.65)
						EndIf
					EndIf
				EndIf
			EndIf
		EndIf
	Next
	
	prevDist = Distance(EntityX(n\Collider), EntityZ(n\Collider), wpX, wpZ)
	
	If n\SoundTimer > 0 Then
		n\SoundTimer = Max(n\SoundTimer - FPSfactor, 0)
		sprinting = True
	Else
		If n\IdleTimer <= 0 Then n\IdleTimer = 70 * Rnd(1.8, 4.4)
		n\IdleTimer = Max(n\IdleTimer - FPSfactor, 0)
		If n\IdleTimer <= 0 And dist > 6.5 And doorWait = False Then
			If Rand(1, 3) = 1 Or dist > 12.0 Then
				n\SoundTimer = 70 * Rnd(1.45, 2.75)
				n\IdleTimer = 70 * Rnd(2.4, 5.6)
				sprinting = True
			EndIf
		EndIf
	EndIf
	
	blocked = D9341AvoidBlockingNPC(n, 0.82)
	If doorWait Then
		targetSpeed = 0
	ElseIf blocked Then
		targetSpeed = 0.006
	ElseIf sprinting Then
		targetSpeed = 0.037
	Else
		targetSpeed = 0.021
	EndIf
	
	targetPvt = CreatePivot()
	lookPvt = CreatePivot()
	PositionEntity targetPvt, wpX, EntityY(n\Collider, True), wpZ, True
	PositionEntity lookPvt, EntityX(n\Collider, True), EntityY(n\Collider, True), EntityZ(n\Collider, True), True
	PointEntity lookPvt, targetPvt
	turnDelta = Abs(angleDist(EntityYaw(lookPvt, True), EntityYaw(n\Collider, True)))
	RotateEntity n\Collider, 0, CurveAngle(EntityYaw(lookPvt, True), EntityYaw(n\Collider, True), 7.0), 0, True
	FreeEntity lookPvt
	FreeEntity targetPvt
	
	If turnDelta > 70 Then targetSpeed = targetSpeed * 0.68
	If turnDelta > 110 Then targetSpeed = targetSpeed * 0.50
	
	n\CurrSpeed = CurveValue(targetSpeed, n\CurrSpeed, 12.0)
	MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
	newDist = Distance(EntityX(n\Collider), EntityZ(n\Collider), wpX, wpZ)
	movedDist = Distance(EntityX(n\Collider, True), EntityZ(n\Collider, True), n\PrevX, n\PrevZ)
	
	If doorWait Or blocked Then
		n\State3 = 0
		n\PathTimer = 0
		n\PrevX = EntityX(n\Collider, True)
		n\PrevZ = EntityZ(n\Collider, True)
	ElseIf targetSpeed > 0 Then
		If Abs(n\PathX - wpX) > 0.01 Or Abs(n\PathZ - wpZ) > 0.01 Then
			n\PathX = wpX
			n\PathZ = wpZ
			n\PathTimer = prevDist
			n\State3 = 0
		EndIf
		If n\PathTimer <= 0 Then n\PathTimer = prevDist
		If newDist < n\PathTimer - (0.010 * FPSfactor) Then
			n\State3 = 0
		ElseIf movedDist < 0.0035 * FPSfactor Then
			n\State3 = n\State3 + FPSfactor
		ElseIf newDist + 0.02 >= n\PathTimer Then
			n\State3 = n\State3 + FPSfactor
		Else
			n\State3 = 0
		EndIf
		n\PathTimer = newDist
		n\PrevX = EntityX(n\Collider, True)
		n\PrevZ = EntityZ(n\Collider, True)
		If ((pathDoor <> Null And n\State3 > 70 * 5.0) Or (pathDoor = Null And n\State3 > 70 * 3.5)) Then
			If pathDoor <> Null Then
				If n\DoorRetry = 0 Then
					n\DoorRetry = 1
					n\DoorWaitTimer = 0
					n\State3 = 0
					Return False
				EndIf
				n\PathStatus = 0
				n\PathTimer = 0
				n\PathLocation = 0
				n\State3 = 0
				n\PathX = 0
				n\PathZ = 0
				n\DoorRetry = 0
				n\DoorWaitTimer = 0
				n\LastDoor = Null
				Return False
			EndIf
			If movedDist < 0.0035 * FPSfactor And n\CurrSpeed > 0.004 Then
				D9341NudgeTowardPoint(n, wpX, wpZ, 0.08)
				If n\PathStatus = 1 And newDist < 1.20 And n\PathLocation < 19 Then
					n\PathLocation = n\PathLocation + 1
					If D9341StartCloseBehind(n, pathDoor, wpX, wpZ) = False Then D9341ClosePathDoorBehind(n, pathDoor)
					n\PathTimer = 0
					n\State3 = 0
					n\PathX = 0
					n\PathZ = 0
				Else
					n\PathStatus = 0
					n\PathTimer = 0
					n\PathLocation = 0
					n\State3 = 0
					n\PathX = 0
					n\PathZ = 0
				EndIf
			Else
				n\PathStatus = 0
				n\PathTimer = 0
				n\PathLocation = 0
				n\State3 = 0
				n\PathX = 0
				n\PathZ = 0
			EndIf
		EndIf
	EndIf
	
	If doorWait = False And blocked = False Then
		If n\PathStatus = 1 Then
				If (newDist < 0.30) Or ((prevDist < newDist) And (prevDist < 0.60)) Then
					n\PathLocation = n\PathLocation + 1
					If D9341StartCloseBehind(n, pathDoor, wpX, wpZ) = False Then D9341ClosePathDoorBehind(n, pathDoor)
					n\PathTimer = 0
					n\State3 = 0
					n\PathX = 0
					n\PathZ = 0
			EndIf
		EndIf
	EndIf
	
	Return False
End Function

Function D9341FollowTarget%(n.NPCs, finalDist#)
	Local dist#
	Local prevDist#
	Local newDist#
	Local movedDist#
	Local wpX#
	Local wpY#
	Local wpZ#
	Local targetPvt%
	Local lookPvt%
	Local targetSpeed#
	Local turnDelta#
	Local d.Doors
	Local pathDoor.Doors
	Local doorDist#
	Local doorWait%
	Local sprinting%
	Local directMode%
	Local blocked%
	Local button%
	Local buttonDist#
	Local keyDoor%
	Local rn$
	Local r.Rooms
	Local hazard%
	Local hazardDist#
	Local pitX#
	Local pitZ#
	Local center1048%
	Local centerX#
	Local centerZ#
	Local segX#
	Local segZ#
	Local segLenSq#
	Local segT#
	Local nearX#
	Local nearZ#
	Local targetDX#
	Local targetDZ#
	Local targetLen#
	Local perpX#
	Local perpZ#
	Local side#
	Local avoid1048%
	Local stuckLimit#
	
	dist = Distance(EntityX(n\Collider), EntityZ(n\Collider), n\EnemyX, n\EnemyZ)
	If dist < finalDist Then Return True
	
	If D9341StartPlayerBypass(n) Then Return False
	
	If D9341MaybeStartTeslaReaction(n) Then Return False
	
	If n\PathStatus <> 1 Then
		n\PathStatus = FindPath(n, n\EnemyX, n\EnemyY + 0.1, n\EnemyZ)
		n\PathTimer = 0
		n\PathLocation = 0
		n\PathX = 0
		n\PathZ = 0
	EndIf
	
	If n\PathStatus = 1 Then
		While n\PathLocation <= 19
			If n\Path[n\PathLocation] = Null Then
				n\PathLocation = n\PathLocation + 1
			Else
				Exit
			EndIf
		Wend
		
		If n\PathLocation > 19 Then
			directMode = True
		Else
			wpX = EntityX(n\Path[n\PathLocation]\obj, True)
			wpY = EntityY(n\Path[n\PathLocation]\obj, True)
			wpZ = EntityZ(n\Path[n\PathLocation]\obj, True)
			If n\Path[n\PathLocation]\room <> Null Then
				If n\Path[n\PathLocation]\room\RoomTemplate <> Null Then
					rn = Lower(n\Path[n\PathLocation]\room\RoomTemplate\Name)
					If Instr(rn, "pit") > 0 Then
						pitX = EntityX(n\Path[n\PathLocation]\room\obj, True)
						pitZ = EntityZ(n\Path[n\PathLocation]\room\obj, True)
						If Distance(EntityX(n\Collider), EntityZ(n\Collider), pitX, pitZ) < 2.35 Then
							If Abs(EntityX(n\Collider) - pitX) > Abs(EntityZ(n\Collider) - pitZ) Then
								If EntityX(n\Collider) < pitX Then wpX = Min(wpX, pitX - 1.05) Else wpX = Max(wpX, pitX + 1.05)
							Else
								If EntityZ(n\Collider) < pitZ Then wpZ = Min(wpZ, pitZ - 1.05) Else wpZ = Max(wpZ, pitZ + 1.05)
							EndIf
						EndIf
					EndIf
				EndIf
			EndIf
			pathDoor = n\Path[n\PathLocation]\door
			If n\PathLocation > 0 Then
				If n\Path[n\PathLocation - 1] <> Null Then d = n\Path[n\PathLocation - 1]\door
			EndIf
			If pathDoor = Null Then pathDoor = d
			If pathDoor <> Null Then
				If n\LastDoor <> pathDoor Then
					n\LastDoor = pathDoor
					n\DoorRetry = 0
					n\DoorWaitTimer = 0
				EndIf
				If pathDoor\frameobj <> 0 Then
					doorDist = Distance(EntityX(n\Collider), EntityZ(n\Collider), EntityX(pathDoor\frameobj, True), EntityZ(pathDoor\frameobj, True))
				Else
					doorDist = Distance(EntityX(n\Collider), EntityZ(n\Collider), wpX, wpZ)
				EndIf
				keyDoor = D9341DoorKeycard(pathDoor)
				button = D9341DoorButton(n, pathDoor)
				If button <> 0 Then buttonDist = EntityDistance(n\Collider, button) Else buttonDist = 1000.0
				If keyDoor And pathDoor\locked And D9341LCZCheckpointLocked() Then
					If Int(n\PrevState) <> 40 Then
						If D9341RouteToLockdownSwitch(n) Then Return False
					ElseIf D9341DoorInRoom(pathDoor, "room2sl") Then
						pathDoor\locked = False
						If pathDoor\LinkedDoor <> Null Then pathDoor\LinkedDoor\locked = False
					EndIf
				EndIf
				If keyDoor And pathDoor\open = False And button <> 0 Then
					wpX = EntityX(button, True)
					wpY = EntityY(button, True)
					wpZ = EntityZ(button, True)
					If buttonDist < 0.90 Then
						D9341TurnTowardPoint(n, EntityX(button, True), EntityZ(button, True), 5.0)
						n\CurrSpeed = CurveValue(0, n\CurrSpeed, 8.0)
						If D9341DoorUsable(n, pathDoor) Then
							If n\Angle < 1 Then
								n\Angle = 1
								n\State2 = 0
							EndIf
							n\State2 = n\State2 + FPSfactor
							If n\State2 > 70 * 0.85 Then
								D9341OpenDoorNow(n, pathDoor)
								n\Angle = 2
								n\State2 = 0
							EndIf
						Else
							If Int(n\PrevState) <> 40 Then
								If D9341RouteToLockdownSwitch(n) Then Return False
							ElseIf D9341DoorInRoom(pathDoor, "room2sl") Then
								pathDoor\locked = False
								If pathDoor\LinkedDoor <> Null Then pathDoor\LinkedDoor\locked = False
								D9341OpenDoorNow(n, pathDoor)
							Else
								n\PathStatus = 0
							EndIf
						EndIf
						Return False
					EndIf
				EndIf
				If doorDist < 0.78 Then
					If pathDoor\open = False Then
						If D9341DoorUsable(n, pathDoor) And (keyDoor = False Or button = 0) Then D9341OpenDoorNow(n, pathDoor)
					EndIf
					If D9341DoorReady(pathDoor) = False Then doorWait = True
				EndIf
				If keyDoor And pathDoor\open And D9341DoorReady(pathDoor) = False Then doorWait = True
				If D9341DoorReady(pathDoor) Then
					n\Angle = 0
					n\State2 = 0
					n\DoorWaitTimer = 0
				EndIf
				If keyDoor And n\DoorRetry >= 2 And D9341DoorReady(pathDoor) Then
					If pathDoor\LinkedDoor <> Null Then
						If pathDoor\LinkedDoor\frameobj <> 0 Then
							wpX = EntityX(pathDoor\LinkedDoor\frameobj, True)
							wpY = EntityY(pathDoor\LinkedDoor\frameobj, True)
							wpZ = EntityZ(pathDoor\LinkedDoor\frameobj, True)
							pathDoor = pathDoor\LinkedDoor
							doorWait = False
						EndIf
					EndIf
				EndIf
				If n\DoorRetry = 1 And pathDoor\open = False Then
					button = D9341DoorButton(n, pathDoor)
					If button <> 0 Then
						wpX = EntityX(button, True)
						wpY = EntityY(button, True)
						wpZ = EntityZ(button, True)
						buttonDist = EntityDistance(n\Collider, button)
						doorWait = False
						If buttonDist < 0.95 Then
							D9341TurnTowardPoint(n, EntityX(button, True), EntityZ(button, True), 5.0)
							PlaySound2(ButtonSFX, Camera, button, 12.0, 0.8)
							If D9341DoorUsable(n, pathDoor) Then
								If pathDoor\open = False Then D9341OpenDoorNow(n, pathDoor)
								pathDoor\dist = 0
								If pathDoor\fastopen < 1 Then pathDoor\fastopen = 1
								If pathDoor\LinkedDoor <> Null Then
									pathDoor\LinkedDoor\dist = 0
									If pathDoor\LinkedDoor\fastopen < 1 Then pathDoor\LinkedDoor\fastopen = 1
								EndIf
							EndIf
							n\DoorRetry = 2
							n\DoorWaitTimer = 0
							Return False
						EndIf
					Else
						n\DoorRetry = 2
						n\DoorWaitTimer = 0
					EndIf
				ElseIf pathDoor\open Then
					n\DoorRetry = 0
					n\DoorWaitTimer = 0
				EndIf
				If doorWait Then
					n\DoorWaitTimer = n\DoorWaitTimer + FPSfactor
					If n\DoorRetry = 0 And n\DoorWaitTimer > 70 * 5.0 Then
						If keyDoor And Int(n\PrevState) <> 40 Then
							If D9341RouteToLockdownSwitch(n) Then Return False
						EndIf
						n\DoorRetry = 1
						n\DoorWaitTimer = 0
					ElseIf n\DoorRetry >= 2 And n\DoorWaitTimer > 70 * 5.0 Then
						If keyDoor And Int(n\PrevState) <> 40 Then
							If D9341RouteToLockdownSwitch(n) Then Return False
						EndIf
						If keyDoor And pathDoor\LinkedDoor <> Null Then
							n\DoorRetry = 1
							n\DoorWaitTimer = 0
							n\State3 = 0
							Return False
						EndIf
						n\PathStatus = 0
						n\PathTimer = 0
						n\PathLocation = 0
						n\PathX = 0
						n\PathZ = 0
						n\DoorRetry = 0
						n\DoorWaitTimer = 0
						n\LastDoor = Null
						Return False
					EndIf
				ElseIf n\DoorRetry = 0 Then
					n\DoorWaitTimer = 0
				EndIf
			Else
				n\LastDoor = Null
				n\DoorRetry = 0
				n\DoorWaitTimer = 0
			EndIf
		EndIf
	Else
		If D9341DirectTargetVisible(n, n\EnemyX, n\EnemyY + 0.25, n\EnemyZ) Or dist < 2.25 Then
			directMode = True
		Else
			n\CurrSpeed = CurveValue(0, n\CurrSpeed, 8.0)
			n\State3 = n\State3 + FPSfactor
			If n\State3 > 70 * 3.5 Then
				n\PathStatus = 0
				n\PathTimer = 0
				n\PathLocation = 0
				n\State3 = 0
			EndIf
			Return False
		EndIf
	EndIf
	
	If directMode Then
		wpX = n\EnemyX
		wpY = n\EnemyY
		wpZ = n\EnemyZ
	EndIf
	
	For r.Rooms = Each Rooms
		If r\RoomTemplate <> Null Then
			rn = Lower(r\RoomTemplate\Name)
			If Instr(rn, "pit") > 0 Then
				hazard = r\obj
				If r\Objects[7] <> 0 Then hazard = r\Objects[7]
				If hazard <> 0 Then
					pitX = EntityX(hazard, True)
					pitZ = EntityZ(hazard, True)
					hazardDist = Distance(EntityX(n\Collider), EntityZ(n\Collider), pitX, pitZ)
					If hazardDist < 2.25 Or Distance(wpX, wpZ, pitX, pitZ) < 1.35 Then
						If Abs(EntityX(n\Collider) - pitX) > Abs(EntityZ(n\Collider) - pitZ) Then
							If EntityX(n\Collider) < pitX Then wpX = Min(wpX, pitX - 1.65) Else wpX = Max(wpX, pitX + 1.65)
						Else
							If EntityZ(n\Collider) < pitZ Then wpZ = Min(wpZ, pitZ - 1.65) Else wpZ = Max(wpZ, pitZ + 1.65)
						EndIf
					EndIf
				EndIf
			EndIf
		EndIf
	Next
	
	center1048 = D93411048CenterEntity()
	If center1048 <> 0 Then
		centerX = EntityX(center1048, True)
		centerZ = EntityZ(center1048, True)
		If Distance(EntityX(n\Collider, True), EntityZ(n\Collider, True), centerX, centerZ) < 5.4 Then
			segX = wpX - EntityX(n\Collider, True)
			segZ = wpZ - EntityZ(n\Collider, True)
			segLenSq = segX * segX + segZ * segZ
			If segLenSq > 0.001 Then
				segT = ((centerX - EntityX(n\Collider, True)) * segX + (centerZ - EntityZ(n\Collider, True)) * segZ) / segLenSq
				If segT > 0 And segT < 1.15 Then
					nearX = EntityX(n\Collider, True) + segX * Min(segT, 1.0)
					nearZ = EntityZ(n\Collider, True) + segZ * Min(segT, 1.0)
					If Distance(nearX, nearZ, centerX, centerZ) < 1.75 Then
						targetDX = wpX - centerX
						targetDZ = wpZ - centerZ
						targetLen = Sqr(targetDX * targetDX + targetDZ * targetDZ)
						If targetLen < 0.10 Then
							targetDX = segX
							targetDZ = segZ
							targetLen = Sqr(targetDX * targetDX + targetDZ * targetDZ)
						EndIf
						If targetLen > 0.01 Then
							perpX = -targetDZ / targetLen
							perpZ = targetDX / targetLen
							side = (EntityX(n\Collider, True) - centerX) * perpX + (EntityZ(n\Collider, True) - centerZ) * perpZ
							If side < 0 Then side = -1 Else side = 1
							wpX = centerX + perpX * side * 1.75
							wpZ = centerZ + perpZ * side * 1.75
							avoid1048 = True
						EndIf
					EndIf
				EndIf
			EndIf
		EndIf
	EndIf
	
	prevDist = Distance(EntityX(n\Collider), EntityZ(n\Collider), wpX, wpZ)
	
	If n\SoundTimer > 0 Then
		n\SoundTimer = Max(n\SoundTimer - FPSfactor, 0)
		sprinting = True
	Else
		If n\IdleTimer <= 0 Then n\IdleTimer = 70 * Rnd(1.8, 4.4)
		n\IdleTimer = Max(n\IdleTimer - FPSfactor, 0)
		If n\IdleTimer <= 0 And dist > 6.5 And doorWait = False Then
			If Rand(1, 3) = 1 Or dist > 12.0 Then
				n\SoundTimer = 70 * Rnd(1.45, 2.75)
				n\IdleTimer = 70 * Rnd(2.4, 5.6)
				sprinting = True
			EndIf
		EndIf
	EndIf
	
	blocked = D9341AvoidBlockingNPC(n, 0.82)
	If avoid1048 Then sprinting = True
	If doorWait Then
		targetSpeed = 0
	ElseIf blocked Then
		targetSpeed = 0.006
	ElseIf sprinting Then
		targetSpeed = 0.037
	Else
		targetSpeed = 0.021
	EndIf
	
	targetPvt = CreatePivot()
	lookPvt = CreatePivot()
	PositionEntity targetPvt, wpX, EntityY(n\Collider, True), wpZ, True
	PositionEntity lookPvt, EntityX(n\Collider, True), EntityY(n\Collider, True), EntityZ(n\Collider, True), True
	PointEntity lookPvt, targetPvt
	turnDelta = Abs(angleDist(EntityYaw(lookPvt, True), EntityYaw(n\Collider, True)))
	RotateEntity n\Collider, 0, CurveAngle(EntityYaw(lookPvt, True), EntityYaw(n\Collider, True), 7.0), 0, True
	FreeEntity lookPvt
	FreeEntity targetPvt
	
	If turnDelta > 70 Then targetSpeed = targetSpeed * 0.68
	If turnDelta > 110 Then targetSpeed = targetSpeed * 0.50
	If Int(n\PrevState) <> 40 Then
		If D9341SteerAroundWall(n, wpX, wpZ) Then
			targetSpeed = Min(targetSpeed, 0.017)
			n\State3 = Max(n\State3, 70 * 1.5)
		EndIf
	EndIf
	
	n\CurrSpeed = CurveValue(targetSpeed, n\CurrSpeed, 12.0)
	MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
	newDist = Distance(EntityX(n\Collider), EntityZ(n\Collider), wpX, wpZ)
	movedDist = Distance(EntityX(n\Collider, True), EntityZ(n\Collider, True), n\PrevX, n\PrevZ)
	If doorWait = False And blocked = False And targetSpeed > 0.004 Then
		If movedDist < 0.0015 * FPSfactor Then
			If D9341LiftNudgeToward(n, wpX, wpZ) Then
				newDist = Distance(EntityX(n\Collider), EntityZ(n\Collider), wpX, wpZ)
				movedDist = Distance(EntityX(n\Collider, True), EntityZ(n\Collider, True), n\PrevX, n\PrevZ)
				n\State3 = 0
				n\PathTimer = newDist
			EndIf
		EndIf
	EndIf
	
	If doorWait Or blocked Then
		n\State3 = 0
		n\PathTimer = 0
		n\PrevX = EntityX(n\Collider, True)
		n\PrevZ = EntityZ(n\Collider, True)
	ElseIf targetSpeed > 0 Then
		If Abs(n\PathX - wpX) > 0.01 Or Abs(n\PathZ - wpZ) > 0.01 Then
			n\PathX = wpX
			n\PathZ = wpZ
			n\PathTimer = prevDist
			n\State3 = 0
		EndIf
		If n\PathTimer <= 0 Then n\PathTimer = prevDist
		If newDist < n\PathTimer - (0.010 * FPSfactor) Then
			n\State3 = 0
		ElseIf movedDist < 0.0035 * FPSfactor Then
			n\State3 = n\State3 + FPSfactor
		ElseIf newDist + 0.02 >= n\PathTimer Then
			n\State3 = n\State3 + FPSfactor
		Else
			n\State3 = 0
		EndIf
		n\PathTimer = newDist
		n\PrevX = EntityX(n\Collider, True)
		n\PrevZ = EntityZ(n\Collider, True)
		If pathDoor <> Null Then stuckLimit = 2.7 Else stuckLimit = 2.1
		If Int(n\PrevState) = 40 Then
			If pathDoor <> Null Then stuckLimit = 5.0 Else stuckLimit = 3.5
		EndIf
		If n\State3 > 70 * stuckLimit Then
			If pathDoor <> Null Then
				If movedDist < 0.0035 * FPSfactor And n\CurrSpeed > 0.004 Then
					If D9341LiftNudgeToward(n, wpX, wpZ) Then
						n\State3 = 0
						n\PathTimer = 0
						Return False
					EndIf
				EndIf
				If n\DoorRetry = 0 Then
					n\DoorRetry = 1
					n\DoorWaitTimer = 0
					n\State3 = 0
					Return False
				EndIf
				n\PathStatus = 0
				n\PathTimer = 0
				n\PathLocation = 0
				n\State3 = 0
				n\PathX = 0
				n\PathZ = 0
				n\DoorRetry = 0
				n\DoorWaitTimer = 0
				n\LastDoor = Null
				Return False
			EndIf
			If movedDist < 0.0035 * FPSfactor And n\CurrSpeed > 0.004 Then
				If D9341LiftNudgeToward(n, wpX, wpZ) Then
					n\State3 = 0
					n\PathTimer = 0
				ElseIf Int(n\PrevState) = 40 Then
					D9341NudgeTowardPoint(n, wpX, wpZ, 0.08)
					n\PathStatus = 0
					n\PathTimer = 0
					n\PathLocation = 0
					n\State3 = 0
					n\PathX = 0
					n\PathZ = 0
				Else
					D9341StartStuckWander(n)
				EndIf
			Else
				n\PathStatus = 0
				n\PathTimer = 0
				n\PathLocation = 0
				n\State3 = 0
				n\PathX = 0
				n\PathZ = 0
			EndIf
		EndIf
	EndIf
	
	If doorWait = False And blocked = False Then
		If n\PathStatus = 1 Then
				If (newDist < 0.30) Or ((prevDist < newDist) And (prevDist < 0.60)) Then
					n\PathLocation = n\PathLocation + 1
					D9341StartCloseBehind(n, pathDoor, wpX, wpZ)
					n\PathTimer = 0
					n\State3 = 0
					n\PathX = 0
					n\PathZ = 0
			EndIf
		EndIf
	EndIf
	
	Return False
End Function


Function D9341HandleInteraction(n.NPCs)
	Local it.Items
	Local r.Rooms
	Local e.Events
	Local spawnX#
	Local spawnY#
	Local spawnZ#
	Local opensfx914%
	Local closesfx914%
	Local outputDist#
	Local pvt%
	
	Select Int(n\PrevState)
		Case 10
			it = D9341FindNearestItem(n, "key1")
			If it <> Null Then
				If EntityDistance(n\Collider, it\collider) < 1.45 Then
					RemoveItem(it)
					D9341AddFlag(n, D9341F_KEY1)
					D9341ChooseNextObjective(n)
				Else
					D9341SetItemTarget(n, it)
				EndIf
			Else
				D9341AddFlag(n, D9341F_KEY1)
				D9341ChooseNextObjective(n)
			EndIf
		Case 20
			it = D9341FindNearestItem(n, "key2")
			If it <> Null Then
				If EntityDistance(n\Collider, it\collider) < 1.45 Then
					RemoveItem(it)
					D9341AddFlag(n, D9341F_KEY1)
					D9341AddFlag(n, D9341F_KEY2)
					D9341AddFlag(n, D9341F_TESTROOMCLOSE)
					D9341ChooseNextObjective(n)
				Else
					D9341SetItemTarget(n, it)
				EndIf
			Else
				D9341AddFlag(n, D9341F_KEY2)
				D9341ChooseNextObjective(n)
			EndIf
		Case 25
			n\State2 = n\State2 + FPSfactor
			If n\State2 > 70 * 1.5 Then
				r = D9341FindNearestRoom(n, "room2shaft")
				If r <> Null Then
					PositionEntity n\Collider, EntityX(r\obj, True), EntityY(r\obj, True) + 0.2, EntityZ(r\obj, True), True
					ResetEntity n\Collider
				EndIf
				D9341AddFlag(n, D9341F_LOCKDOWN)
				D9341AddFlag(n, D9341F_KEY1)
				D9341AddFlag(n, D9341F_KEY2)
				D9341AddFlag(n, D9341F_OMNI)
				D9341ChooseNextObjective(n)
			EndIf
		Case 30
			r = D9341FindNearestRoom(n, "914")
			If r = Null Then
				D9341AddFlag(n, D9341F_OMNI)
				D9341ChooseNextObjective(n)
			Else
				If D9341HasFlag(n, D9341F_914DROP) = False Then
					If r\RoomDoors[0] <> Null Then D9341OpenDoorNow(n, r\RoomDoors[0])
					If r\Objects[2] <> 0 Then
						If EntityDistance(n\Collider, r\Objects[2]) > 0.95 Then
							If D9341SetRoomTarget(n, r, 2) Then n\PrevState = 30
							Return
						EndIf
						D9341Remove914InputKey2(r)
						it = CreateItem("key2", EntityX(r\Objects[2], True), EntityY(r\Objects[2], True) + 0.1, EntityZ(r\Objects[2], True))
						EntityType(it\collider, HIT_ITEM)
						D9341RemoveFlag(n, D9341F_KEY2)
						D9341AddFlag(n, D9341F_914DROP)
						n\State3 = 0
					Else
						D9341AddFlag(n, D9341F_914DROP)
					EndIf
					Return
				EndIf
				If D9341HasFlag(n, D9341F_914APPROACH) = False Then
					If EntityDistance(n\Collider, r\obj) > 1.35 Then
						D9341SetTarget(n, EntityX(r\obj, True), EntityY(r\obj, True), EntityZ(r\obj, True))
						n\PrevState = 30
						Return
					EndIf
					D9341AddFlag(n, D9341F_914APPROACH)
					Return
				EndIf
				If D9341HasFlag(n, D9341F_914STARTED) = False Then
					If r\Objects[1] <> 0 Then
						If EntityDistance(n\Collider, r\Objects[1]) > 1.25 Then
							If D9341SetRoomTarget(n, r, 1) Then n\PrevState = 30
							Return
						EndIf
						D9341TurnTowardPoint(n, EntityX(r\Objects[1], True), EntityZ(r\Objects[1], True), 6.0)
						RotateEntity r\Objects[1], EntityPitch(r\Objects[1]), EntityYaw(r\Objects[1]), -40
						PlaySound2(ButtonSFX, Camera, r\Objects[1], 12.0, 1.0)
					EndIf
					For e.Events = Each Events
						If e\EventName = "914" And e\room = r Then
							If e\EventState <= 0 Then e\EventState = 1
							If e\SoundCHN = 0 Then e\SoundCHN = PlaySound2(MachineSFX, Camera, r\Objects[1], 12.0, 1.0)
						EndIf
					Next
					closesfx914 = LoadTempSound("SFX\SCP\914\DoorClose.ogg")
					If r\RoomDoors[0] <> Null Then r\RoomDoors[0]\open = False : r\RoomDoors[0]\SoundCHN = PlaySound2(closesfx914, Camera, r\RoomDoors[0]\obj, 12.0, 1.0)
					If r\RoomDoors[1] <> Null Then r\RoomDoors[1]\open = False : r\RoomDoors[1]\SoundCHN = PlaySound2(closesfx914, Camera, r\RoomDoors[1]\obj, 12.0, 1.0)
					If r\Objects[0] <> 0 Then RotateEntity r\Objects[0], EntityPitch(r\Objects[0]), EntityYaw(r\Objects[0]), 180
					D9341Remove914InputKey2(r)
					D9341AddFlag(n, D9341F_914STARTED)
					n\State3 = 0
					n\PrevY = 0
					Return
				EndIf
				If D9341HasFlag(n, D9341F_914OUTPUT) = False Then
					If r\Objects[3] <> 0 Then
						If n\PrevY < 70 * 8.5 Then
							outputDist = EntityDistance(n\Collider, r\Objects[3])
							If outputDist < 1.05 Or outputDist > 2.45 Then
								If D9341Set914OutputWaitTarget(n, r) Then n\PrevState = 30
								Return
							EndIf
							n\CurrSpeed = CurveValue(0, n\CurrSpeed, 8.0)
							If NoTarget Then
								If n\State3 <= 0 Then
									n\Angle = WrapAngle(EntityYaw(n\Collider, True) + Rnd(-85.0, 85.0))
									n\State3 = 70 * Rnd(0.9, 2.4)
								EndIf
								RotateEntity n\Collider, 0, CurveAngle(n\Angle, EntityYaw(n\Collider, True), 85.0), 0, True
								n\State3 = Max(n\State3 - FPSfactor, 0)
							Else
								If EntityDistance(n\Collider, Collider) < 8.0 Then TurnConsoleNPCToPlayer(n, 9.0)
							EndIf
							Return
						EndIf
					EndIf
					If n\PrevY > 70 * 8.5 Then
						D9341Remove914InputKey2(r)
						If D9341HasFlag(n, D9341F_914OMNISPAWN) = False Then
							D9341Remove914OutputOmni(r)
							If r\Objects[3] <> 0 Then
								spawnX = EntityX(r\Objects[3], True)
								spawnY = EntityY(r\Objects[3], True) + 0.1
								spawnZ = EntityZ(r\Objects[3], True)
							Else
								spawnX = EntityX(r\obj, True)
								spawnY = EntityY(r\obj, True) + 0.3
								spawnZ = EntityZ(r\obj, True)
							EndIf
							If r\RoomDoors[1] <> Null Then
								If r\RoomDoors[1]\frameobj <> 0 Then
									pvt = CreatePivot()
									PositionEntity pvt, EntityX(r\RoomDoors[1]\frameobj, True), EntityY(r\obj, True), EntityZ(r\RoomDoors[1]\frameobj, True), True
									RotateEntity pvt, 0, EntityYaw(r\RoomDoors[1]\frameobj, True), 0, True
									MoveEntity pvt, 0.34, 0, 0.68
									spawnX = EntityX(pvt, True)
									spawnY = EntityY(pvt, True) + 0.1
									spawnZ = EntityZ(pvt, True)
									FreeEntity pvt
								EndIf
							EndIf
							it = CreateItem("key6", spawnX, spawnY, spawnZ)
							EntityType(it\collider, HIT_ITEM)
							D9341AddFlag(n, D9341F_914OMNISPAWN)
						EndIf
						D9341AddFlag(n, D9341F_914OUTPUT)
						opensfx914 = LoadTempSound("SFX\SCP\914\DoorOpen.ogg")
						If r\RoomDoors[0] <> Null Then r\RoomDoors[0]\open = True : r\RoomDoors[0]\SoundCHN = PlaySound2(opensfx914, Camera, r\RoomDoors[0]\obj, 12.0, 1.0)
						If r\RoomDoors[1] <> Null Then r\RoomDoors[1]\open = True : r\RoomDoors[1]\SoundCHN = PlaySound2(opensfx914, Camera, r\RoomDoors[1]\obj, 12.0, 1.0)
						If r\Objects[0] <> 0 Then RotateEntity r\Objects[0], EntityPitch(r\Objects[0]), EntityYaw(r\Objects[0]), 0
						For e.Events = Each Events
							If e\EventName = "914" And e\room = r Then e\EventState = 0
						Next
						n\PrevState = 31
						n\State3 = 0
						n\Angle = 0
						If it <> Null Then D9341SetItemTarget(n, it)
					EndIf
				EndIf
			EndIf
		Case 31
			it = D9341FindNearestItem(n, "key6")
			If it <> Null Then
				If EntityDistance(n\Collider, it\collider) < 1.05 Then
					n\CurrSpeed = CurveValue(0, n\CurrSpeed, 8.0)
					n\State3 = n\State3 + FPSfactor
					If n\State3 > 70 * 0.75 Then
						RemoveItem(it)
						D9341AddFlag(n, D9341F_OMNI)
						D9341ChooseNextObjective(n)
					EndIf
				Else
					n\State3 = 0
					D9341SetItemTarget(n, it)
				EndIf
			Else
				D9341AddFlag(n, D9341F_OMNI)
				D9341ChooseNextObjective(n)
			EndIf
		Case 40
			For e.Events = Each Events
				If e\EventName = "room2sl" Then
					If e\room <> Null Then
						If e\room\Levers[0] <> 0 Then
							If Distance(EntityX(n\Collider), EntityZ(n\Collider), EntityX(e\room\Levers[0], True), EntityZ(e\room\Levers[0], True)) > 0.95 Then
								D9341SetTarget(n, EntityX(e\room\Levers[0], True), EntityY(e\room\Levers[0], True), EntityZ(e\room\Levers[0], True))
								n\PrevState = 40
								Return
							EndIf
							D9341TurnTowardPoint(n, EntityX(e\room\Levers[0], True), EntityZ(e\room\Levers[0], True), 6.0)
							PlaySound2(ButtonSFX, Camera, e\room\Levers[0], 8.0, 0.6)
							RotateEntity e\room\Levers[0], -80, EntityYaw(e\room\Levers[0]), 0
						EndIf
					EndIf
					e\EventState3 = 0
				EndIf
			Next
			TurnCheckpointMonitorsOff(0)
			D9341AddFlag(n, D9341F_LOCKDOWN)
			D9341ResumeObjective(n)
		Case 50
			For e.Events = Each Events
				If e\EventName = "008" Then
					e\EventState = 2
					If e\room <> Null Then
						If e\room\RoomDoors[0] <> Null Then e\room\RoomDoors[0]\locked = False : e\room\RoomDoors[0]\open = True
						If e\room\RoomDoors[1] <> Null Then e\room\RoomDoors[1]\locked = False : e\room\RoomDoors[1]\open = True
						If e\room\Levers[0] <> 0 Then
							If EntityDistance(n\Collider, e\room\Levers[0]) > 1.0 Then
								D9341SetTarget(n, EntityX(e\room\Levers[0], True), EntityY(e\room\Levers[0], True), EntityZ(e\room\Levers[0], True))
								n\PrevState = 50
								Return
							EndIf
							PlaySound2(ButtonSFX, Camera, e\room\Levers[0], 8.0, 0.6) : RotateEntity e\room\Levers[0], 35, EntityYaw(e\room\Levers[0]), 0
						EndIf
					EndIf
				EndIf
			Next
			D9341AddFlag(n, D9341F_008)
			D9341ChooseNextObjective(n)
		Case 60
			RemoteDoorOn = False
			For r.Rooms = Each Rooms
				If r\RoomTemplate <> Null Then
					If Lower(r\RoomTemplate\Name) = "room2ccont" Then
						If r\Objects[5] <> 0 Then PlaySound2(ButtonSFX, Camera, r\Objects[5], 12.0, 1.0) : RotateEntity r\Objects[5], -80, EntityYaw(r\Objects[5]), 0
					EndIf
				EndIf
			Next
			n\LastDist = 70 * 1.2
			D9341AddFlag(n, D9341F_RCCOFF)
			D9341ChooseNextObjective(n)
			D9341StartStuckWander(n)
		Case 70
			If n\State2 = 0 Then
				For e.Events = Each Events
					If e\EventName = "room079" Then
						e\EventState = 3
						e\EventState2 = 1
						If e\SoundCHN <> 0 Then
							StopStream_Strict(e\SoundCHN)
							e\SoundCHN = 0
							e\SoundCHN_isStream = False
						EndIf
						e\SoundCHN = StreamSound_Strict("SFX\SCP\079\Speech.ogg", SFXVolume, 0)
						e\SoundCHN_isStream = True
					EndIf
				Next
				n\State2 = 1
			Else
				n\State2 = n\State2 + FPSfactor
				If n\State2 > 70 * 14 Then
					D9341AddFlag(n, D9341F_079)
					D9341ChooseNextObjective(n)
				EndIf
			EndIf
		Case 80
			RemoteDoorOn = True
			For r.Rooms = Each Rooms
				If r\RoomTemplate <> Null Then
					If Lower(r\RoomTemplate\Name) = "room2ccont" Then
						If r\Objects[5] <> 0 Then PlaySound2(ButtonSFX, Camera, r\Objects[5], 12.0, 1.0) : RotateEntity r\Objects[5], 80, EntityYaw(r\Objects[5]), 0
					EndIf
				EndIf
			Next
			n\LastDist = 70 * 1.2
			D9341AddFlag(n, D9341F_RCCON)
			D9341ChooseNextObjective(n)
			D9341StartStuckWander(n)
		Case 90
			r = D9341NearestRoomByName(n, "gateaentrance")
			If r <> Null Then
				If EntityDistance(n\Collider, r\obj) < 8.0 Then
					If r\RoomDoors[0] <> Null Then r\RoomDoors[0]\locked = False : r\RoomDoors[0]\open = True
					If r\RoomDoors[1] <> Null Then r\RoomDoors[1]\locked = False : r\RoomDoors[1]\open = True
					For r.Rooms = Each Rooms
						If r\RoomTemplate <> Null Then
							If Lower(r\RoomTemplate\Name) = "gatea" Then
								If r\RoomDoors[1] <> Null Then r\RoomDoors[1]\locked = False : r\RoomDoors[1]\open = True
								If r\Objects[1] <> 0 Then
									PositionEntity n\Collider, EntityX(r\Objects[1], True), EntityY(r\Objects[1], True), EntityZ(r\Objects[1], True), True
									ResetEntity n\Collider
								EndIf
								D9341SetRoomTarget(n, r, 4)
								n\PrevState = 90
								Return
							EndIf
						EndIf
					Next
				EndIf
			EndIf
			r = D9341NearestRoomByName(n, "gatea")
			If r <> Null Then
				If EntityDistance(n\Collider, r\obj) < 18.0 Then
					If r\Objects[4] <> 0 Then
						If EntityDistance(n\Collider, r\Objects[4]) > 1.2 Then
							D9341SetRoomTarget(n, r, 4)
							n\PrevState = 90
							Return
						EndIf
					EndIf
				EndIf
			EndIf
			D9341AddFlag(n, D9341F_GATEB)
			n\State = 0
			n\CurrSpeed = 0
		Default
			D9341ChooseNextObjective(n)
	End Select
End Function

Function D9341StartMirrorReaction%(n.NPCs)
	If NoTarget Then Return False
	If n\LastSeen > 0 Then Return False
	If n\State >= 130 And n\State <> 180 Then Return False
	If n\State < 100 Then Return False
	If ConsoleNPCPlayerVisible(n, 4.8) = False Then Return False
	
	n\Idle = 101
	n\WanderTimer = 0
	n\GoalTimer = 0
	D9341ClearPath(n)
	n\State = 130
	n\State2 = 0
	n\State3 = 0
	n\SoundTimer = 0
	n\Angle = 0
	n\CurrSpeed = 0
	Return True
End Function

Function UpdateD9341BodyNPC(n.NPCs)
	RotateEntity(n\Collider, 0, EntityYaw(n\Collider), EntityRoll(n\Collider), True)
	If ThirdPerson Then
		n\NPCNameInSection = "Guard"
		n\BoneToManipulate = "head"
		n\ManipulationType = 0
		n\ManipulateBone = True
	Else
		n\ManipulateBone = False
	EndIf
	
	Select n\State
		Case 0
			n\CurrSpeed = CurveValue(0.0, n\CurrSpeed, 5.0)
			Animate2(n\obj, AnimTime(n\obj), 212, 235, 0.10)
		Case 1
			n\CurrSpeed = CurveValue(0.020, n\CurrSpeed, 5.0)
			Animate2(n\obj, AnimTime(n\obj), 236, 260, n\CurrSpeed * 15.0)
		Case 2
			n\CurrSpeed = CurveValue(0.030, n\CurrSpeed, 5.0)
			Animate2(n\obj, AnimTime(n\obj), 301, 319, n\CurrSpeed * 15.0)
		Case 3
			n\CurrSpeed = CurveValue(0.0, n\CurrSpeed, 5.0)
			Animate2(n\obj, AnimTime(n\obj), 357, 381, 0.08)
		Case 4
			n\CurrSpeed = CurveValue(0.020, n\CurrSpeed, 5.0)
			Animate2(n\obj, AnimTime(n\obj), 382, 406, n\CurrSpeed * 15.0)
		Case 5
			n\CurrSpeed = CurveValue(0.020, n\CurrSpeed, 5.0)
			Animate2(n\obj, AnimTime(n\obj), 281, 300, n\CurrSpeed * 16.0)
		Case 6
			n\CurrSpeed = CurveValue(0.020, n\CurrSpeed, 5.0)
			Animate2(n\obj, AnimTime(n\obj), 261, 280, n\CurrSpeed * 16.0)
		Case 7
			n\CurrSpeed = CurveValue(-0.012, n\CurrSpeed, 5.0)
			AnimateNPC(n, 260, 236, n\CurrSpeed * 25)
		Case 8
			n\CurrSpeed = CurveValue(-0.010, n\CurrSpeed, 5.0)
			AnimateNPC(n, 406, 382, n\CurrSpeed * 26)
		Case 9
			n\CurrSpeed = CurveValue(-0.018, n\CurrSpeed, 5.0)
			AnimateNPC(n, 319, 301, n\CurrSpeed * 26)
	End Select
End Function

Function D9341RoomAtPosition.Rooms(x#, y#, z#)
	Local r.Rooms
	Local best.Rooms
	Local dy#
	Local bestDy# = 1000000.0
	
	For r.Rooms = Each Rooms
		If r\obj <> 0 Then
			If IsInRoom(r, x, z) Then
				dy = Abs(y - EntityY(r\obj, True))
				If dy < bestDy Then
					bestDy = dy
					best = r
				EndIf
			EndIf
		EndIf
	Next
	
	Return best
End Function

Function D9341ClosestRoom.Rooms(x#, y#, z#)
	Local r.Rooms
	Local best.Rooms
	Local dist#
	Local bestDist# = 1000000.0
	
	For r.Rooms = Each Rooms
		If r\obj <> 0 Then
			dist = Distance(x, z, EntityX(r\obj, True), EntityZ(r\obj, True))
			If dist < bestDist Then
				bestDist = dist
				best = r
			EndIf
		EndIf
	Next
	
	Return best
End Function

Function D9341NearestRoomWaypoint.WayPoints(n.NPCs, r.Rooms)
	Local w.WayPoints
	Local best.WayPoints
	Local dist#
	Local bestDist# = 1000000.0
	Local x# = EntityX(n\Collider, True)
	Local y# = EntityY(n\Collider, True)
	Local z# = EntityZ(n\Collider, True)
	
	For w.WayPoints = Each WayPoints
		If w\obj <> 0 Then
			If r = Null Or w\room = r Then
				dist = Distance(x, z, EntityX(w\obj, True), EntityZ(w\obj, True)) + Abs(y - EntityY(w\obj, True)) * 0.18
				If dist < bestDist Then
					bestDist = dist
					best = w
				EndIf
			EndIf
		EndIf
	Next
	
	Return best
End Function

Function D9341ShowDoor(d.Doors)
	If d = Null Then Return
	d\dist = 0
	If d\obj <> 0 Then ShowEntity d\obj
	If d\obj2 <> 0 Then ShowEntity d\obj2
	If d\frameobj <> 0 Then ShowEntity d\frameobj
	If d\buttons[0] <> 0 Then ShowEntity d\buttons[0]
	If d\buttons[1] <> 0 Then ShowEntity d\buttons[1]
End Function

Function D9341ShowRoom(r.Rooms)
	Local i%
	Local d.Doors
	
	If r = Null Then Return
	If r\obj <> 0 Then ShowEntity r\obj
	For i = 0 To 7
		d = r\RoomDoors[i]
		If d <> Null Then
			D9341ShowDoor(d)
			If d\LinkedDoor <> Null Then D9341ShowDoor(d\LinkedDoor)
		EndIf
	Next
	For i = 0 To 3
		d = r\AdjDoor[i]
		If d <> Null Then
			D9341ShowDoor(d)
			If d\LinkedDoor <> Null Then D9341ShowDoor(d\LinkedDoor)
		EndIf
	Next
End Function

Function D9341KeepRouteLoaded(n.NPCs)
	Local r.Rooms
	Local targetRoom.Rooms
	Local pathRoom.Rooms
	Local i%
	
	r = D9341RoomAtPosition(EntityX(n\Collider, True), EntityY(n\Collider, True), EntityZ(n\Collider, True))
	If r = Null Then r = D9341ClosestRoom(EntityX(n\Collider, True), EntityY(n\Collider, True), EntityZ(n\Collider, True))
	If r <> Null Then
		D9341ShowRoom(r)
		For i = 0 To 3
			If r\Adjacent[i] <> Null Then D9341ShowRoom(r\Adjacent[i])
		Next
	EndIf
	
	If n\PathStatus = 1 Then
		For i = n\PathLocation To Min(n\PathLocation + 2, 19)
			If i >= 0 Then
				If n\Path[i] <> Null Then
					pathRoom = n\Path[i]\room
					If pathRoom <> Null Then D9341ShowRoom(pathRoom)
				EndIf
			EndIf
		Next
	EndIf
	
	If n\EnemyX <> 0 Or n\EnemyY <> 0 Or n\EnemyZ <> 0 Then
		targetRoom = D9341RoomAtPosition(n\EnemyX, n\EnemyY, n\EnemyZ)
		If targetRoom <> Null Then D9341ShowRoom(targetRoom)
	EndIf
End Function

Function D9341RecoverOutOfBounds%(n.NPCs)
	Local r.Rooms
	Local targetRoom.Rooms
	Local w.WayPoints
	Local targetWP.WayPoints
	Local x# = EntityX(n\Collider, True)
	Local y# = EntityY(n\Collider, True)
	Local z# = EntityZ(n\Collider, True)
	Local wx#
	Local wy#
	Local wz#
	Local bad%
	Local horizontal#
	
	r = D9341RoomAtPosition(x, y, z)
	If r <> Null Then
		w = D9341NearestRoomWaypoint(n, r)
	Else
		w = D9341NearestRoomWaypoint(n, Null)
	EndIf
	
	If y < -100.0 Or y > 150.0 Then bad = True
	
	If w <> Null Then
		wx = EntityX(w\obj, True)
		wy = EntityY(w\obj, True)
		wz = EntityZ(w\obj, True)
		horizontal = Distance(x, z, wx, wz)
		If y < wy - 2.4 Then bad = True
		If r = Null And horizontal > 4.5 Then bad = True
	Else
		If r = Null Then
			bad = True
		ElseIf y < EntityY(r\obj, True) - 3.0 Then
			bad = True
		EndIf
	EndIf
	
	If bad = False Then
		n\BoundsTimer = 0
		If r <> Null Then
			n\SafeRoom = r
			If w <> Null Then
				n\SafeX = EntityX(w\obj, True)
				n\SafeY = EntityY(w\obj, True) + 0.35
				n\SafeZ = EntityZ(w\obj, True)
			Else
				n\SafeX = EntityX(r\obj, True)
				n\SafeY = EntityY(r\obj, True) + 0.5
				n\SafeZ = EntityZ(r\obj, True)
			EndIf
		EndIf
		Return False
	EndIf
	
	n\BoundsTimer = n\BoundsTimer + FPSfactor
	If n\BoundsTimer < 70 * 0.8 Then Return False
	
	targetRoom = r
	If targetRoom = Null Then targetRoom = D9341ClosestRoom(x, y, z)
	If targetRoom = Null Then targetRoom = n\SafeRoom
	If targetRoom = Null Then Return False
	
	targetWP = D9341NearestRoomWaypoint(n, targetRoom)
	If targetWP <> Null Then
		n\SafeX = EntityX(targetWP\obj, True)
		n\SafeY = EntityY(targetWP\obj, True) + 0.35
		n\SafeZ = EntityZ(targetWP\obj, True)
	Else
		n\SafeX = EntityX(targetRoom\obj, True)
		n\SafeY = EntityY(targetRoom\obj, True) + 0.5
		n\SafeZ = EntityZ(targetRoom\obj, True)
	EndIf
	
	D9341ShowRoom(targetRoom)
	PositionEntity n\Collider, n\SafeX, n\SafeY, n\SafeZ, True
	ResetEntity n\Collider
	n\DropSpeed = 0
	n\CurrSpeed = 0
	n\BoundsTimer = 0
	n\SafeRoom = targetRoom
	n\DoorTarget = Null
	n\TeslaRoom = Null
	D9341ClearPath(n)
	n\State = 100
	n\State2 = 0
	n\State3 = 0
	Return True
End Function

Function D9341UpdateWanderTimeout%(n.NPCs)
	If n = Null Then Return False
	If n\State = 180 Or (n\State = 101 And Int(n\PrevState) = 0) Then
		If n\WanderTimer <= 0 Then n\WanderTimer = 70 * 60.0
		n\WanderTimer = Max(n\WanderTimer - FPSfactor, 0)
		If n\WanderTimer <= 0 Then
			n\State = 100
			n\State2 = 0
			n\State3 = 0
			n\CurrSpeed = 0
			n\GoalTimer = 0
			n\PrevState = 0
			D9341ClearPath(n)
			Return True
		EndIf
	Else
		n\WanderTimer = 0
	EndIf
	Return False
End Function

Function UpdateD9341GameNPC(n.NPCs)
	Local prevFrame#
	Local dist#
	Local targetSpeed#
	Local teslaRoom.Rooms
	Local button%
	Local buttonDist#
	Local sideStep#
	Local lever%
	Local r.Rooms
	
	D9341KeepRouteLoaded(n)
	D9341RecoverOutOfBounds(n)
	D9341KeepRoom2SLAccessOpen(n)
	RotateEntity(n\Collider, 0, EntityYaw(n\Collider), EntityRoll(n\Collider), True)
	prevFrame = AnimTime(n\obj)
	If n\LastSeen > 0 Then
		If NoTarget Or EntityDistance(n\Collider, Collider) > 14.0 Or ConsoleNPCPlayerVisible(n, 14.0) = False Then
			n\LastSeen = Max(n\LastSeen - FPSfactor, 0)
		EndIf
	EndIf
	If n\LastDist > 0 Then n\LastDist = Max(n\LastDist - FPSfactor, 0)
	If n\BlinkTimer > 0 Then n\BlinkTimer = Max(n\BlinkTimer - FPSfactor, 0)
	If Int(n\PrevState) = 30 Then
		If D9341HasFlag(n, D9341F_914STARTED) Then
			If D9341HasFlag(n, D9341F_914OUTPUT) = False Then n\PrevY = n\PrevY + FPSfactor
		EndIf
	EndIf
	D9341CloseTestroomAfterLeaving(n)
	D9341UpdateIdleWatchdog(n)
	D9341StartClickFollow(n)
	D9341UpdateWanderTimeout(n)
	If NoTarget And n\State >= 130 And n\State <= 132 Then
		n\State = n\Idle
		If n\State < 100 Then n\State = 101
		n\State2 = 0
		n\State3 = 0
		n\CurrSpeed = 0
	EndIf
	
	If D9341StartMirrorReaction(n) = False Then
		Select n\State
			Case 0
				n\CurrSpeed = CurveValue(0, n\CurrSpeed, 5.0)
			Case 100
				n\State2 = 0
				n\State3 = 0
				n\PrevState = 0
				n\Angle = 0
				D9341ChooseNextObjective(n)
			Case 101
				If D9341MaybeStartServerRoom(n) Then
				ElseIf Int(n\PrevState) = 40 And D9341InsideRoomName(n, "room2sl") Then
					If D9341FollowTargetRoom2SL(n, 0.85) Then
						n\State = 102
						n\State2 = 0
						n\State3 = 0
						n\Angle = 0
						n\PathStatus = 0
						n\PathLocation = 0
					EndIf
				ElseIf D9341FollowTarget(n, 0.85) Then
					n\State = 102
					n\State2 = 0
					n\State3 = 0
					n\Angle = 0
					n\PathStatus = 0
					n\PathLocation = 0
				EndIf
			Case 102
				n\CurrSpeed = CurveValue(0, n\CurrSpeed, 8.0)
				D9341HandleInteraction(n)
			Case 130
				TurnConsoleNPCToPlayer(n, 7.5)
				n\CurrSpeed = CurveValue(0, n\CurrSpeed, 7.0)
				n\State2 = n\State2 + FPSfactor
				If n\State2 > 70 * Rnd(0.55, 0.9) Then
					n\State = 131
					n\State2 = 0
					n\State3 = 0
					n\SoundTimer = 0
					n\Angle = 0
				EndIf
			Case 131
				TurnConsoleNPCToPlayer(n, 5.5)
				dist = EntityDistance(n\Collider, Collider)
				If n\SoundTimer <= 0 Then
					n\State3 = Rand(0, 5)
					n\SoundTimer = 70 * Rnd(0.22, 0.58)
					If Rand(1, 5) = 1 Then
						If n\Angle = 0 Then n\Angle = 1 Else n\Angle = 0
					EndIf
				Else
					n\SoundTimer = Max(n\SoundTimer - FPSfactor, 0)
				EndIf
				If dist < 1.0 Then
					targetSpeed = -Rnd(0.010, 0.018)
				ElseIf dist > 2.3 Then
					targetSpeed = Rnd(0.010, 0.020)
				Else
					Select Int(n\State3)
						Case 0
							targetSpeed = Rnd(0.008, 0.016)
						Case 1
							targetSpeed = -Rnd(0.008, 0.016)
						Case 2
							targetSpeed = Rnd(0.018, 0.030)
						Case 3
							targetSpeed = -Rnd(0.012, 0.022)
						Default
							targetSpeed = 0
					End Select
				EndIf
				If n\Angle = 1 Then targetSpeed = targetSpeed * 0.55
				n\CurrSpeed = CurveValue(targetSpeed, n\CurrSpeed, 6.0)
				MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
				n\State2 = n\State2 + FPSfactor
				If n\State2 > 70 * Rnd(2.1, 3.0) Then
					n\State = 132
					n\State2 = 0
					n\Angle = 0
				EndIf
			Case 132
				TurnConsoleNPCToPlayer(n, 8.0)
				n\CurrSpeed = CurveValue(-0.014, n\CurrSpeed, 7.0)
				MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
				n\State2 = n\State2 + FPSfactor
				If n\State2 > 70 * 0.75 Then
					n\LastSeen = 70 * 60
					D9341ResumeObjective(n)
				EndIf
			Case 140
				D9341TurnTowardPoint(n, n\PrevX, n\PrevZ, 5.5)
				dist = Distance(EntityX(n\Collider), EntityZ(n\Collider), n\PrevX, n\PrevZ)
				If dist > 1.18 Then
					targetSpeed = 0.014
				ElseIf n\State2 < 70 * 0.30 Then
					targetSpeed = 0
				ElseIf n\State2 < 70 * 0.50 Then
					targetSpeed = -0.006
				ElseIf n\State2 < 70 * 0.82 Then
					targetSpeed = 0.010
				Else
					targetSpeed = 0
				EndIf
				n\CurrSpeed = CurveValue(targetSpeed, n\CurrSpeed, 8.0)
				MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
				If dist <= 1.20 Then n\State2 = n\State2 + FPSfactor
				If (dist < 1.45 And n\State2 > 70 * 0.82) Or n\State2 > 70 * 1.15 Then
					teslaRoom = D9341NearestTeslaRoom(n, 3.2)
					D9341TriggerTeslaRoom(teslaRoom)
					n\State = 141
					n\State2 = 0
					n\CurrSpeed = 0
				EndIf
			Case 141
				D9341TurnTowardPoint(n, n\PathX, n\PathZ, 4.0)
				If n\State2 < 70 * 1.05 Then
					n\CurrSpeed = CurveValue(0, n\CurrSpeed, 5.0)
				Else
					n\CurrSpeed = CurveValue(0.052, n\CurrSpeed, 3.5)
				EndIf
				MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
				n\State2 = n\State2 + FPSfactor
				If Distance(EntityX(n\Collider), EntityZ(n\Collider), n\PathX, n\PathZ) < 0.8 Or n\State2 > 70 * 2.35 Then
					D9341ResetTeslaRoom(n\TeslaRoom)
					n\State = n\Idle
					If n\State < 100 Then n\State = 101
					n\State2 = 0
					n\State3 = 0
					n\LastDist = 70 * 4
					n\PathStatus = 0
				EndIf
			Case 150
				D9341TurnTowardPoint(n, n\PrevX, n\PrevZ, 8.0)
				buttonDist = Distance(EntityX(n\Collider), EntityZ(n\Collider), n\PathX, n\PathZ)
				If n\DoorTarget <> Null Then
					If n\DoorTarget\frameobj <> 0 Then
						dist = EntityDistance(n\Collider, n\DoorTarget\frameobj)
					Else
						dist = EntityDistance(n\Collider, n\DoorTarget\obj)
					EndIf
				Else
					dist = 0.7
				EndIf
				If n\State2 < 70 * 0.38 Or (dist > 0.62 And n\State2 < 70 * 0.95) Then
					n\CurrSpeed = CurveValue(-0.010, n\CurrSpeed, 4.5)
					sideStep = 0.0065 * n\State3
					If n\State2 > 70 * 0.38 Then sideStep = -sideStep * 0.45
					MoveEntity n\Collider, sideStep * FPSfactor, 0, n\CurrSpeed * FPSfactor
					AnimateNPC(n, 260, 236, n\CurrSpeed * 18.0)
				Else
					If n\DoorTarget <> Null Then
						button = D9341DoorButton(n, n\DoorTarget)
						If button <> 0 Then
							PlaySound2(ButtonSFX, Camera, button, 12.0, 0.75)
						Else
							PlaySound2(ButtonSFX, Camera, n\Collider, 12.0, 0.75)
						EndIf
						D9341CloseDoorNow(n\DoorTarget)
					Else
						PlaySound2(ButtonSFX, Camera, n\Collider, 12.0, 0.75)
						D9341CloseNearestDoorAt(n, n\PathX, n\PathZ)
					EndIf
					n\DoorTarget = Null
					n\State = n\Idle
					If n\State < 100 Then n\State = 101
					n\State2 = 0
					n\State3 = 0
					n\CurrSpeed = 0
				EndIf
				n\State2 = n\State2 + FPSfactor
			Case 170
				r = D9341NearestRoomByName(n, "room2servers")
				If r = Null Then
					D9341AddFlag(n, D9341F_SERVERS)
					D9341SetTarget(n, n\PrevX, n\Angle, n\PrevZ)
					n\State = 101
					n\State2 = 0
				ElseIf n\PrevY < 3 Then
					lever = r\Levers[Int(n\PrevY)]
					If lever <> 0 Then
						If Distance(EntityX(n\Collider), EntityZ(n\Collider), EntityX(lever, True), EntityZ(lever, True)) > 0.78 Then
							If Abs(n\EnemyX - EntityX(lever, True)) > 0.05 Or Abs(n\EnemyZ - EntityZ(lever, True)) > 0.05 Then
								D9341SetTarget(n, EntityX(lever, True), EntityY(lever, True), EntityZ(lever, True))
							EndIf
							D9341FollowTarget(n, 0.72)
						Else
							D9341TurnTowardPoint(n, EntityX(lever, True), EntityZ(lever, True), 6.0)
							n\CurrSpeed = CurveValue(0, n\CurrSpeed, 7.0)
							n\State2 = n\State2 + FPSfactor
							If n\State2 > 70 * 0.38 Then
								PlaySound2(LeverSFX, Camera, lever, 8.0, 0.7)
								If Int(n\PrevY) = 0 Then
									RotateEntity lever, -80, EntityYaw(lever), 0
								Else
									RotateEntity lever, 80, EntityYaw(lever), 0
								EndIf
								n\PrevY = n\PrevY + 1
								n\State2 = 0
								n\PathStatus = 0
							EndIf
						EndIf
					Else
						n\PrevY = n\PrevY + 1
					EndIf
				Else
					If r\RoomDoors[0] <> Null Then r\RoomDoors[0]\locked = False
					If r\RoomDoors[1] <> Null Then r\RoomDoors[1]\locked = False
					D9341AddFlag(n, D9341F_SERVERS)
					D9341SetTarget(n, n\PrevX, n\Angle, n\PrevZ)
					n\State = 101
					n\State2 = 0
					n\State3 = 0
					n\PathStatus = 0
				EndIf
			Case 190
				n\FollowTimer = Max(n\FollowTimer - FPSfactor, 0)
				If Collider = 0 Or n\FollowTimer <= 0 Or EntityDistance(n\Collider, Collider) > 10.0 Then
					n\LastSeen = 70 * 20
					D9341ResumeObjective(n)
				ElseIf n\State2 < 70 * 0.40 Then
					TurnConsoleNPCToPlayer(n, 6.0)
					n\CurrSpeed = CurveValue(0, n\CurrSpeed, 7.0)
					n\State2 = n\State2 + FPSfactor
				Else
					If Distance(EntityX(Collider, True), EntityZ(Collider, True), n\FollowX, n\FollowZ) > 0.65 Or n\PathStatus = 0 Then
						n\FollowX = EntityX(Collider, True)
						n\FollowZ = EntityZ(Collider, True)
						n\EnemyX = n\FollowX
						n\EnemyY = EntityY(Collider, True)
						n\EnemyZ = n\FollowZ
						D9341ClearPath(n)
						n\PathX = 0
						n\PathZ = 0
					EndIf
					If EntityDistance(n\Collider, Collider) > 1.25 Then
						D9341FollowTarget(n, 1.10)
					Else
						TurnConsoleNPCToPlayer(n, 7.0)
						n\CurrSpeed = CurveValue(0, n\CurrSpeed, 7.0)
					EndIf
				EndIf
			Case 191
				TurnConsoleNPCToPlayer(n, 8.0)
				n\CurrSpeed = CurveValue(-0.014, n\CurrSpeed, 7.0)
				MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
				n\State2 = n\State2 + FPSfactor
				If n\State2 > 70 * 0.75 Then
					n\LastSeen = 70 * 20
					D9341ResumeObjective(n)
				EndIf
			Case 180
				RotateEntity n\Collider, 0, CurveAngle(n\Angle, EntityYaw(n\Collider, True), 6.0), 0, True
				If D9341ForwardBlocked(n, EntityYaw(n\Collider, True), 0.52) Then
					n\Angle = WrapAngle(EntityYaw(n\Collider, True) + (72.0 * n\State3))
					n\State3 = -n\State3
				EndIf
				If n\State2 < 70 * 0.28 Then
					n\CurrSpeed = CurveValue(0, n\CurrSpeed, 5.0)
				ElseIf n\State2 < 70 * 1.05 Then
					n\CurrSpeed = CurveValue(0.018, n\CurrSpeed, 7.0)
					sideStep = 0.007 * n\State3
					If n\State2 > 70 * 0.68 Then sideStep = -sideStep
					MoveEntity n\Collider, sideStep * FPSfactor, 0, n\CurrSpeed * FPSfactor
				Else
					n\State = n\Idle
					If n\State < 100 Then n\State = 101
					n\State2 = 0
					n\State3 = 0
					n\CurrSpeed = 0
					n\PathStatus = 0
					n\PathTimer = 0
					n\PathLocation = 0
					n\PathX = 0
					n\PathZ = 0
				EndIf
				n\State2 = n\State2 + FPSfactor
			Case 160
				TurnConsoleNPCToPlayer(n, 5.5)
				If n\State2 < 70 * 0.24 Then
					n\CurrSpeed = CurveValue(-0.009, n\CurrSpeed, 5.5)
					MoveEntity n\Collider, 0, 0, n\CurrSpeed * FPSfactor
				ElseIf n\State2 < 70 * 0.62 Then
					n\CurrSpeed = CurveValue(0.006, n\CurrSpeed, 8.0)
					MoveEntity n\Collider, 0.010 * n\State3 * FPSfactor, 0, 0
				Else
					n\State = n\Idle
					If n\State < 100 Then n\State = 101
					n\State2 = 0
					n\State3 = 0
					n\CurrSpeed = 0
					n\PathStatus = 0
					n\PathTimer = 0
					n\PathLocation = 0
					n\PathX = 0
					n\PathZ = 0
				EndIf
				n\State2 = n\State2 + FPSfactor
			Default
				n\State = 100
		End Select
	EndIf
	
	If n\State = 150 And n\CurrSpeed < -0.002 Then
		AnimateNPC(n, 260, 236, n\CurrSpeed * 18.0)
	ElseIf n\State = 131 And n\Angle = 1 Then
		If n\CurrSpeed > 0.004 Then
			Animate2(n\obj, AnimTime(n\obj), 382, 406, Abs(n\CurrSpeed) * 22.0)
		ElseIf n\CurrSpeed < -0.004 Then
			AnimateNPC(n, 406, 382, Abs(n\CurrSpeed) * 28.0)
		Else
			Animate2(n\obj, AnimTime(n\obj), 357, 381, 0.08)
		EndIf
	ElseIf n\CurrSpeed > 0.030 Then
		Animate2(n\obj, AnimTime(n\obj), 301, 319, Abs(n\CurrSpeed) * 18.0)
		If prevFrame < 306 And AnimTime(n\obj) => 306 Then
			PlaySound2(StepSFX(GetStepSound(n\Collider), 0, Rand(0, 2)), Camera, n\Collider, 8.0, Rnd(0.35, 0.55))
		ElseIf prevFrame < 315 And AnimTime(n\obj) => 315 Then
			PlaySound2(StepSFX(GetStepSound(n\Collider), 0, Rand(0, 2)), Camera, n\Collider, 8.0, Rnd(0.35, 0.55))
		EndIf
	ElseIf n\CurrSpeed > 0.004 Then
		Animate2(n\obj, AnimTime(n\obj), 236, 260, Abs(n\CurrSpeed) * 22.0)
		If prevFrame < 244 And AnimTime(n\obj) => 244 Then
			PlaySound2(StepSFX(GetStepSound(n\Collider), 0, Rand(0, 2)), Camera, n\Collider, 8.0, Rnd(0.3, 0.5))
		ElseIf prevFrame < 256 And AnimTime(n\obj) => 256 Then
			PlaySound2(StepSFX(GetStepSound(n\Collider), 0, Rand(0, 2)), Camera, n\Collider, 8.0, Rnd(0.3, 0.5))
		EndIf
	ElseIf n\CurrSpeed < -0.004 Then
		AnimateNPC(n, 260, 236, n\CurrSpeed * 18.0)
	Else
		Animate2(n\obj, AnimTime(n\obj), 212, 235, 0.10)
	EndIf
	
	PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) - 0.32, EntityZ(n\Collider))
	RotateEntity n\obj, EntityPitch(n\Collider), EntityYaw(n\Collider) - 180.0, 0
End Function

Function UpdateNaziOfficerNPC(n.NPCs)
	Local pvt%
	Local visible%
	Local dist#
	
	If n\State < 100 Then
		If n\State = 0 Then AnimateNPC(n, 3, 26, 0.2, True)
		PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) - 0.32, EntityZ(n\Collider))
		RotateEntity n\obj, EntityPitch(n\Collider), EntityYaw(n\Collider) - 180.0, 0
		Return
	EndIf
	
	dist = EntityDistance(n\Collider, Collider)
	
	; If the player backs far enough away, return the officer to the initial idle state.
	; State3 is cleared so Officer1.ogg is only replayed once the player comes close again.
	If dist > 16.0 Then
		If n\SoundChn <> 0 Then StopChannel n\SoundChn : n\SoundChn = 0
		If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
		n\State = 100
		n\State2 = 0
		n\State3 = 0
		SetNPCFrame(n, 3)
		AnimateNPC(n, 3, 26, 0.2, True)
		PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) - 0.32, EntityZ(n\Collider))
		RotateEntity n\obj, EntityPitch(n\Collider), EntityYaw(n\Collider) - 180.0, 0
		Return
	EndIf
	
	TurnConsoleNPCToPlayer(n, 15.0)
	visible = ConsoleNPCPlayerVisible(n, 12.0)
	
	Select n\State
		Case 100
			AnimateNPC(n, 3, 26, 0.2, True)
			
			If n\State3 = 0 Then
				If visible Then
					SetNPCFrame(n, 3)
					PlaySound2(LoadTempSound("SFX\SCP\1123\Officer1.ogg"), Camera, n\Collider, 8.0)
					n\State3 = 1
					n\State2 = 0
				EndIf
			Else
				If visible Then
					n\State2 = n\State2 + FPSfactor
				Else
					n\State2 = Max(n\State2 - FPSfactor, 0.0)
				EndIf
			EndIf
			
			If n\State2 > 360 Then
				If visible Then
					PlaySound2(LoadTempSound("SFX\SCP\1123\Officer2.ogg"), Camera, n\Collider, 8.0)
					n\State = 101
					n\State2 = 0
				EndIf
			EndIf
		Case 101
			AnimateNPC(n, 3, 26, 0.2, True)
			
			If visible Then
				n\State2 = n\State2 + FPSfactor
			Else
				n\State2 = Max(n\State2 - FPSfactor, 0.0)
			EndIf
			
			If n\State2 > 280 Then
				If visible Then
					SetNPCFrame(n, 75)
					If n\Sound = 0 Then n\Sound = LoadSound_Strict("SFX\SCP\1123\Officer3.ogg")
					n\SoundChn = 0
					n\State = 102
					n\State2 = 0
				EndIf
			EndIf
		Case 102
			AnimateNPC(n, 75, 128, 0.04, True)
			
			If n\Sound = 0 Then n\Sound = LoadSound_Strict("SFX\SCP\1123\Officer3.ogg")
			
			If n\SoundChn <> 0 Then
				If (Not ChannelPlaying(n\SoundChn)) Then
					If ConsoleNPCPlayerClearShot(n, 12.0) Then
						PlaySound2(LoadTempSound("SFX\SCP\1123\Gunshot.ogg"), Camera, n\Collider, 35.0)
						pvt = CreatePivot()
						PositionEntity(pvt, EntityX(n\obj), EntityY(n\obj), EntityZ(n\obj))
						RotateEntity(pvt, EntityPitch(n\Collider), EntityYaw(n\Collider), 0, True)
						MoveEntity(pvt, 0.8 * 0.079, 10.75 * 0.079, 6.9 * 0.079)
						Shoot(EntityX(pvt), EntityY(pvt), EntityZ(pvt), 1.0, False, True)
						FreeEntity(pvt)
						n\State = 103
						If n\Sound <> 0 Then FreeSound_Strict n\Sound : n\Sound = 0
					Else
						; Lost the clean shot after the gun-load line, so reset and wait for the player again.
						n\SoundChn = 0
						n\State = 100
						n\State2 = 0
						n\State3 = 0
					EndIf
				EndIf
			EndIf
			
			If n\State = 102 Then
				If n\SoundChn = 0 Then
					n\SoundChn = PlaySound2(n\Sound, Camera, n\Collider, 15.0)
				Else
					n\SoundChn = LoopSound2(n\Sound, n\SoundChn, Camera, n\Collider, 15.0)
				EndIf
			EndIf
		Case 103
			AnimateNPC(n, 128, 128, 0.0, False)
	End Select
	
	PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) - 0.32, EntityZ(n\Collider))
	RotateEntity n\obj, EntityPitch(n\Collider), EntityYaw(n\Collider) - 180.0, 0
End Function

Function Update970CorpseNPC(n.NPCs)
	Local dist#
	Local y#
	Local floorY#
	Local ceilingY#
	Local targetY#
	
	If n\State < 100 Then
		PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) - 0.32, EntityZ(n\Collider))
		RotateEntity n\obj, EntityPitch(n\Collider), EntityYaw(n\Collider) - 180.0, 0
		Return
	EndIf
	
	n\IsDead = True
	dist = EntityDistance(Collider, n\Collider)
	
	Select n\State
		Case 100
			If n\State3 = 0 Then
				; This prevents low-ceiling rooms from being mistaken for the floor.
				ceilingY = ConsoleNPCFindCeilingY(EntityX(n\Collider), EntityY(n\Collider) + 0.05, EntityZ(n\Collider), EntityY(n\Collider) + 2.5)
				floorY = ConsoleNPCFindFloorY(EntityX(n\Collider), Min(EntityY(n\Collider) + 0.05, ceilingY - 0.05), EntityZ(n\Collider), EntityY(n\Collider) - 0.35)
				PositionEntity n\Collider, EntityX(n\Collider), floorY + 0.32, EntityZ(n\Collider)
				ResetEntity n\Collider
				SetNPCFrame(n, 80)
				n\IsDead = True
				EntityType n\Collider, HIT_DEAD
				n\EnemyY = floorY + 0.32
				n\EnemyX = ceilingY
				n\GravityMult = 0.0
				n\DropSpeed = 0.0
				n\FallingPickDistance = 0.0
				n\State2 = 0
				n\State3 = 1
			EndIf
			
			SetNPCFrame(n, 80)
			
			If dist < 3.0 Then
				If EntityInView(n\obj, Camera) Then
					CurrCameraZoom = (Sin(Float(MilliSecs()) / 20.0) + 1.0) * 15.0
					HeartBeatVolume = Max(CurveValue(0.3, HeartBeatVolume, 2.0), HeartBeatVolume)
					HeartBeatRate = Max(HeartBeatRate, 120)
				EndIf
			EndIf
			
			If dist < 8.0 Then n\State2 = n\State2 + FPSfactor
			If n\State2 > 420 Then
				n\State = 101
				n\GravityMult = 0.0
				n\DropSpeed = 0.0
			EndIf
		Case 101
			n\GravityMult = 0.0
			n\DropSpeed = 0.0
			n\FallingPickDistance = 0.0
			n\IsDead = True
			EntityType n\Collider, HIT_DEAD
			
			If n\Sound = 0 Then n\Sound = LoadSound_Strict("SFX\SCP\970\Corpse.ogg")
			n\SoundChn = LoopSound2(n\Sound, n\SoundChn, Camera, n\Collider, 8.0)
			
			AnimateNPC(n, 80, 61, -0.02, False)
			
			targetY = n\EnemyY + 0.85
			If n\EnemyX > n\EnemyY Then targetY = Min(targetY, n\EnemyX - 0.75)
			targetY = Max(targetY, n\EnemyY + 0.35)
			y = CurveValue(targetY + Sin(Float(MilliSecs()) / 20.0) * 0.1, EntityY(n\Collider), 50.0)
			PositionEntity n\Collider, EntityX(n\Collider), y, EntityZ(n\Collider)
			TurnEntity n\Collider, 0, 0.1 * FPSfactor, 0
			
			If dist < 3.0 Then
				CurrCameraZoom = (Sin(Float(MilliSecs()) / 20.0) + 1.0) * 15.0
				HeartBeatVolume = Max(CurveValue(0.3, HeartBeatVolume, 2.0), HeartBeatVolume)
				HeartBeatRate = Max(HeartBeatRate, 120)
			EndIf
	End Select
	
	PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) - 0.32, EntityZ(n\Collider))
	RotateEntity n\obj, EntityPitch(n\Collider), EntityYaw(n\Collider) - 180.0, 0
End Function

Function Update106VictimNPC(n.NPCs)
	Local de.Decals
	Local ceilingY#
	Local floorY#
	Local dist#
	Local fdir#
	
	If n\State < 100 Then
		PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) - 0.32, EntityZ(n\Collider))
		RotateEntity n\obj, EntityPitch(n\Collider), EntityYaw(n\Collider) - 180.0, 0
		Return
	EndIf
	
	Select n\State
		Case 100
			If n\State3 = 0 Then
				; While the decal appears, mimic the room event by keeping the victim out of sight.
				n\IsDead = False
				EntityType n\Collider, 0
				HideEntity n\obj
				
				; Find the ceiling above the spawn point, then find the actual floor below that ceiling.
				; The helper skips non-map hits instead of disabling the player collider, so spawning no longer nudges the player.
				ceilingY = ConsoleNPCFindCeilingY(EntityX(n\Collider), EntityY(n\Collider) + 0.05, EntityZ(n\Collider), EntityY(n\Collider) + 2.5)
				floorY = ConsoleNPCFindFloorY(EntityX(n\Collider), ceilingY - 0.05, EntityZ(n\Collider), EntityY(n\Collider) - 1.5)
				If floorY > ceilingY - 0.5 Then floorY = ConsoleNPCFindFloorY(EntityX(n\Collider), EntityY(n\Collider) + 0.05, EntityZ(n\Collider), EntityY(n\Collider) - 1.5)
				
				; Move the ceiling decal down a hair to prevent z-fighting/flicker against the ceiling surface.
				de.Decals = CreateDecal(0, EntityX(n\Collider), ceilingY - 0.005, EntityZ(n\Collider), -90, Rand(360), 0)
				If de <> Null Then
					de\Size = 0.05
					de\SizeChange = 0.0015
					EntityAlpha(de\obj, 0.8)
					UpdateDecals()
					PlaySound2(DecaySFX(3), Camera, de\obj, 15.0)
				EndIf
				
				PositionEntity n\Collider, EntityX(n\Collider), ceilingY + 0.85, EntityZ(n\Collider)
				ResetEntity n\Collider
				RotateEntity n\Collider, 0, Rnd(360), 0, True
				SetNPCFrame(n, 1)
				n\EnemyY = floorY + 0.32
				n\EnemyX = ceilingY
				n\FallingPickDistance = 0.0
				n\GravityMult = 0.0
				n\DropSpeed = 0.0
				n\State2 = 0
				n\State3 = 1
			EndIf
			
			n\IsDead = False
			EntityType n\Collider, 0
			HideEntity n\obj
			
			n\State2 = n\State2 + FPSfactor
			If n\State2 > 200 Then
				n\State = 101
				n\State2 = 0
				n\GravityMult = 1.0
				n\FallingPickDistance = 0.0
				n\DropSpeed = 0.0
				n\IsDead = False
				EntityType n\Collider, 0
				PositionEntity n\Collider, EntityX(n\Collider), n\EnemyX + 0.85, EntityZ(n\Collider)
				ResetEntity n\Collider
				ShowEntity n\obj
				PlaySound_Strict HorrorSFX(0)
				PlaySound2(DecaySFX(2), Camera, n\Collider, 15.0)
			EndIf
		Case 101
			n\FallingPickDistance = 0.0
			n\GravityMult = 1.0
			n\IsDead = False
			EntityType n\Collider, 0
			ShowEntity n\obj
			
			If EntityY(n\Collider) > n\EnemyY + 0.05 Then
				AnimateNPC(n, 1, 10, 0.12, False)
			Else
				PositionEntity n\Collider, EntityX(n\Collider), n\EnemyY, EntityZ(n\Collider)
				n\DropSpeed = 0.0
				n\GravityMult = 0.0
				EntityType n\Collider, HIT_DEAD
				AnimateNPC(n, 11, 19, 0.25, False)
				
				If n\Sound = 0 Then
					n\Sound = LoadSound_Strict("SFX\General\BodyFall.ogg")
					PlaySound_Strict n\Sound
					floorY = n\EnemyY - 0.32 + 0.001
					de.Decals = CreateDecal(0, EntityX(n\Collider), floorY, EntityZ(n\Collider), 90, Rand(360), 0)
					If de <> Null Then
						de\Size = 0.4
						EntityAlpha(de\obj, 0.8)
						UpdateDecals()
					EndIf
				EndIf
				
				n\State = 102
				n\State2 = 0
			EndIf
		Case 102
			n\DropSpeed = 0.0
			n\GravityMult = 0.0
			n\FallingPickDistance = 0.0
			n\IsDead = True
			EntityType n\Collider, HIT_DEAD
			PositionEntity n\Collider, EntityX(n\Collider), n\EnemyY, EntityZ(n\Collider)
			AnimateNPC(n, 11, 19, 0.25, False)
	End Select
	
	PositionEntity(n\obj, EntityX(n\Collider), EntityY(n\Collider) - 0.32, EntityZ(n\Collider))
	RotateEntity n\obj, EntityPitch(n\Collider), EntityYaw(n\Collider) - 180.0, 0
End Function

Function Console_SpawnNPC(c_input$, c_state$ = "")
	Local n.NPCs
	Local consoleMSG$
	
	Select c_input$ 
		Case "008", "008zombie"
			n.NPCs = CreateNPC(NPCtype008, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			n\State = 1
			consoleMSG = "SCP-008 infected human spawned."
			
		Case "049", "scp049", "scp-049"
			n.NPCs = CreateNPC(NPCtype049, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			n\State = 1
			consoleMSG = "SCP-049 spawned."
			
		Case "049-2", "0492", "scp-049-2", "scp049-2", "049zombie"
			n.NPCs = CreateNPC(NPCtypeZombie, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			n\State = 1
			consoleMSG = "SCP-049-2 spawned."
			
		Case "066", "scp066", "scp-066"
			n.NPCs = CreateNPC(NPCtype066, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			consoleMSG = "SCP-066 spawned."
			
		Case "096", "scp096", "scp-096"
			n.NPCs = CreateNPC(NPCtype096, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			n\State = 5
			If (Curr096 = Null) Then Curr096 = n
			consoleMSG = "SCP-096 spawned."
			
		Case "106", "scp106", "scp-106", "larry"
			n.NPCs = CreateNPC(NPCtypeOldMan, EntityX(Collider), EntityY(Collider) - 0.5, EntityZ(Collider))
			n\State = -1
			consoleMSG = "SCP-106 spawned."
			
		Case "173", "scp173", "scp-173", "statue"
			n.NPCs = CreateNPC(NPCtype173, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			Curr173 = n
			If (Curr173\Idle = 3) Then Curr173\Idle = False
			consoleMSG = "SCP-173 spawned."
		Case "372", "scp372", "scp-372"
			n.NPCs = CreateNPC(NPCtype372, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			consoleMSG = "SCP-372 spawned."
			
		Case "513-1", "5131", "scp513-1", "scp-513-1"
			n.NPCs = CreateNPC(NPCtype5131, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			consoleMSG = "SCP-513-1 spawned."
			
		Case "860-2", "8602", "scp860-2", "scp-860-2"
			CreateConsoleMsg("SCP-860-2 cannot be spawned with the console. Sorry!", 255, 0, 0)
			
		Case "939", "scp939", "scp-939"
			CreateConsoleMsg("SCP-939 instances cannot be spawned with the console. Sorry!", 255, 0, 0)

		Case "966", "scp966", "scp-966"
			n.NPCs = CreateNPC(NPCtype966, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			consoleMSG = "SCP-966 instance spawned."
			
		Case "1048a", "1048-a", "scp1048-a", "scp-1048-a", "scp1048a", "scp-1048a"
			n.NPCs = CreateNPC(NPCtype1048a, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			consoleMSG = "SCP-1048-A spawned."
			
		Case "1499-1", "14991", "scp-1499-1", "scp1499-1"
			n.NPCs = CreateNPC(NPCtype1499, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			consoleMSG = "SCP-1499-1 instance spawned."
			;NPCtypeD2, NPCtypeD3, NPCtypeS1, NPCtypeS2, NPCtypeB1, NPCtypeB2, NPCtypeV, NPCtypeC, NPCtypeG, NPCtypeJ,
		Case "class-d", "classd", "d"
			n.NPCs = CreateNPC(NPCtypeD, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			consoleMSG = "D-Class spawned."

		Case "class-d2", "classd2", "d2"
			n.NPCs = CreateNPC(NPCtypeD2, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			consoleMSG = "D-Class 2 spawned."
		
		Case "class-d3", "classd3", "d3"
			n.NPCs = CreateNPC(NPCtypeD3, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			consoleMSG = "D-Class spawned."

		Case "scientist", "scientist2", "franklin", "Security Chief Franklin", "Chief Franklin", "Franklin Carey"
			n.NPCs = CreateNPC(NPCtypeS2, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			consoleMSG = "Security Chief Franklin spawned."

		Case "scientist", "scientist1"
			n.NPCs = CreateNPC(NPCtypeS1, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			consoleMSG = "Scientist 1 spawned."
		
		Case "106victim", "Victim 106", "106 victim"
			n.NPCs = CreateNPC(NPCtypeV, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			n\State = 100
			n\State2 = 0
			n\State3 = 0
			consoleMSG = "SCP-106 Victim spawned."

		Case "corpse", "970 corpse", "corpse in 970", "body in 970", "corpse in 970"
			n.NPCs = CreateNPC(NPCtypeC, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			n\State = 100
			n\State2 = 0
			n\State3 = 0
			SetNPCFrame(n, 80)
			consoleMSG = "Corpse spawned."

		Case "dead class-d", "dead classd", "body", "body1"
			n.NPCs = CreateNPC(NPCtypeB1, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			n\State = 3
			n\IsDead = True
			EntityType n\Collider, HIT_DEAD
			SetNPCFrame(n, 80)
			consoleMSG = "Dead D-Class spawned."

		Case "dead scientist", "deadscientist", "body2", "nuke body", "nuke", "nukebody", "nuke corpse", "nukecorpse", "warhead room corpse"
			n.NPCs = CreateNPC(NPCtypeB2, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			n\State = 3
			n\IsDead = True
			EntityType n\Collider, HIT_DEAD
			SetNPCFrame(n, 40)
			consoleMSG = "Dead Scientist spawned."

		Case "janitor", "janitorial", "endroom janitor"
			n.NPCs = CreateNPC(NPCtypeJ, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			consoleMSG = "Janitor spawned."
		
		Case "gonzales", "jim gonzales", "gonzales jim"
			n.NPCs = CreateNPC(NPCtypeG, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			n\State = 3
			n\IsDead = True
			EntityType n\Collider, HIT_DEAD
			SetNPCFrame(n, 80)
			consoleMSG = "Gonzales Jim spawned."
		
		Case "nazi", "german", "naziofficer", "nazi officer", "ss officer", "ss"
			n.NPCs = CreateNPC(NPCtypeNazi, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			n\State = 100
			n\State2 = 0
			n\State3 = 0
			consoleMSG = "Nazi Officer spawned."

		Case "d9341", "d-9341", "benjamin walker", "benjamin oliver walker", "benjamin"
			n.NPCs = CreateNPC(NPCtypeD9341, EntityX(Collider), EntityY(Collider) + 0.65, EntityZ(Collider) + 0.2)
			n\State = 100
			n\State2 = 0
			n\State3 = 0
			n\PrevState = 0
			n\Reload = 0
			n\IdleTimer = 70 * Rnd(3.0, 6.0)
			n\SoundTimer = 0
			ResetEntity n\Collider
			consoleMSG = "Subject D-9341 spawned."
			
		Case "guard"
			n.NPCs = CreateNPC(NPCtypeGuard, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			consoleMSG = "Guard spawned."
			
		Case "mtf"
			n.NPCs = CreateNPC(NPCtypeMTF, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			consoleMSG = "MTF unit spawned."
			
		Case "apache", "helicopter"
			n.NPCs = CreateNPC(NPCtypeApache, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			consoleMSG = "Apache spawned."
			
		Case "tentacle"
			n.NPCs = CreateNPC(NPCtypeTentacle, EntityX(Collider), EntityY(Collider), EntityZ(Collider))
			consoleMSG = "SCP-035 tentacle spawned."
			
		Case "clerk", "teslaclerk", "tesla clerk"
			n.NPCs = CreateNPC(NPCtypeClerk, EntityX(Collider), EntityY(Collider) + 0.2, EntityZ(Collider))
			n\State = 100
			n\State2 = 0
			n\State3 = 0
			consoleMSG = "Clerk spawned."
			
		Default 
			CreateConsoleMsg("NPC type not found.", 255, 0, 0) : Return
	End Select
	
	If n <> Null
		If c_state <> "" Then n\State = Float(c_state) : consoleMSG = consoleMSG + " (State = " + n\State + ")"
	EndIf
	
	CreateConsoleMsg(consoleMSG)
	
End Function

Function ManipulateNPCBones()
	Local n.NPCs,bone%,pvt%,target%,bonename$
	Local maxvalue#,minvalue#,offset#,smooth#
	Local i%
	Local tovalue#
	
	For n = Each NPCs
		If n\ManipulateBone
			bonename$ = GetNPCManipulationValue(n\NPCNameInSection,n\BoneToManipulate,"bonename",0)
			If n = CurrD9341 And ThirdPerson Then bonename$ = "Bip01_Head"
			If bonename$<>""
				pvt% = CreatePivot()
				bone% = FindChild(n\obj,bonename$)
				If bone% = 0 Then RuntimeErrorExt "ERROR: NPC bone "+Chr(34)+bonename$+Chr(34)+" does not exist."
				PositionEntity pvt%,EntityX(bone%,True),EntityY(bone%,True),EntityZ(bone%,True)
				target = Camera
				If n = CurrD9341 And ThirdPerson Then
					RotateEntity pvt, user_camera_pitch, EntityYaw(Collider, True), 0, True
					MoveEntity pvt, 0, 0, 5.0
					target = pvt
				EndIf
				Select n\ManipulationType
					Case 0 ;<--- looking at player
						For i = 1 To GetNPCManipulationValue(n\NPCNameInSection,n\BoneToManipulate,"controller_max",1)
							If GetNPCManipulationValue(n\NPCNameInSection,n\BoneToManipulate,"controlleraxis"+i,0) = "pitch"
								maxvalue# = GetNPCManipulationValue(n\NPCNameInSection,n\BoneToManipulate,"controlleraxis"+i+"_max",2)
								minvalue# = GetNPCManipulationValue(n\NPCNameInSection,n\BoneToManipulate,"controlleraxis"+i+"_min",2)
								offset# = GetNPCManipulationValue(n\NPCNameInSection,n\BoneToManipulate,"controlleraxis"+i+"_offset",2)
								If GetNPCManipulationValue(n\NPCNameInSection,n\BoneToManipulate,"controlleraxis"+i+"_inverse",3)
									tovalue = -DeltaPitch(bone,target)+offset
								Else
									tovalue = DeltaPitch(bone,target)+offset
								EndIf
								;n\BonePitch = CurveAngle(tovalue,n\BonePitch,20.0)
								smooth# = GetNPCManipulationValue(n\NPCNameInSection,n\BoneToManipulate,"controlleraxis"+i+"_smoothing",2)
								If smooth>0.0
									n\BonePitch = CurveAngle(tovalue,n\BonePitch,smooth)
								Else
									n\BonePitch = tovalue
								EndIf
								n\BonePitch = ChangeAngleValueForCorrectBoneAssigning(n\BonePitch)
								n\BonePitch = Max(Min(n\BonePitch,maxvalue),minvalue)
							ElseIf GetNPCManipulationValue(n\NPCNameInSection,n\BoneToManipulate,"controlleraxis1",0) = "yaw"
								maxvalue# = GetNPCManipulationValue(n\NPCNameInSection,n\BoneToManipulate,"controlleraxis"+i+"_max",2)
								minvalue# = GetNPCManipulationValue(n\NPCNameInSection,n\BoneToManipulate,"controlleraxis"+i+"_min",2)
								offset# = GetNPCManipulationValue(n\NPCNameInSection,n\BoneToManipulate,"controlleraxis"+i+"_offset",2)
								If GetNPCManipulationValue(n\NPCNameInSection,n\BoneToManipulate,"controlleraxis"+i+"_inverse",3)
									tovalue = -DeltaYaw(bone,target)+offset
								Else
									tovalue = DeltaYaw(bone,target)+offset
								EndIf
								;n\BoneYaw = CurveAngle(tovalue,n\BoneYaw,20.0)
								smooth# = GetNPCManipulationValue(n\NPCNameInSection,n\BoneToManipulate,"controlleraxis"+i+"_smoothing",2)
								If smooth>0.0
									n\BoneYaw = CurveAngle(tovalue,n\BoneYaw,smooth)
								Else
									n\BoneYaw = tovalue
								EndIf
								n\BoneYaw = ChangeAngleValueForCorrectBoneAssigning(n\BoneYaw)
								n\BoneYaw = Max(Min(n\BoneYaw,maxvalue),minvalue)
							;ElseIf --> (Roll Value)
							;	
							EndIf
						Next
						
						If n = CurrD9341 And ThirdPerson Then
							n\BoneYaw = 0
							n\BoneRoll = 0
							RotateEntity bone%,EntityPitch(bone),EntityYaw(bone)+n\BonePitch,EntityRoll(bone)
						Else
							RotateEntity bone%,EntityPitch(bone)+n\BonePitch,EntityYaw(bone)+n\BoneYaw,EntityRoll(bone)+n\BoneRoll
						EndIf
				End Select
				FreeEntity pvt%
			EndIf
		Else
			
		EndIf
	Next
	
End Function

Function GetNPCManipulationValue$(NPC$,bone$,section$,valuetype%=0)
	;valuetype determines what type of variable should the Output be returned
	;0 - String
	;1 - Int
	;2 - Float
	;3 - Boolean
	
	Local value$ = GetModdedINIString(NPCBones,NPC$,bone$+"_"+section$)
	Select valuetype%
		Case 0
			Return value$
		Case 1
			Return Int(value$)
		Case 2
			Return Float(value$)
		Case 3
			If value$ = "true" Or value$ = "1"
				Return True
			Else
				Return False
			EndIf
	End Select
	
End Function

Function ChangeAngleValueForCorrectBoneAssigning(value#)
	Local numb#
	
	If value# <= 180.0
		numb# = value#
	Else
		numb# = -360+value#
	EndIf
	
	Return numb#
End Function

Function NPCSpeedChange(n.NPCs)
	
	Select n\NPCtype
		Case NPCtype173,NPCtypeOldMan,NPCtype096,NPCtype049,NPCtype939,NPCtypeMTF
			Select SelectedDifficulty\otherFactors
				Case NORMAL
					n\Speed = n\Speed * 1.1
				Case HARD
					n\Speed = n\Speed * 1.2
			End Select
	End Select
	
End Function

Function PlayerInReachableRoom(canSpawnIn049Chamber%=False)
	Local RN$ = PlayerRoom\RoomTemplate\Name$
	Local e.Events, temp
	
	;Player is in these rooms, returning false
	If MTFDisabled = 1 Then
		Return False
	EndIf
	If RN = "pocketdimension" Or RN = "gatea" Or RN = "dimension1499" Or RN = "173" Then
		Return False
	EndIf
	;Player is at GateB and is at the surface, returning false
	If RN = "exit1" And EntityY(Collider)>1040.0*RoomScale Then
		Return False
	EndIf
	;Player is in 860's test room and inside the forest, returning false
	temp = False
	For e = Each Events
		If e\EventName$ = "room860" And e\EventState = 1.0 Then
			temp = True
			Exit
		EndIf
	Next
	If RN = "room860" And temp Then
		Return False
	EndIf
	If (Not canSpawnIn049Chamber) Then
		If SelectedDifficulty\aggressiveNPCs = False Then
			If RN = "room049" And EntityY(Collider)<=-2848*RoomScale Then
				Return False
			EndIf
		EndIf
	EndIf
	;Return true, this means player is in reachable room
	Return True
	
End Function

Function CheckForNPCInFacility(n.NPCs)
	;False (=0): NPC is not in facility (mostly meant for "dimension1499")
	;True (=1): NPC is in facility
	;2: NPC is in tunnels (maintenance tunnels/049 tunnels/939 storage room, etc...)
	
	If EntityY(n\Collider)>100.0
		Return False
	EndIf
	If EntityY(n\Collider)< -10.0
		Return 2
	EndIf
	If EntityY(n\Collider)> 7.0 And EntityY(n\Collider)<=100.0
		Return 2
	EndIf
	
	Return True
End Function

Function FindNextElevator(n.NPCs)
	Local eo.ElevatorObj, eo2.ElevatorObj
	
	For eo = Each ElevatorObj
		If eo\InFacility = n\InFacility
			If Abs(EntityY(eo\obj,True)-EntityY(n\Collider))<10.0
				For eo2 = Each ElevatorObj
					If eo2 <> eo
						If eo2\InFacility = n\InFacility
							If Abs(EntityY(eo2\obj,True)-EntityY(n\Collider))<10.0
								If EntityDistance(eo2\obj,n\Collider)<EntityDistance(eo\obj,n\Collider)
									n\PathStatus = FindPath(n, EntityX(eo2\obj,True),EntityY(eo2\obj,True),EntityZ(eo2\obj,True))
									n\CurrElevator = eo2
									DebugLog "eo2 found for "+n\NPCtype
									Exit
								EndIf
							EndIf
						EndIf
					EndIf
				Next
				If n\CurrElevator = Null
					n\PathStatus = FindPath(n, EntityX(eo\obj,True),EntityY(eo\obj,True),EntityZ(eo\obj,True))
					n\CurrElevator = eo
					DebugLog "eo found for "+n\NPCtype
				EndIf
				If n\PathStatus <> 1
					n\CurrElevator = Null
					DebugLog "Unable to find elevator path: Resetting CurrElevator"
				EndIf
				Exit
			EndIf
		EndIf
	Next
	
End Function

Function GoToElevator(n.NPCs)
	Local dist#,inside%
	
	If n\PathStatus <> 1
		PointEntity n\obj,n\CurrElevator\obj
		RotateEntity n\Collider,0,CurveAngle(EntityYaw(n\obj),EntityYaw(n\Collider),20.0),0
		
		inside% = False
		If Abs(EntityX(n\Collider)-EntityX(n\CurrElevator\obj,True))<280.0*RoomScale
			If Abs(EntityZ(n\Collider)-EntityZ(n\CurrElevator\obj,True))<280.0*RoomScale Then
				If Abs(EntityY(n\Collider)-EntityY(n\CurrElevator\obj,True))<280.0*RoomScale Then
					inside% = True
				EndIf
			EndIf
		EndIf
		
		dist# = EntityDistance(n\Collider,n\CurrElevator\door\frameobj)
		If n\CurrElevator\door\open
			If (dist# > 0.4 And dist# < 0.7) And inside%
				UseDoor(n\CurrElevator\door,False)
				DebugLog n\NPCtype+" used elevator"
			EndIf
		Else
			If dist# < 0.7
				n\CurrSpeed = 0.0
				If n\CurrElevator\door\NPCCalledElevator=False
					n\CurrElevator\door\NPCCalledElevator = True
					DebugLog n\NPCtype+" called elevator"
				EndIf
			EndIf
		EndIf
	EndIf
	
End Function

Function FinishWalking(n.NPCs,startframe#,endframe#,speed#)
	Local centerframe#
	
	If n<>Null
		centerframe# = (endframe#-startframe#)/2
		If n\Frame >= centerframe#
			AnimateNPC(n,startframe#,endframe#,speed#,False)
		Else
			AnimateNPC(n,endframe#,startframe#,-speed#,False)
		EndIf
	EndIf
	
End Function

Function ChangeNPCTextureID(n.NPCs,textureid%)
	If (n=Null) Then
		CreateConsoleMsg("Tried to change the texture of an invalid NPC")
		If ConsoleOpening Then
			ConsoleOpen = True
		EndIf
		Return
	EndIf
	
	n\TextureID = textureid%+1
	FreeEntity n\obj
	n\obj = CopyEntity(DTextures[textureid%+1])
	
	temp# = 0.5 / MeshWidth(n\obj)
	ScaleEntity n\obj, temp, temp, temp
	MeshCullBox (n\obj, -MeshWidth(ClassDObj), -MeshHeight(ClassDObj), -MeshDepth(ClassDObj)*4, MeshWidth(ClassDObj)*2, MeshHeight(ClassDObj)*2, MeshDepth(ClassDObj)*8)
	
	SetNPCFrame(n,n\Frame)
	
End Function









;~IDEal Editor Parameters:
;~F#0
;~B#197#12A9#1343#13DC#1590#169B#185C#18B8
;~C#Blitz3D
