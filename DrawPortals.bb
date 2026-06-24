Type DrawPortal
	;Field x#,y#,z#
	Field w#,h#
	;Field pitch#,yaw#,roll#

	Field cam%,portal%
	;Field camx#,camy#,camz#
	Field camZoom#
	Field campitch#,camyaw#,camroll#
	Field tex%
	Field texw%,texh%
	Field id%

	; Subdivided doorway plane used by the screen-space portal mapping.
	Field surface%
	Field gridX%
	Field gridY%
End Type

Function BuildDrawPortalGrid(ndp.DrawPortal,gridX%,gridY%)
	Local gx%,gy%
	Local rowWidth%
	Local vTL%,vTR%,vBL%,vBR%
	Local fx#,fy#
	Local px#,py#

	If ndp\surface=0 Then Return

	; Remove the previous vertices and triangles before rebuilding at
	; the new subdivision level.
	ClearSurface ndp\surface,True,True

	ndp\gridX = gridX
	ndp\gridY = gridY

	For gy = 0 To ndp\gridY
		fy = Float(gy)/Float(ndp\gridY)
		py = (ndp\h/2.0)-(fy*ndp\h)

		For gx = 0 To ndp\gridX
			fx = Float(gx)/Float(ndp\gridX)
			px = (-ndp\w/2.0)+(fx*ndp\w)

			AddVertex ndp\surface,px,py,0.0,fx,fy
		Next
	Next

	rowWidth = ndp\gridX+1

	For gy = 0 To ndp\gridY-1
		For gx = 0 To ndp\gridX-1
			vTL = gx+(gy*rowWidth)
			vTR = (gx+1)+(gy*rowWidth)
			vBL = gx+((gy+1)*rowWidth)
			vBR = (gx+1)+((gy+1)*rowWidth)

			If ((gx+gy) Mod 2)=0 Then
				AddTriangle ndp\surface,vTL,vTR,vBL
				AddTriangle ndp\surface,vTR,vBR,vBL
			Else
				AddTriangle ndp\surface,vTL,vTR,vBR
				AddTriangle ndp\surface,vTL,vBR,vBL
			EndIf
		Next
	Next

	UpdateNormals ndp\portal
End Function


Function CreateDrawPortal.DrawPortal(x#,y#,z#,pitch#,yaw#,roll#,w#,h#,camx#=0.0,camy#=0.0,camz#=0.0,campitch#=0.0,camyaw#=0.0,camroll#=0.0,camZoom#=1.0,texw%=2048,texh%=2048)
	Local ndp.DrawPortal = New DrawPortal
	Local temp%
	Local gx%,gy%
	Local rowWidth%
	Local vTL%,vTR%,vBL%,vBR%
	Local fx#,fy#
	Local px#,py#

	ndp\w = w
	ndp\h = h

	; Render target texture. Clamp U/V so the screen-space mapping cannot wrap.
	ndp\tex = CreateTexture(texw,texh,1+16+32+256+1024)
	ndp\texw = TextureWidth(ndp\tex)
	ndp\texh = TextureHeight(ndp\tex)

	ndp\cam = CreateCamera()
	CameraViewport ndp\cam,(ndp\texw-GraphicWidth)/2,(ndp\texh-GraphicHeight)/2,GraphicWidth,GraphicHeight
	CameraRange ndp\cam,0.5,20.0
	PositionEntity ndp\cam,camx,camy,camz,True
	RotateEntity ndp\cam,campitch,camyaw,camroll,True
	CameraZoom ndp\cam,camZoom

	ndp\campitch = campitch
	ndp\camyaw   = camyaw
	ndp\camroll  = camroll
	ndp\camZoom  = camZoom

	; Start at 64x64. UpdateDrawPortal raises this to 128x128 only
	; when the player is extremely close to the doorway.
	ndp\portal = CreateMesh()
	ndp\surface = CreateSurface(ndp\portal)
	BuildDrawPortalGrid(ndp,64,64)

	EntityTexture ndp\portal,ndp\tex
	; Full-bright + no backface culling, so both sides of the room door can display it.
	EntityFX ndp\portal,1+16
	PositionEntity ndp\portal,x,y,z,True
	RotateEntity ndp\portal,pitch,yaw,roll,True

	ndp\id = 0
	temp = 0
	For c.DrawPortal = Each DrawPortal
		temp = Max(c\id,temp)
	Next
	ndp\id = temp+1

	Return ndp
End Function

; Maps one doorway vertex to the matching pixel position in the portal-camera render.
Function ProjectDrawPortalVertex(ndp.DrawPortal,vertex%)
	Local viewX%,viewY%
	Local u#,v#

	viewX = (ndp\texw-GraphicWidth)/2
	viewY = (ndp\texh-GraphicHeight)/2

	; Convert this vertex from portal-local coordinates to world coordinates.
	TFormPoint VertexX(ndp\surface,vertex),VertexY(ndp\surface,vertex),VertexZ(ndp\surface,vertex),ndp\portal,0

	; Project that world point through the real player camera.
	CameraProject Camera,TFormedX(),TFormedY(),TFormedZ()

	; The portal camera renders a GraphicWidth x GraphicHeight viewport centered
	; inside the larger render texture, so offset the screen coordinates into it.
	u = (Float(viewX)+ProjectedX())/Float(ndp\texw)
	v = (Float(viewY)+ProjectedY())/Float(ndp\texh)

	VertexTexCoords ndp\surface,vertex,u,v
End Function

Function UpdateDrawPortalUVs(ndp.DrawPortal)
	Local vertex%

	For vertex = 0 To CountVertices(ndp\surface)-1
		ProjectDrawPortalVertex(ndp,vertex)
	Next
End Function

Function DestroyDrawPortal(ndp.DrawPortal)
	If ndp\tex<>0 Then FreeTexture ndp\tex
	ndp\tex = 0 : ndp\texw = 0 : ndp\texh = 0
	If ndp\cam<>0 Then FreeEntity ndp\cam
	ndp\cam = 0
	If ndp\portal<>0 Then FreeEntity ndp\portal
	ndp\portal = 0 : ndp\surface = 0
	Delete ndp
End Function

Function UpdateDrawPortal(ndp.DrawPortal)
	Local viewX%,viewY%
	Local distToPortal#
	Local targetGrid%

	; Stable default: 64x64
	; Close range (roughly about a foot from the doorway): 128x128
	distToPortal = EntityDistance(Camera,ndp\portal)
	targetGrid = 64
	If distToPortal<1.1 Then targetGrid = 128

	If ndp\gridX<>targetGrid Then
		BuildDrawPortalGrid(ndp,targetGrid,targetGrid)
	EndIf

	RotateEntity ndp\cam,ndp\campitch,ndp\camyaw,ndp\camroll,True
	CameraZoom ndp\cam,ndp\camZoom
	CameraClsMode ndp\cam,True,True

	viewX = (ndp\texw-GraphicWidth)/2
	viewY = (ndp\texh-GraphicHeight)/2

	SetBuffer(TextureBuffer(ndp\tex))
	Cls
	SetBuffer(BackBuffer())

	; Render the forest through the real backbuffer. Unlike the texture target,
	CameraViewport ndp\cam,0,0,GraphicWidth,GraphicHeight

	HideEntity ndp\portal
	ShowEntity ndp\cam
	HideEntity Camera

	Cls
	RenderWorld

	CopyRect 0,0,GraphicWidth,GraphicHeight,viewX,viewY,BackBuffer(),TextureBuffer(ndp\tex)

	HideEntity ndp\cam
	ShowEntity Camera
	ShowEntity ndp\portal
	CameraViewport Camera,0,0,GraphicWidth,GraphicHeight

	UpdateDrawPortalUVs(ndp)
End Function

;~IDEal Editor Parameters:
;~C#Blitz3D
