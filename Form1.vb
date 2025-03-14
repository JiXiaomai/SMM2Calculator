
Imports System.Collections.ObjectModel
Imports System.Drawing.Imaging
Imports System.Globalization
Imports System.IO
Imports System.Resources
Imports System.Runtime.InteropServices
Imports System.Threading
Imports ProcessMemoryScanner

Partial Class Form1
    <DllImport("user32.dll", SetLastError:=True)>
    Private Shared Function SetParent(ByVal hWndChild As IntPtr, ByVal hWndNewParent As IntPtr) As IntPtr
    End Function

    <DllImport("user32.dll", SetLastError:=True)>
    Private Shared Function FindWindow(ByVal lpClassName As String, ByVal lpWindowName As String) As IntPtr
    End Function
    Function SNG(A As Byte(), B As Byte()) As Single
        SNG = BitConverter.ToSingle(A, 0) + BitConverter.ToSingle(B, 0)
    End Function
    Public Function Sng2Byte(SingleData As Single, ByRef BinData() As Byte) As Boolean
        '高位在前，低位在后
        Dim i As Integer
        Dim PorN As Byte
        Dim ExpVal, FraVal As Long
        Dim SingleFra As Single
        If SingleData = 0 Then
            For i = 0 To 3
                BinData(i) = 0
            Next i
            Sng2Byte = True
            Exit Function
        End If
        If SingleData >= 0 Then '符号位
            PorN = 0
        Else
            PorN = &H80
            SingleData = -1 * SingleData '变为正数
        End If
        ExpVal = Int(Math.Log(SingleData) / Math.Log(2)) '指数
        If ExpVal > 128 Then
            Sng2Byte = False
            Exit Function
        End If
        If ExpVal = 127 Then
            SingleFra = SingleData / (2 ^ ExpVal)
        Else
            SingleFra = SingleData / (2 ^ ExpVal) - 1
        End If
        FraVal = Int(SingleFra * 128 * 256 * 256) And &HFFFFFF
        ExpVal += 127
        BinData(0) = PorN + ExpVal \ 2
        BinData(3) = FraVal And &HFF
        FraVal = Int(FraVal \ 256)
        BinData(2) = FraVal Mod 256
        FraVal \= 256
        BinData(1) = (FraVal Mod 256) Or ((ExpVal And 1) * 128)
        Sng2Byte = True
    End Function
    Public Function S2B(SingleData As Single, ByRef BinData() As Byte) As Boolean
        '高位在前，低位在后
        Dim i As Integer
        Dim PorN As Byte
        Dim ExpVal, FraVal As Long
        Dim SingleFra As Single
        Dim TP(3) As Byte
        If SingleData = 0 Then
            For i = 0 To 3
                BinData(i) = 0
            Next i
            S2B = True
            Exit Function
        End If
        If SingleData >= 0 Then '符号位
            PorN = 0
        Else
            PorN = &H80
            SingleData = -1 * SingleData '变为正数
        End If
        ExpVal = Int(Math.Log(SingleData) / Math.Log(2)) '指数
        If ExpVal > 128 Then
            S2B = False
            Exit Function
        End If
        If ExpVal = 127 Then
            SingleFra = SingleData / (2 ^ ExpVal)
        Else
            SingleFra = SingleData / (2 ^ ExpVal) - 1
        End If
        FraVal = Int(SingleFra * 128 * 256 * 256) And &HFFFFFF
        ExpVal += 127
        TP(0) = PorN + ExpVal \ 2
        TP(3) = FraVal And &HFF
        FraVal = Int(FraVal \ 256)
        TP(2) = FraVal Mod 256
        FraVal \= 256
        TP(1) = (FraVal Mod 256) Or ((ExpVal And 1) * 128)
        S2B = True
        For i = 0 To 3
            BinData(i) = TP(3 - i)
        Next
    End Function
    '走路+跑步动作帧
    Dim WalkFrameR() As Integer = New Integer() {3, 3, 3, 1, 1, 2, 2, 2, 3, 3, 1, 1, 1, 2, 2}
    Dim WalkFrameW() As Integer = New Integer() {2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 1, 1, 1, 1, 1}
    Function GetWalkFrame(f As Integer, isRUN As Boolean) As Integer
        If isRUN Then
            Select Case f
                Case 1
                    Return 0
                Case Is < 12
                    Return 1
                Case Is < 19
                    Return 2
                Case Is < 23
                    Return 3
                Case Is < 28
                    Return 1
                Case Is < 31
                    Return 2
                Case Is < 35
                    Return 3
                Case Is < 39
                    Return 1
                Case Is < 42
                    Return 2
                Case Is < 45
                    Return 3
                Case Is < 48
                    Return 1
                Case Is < 51
                    Return 2
                Case Else
                    '333 11 222 33 111 22
                    Return WalkFrameR((f - 51) Mod 15)
            End Select
        Else
            Select Case f
                Case 1
                    Return 0
                Case Is < 12
                    Return 1
                Case Is < 20
                    Return 2
                Case Is < 27
                    Return 3
                Case Is < 32
                    Return 1
                Case Is < 37
                    Return 2
                Case Is < 41
                    Return 3
                Case Is < 46
                    Return 1
                Case Is < 50
                    Return 2
                Case Is < 55
                    Return 3
                Case Is < 59
                    Return 1
                Case Else
                    '2222 33333 1111 22222 3333 11111
                    Return WalkFrameW((f - 59) Mod 27)
            End Select
        End If

    End Function

    Private Sub SaveCamImg(B As Bitmap, F As Integer)
        '保存动画帧，弃用，未更新
        Dim BB As New Bitmap((CamBlk(1).X - CamBlk(0).X) * 80 + 80, (CamBlk(1).Y - CamBlk(0).Y) * 80 + 80)
        Dim GG As Graphics = Graphics.FromImage(BB)
        GG.DrawImage(B, New Rectangle(0, 0, BB.Width, BB.Height), New Rectangle(CamBlk(0).X * 80, CamBlk(0).Y * 80, BB.Width, BB.Height), GraphicsUnit.Pixel)
        BB.Save(Application.StartupPath & "\temp\" & F.ToString & ".PNG", Imaging.ImageFormat.Png)
    End Sub

    Private Function IsHitSpike(X As Single, Y As Single) As Boolean
        '判定检测
        If SpikeBlk Is Nothing Then
            Return False
        End If
        '碰撞判定 16*12
        '伤害判定 16*14 16*7
        '刺
        '绿花8*21 大绿花24*42
        '火花6*14

        For i As Integer = 0 To UBound(SpikeBlk)
            Select Case SpikeBlk(i).type
                Case 0, 3 '刺
                    If RectSetup(0,
                                     X, (NumH.Value - 1) * 16 - Y + If(CkWU.Checked, 0.1, 4), 0, 0,
                                     CSng(SpikeBlk(i).x * 8), CSng(SpikeBlk(i).y * 8),
                                     0, 0) Then
                        Return True
                    End If
                Case 1 '绿花
                    If RectSetup(1,
                                 X, (NumH.Value - 1) * 16 - Y + If(CkDuck.Checked, 9, 2),
                                 16, If(CkDuck.Checked, 7, 14),
                                 CSng(SpikeBlk(i).x * 8 + 4), CSng(SpikeBlk(i).y * 8) - 4,
                                 8, 21) Then
                        Return True
                    End If
                Case 2 '倒绿花
                    If RectSetup(1,
                                 X, (NumH.Value - 1) * 16 - Y + If(CkDuck.Checked, 9, 2),
                                 16, If(CkDuck.Checked, 7, 14),
                                 CSng(SpikeBlk(i).x * 8 + 4), CSng(SpikeBlk(i).y * 8) + 4,
                                 8, 21) Then
                        Return True
                    End If
                Case 4 '大绿花
                    If RectSetup(1,
                                 X, (NumH.Value - 1) * 16 - Y + If(CkDuck.Checked, 9, 2),
                                 16, If(CkDuck.Checked, 7, 14),
                                 CSng(SpikeBlk(i).x * 8 + 4), CSng(SpikeBlk(i).y * 8) - 26,
                                 24, 42) Then
                        Return True
                    End If
                Case 5 '倒大绿花
                    If RectSetup(1,
                                 X, (NumH.Value - 1) * 16 - Y + If(CkDuck.Checked, 9, 2),
                                 16, If(CkDuck.Checked, 7, 14),
                                 CSng(SpikeBlk(i).x * 8 + 4), CSng(SpikeBlk(i).y * 8),
                                 24, 42) Then
                        Return True
                    End If
            End Select
        Next
        Return False
    End Function
    Structure SpikeRect
        Dim x As Single
        Dim y As Single
        Dim w As Single
        Dim h As Single
    End Structure
    Private Function RectSetup(type As Integer,
                               x1 As Single, y1 As Single, w1 As Single, h1 As Single,
                               x2 As Single, y2 As Single, w2 As Single, h2 As Single) As Boolean
        '矩形碰撞判定
        '   0.1-3.1	  3.1-6	   6-8
        '顶   4 	       0	    0
        '底   4	  	   4	    0
        Dim SRect(3) As SpikeRect
        Select Case type
            Case 0
                With SRect(0) 'Mario
                    .x = x1
                    .y = y1
                    .w = 16
                    .h = If(CkWU.Checked, 15.9, 12)
                End With
                With SRect(1) '0.1-3.1
                    .x = x2 + 0.1
                    .y = y2 + 4
                    .w = 15.8
                    .h = 8
                End With
                With SRect(2) '3.1-6
                    .x = x2 + 3.1
                    .y = y2
                    .w = 9.8
                    .h = 12
                End With
                With SRect(3) '6-8
                    .x = x2 + 6
                    .y = y2
                    .w = 4
                    .h = 16
                End With
                'x方向：| (x1+w1/2) – (x2+w2/2) | < | (w1+w2)/2 | 
                'y方向：| (y1+h1/2) – (y2+h2/2) | < | (h1+h2)/2 |
                For i As Integer = 1 To 3
                    If Math.Abs(SRect(0).x + SRect(0).w / 2 - SRect(i).x - SRect(i).w / 2) <
                        Math.Abs((SRect(0).w + SRect(i).w) / 2) And
                        Math.Abs(SRect(0).y + SRect(0).h / 2 - SRect(i).y - SRect(i).h / 2) <
                        Math.Abs((SRect(0).h + SRect(i).h) / 2) Then
                        Return True
                    End If
                Next
                Return False
            Case 1
                With SRect(0) 'Mario
                    .x = x1
                    .y = y1
                    .w = w1
                    .h = h1
                End With
                With SRect(1)
                    .x = x2
                    .y = y2
                    .w = w2
                    .h = h2
                End With
                If Math.Abs(SRect(0).x + SRect(0).w / 2 - SRect(1).x - SRect(1).w / 2) <
                        Math.Abs((SRect(0).w + SRect(1).w) / 2) And
                        Math.Abs(SRect(0).y + SRect(0).h / 2 - SRect(1).y - SRect(1).h / 2) <
                        Math.Abs((SRect(0).h + SRect(1).h) / 2) Then
                    Return True
                End If
            Case Else
                Return False
        End Select
        Return False
    End Function
    Private Function Draw2(Cmd As String, IsSave As Boolean) As Integer
        '画图

        '相机数据，未更新
        '弃用
        'If CamBlk Is Nothing Then
        '    ReDim CamBlk(1)
        '    CamBlk(0).X = 0
        '    CamBlk(0).Y = 0
        '    CamBlk(1).X = NumW.Value - 1
        '    CamBlk(1).Y = NumH.Value - 1
        'End If


        Dim i, j As Integer, Opc As Single

        '计算钻洞最优偏差
        '弃用
        'Dim mx, mi, nx, ni As Single
        'For i = 0 To FrameLoc - 1 ' FrameData.Length - 1
        '    zz = 1
        '    mx = FrameData(i).X
        '    mi = FrameData(i).X
        '    nx = mx
        '    ni = mi
        '    For j = i To FrameLoc - 1 'FrameData.Length - 1
        '        If FrameData(j).X > mx Then mx = FrameData(j).X
        '        If FrameData(j).X < mi Then mi = FrameData(j).X
        '        If Math.Abs(mx - mi) <= 0.2 Then
        '            nx = mx
        '            ni = mi
        '            zz += 1
        '        Else
        '            Exit For
        '        End If
        '    Next
        '    If zz >= 4 Then
        '        Label3.Text = zz.ToString & "帧 " & ((ni + nx) / 2).ToString & "[" & FrameData(i).X & "] → " & ni.ToString & " - " & nx.ToString
        '    End If
        'Next


        '背景

        DrawBG(NumW.Value, NumH.Value)

        Dim B As Bitmap = PB.Image
        Dim G As Graphics = Graphics.FromImage(B)

        '第一帧
        G.DrawImage(CharAct(0, 0), CSng(TX1.Text) * ImgZoom + OFX,
                    OFY + (1 + NumH.Value * 16 - CSng(TY1.Text) - 16) * ImgZoom, CHW, CHH)
        Dim MH As Single = If(CkWU.Checked, 0.1 * ImgZoom, 4 * ImgZoom)
        Dim MH2 As Single = If(CkWU.Checked, 16 * ImgZoom - 1, 12 * ImgZoom - 1)
        Dim ii, jj As Integer
        Dim TxtW As Integer, Txt As String
        Dim GrdX, GrdY As Single
        GrdX = TYW.Value
        GrdY = TYH.Value
        If CkMove.Checked Then
            Txt = FrameData(0).Cmd & FrameData(0).F.ToString
            TxtW = GetStrW(Txt) \ 2
            For ii = -1 To 1
                For jj = -1 To 1
                    G.DrawString(Txt, LblCal.Font, Brushes.White, CSng((Val(TX1.Text) + 8) * ImgZoom - TxtW + ii),
                                CSng(5 + (NumH.Value * 16 - Val(TY1.Text) - 16 - 4) * ImgZoom + jj))
                Next
            Next
            G.DrawString(Txt, LblCal.Font, Brushes.Black, CSng((Val(TX1.Text) + 8) * ImgZoom - TxtW),
                        CSng(5 + (NumH.Value * 16 - Val(TY1.Text) - 16 - 4) * ImgZoom))
        End If

        'tempB = New Bitmap(B)
        j = 0

        For i = 1 To FrameLoc - 1
            If FrameData(i).F > 0 OrElse i = FrameLoc - 1 Then
                Opc = 0
                If CkMove.Checked AndAlso i < FrameLoc - 1 Then
                    Txt = FrameData(i).Cmd & FrameData(i).F.ToString
                    TxtW = GetStrW(Txt) \ 2
                    For ii = -1 To 1
                        For jj = -1 To 1
                            G.DrawString(Txt, LblCal.Font, Brushes.White, Int(FrameData(i).X * ImgZoom) - TxtW + 8 * ImgZoom + ii,
                                         5 + (NumH.Value * 16 - 16 - 4) * ImgZoom - Int(FrameData(i).Y * ImgZoom) + jj)
                        Next
                    Next
                    G.DrawString(Txt, LblCal.Font, Brushes.Black, Int(FrameData(i).X * ImgZoom) - TxtW + 8 * ImgZoom,
                                 5 + (NumH.Value * 16 - 16 - 4) * ImgZoom - Int(FrameData(i).Y * ImgZoom))
                End If
            Else
                Opc = 1
            End If

            If CkTrace.Checked OrElse Opc = 0 Then
                If FrameData(i).Y = GrdY AndAlso FrameData(i).X <= GrdX - 3.1 Then
                    If IsHitSpike(FrameData(i).X, FrameData(i).Y) Then
                        G.DrawImage(CharAct(GetWalkFrame(i, AccFrame), 2 + Opc), Int(FrameData(i).X * ImgZoom) + OFX,
                                    OFY + (1 + NumH.Value * 16 - 16) * ImgZoom - Int(FrameData(i).Y * ImgZoom), CHW, CHH)
                        If CkHitbox.Checked Then
                            G.DrawRectangle(Pens.Red, FrameData(i).X * ImgZoom,
                                            (NumH.Value * 16 - 16 - FrameData(i).Y) * ImgZoom + MH, 16 * ImgZoom - 1, MH2)
                        End If
                        If CkFrame.Checked Then
                            G.DrawString(i.ToString, LblCal.Font, Brushes.Red, FrameData(i).X * ImgZoom + OFX,
                                         OFY + (1 + NumH.Value * 16 - 12 - FrameData(i).Y) * ImgZoom)
                        End If
                    Else
                        G.DrawImage(CharAct(GetWalkFrame(i, AccFrame), Opc), Int(FrameData(i).X * ImgZoom) + OFX,
                                    OFY + (1 + NumH.Value * 16 - 16) * ImgZoom - Int(FrameData(i).Y * ImgZoom), CHW, CHH)
                        If CkHitbox.Checked Then
                            G.DrawRectangle(Pens.White, FrameData(i).X * ImgZoom,
                                           (NumH.Value * 16 - 16 - FrameData(i).Y) * ImgZoom + MH, 16 * ImgZoom - 1, MH2)
                        End If
                        If CkFrame.Checked Then
                            G.DrawString(i.ToString, LblCal.Font, Brushes.White, FrameData(i).X * ImgZoom + OFX,
                                         OFY + (1 + NumH.Value * 16 - 12 - FrameData(i).Y) * ImgZoom)
                        End If
                    End If
                Else
                    If IsHitSpike(FrameData(i).X, FrameData(i).Y) Then
                        G.DrawImage(CharAct(4, 2 + Opc), Int(FrameData(i).X * ImgZoom) + OFX,
                                    OFY + (1 + NumH.Value * 16 - 16) * ImgZoom - Int(FrameData(i).Y * ImgZoom), CHW, CHH)
                        If CkHitbox.Checked Then
                            G.DrawRectangle(Pens.Red, FrameData(i).X * ImgZoom,
                                            (NumH.Value * 16 - 16 - FrameData(i).Y) * ImgZoom + MH, 16 * ImgZoom - 1, MH2)
                        End If
                        If CkFrame.Checked Then
                            G.DrawString(i.ToString, LblCal.Font, Brushes.Red, FrameData(i).X * ImgZoom + OFX,
                                         OFY + (1 + NumH.Value * 16 - 12 - FrameData(i).Y) * ImgZoom)
                        End If
                    Else
                        G.DrawImage(CharAct(4, Opc), Int(FrameData(i).X * ImgZoom) + OFX,
                                    OFY + (1 + NumH.Value * 16 - 16) * ImgZoom - Int(FrameData(i).Y * ImgZoom), CHW, CHH)
                        If CkHitbox.Checked Then
                            G.DrawRectangle(Pens.White, FrameData(i).X * ImgZoom,
                                           (NumH.Value * 16 - 16 - FrameData(i).Y) * ImgZoom + MH, 16 * ImgZoom - 1, MH2)
                        End If
                        If CkFrame.Checked Then
                            G.DrawString(i.ToString, LblCal.Font, Brushes.White, FrameData(i).X * ImgZoom + OFX,
                                         OFY + (1 + NumH.Value * 16 - 12 - FrameData(i).Y) * ImgZoom)
                        End If
                    End If
                End If
            End If

            '保存动画帧
            If IsSave Then
                'If ZL <= UBound(SSS) AndAlso CkCtrl.Checked Then 
                '    G.DrawImage(GetButtonImg(Cmd2Button(GetCmd(SSS(ZL)))), 0, (NumH.Value * 80) - 80, 240, 80)
                'End If
                'B.Save(Application.StartupPath & "\temp\" & j.ToString & ".PNG", Imaging.ImageFormat.Png)
                SaveCamImg(B, j)
                j += 1
            End If
        Next

        PB.Image = B
        Return j - 1
    End Function

    Private Function Draw3(Cmd As String, BorderColor As Pen) As Bitmap
        '画角色图
        Dim i As Integer, Opc As Single
        Dim B = New Bitmap(CInt(NumW.Value * 16 * ImgZoom), CInt(NumH.Value * 16 * ImgZoom))
        Dim G As Graphics = Graphics.FromImage(B)

        '第一帧
        G.DrawImage(CharAct(0, 0), CSng(TX1.Text) * ImgZoom + OFX,
                    OFY + (1 + NumH.Value * 16 - CSng(TY1.Text) - 16) * ImgZoom, CHW, CHH)
        Dim MH As Single = If(CkWU.Checked, 0.1 * ImgZoom, 4 * ImgZoom)
        Dim MH2 As Single = If(CkWU.Checked, 16 * ImgZoom - 1, 12 * ImgZoom - 1)
        Dim ii, jj As Integer
        Dim TxtW As Integer, Txt As String
        Dim GrdX, GrdY As Single
        GrdX = TYW.Value
        GrdY = TYH.Value
        If CkMove.Checked Then
            Txt = FrameData(0).Cmd & FrameData(0).F.ToString
            TxtW = GetStrW(Txt) \ 2
            For ii = -1 To 1
                For jj = -1 To 1
                    G.DrawString(Txt, LblCal.Font, Brushes.White, CSng((Val(TX1.Text) - TxtW + 8) * ImgZoom + ii),
                               CSng(5 + (NumH.Value * 16 - Val(TY1.Text) - 16 - 4) * ImgZoom + jj))
                Next
            Next
            G.DrawString(Txt, LblCal.Font, Brushes.Black, CSng((Val(TX1.Text) - TxtW + 8) * ImgZoom),
                         CSng(5 + (NumH.Value * 16 - Val(TY1.Text) - 16 - 4) * ImgZoom))
        End If

        For i = 1 To FrameLoc - 1
            If FrameData(i).F > 0 OrElse i = FrameLoc - 1 Then
                Opc = 0
                If CkMove.Checked AndAlso i < FrameLoc - 1 Then
                    Txt = FrameData(i).Cmd & FrameData(i).F.ToString
                    TxtW = GetStrW(Txt) \ 2
                    For ii = -1 To 1
                        For jj = -1 To 1
                            G.DrawString(Txt, LblCal.Font, Brushes.White, Int(FrameData(i).X * ImgZoom) - TxtW + 8 * ImgZoom + ii,
                                         5 + (NumH.Value * 16 - 16 - 8) * ImgZoom - Int(FrameData(i).Y * ImgZoom) + jj)
                        Next
                    Next
                    G.DrawString(Txt, LblCal.Font, Brushes.Black, Int(FrameData(i).X * ImgZoom) - TxtW + 8 * ImgZoom,
                                 5 + (NumH.Value * 16 - 16 - 4) * ImgZoom - Int(FrameData(i).Y * ImgZoom))
                End If
            Else
                Opc = 1
            End If

            If CkTrace.Checked OrElse Opc = 0 Then
                If FrameData(i).Y = GrdY AndAlso FrameData(i).X <= GrdX - 3.1 Then
                    If IsHitSpike(FrameData(i).X, FrameData(i).Y) Then
                        G.DrawImage(CharAct(GetWalkFrame(i, AccFrame), 2 + Opc), Int(FrameData(i).X * ImgZoom) + OFX,
                                    OFY + (1 + NumH.Value * 16 - 16) * ImgZoom - Int(FrameData(i).Y * ImgZoom), CHW, CHH)
                        If CkHitbox.Checked Then
                            G.DrawRectangle(Pens.Red, FrameData(i).X * ImgZoom,
                                            (1 + NumH.Value * 16 - 16 - FrameData(i).Y) * ImgZoom + MH, 16 * ImgZoom - 1, MH2)
                        End If
                        If CkFrame.Checked Then
                            G.DrawString(i.ToString, LblCal.Font, Brushes.Red, FrameData(i).X * ImgZoom + OFX,
                                         OFY + (1 + NumH.Value * 16 - 12 - FrameData(i).Y) * ImgZoom)
                        End If
                    Else
                        G.DrawImage(CharAct(GetWalkFrame(i, AccFrame), Opc), Int(FrameData(i).X * ImgZoom) + OFX,
                                    OFY + (1 + NumH.Value * 16 - 16) * ImgZoom - Int(FrameData(i).Y * 10), CHW, CHH)
                        If CkHitbox.Checked Then
                            G.DrawRectangle(BorderColor, FrameData(i).X * ImgZoom,
                                           (1 + NumH.Value * 16 - 16 - FrameData(i).Y) * ImgZoom + MH, 16 * ImgZoom - 1, MH2)
                        End If
                        If CkFrame.Checked Then
                            G.DrawString(i.ToString, LblCal.Font, Brushes.White, FrameData(i).X * ImgZoom + OFX,
                                         OFY + (1 + NumH.Value * 16 - 12 - FrameData(i).Y) * ImgZoom)
                        End If
                    End If
                Else
                    If IsHitSpike(FrameData(i).X, FrameData(i).Y) Then
                        G.DrawImage(CharAct(4, 2 + Opc), Int(FrameData(i).X * ImgZoom) + OFX,
                                    OFY + (1 + NumH.Value * 16 - 16) * ImgZoom - Int(FrameData(i).Y * ImgZoom), CHW, CHH)
                        If CkHitbox.Checked Then
                            G.DrawRectangle(Pens.Red, FrameData(i).X * ImgZoom,
                                            (1 + NumH.Value * 16 - 16 - FrameData(i).Y) * ImgZoom + MH, 16 * ImgZoom - 1, MH2)
                        End If
                        If CkFrame.Checked Then
                            G.DrawString(i.ToString, LblCal.Font, Brushes.Red, FrameData(i).X * ImgZoom + OFX,
                                         OFY + (1 + NumH.Value * 16 - 12 - FrameData(i).Y) * ImgZoom)
                        End If
                    Else
                        G.DrawImage(CharAct(4, Opc), Int(FrameData(i).X * ImgZoom) + OFX,
                                    OFY + (1 + NumH.Value * 16 - 16) * ImgZoom - Int(FrameData(i).Y * 10), CHW, CHH)
                        If CkHitbox.Checked Then
                            G.DrawRectangle(BorderColor, FrameData(i).X * ImgZoom,
                                           (1 + NumH.Value * 16 - 16 - FrameData(i).Y) * ImgZoom + MH, 16 * ImgZoom - 1, MH2)
                        End If
                        If CkFrame.Checked Then
                            G.DrawString(i.ToString, LblCal.Font, Brushes.White, FrameData(i).X * ImgZoom + OFX,
                                         OFY + (1 + NumH.Value * 16 - 12 - FrameData(i).Y) * ImgZoom)
                        End If
                    End If
                End If
            End If
        Next
        Return B
    End Function

    Private Function GetStrW(s As String) As Integer
        Dim B As New Bitmap(300, 100)
        Dim G As Graphics = Graphics.FromImage(B), SZ As SizeF
        SZ = G.MeasureString(s, LblCal.Font)
        Return SZ.Width
    End Function

    Sub SearchMoveSpikeM(TCmd As String, RN As Single, ST As Single)
        '多线程搜索，未测试
        LBox.Items.Clear()
        FileNum = 1
        '线程数组
        Dim threads() As Thread
        Dim i, j, k, CU, CL As Integer
        Dim XRange(5) As Single
        Dim Cmd() = Txt2Cmd(TCmd, CU).Split(" ")
        Dim RCmd() As String
        CL = 0
        CU = 0
        For i = 0 To Cmd.Length - 1
            GetNum(Cmd(i), CL, CU)
            If CL <> CU Then
                Exit For
            End If
        Next
        If CL = 0 AndAlso CU = 0 Then
            Exit Sub
        End If
        '线程分配
        ReDim threads(CU - CL)
        ReDim RCmd(CU - CL)
        For k = CL To CU
            RCmd(k - CL) = ""
            For j = 0 To Cmd.Length - 1
                If j = i Then
                    RCmd(k - CL) &= GetCmd(Cmd(j)) & k.ToString & "-" & k.ToString
                Else
                    RCmd(k - CL) &= Cmd(j)
                End If
                If j < Cmd.Length - 1 Then
                    RCmd(k - CL) &= " "
                End If
            Next
        Next
        ' 创建并启动5个线程
        For k = CL To CU
            threads(k - CL) = New Thread(Sub()
                                             SearchMoveSpike(RCmd(k - CL), RN, ST)
                                             Debug.Print(DateTime.Now & "线程" & Environment.CurrentManagedThreadId.ToString & "启动")
                                             'LBox.Items.Add(DateTime.Now & "线程" & (k - CL).ToString & "启动")
                                             '' 在这里，number是线程索引，可以用作线程任务的参数
                                             'Dim number As Integer = i
                                             '' 执行计算任务
                                             'Dim result As Integer = i
                                             '' 输出结果
                                             'Debug.Print("Thread {0}: {1} squared is {2}", Thread.CurrentThread.ManagedThreadId, number, result)
                                         End Sub)
            threads(k - CL).Start()
        Next

        ' 等待所有线程完成
        For k = CL To CU
            threads(k - CL).Join()
        Next
        Debug.Print(DateTime.Now & "线程已完成")
        'LBox.Items.Add(DateTime.Now & "All threads have completed execution.")
    End Sub
    Dim FileNum As Integer = 1
    Sub SearchMoveSpike(Cmds As String, RN As Single, ST As Single)
        '碰撞判定搜索
        Dim FNum = FileNum
        FileNum += 1
        FileOpen(FNum, Application.StartupPath & "\output\[" & DateString.Replace("/", "") & "-" & TimeString.Replace(":", "") & "]" & Cmds & ".TXT", OpenMode.Output)

        Dim X, X2, Y, Y2, SPD, SPD2, RX, RY, RS As Single
        Dim SPY As Single, IsJump As Boolean

        X2 = Val(TX2.Text)
        Y2 = Val(TY2.Text)
        SPD2 = Val(TS2.Text)

        RX = Val(TX3.Text)
        RY = Val(TY3.Text)
        RS = Val(TS3.Text)

        HitBlk = CkBlk.Checked
        HitBlkLoc = Val(TB.Text)

        LblCal.Text = DateTime.Now & " 开始搜索操作"

        Dim i, j, k As Long, m As Single
        Dim S(), CmdT(), Re As String
        Dim C(), UC(), LC() As Integer

        S = Cmds.Split(" ")
        ReDim C(S.Length - 1), UC(S.Length - 1), LC(S.Length - 1)
        ReDim CmdT(S.Length - 1)
        For i = 0 To S.Length - 1
            CmdT(i) = GetCmd(S(i))
            GetNum(S(i), LC(i), UC(i))
            C(i) = LC(i)
        Next

        PBar.Maximum = UC(0)
        T1.Text = ""
        RefBtn(Cmds)
        k = 0
        Dim CSpike As Boolean
        Dim MaxF As Integer = 0, TFrame As Integer, Temp As Integer
        Dim NowF As Integer = 0
        For m = Val(TX1.Text) To Val(TX1.Text) + RN Step ST
            Application.DoEvents()
            LblCal.Text = "搜索解法 " & Format(m, "0.000") & " → 帧" & MaxF.ToString & " [" & k.ToString & "] " & Format(C(0) / UC(0), "0.00%")

            For i = 0 To S.Length - 1
                C(i) = LC(i)
                NowF += C(i)
            Next

            Do
                Application.DoEvents()
                If GetAsyncKeyState(27) Then
                    Exit For
                End If
                If NowF >= MaxF Then '搜索解法
                    X = m
                    SPD = Val(TS1.Text)
                    DSpd = Val(TD.Text)
                    Y = Val(TY1.Text)
                    SPY = 0
                    IsJump = False
                    PBar.Value = C(0)
                    WSpd = 0
                    WFrame = 0
                    CSpike = False

                    Temp = 0
                    For i = 0 To S.Length - 1
                        Temp += C(i)
                    Next

                    FrameLoc = 1
                    TFrame = 0

                    For i = 0 To S.Length - 1
                        TFrame += C(i)
                        MarioMove(CmdT(i), X, SPD, C(i), Y, SPY, IsJump)
                        '检查碰撞
                        For j = 1 To FrameLoc - 1
                            If IsHitSpike(FrameData(j).X, FrameData(j).Y) Then
                                CSpike = True
                                Exit For
                            End If
                        Next
                        If CSpike Then
                            Exit For
                        End If
                    Next

                    If Not CSpike Then
                        If TFrame >= MaxF Then
                            If TFrame > MaxF Then
                                LBox.Items.Clear()
                                LblState.Text = "[" & TFrame.ToString & "]"
                            End If
                            MaxF = TFrame
                            Re = ""
                            For i = 0 To S.GetUpperBound(0)
                                Re &= CmdT(i) & C(i).ToString & " "
                                CmdBtn(i).Label2.Text = C(i).ToString
                            Next

                            LBox.Items.Add(Re)
                            TX1.Text = m.ToString
                            'TestJump(Re, False)
                            Draw2(Re, False)

                            Re += "'" & TFrame.ToString & vbTab & Sng2Hex(X) & vbTab & "'" & Sng2Hex(SPD) & vbTab & CDec(X).ToString & vbTab &
                            CDec(SPD).ToString & vbTab & CDec(Y).ToString & vbTab & CDec(SPY).ToString & vbCrLf
                            Print(FNum, m.ToString & vbTab & Re.Replace(" ", vbTab))
                        End If
                        k += 1
                        LblCal.Text = "搜索解法 " & Format(m, "0.000") & " → 帧" & MaxF.ToString & " [" & k.ToString & "] " & Format(C(0) / UC(0), "0.00%")
                        '搜索下一解法
                        C(S.Length - 1) += 1
                        NowF += 1
                        For i = S.Length - 1 To 1 Step -1
                            If C(i) > UC(i) Then
                                NowF = NowF - C(i) + LC(i) + 1
                                C(i) = LC(i)
                                C(i - 1) += 1
                            End If
                        Next
                    Else
                        '已碰撞，跳过后续解法
                        For i = S.Length - 1 To 1 Step -1
                            If C(i) > 0 Then
                                For j = i To S.Length - 1
                                    NowF = NowF - C(j) + LC(j)
                                    C(j) = LC(j)
                                Next
                                C(i - 1) += 1
                                NowF += 1
                                For j = S.Length - 1 To 1 Step -1
                                    If C(j) > UC(j) Then
                                        NowF = NowF - C(j) + LC(j) + 1
                                        C(j) = LC(j)
                                        C(j - 1) += 1
                                    End If
                                Next
                                LblCal.Text = "搜索解法 " & Format(m, "0.000") & " → 帧" & MaxF.ToString & " [" & k.ToString & "] " & Format(C(0) / UC(0), "0.00%")
                                Exit For
                            End If
                        Next
                    End If

                Else '跳过解法
                    '搜索下一解法
                    C(S.Length - 1) += 1
                    NowF += 1
                    For i = S.Length - 1 To 1 Step -1
                        If C(i) > UC(i) Then
                            NowF = NowF - C(i) + LC(i) + 1
                            C(i) = LC(i)
                            C(i - 1) += 1
                        End If
                    Next
                End If
            Loop Until C(0) > UC(0)
        Next

        PBar.Value = PBar.Maximum
        LblCal.Text = DateTime.Now & " 找到解法" & k.ToString & "个"
        FileClose(FNum)

        'Re = ""
        'For i = 0 To S.GetUpperBound(0)
        '    Re &= CmdT(i) & C(i).ToString & " "
        'Next
        'T1.Text = Re
    End Sub
    Sub SearchMove()
        '未更新
        Dim X, X2, Y, Y2, SPD, SPD2, RX, RY, RS, RSY As Single
        Dim SPY, SPY2 As Single, IsJump As Boolean
        T2.Text = T2.Text.Replace(vbTab, " ")
        RefBtn(T2.Text)
        X2 = Val(TX2.Text)
        Y2 = Val(TY2.Text)
        SPD2 = Val(TS2.Text)
        SPY2 = Val(TSY2.Text)

        RX = Val(TX3.Text)
        RY = Val(TY3.Text)
        RS = Val(TS3.Text)
        RSY = Val(TSY3.Text)

        LblCal.Text = DateTime.Now & " 开始搜索操作"
        FileOpen(1, Application.StartupPath & "\output\[" & DateString.Replace("/", "") & "-" & TimeString.Replace(":", "") & "]" & T2.Text & ".TXT", OpenMode.Output)

        Dim i, j As Long
        Dim S(), CmdT() As String, RESULT As String
        Dim C(), UC(), LC() As Integer
        S = T2.Text.Split(" ")
        ReDim C(S.Length - 1), UC(S.Length - 1), LC(S.Length - 1)
        ReDim CmdT(S.Length - 1)
        Dim PR, PN As Integer
        PR = 1
        PN = 0
        For i = 0 To S.Length - 1
            CmdT(i) = GetCmd(S(i))
            GetNum(S(i), LC(i), UC(i))
            C(i) = LC(i)
        Next
        'PBar.Maximum = PR
        T1.Text = ""
        j = 0
        Do
            Application.DoEvents()
            If GetAsyncKeyState(27) Then
                Exit Do
            End If
            X = Val(TX1.Text)
            SPD = Val(TS1.Text)
            DSpd = Val(TD.Text)
            Y = Val(TY1.Text)
            SPY = Val(TSY1.Text)

            IsJump = False
            'PBar.Value = PN
            LblCal.Text = "搜索解法 → [" & j.ToString & "]"
            WSpd = 0
            WFrame = 0
            FrameLoc = 1
            For i = 0 To S.Length - 1
                MarioMove(GetCmd(S(i)), X, SPD, C(i), Y, SPY, IsJump)
            Next

            If Not TS4.Checked OrElse Math.Abs(SPD - SPD2) <= RS Then
                If Not TX4.Checked OrElse Math.Abs(X - X2) <= RX Then
                    If Not TY4.Checked OrElse Math.Abs(Y - Y2) <= RY Then
                        If Not TSY4.Checked OrElse Math.Abs(SPY - SPY2) <= RSY Then
                            RESULT = ""
                            For i = 0 To S.Length - 1
                                RESULT += CmdT(i) & C(i).ToString & " "
                                If CkTrace.Checked Then
                                    CmdBtn(i).Label2.Text = C(i).ToString
                                End If
                            Next
                            If CkTrace.Checked Then
                                Draw2(RESULT, False)
                            End If
                            RESULT += "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(SPD) & vbTab & CDec(X).ToString & vbTab &
                                        CDec(SPD).ToString & vbTab & CDec(Y).ToString & vbTab & CDec(SPY).ToString & vbCrLf
                            Print(1, RESULT.Replace(" ", vbTab))
                            j += 1
                        End If
                    End If
                End If
            End If
            C(0) = C(0) + 1
            For i = 0 To S.Length - 2
                If C(i) > UC(i) Then
                    C(i) = LC(i)
                    C(i + 1) += 1
                End If
            Next
            'PN += 1
        Loop Until C(C.Length - 1) > UC(UC.Length - 1)
        'PBar.Value = PBar.Maximum
        LblCal.Text = DateTime.Now & " 找到解法" & j.ToString & "个"
        FileClose(1)
    End Sub
    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles BtnSrc.Click
        If TSP.Checked Then
            '搜索碰撞判定
            LBox.Items.Clear()
            FileNum = 1
            SearchMoveSpike(T2.Text, Val(TSPX1.Text), Val(TSPX2.Text))
        Else
            '求解X Y S
            SearchMove()
        End If
    End Sub

    Private Function GetCmd(a As String) As String
        Dim i As Integer, s As String = ""
        If a.Length = 0 Then Return ""
        If InStr(a, "[") > 0 Then Return ""
        For i = 1 To a.Length
            If Not IsNumeric(Mid(a, i, 1)) Then
                s &= Mid(a, i, 1)
            Else
                Exit For
            End If
        Next
        Return s
    End Function
    Private Sub GetNum(a As String, ByRef L As Integer, ByRef U As Integer)
        Dim i, j As Integer, s(1) As String
        j = 0
        s(0) = "0" : s(1) = "0"
        If InStr(a, "-") > 0 Then
            For i = 1 To a.Length
                If IsNumeric(Mid(a, i, 1)) Then
                    s(j) &= Mid(a, i, 1)
                ElseIf Mid(a, i, 1) = "-" Then
                    j = 1
                End If
            Next
        Else
            For i = 1 To a.Length
                If IsNumeric(Mid(a, i, 1)) Then
                    s(1) &= Mid(a, i, 1)
                End If
            Next
        End If
        L = Int(s(0))
        U = Int(s(1))
    End Sub
    Dim CResult As String

    Private Sub BtnCal_Click(sender As Object, e As EventArgs) Handles BtnCal.Click
        If CkMulti.Checked Then
            ReDrawMulti()
        Else
            ReDraw()
        End If
    End Sub

    Public Function SetOpacity(ByVal BB As Bitmap, ByVal D As Double) As Bitmap
        Try
            Dim bmpDATA As New Imaging.BitmapData
            Dim tmpBMP = New Bitmap(BB)
            Dim Rct = New Rectangle(0, 0, BB.Width, BB.Height)
            bmpDATA = tmpBMP.LockBits(Rct, Imaging.ImageLockMode.ReadWrite, Imaging.PixelFormat.Format32bppArgb)
            Dim BTS(bmpDATA.Stride * bmpDATA.Height) As Byte
            Runtime.InteropServices.Marshal.Copy(bmpDATA.Scan0, BTS, 0, BTS.Length - 1)
            Dim T As Double = 0
            For I As Integer = 0 To BTS.Length - 4 Step 4
                T = BTS(I + 3)
                T *= D
                BTS(I + 3) = T
            Next
            Runtime.InteropServices.Marshal.Copy(BTS, 0, bmpDATA.Scan0, BTS.Length - 1)
            tmpBMP.UnlockBits(bmpDATA)
            Return tmpBMP
        Catch
            Return Nothing
        End Try
    End Function
    Public Function SetReverseColor(ByVal BB As Bitmap) As Bitmap
        Try
            Dim bmpDATA As New Imaging.BitmapData
            Dim tmpBMP = New Bitmap(BB)
            Dim Rct = New Rectangle(0, 0, BB.Width, BB.Height)
            bmpDATA = tmpBMP.LockBits(Rct, Imaging.ImageLockMode.ReadWrite, Imaging.PixelFormat.Format32bppArgb)
            Dim BTS(bmpDATA.Stride * bmpDATA.Height) As Byte
            Runtime.InteropServices.Marshal.Copy(bmpDATA.Scan0, BTS, 0, BTS.Length - 1)
            Dim T As Double = 0
            For I As Integer = 0 To BTS.Length - 4 Step 4
                BTS(I) = 255 - BTS(I)
                BTS(I + 1) = 255 - BTS(I + 1)
                BTS(I + 2) = 255 - BTS(I + 2)
            Next
            Runtime.InteropServices.Marshal.Copy(BTS, 0, bmpDATA.Scan0, BTS.Length - 1)
            tmpBMP.UnlockBits(bmpDATA)
            Return tmpBMP
        Catch
            'T2.Text = (ErrorToString())
            Return BB
        End Try
    End Function

    Dim CamBlk(1) As Point

    Private Sub DrawTxt(t As String, x As Integer, y As Integer)
        On Error GoTo Err
        Dim B As Bitmap = PB.Image
        Dim G As Graphics = Graphics.FromImage(B)
        Dim i, j As Integer
        For i = -1 To 1
            For j = -1 To 1
                G.DrawString(t, LblCal.Font, Brushes.White, x + i, y + j)
            Next
        Next
        G.DrawString(t, LblCal.Font, Brushes.Black, x, y)
        PB.Image = B
Err:
    End Sub

    Dim RefStrat As Boolean = False
    Dim StratData As DataTable
    Sub LoadStrat()
        StratData = New DataTable("Strat")
        StratData.Columns.Add("t0")
        StratData.Columns.Add("t1")
        StratData.Columns.Add("t2")
        StratData.Columns.Add("t20")
        StratData.Columns.Add("t21")
        If File.Exists(Application.StartupPath & "\Data.xml") Then
            RefStrat = False
            CBStrat.Items.Clear()
            StratData.ReadXml(Application.StartupPath & "\Data.xml")
            For i As Integer = 0 To StratData.Rows.Count - 1
                CBStrat.Items.Add(StratData.Rows(i).Item(0))
            Next
            RefStrat = True
            If CBStrat.Items.Count > 0 Then
                CBStrat.SelectedIndex = 0
            End If
        End If
    End Sub

    Private Sub SaveControlTextsToFile(filePath As String)
        Using writer As New StreamWriter(filePath)
            SaveControlTexts(Me, writer)
        End Using
    End Sub

    Private Sub SaveControlTexts(parent As Control, writer As StreamWriter)
        For Each ctrl As Control In parent.Controls
            If Not String.IsNullOrEmpty(ctrl.Text) Then
                writer.WriteLine($"{ctrl.Text}={ctrl.Text}")
            End If
            If ctrl.HasChildren Then
                SaveControlTexts(ctrl, writer)
            End If
        Next
    End Sub


    Sub LoadCmdTrans(filePath As String)
        If File.Exists(filePath) Then
            Dim lines = File.ReadAllLines(filePath)
            Dim idx As Integer = 0
            ReDim CmdTrans(lines.Length - 1, 1)
            For Each line In lines
                Dim parts = line.Split("=")
                If parts.Length < 2 Then
                    CmdTrans(idx, 0) = ""
                    CmdTrans(idx, 1) = ""
                Else
                    CmdTrans(idx, 0) = parts(0)
                    CmdTrans(idx, 1) = parts(1)
                End If
                idx += 1
            Next
        End If
    End Sub
    Private Sub BtnSaveTexts_Click(sender As Object, e As EventArgs) Handles Button1.Click
        'SaveControlTextsToFile("ControlTexts.txt")
        ChangeControlTexts("ControlTexts.txt")
    End Sub
    Sub ChangeControlTexts(filePath As String)
        If File.Exists(filePath) Then
            Dim lines = File.ReadAllLines(filePath)
            For Each line In lines
                Dim parts = line.Split("=")
                For Each ctrl In Me.Controls
                    SetControlText(ctrl, parts(0), parts(1))
                Next
            Next
        End If
    End Sub
    Sub SetControlText(ByRef parent As Control, ByVal text1 As String, ByVal text2 As String)
        If Not String.IsNullOrEmpty(parent.Text) AndAlso parent.Text = text1 Then
            parent.Text = text2
        End If

        If parent.HasChildren Then
            For Each ctrl As Control In parent.Controls
                SetControlText(ctrl, text1, text2)
            Next
        End If

    End Sub

    Dim PicTip As New PictureBox


    ''' <summary>Loads the private fonts.</summary>
    ''' <param name="fonts">The fonts to be loaded into the private font collection.</param>
    Private Sub LoadPrivateFonts(ByVal fonts As IEnumerable(Of Byte()))
        For Each resFont In fonts
            pfc.AddMemoryFont(Runtime.InteropServices.Marshal.UnsafeAddrOfPinnedArrayElement(resFont, 0), resFont.Length)
        Next
    End Sub
    Sub SetCtrlFont()
        Dim F = New Font(pfc.Families(0), 9)
        For Each i As Control In Me.Controls
            i.Font = F
            If i.Controls IsNot Nothing Then
                For Each j As Control In i.Controls
                    j.Font = F
                Next
            End If
        Next
    End Sub


    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ' Add any initialization after the InitializeComponent() call.
        pfc = New System.Drawing.Text.PrivateFontCollection()
        LoadPrivateFonts({My.Resources.Resource1.fusion_pixel_12px_proportional})
        SetCtrlFont()

        LoadCmdTrans("CommandTexts.txt")

        Dim i As Integer
        PB.Width = Me.ClientSize.Width - 353
        PB.Height = Me.ClientSize.Height - 10

        ComboBox3.SelectedIndex = 0
        ComboBox1.SelectedIndex = 0
        ComboBox4.SelectedIndex = 1
        ComboBox5.SelectedIndex = 1
        ComboBox6.SelectedIndex = 0

        CBItem.SelectedIndex = 0
        CBJumpAcc.SelectedIndex = 0
        Tile = Image.FromFile(Application.StartupPath &
                              "\IMG\Model\M1_Field_underground.Nin_NX_NVN\M1_Field_underground.png")

        ReDim CamBlk(1)
        CamBlk(0).X = 0
        CamBlk(0).Y = 0
        CamBlk(1).X = 15
        CamBlk(1).Y = 9

        'With Form2
        '    .FormBorderStyle = FormBorderStyle.None
        '    .Width = 1920
        '    .Height = 1080
        '    .TopMost = True
        '    .BackColor = Color.FromArgb(255, 1, 2, 3)
        '    .TransparencyKey = Color.FromArgb(255, 1, 2, 3)
        '    .Opacity = 0.5
        '    .KeyPreview = True
        'End With

        'With PicTip
        '    .Width = 1920
        '    .Height = 1080
        '    .Left = 0
        '    .Top = 0
        '    .BackColor = Color.FromArgb(255, 1, 2, 3)
        'End With
        'Form2.Controls.Add(PicTip)

        'AddHandler PicTip.MouseDoubleClick, AddressOf Form1_MouseDoubleClick
        'AddHandler Form2.KeyPress, AddressOf Form1_KeyPress

        For i = 0 To 99
            CmdBtn(i) = New CmdCtrl
            With CmdBtn(i)
                .Label1.Text = ""
                .Label2.Text = ""
                .Left = Panel1.Width + 10
                .Top = 10 + i * 26
                .BackColor = If(i Mod 2 = 0, Color.White, Color.LightGray)
                .Visible = False
            End With
            Me.Controls.Add(CmdBtn(i))
        Next
        DrawTileMode = 0
        LoadPItem()
        SelBackTile = New Bitmap(16, 16)
        SelBackTileLoc = New Point(1, 0)
        Dim G = Graphics.FromImage(SelBackTile)
        G.DrawImage(PItem.Image, New Rectangle(0, 0, 16, 16),
                    New Rectangle(16, 0, 16, 16), GraphicsUnit.Pixel)
        BtnTile.Image = SelBackTile

        LoadStrat()

    End Sub

    Dim GG As Graphics
    Dim GB As Bitmap
    Dim Tile As Image
    Public Function GetTile(x As Integer, y As Integer) As Bitmap
        GB = New Bitmap(16, 16)
        GG = Graphics.FromImage(GB)
        GG.InterpolationMode = Drawing2D.InterpolationMode.NearestNeighbor
        GG.SmoothingMode = Drawing2D.SmoothingMode.None
        GG.DrawImage(Tile, New Rectangle(0, 0, 16, 16), New Rectangle(16 * x, 16 * y, 16, 16), GraphicsUnit.Pixel)
        GetTile = GB
    End Function
    Public Function Magnifier(srcB As Bitmap, multiple As Integer) As Bitmap
        If multiple <= 0 Then
            Return srcB
        End If
        Dim B = New Bitmap(srcB.Size.Width * multiple, srcB.Size.Height * multiple)
        Dim srcData = srcB.LockBits(New Rectangle(New Point(0, 0), srcB.Size), ImageLockMode.ReadOnly, PixelFormat.Format32bppArgb)
        Dim BData = B.LockBits(New Rectangle(New Point(0, 0), B.Size), ImageLockMode.ReadWrite, PixelFormat.Format32bppArgb)
        Dim srcPtr = srcData.Scan0
        Dim BPtr = BData.Scan0
        For y As Integer = 0 To srcData.Height - 1
            For x As Integer = 0 To srcData.Width - 1
                For i As Integer = 0 To multiple - 1
                    For j As Integer = 0 To multiple - 1
                        Marshal.WriteInt32(BPtr, (x * multiple + i + (y * multiple + j) * BData.Width) * 4, Marshal.ReadInt32(srcPtr, (x + y * srcData.Width) * 4))
                    Next
                Next
            Next
        Next
        srcB.UnlockBits(srcData)
        B.UnlockBits(BData)
        Return B
    End Function

    Sub DrawBG(W As Integer, H As Integer) '设置背景

        Dim i, j As Integer
        Dim B As New Bitmap(W * 16, H * 16)
        Dim G As Graphics = Graphics.FromImage(B)
        Tile = Image.FromFile(Application.StartupPath & "\img\Model\" & ComboBox3.Text & "_Field_" &
                              ComboBox4.Text & If(CheckBox7.Checked, "_D", "") &
                              ".Nin_NX_NVN\" & ComboBox3.Text & "_Field_" &
                              ComboBox4.Text & If(CheckBox7.Checked, "_D", "") & ".png")
        Dim TX, TY As Integer
        Select Case ComboBox5.SelectedIndex
            Case 0, 1, 2 '平台1
                TX = 7 + ComboBox5.SelectedIndex * 3
                G.DrawImage(GetTile(TX, 3), 0, 0, 16, 16)
                For j = 1 To H
                    G.DrawImage(GetTile(TX, 5 - (j Mod 2)), 0, j * 16, 16, 16)
                Next
                G.DrawImage(GetTile(TX, 6), 0, j * 16, 16, 16)
                For i = 1 To W
                    G.DrawImage(GetTile(TX + 1, 3), i * 16, 0, 16, 16)
                    For j = 1 To H
                        G.DrawImage(GetTile(TX + 1, 5 - (j Mod 2)), i * 16, j * 16, 16, 16)
                    Next
                    G.DrawImage(GetTile(TX + 1, 6), i * 16, j * 16, 16, 16)
                Next
                G.DrawImage(GetTile(TX + 2, 3), 240, 0, 16, 16)
                For j = 1 To H
                    G.DrawImage(GetTile(TX + 2, 5 - (j Mod 2)), 240, j * 16, 16, 16)
                Next
                G.DrawImage(GetTile(TX + 2, 6), 240, j * 16, 16, 16)
            Case 3 '蘑菇平台
                G.DrawImage(GetTile(3, 2), 0, 0, 16, 16)
                For i = 1 To W
                    G.DrawImage(GetTile(4, 2), i * 16, 0, 16, 16)
                Next
                G.DrawImage(GetTile(5, 2), 240, 0, 16, 16)
            Case 4 '无
        End Select

        Select Case ComboBox6.SelectedIndex
            Case 0 '地面
                TX = 9 : TY = 7
            Case 1  '硬砖
                TX = 6 : TY = 0
            Case 2   '云
                TX = 6 : TY = 6
            Case 3   '软砖
                TX = 1 : TY = 0
            Case 4   '问号砖
                TX = 2 : TY = 0
            Case 5   '蘑菇平台
                TX = 4 : TY = 2
            Case 6   '平台1
                TX = 8 : TY = 3
            Case 7  '平台2
                TX = 11 : TY = 3
            Case 8   '平台3
                TX = 14 : TY = 3
            Case 9    '冰块
                TX = 8 : TY = 7
            Case 10    '开关砖
                TX = 2 : TY = 21
            Case 11    '城堡桥
                TX = 15 : TY = 15
            Case 12    '桥
                TX = 1 : TY = 3
            Case 13    '管道
                TX = 0 : TY = 0
                For i = 0 To W - 1
                    G.DrawImage(GetTile(14 + i Mod 2, 0), i * 16, (H - 2) * 16)
                    G.DrawImage(GetTile(14 + i Mod 2, 1), i * 16, (H - 1) * 16)
                Next
            Case 14   '管道横
                TX = 12 : TY = 0
                For i = 0 To W - 1
                    G.DrawImage(GetTile(12, 1), i * 16, (H - 1) * 16)
                Next
        End Select

        For i = 0 To (TYW.Value \ 16) - 1
            For j = 0 To (TYH.Value \ 16) - 1
                G.DrawImage(GetTile(TX, TY), i * 16, (H - j - 1) * 16)
            Next
        Next

        If CkBlk.Checked Then
            For i = 0 To W - 1
                For j = (TB.Value \ 16) + 1 To H - 1
                    G.DrawImage(GetTile(TX, TY), i * 16, (H - j - 1) * 16)
                Next
            Next
        End If

        B = Magnifier(B, ImgZoom)
        G = Graphics.FromImage(B)

        If SpikeBlk IsNot Nothing Then
            '画刺
            '绿花8*21 大绿花24*42
            '火花6*14
            For i = 0 To SpikeBlk.Length - 1
                Select Case SpikeBlk(i).type
                    Case 0 '刺
                        G.DrawImage(
                        Magnifier(Image.FromFile(Application.StartupPath & "\img\Pack\" & ComboBox3.Text & "_FieldAnime_Normal\" &
                              ComboBox3.Text & "_Field_anime_toge_N.Nin_NX_NVN\wait.0.png"), ImgZoom),
                              SpikeBlk(i).x * 8 * ImgZoom, SpikeBlk(i).y * 8 * ImgZoom, 16 * ImgZoom, 16 * ImgZoom)
                    Case 1 '绿花
                        G.DrawImage(
                            Magnifier(Image.FromFile(Application.StartupPath & "\img\Pack\" & ComboBox3.Text & "_Model\" &
                              ComboBox3.Text & "_Enemy_packun.Nin_NX_NVN\wait.0.png"), ImgZoom),
                            SpikeBlk(i).x * 8 * ImgZoom, SpikeBlk(i).y * 8 * ImgZoom - 8 * ImgZoom, 16 * ImgZoom, 24 * ImgZoom)
                    Case 2 '倒绿花
                        G.DrawImage(
                            Magnifier(Image.FromFile(Application.StartupPath & "\img\Pack\" & ComboBox3.Text & "_Model\" &
                              ComboBox3.Text & "_Enemy_packun.Nin_NX_NVN\wait.0.png"), ImgZoom),
                            SpikeBlk(i).x * 8 * ImgZoom, SpikeBlk(i).y * 8 * ImgZoom + 24 * ImgZoom, 16 * ImgZoom, -24 * ImgZoom)
                    Case 3 '黑花
                        G.DrawImage(
                            Magnifier(Image.FromFile(Application.StartupPath & "\img\Pack\" & ComboBox3.Text & "_Model\" &
                              ComboBox3.Text & "_Enemy_packunblack.Nin_NX_NVN\wait.0.png"), ImgZoom),
                             SpikeBlk(i).x * 8 * ImgZoom, SpikeBlk(i).y * 8 * ImgZoom, 16 * ImgZoom, 16 * ImgZoom)
                    Case 4 '大绿花
                        G.DrawImage(
                            Magnifier(Image.FromFile(Application.StartupPath & "\img\Pack\" & ComboBox3.Text & "_Model\" &
                              ComboBox3.Text & "_Enemy_packun.Nin_NX_NVN\wait.0.png"), 2 * ImgZoom),
                            SpikeBlk(i).x * 8 * ImgZoom, SpikeBlk(i).y * 8 * ImgZoom - 32 * ImgZoom, 32 * ImgZoom, 48 * ImgZoom)
                    Case 5 '大倒绿花
                        G.DrawImage(
                            Magnifier(Image.FromFile(Application.StartupPath & "\img\Pack\" & ComboBox3.Text & "_Model\" &
                              ComboBox3.Text & "_Enemy_packun.Nin_NX_NVN\wait.0.png"), 2 * ImgZoom),
                            SpikeBlk(i).x * 8 * ImgZoom, SpikeBlk(i).y * 8 * ImgZoom + 48 * ImgZoom, 32 * ImgZoom, -48 * ImgZoom)
                    Case 99
                        G.DrawImage(Magnifier(GetTile(SpikeBlk(i).tx, SpikeBlk(i).ty), ImgZoom),
                                    SpikeBlk(i).x * 8 * ImgZoom, SpikeBlk(i).y * 8 * ImgZoom, 16 * ImgZoom, 16 * ImgZoom)
                End Select
            Next
        End If

        If SpikeBlk IsNot Nothing And CkHitbox.Checked Then
            '画刺
            '绿花8*21 大绿花24*42
            '火花6*14
            For i = 0 To SpikeBlk.Length - 1
                Select Case SpikeBlk(i).type
                    Case 0, 3 '刺
                        Dim np As Point() = {New Point((SpikeBlk(i).x * 8 + 3.1) * ImgZoom, (SpikeBlk(i).y * 8) * ImgZoom),
                                New Point((SpikeBlk(i).x * 8 + 12.9) * ImgZoom - 1, (SpikeBlk(i).y * 8) * ImgZoom),
                                New Point((SpikeBlk(i).x * 8 + 12.9) * ImgZoom - 1, (SpikeBlk(i).y * 8 + 4) * ImgZoom),
                                New Point((SpikeBlk(i).x * 8 + 15.9) * ImgZoom - 1, (SpikeBlk(i).y * 8 + 4) * ImgZoom),
                                New Point((SpikeBlk(i).x * 8 + 15.9) * ImgZoom - 1, (SpikeBlk(i).y * 8 + 12) * ImgZoom - 1),
                                New Point((SpikeBlk(i).x * 8 + 10) * ImgZoom - 1, (SpikeBlk(i).y * 8 + 12) * ImgZoom - 1),
                                New Point((SpikeBlk(i).x * 8 + 10) * ImgZoom - 1, (SpikeBlk(i).y * 8 + 16) * ImgZoom - 1),
                                New Point((SpikeBlk(i).x * 8 + 6) * ImgZoom - 1, (SpikeBlk(i).y * 8 + 16) * ImgZoom - 1),
                                New Point((SpikeBlk(i).x * 8 + 6) * ImgZoom - 1, (SpikeBlk(i).y * 8 + 12) * ImgZoom - 1),
                                New Point((SpikeBlk(i).x * 8 + 0.1) * ImgZoom, (SpikeBlk(i).y * 8 + 12) * ImgZoom - 1),
                                New Point((SpikeBlk(i).x * 8 + 0.1) * ImgZoom, (SpikeBlk(i).y * 8 + 4) * ImgZoom),
                                New Point((SpikeBlk(i).x * 8 + 3.1) * ImgZoom, (SpikeBlk(i).y * 8 + 4) * ImgZoom),
                                New Point((SpikeBlk(i).x * 8 + 3.1) * ImgZoom, (SpikeBlk(i).y * 8) * ImgZoom)}
                        G.DrawLines(Pens.Red, np)
                    Case 1 '绿花
                        G.DrawRectangle(Pens.Red, (SpikeBlk(i).x * 8 + 4) * ImgZoom,
                                        (SpikeBlk(i).y * 8 - 5) * ImgZoom, 8 * ImgZoom - 1, 21 * ImgZoom - 1)
                    Case 2 '倒绿花
                        G.DrawRectangle(Pens.Red, (SpikeBlk(i).x * 8 + 4) * ImgZoom,
                                        SpikeBlk(i).y * 8 * ImgZoom, 8 * ImgZoom - 1, 21 * ImgZoom - 1)
                    Case 4 '大绿花
                        G.DrawRectangle(Pens.Red, (SpikeBlk(i).x * 8 + 4) * ImgZoom,
                                        (SpikeBlk(i).y * 8 - 26) * ImgZoom, 24 * ImgZoom - 1, 42 * ImgZoom - 1)
                    Case 5 '倒大绿花
                        G.DrawRectangle(Pens.Red, (SpikeBlk(i).x * 8 + 4) * ImgZoom,
                                        SpikeBlk(i).y * 8 * ImgZoom, 24 * ImgZoom - 1, 42 * ImgZoom - 1)
                    Case 99

                End Select
            Next
        End If
        PB.Image = B
    End Sub
    Private Function GetInsert(s As String, s0 As String, s1 As String) As List(Of String)
        Dim f, tf As New List(Of String)
        'Dim c As Integer '= GetNum(s)
        Dim ss As String '= GetCmd(s)
        Dim t() As String, i, i0, i1 As Integer, t0, t1 As String
        Dim TL, TU As Integer
        If InStr(s, " ") > 0 Then
            '复合指令
            t = s.Split(" ")
            For i = 0 To UBound(t)
                t0 = ""
                t1 = ""
                For i0 = 0 To i - 1
                    t0 += t(i0)
                    If i0 < i - 1 Then t0 += " "
                Next
                For i1 = i + 1 To UBound(t)
                    t1 += t(i1)
                    If i1 < UBound(t) Then t1 += " "
                Next
                tf = GetInsert(t(i), If(t0 = "", "", t0 & " "), If(t1 = "", "", " " & t1))
                If tf IsNot Nothing Then
                    f.AddRange(tf)
                End If

            Next
            For i = 0 To UBound(t) - 1
                t0 = ""
                t1 = ""
                For i0 = 0 To i
                    t0 += t(i0)
                    If i0 < i Then t0 += " "
                Next
                For i1 = i + 1 To UBound(t)
                    t1 += t(i1)
                    If i1 < UBound(t) Then t1 += " "
                Next
                f.Add(t0 & " i0 " & t1)
            Next
        Else
            '单独指令
            If InStr(s, "跑") > 0 OrElse InStr(s, "走") > 0 OrElse InStr(s, "蹲") > 0 OrElse InStr(s, "停") > 0 OrElse InStr(s, "立") > 0 Then
                '地面指令不插入
                Return Nothing
            Else
                GetNum(s, TL, TU)
                'c = TL.ToString & "-" & TU.ToString
                ss = GetCmd(s)
                If TU < 4 Then
                    Return Nothing
                End If
                For i = 2 To TU - 2
                    f.Add(s0 & ss & i.ToString & " " & "i0" & " " & ss & (TU - i).ToString & s1)
                Next
            End If
        End If
        Return f
    End Function
    Private Sub BtnFIns_Click(sender As Object, e As EventArgs) Handles BtnFIns.Click
        FindIns(NIns.Value)
    End Sub
    Function Txt2Cmd(t As String, ByRef UF As Integer) As String
        Dim S(), temp As String
        Dim i, k, m As Integer
        Dim R As String = ""
        UF = 0
        S = t.Replace(vbCr, " ").Replace(vbLf, " ").Replace(vbTab, " ").Split(" ")
        For i = 0 To S.Length - 1
            temp = GetCmd(S(i))
            If temp.Length > 0 Then
                GetNum(S(i), k, m)
                UF += m
                R &= temp & m.ToString & " "
            End If
        Next
        Return R.TrimEnd
    End Function
    Dim AccFrame As Boolean = False

    Private Function TestJump(CmdS As String, ShowRst As Boolean) As Single
        Dim X, SPD, SPD2, R, Y, SPY As Single
        Dim i As Long
        Dim S(), CmdT As String
        Dim C(), TP, TF As Integer
        Dim IsJump As Boolean
        Dim RE As String = ""

        S = CmdS.Split(" ")
        ReDim C(S.Length - 1)
        TF = 0
        For i = 0 To S.Length - 1
            If GetCmd(S(i)).Length > 0 Then
                GetNum(S(i), TP, C(i))
                TF += C(i)
            End If
        Next
        'ReDim FrameData(TF)
        'Debug.Print("REDIM=" & TF.ToString)
        For i = 0 To TF
            FrameData(i).F = 0
            FrameData(i).Cmd = ""
        Next
        FrameLoc = 1

        WSpd = 0
        WFrame = 0
        SPD2 = Val(TS2.Text)
        R = Val(TS3.Text)
        X = Val(TX1.Text)
        DSpd = Val(TD.Text)
        SPD = Val(TS1.Text)
        SPY = Val(TSY1.Text)
        Y = Val(TY1.Text)

        IsJump = False
        AccFrame = False

        HitBlk = CkBlk.Checked
        HitBlkLoc = TB.Value

        'If CkLoc.Checked Then
        '    MoveData = New DataTable
        '    MoveData.Columns.Add("F")
        '    MoveData.Columns.Add("X")
        '    MoveData.Columns.Add("Y")
        '    MoveData.Columns.Add("Sx")
        '    MoveData.Columns.Add("Sy")
        'End If


        For i = 0 To UBound(S)
            CmdT = GetCmd(S(i))
            If CmdT.Length > 0 Then
                GetNum(S(i), TF, TP)
                If ShowRst Then
                    RE += CmdT & TP.ToString & "▶"
                End If


                FrameData(FrameLoc - 1).Cmd = CmdT
                FrameData(FrameLoc - 1).F = TP

                MarioMove(CmdT, X, SPD, C(i), Y, SPY, IsJump)

                'CResult += vbTab & X.ToString & vbTab & SPD.ToString & vbCrLf & RE
                If ShowRst Then
                    'ⒶⒷⒸⒹⒺⒻⒼⒽⒾⒿⓀⓁⓂⓃⓄⓅⓆⓇⓈⓉⓊⓋⓌⓍⓎⓏ
                    'ⓐⓑⓒⓓⓔⓕⓖⓗⓘⓙⓚⓛⓜⓝⓞⓟⓠⓡⓢⓣⓤⓥⓦⓧⓨⓩ
                    '⒜⒝⒞⒟⒠⒡⒢⒣⒤⒥⒦⒧⒨⒩⒪⒫⒬⒭⒮⒯⒰⒱⒲⒳⒴⒵
                    RE += "ⓧ" & Format(X, "0.0000") & " ⓨ" & Format(Y, "0.0000") & " Ⓢ" & Format(SPD, "0.0000") & " Ⓢ" & Format(SPY, "0.0000") & vbCrLf
                End If

            End If
        Next
        Dim CR As String = "X" & vbTab & "Sx" & vbTab & "Y" & vbTab & "Sy" & vbCrLf
        If CkLoc.Checked Then
            For i = 0 To FrameData.Length - 1
                CR &= FrameData(i).X & vbTab & FrameData(i).Sx & vbTab & FrameData(i).Y & vbTab & FrameData(i).Sy & vbCrLf
            Next
            Clipboard.Clear()
            Clipboard.SetText(CR)
        End If
        TX2.Text = Format(X, "0.000")
        TY2.Text = Format(Y, "0.000")
        TS2.Text = Format(SPD, "0.000")
        TSY2.Text = Format(SPY, "0.000")
        If ShowRst Then
            LblCal.Text = "[" & (FrameLoc - 1).ToString & "] X=" & Format(X, "0.000") & " Sx=" & Format(SPD, "0.0000000000") & " Y=" & Format(Y, "0.000") & " Sy=" & Format(SPY, "0.000")
            T1.Text = RE
        End If

        Return X
    End Function

    Dim MoveData As DataTable
    Sub AddDataGrid(s As String, CMD As String)
        Dim P() = s.Replace(vbCrLf, "|").Split("|")
        Dim R() As String, i, F As Integer
        F = 1
        For i = 0 To P.Length - 1
            R = P(i).Split(vbTab)
            If R.Length >= 4 Then
                If F = 1 Then
                    MoveData.Rows.Add("[" & CMD & "]1", R(0), R(1), R(2), R(3))
                Else
                    MoveData.Rows.Add(F.ToString, R(0), R(1), R(2), R(3))
                End If
                F += 1
            End If
        Next
        DataGridView1.DataSource = MoveData
    End Sub
    Private Sub SaveGif(n As String, delayMs As Integer, endRe As Integer, Fr As Integer)
        '保存GIF
        LblCal.Text = DateTime.Now.ToString & " 正在保存Gif..."
        Application.DoEvents()
        Dim fileList() As String = System.IO.Directory.GetFiles(Application.StartupPath & "\temp")
        Dim c As Integer = fileList.Length
        Dim gif As AnimatedGif.AnimatedGifCreator = AnimatedGif.AnimatedGif.Create(n, delayMs, 0)

        For i As Integer = 0 To Fr
            gif.AddFrame(Image.FromFile(Application.StartupPath & "\temp\" & i.ToString & ".png"), -1, AnimatedGif.GifQuality.Bit8)
        Next
        For i = 1 To endRe
            gif.AddFrame(Image.FromFile(Application.StartupPath & "\temp\" & Fr.ToString & ".png"), -1, AnimatedGif.GifQuality.Bit8)
        Next
        gif.Dispose()
        LblCal.Text = DateTime.Now.ToString & " 已保存Gif"
    End Sub

    Private Sub Button10_Click(sender As Object, e As EventArgs)
        On Error Resume Next
        PB.Image.Save(Application.StartupPath & "\" & T2.Text & ".PNG")
    End Sub

    Private Sub Button11_Click(sender As Object, e As EventArgs) Handles BtnGif.Click
        SaveGif(T2.Text & ".GIF", CInt(TDL.Text), 5, Draw2(T2.Text, True))
    End Sub
    Structure Blocks
        Dim type As Integer
        Dim x As Integer
        Dim y As Integer
        Dim tx As Integer
        Dim ty As Integer
    End Structure
    Dim SpikeBlk() As Blocks
    Private Sub Button12_Click(sender As Object, e As EventArgs) Handles BtnCls.Click
        Erase SpikeBlk
        DrawBG(NumW.Value, NumH.Value)
    End Sub

    Dim AniFrame As Integer = 0, NowFrame As Integer = 0
    Dim IsSaveFrame As Boolean = False

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        '播放动画，弃用
        'PB.Image = Image.FromFile(Application.StartupPath & "\TEMP\" & NowFrame.ToString & ".PNG")
        'NowFrame += 1
        'If NowFrame > AniFrame Then NowFrame = 0
        If GetAsyncKeyState(Keys.Q) Then
            Timer1.Enabled = False
            Me.Text = "Recording..."
            ReadData()
            Me.Text = "Done"
            Timer1.Enabled = True
        End If

    End Sub

    Dim _MX, _MY As Single
    Dim MSpx, MSpy As Single, MAccX, MAccY As Single


    Dim PlayDelay As Integer
    Private Sub BtnEmu_Click(sender As Object, e As EventArgs) Handles BtnEmu.Click
        '模拟，未更新，弃用
        PlayDelay = Val(TFPS.Text)
        GetCharAct()
        _MX = 8
        _MY = 48
        MSpx = 0
        MSpy = 0
        MAccX = 0
        MAccY = 0
        DrawBG(NumW.Value, NumH.Value)
        MP = PB.Image
        MIsJump = False
        Timer2.Enabled = Not Timer2.Enabled
        If Timer2.Enabled Then
            BtnEmu.Text = "停止"
        Else
            BtnEmu.Text = "模拟"
        End If
    End Sub

    Dim MIsJump As Boolean
    Dim MP, MP2 As Image
    Private Declare Function GetAsyncKeyState Lib "user32" (ByVal vKey As Integer) As Integer
    Dim FPS As Integer = 0, LastFrame As Integer = 0


    Private Sub Form1_KeyPress(sender As Object, e As KeyPressEventArgs)
        '弃用，未更新
        Select Case e.KeyChar
            Case "W"
                PicTip.Top -= 160
            Case "A"
                PicTip.Left -= 160
            Case "S"
                PicTip.Top += 160
            Case "D"
                PicTip.Left += 160
        End Select
    End Sub

    Private Sub BtnSave_Click(sender As Object, e As EventArgs) Handles BtnSave.Click
        StratData.Rows.Add(TxtStrat.Text,
                            TX1.Text & " " & TS1.Text & " " & TY1.Text & " " & TYW.Value.ToString & " " &
                            TB.Value.ToString & " " & TD.Text & " " & If(CkBlk.Checked, "1", "0"),
                            T2.Text, T20.Text, T21.Text)
        StratData.GetChanges().WriteXml(Application.StartupPath & "\Data.xml")
        LoadStrat()
    End Sub

    Private Sub CalStep()
        '走位叠加，旧算法，弃用
        'Dim s1(), t0() As String
        'Dim data() As Integer, cmd() As String
        'Dim dataR() As Integer, cmdR() As String
        'Dim i, k1, k2, k3 As Integer
        's1 = File.ReadAllLines(Application.StartupPath & "\data\step.txt")
        'Dim RRR As New Random
        'ReDim data(s1.Length - 1), cmd(s1.Length - 1), dataR(s1.Length - 1), cmdR(s1.Length - 1)
        'For i = 0 To s1.Length - 1
        '    t0 = s1(i).Split(vbTab)
        '    data(i) = Val(t0(1))
        '    cmd(i) = t0(0)
        '    dataR(i) = Val(t0(1))
        '    cmdR(i) = t0(0)
        'Next

        'Dim td As Single
        'Dim newcmd() As String

        'Application.DoEvents()
        'ReDim newcmd(0)
        'For k1 = 0 To data.Length - 1
        '    For k2 = k1 + 1 To data.Length - 1
        '        For k3 = k2 + 1 To data.Length - 1
        '            td = dataR(k1) + data(k2) + data(k3)
        '            If td >= 0 AndAlso td <= 1600 Then
        '                ReDim Preserve newcmd(newcmd.Length)
        '                newcmd(newcmd.Length - 1) = cmdR(k1) & " " & cmd(k2) & " " & cmd(k3) & vbTab & td.ToString
        '            End If
        '        Next
        '    Next
        'Next

        'Debug.Print(newcmd.Length)
        'File.WriteAllLines(Application.StartupPath & "\data\step_output_D3.txt", newcmd, System.Text.Encoding.UTF8)
        'Me.Text = "STEP" & k1.ToString

        'Dim s1(), t(), t2() As String
        'Dim r As String
        's1 = File.ReadAllLines(Application.StartupPath & "\data\step3.txt")
        'Dim i As Integer
        'i = 0
        'Do While i < s1.Length
        '    t = s1(i).Split(vbTab)
        '    r = t(0)
        '    For j As Integer = i + 1 To s1.Length - 1
        '        t2 = s1(j).Split(vbTab)
        '        If t(1) = t2(1) Then
        '            r = t2(0) & vbCrLf & r
        '            i += 1
        '        Else
        '            File.WriteAllText(Application.StartupPath & "\step\" & t(1) & ".txt", r)
        '            Exit For
        '        End If
        '    Next

        '    i += 1
        '    If i = s1.Length Then
        '        File.WriteAllText(Application.StartupPath & "\step\" & t(1) & ".txt", r)
        '        Exit Do
        '    End If
        'Loop
        'Me.Text = "STEP"
    End Sub
    Structure StepData
        Dim X As Integer
        Dim C As String
    End Structure
    Dim isStepOpen As Boolean = False, MStepData() As StepData, MStepLoc(), MaxStep As Integer
    Sub ReadStepData()
        Dim s() = File.ReadAllLines(Application.StartupPath & "step\Step.txt")
        ReDim MStepData(s.Length)
        MStepData(s.Length).C = ""
        MStepData(s.Length).X = 2100000000
        For i As Integer = 0 To s.Length - 1
            Dim p() As String = s(i).Split(vbTab)
            MStepData(i).X = Int(p(0))
            MStepData(i).C = ""
            For j As Integer = 1 To p.Length - 1
                If p(j).Length > 0 Then
                    MStepData(i).C &= p(j) & " "
                End If
            Next
        Next
        MaxStep = MStepData(s.Length - 1).X
        MStepLoc = New Integer(MStepData.Last.X) {}
        'Debug.Print(MStepLoc.Length)
        For i = 0 To MStepData.Length - 1
            If MStepLoc(MStepData(i).X) = 0 Then
                MStepLoc(MStepData(i).X) = i
            End If
        Next
        isStepOpen = True
    End Sub
    Function FindStepCombo(S As Integer) As List(Of Integer)
        Dim R = New List(Of Integer)
        'A+0
        'If S < MStepLoc.Length AndAlso MStepLoc(S) > 0 Then
        '    R.Add(S)
        'End If
        'A+B
        For i = 1 To Math.Min(S \ 2, MStepLoc.Length)
            If S - i < MStepLoc.Length AndAlso MStepLoc(i) > 0 AndAlso MStepLoc(S - i) > 0 Then
                R.Add(i)
            End If
        Next
        'A-B
        For i = S + 1 To MStepLoc.Length - 1
            If MStepLoc(i) > 0 AndAlso MStepLoc(i - S) > 0 Then
                R.Add(-i)
            End If
        Next
        Return R
    End Function
    Sub AddStepSplit(C As Integer, R As List(Of Integer), isLeft As Boolean)
        Dim i, m, n As Integer
        Dim K1, K2 As Integer
        Dim SType As Integer
        For Each i In R
            Select Case i
                Case Is > 0
                    SType = 1
                    K1 = i
                    K2 = C - i
                Case Is < 0
                    SType = 2
                    K1 = C - i
                    K2 = -i
                Case Else
                    Exit Sub
            End Select
            Select Case SType
                Case 1
                    m = MStepLoc(Math.Abs(K1))
                    Do
                        n = MStepLoc(Math.Abs(K2))
                        Do
                            LBox.Items.Add(
                                "[" & K1.ToString & "+" & K2.ToString & "] " &
                                If(isLeft, RCmd(Eng2Cmd(MStepData(m).C)), Eng2Cmd(MStepData(m).C)) &
                                If(isLeft, RCmd(Eng2Cmd(MStepData(n).C)), Eng2Cmd(MStepData(n).C)))
                            n += 1
                        Loop While MStepData(n).X = K2
                        m += 1
                    Loop While MStepData(m).X = K1
                Case Else
                    m = MStepLoc(Math.Abs(K1))
                    Do
                        n = MStepLoc(Math.Abs(K2))
                        Do
                            LBox.Items.Add(
                                "[" & K1.ToString & "-" & K2.ToString & "] " &
                                If(isLeft, RCmd(Eng2Cmd(MStepData(m).C)), Eng2Cmd(MStepData(m).C)) &
                                If(isLeft, Eng2Cmd(MStepData(n).C), RCmd(Eng2Cmd(MStepData(n).C))))
                            n += 1
                        Loop While MStepData(n).X = K2
                        m += 1
                    Loop While MStepData(m).X = K1
            End Select

        Next
    End Sub
    Function Eng2Cmd(S As String) As String
        Dim R = S.Replace("L", "左跑")
        R = R.Replace("R", "右跑")
        R = R.Replace("l", "左走")
        R = R.Replace("r", "右走")
        R = R.Replace("s", "站立")
        R = R.Replace("f", "正蹲")
        R = R.Replace("b", "反蹲")
        R = R.Replace("S", "站停")
        R = R.Replace("F", "正停")
        R = R.Replace("B", "反停")
        Return R
    End Function
    Private Sub TxtStep_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TxtStep.KeyPress
        If e.KeyChar = vbCr Then
            If isStepOpen = False Then
                ReadStepData()
            End If
            '步幅搜索
            Dim i As Integer
            Dim C As Integer = Int(Val(TxtStep.Text) * 100 + 0.5)
            Dim IsLeft As Boolean = C < 0
            C = Math.Abs(C)
            If C <= MaxStep * 2 Then
                LBox.Items.Clear()
                If IsLeft Then
                    For i = 0 To MStepData.Length - 2
                        If MStepData(i).X = C Then
                            LBox.Items.Add(RCmd(Eng2Cmd(MStepData(i).C)))
                        ElseIf MStepData(i).X > C Then
                            Exit For
                        End If
                    Next
                Else
                    For i = 0 To MStepData.Length - 2
                        If MStepData(i).X = C Then
                            LBox.Items.Add(Eng2Cmd(MStepData(i).C))
                        ElseIf MStepData(i).X > C Then
                            Exit For
                        End If
                    Next
                End If
                If CB1F.Checked Then
                    Dim R = FindStepCombo(C)
                    If R IsNot Nothing Then
                        AddStepSplit(C, R, IsLeft)
                    End If
                    LblState.Text = "步幅 -> " & TxtStep.Text & " 解法 +" & LBox.Items.Count + R.Count
                Else
                    LblState.Text = "步幅 -> " & TxtStep.Text & " 解法 +" & LBox.Items.Count
                End If

            End If
        End If
    End Sub
    Function RCmd(S As String) As String '操作镜像
        Dim R = S.Replace("右", "||")
        R = R.Replace("左", "{}")
        R = R.Replace("{}", "右")
        R = R.Replace("||", "左")
        R = S.Replace("Right", "||")
        R = R.Replace("Left", "{}")
        R = R.Replace("{}", "Right")
        R = R.Replace("||", "Left")
        Return R
    End Function
    Dim LB1State As Integer = 0

    Private Sub TxtAir_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TxtAir.KeyPress
        '滞空数据列表，弃用
        If e.KeyChar = vbCr Then
            '滞空搜索
            Dim C() As String = TxtAir.Text.Split("-")
            Dim R1 As Integer = C(0) * 1000
            Dim R2 As Integer = C(1) * 1000
            Dim s() = File.ReadAllLines(Application.StartupPath & "\jump\" & CBJumpAcc.Text & ".txt")
            LBox.Items.Clear()
            LB1State = 1
            For i As Integer = 0 To s.Length - 1
                If Int(s(i)) <= R2 AndAlso Int(s(i)) >= R1 Then
                    Dim s2() = File.ReadAllLines(Application.StartupPath & "\jump\" & CBJumpAcc.Text & "\" & s(i) & ".txt")
                    For j As Integer = 0 To s2.Length - 1
                        LBox.Items.Add(AJump2Cmd(s2(j)))
                    Next
                End If
            Next
            If LBox.Items.Count > 0 Then
                ReDraw()
            End If
        End If
    End Sub
    Function AJump2Cmd(A As String) As String
        Dim s() = A.Split(vbTab)
        If s.Length = 9 Then
            Select Case CBJumpAcc.Text
                Case "3.568", "3.748", "3.808", "3.868" '普通跳
                    If s(3) = "0" Then
                        Return "[" & s(5) & "/" & s(6) & "/" & s(7) & "/" & s(8) & "]" & " 加速右跳" & s(1) & " 加速右" & s(2) & " 加速右跳30"
                    Else
                        Return "[" & s(5) & "/" & s(6) & "/" & s(7) & "/" & s(8) & "]" & " 加速右跳" & s(1) & " 加速右" & s(2) & " 加速右跳" & s(3) & " 加速右" & s(4) & " 加速右跳30"
                    End If
                Case Else '缓冲跳
                    If s(3) = "0" Then
                        Return "[" & s(5) & "/" & s(6) & "/" & s(7) & "/" & s(8) & "]" & " 加速右缓冲跳" & s(1) & " 加速右" & s(2) & " 加速右跳30"
                    Else
                        Return "[" & s(5) & "/" & s(6) & "/" & s(7) & "/" & s(8) & "]" & " 加速右缓冲跳" & s(1) & " 加速右" & s(2) & " 加速右跳" & s(3) & " 加速右" & s(4) & " 加速右跳30"
                    End If
            End Select
        Else
            Return ""
        End If
    End Function
    Private Sub TxtStep_Click(sender As Object, e As EventArgs) Handles TxtStep.Click
        TxtStep.SelectAll()
    End Sub

    Private Sub TxtAir_Click(sender As Object, e As EventArgs) Handles TxtAir.Click
        TxtAir.SelectAll()
    End Sub

    Private Sub CBStrat_SelectedIndexChanged(sender As Object, e As EventArgs) Handles CBStrat.SelectedIndexChanged
        If RefStrat AndAlso CBStrat.SelectedIndex >= 0 Then
            Dim s() = StratData.Rows(CBStrat.SelectedIndex).Item(1).ToString.Split(" ")
            TxtStrat.Text = StratData.Rows(CBStrat.SelectedIndex).Item(0).ToString
            TX1.Text = s(0)
            TS1.Text = s(1)
            TY1.Text = s(2)
            TYH.Value = Val(s(2))
            TYW.Value = Val(s(3))
            TB.Value = Val(s(4))
            TD.Text = s(5)
            CkBlk.Checked = s(6) = "1"
            T2.Text = StratData.Rows(CBStrat.SelectedIndex).Item(2).ToString
            T20.Text = StratData.Rows(CBStrat.SelectedIndex).Item(3).ToString
            T21.Text = StratData.Rows(CBStrat.SelectedIndex).Item(4).ToString
            BtnCal_Click(BtnCal, New EventArgs())
        End If
    End Sub

    Private Declare Function timeGetTime Lib "winmm.dll" () As Integer
    Dim MJumpF As Byte = 1
    Dim CharAct(11, 3) As Bitmap, CharActName As String = "MarioMdl", CharPack As String = "12621"
    Dim OFX, OFY, CHW, CHH As Integer
    Sub GetCharAct()
        '角色贴图加载，待更新
        Dim F As String = Application.StartupPath & "\img\pack\"
        Select Case CharPack
            Case "M1"
                F &= "M1_Model\M1_Player_" & CharActName & ".Nin_NX_NVN\"
                Select Case CharActName
                    Case "MarioMdl_4"
                        OFX = -8 * ImgZoom : OFY = 0
                        CHW = 24 * ImgZoom : CHH = 16 * ImgZoom
                    Case Else
                        OFX = 0 : OFY = 0
                        CHW = 16 * ImgZoom : CHH = 16 * ImgZoom
                End Select
            Case "M3"
                F &= "M3_Model\M3_Player_" & CharActName & ".Nin_NX_NVN\"
                Select Case CharActName
                    Case "MarioMdl_4"
                        OFX = -8 * ImgZoom : OFY = 0
                        CHW = 32 * ImgZoom : CHH = 16 * ImgZoom
                    Case Else
                        OFX = 0 : OFY = 0
                        CHW = 16 * ImgZoom : CHH = 16 * ImgZoom
                End Select
            Case "MW"
                F &= "MW_Model\MW_Player_" & CharActName & ".Nin_NX_NVN\"
                Select Case CharActName
                    Case "MarioMdl_4", "MarioMdl_3"
                        OFX = -8 * ImgZoom : OFY = -16 * ImgZoom
                        CHW = 32 * ImgZoom : CHH = 32 * ImgZoom
                    Case Else
                        OFX = 0 : OFY = -16 * ImgZoom
                        CHW = 16 * ImgZoom : CHH = 32 * ImgZoom
                End Select
            Case Else
                F &= "M1_Model\M1_Player_" & CharActName & ".Nin_NX_NVN\"
                Select Case CharActName
                    Case "MarioMdl_4"
                        OFX = -8 * ImgZoom : OFY = 0
                        CHW = 24 * ImgZoom : CHH = 16 * ImgZoom
                    Case Else
                        OFX = 0 : OFY = 0
                        CHW = 16 * ImgZoom : CHH = 16 * ImgZoom
                End Select
        End Select

        Dim i, j As Integer
        If Not CkMario.Checked Then
            For i = 0 To 11
                For j = 0 To 3
                    CharAct(i, j) = New Bitmap(CHW, CHH)
                Next
            Next
        Else
            If CkDuck.Checked Then
                For i = 0 To 4
                    CharAct(i, 0) = Magnifier(Image.FromFile(F & "stoop.0.png"), ImgZoom)
                    CharAct(i + 5, 0) = CharAct(i, 0).Clone()
                    CharAct(i + 5, 0).RotateFlip(RotateFlipType.RotateNoneFlipX)
                Next
                CharAct(10, 0) = Magnifier(Image.FromFile(F & "stoop.0.png"), ImgZoom)
                CharAct(11, 0) = CharAct(10, 0).Clone()
                CharAct(11, 0).RotateFlip(RotateFlipType.RotateNoneFlipX)
            Else
                '0站 1走A 2走B 3走C 4跳 5-9镜像
                CharAct(0, 0) = Magnifier(Image.FromFile(F & "wait.0.png"), ImgZoom)
                CharAct(1, 0) = Magnifier(Image.FromFile(F & "walk.0.png"), ImgZoom)
                CharAct(2, 0) = Magnifier(Image.FromFile(F & "walk.1.png"), ImgZoom)

                If File.Exists(F & "walk.2.png") Then
                    CharAct(3, 0) = Magnifier(Image.FromFile(F & "walk.2.png"), ImgZoom)
                Else
                    CharAct(3, 0) = Magnifier(Image.FromFile(F & "walk.0.png"), ImgZoom)
                End If

                CharAct(4, 0) = Magnifier(Image.FromFile(F & "jump.0.png"), ImgZoom)

                For i = 0 To 4
                    CharAct(i + 5, 0) = CharAct(i, 0).Clone()
                    CharAct(i + 5, 0).RotateFlip(RotateFlipType.RotateNoneFlipX)
                Next
                CharAct(10, 0) = Magnifier(Image.FromFile(F & "stoop.0.png"), ImgZoom)
                CharAct(11, 0) = CharAct(10, 0).Clone()
                CharAct(11, 0).RotateFlip(RotateFlipType.RotateNoneFlipX)
            End If
            For i = 0 To 11
                CharAct(i, 1) = SetOpacity(CharAct(i, 0), Val(TxtOpc.Text) / 100)
                CharAct(i, 2) = SetReverseColor(CharAct(i, 0))
                CharAct(i, 3) = SetReverseColor(SetOpacity(CharAct(i, 0), Val(TxtOpc.Text) / 100))
            Next
        End If

    End Sub
    Private Sub Timer2_Tick(sender As Object, e As EventArgs) Handles Timer2.Tick
        '未更新，弃用

        Do Until timeGetTime - LastFrame > PlayDelay

        Loop

        Me.Text = Int(1000 / (timeGetTime - LastFrame)).ToString
        LastFrame = timeGetTime
        FPS += 1
        If MIsJump Then
            '空中
            'X轴

            If GetAsyncKeyState(Keys.D) Then
                Select Case MSpx
                    Case Is < 0
                        MAccX = 0.09
                    Case Is <= 0.5
                        MAccX = 0.08
                    Case Is < 1.5
                        MAccX = 0.06
                    Case Is < 2.25
                        MAccX = 0.029
                    Case Else
                        MAccX = 0.021
                End Select
                MSpx += MAccX
                If MSpx > 3 Then MSpx = 3
                _MX += MSpx
            ElseIf GetAsyncKeyState(Keys.A) Then
                Select Case MSpx
                    Case Is > 0
                        MAccX = -0.09
                    Case Is >= 0.5
                        MAccX = -0.08
                    Case Is > -1.5
                        MAccX = -0.06
                    Case Is > -2.25
                        MAccX = -0.029
                    Case Else
                        MAccX = -0.021
                End Select
                MSpx += MAccX
                If MSpx < -3 Then MSpx = -3
                _MX += MSpx
            ElseIf MSpx <> 0 Then
                _MX += MSpx
            End If
            'Y轴
            If GetAsyncKeyState(Keys.J) Then
                Select Case MSpy
                    Case Is > 2.5
                        MAccY = -0.06
                    Case Is > 1.5
                        MAccY = -0.25
                    Case Is > 0.3
                        MAccY = -0.34
                    Case Is > -0.15
                        MAccY = -0.08
                    Case Is > -3
                        MAccY = -0.31
                    Case Else
                        MAccY = -0.34
                End Select
            Else
                Select Case MSpy
                    Case Is > 0.3
                        MAccY = -0.34
                    Case Is > -0.15
                        MAccY = -0.25
                    Case Else
                        MAccY = -0.34
                End Select
            End If
            MSpy += MAccY
            If MSpy < -4 Then MSpy = -4
            _MY += MSpy

            '碰撞检测
            If _MY <= 48 Then
                MSpy = 0
                _MY = 48
                MJumpF = If(MSpx < 0, 1, 0)
                MIsJump = False
            End If

        Else
            '地面
            'X轴
            If GetAsyncKeyState(Keys.D) Then
                MJumpF = 0
                Select Case MSpx
                    Case Is < 0
                        MAccX = 0.1
                    Case Is <= 0.5
                        MAccX = 0.1
                    Case Is < 1.5
                        MAccX = 0.06
                    Case Is < 2.25
                        MAccX = 0.029
                    Case Else
                        MAccX = 0.035
                End Select
                MSpx += MAccX
                If MSpx > 3 Then MSpx = 3
                _MX += MSpx
            ElseIf GetAsyncKeyState(Keys.A) Then
                MJumpF = 1
                Select Case MSpx
                    Case Is > 0
                        MAccX = -0.1
                    Case Is >= 0.5
                        MAccX = -0.1
                    Case Is > -1.5
                        MAccX = -0.06
                    Case Is > -2.25
                        MAccX = -0.029
                    Case Else
                        MAccX = -0.035
                End Select
                MSpx += MAccX
                If MSpx < -3 Then MSpx = -3
                _MX += MSpx
            ElseIf MSpx <> 0 Then
                Select Case MSpx
                    Case Is > 1.5
                        MAccX = -0.035
                    Case Is > 0
                        MAccX = -0.05
                    Case Is = 0
                        MAccX = 0
                    Case Is > -1.5
                        MAccX = 0.05
                    Case Else
                        MAccX = 0.035
                End Select
                If Math.Abs(MAccX) >= Math.Abs(MSpx) Then
                    MSpx = 0
                Else
                    MSpx += MAccX
                End If
                _MX += MSpx
            End If
            'Y轴
            If GetAsyncKeyState(Keys.J) AndAlso Not MIsJump Then
                MIsJump = True
                Select Case Math.Abs(MSpx)
                    Case Is >= 2.8
                        MSpy = 3.868
                    Case Is >= 1.5
                        MSpy = 3.808
                    Case >= 0.7
                        MSpy = 3.748
                    Case Else
                        MSpy = 3.568
                End Select
                _MY += MSpy
            End If
        End If


        MP2 = New Bitmap(1280, 800)
        Dim G = Graphics.FromImage(MP2)
        G.DrawImage(MP, 0, 0)
        If IsHitSpike(Int(_MX * 5), Int(805 - _MY * 5)) Then
            If MIsJump Then
                G.DrawImage(CharAct(4 + MJumpF * 5, 0), _MX * 5, 805 - _MY * 5, 80, 80)
            Else
                G.DrawImage(CharAct(MJumpF * 5, 0), _MX * 5, 805 - _MY * 5, 80, 80)
            End If
        Else
            If MIsJump Then
                G.DrawImage(CharAct(4 + MJumpF * 5, 0), _MX * 5, 805 - _MY * 5, 80, 80)
            Else
                G.DrawImage(CharAct(MJumpF * 5, 0), _MX * 5, 805 - _MY * 5, 80, 80)
            End If
        End If
        G.DrawString("X=" & _MX.ToString & vbCrLf & "Y=" & _MY.ToString & vbCrLf & "Sx=" & MSpx.ToString & vbCrLf & "Sy=" & MSpy.ToString,
                        LblCal.Font, Brushes.White, _MX * 5, 805 - _MY * 5 - 100)
        PB.Image = MP2
    End Sub

    Private Sub BtnPlay_Click(sender As Object, e As EventArgs) Handles BtnPlay.Click
        If Timer1.Enabled Then
            BtnPlay.Text = "播放"
            Timer1.Enabled = False
        Else
            If Not IsSaveFrame Then
                '未保存动画，生成新帧
                Dim CU As Integer
                TestJump(Txt2Cmd(T2.Text, CU), True)
                AniFrame = Draw2(T2.Text, True)
            End If
            If NowFrame > AniFrame Then NowFrame = 0
            Timer1.Interval = Int(TDL.Text)
            Timer1.Enabled = True
            BtnPlay.Text = "停止"
        End If
    End Sub

    Private Sub LBox_SelectedIndexChanged(sender As System.Object, e As Global.System.EventArgs) Handles LBox.SelectedIndexChanged
        If LBox.SelectedItem IsNot Nothing Then
            T2.Text = LBox.SelectedItem.ToString
            ReDraw()
        End If
    End Sub
    Private Sub Form1_Resize(sender As System.Object, e As Global.System.EventArgs) Handles MyBase.Resize
        PB.Width = Me.ClientSize.Width - 353
        PB.Height = Me.ClientSize.Height - 10
    End Sub

    Private Sub PB_MouseClick(sender As System.Object, e As Global.System.Windows.Forms.MouseEventArgs) Handles PB.MouseClick
        Dim B As Bitmap = PB.Image
        Dim G As Graphics = Graphics.FromImage(B)
        If Not PB.IsPointInImage(e.X, e.Y) Then Exit Sub
        Select Case e.Button
            Case MouseButtons.Right '画道具
                Dim EPoint = PB.PointToImage(e.X, e.Y)
                If SpikeBlk Is Nothing Then
                    ReDim Preserve SpikeBlk(0)
                    SpikeBlk(0).x = (EPoint.X - 4 * ImgZoom) \ (8 * ImgZoom)
                    SpikeBlk(0).y = (EPoint.Y - 4 * ImgZoom) \ (8 * ImgZoom)
                    If DrawTileMode = 0 Then
                        SpikeBlk(0).type = CBItem.SelectedIndex
                    Else
                        SpikeBlk(0).type = 99
                        SpikeBlk(0).tx = SelBackTileLoc.X
                        SpikeBlk(0).ty = SelBackTileLoc.Y
                    End If
                Else
                    ReDim Preserve SpikeBlk(UBound(SpikeBlk) + 1)
                    SpikeBlk(UBound(SpikeBlk)).x = (EPoint.X - 4 * ImgZoom) \ (8 * ImgZoom)
                    SpikeBlk(UBound(SpikeBlk)).y = (EPoint.Y - 4 * ImgZoom) \ (8 * ImgZoom)
                    If DrawTileMode = 0 Then
                        SpikeBlk(UBound(SpikeBlk)).type = CBItem.SelectedIndex
                    Else
                        SpikeBlk(UBound(SpikeBlk)).type = 99
                        SpikeBlk(UBound(SpikeBlk)).tx = SelBackTileLoc.X
                        SpikeBlk(UBound(SpikeBlk)).ty = SelBackTileLoc.Y
                    End If
                End If

                Select Case SpikeBlk(UBound(SpikeBlk)).type
                    Case 0 '刺
                        G.DrawImage(
                        Magnifier(Image.FromFile(Application.StartupPath & "\img\Pack\" & ComboBox3.Text & "_FieldAnime_Normal\" &
                              ComboBox3.Text & "_Field_anime_toge_N.Nin_NX_NVN\wait.0.png"), ImgZoom),
                              SpikeBlk(UBound(SpikeBlk)).x * 8 * ImgZoom, SpikeBlk(UBound(SpikeBlk)).y * 8 * ImgZoom,
                              16 * ImgZoom, 16 * ImgZoom)
                        If CkHitbox.Checked Then
                            Dim np As Point() = {New Point((SpikeBlk(UBound(SpikeBlk)).x * 8 + 3.1) * ImgZoom, (SpikeBlk(UBound(SpikeBlk)).y * 8) * ImgZoom),
                                New Point((SpikeBlk(UBound(SpikeBlk)).x * 8 + 12.9) * ImgZoom - 1, (SpikeBlk(UBound(SpikeBlk)).y * 8) * ImgZoom),
                                New Point((SpikeBlk(UBound(SpikeBlk)).x * 8 + 12.9) * ImgZoom - 1, (SpikeBlk(UBound(SpikeBlk)).y * 8 + 4) * ImgZoom),
                                New Point((SpikeBlk(UBound(SpikeBlk)).x * 8 + 15.9) * ImgZoom - 1, (SpikeBlk(UBound(SpikeBlk)).y * 8 + 4) * ImgZoom),
                                New Point((SpikeBlk(UBound(SpikeBlk)).x * 8 + 15.9) * ImgZoom - 1, (SpikeBlk(UBound(SpikeBlk)).y * 8 + 12) * ImgZoom - 1),
                                New Point((SpikeBlk(UBound(SpikeBlk)).x * 8 + 10) * ImgZoom - 1, (SpikeBlk(UBound(SpikeBlk)).y * 8 + 12) * ImgZoom - 1),
                                New Point((SpikeBlk(UBound(SpikeBlk)).x * 8 + 10) * ImgZoom - 1, (SpikeBlk(UBound(SpikeBlk)).y * 8 + 16) * ImgZoom - 1),
                                New Point((SpikeBlk(UBound(SpikeBlk)).x * 8 + 6) * ImgZoom - 1, (SpikeBlk(UBound(SpikeBlk)).y * 8 + 16) * ImgZoom - 1),
                                New Point((SpikeBlk(UBound(SpikeBlk)).x * 8 + 6) * ImgZoom - 1, (SpikeBlk(UBound(SpikeBlk)).y * 8 + 12) * ImgZoom - 1),
                                New Point((SpikeBlk(UBound(SpikeBlk)).x * 8 + 0.1) * ImgZoom, (SpikeBlk(UBound(SpikeBlk)).y * 8 + 12) * ImgZoom - 1),
                                New Point((SpikeBlk(UBound(SpikeBlk)).x * 8 + 0.1) * ImgZoom, (SpikeBlk(UBound(SpikeBlk)).y * 8 + 4) * ImgZoom),
                                New Point((SpikeBlk(UBound(SpikeBlk)).x * 8 + 3.1) * ImgZoom, (SpikeBlk(UBound(SpikeBlk)).y * 8 + 4) * ImgZoom),
                                New Point((SpikeBlk(UBound(SpikeBlk)).x * 8 + 3.1) * ImgZoom, (SpikeBlk(UBound(SpikeBlk)).y * 8) * ImgZoom)}
                            G.DrawLines(Pens.Red, np)
                        End If
                    Case 1 '绿花
                        G.DrawImage(
                            Magnifier(Image.FromFile(Application.StartupPath & "\img\Pack\" & ComboBox3.Text & "_Model\" &
                              ComboBox3.Text & "_Enemy_packun.Nin_NX_NVN\wait.0.png"), ImgZoom),
                            SpikeBlk(UBound(SpikeBlk)).x * 8 * ImgZoom, (SpikeBlk(UBound(SpikeBlk)).y * 8 - 8) * ImgZoom,
                            16 * ImgZoom, 24 * ImgZoom)
                        If CkHitbox.Checked Then
                            G.DrawRectangle(Pens.Red, (SpikeBlk(UBound(SpikeBlk)).x * 8 + 4) * ImgZoom,
                                           (SpikeBlk(UBound(SpikeBlk)).y * 8 - 5) * ImgZoom,
                                           8 * ImgZoom - 1, 21 * ImgZoom - 1)
                        End If
                    Case 2 '倒绿花
                        G.DrawImage(
                            Magnifier(Image.FromFile(Application.StartupPath & "\img\Pack\" & ComboBox3.Text & "_Model\" &
                              ComboBox3.Text & "_Enemy_packun.Nin_NX_NVN\wait.0.png"), ImgZoom),
                            SpikeBlk(UBound(SpikeBlk)).x * 8 * ImgZoom, (SpikeBlk(UBound(SpikeBlk)).y * 8 + 24) * ImgZoom,
                            16 * ImgZoom, -24 * ImgZoom)
                        If CkHitbox.Checked Then
                            G.DrawRectangle(Pens.Red, (SpikeBlk(UBound(SpikeBlk)).x * 8 + 4) * ImgZoom,
                                            SpikeBlk(UBound(SpikeBlk)).y * 8 * ImgZoom,
                                            8 * ImgZoom - 1, 21 * ImgZoom - 1)
                        End If
                    Case 4 '大绿花 '绿花8*21 大绿花24*42
                        G.DrawImage(
                            Magnifier(Image.FromFile(Application.StartupPath & "\img\Pack\" & ComboBox3.Text & "_Model\" &
                              ComboBox3.Text & "_Enemy_packun.Nin_NX_NVN\wait.0.png"), 2 * ImgZoom),
                            SpikeBlk(UBound(SpikeBlk)).x * 8 * ImgZoom, (SpikeBlk(UBound(SpikeBlk)).y * 8 - 32) * ImgZoom,
                            32 * ImgZoom, 48 * ImgZoom)
                        If CkHitbox.Checked Then
                            G.DrawRectangle(Pens.Red, (SpikeBlk(UBound(SpikeBlk)).x * 8 + 4) * ImgZoom,
                                            (SpikeBlk(UBound(SpikeBlk)).y * 8 - 26) * ImgZoom,
                                            24 * ImgZoom - 1, 42 * ImgZoom - 1)
                        End If
                    Case 5 '倒大绿花
                        G.DrawImage(
                            Magnifier(Image.FromFile(Application.StartupPath & "\img\Pack\" & ComboBox3.Text & "_Model\" &
                              ComboBox3.Text & "_Enemy_packun.Nin_NX_NVN\wait.0.png"), 2 * ImgZoom),
                            SpikeBlk(UBound(SpikeBlk)).x * 8 * ImgZoom,
                            (SpikeBlk(UBound(SpikeBlk)).y * 8 + 48) * ImgZoom, 32 * ImgZoom, -48 * ImgZoom)
                        If CkHitbox.Checked Then
                            G.DrawRectangle(Pens.Red, (SpikeBlk(UBound(SpikeBlk)).x * 8 + 4) * ImgZoom,
                                            SpikeBlk(UBound(SpikeBlk)).y * 8 * ImgZoom,
                                            24 * ImgZoom - 1, 42 * ImgZoom - 1)
                        End If
                    Case 3 '黑花
                        G.DrawImage(
                        Magnifier(Image.FromFile(Application.StartupPath & "\img\Pack\" & ComboBox3.Text & "_Model\" &
                          ComboBox3.Text & "_Enemy_packunblack.Nin_NX_NVN\wait.0.png"), ImgZoom),
                        SpikeBlk(UBound(SpikeBlk)).x * 8 * ImgZoom,
                        SpikeBlk(UBound(SpikeBlk)).y * 8 * ImgZoom, 16 * ImgZoom, 16 * ImgZoom)
                    Case 99
                        G.DrawImage(Magnifier(GetTile(SpikeBlk(UBound(SpikeBlk)).tx, SpikeBlk(UBound(SpikeBlk)).ty), ImgZoom),
                         SpikeBlk(UBound(SpikeBlk)).x * 8 * ImgZoom,
                         SpikeBlk(UBound(SpikeBlk)).y * 8 * ImgZoom, 16 * ImgZoom, 16 * ImgZoom)
                End Select

                'Case MouseButtons.Left '画砖
                '    If GrdBlk Is Nothing Then
                '        ReDim Preserve GrdBlk(0)
                '        GrdBlk(0).X = e.X \ 80
                '        GrdBlk(0).Y = e.Y \ 80
                '    Else
                '        ReDim Preserve GrdBlk(UBound(GrdBlk) + 1)
                '        GrdBlk(UBound(GrdBlk)).X = e.X \ 80
                '        GrdBlk(UBound(GrdBlk)).Y = e.Y \ 80
                '    End If
                '    G.DrawImage(Magnifier(GetTile(9, 12), 5), (e.X \ 80) * 80, (e.Y \ 80) * 80, 80, 80)

                'Case MouseButtons.Middle '设置像机，弃用
                '    If CamBlk Is Nothing Then
                '        ReDim Preserve CamBlk(0)
                '        CamBlk(0).X = e.X \ 80
                '        CamBlk(0).Y = e.Y \ 80
                '    ElseIf UBound(CamBlk) = 1 Then
                '        ReDim CamBlk(0)
                '        CamBlk(0).X = e.X \ 80
                '        CamBlk(0).Y = e.Y \ 80
                '    Else
                '        ReDim Preserve CamBlk(1)
                '        CamBlk(1).X = e.X \ 80
                '        CamBlk(1).Y = e.Y \ 80
                '    End If
                '    G.DrawImage(Image.FromFile(Application.StartupPath & "\img\C.png"), (e.X \ 80) * 80, (e.Y \ 80) * 80, 80, 80)
        End Select
        PB.Image = B
        PB.Refresh()
    End Sub
    Sub ReDraw()
        Dim CU As Integer
        Dim t = Txt2Cmd(T2.Text, CU)
        If t.Length = 0 Then Exit Sub
        T2.Text = t
        '增加调整按钮
        RefBtn(t)
        TestJump(t, True)
        Draw2(T2.Text, False)
    End Sub

    Sub ReDrawMulti()
        Dim CU As Integer
        DrawBG(NumW.Value, NumH.Value)
        Dim B As Bitmap = PB.Image
        Dim G As Graphics = Graphics.FromImage(B)
        Dim t As String

        t = Txt2Cmd(T2.Text, CU)
        If t.Length > 0 Then
            T2.Text = t
            RefBtn(t)
            TestJump(t, True)
            G.DrawImage(Draw3(t, Pens.White), 0, 0)
        End If
        t = Txt2Cmd(T20.Text, CU)
        If t.Length > 0 Then
            T20.Text = t
            TestJump(t, True)
            G.DrawImage(SetOpacity(Draw3(t, Pens.Yellow), 0.5), 0, 0)
        End If
        t = Txt2Cmd(T21.Text, CU)
        If t.Length > 0 Then
            T21.Text = t
            TestJump(t, True)
            G.DrawImage(SetOpacity(Draw3(t, Pens.GreenYellow), 0.5), 0, 0)
        End If
        PB.Image = B
    End Sub
    Sub RefBtn(t As String)
        Dim s() = t.Split(" ")
        Dim i, k, m As Integer

        For i = 0 To Math.Min(s.Length - 1, 99)
            GetNum(s(i), k, m)
            With CmdBtn(i)
                .Label1.Text = GetCnCmd(GetCmd(s(i)), False)
                .Label2.Text = m
                .Visible = True
            End With
        Next
        For i = Math.Min(s.Length - 1, 99) + 1 To 99
            CmdBtn(i).Visible = False
        Next
        PB.SendToBack()
    End Sub
    Private Sub BtnP1_Click(sender As Object, e As EventArgs) Handles BtnP1.Click
        TX1.Text = (Val(TX1.Text) + 1).ToString
        ReDraw()
    End Sub

    Private Sub BtnP01_Click(sender As Object, e As EventArgs) Handles BtnP01.Click
        TX1.Text = ((Val(TX1.Text) * 10 + 1) / 10).ToString
        ReDraw()
    End Sub

    Private Sub BtnM1_Click(sender As Object, e As EventArgs) Handles BtnM1.Click
        TX1.Text = (Val(TX1.Text) - 1).ToString
        ReDraw()
    End Sub

    Private Sub BtnM01_Click(sender As Object, e As EventArgs) Handles BtnM01.Click
        TX1.Text = ((Val(TX1.Text) * 10 - 1) / 10).ToString
        ReDraw()
    End Sub


    Private Sub TYW_ValueChanged(sender As Object, e As EventArgs) Handles TYW.ValueChanged
        ReDraw()
    End Sub

    Private Sub TB_ValueChanged(sender As Object, e As EventArgs) Handles TB.ValueChanged
        ReDraw()
    End Sub

    Private Sub TYH_ValueChanged(sender As Object, e As EventArgs) Handles TYH.ValueChanged
        TY1.Text = TYH.Value.ToString
        ReDraw()
    End Sub

    Private Sub NumW_ValueChanged(sender As Object, e As EventArgs) Handles NumW.ValueChanged
        ReDraw()
    End Sub

    Private Sub NumH_ValueChanged(sender As Object, e As EventArgs) Handles NumH.ValueChanged
        ReDraw()
    End Sub

    Private Sub NumericUpDown1_ValueChanged(sender As Object, e As EventArgs)
        ReDraw()
    End Sub

    Private Sub BtnMont_Click(sender As Object, e As EventArgs) Handles BtnMont.Click
        '多线程搜索测试
        SearchMoveSpikeM(T2.Text, Val(TSPX1.Text), Val(TSPX2.Text))
    End Sub

    Structure MoveNode
        Dim Node As Collection(Of MoveNode)
        Dim State As MarioFrameData
    End Structure
    Sub SearchMoveMont(RN As Single, ST As Single)
        '搜索

    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox1.SelectedIndexChanged
        CharActName = ComboBox1.Text
        GetCharAct()
    End Sub

    Private Sub CkMario_CheckedChanged(sender As Object, e As EventArgs) Handles CkMario.CheckedChanged
        GetCharAct()
    End Sub

    Private Sub ComboBox3_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox3.SelectedIndexChanged
        CharPack = ComboBox3.Text
        GetCharAct()
    End Sub

    Private Sub CkDuck_CheckedChanged(sender As Object, e As EventArgs) Handles CkDuck.CheckedChanged
        GetCharAct()
    End Sub

    Dim TasData() As String
    Private Sub BTas_Click(sender As Object, e As EventArgs) Handles BTas.Click
        Dim i, j, z As Integer
        Dim CL, CU, CF As Integer
        Dim t As String, LastOri As String = ""
        t = T20.Text & " " & T2.Text & " " & T21.Text
        t = t.Replace("停0", "停0 站立10")
        Dim s() = t.Split(" ")
        ReDim TasData(s.Length - 1)
        z = 0
        For i = 0 To s.Length - 1
            If s(i).Length > 0 Then
                GetNum(s(i), CL, CU)
                If CU = 0 Then
                    z += 60
                Else
                    z += CU
                End If
                s(i) = GetCnCmd(GetCmd(s(i)), True) & CL.ToString & "-" & CU.ToString
            End If
        Next
        ReDim TasData(z - 1)
        T1.Text = ""
        z = 0
        For i = 0 To s.Length - 1
            '3 KEY_DLEFT;KEY_DDOWN 0;0 0;0
            '帧 按键;按键 0;0 0;0
            'A B X Y
            t = " "
            If s(i).Length > 0 Then
                If InStr(s(i), "正") > 0 Then
                    Select Case LastOri
                        Case ""

                        Case "R"
                            t &= "KEY_DRIGHT;KEY_DDOWN;"
                            LastOri = "R"
                        Case "L"
                            t &= "KEY_DLEFT;KEY_DDOWN;"
                            LastOri = "L"
                    End Select
                End If
                If InStr(s(i), "反") > 0 Then
                    Select Case LastOri
                        Case ""

                        Case "R"
                            t &= "KEY_DLEFT;KEY_DDOWN;"
                            LastOri = "L"
                        Case "L"
                            t &= "KEY_DRIGHT;KEY_DDOWN;"
                            LastOri = "R"
                    End Select
                End If
                If InStr(s(i), "右") > 0 Then
                    t &= "KEY_DRIGHT;"
                    LastOri = "R"
                End If
                If InStr(s(i), "左") > 0 Then
                    t &= "KEY_DLEFT;"
                    LastOri = "L"
                End If
                If InStr(s(i), "旋转跳") > 0 Then
                    t &= "KEY_R;"
                ElseIf InStr(s(i), "旋转") > 0 Then
                    t &= "KEY_R;"
                ElseIf InStr(s(i), "缓冲跳") > 0 Then

                ElseIf InStr(s(i), "跳") > 0 Then
                    t &= "KEY_A;"
                End If
                If InStr(s(i), "滞空") > 0 Then
                    t &= "KEY_A;"
                End If
                If InStr(s(i), "加速") > 0 Then
                    t &= "KEY_Y;"
                End If
                If InStr(s(i), "跑") > 0 Then
                    t &= "KEY_Y;"
                End If
                If InStr(s(i), "转") > 0 Then
                    t &= "KEY_R;"
                End If
                If InStr(s(i), "上") > 0 Then
                    t &= "KEY_DUP;"
                End If
                If InStr(s(i), "下") > 0 Then
                    t &= "KEY_DDOWN;"
                End If
                If InStr(s(i), "D") > 0 Then
                    t &= "KEY_DDOWN;"
                End If
                If InStr(s(i), "+") > 0 Then
                    t &= "KEY_PLUS;"
                End If
                t = Strings.Left(t, t.Length - 1)
                t &= " 0;0 0;0"

                GetNum(s(i), CL, CU)
                If CU = 0 Then CU = 60
                For j = 1 To CU
                    TasData(z) = z.ToString & t
                    T1.Text &= TasData(z) & vbCrLf
                    z += 1
                Next
            End If
        Next

        File.WriteAllLines(Application.StartupPath & "Tas\Script0-1.txt", TasData)

        LblCal.Text = DateTime.Now & " 已保存Script0-1"
    End Sub

    Dim YuzuM As MemoryScanner, isFind As Boolean = False
    Dim XAddr, YAddr, SxAddr, SyAddr, AyAddr, AxAddr As String
    Private Sub BReadMem_Click(sender As Object, e As EventArgs) Handles BReadMem.Click
        BReadMem.Enabled = False
        If TimerEmu.Enabled Then
            TimerEmu.Enabled = False
            LblCal.Text = Date.Now & " 已关闭Emu"
            T1.Text = "[X ]" & XAddr & vbCrLf & "[Y ]" & YAddr & vbCrLf &
                "[Sx]" & SxAddr & vbCrLf & "[Sy]" & SyAddr & vbCrLf &
                "[Ax]" & AxAddr & vbCrLf & "[Ay]" & AyAddr
            BReadMem.Enabled = True
            Exit Sub
        End If
        '7B 14 AE BE CD CC 4C 3D -H50
        '70 54 85 82 00 00 00 00 50 8C 00 99 21 00 00 00 00 E5 8D 80 +H30
        Dim F = &H3D4CCCCDBEAE147B
        If Not isFind Then
            YuzuM = New MemoryScanner(Function(p) p.ProcessName = "yuzu")

            For i = &H10000008ED0L To &H3F000008ED0L Step &H100000L
                If YuzuM.ReadMemory(Of Long)(i) = F Then
                    F = i
                    XAddr = F - &H50
                    YAddr = F - &H50 + &H4
                    SxAddr = F - &H50 + &HC
                    SyAddr = F - &H50 + &H10
                    AyAddr = F - &H50 + &H50
                    AxAddr = F - &H50 + &H54
                    If YuzuM.ReadMemory(Of Byte)(XAddr) = 72 Then
                        isFind = True
                    End If
                    Exit For
                End If
            Next
        End If

        If isFind Then
            'T1.Text = "[X ]" & XAddr & vbCrLf & "[Y ]" & YAddr & vbCrLf &
            '    "[Sx]" & SxAddr & vbCrLf & "[Sy]" & SyAddr & vbCrLf &
            '    "[Ax]" & AxAddr & vbCrLf & "[Ay]" & AyAddr
            Debug.Print("X = {0} , Y = {1} , Sx = {2} , Sy = {3}", XAddr, YAddr, SxAddr, SyAddr)
            TimerEmu.Enabled = True
            LblCal.Text = Date.Now & " 已开启Emu"
        Else
            LblCal.Text = Date.Now & " 未找到基址"
        End If
        BReadMem.Enabled = True
    End Sub
    Sub ReadData()
        Dim LX, NX As Integer
        Dim Re(1) As Collection(Of Single)
        Dim ReH(1) As Collection(Of Integer)
        Dim i As Integer

        Dim M = New MemoryScanner(Function(p) p.ProcessName = "yuzu")
        For i = 0 To 0
            Re(i) = New Collection(Of Single)
            ReH(i) = New Collection(Of Integer)
        Next
        LX = -1
        Do
            Application.DoEvents()
            NX = M.ReadMemory(Of Single)(XAddr)
            'NY = M.ReadInt(YAddr)
            If NX <> LX Then
                LX = NX
                'LY = NY
                Re(0).Add(M.ReadMemory(Of Single)(XAddr))
                ReH(0).Add(NX)
                'Re(1).Add(M.ReadFloat(YAddr))
                'ReH(1).Add(NY)
            End If
        Loop Until GetAsyncKeyState(Keys.E)

        Dim R = ""
        For i = 0 To Re(0).Count - 1
            R &= i.ToString & vbTab
            For j = 0 To 0
                R &= Re(j).Item(i).ToString & vbTab & Strings.Right("00000000" & Hex(ReH(j).Item(i)), 8) & vbTab & Hex2Dbl(Strings.Right("00000000" & Hex(ReH(j).Item(i)), 8))
            Next
            R &= vbCrLf
        Next

        T1.Text = R
        Clipboard.Clear()
        Clipboard.SetText(R)
    End Sub
    Structure InsData
        Dim Loc As List(Of Integer)
        Dim Frame As List(Of Integer)
    End Structure


    Sub FindIns(D As Integer) '找插入
        LblState.Text = Date.Now & " 开始搜索"
        LBox.Items.Clear()
        Dim i, j, k, LF, NF, UF, F, A, CL, CU As Integer
        Dim X, CX, CA, EX As Single
        Dim Mx(), Ms() As Single
        T20.Text = Txt2Cmd(T20.Text, LF)
        T2.Text = Txt2Cmd(T2.Text, NF)
        NF += LF
        T21.Text = Txt2Cmd(T21.Text, UF)
        TestJump(T20.Text & " " & T2.Text & " " & T21.Text, False)
        F = FrameLoc - 1
        ReDim Mx(F), Ms(F)
        Mx(0) = Val(TX1.Text)
        Ms(0) = Val(TS2.Text)
        Debug.Print("帧{0}:X={1},S={2}", 0, Mx(i), Ms(i))
        For i = 1 To F
            Mx(i) = FrameData(i).X
            Ms(i) = FrameData(i).Sx
            Debug.Print("帧{0}:X={1},S={2}", i, Mx(i), Ms(i))
        Next

        EX = FrameData(F).X '末位置
        CX = Val(TR2.Text) '目标
        CA = Val(TR3.Text) '容错
        A = Val(TR4.Text) '帧数

        Dim InsLoc = New List(Of Integer) '有效位置
        Dim RCmd = GetInsCmd(T2.Text)
        T2.Text = RCmd
        Dim Cmd() = RCmd.Split(" ")
        LF += GetAirCmd(RCmd)
        Debug.Print("LF={0} NF={1} UF={2}", LF, NF, UF)
        F = 0
        Dim C1F As Integer = If(CB1F.Checked, 1, 2)
        For i = 0 To Cmd.Length - 1
            GetNum(Cmd(i), CL, CU)
            If F >= LF AndAlso F <= NF Then
                For j = C1F To CU - C1F
                    InsLoc.Add(F + j)
                    Debug.Print(Cmd(i) & "->" & j.ToString & "[" & (F + j).ToString & "]")
                Next
                If i < Cmd.Length - 1 Then
                    InsLoc.Add(F + CU)
                    Debug.Print(Cmd(i) & "->" & CU.ToString & "[" & (F + CU).ToString & "]")
                End If
            End If
            F += CU
        Next

        Dim Deep As Integer = NIns.Value
        Dim C As New Combination
        Dim R, RR As List(Of Integer())
        Dim CF() As Integer
        'Dim TR As String
        Debug.Print(Date.Now & " 开始搜索")

        For i = 1 To Deep
            Application.DoEvents()
            Debug.Print(Date.Now & " [搜索] D -> " & i.ToString)
            R = C.GetComb(InsLoc, i)
            RR = New List(Of Integer())
            For Each R2 In R
                For k = 0 To R2.Length - 2
                    If R2(k + 1) - R2(k) <= 1 Then
                        Exit For
                    End If
                Next
                If k = R2.Length - 1 Then
                    RR.Add(R2)
                End If
            Next

            ReDim CF(i - 1)
            For j = 0 To i - 1
                CF(j) = C1F
            Next
            Do
                Application.DoEvents()
                For Each R2 In RR
                    X = EX
                    For k = 0 To R2.Length - 1
                        '第R2(k)位置插入CF(k)帧
                        'EX = FrameData(F).X '末位置
                        'CX = Val(TR2.Text) '目标
                        'CA = Val(TR3.Text) '容错
                        X += Ms(R2(k)) * CF(k)
                    Next
                    If Math.Abs(X - CX) <= CA Then
                        'TR = ""
                        'For k = 0 To R2.Length - 1
                        '    TR &= R2(k).ToString & "*" & CF(k).ToString & "F "
                        'Next
                        LBox.Items.Add("[" & X.ToString & "] " & OutputInsCmd(RCmd, R2, CF))
                    End If
                Next
                CF(i - 1) += 1
                For k = i - 1 To 1 Step -1
                    If CF(k) > A Then
                        CF(k) = C1F
                        CF(k - 1) += 1
                    End If
                Next
            Loop Until CF(0) > A
        Next

        LblState.Text = Date.Now & " 搜索完成 -> " & LBox.Items.Count.ToString
        Debug.Print(Date.Now & " 搜索完成")
    End Sub
    Function OutputInsCmd(Cmd As String, InsR() As Integer, CF() As Integer) As String
        Dim i As Integer
        Dim S() = SplitCmd(Cmd).Split(" ")
        Dim ZR As Integer = 0
        Dim R As String = ""
        For i = 0 To S.Length - 1
            If ZR = InsR.Length Then
                R &= S(i) & " "
            ElseIf i = InsR(ZR) Then
                R &= "跳" & CF(ZR).ToString & " "
                R &= S(i) & " "
                ZR += 1
            Else
                R &= S(i) & " "
            End If
        Next
        Return GetInsCmd(R.TrimEnd)
    End Function
    Function SplitCmd(Cmd As String) As String
        Dim i, j As Integer
        Dim CL, CU As Integer
        Dim S() = Cmd.Split(" ")
        Dim CS As String
        Dim R As String = ""
        For i = 0 To S.Length - 1
            GetNum(S(i), CL, CU)
            CS = GetCmd(S(i))
            For j = 1 To CU
                R &= CS & "1" & " "
            Next
        Next
        Return R.TrimEnd
    End Function
    Function GetInsCmd(Cmd As String) As String
        Dim S() = Cmd.Split(" ")
        Dim CL, CU, CL2, CU2 As Integer
        Dim TU As Integer
        Dim R As String = ""
        For i As Integer = 0 To S.Length - 1
            GetNum(S(i), CL, CU)
            TU = CU
            For j As Integer = i + 1 To S.Length - 1
                If GetCmd(S(i)) = GetCmd(S(j)) Then
                    GetNum(S(j), CL2, CU2)
                    TU += CU2
                    i += 1
                Else
                    Exit For
                End If
            Next
            R &= GetCmd(S(i)) & TU.ToString & " "
        Next
        Return R.TrimEnd
    End Function
    Function GetAirCmd(S As String) As Integer
        Dim T() = S.Split(" ")
        Dim CL, CU As Integer
        Dim F As Integer = 0
        For i As Integer = 0 To T.Length - 1
            If InStr(T(i), "跳") > 0 Then
                Return F
            Else
                GetNum(T(i), CL, CU)
                F += CU
            End If
        Next
        Return 0
    End Function

    Private Sub BCmdR_Click(sender As Object, e As EventArgs) Handles BCmdR.Click
        T2.Text = RCmd（T2.Text）
    End Sub

    Private Sub BtnUndo_Click(sender As Object, e As EventArgs) Handles BtnUndo.Click
        If SpikeBlk.Length > 0 Then
            ReDim Preserve SpikeBlk(SpikeBlk.Length - 2)
        End If
        DrawBG(NumW.Value, NumH.Value)
    End Sub

    Private Sub BtnGenJump_Click(sender As Object, e As EventArgs) Handles BtnGenJump.Click

    End Sub

    Private Sub LHis_SelectedIndexChanged(sender As Object, e As EventArgs) Handles LHis.SelectedIndexChanged
        Dim S() = LHis.SelectedItem.ToString.Split("|")
        If S.Length = 3 Then
            T20.Text = S(0)
            T2.Text = S(1)
            T21.Text = S(2)
        End If
    End Sub

    Private Sub BAddHis_Click(sender As Object, e As EventArgs) Handles BAddHis.Click
        LHis.Visible = True
        LHis.Items.Add(T20.Text & "|" & T2.Text & "|" & T21.Text)
    End Sub

    Private Sub TimerEmu_Tick(sender As Object, e As EventArgs) Handles TimerEmu.Tick
        Dim p(5) As String
        p(0) = Hex(YuzuM.ReadMemory(Of Single)(XAddr)).PadLeft(8, "0"c)
        p(1) = Hex(YuzuM.ReadMemory(Of Single)(YAddr)).PadLeft(8, "0"c)
        p(2) = Hex(YuzuM.ReadMemory(Of Single)(SxAddr)).PadLeft(8, "0"c)
        p(3) = Hex(YuzuM.ReadMemory(Of Single)(SyAddr)).PadLeft(8, "0"c)
        p(4) = Hex(YuzuM.ReadMemory(Of Single)(AxAddr)).PadLeft(8, "0"c)
        p(5) = Hex(YuzuM.ReadMemory(Of Single)(AyAddr)).PadLeft(8, "0"c)

        T1.Text = "X =[" & p(0) & "]" & Hex2Dbl(p(0)) & vbCrLf &
                    "Y =[" & p(1) & "]" & Hex2Dbl(p(1)) & vbCrLf &
                    "Sx=[" & p(2) & "]" & Hex2Dbl(p(2)) & vbCrLf &
                    "Sy=[" & p(3) & "]" & Hex2Dbl(p(3)) & vbCrLf &
                    "Ax=[" & p(4) & "]" & Hex2Dbl(p(4)) & vbCrLf &
                    "Ay=[" & p(5) & "]" & Hex2Dbl(p(5))
    End Sub

    Private Sub BEmu_Click(sender As Object, e As EventArgs) Handles BEmu.Click
        'If isFind Then
        '    TimerEmu.Enabled = Not TimerEmu.Enabled
        '    Me.Text = If(TimerEmu.Enabled, "ON", "OFF")
        'End If
    End Sub

    Private Sub BtnTile_Click(sender As Object, e As EventArgs) Handles BtnTile.Click
        CBItem.BackColor = Color.White
        BtnTile.BackColor = Color.LightBlue
        DrawTileMode = 1
        LoadPItem()
        PItem.Visible = Not PItem.Visible
    End Sub

    Sub LoadPItem()
        PItem.Image = Image.FromFile(Application.StartupPath & "\img\Model\" & ComboBox3.Text & "_Field_" &
                              ComboBox4.Text & If(CheckBox7.Checked, "_D", "") &
                              ".Nin_NX_NVN\" & ComboBox3.Text & "_Field_" &
                              ComboBox4.Text & If(CheckBox7.Checked, "_D", "") & ".png")
    End Sub
    Dim SelBackTile As Bitmap, SelBackTileLoc As Point, DrawTileMode As Integer
    Private Sub PItem_MouseClick(sender As Object, e As MouseEventArgs) Handles PItem.MouseClick
        If PItem.IsPointInImage(e.X, e.Y) Then
            SelBackTileLoc = PItem.PointToImage(e.X, e.Y)
            SelBackTileLoc.X \= 16
            SelBackTileLoc.Y \= 16
            SelBackTile = GetTile(SelBackTileLoc.X, SelBackTileLoc.Y)
            BtnTile.Image = SelBackTile
        End If
    End Sub

    Private Sub BtnTile2_Click(sender As Object, e As EventArgs) Handles BtnTile2.Click
        CBItem.Items.Add(SelBackTile)
    End Sub

    Private Sub CBItem_SelectedIndexChanged(sender As Object, e As EventArgs) Handles CBItem.SelectedIndexChanged
        CBItem.BackColor = Color.LightBlue
        BtnTile.BackColor = Color.White
        DrawTileMode = 0
        PItem.Visible = False
    End Sub

    Private Sub CBItem_MouseClick(sender As Object, e As MouseEventArgs) Handles CBItem.MouseClick
        CBItem.BackColor = Color.LightBlue
        BtnTile.BackColor = Color.White
        DrawTileMode = 0
        PItem.Visible = False
    End Sub
    Dim ImgZoom As Integer = 5
    Private Sub NumZoom_ValueChanged(sender As Object, e As EventArgs) Handles NumZoom.ValueChanged
        ImgZoom = NumZoom.Value
        GetCharAct()
    End Sub

End Class
