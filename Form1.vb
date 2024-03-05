
Imports System.Drawing.Imaging
Imports System.IO
Imports System.Runtime.InteropServices
Imports System.Security.Cryptography
Imports System.Threading
Partial Class Form1
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
    Function GetWalkFrame(f As Integer, SPX As Single) As Integer
        '未测试
        Select Case Math.Abs(SPX)
            Case Is = 0
                Return 0
            Case Is < 0.7
                Return 1
            Case Is < 1.1
                Return 2
            Case Is < 1.5
                Return 3
            Case Is < 1.75
                Return 1
            Case Is < 2
                Return 2
            Case Is < 2.15
                Return 3
            Case Is < 2.25
                Return 1
            Case Else
                Return ((f \ 3) Mod 3) + 1
        End Select
    End Function

    Private Sub SaveCamImg(B As Bitmap, F As Integer)
        Dim BB As New Bitmap((CamBlk(1).X - CamBlk(0).X) * 80 + 80, (CamBlk(1).Y - CamBlk(0).Y) * 80 + 80)
        Dim GG As Graphics = Graphics.FromImage(BB)
        GG.DrawImage(B, New Rectangle(0, 0, BB.Width, BB.Height), New Rectangle(CamBlk(0).X * 80, CamBlk(0).Y * 80, BB.Width, BB.Height), GraphicsUnit.Pixel)
        BB.Save(Application.StartupPath & "\temp\" & F.ToString & ".PNG", Imaging.ImageFormat.Png)
    End Sub

    Private Function IsHitSpike(X As Single, Y As Single) As Boolean
        If SpikeBlk Is Nothing Then
            Return False
        End If
        '碰撞判定 16*12
        '伤害判定 16*14 16*7
        '刺
        '绿花8*21
        For i As Integer = 0 To UBound(SpikeBlk)
            Select Case BlkType(i)
                Case 0 '刺
                    If RectSetup(0, X, (NumH.Value - 3) * 16 - Y + 4, 0, 0, CSng(SpikeBlk(i).X * 16), CSng(SpikeBlk(i).Y * 16), 0, 0) Then
                        Return True
                    End If
                Case 1 '绿花
                    If RectSetup(1,
                                 X, (NumH.Value - 3) * 16 - Y + IIf(CkDuck.Checked, 9, 2),
                                 16, IIf(CkDuck.Checked, 7, 14),
                                 CSng(SpikeBlk(i).X * 16), CSng(SpikeBlk(i).Y * 16) - 8,
                                 8, 21) Then
                        Return True
                    End If
                Case 2 '倒绿花
                    If RectSetup(1,
                                 X, (NumH.Value - 3) * 16 - Y + IIf(CkDuck.Checked, 9, 2),
                                 16, IIf(CkDuck.Checked, 7, 14),
                                 CSng(SpikeBlk(i).X * 16), CSng(SpikeBlk(i).Y * 16),
                                 8, 21) Then
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
                    .h = 12
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
            Case Else
                With SRect(0) 'Mario
                    .x = x1
                    .y = y1
                    .w = w1
                    .h = h1
                End With
                With SRect(1) '0.1-3.1
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
        End Select
        Return False
    End Function
    Private Function Draw2(IsSave As Boolean) As Integer
        If CamBlk Is Nothing Then
            ReDim CamBlk(1)
            CamBlk(0).X = 0
            CamBlk(0).Y = 0
            CamBlk(1).X = NumW.Value - 1
            CamBlk(1).Y = NumH.Value - 1
        End If
        Dim FRM As Integer = Val(NumericUpDown1.Text)
        DrawBG(NumW.Value, NumH.Value)
        Dim B As Bitmap = PB.Image
        Dim tempB As Bitmap
        Dim G As Graphics = Graphics.FromImage(B)
        Dim S() As String = CResult.Split(vbCrLf)
        Dim SS() As String, i, j As Integer
        Dim Ps(), Pz() As Point, z, zz As Integer, ZL As Integer, Pk(,) As Single
        Dim SSS() As String = T2.Text.Split(" ")
        Dim SpdX() As Single
        'For i = 0 To 20
        '    Lbl(i).Visible = False
        'Next

        ReDim Ps(UBound(S)), Pz(UBound(SSS)), SpdX(UBound(S)), Pk(UBound(S), 1)
        z = 0
        For i = 0 To UBound(S)
            SS = S(i).Split(vbTab)
            If UBound(SS) > 2 Then
                Ps(i).X = Val(SS(0)) * 5
                Ps(i).Y = Val(SS(1)) * 5
                SpdX(i) = Val(SS(2))
                Pk(i, 0) = Val(SS(0))
                Pk(i, 1) = Val(SS(1))
                z += 1
            Else
                Ps(i).X = 999
                Ps(i).Y = 999
                Pk(i, 0) = 999
                Pk(i, 1) = 999
                SpdX(i) = 0
            End If
        Next

        '计算钻洞最优偏差
        Dim mx, mi, nx, ni As Single
        'ListBox1.Items.Clear()
        For i = z - 1 To 0 Step -1
            zz = 1
            mx = Pk(i, 0)
            mi = Pk(i, 0)
            nx = mx
            ni = mi
            For j = i - 1 To 0 Step -1
                If Pk(j, 0) > mx Then mx = Pk(j, 0)
                If Pk(j, 0) < mi Then mi = Pk(j, 0)
                If Math.Abs(mx - mi) <= 0.2 Then
                    nx = mx
                    ni = mi
                    zz += 1
                Else
                    Exit For
                End If
            Next
            If zz >= 4 Then
                Label3.Text = zz.ToString & "帧 " & ((ni + nx) / 2).ToString & "[" & Pk(i, 0).ToString & "]-> " & ni.ToString & " - " & nx.ToString
            End If
        Next

        '刷新人物贴图
        GetCharAct()
        G.DrawImage(CharAct(0), CSng(TX1.Text) * 5 + OFX, OFY + 5 + (NumH.Value * 80) - CSng(TY1.Text) * 5 - 240, CHW, CHH)

        Dim ii, jj As Integer
        Dim TxtW As Integer
        If CkMove.Checked Then
            TxtW = GetStrW(SSS(0)) \ 2
            For ii = -1 To 1
                For jj = -1 To 1
                    G.DrawString(SSS(0), Label1.Font, Brushes.White, Val(TX1.Text) * 5 - TxtW + 40 + ii, 5 + (NumH.Value * 80) - (Val(TY1.Text) * 5) - 240 - 20 + jj)
                Next
            Next
            G.DrawString(SSS(0), Label1.Font, Brushes.Black, Val(TX1.Text) * 5 - TxtW + 40, 5 + (NumH.Value * 80) - (Val(TY1.Text) * 5) - 240 - 20)
        End If

        tempB = New Bitmap(B)
        ZL = 0
        j = 0
        z = 0
        If CkTrace.Checked Then
            For i = 0 To UBound(S) - 1 ' Step -1
                If Ps(i + 1).Y = 999 Then
                    If Ps(i).Y = 0 Then
                        If IsHitSpike(Pk(i, 0), Pk(i, 1)) Then
                            G.DrawImage(SetReverseColor(CharAct(GetWalkFrame(i, SpdX(i)))), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 240 - Ps(i).Y, CHW, CHH)
                        Else
                            G.DrawImage(CharAct(GetWalkFrame(i, SpdX(i))), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 240 - Ps(i).Y, CHW, CHH)
                        End If
                    Else
                        If IsHitSpike(Pk(i, 0), Pk(i, 1)) Then
                            G.DrawImage(SetReverseColor(CharAct(4)), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 240 - Ps(i).Y, CHW, CHH)
                        Else
                            G.DrawImage(CharAct(4), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 240 - Ps(i).Y, CHW, CHH)
                        End If
                    End If
                    If CkFrame.Checked Then G.DrawString(i.ToString, Label1.Font, Brushes.White, Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 220 - Ps(i).Y)
                    If CkHitbox.Checked Then
                        G.DrawRectangle(Pens.White, Ps(i).X, 5 + (NumH.Value * 80) - 240 - Ps(i).Y + 20, 79, 59)
                    End If
                    z = 0
                    ZL += 1
                    If ZL <= UBound(SSS) Then
                        If CkMove.Checked Then
                            TxtW = GetStrW(SSS(ZL)) \ 2
                            For ii = -1 To 1
                                For jj = -1 To 1
                                    G.DrawString(SSS(ZL), Label1.Font, Brushes.White, Ps(i).X - TxtW + 40 + ii, 5 + (NumH.Value * 80) - 240 - Ps(i).Y - 20 + jj)
                                Next
                            Next
                            G.DrawString(SSS(ZL), Label1.Font, Brushes.Black, Ps(i).X - TxtW + 40, 5 + (NumH.Value * 80) - 240 - Ps(i).Y - 20)
                        End If
                    End If
                Else
                    z = (z + 1) Mod FRM
                    If z = FRM - 1 AndAlso Ps(i).Y <> 999 Then
                        If Ps(i).Y = 0 Then
                            If IsHitSpike(Pk(i, 0), Pk(i, 1)) Then
                                G.DrawImage(SetOpacity(SetReverseColor(CharAct(GetWalkFrame(i, SpdX(i)))), 0.5), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 240 - Ps(i).Y, CHW, CHH)
                            Else
                                G.DrawImage(SetOpacity(CharAct(GetWalkFrame(i, SpdX(i))), 0.5), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 240 - Ps(i).Y, CHW, CHH)
                            End If
                        Else
                            If IsHitSpike(Pk(i, 0), Pk(i, 1)) Then
                                G.DrawImage(SetOpacity(SetReverseColor(CharAct(4)), 0.5), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 240 - Ps(i).Y, CHW, CHH)
                            Else
                                G.DrawImage(SetOpacity(CharAct(4), 0.5), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 240 - Ps(i).Y, CHW, CHH)
                            End If
                        End If
                        If CkFrame.Checked Then G.DrawString(i.ToString, Label1.Font, Brushes.White, Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 220 - Ps(i).Y)
                        If CkHitbox.Checked Then
                            G.DrawRectangle(Pens.White, Ps(i).X, 5 + (NumH.Value * 80) - 240 - Ps(i).Y + 20, 79, 59)
                        End If
                    End If
                End If
                If IsSave Then
                    If ZL <= UBound(SSS) AndAlso CkCtrl.Checked Then
                        G.DrawImage(GetButtonImg(Cmd2Button(GetCmd(SSS(ZL)))), 0, (NumH.Value * 80) - 80, 240, 80)
                    End If
                    'B.Save(Application.StartupPath & "\temp\" & j.ToString & ".PNG", Imaging.ImageFormat.Png)
                    SaveCamImg(B, j)
                    j += 1
                End If
            Next
        Else
            For i = 0 To UBound(S) - 1 ' Step -1
                B = New Bitmap(tempB)
                G = Graphics.FromImage(B)
                If Ps(i + 1).Y = 999 Then
                    If Ps(i).Y = 0 Then
                        If IsHitSpike(Pk(i, 0), Pk(i, 1)) Then
                            G.DrawImage(SetReverseColor(CharAct(GetWalkFrame(i, SpdX(i)))), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 240 - Ps(i).Y, CHW, CHH)
                        Else
                            G.DrawImage(CharAct(GetWalkFrame(i, SpdX(i))), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 240 - Ps(i).Y, CHW, CHH)
                        End If
                    Else
                        If IsHitSpike(Pk(i, 0), Pk(i, 1)) Then
                            G.DrawImage(SetReverseColor(CharAct(4)), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 240 - Ps(i).Y, CHW, CHH)
                        Else
                            G.DrawImage(CharAct(4), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 240 - Ps(i).Y, CHW, CHH)
                        End If
                    End If
                    If CkFrame.Checked Then G.DrawString(i.ToString, Label1.Font, Brushes.White, Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 220 - Ps(i).Y)
                    If CkHitbox.Checked Then
                        G.DrawRectangle(Pens.White, Ps(i).X, 5 + (NumH.Value * 80) - 240 - Ps(i).Y + 20, 79, 59)
                    End If
                    z = 0
                    ZL += 1
                    If ZL <= UBound(SSS) Then
                        If CkMove.Checked Then
                            TxtW = GetStrW(SSS(ZL)) \ 2
                            For ii = -1 To 1
                                For jj = -1 To 1
                                    G.DrawString(SSS(ZL), Label1.Font, Brushes.White, Ps(i).X - TxtW + 40 + ii, 5 + (NumH.Value * 80) - 240 - Ps(i).Y - 20 + jj)
                                Next
                            Next
                            G.DrawString(SSS(ZL), Label1.Font, Brushes.Black, Ps(i).X - TxtW + 40, 5 + (NumH.Value * 80) - 240 - Ps(i).Y - 20)
                        End If
                    End If
                    tempB = New Bitmap(B)
                Else
                    z = (z + 1) Mod FRM
                    If z = FRM - 1 AndAlso Ps(i).Y <> 999 Then
                        If Ps(i).Y = 0 Then
                            If IsHitSpike(Pk(i, 0), Pk(i, 1)) Then
                                G.DrawImage(SetOpacity(SetReverseColor(CharAct(GetWalkFrame(i, SpdX(i)))), 0.5), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 240 - Ps(i).Y, CHW, CHH)
                            Else
                                G.DrawImage(SetOpacity(CharAct(GetWalkFrame(i, SpdX(i))), 0.5), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 240 - Ps(i).Y, CHW, CHH)
                            End If
                        Else
                            If IsHitSpike(Pk(i, 0), Pk(i, 1)) Then
                                G.DrawImage(SetOpacity(SetReverseColor(CharAct(4)), 0.5), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 240 - Ps(i).Y, CHW, CHH)
                            Else
                                G.DrawImage(SetOpacity(CharAct(4), 0.5), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 240 - Ps(i).Y, CHW, CHH)
                            End If
                        End If
                        If CkFrame.Checked Then G.DrawString(i.ToString, Label1.Font, Brushes.White, Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - 220 - Ps(i).Y)
                        If CkHitbox.Checked Then
                            G.DrawRectangle(Pens.White, Ps(i).X, 5 + (NumH.Value * 80) - 240 - Ps(i).Y + 20, 79, 59)
                        End If
                    End If
                End If
                If IsSave Then
                    If ZL <= UBound(SSS) AndAlso CkCtrl.Checked Then
                        G.DrawImage(GetButtonImg(Cmd2Button(GetCmd(SSS(ZL)))), 0, (NumH.Value * 80) - 80, 240, 80)
                    End If
                    'B.Save(Application.StartupPath & "\temp\" & j.ToString & ".PNG", Imaging.ImageFormat.Png)
                    SaveCamImg(B, j)
                    j += 1
                End If
            Next
        End If

        PB.Image = B
        Return j - 1
    End Function

    Private Function Draw2form(IsSave As Boolean) As Integer
        If CamBlk Is Nothing Then
            ReDim CamBlk(1)
            CamBlk(0).X = 0
            CamBlk(0).Y = 0
            CamBlk(1).X = NumW.Value - 1
            CamBlk(1).Y = NumH.Value - 1
        End If
        Dim FRM As Integer = Val(NumericUpDown1.Text)
        Dim B As Bitmap = New Bitmap(1920, 1080)
        Dim G As Graphics = Graphics.FromImage(B)
        Dim S() As String = CResult.Split(vbCrLf)
        Dim SS() As String, i As Integer
        Dim Ps(), Pz() As Point, z As Integer, ZL As Integer
        Dim SSS() As String = T2.Text.Split(" ")
        Dim SpdX() As Single
        For i = 0 To 20
            Lbl(i).Visible = False
        Next

        ReDim Ps(UBound(S)), Pz(UBound(SSS)), SpdX(UBound(S))
        For i = 0 To UBound(S)
            SS = S(i).Split(vbTab)
            If UBound(SS) > 2 Then
                Ps(i).X = Val(SS(0)) * 5
                Ps(i).Y = Val(SS(1) - 40) * 5 + 80
                SpdX(i) = Val(SS(2))
            Else
                Ps(i).X = 999
                Ps(i).Y = 999
                SpdX(i) = 0
            End If
        Next
        '刷新人物贴图
        GetCharAct()
        G.DrawImage(CharAct(0), CSng(TX1.Text) * 5 + OFX, OFY + 5 + (NumH.Value * 80) - CSng(TY1.Text) * 5 + 200 - 80, CHW, CHH)

        Dim ii, jj As Integer
        Dim TxtW As Integer
        If CkMove.Checked Then
            TxtW = GetStrW(If(CkFrame.Checked, SSS(0), GetCmd(SSS(0)))) \ 2
            For ii = -1 To 1
                For jj = -1 To 1
                    G.DrawString(If(CkFrame.Checked, SSS(0), GetCmd(SSS(0))), Label1.Font, Brushes.White, Val(TX1.Text) * 5 - TxtW + 40 + ii, 5 + (NumH.Value * 80) - 240 - 20 + jj)
                Next
            Next
            G.DrawString(If(CkFrame.Checked, SSS(0), GetCmd(SSS(0))), Label1.Font, Brushes.Black, Val(TX1.Text) * 5 - TxtW + 40, 5 + (NumH.Value * 80) - 240 - 20)
        End If

        ZL = 0
        z = 0
        If CkTrace.Checked Then
            For i = 0 To UBound(S) - 1 ' Step -1
                If Ps(i + 1).Y = 999 Then
                    If Ps(i).Y = 0 Then
                        If IsHitSpike(Ps(i).X, (NumH.Value * 80) - 240 + 10 - Ps(i).Y) Then
                            G.DrawImage(SetReverseColor(CharAct(GetWalkFrame(i, SpdX(i)))), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - Ps(i).Y, CHW, CHH)
                        Else
                            G.DrawImage(CharAct(GetWalkFrame(i, SpdX(i))), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - Ps(i).Y, CHW, CHH)
                        End If

                    Else
                        If IsHitSpike(Ps(i).X, (NumH.Value * 80) - 240 + 10 - Ps(i).Y) Then
                            G.DrawImage(SetReverseColor(CharAct(4)), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - Ps(i).Y, CHW, CHH)
                        Else
                            G.DrawImage(CharAct(4), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - Ps(i).Y, CHW, CHH)
                        End If
                    End If
                    If CkHitbox.Checked Then
                        G.DrawRectangle(Pens.White, Ps(i).X, 5 + (NumH.Value * 80) - Ps(i).Y + 20, 79, 59)
                    End If
                    z = 0
                    ZL += 1
                    If ZL <= UBound(SSS) Then
                        If CkMove.Checked Then
                            TxtW = GetStrW(If(CkFrame.Checked, SSS(ZL), GetCmd(SSS(ZL)))) \ 2
                            For ii = -1 To 1
                                For jj = -1 To 1
                                    G.DrawString(If(CkFrame.Checked, SSS(ZL), GetCmd(SSS(ZL))), Label1.Font, Brushes.White, Ps(i).X - TxtW + 40 + ii, 5 + (NumH.Value * 80) - Ps(i).Y - 20 + jj)
                                Next
                            Next
                            G.DrawString(If(CkFrame.Checked, SSS(ZL), GetCmd(SSS(ZL))), Label1.Font, Brushes.Black, Ps(i).X - TxtW + 40, 5 + (NumH.Value * 80) - Ps(i).Y - 20)
                        End If
                    End If
                Else
                    z = (z + 1) Mod FRM
                    If z = FRM - 1 Then
                        If Ps(i).Y = 0 Then
                            If IsHitSpike(Ps(i).X, (NumH.Value * 80) - 240 + 10 - Ps(i).Y) Then
                                G.DrawImage(SetOpacity(SetReverseColor(CharAct(GetWalkFrame(i, SpdX(i)))), 0.5), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - Ps(i).Y, CHW, CHH)
                            Else
                                G.DrawImage(SetOpacity(CharAct(GetWalkFrame(i, SpdX(i))), 0.5), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - Ps(i).Y, CHW, CHH)
                            End If
                        Else
                            If IsHitSpike(Ps(i).X, (NumH.Value * 80) - 240 + 10 - Ps(i).Y) Then
                                G.DrawImage(SetOpacity(SetReverseColor(CharAct(4)), 0.5), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - Ps(i).Y, CHW, CHH)
                            Else
                                G.DrawImage(SetOpacity(CharAct(4), 0.5), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - Ps(i).Y, CHW, CHH)
                            End If
                        End If
                        If CkHitbox.Checked Then
                            G.DrawRectangle(Pens.White, Ps(i).X, 5 + (NumH.Value * 80) - Ps(i).Y + 20, 79, 59)
                        End If
                    End If
                End If

            Next
        Else
            For i = 0 To UBound(S) - 1 ' Step -1
                If Ps(i + 1).Y = 999 Then
                    If Ps(i).Y = 0 Then
                        If IsHitSpike(Ps(i).X, (NumH.Value * 80) - 240 + 20 - 10 - Ps(i).Y) Then
                            G.DrawImage(SetReverseColor(CharAct(GetWalkFrame(i, SpdX(i)))), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - Ps(i).Y, CHW, CHH)
                        Else
                            G.DrawImage(CharAct(GetWalkFrame(i, SpdX(i))), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - Ps(i).Y, CHW, CHH)
                        End If

                    Else
                        If IsHitSpike(Ps(i).X, (NumH.Value * 80) - 240 + 20 - 10 - Ps(i).Y) Then
                            G.DrawImage(SetReverseColor(CharAct(4)), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - Ps(i).Y, CHW, CHH)
                        Else
                            G.DrawImage(CharAct(4), Ps(i).X + OFX, OFY + 5 + (NumH.Value * 80) - Ps(i).Y, CHW, CHH)
                        End If
                    End If
                    z = 0
                    ZL += 1
                    If ZL <= UBound(SSS) Then
                        If CkMove.Checked Then
                            TxtW = GetStrW(If(CkFrame.Checked, SSS(ZL), GetCmd(SSS(ZL)))) \ 2
                            For ii = -1 To 1
                                For jj = -1 To 1
                                    G.DrawString(If(CkFrame.Checked, SSS(ZL), GetCmd(SSS(ZL))), Label1.Font, Brushes.White, Ps(i).X - TxtW + 40 + ii, 5 + (NumH.Value * 80) - Ps(i).Y - 20 + jj)
                                Next
                            Next
                            G.DrawString(If(CkFrame.Checked, SSS(ZL), GetCmd(SSS(ZL))), Label1.Font, Brushes.Black, Ps(i).X - TxtW + 40, 5 + (NumH.Value * 80) - Ps(i).Y - 20)
                        End If
                    End If
                End If
            Next
        End If

        'PicTip.Image = B

        B.Save("投屏.png", ImageFormat.Png)
        Return 0
    End Function
    Private Function Cmd2Button(s As String) As String
        Select Case Replace(GetCmd(s), "低重", "")
            Case "加速右跳", "加速右缓冲跳"    '加速右跳
                Return "RYB"
            Case "加速左跳", "加速左缓冲跳"    '加速左跳
                Return "LYB"
            Case "加速右"    '加速右跳
                Return "RY"
            Case "加速左"    '加速左跳
                Return "LY"
            Case "右跳", "右缓冲跳"  '右跳
                Return "RB"
            Case "左跳", "左缓冲跳"    '左跳
                Return "LB"
            Case "右"    '右跳
                Return "R"
            Case "左"    '左跳
                Return "L"
            Case "滞空"    '匀速滞空跳
                Return "B"
            Case "落体"    '匀速落体跳
                Return ""
            Case "跳", "缓冲跳" '原地跳
                Return "B"
            Case "右跑"    '地面右跑
                Return "RY"
            Case "左跑"    '地面左跑
                Return "LY"
            Case "右走"    '地面右走
                Return "R"
            Case "左走"    '地面左走
                Return "L"
            Case "冰右跑"    '冰面右跑
                Return "RY"
            Case "冰右走"    '冰面右跑
                Return "R"
            Case "冰左跑"    '冰面右跑
                Return "LY"
            Case "冰左走"    '冰面右跑
                Return "L"
            Case Else
                Return ""
        End Select
    End Function
    Private Function GetButtonImg(s As String) As Bitmap
        Dim B As New Bitmap(3 * 64, 64)
        Dim g As Graphics = Graphics.FromImage(B)
        g.FillRectangle(Brushes.Black, 0, 0, 3 * 64, 64)
        For i As Integer = 1 To s.Length
            g.DrawImage(Image.FromFile(Application.StartupPath & "\img\a" & Mid(s, i, 1) & ".png"), i * 64 - 64, 0)
        Next
        Return B
    End Function
    Private Function GetStrW(s As String) As Integer
        Dim B As New Bitmap(300, 100)
        Dim G As Graphics = Graphics.FromImage(B), SZ As SizeF
        SZ = G.MeasureString(s, Label1.Font)
        Return SZ.Width
    End Function
    Sub SearchMoveSpike()
        Dim X, X2, Y, Y2, SPD, SPD2, RX, RY, RS As Single
        Dim SPY As Single, IsJump As Boolean
        T2.Text = T2.Text.Replace(vbTab, " ")

        X2 = Val(TX2.Text)
        Y2 = Val(TY2.Text)
        SPD2 = Val(TS2.Text)

        RX = Val(TX3.Text)
        RY = Val(TY3.Text)
        RS = Val(TS3.Text)

        Label1.Text = DateTime.Now & " 开始搜索操作"
        FileOpen(1, Application.StartupPath & "\" & T2.Text & ".TXT", OpenMode.Output)

        Dim I, J, K As Long
        Dim S() As String, RESULT, RE As String
        Dim C(), UC(), LC() As Integer
        S = T2.Text.Trim.Split(" ")
        ReDim C(S.GetUpperBound(0)), UC(S.GetUpperBound(0)), LC(S.GetUpperBound(0))
        Dim PR, PN As Integer
        PR = 1
        PN = 0
        For I = 0 To S.GetUpperBound(0)
            GetNum(S(I), LC(I), UC(I))
            C(I) = LC(I)
            If UC(I) > 0 Then
                PR *= UC(I) - LC(I) + 1
            End If
        Next
        MaterialProgressBar1.Maximum = PR
        T1.Text = ""
        J = 0
        Dim CSpike As Boolean, TFrame As Integer
        Dim R2(), R3() As String
        Do
            Application.DoEvents()
            X = Val(TX1.Text)
            SPD = Val(TS1.Text)
            DSpd = Val(TD.Text)
            Y = Val(TY1.Text)
            SPY = 0
            IsJump = False
            MaterialProgressBar1.Value = PN
            'Me.Text = PN.ToString
            WSpd = 0
            WFrame = 0
            CSpike = False
            RE = ""
            TFrame = 0
            For I = 0 To S.GetUpperBound(0)
                TFrame += C(I)
                Select Case GetCmd(S(I))
                    Case "左风站立"
                        RE = WAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "风右跑跳"    '加速右跳
                        RE = WJump(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "风左跑跳"    '加速左跳
                        RE = WJump(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "风右跳"    '右跳
                        RE = WJump(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "风左跳"    '左跳
                        RE = WJump(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "加速右跳"    '加速右跳
                        RE = MJump(X, SPD, True, True, C(I), Y, SPY, IsJump, False)
                    Case "加速左跳"    '加速左跳
                        RE = MJump(X, SPD, False, True, C(I), Y, SPY, IsJump, False)
                    Case "加速右"    '加速右跳
                        RE = MAJump(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "加速左"    '加速左跳
                        RE = MAJump(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "右跳"    '右跳
                        RE = MJump(X, SPD, True, False, C(I), Y, SPY, IsJump, False)
                    Case "左跳"    '左跳
                        RE = MJump(X, SPD, False, False, C(I), Y, SPY, IsJump, False)
                    Case "右"    '右跳
                        RE = MAJump(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "左"    '左跳
                        RE = MAJump(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "左反墙"
                        RE = MWallJump(X, SPD, True, False, C(I), Y, SPY, IsJump, False)
                    Case "右反墙"
                        RE = MWallJump(X, SPD, False, False, C(I), Y, SPY, IsJump, False)
                    Case "加速右旋转"    '加速右跳
                        RE = SpinJump(X, SPD, True, True, 0, Y, SPY, IsJump)
                    Case "加速左旋转"    '加速左跳
                        RE = SpinJump(X, SPD, False, True, 0, Y, SPY, IsJump)
                    Case "右旋转"    '右跳
                        RE = SpinJump(X, SPD, True, False, 0, Y, SPY, IsJump)
                    Case "左旋转"    '左跳
                        RE = SpinJump(X, SPD, False, False, 0, Y, SPY, IsJump)
                    Case "旋转"    '匀速滞空跳
                        RE = SpinAir(X, SPD, 0, Y, SPY, IsJump)

                    Case "加速右旋转跳"    '加速右跳
                        RE = MSpinJump(X, SPD, True, True, C(I), Y, SPY, IsJump, False)
                    Case "加速左旋转跳"    '加速左跳
                        RE = MSpinJump(X, SPD, False, True, C(I), Y, SPY, IsJump, False)
                    Case "右旋转跳"    '右跳
                        RE = MSpinJump(X, SPD, True, False, C(I), Y, SPY, IsJump, False)
                    Case "左旋转跳"    '左跳
                        RE = MSpinJump(X, SPD, False, False, C(I), Y, SPY, IsJump, False)

                    Case "滞空"    '匀速滞空跳
                        RE = MAAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "落体"    '匀速落体跳
                        RE = MAir(X, SPD, C(I), Y, SPY, IsJump, 0)
                    Case "落体站"    '匀速落体跳
                        RE = MAir(X, SPD, C(I), Y, SPY, IsJump, 0)
                    Case "落体正"    '匀速落体跳
                        RE = MAir(X, SPD, C(I), Y, SPY, IsJump, 1)
                    Case "落体反"    '匀速落体跳
                        RE = MAir(X, SPD, C(I), Y, SPY, IsJump, -1)

                    Case "跳" '原地跳
                        RE = Jump(X, SPD, C(I), Y, SPY, IsJump)
                    Case "缓冲跳"
                        RE = BJump(X, SPD, C(I), Y, SPY, IsJump)
                    Case "弹簧跳" '弹簧跳
                        RE = TJump(X, SPD, Y, SPY, IsJump)
                    Case "低重滞空"    '低重力匀速跳
                        RE = LAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "低重落体" '低重力落体
                        RE = LAAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "低重跳"    '低重力匀速跳
                        RE = LAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "低重缓冲跳"    '低重力匀速跳
                        RE = LBAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "风地右跑"    '风地面右跑
                        RE = WRun(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "风地左跑"    '风地面左跑
                        RE = WRun(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "风地右走"    '风地面右走
                        RE = WRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "风地左走"    '风地面左走
                        RE = WRun(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "右跑"    '地面右跑
                        RE = MRun(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "左跑"    '地面左跑
                        RE = MRun(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "右走"    '地面右走
                        RE = MRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "左走"    '地面左走
                        RE = MRun(X, SPD, False, False, C(I), Y, SPY, IsJump)

                    Case "右陡坡右跑"    '地面右跑
                        RE = MDSlopeRun(X, SPD, True, True, C(I), Y, SPY, True)
                    Case "右陡坡左跑"    '地面左跑
                        RE = MDSlopeRun(X, SPD, False, True, C(I), Y, SPY, True)
                    Case "右陡坡右走"    '地面右走
                        RE = MDSlopeRun(X, SPD, True, False, C(I), Y, SPY, True)
                    Case "右陡坡左走"    '地面左走
                        RE = MDSlopeRun(X, SPD, False, False, C(I), Y, SPY, True)

                    Case "左陡坡右跑"    '地面右跑
                        RE = MDSlopeRun(X, SPD, True, True, C(I), Y, SPY, False)
                    Case "左陡坡左跑"    '地面左跑
                        RE = MDSlopeRun(X, SPD, False, True, C(I), Y, SPY, False)
                    Case "左陡坡右走"    '地面右走
                        RE = MDSlopeRun(X, SPD, True, False, C(I), Y, SPY, False)
                    Case "左陡坡左走"    '地面左走
                        RE = MDSlopeRun(X, SPD, False, False, C(I), Y, SPY, False)

                    Case "右缓坡右跑"    '地面右跑
                        RE = MHSlopeRun(X, SPD, True, True, C(I), Y, SPY, True)
                    Case "右缓坡左跑"    '地面左跑
                        RE = MHSlopeRun(X, SPD, False, True, C(I), Y, SPY, True)
                    Case "右缓坡右走"    '地面右走
                        RE = MHSlopeRun(X, SPD, True, False, C(I), Y, SPY, True)
                    Case "右缓坡左走"    '地面左走
                        RE = MHSlopeRun(X, SPD, False, False, C(I), Y, SPY, True)

                    Case "左缓坡右跑"    '地面右跑
                        RE = MHSlopeRun(X, SPD, True, True, C(I), Y, SPY, False)
                    Case "左缓坡左跑"    '地面左跑
                        RE = MHSlopeRun(X, SPD, False, True, C(I), Y, SPY, False)
                    Case "左缓坡右走"    '地面右走
                        RE = MHSlopeRun(X, SPD, True, False, C(I), Y, SPY, False)
                    Case "左缓坡左走"    '地面左走
                        RE = MHSlopeRun(X, SPD, False, False, C(I), Y, SPY, False)

                    Case "岩浆右走"    '地面右走
                        RE = MFireRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "岩浆左走"    '地面左走
                        RE = MFireRun(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "岩浆站立"    '地面站立
                        RE = MFireDuck(X, SPD, C(I), Y, SPY)
                    Case "岩浆站停"    '地面站立
                        RE = MFireDuck(X, SPD, 999, Y, SPY)
                    '鞋走路 USA大跳

                    Case "站立"    '地面站立
                        RE = MDuck(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "正蹲"    '地面正蹲
                        RE = MDuck(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "反蹲"    '地面反蹲
                        RE = MDuck(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "站停"    '地面站立
                        RE = MDuck(X, SPD, True, True, 999, Y, SPY, IsJump)
                    Case "正停"    '地面正蹲
                        RE = MDuck(X, SPD, False, True, 999, Y, SPY, IsJump)
                    Case "反停"    '地面反蹲
                        RE = MDuck(X, SPD, False, False, 999, Y, SPY, IsJump)
                    Case "冰站立"    '地面站立
                        RE = IDuck(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "冰正蹲"    '地面正蹲
                        RE = IDuck(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "冰反蹲"    '地面反蹲
                        RE = IDuck(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "低重右跳"    '低重力右跳
                        RE = LJump(X, SPD, True, False, C(I), Y, SPY, IsJump, False)
                    Case "低重左跳"    '低重力左跳
                        RE = LJump(X, SPD, False, False, C(I), Y, SPY, IsJump, False)
                    Case "低重右"    '低重力右跳
                        RE = LAJump(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "低重左"    '低重力左跳
                        RE = LAJump(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "低重加速右跳"    '低重力右跳
                        RE = LJump(X, SPD, True, True, C(I), Y, SPY, IsJump, False)
                    Case "低重加速左跳"    '低重力左跳
                        RE = LJump(X, SPD, False, True, C(I), Y, SPY, IsJump, False)
                    Case "低重加速右"    '低重力右跳
                        RE = LAJump(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "低重加速左"    '低重力左跳
                        RE = LAJump(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "坐莲" '坐莲
                        RE = ZAAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "滞空坐莲" '坐莲
                        RE = ZAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "低重坐莲" '坐莲
                        RE = LZAAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "低重滞空坐莲" '坐莲
                        RE = LZAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "冰右跑"    '冰面右跑
                        RE = IRun(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "冰右走"    '冰面右跑
                        RE = IRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "冰左跑"    '冰面右跑
                        RE = IRun(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "冰左走"    '冰面右跑
                        RE = IRun(X, SPD, False, False, C(I), Y, SPY, IsJump)

                    Case "加速右缓冲跳"    '加速右跳
                        RE = MJump(X, SPD, True, True, C(I), Y, SPY, IsJump, True)
                    Case "加速左缓冲跳"    '加速左跳
                        RE = MJump(X, SPD, False, True, C(I), Y, SPY, IsJump, True)
                    Case "右缓冲跳"    '右跳
                        RE = MJump(X, SPD, True, False, C(I), Y, SPY, IsJump, True)
                    Case "左缓冲跳"    '左跳
                        RE = MJump(X, SPD, False, False, C(I), Y, SPY, IsJump, True)
                    Case "低重加速右缓冲跳"    '低重力右跳
                        RE = LJump(X, SPD, True, True, C(I), Y, SPY, IsJump, True)
                    Case "低重加速左缓冲跳"    '低重力左跳
                        RE = LJump(X, SPD, False, True, C(I), Y, SPY, IsJump, True)
                    Case "低重右缓冲跳"    '低重力右跳
                        RE = LJump(X, SPD, True, False, C(I), Y, SPY, IsJump, True)
                    Case "低重左缓冲跳"    '低重力左跳
                        RE = LJump(X, SPD, False, False, C(I), Y, SPY, IsJump, True)

                    Case "水右跳"    '右跳
                        RE = WtJump(X, SPD, True, True, C(I), Y, SPY, True, IsJump)
                    Case "水左跳"    '左跳
                        RE = WtJump(X, SPD, False, True, C(I), Y, SPY, True, IsJump)
                    Case "水右"    '右
                        RE = WtJump(X, SPD, True, True, C(I), Y, SPY, False, IsJump)
                    Case "水左"    '左
                        RE = WtJump(X, SPD, False, True, C(I), Y, SPY, False, IsJump)
                    Case "水右走"    '地面右走
                        RE = WtRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "水左走"    '地面左走
                        RE = WtRun(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "水跳"
                        RE = WtAir(X, SPD, False, True, C(I), Y, SPY, True, IsJump)
                    Case "水落"
                        RE = WtAir(X, SPD, False, True, C(I), Y, SPY, False, IsJump)
                    Case "水站"    '地面站立
                        RE = WtDuck(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "水冰右走"    '地面右走
                        RE = WtIceRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "水冰左走"    '地面左走
                        RE = WtIceRun(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "水冰站"    '地面站立
                        RE = WtIceDuck(X, SPD, True, True, C(I), Y, SPY, IsJump)

                    Case "无敌加速右跳"    '加速右跳
                        RE = MSJump(X, SPD, True, True, C(I), Y, SPY, IsJump, False)
                    Case "无敌加速左跳"    '加速左跳
                        RE = MSJump(X, SPD, False, True, C(I), Y, SPY, IsJump, False)
                    Case "无敌加速右"    '加速右跳
                        RE = MSAJump(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "无敌加速左"    '加速左跳
                        RE = MSAJump(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "无敌右跳"    '右跳
                        RE = MSJump(X, SPD, True, False, C(I), Y, SPY, IsJump, False)
                    Case "无敌左跳"    '左跳
                        RE = MSJump(X, SPD, False, False, C(I), Y, SPY, IsJump, False)
                    Case "无敌右"    '右跳
                        RE = MSAJump(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "无敌左"    '左跳
                        RE = MSAJump(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "无敌右跑"    '地面右跑
                        RE = MSRun(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "无敌左跑"    '地面左跑
                        RE = MSRun(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "无敌右走"    '地面右走
                        RE = MSRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "无敌左走"    '地面左走
                        RE = MSRun(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "无敌站立"    '地面站立
                        RE = MSDuck(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "无敌正蹲"    '地面正蹲
                        RE = MSDuck(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "无敌反蹲"    '地面反蹲
                        RE = MSDuck(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "无敌站停"    '地面站立
                        RE = MSDuck(X, SPD, True, True, 999, Y, SPY, IsJump)
                    Case "无敌正停"    '地面正蹲
                        RE = MSDuck(X, SPD, False, True, 999, Y, SPY, IsJump)
                    Case "无敌反停"    '地面反蹲
                        RE = MSDuck(X, SPD, False, False, 999, Y, SPY, IsJump)
                End Select
                '检查碰撞
                R2 = RE.Replace(vbCrLf, "|").Split("|")
                For K = 0 To R2.Length - 1
                    R3 = R2(K).Split(vbTab)
                    If R3.Length > 1 Then
                        If IsHitSpike(Val(R3(0)), Val(R3(1))) Then
                            CSpike = True
                            Exit For
                        End If
                    End If
                Next
                If CSpike Then
                    Exit For
                End If
            Next

            If Not CSpike Then
                RESULT = ""
                For I = 0 To S.GetUpperBound(0)
                    RESULT += GetCmd(S(I)) & C(I).ToString & vbTab
                Next
                RESULT += "'" & TFrame.ToString & vbTab & Sng2Hex(X) & vbTab & "'" & Sng2Hex(SPD) & vbTab & CDec(X).ToString & vbTab &
                    CDec(SPD).ToString & vbTab & CDec(Y).ToString & vbTab & CDec(SPY).ToString & vbCrLf
                Print(1, RESULT)
                J += 1
            End If

            C(0) = C(0) + 1
            For I = 0 To S.GetUpperBound(0) - 1
                If C(I) > UC(I) Then
                    C(I) = LC(I)
                    C(I + 1) += 1
                End If
            Next
            PN += 1 '= C(UC.GetUpperBound(0))
        Loop Until C(UC.GetUpperBound(0)) > UC(UC.GetUpperBound(0))
        MaterialProgressBar1.Value = MaterialProgressBar1.Maximum
        Label1.Text = DateTime.Now & " 找到解法" & J.ToString & "个"
        FileClose(1)
    End Sub
    Sub SearchMove()
        Dim X, X2, Y, Y2, SPD, SPD2, RX, RY, RS As Single
        Dim SPY As Single, IsJump As Boolean
        T2.Text = T2.Text.Replace(vbTab, " ")

        X2 = Val(TX2.Text)
        Y2 = Val(TY2.Text)
        SPD2 = Val(TS2.Text)

        RX = Val(TX3.Text)
        RY = Val(TY3.Text)
        RS = Val(TS3.Text)

        Label1.Text = DateTime.Now & " 开始搜索操作"
        FileOpen(1, Application.StartupPath & "\" & T2.Text & ".TXT", OpenMode.Output)

        Dim I, J As Long
        Dim S() As String, RESULT As String
        Dim C(), UC(), LC() As Integer
        S = T2.Text.Trim.Split(" ")
        ReDim C(S.GetUpperBound(0)), UC(S.GetUpperBound(0)), LC(S.GetUpperBound(0))
        Dim PR, PN As Integer
        PR = 1
        PN = 0
        For I = 0 To S.GetUpperBound(0)
            GetNum(S(I), LC(I), UC(I))
            C(I) = LC(I)
            If UC(I) > 0 Then
                PR *= UC(I) - LC(I) + 1
            End If
        Next
        MaterialProgressBar1.Maximum = PR
        T1.Text = ""
        J = 0
        Do
            Application.DoEvents()
            X = Val(TX1.Text)
            SPD = Val(TS1.Text)
            DSpd = Val(TD.Text)
            Y = Val(TY1.Text)
            SPY = 0
            IsJump = False
            MaterialProgressBar1.Value = PN
            'Me.Text = PN.ToString
            WSpd = 0
            WFrame = 0
            For I = 0 To S.GetUpperBound(0)
                Select Case GetCmd(S(I)) 'Strings.Left(S(I), 4)
                    Case "左风站立"
                        WAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "风右跑跳"    '加速右跳
                        WJump(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "风左跑跳"    '加速左跳
                        WJump(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "风右跳"    '右跳
                        WJump(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "风左跳"    '左跳
                        WJump(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "加速右跳"    '加速右跳
                        MJump(X, SPD, True, True, C(I), Y, SPY, IsJump, False)
                    Case "加速左跳"    '加速左跳
                        MJump(X, SPD, False, True, C(I), Y, SPY, IsJump, False)
                    Case "加速右"    '加速右跳
                        MAJump(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "加速左"    '加速左跳
                        MAJump(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "右跳"    '右跳
                        MJump(X, SPD, True, False, C(I), Y, SPY, IsJump, False)
                    Case "左跳"    '左跳
                        MJump(X, SPD, False, False, C(I), Y, SPY, IsJump, False)
                    Case "右"    '右跳
                        MAJump(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "左"    '左跳
                        MAJump(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "左反墙"
                        MWallJump(X, SPD, True, False, C(I), Y, SPY, IsJump, False)
                    Case "右反墙"
                        MWallJump(X, SPD, False, False, C(I), Y, SPY, IsJump, False)
                    Case "加速右旋转"    '加速右跳
                        SpinJump(X, SPD, True, True, 0, Y, SPY, IsJump)
                    Case "加速左旋转"    '加速左跳
                        SpinJump(X, SPD, False, True, 0, Y, SPY, IsJump)
                    Case "右旋转"    '右跳
                        SpinJump(X, SPD, True, False, 0, Y, SPY, IsJump)
                    Case "左旋转"    '左跳
                        SpinJump(X, SPD, False, False, 0, Y, SPY, IsJump)
                    Case "旋转"    '匀速滞空跳
                        SpinAir(X, SPD, 0, Y, SPY, IsJump)

                    Case "加速右旋转跳"    '加速右跳
                        MSpinJump(X, SPD, True, True, C(I), Y, SPY, IsJump, False)
                    Case "加速左旋转跳"    '加速左跳
                        MSpinJump(X, SPD, False, True, C(I), Y, SPY, IsJump, False)
                    Case "右旋转跳"    '右跳
                        MSpinJump(X, SPD, True, False, C(I), Y, SPY, IsJump, False)
                    Case "左旋转跳"    '左跳
                        MSpinJump(X, SPD, False, False, C(I), Y, SPY, IsJump, False)

                    Case "滞空"    '匀速滞空跳
                        MAAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "落体"    '匀速落体跳
                        MAir(X, SPD, C(I), Y, SPY, IsJump, 0)
                    Case "落体站"    '匀速落体跳
                        MAir(X, SPD, C(I), Y, SPY, IsJump, 0)
                    Case "落体正"    '匀速落体跳
                        MAir(X, SPD, C(I), Y, SPY, IsJump, 1)
                    Case "落体反"    '匀速落体跳
                        MAir(X, SPD, C(I), Y, SPY, IsJump, -1)

                    Case "跳" '原地跳
                        Jump(X, SPD, C(I), Y, SPY, IsJump)
                    Case "缓冲跳"
                        BJump(X, SPD, C(I), Y, SPY, IsJump)
                    Case "弹簧跳" '弹簧跳
                        TJump(X, SPD, Y, SPY, IsJump)
                    Case "低重滞空"    '低重力匀速跳
                        LAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "低重落体" '低重力落体
                        LAAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "低重跳"    '低重力匀速跳
                        LAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "低重缓冲跳"    '低重力匀速跳
                        LBAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "风地右跑"    '风地面右跑
                        WRun(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "风地左跑"    '风地面左跑
                        WRun(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "风地右走"    '风地面右走
                        WRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "风地左走"    '风地面左走
                        WRun(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "右跑"    '地面右跑
                        MRun(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "左跑"    '地面左跑
                        MRun(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "右走"    '地面右走
                        MRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "左走"    '地面左走
                        MRun(X, SPD, False, False, C(I), Y, SPY, IsJump)

                    Case "右陡坡右跑"    '地面右跑
                        MDSlopeRun(X, SPD, True, True, C(I), Y, SPY, True)
                    Case "右陡坡左跑"    '地面左跑
                        MDSlopeRun(X, SPD, False, True, C(I), Y, SPY, True)
                    Case "右陡坡右走"    '地面右走
                        MDSlopeRun(X, SPD, True, False, C(I), Y, SPY, True)
                    Case "右陡坡左走"    '地面左走
                        MDSlopeRun(X, SPD, False, False, C(I), Y, SPY, True)

                    Case "左陡坡右跑"    '地面右跑
                        MDSlopeRun(X, SPD, True, True, C(I), Y, SPY, False)
                    Case "左陡坡左跑"    '地面左跑
                        MDSlopeRun(X, SPD, False, True, C(I), Y, SPY, False)
                    Case "左陡坡右走"    '地面右走
                        MDSlopeRun(X, SPD, True, False, C(I), Y, SPY, False)
                    Case "左陡坡左走"    '地面左走
                        MDSlopeRun(X, SPD, False, False, C(I), Y, SPY, False)

                    Case "右缓坡右跑"    '地面右跑
                        MHSlopeRun(X, SPD, True, True, C(I), Y, SPY, True)
                    Case "右缓坡左跑"    '地面左跑
                        MHSlopeRun(X, SPD, False, True, C(I), Y, SPY, True)
                    Case "右缓坡右走"    '地面右走
                        MHSlopeRun(X, SPD, True, False, C(I), Y, SPY, True)
                    Case "右缓坡左走"    '地面左走
                        MHSlopeRun(X, SPD, False, False, C(I), Y, SPY, True)

                    Case "左缓坡右跑"    '地面右跑
                        MHSlopeRun(X, SPD, True, True, C(I), Y, SPY, False)
                    Case "左缓坡左跑"    '地面左跑
                        MHSlopeRun(X, SPD, False, True, C(I), Y, SPY, False)
                    Case "左缓坡右走"    '地面右走
                        MHSlopeRun(X, SPD, True, False, C(I), Y, SPY, False)
                    Case "左缓坡左走"    '地面左走
                        MHSlopeRun(X, SPD, False, False, C(I), Y, SPY, False)

                    Case "岩浆右走"    '地面右走
                        MFireRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "岩浆左走"    '地面左走
                        MFireRun(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "岩浆站立"    '地面站立
                        MFireDuck(X, SPD, C(I), Y, SPY)
                    Case "岩浆站停"    '地面站立
                        MFireDuck(X, SPD, 999, Y, SPY)
                    '鞋走路 USA大跳

                    Case "站立"    '地面站立
                        MDuck(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "正蹲"    '地面正蹲
                        MDuck(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "反蹲"    '地面反蹲
                        MDuck(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "站停"    '地面站立
                        MDuck(X, SPD, True, True, 999, Y, SPY, IsJump)
                    Case "正停"    '地面正蹲
                        MDuck(X, SPD, False, True, 999, Y, SPY, IsJump)
                    Case "反停"    '地面反蹲
                        MDuck(X, SPD, False, False, 999, Y, SPY, IsJump)
                    Case "冰站立"    '地面站立
                        IDuck(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "冰正蹲"    '地面正蹲
                        IDuck(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "冰反蹲"    '地面反蹲
                        IDuck(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "低重右跳"    '低重力右跳
                        LJump(X, SPD, True, False, C(I), Y, SPY, IsJump, False)
                    Case "低重左跳"    '低重力左跳
                        LJump(X, SPD, False, False, C(I), Y, SPY, IsJump, False)
                    Case "低重右"    '低重力右跳
                        LAJump(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "低重左"    '低重力左跳
                        LAJump(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "低重加速右跳"    '低重力右跳
                        LJump(X, SPD, True, True, C(I), Y, SPY, IsJump, False)
                    Case "低重加速左跳"    '低重力左跳
                        LJump(X, SPD, False, True, C(I), Y, SPY, IsJump, False)
                    Case "低重加速右"    '低重力右跳
                        LAJump(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "低重加速左"    '低重力左跳
                        LAJump(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "坐莲" '坐莲
                        ZAAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "滞空坐莲" '坐莲
                        ZAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "低重坐莲" '坐莲
                        LZAAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "低重滞空坐莲" '坐莲
                        LZAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "冰右跑"    '冰面右跑
                        IRun(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "冰右走"    '冰面右跑
                        IRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "冰左跑"    '冰面右跑
                        IRun(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "冰左走"    '冰面右跑
                        IRun(X, SPD, False, False, C(I), Y, SPY, IsJump)

                    Case "加速右缓冲跳"    '加速右跳
                        MJump(X, SPD, True, True, C(I), Y, SPY, IsJump, True)
                    Case "加速左缓冲跳"    '加速左跳
                        MJump(X, SPD, False, True, C(I), Y, SPY, IsJump, True)
                    Case "右缓冲跳"    '右跳
                        MJump(X, SPD, True, False, C(I), Y, SPY, IsJump, True)
                    Case "左缓冲跳"    '左跳
                        MJump(X, SPD, False, False, C(I), Y, SPY, IsJump, True)
                    Case "低重加速右缓冲跳"    '低重力右跳
                        LJump(X, SPD, True, True, C(I), Y, SPY, IsJump, True)
                    Case "低重加速左缓冲跳"    '低重力左跳
                        LJump(X, SPD, False, True, C(I), Y, SPY, IsJump, True)
                    Case "低重右缓冲跳"    '低重力右跳
                        LJump(X, SPD, True, False, C(I), Y, SPY, IsJump, True)
                    Case "低重左缓冲跳"    '低重力左跳
                        LJump(X, SPD, False, False, C(I), Y, SPY, IsJump, True)

                    Case "水右跳"    '右跳
                        WtJump(X, SPD, True, True, C(I), Y, SPY, True, IsJump)
                    Case "水左跳"    '左跳
                        WtJump(X, SPD, False, True, C(I), Y, SPY, True, IsJump)
                    Case "水右"    '右
                        WtJump(X, SPD, True, True, C(I), Y, SPY, False, IsJump)
                    Case "水左"    '左
                        WtJump(X, SPD, False, True, C(I), Y, SPY, False, IsJump)
                    Case "水右走"    '地面右走
                        WtRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "水左走"    '地面左走
                        WtRun(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "水跳"
                        WtAir(X, SPD, False, True, C(I), Y, SPY, True, IsJump)
                    Case "水落"
                        WtAir(X, SPD, False, True, C(I), Y, SPY, False, IsJump)
                    Case "水站"    '地面站立
                        WtDuck(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "水冰右走"    '地面右走
                        WtIceRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "水冰左走"    '地面左走
                        WtIceRun(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "水冰站"    '地面站立
                        WtIceDuck(X, SPD, True, True, C(I), Y, SPY, IsJump)

                    Case "无敌加速右跳"    '加速右跳
                        MSJump(X, SPD, True, True, C(I), Y, SPY, IsJump, False)
                    Case "无敌加速左跳"    '加速左跳
                        MSJump(X, SPD, False, True, C(I), Y, SPY, IsJump, False)
                    Case "无敌加速右"    '加速右跳
                        MSAJump(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "无敌加速左"    '加速左跳
                        MSAJump(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "无敌右跳"    '右跳
                        MSJump(X, SPD, True, False, C(I), Y, SPY, IsJump, False)
                    Case "无敌左跳"    '左跳
                        MSJump(X, SPD, False, False, C(I), Y, SPY, IsJump, False)
                    Case "无敌右"    '右跳
                        MSAJump(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "无敌左"    '左跳
                        MSAJump(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "无敌右跑"    '地面右跑
                        MSRun(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "无敌左跑"    '地面左跑
                        MSRun(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "无敌右走"    '地面右走
                        MSRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "无敌左走"    '地面左走
                        MSRun(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "无敌站立"    '地面站立
                        MSDuck(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "无敌正蹲"    '地面正蹲
                        MSDuck(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "无敌反蹲"    '地面反蹲
                        MSDuck(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "无敌站停"    '地面站立
                        MSDuck(X, SPD, True, True, 999, Y, SPY, IsJump)
                    Case "无敌正停"    '地面正蹲
                        MSDuck(X, SPD, False, True, 999, Y, SPY, IsJump)
                    Case "无敌反停"    '地面反蹲
                        MSDuck(X, SPD, False, False, 999, Y, SPY, IsJump)

                End Select
            Next

            If Not TS4.Checked OrElse Math.Abs(SPD - SPD2) <= RS Then
                If Not TX4.Checked OrElse Math.Abs(X - X2) <= RX Then
                    If Not TY4.Checked OrElse Math.Abs(Y - Y2) <= RY Then
                        RESULT = ""
                        For I = 0 To S.GetUpperBound(0)
                            RESULT += GetCmd(S(I)) & C(I).ToString & vbTab
                        Next
                        RESULT += "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(SPD) & vbTab & CDec(X).ToString & vbTab &
                            CDec(SPD).ToString & vbTab & CDec(Y).ToString & vbTab & CDec(SPY).ToString & vbCrLf
                        Print(1, RESULT)
                        J += 1
                    End If
                End If
            End If
            C(0) = C(0) + 1
            For I = 0 To S.GetUpperBound(0) - 1
                If C(I) > UC(I) Then
                    C(I) = LC(I)
                    C(I + 1) += 1
                End If
            Next
            PN += 1 '= C(UC.GetUpperBound(0))
        Loop Until C(UC.GetUpperBound(0)) > UC(UC.GetUpperBound(0))
        MaterialProgressBar1.Value = MaterialProgressBar1.Maximum
        Label1.Text = DateTime.Now & " 找到解法" & J.ToString & "个"
        FileClose(1)
    End Sub
    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        If TSP.Checked Then
            SearchMoveSpike()
            Exit Sub
        End If

        If TX4.Checked OrElse TY4.Checked OrElse TS4.Checked Then
            SearchMove()
            Exit Sub
        End If
    End Sub
    Dim Lbl(20) As Label
    Private Function GetCmd(a As String) As String
        Dim i As Integer, s As String = ""
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

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        If TX4.Checked OrElse TY4.Checked OrElse TS4.Checked Then
            T2.Text = T2.Text.Replace(vbTab, " ")
            T2.Text = T2.Text.Replace(vbCr, "")
            T2.Text = T2.Text.Replace(vbLf, "")
            TestJump(T2.Text, True)
            Draw2(False)
            If Form2.Visible Then
                Draw2form(False)
            End If
        End If
    End Sub

    Public Function SetOpacity(ByVal BB As Bitmap, ByVal D As Double) As Bitmap
        Try
            Dim bmpDATA As New Imaging.BitmapData
            Dim tmpBMP = New Bitmap(BB)
            Dim Rct As Rectangle = New Rectangle(0, 0, BB.Width, BB.Height)
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
            Dim Rct As Rectangle = New Rectangle(0, 0, BB.Width, BB.Height)
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
                G.DrawString(t, Label1.Font, Brushes.White, x + i, y + j)
            Next
        Next
        G.DrawString(t, Label1.Font, Brushes.Black, x, y)
        PB.Image = B
Err:
    End Sub
    Sub LoadStrat()
        If File.Exists(Application.StartupPath & "\data.txt") Then
            ComboBox2.Items.Clear()
            Dim r As StreamReader = New IO.StreamReader(Application.StartupPath & "\data.txt", System.Text.Encoding.Default)
            Do Until r.EndOfStream
                Dim s = r.ReadLine
                ComboBox2.Items.Add(s)
            Loop
            r.Close()
            If ComboBox2.Items.Count > 0 Then
                ComboBox2.SelectedIndex = 0
            End If
        End If
    End Sub

    Dim Form2 As New Form, PicTip As New PictureBox
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Dim I As Integer
        For I = 0 To 20
            Lbl(I) = New Label
            With Lbl(I)
                .AutoSize = True
                .BackColor = Color.White
                .ForeColor = Color.Black
            End With
            Me.Controls.Add(Lbl(I))
            Me.Controls.SetChildIndex(Lbl(I), 0)
        Next

        PB.Image = Image.FromFile(Application.StartupPath & "\img\bg2.png")
        PB.Width = Me.ClientSize.Width - 362
        PB.Height = Me.ClientSize.Height - 10

        ComboBox1.SelectedIndex = 0
        ComboBox3.SelectedIndex = 0
        ComboBox4.SelectedIndex = 1
        ComboBox5.SelectedIndex = 1
        ComboBox6.SelectedIndex = 0
        ComboBox7.SelectedIndex = 0
        MaterialComboBox1.SelectedIndex = 0
        Tile = Image.FromFile(Application.StartupPath & "\IMG\TILE\12621-0.png")

        ReDim CamBlk(1)
        CamBlk(0).X = 0
        CamBlk(0).Y = 0
        CamBlk(1).X = 15
        CamBlk(1).Y = 9

        With Form2
            .FormBorderStyle = FormBorderStyle.None
            .Width = 1920
            .Height = 1080
            .TopMost = True
            .BackColor = Color.FromArgb(255, 1, 2, 3)
            .TransparencyKey = Color.FromArgb(255, 1, 2, 3)
            .Opacity = 0.5
            .KeyPreview = True
        End With

        With PicTip
            .Width = 1920
            .Height = 1080
            .Left = 0
            .Top = 0
            .BackColor = Color.FromArgb(255, 1, 2, 3)
        End With
        Form2.Controls.Add(PicTip)

        AddHandler PicTip.MouseDoubleClick, AddressOf Form1_MouseDoubleClick
        AddHandler Form2.KeyPress, AddressOf Form1_KeyPress

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
        Dim B As Bitmap = New Bitmap(srcB.Size.Width * multiple, srcB.Size.Height * multiple)
        Dim srcData As BitmapData = srcB.LockBits(New Rectangle(New Point(0, 0), srcB.Size), ImageLockMode.ReadOnly, PixelFormat.Format32bppArgb)
        Dim BData As BitmapData = B.LockBits(New Rectangle(New Point(0, 0), B.Size), ImageLockMode.ReadWrite, PixelFormat.Format32bppArgb)
        Dim srcPtr As IntPtr = srcData.Scan0
        Dim BPtr As IntPtr = BData.Scan0
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

    Sub DrawBG(W As Integer, H As Integer)
        Dim i, j As Integer
        Dim B As New Bitmap(W * 16, H * 16)
        Dim G As Graphics = Graphics.FromImage(B)
        Tile = Image.FromFile(Application.StartupPath & "\IMG\TILE\" & ComboBox3.Text & "-" & ComboBox4.SelectedIndex.ToString & If(CheckBox7.Checked, "", "A") & ".PNG")
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
                For i = 0 To W - 1
                    G.DrawImage(GetTile(8, 12), i * 16, (H - 1) * 16)
                Next
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

        For i = 0 To W - 1
            G.DrawImage(GetTile(TX, TY), i * 16, (H - 2) * 16)
        Next

        If SpikeBlk IsNot Nothing Then
            '画刺
            For i = 0 To SpikeBlk.Length - 1
                Select Case BlkType(i)
                    Case 0 '刺
                        G.DrawImage(Image.FromFile(Application.StartupPath & "\img\T2.png"), SpikeBlk(i).X * 16, SpikeBlk(i).Y * 16, 16, 16)
                    Case 1 '绿花
                        G.DrawImage(Image.FromFile(Application.StartupPath & "\img\T3.png"), SpikeBlk(i).X * 16, SpikeBlk(i).Y * 16 - 8, 16, 24)
                    Case 2 '倒绿花
                        G.DrawImage(Image.FromFile(Application.StartupPath & "\img\T3.png"), SpikeBlk(i).X * 16, SpikeBlk(i).Y * 16 + 24, 16, -24)
                End Select
            Next
        End If
        PB.Image = Magnifier(B, 5)
    End Sub
    Private Function GetInsert(s As String, s0 As String, s1 As String) As String
        Dim f As String = ""
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
                f += GetInsert(t(i), If(t0 = "", "", t0 & " "), If(t1 = "", "", " " & t1))
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
                f += t0 & " i0 " & t1 & vbCrLf
            Next
        Else
            '单独指令
            If InStr(s, "跑") > 0 OrElse InStr(s, "走") > 0 OrElse InStr(s, "蹲") > 0 OrElse InStr(s, "停") > 0 OrElse InStr(s, "立") > 0 Then
                '地面指令不插入
                Return ""
            Else
                GetNum(s, TL, TU)
                'c = TL.ToString & "-" & TU.ToString
                ss = GetCmd(s)
                If TU < 4 Then
                    Return ""
                End If
                For i = 2 To TU - 2
                    f += s0 & ss & i.ToString & " " & "i0" & " " & ss & (TU - i).ToString & s1 & vbCrLf
                Next
            End If
        End If
        Return f
    End Function
    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        '找插入
        Label3.Text = DateTime.Now & " 开始搜索"
        T2.Text = T2.Text.Replace(vbTab, " ")
        Dim i As Integer
        Dim S(), Tf, Si() As String
        'Dim C(), D() As Integer
        S = T2.Text.Split(" ")
        'ReDim C(UBound(S)), D(UBound(S))
        'For i = 0 To UBound(S)
        '    GetNum(S(i), D(i), C(i))
        'Next

        Dim j As Integer
        Tf = GetInsert(T2.Text, "", "")
        Dim X, CX, CA As Single
        CX = Val(TR2.Text) '目标
        CA = Val(TR3.Text) '容错
        Si = Tf.Replace(vbCrLf, "|").Split("|")
        ListBox1.Items.Clear()
        LB1State = 0
        For j = 2 To Val(TR4.Text) '滞空帧
            For i = 0 To UBound(Si) - 1
                Tf = Si(i).Replace("i0", "滞空" & j.ToString)
                X = TestJump(Tf, False)
                If Math.Abs(X - CX) <= CA Then
                    ListBox1.Items.Add(X & " " & Tf)
                End If
            Next
        Next
        Label3.Text = DateTime.Now & " 搜索完成"
    End Sub
    Private Function TestJump(CmdS As String, ShowRst As Boolean) As Single
        Dim X, SPD, SPD2, R As Single

        WSpd = 0
        WFrame = 0
        SPD2 = Val(TS2.Text)
        R = Val(TS3.Text)

        Dim I As Long
        Dim S(), CmdT As String
        Dim C() As Integer, TP As Integer
        S = CmdS.Split(" ")
        ReDim C(S.GetUpperBound(0))
        For I = 0 To S.GetUpperBound(0)
            If GetCmd(S(I)).Length > 0 Then GetNum(S(I), TP, C(I))
        Next

        T1.Text = ""
        CResult = ""
        X = Val(TX1.Text)
        DSpd = Val(TD.Text)
        SPD = Val(TS1.Text)

        Dim Y, SPY, DY As Single, IsJump As Boolean
        Dim RE As String
        SPY = 0
        DY = Val(TY1.Text)
        Y = DY
        IsJump = False

        If CkLoc.Checked Then
            MoveData = New DataTable
            MoveData.Columns.Add("F")
            MoveData.Columns.Add("X")
            MoveData.Columns.Add("Y")
            MoveData.Columns.Add("Sx")
            MoveData.Columns.Add("Sy")
        End If

        For I = 0 To UBound(S)
            CmdT = GetCmd(S(I))
            If CmdT.Length > 0 Then
                GetNum(S(I), TP, TP)
                If ShowRst Then
                    T1.Text += GetCmd(S(I)) & "->" & TP.ToString
                    CResult += GetCmd(S(I)) & "->" & TP.ToString
                End If

                RE = ""
                Select Case GetCmd(S(I)) ' Strings.Left(S(I), 4)
                    Case "左反墙"
                        RE += MWallJump(X, SPD, True, False, C(I), Y, SPY, IsJump, False)
                    Case "右反墙"
                        RE += MWallJump(X, SPD, False, False, C(I), Y, SPY, IsJump, False)

                    Case "加速右跳"    '加速右跳
                        RE += MJump(X, SPD, True, True, C(I), Y, SPY, IsJump, False)
                    Case "加速左跳"    '加速左跳
                        RE += MJump(X, SPD, False, True, C(I), Y, SPY, IsJump, False)
                    Case "加速右"    '加速右跳
                        RE += MAJump(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "加速左"    '加速左跳
                        RE += MAJump(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "右跳"    '右跳
                        RE += MJump(X, SPD, True, False, C(I), Y, SPY, IsJump, False)
                    Case "左跳"    '左跳
                        RE += MJump(X, SPD, False, False, C(I), Y, SPY, IsJump, False)

                    Case "无敌加速右跳"    '加速右跳
                        RE += MSJump(X, SPD, True, True, C(I), Y, SPY, IsJump, False)
                    Case "无敌加速左跳"    '加速左跳
                        RE += MSJump(X, SPD, False, True, C(I), Y, SPY, IsJump, False)
                    Case "无敌加速右"    '加速右跳
                        RE += MSAJump(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "无敌加速左"    '加速左跳
                        RE += MSAJump(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "无敌右跳"    '右跳
                        RE += MSJump(X, SPD, True, False, C(I), Y, SPY, IsJump, False)
                    Case "无敌左跳"    '左跳
                        RE += MSJump(X, SPD, False, False, C(I), Y, SPY, IsJump, False)

                    Case "加速右旋转"    '加速右跳
                        RE += SpinJump(X, SPD, True, True, 0, Y, SPY, IsJump)
                    Case "加速左旋转"    '加速左跳
                        RE += SpinJump(X, SPD, False, True, 0, Y, SPY, IsJump)

                    Case "右旋转"    '右跳
                        RE += SpinJump(X, SPD, True, False, 0, Y, SPY, IsJump)
                    Case "左旋转"    '左跳
                        RE += SpinJump(X, SPD, False, False, 0, Y, SPY, IsJump)
                    Case "旋转"    '匀速滞空跳
                        RE += SpinAir(X, SPD, 0, Y, SPY, IsJump)
                    Case "加速右旋转跳"    '加速右跳
                        RE += MSpinJump(X, SPD, True, True, C(I), Y, SPY, IsJump, False)
                    Case "加速左旋转跳"    '加速左跳
                        RE += MSpinJump(X, SPD, False, True, C(I), Y, SPY, IsJump, False)
                    Case "右旋转跳"    '右跳
                        RE += MSpinJump(X, SPD, True, False, C(I), Y, SPY, IsJump, False)
                    Case "左旋转跳"    '左跳
                        RE += MSpinJump(X, SPD, False, False, C(I), Y, SPY, IsJump, False)

                    Case "右"    '右跳
                        RE += MAJump(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "左"    '左跳
                        RE += MAJump(X, SPD, False, False, C(I), Y, SPY, IsJump)

                    Case "无敌右"    '右跳
                        RE += MSAJump(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "无敌左"    '左跳
                        RE += MSAJump(X, SPD, False, False, C(I), Y, SPY, IsJump)

                    Case "滞空"    '匀速滞空跳
                        RE += MAAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "落体"    '匀速落体跳
                        RE += MAir(X, SPD, C(I), Y, SPY, IsJump, 0)
                    Case "落体站"    '匀速落体跳
                        RE += MAir(X, SPD, C(I), Y, SPY, IsJump, 0)
                    Case "落体正"    '匀速落体跳
                        RE += MAir(X, SPD, C(I), Y, SPY, IsJump, 1)
                    Case "落体反"    '匀速落体跳
                        RE += MAir(X, SPD, C(I), Y, SPY, IsJump, -1)

                    Case "跳" '原地跳
                        RE += Jump(X, SPD, C(I), Y, SPY, IsJump)
                    Case "缓冲跳"
                        RE += BJump(X, SPD, C(I), Y, SPY, IsJump)
                    Case "弹簧跳" '弹簧跳
                        RE += TJump(X, SPD, Y, SPY, IsJump)

                    Case "右跑"    '地面右跑
                        RE += MRun(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "左跑"    '地面左跑
                        RE += MRun(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "右走"    '地面右走
                        RE += MRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "左走"    '地面左走
                        RE += MRun(X, SPD, False, False, C(I), Y, SPY, IsJump)


                    Case "右陡坡右跑"    '地面右跑
                        RE += MDSlopeRun(X, SPD, True, True, C(I), Y, SPY, True)
                    Case "右陡坡左跑"    '地面左跑
                        RE += MDSlopeRun(X, SPD, False, True, C(I), Y, SPY, True)
                    Case "右陡坡右走"    '地面右走
                        RE += MDSlopeRun(X, SPD, True, False, C(I), Y, SPY, True)
                    Case "右陡坡左走"    '地面左走
                        RE += MDSlopeRun(X, SPD, False, False, C(I), Y, SPY, True)

                    Case "左陡坡右跑"    '地面右跑
                        RE += MDSlopeRun(X, SPD, True, True, C(I), Y, SPY, False)
                    Case "左陡坡左跑"    '地面左跑
                        RE += MDSlopeRun(X, SPD, False, True, C(I), Y, SPY, False)
                    Case "左陡坡右走"    '地面右走
                        RE += MDSlopeRun(X, SPD, True, False, C(I), Y, SPY, False)
                    Case "左陡坡左走"    '地面左走
                        RE += MDSlopeRun(X, SPD, False, False, C(I), Y, SPY, False)

                    Case "右缓坡右跑"    '地面右跑
                        RE += MHSlopeRun(X, SPD, True, True, C(I), Y, SPY, True)
                    Case "右缓坡左跑"    '地面左跑
                        RE += MHSlopeRun(X, SPD, False, True, C(I), Y, SPY, True)
                    Case "右缓坡右走"    '地面右走
                        RE += MHSlopeRun(X, SPD, True, False, C(I), Y, SPY, True)
                    Case "右缓坡左走"    '地面左走
                        RE += MHSlopeRun(X, SPD, False, False, C(I), Y, SPY, True)

                    Case "左缓坡右跑"    '地面右跑
                        RE += MHSlopeRun(X, SPD, True, True, C(I), Y, SPY, False)
                    Case "左缓坡左跑"    '地面左跑
                        RE += MHSlopeRun(X, SPD, False, True, C(I), Y, SPY, False)
                    Case "左缓坡右走"    '地面右走
                        RE += MHSlopeRun(X, SPD, True, False, C(I), Y, SPY, False)
                    Case "左缓坡左走"    '地面左走
                        RE += MHSlopeRun(X, SPD, False, False, C(I), Y, SPY, False)

                    Case "无敌右跑"    '地面右跑
                        RE += MSRun(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "无敌左跑"    '地面左跑
                        RE += MSRun(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "无敌右走"    '地面右走
                        RE += MSRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "无敌左走"    '地面左走
                        RE += MSRun(X, SPD, False, False, C(I), Y, SPY, IsJump)

                    Case "岩浆右走"    '地面右走
                        RE += MFireRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "岩浆左走"    '地面左走
                        RE += MFireRun(X, SPD, False, False, C(I), Y, SPY, IsJump)

'=======================================================================
                    Case "岩浆站立"    '地面站立
                        RE += MFireDuck(X, SPD, C(I), Y, SPY)
                    Case "岩浆站停"    '地面站立
                        RE += MFireDuck(X, SPD, 999, Y, SPY)

                    Case "站立"    '地面站立
                        RE += MDuck(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "正蹲"    '地面正蹲
                        RE += MDuck(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "反蹲"    '地面反蹲
                        RE += MDuck(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "站停"    '地面站立
                        RE += MDuck(X, SPD, True, True, 999, Y, SPY, IsJump)
                    Case "正停"    '地面正蹲
                        RE += MDuck(X, SPD, False, True, 999, Y, SPY, IsJump)
                    Case "反停"    '地面反蹲
                        RE += MDuck(X, SPD, False, False, 999, Y, SPY, IsJump)
                    Case "无敌站立"    '地面站立
                        RE += MSDuck(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "无敌正蹲"    '地面正蹲
                        RE += MSDuck(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "无敌反蹲"    '地面反蹲
                        RE += MSDuck(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "无敌站停"    '地面站立
                        RE += MSDuck(X, SPD, True, True, 999, Y, SPY, IsJump)
                    Case "无敌正停"    '地面正蹲
                        RE += MSDuck(X, SPD, False, True, 999, Y, SPY, IsJump)
                    Case "无敌反停"    '地面反蹲
                        RE += MSDuck(X, SPD, False, False, 999, Y, SPY, IsJump)
'=======================================================================
                    Case "低重滞空"    '低重力匀速跳
                        RE += LAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "低重落体" '低重力落体
                        RE += LAAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "低重跳"    '低重力匀速跳
                        RE += LAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "低重右跳"    '低重力右跳
                        RE += LJump(X, SPD, True, False, C(I), Y, SPY, IsJump, False)
                    Case "低重左跳"    '低重力左跳
                        RE += LJump(X, SPD, False, False, C(I), Y, SPY, IsJump, False)
                    Case "低重右"    '低重力右跳
                        RE += LAJump(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "低重左"    '低重力左跳
                        RE += LAJump(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "低重加速右跳"    '低重力右跳
                        RE += LJump(X, SPD, True, True, C(I), Y, SPY, IsJump, False)
                    Case "低重加速左跳"    '低重力左跳
                        RE += LJump(X, SPD, False, True, C(I), Y, SPY, IsJump, False)
                    Case "低重加速右"    '低重力右跳
                        RE += LAJump(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "低重加速左"    '低重力左跳
                        RE += LAJump(X, SPD, False, True, C(I), Y, SPY, IsJump)
'=======================================================================
                    Case "坐莲" '坐莲
                        RE += ZAAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "滞空坐莲" '坐莲
                        RE += ZAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "低重坐莲" '坐莲
                        RE += LZAAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "低重滞空坐莲" '坐莲
                        RE += LZAir(X, SPD, C(I), Y, SPY, IsJump)
'=======================================================================
                    Case "冰站立"    '地面站立
                        RE += IDuck(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "冰正蹲"    '地面正蹲
                        RE += IDuck(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "冰反蹲"    '地面反蹲
                        RE += IDuck(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "冰右跑"    '冰面右跑
                        RE += IRun(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "冰右走"    '冰面右跑
                        RE += IRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "冰左跑"    '冰面右跑
                        RE += IRun(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "冰左走"    '冰面右跑
                        RE += IRun(X, SPD, False, False, C(I), Y, SPY, IsJump)
'=======================================================================
                    Case "加速右缓冲跳"    '加速右跳
                        RE += MJump(X, SPD, True, True, C(I), Y, SPY, IsJump, True)
                    Case "加速左缓冲跳"    '加速左跳
                        RE += MJump(X, SPD, False, True, C(I), Y, SPY, IsJump, True)
                    Case "右缓冲跳"    '右跳
                        RE += MJump(X, SPD, True, False, C(I), Y, SPY, IsJump, True)
                    Case "左缓冲跳"    '左跳
                        RE += MJump(X, SPD, False, False, C(I), Y, SPY, IsJump, True)
                    Case "低重加速右缓冲跳"    '低重力右跳
                        RE += LJump(X, SPD, True, True, C(I), Y, SPY, IsJump, True)
                    Case "低重加速左缓冲跳"    '低重力左跳
                        RE += LJump(X, SPD, False, True, C(I), Y, SPY, IsJump, True)
                    Case "低重右缓冲跳"    '低重力右跳
                        RE += LJump(X, SPD, True, False, C(I), Y, SPY, IsJump, True)
                    Case "低重左缓冲跳"    '低重力左跳
                        RE += LJump(X, SPD, False, False, C(I), Y, SPY, IsJump, True)
'=======================================================================
                    Case "风地右跑"    '风地面右跑
                        RE += WRun(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "风地左跑"    '风地面左跑
                        RE += WRun(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "风地右走"    '风地面右走
                        RE += WRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "风地左走"    '风地面左走
                        RE += WRun(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "左风站立"
                        RE += WAir(X, SPD, C(I), Y, SPY, IsJump)
                    Case "风右跑跳"    '加速右跳
                        RE += WJump(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "风左跑跳"    '加速左跳
                        RE += WJump(X, SPD, False, True, C(I), Y, SPY, IsJump)
                    Case "风右跳"    '右跳
                        RE += WJump(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "风左跳"    '左跳
                        RE += WJump(X, SPD, False, False, C(I), Y, SPY, IsJump)
 '=======================================================================
                    Case "水右跳"    '右跳
                        RE += WtJump(X, SPD, True, True, C(I), Y, SPY, True, IsJump)
                    Case "水左跳"    '左跳
                        RE += WtJump(X, SPD, False, True, C(I), Y, SPY, True, IsJump)
                    Case "水右"    '右
                        RE += WtJump(X, SPD, True, True, C(I), Y, SPY, False, IsJump)
                    Case "水左"    '左
                        RE += WtJump(X, SPD, False, True, C(I), Y, SPY, False, IsJump)
                    Case "水右走"    '地面右走
                        RE += WtRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "水左走"    '地面左走
                        RE += WtRun(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "水跳"
                        RE += WtAir(X, SPD, False, True, C(I), Y, SPY, True, IsJump)
                    Case "水落"
                        RE += WtAir(X, SPD, False, True, C(I), Y, SPY, False, IsJump)
                    Case "水站"    '地面站立
                        RE += WtDuck(X, SPD, True, True, C(I), Y, SPY, IsJump)
                    Case "水冰右走"    '地面右走
                        RE += WtIceRun(X, SPD, True, False, C(I), Y, SPY, IsJump)
                    Case "水冰左走"    '地面左走
                        RE += WtIceRun(X, SPD, False, False, C(I), Y, SPY, IsJump)
                    Case "水冰站"    '地面站立
                        RE += WtIceDuck(X, SPD, True, True, C(I), Y, SPY, IsJump)
                End Select

                If ShowRst Then
                    CResult += vbTab & X.ToString & vbTab & SPD.ToString & vbCrLf & RE
                    T1.Text += vbTab & X.ToString & vbTab & SPD.ToString & vbCrLf
                    If CkLoc.Checked Then
                        T1.Text += RE
                        AddDataGrid(RE, S(I))
                    End If
                End If

            End If
        Next

        If ShowRst Then
            Label1.Text = "x=" & X & " Sx=" & SPD & " y=" & Y & " Sy=" & SPY
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
                    MoveData.Rows.Add(CMD, R(0), R(1), R(2), R(3))
                Else
                    MoveData.Rows.Add(F, R(0), R(1), R(2), R(3))
                End If
                F += 1
            End If
        Next
    End Sub
    Private Sub Button8_Click(sender As Object, e As EventArgs) Handles Button8.Click
        '找插入
        Label3.Text = DateTime.Now & " 开始搜索"
        T2.Text = T2.Text.Replace(vbTab, " ")
        Dim i As Integer
        Dim S(), Tf, Si(), Sii() As String
        S = T2.Text.Split(" ")

        Dim j, k As Integer
        Tf = GetInsert(T2.Text, "", "")

        Dim X, CX, CA As Single
        CX = Val(TR2.Text) '目标
        CA = Val(TR3.Text) '容错

        Si = Tf.Replace(vbCrLf, "|").Split("|")
        ListBox1.Items.Clear()

        For j = 2 To Val(TR4.Text) '滞空帧
            For i = 0 To UBound(Si) - 1
                Tf = Si(i).Replace("i0", "滞空" & j.ToString)
                X = TestJump(Tf, False)
                If Math.Abs(X - CX) <= CA Then
                    ListBox1.Items.Add(X & " " & Tf)
                End If
            Next
        Next

        For k = 0 To UBound(Si) - 1
            Tf = GetInsert(Si(k), "", "")
            Sii = Tf.Replace(vbCrLf, "|").Split("|")
            For j = 2 To Val(TR4.Text) '滞空帧
                For i = 0 To UBound(Sii) - 1
                    Tf = Sii(i).Replace("i0", "滞空" & j.ToString)
                    X = TestJump(Tf, False)
                    If Math.Abs(X - CX) <= CA Then
                        ListBox1.Items.Add(X & " " & Tf)
                    End If
                Next
            Next
        Next

        Label3.Text = DateTime.Now & " 搜索完成"
    End Sub


    Private Sub SaveGif(n As String, delayMs As Integer, endRe As Integer, Fr As Integer)
        Label1.Text = "正在保存Gif..."
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
        Label1.Text = "已保存Gif"
    End Sub

    Private Sub Button10_Click(sender As Object, e As EventArgs)
        On Error Resume Next
        PB.Image.Save(Application.StartupPath & "\" & T2.Text & ".PNG")
    End Sub

    Private Sub Button11_Click(sender As Object, e As EventArgs) Handles Button11.Click
        SaveGif(T2.Text & ".GIF", CInt(TDL.Text), 5, Draw2(True))
    End Sub
    Dim SpikeBlk(), GrdBlk() As Point, BlkType() As Integer
    Private Sub Button12_Click(sender As Object, e As EventArgs) Handles Button12.Click
        Erase SpikeBlk, CamBlk, GrdBlk
        DrawBG(NumW.Value, NumH.Value)
    End Sub

    Dim AniFrame As Integer = 0, NowFrame As Integer = 0
    Dim IsSaveFrame As Boolean = False

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        'GIF 已过时
        'PB.Image = Image.FromFile(Application.StartupPath & "\TEMP\" & NowFrame.ToString & ".PNG")
        'NowFrame += 1
        'If NowFrame > AniFrame Then NowFrame = 0
    End Sub

    Dim MX, MY As Single
    Dim MSpx, MSpy As Single, MAccX, MAccY As Single


    Dim PlayDelay As Integer
    Private Sub Button14_Click(sender As Object, e As EventArgs) Handles Button14.Click
        '模拟 暂时禁用
        PlayDelay = Val(TFPS.Text)
        GetCharAct()
        MX = 8
        MY = 48
        MSpx = 0
        MSpy = 0
        MAccX = 0
        MAccY = 0
        DrawBG(NumW.Value, NumH.Value)
        MP = PB.Image
        MIsJump = False
        Timer2.Enabled = Not Timer2.Enabled
        If Timer2.Enabled Then
            Button14.Text = "停止"
        Else
            Button14.Text = "模拟"
        End If
    End Sub

    Dim MIsJump As Boolean
    Dim MP, MP2 As Image
    Private Declare Function GetAsyncKeyState Lib "user32" (ByVal vKey As Integer) As Integer
    Dim FPS As Integer = 0, LastFrame As Integer = 0

    Private Sub Form1_MouseDoubleClick(sender As Object, e As MouseEventArgs) Handles MyBase.MouseDoubleClick
        If e.Button = MouseButtons.Right Then
            Form2.Visible = False
        End If
    End Sub

    Private Sub Form1_KeyPress(sender As Object, e As KeyPressEventArgs) Handles MyBase.KeyPress
        Select Case e.KeyChar
            Case "W"
                PicTip.Top -= 80
            Case "A"
                PicTip.Left -= 80
            Case "S"
                PicTip.Top += 80
            Case "D"
                PicTip.Left += 80
        End Select
    End Sub

    Private Sub Button16_Click(sender As Object, e As EventArgs) Handles Button16.Click
        Dim r = New StreamWriter(Application.StartupPath & "\data.txt", False, System.Text.Encoding.Default)
        For i = 0 To ComboBox2.Items.Count - 1
            r.WriteLine(ComboBox2.Items(i).ToString)
        Next
        r.WriteLine(TxtStrat.Text & " " & TX1.Text & " " & TS1.Text & " " & TD.Text & " " & T2.Text)
        r.Close()
        LoadStrat()
    End Sub

    Private Sub MaterialButton1_Click(sender As Object, e As EventArgs)
        ''走位叠加
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

    Private Sub MaterialTextBox21_KeyPress(sender As Object, e As KeyPressEventArgs) Handles MaterialTextBox21.KeyPress
        If e.KeyChar = vbCr Then
            '步幅搜索
            Dim C As Integer = Int(Val(MaterialTextBox21.Text) * 100 + 0.5)
            If C >= 0 AndAlso C < 1600 Then
                Dim R As String = C.ToString
                Dim s() = File.ReadAllLines(Application.StartupPath & "\step\" & R & ".txt")
                ListBox1.Items.Clear()
                For i As Integer = 0 To s.Length - 1
                    ListBox1.Items.Add(s(i))
                Next
                T2.Text = s(0)
                TestJump(T2.Text, True)
                Draw2(False)
                If Form2.Visible Then
                    Draw2form(False)
                End If
                Label3.Text = "步幅->" & MaterialTextBox21.Text
            End If
        End If
    End Sub

    Dim LB1State As Integer = 0
    Function Jump2Cmd(s As String) As String
        Dim r() = s.Split(vbTab)
        If r(3) = "0" Then
            Return "加速右跳" & r(1) & " 加速右" & r(2) & " 加速右跳20"
        Else
            Return "加速右跳" & r(1) & " 加速右" & r(2) & " 加速右跳" & r(3) & " 加速右" & r(4) & " 加速右跳20"
        End If
    End Function
    Private Sub MaterialTextBox22_KeyPress(sender As Object, e As KeyPressEventArgs) Handles MaterialTextBox22.KeyPress
        If e.KeyChar = vbCr Then
            '滞空搜索
            Dim C() As String = MaterialTextBox22.Text.Split("-")
            Dim R1 As Integer = C(0) * 1000
            Dim R2 As Integer = C(1) * 1000
            Dim s() = File.ReadAllLines(Application.StartupPath & "\jump\" & MaterialComboBox1.Text & ".txt")
            ListBox1.Items.Clear()
            LB1State = 1
            For i As Integer = 0 To s.Length - 1
                If Int(s(i)) <= R2 AndAlso Int(s(i)) >= R1 Then
                    Dim s2() = File.ReadAllLines(Application.StartupPath & "\jump\" & MaterialComboBox1.Text & "\" & s(i) & ".txt")
                    For j As Integer = 0 To s2.Length - 1
                        ListBox1.Items.Add(AJump2Cmd(s2(j)))
                    Next
                End If
            Next
            If ListBox1.Items.Count > 0 Then
                T2.Text = ListBox1.Items(0).ToString
                TestJump(T2.Text, True)
                Draw2(False)
            End If
        End If
    End Sub
    Function AJump2Cmd(A As String) As String
        Dim s() = A.Split(vbTab)
        If s.Length = 9 Then
            Return "[" & s(5) & "/" & s(6) & "/" & s(7) & "/" & s(8) & "]" & " 加速右跳" & s(1) & " 加速右" & s(2) & " 加速右跳" & s(3) & " 加速右" & s(4) & " 加速右跳20"
        Else
            Return ""
        End If
    End Function
    Private Sub MaterialTextBox21_Enter(sender As Object, e As EventArgs) Handles MaterialTextBox21.Click
        MaterialTextBox21.SelectAll()
    End Sub

    Private Sub MaterialTextBox22_Enter(sender As Object, e As EventArgs) Handles MaterialTextBox22.Click
        MaterialTextBox22.SelectAll()
    End Sub

    Private Sub ComboBox2_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox2.SelectedIndexChanged
        If ComboBox2.SelectedIndex >= 0 Then
            Dim s() = ComboBox2.SelectedItem.ToString.Split(" ")
            TxtStrat.Text = s(0)
            TX1.Text = s(1)
            TS1.Text = s(2)
            TD.Text = s(3)
            T2.Text = ""
            For i As Integer = 4 To s.Length - 1
                T2.Text += s(i) & " "
            Next
            Button4_Click(Button4, New EventArgs())
        End If
    End Sub

    Private Sub Button15_Click(sender As Object, e As EventArgs) Handles Button15.Click
        '弃用
        Form2.Visible = Not Form2.Visible
        If Form2.Visible Then
            With Form2
                .Left = Val(TxtFormLoc.Text)
                .Top = 0
                .Opacity = Val(TxtOpc.Text)
            End With
            With PicTip
                .Left = 0
                .Top = 0
            End With
        End If
    End Sub


    Private Declare Function timeGetTime Lib "winmm.dll" () As Integer
    Dim MJumpF As Byte = 1
    Dim CharAct(11) As Bitmap
    Dim OFX, OFY, CHW, CHH As Integer
    Sub GetCharAct()
        Dim F As String = Application.StartupPath & "\img\pack\" & ComboBox3.Text
        Select Case ComboBox3.Text
            Case "12621"
                F &= "\M1_Model-M1_Player_"
                Select Case ComboBox1.Text
                    Case "MarioMdl_4"
                        OFX = -8 * 5 : OFY = 0
                        CHW = 24 * 5 : CHH = 16 * 5
                    Case Else
                        OFX = 0 : OFY = 0
                        CHW = 16 * 5 : CHH = 16 * 5
                End Select
            Case "13133"
                F &= "\M3_Model-M3_Player_"
                Select Case ComboBox1.Text
                    Case "MarioMdl_4"
                        OFX = -8 * 5 : OFY = 0
                        CHW = 32 * 5 : CHH = 16 * 5
                    Case Else
                        OFX = 0 : OFY = 0
                        CHW = 16 * 5 : CHH = 16 * 5
                End Select
            Case "22349"
                F &= "\MW_Model-MW_Player_"
                Select Case ComboBox1.Text
                    Case "MarioMdl_4", "MarioMdl_3"
                        OFX = -8 * 5 : OFY = -16 * 5
                        CHW = 32 * 5 : CHH = 32 * 5
                    Case Else
                        OFX = 0 : OFY = -16 * 5
                        CHW = 16 * 5 : CHH = 32 * 5
                End Select
        End Select
        If CkDuck.Checked Then
            For I As Integer = 0 To 4
                CharAct(I) = Magnifier(Image.FromFile(F & ComboBox1.Text & "--stoop.0.png"), 5)
                CharAct(I + 5) = CharAct(I).Clone()
                CharAct(I + 5).RotateFlip(RotateFlipType.RotateNoneFlipX)
            Next
            CharAct(10) = Magnifier(Image.FromFile(F & ComboBox1.Text & "--stoop.0.png"), 5)
            CharAct(11) = CharAct(10).Clone()
            CharAct(11).RotateFlip(RotateFlipType.RotateNoneFlipX)
        Else
            'M1_Model-M1_Player_MarioMdl_4--stoop.0.png
            CharAct(0) = Magnifier(Image.FromFile(F & ComboBox1.Text & "--wait.0.png"), 5)
            CharAct(1) = Magnifier(Image.FromFile(F & ComboBox1.Text & "--walk.0.png"), 5)
            CharAct(2) = Magnifier(Image.FromFile(F & ComboBox1.Text & "--walk.1.png"), 5)

            Try
                CharAct(3) = Magnifier(Image.FromFile(F & ComboBox1.Text & "--walk.2.png"), 5)
            Catch
                CharAct(3) = Magnifier(Image.FromFile(F & ComboBox1.Text & "--walk.0.png"), 5)
            End Try

            CharAct(4) = Magnifier(Image.FromFile(F & ComboBox1.Text & "--jump.0.png"), 5)

            For I As Integer = 0 To 4
                CharAct(I + 5) = CharAct(I).Clone()
                CharAct(I + 5).RotateFlip(RotateFlipType.RotateNoneFlipX)
            Next
            CharAct(10) = Magnifier(Image.FromFile(F & ComboBox1.Text & "--stoop.0.png"), 5)
            CharAct(11) = CharAct(10).Clone()
            CharAct(11).RotateFlip(RotateFlipType.RotateNoneFlipX)
        End If


    End Sub
    Private Sub Timer2_Tick(sender As Object, e As EventArgs) Handles Timer2.Tick
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
                MX += MSpx
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
                MX += MSpx
            ElseIf MSpx <> 0 Then
                MX += MSpx
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
            MY += MSpy

            '碰撞检测
            If MY <= 48 Then
                MSpy = 0
                MY = 48
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
                MX += MSpx
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
                MX += MSpx
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
                MX += MSpx
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
                MY += MSpy
            End If
        End If


        MP2 = New Bitmap(1280, 800)
        Dim G = Graphics.FromImage(MP2)
        G.DrawImage(MP, 0, 0)
        If IsHitSpike(Int(MX * 5), Int(805 - MY * 5)) Then
            If MIsJump Then
                G.DrawImage(SetReverseColor(CharAct(4 + MJumpF * 5)), MX * 5, 805 - MY * 5, 80, 80)
            Else
                G.DrawImage(SetReverseColor(CharAct(MJumpF * 5 + GetWalkFrame(FPS, MSpx))), MX * 5, 805 - MY * 5, 80, 80)
            End If
        Else
            If MIsJump Then
                G.DrawImage(CharAct(4 + MJumpF * 5), MX * 5, 805 - MY * 5, 80, 80)
            Else
                G.DrawImage(CharAct(MJumpF * 5 + GetWalkFrame(FPS, MSpx)), MX * 5, 805 - MY * 5, 80, 80)
            End If
        End If
        G.DrawString("X=" & MX.ToString & vbCrLf & "Y=" & MY.ToString & vbCrLf & "Sx=" & MSpx.ToString & vbCrLf & "Sy=" & MSpy.ToString,
                        Label1.Font, Brushes.White, MX * 5, 805 - MY * 5 - 100)
        PB.Image = MP2
    End Sub

    Private Sub Button13_Click(sender As Object, e As EventArgs) Handles Button13.Click
        If Timer1.Enabled Then
            Button13.Text = "播放"
            Timer1.Enabled = False
        Else
            If Not IsSaveFrame Then
                '未保存动画，生成新帧
                T2.Text = T2.Text.Replace(vbTab, " ")
                T2.Text = T2.Text.Replace(vbCr, "")
                T2.Text = T2.Text.Replace(vbLf, "")
                TestJump(T2.Text, True)
                AniFrame = Draw2(True)
            End If
            If NowFrame > AniFrame Then NowFrame = 0
            Timer1.Interval = Int(TDL.Text)
            Timer1.Enabled = True
            Button13.Text = "停止"
        End If
    End Sub
    Private Sub MaterialButton3_Click(sender As Object, e As EventArgs) Handles MaterialButton3.Click
        Draw2form(True)
    End Sub
    Private Sub ListBox1_SelectedIndexChanged(sender As System.Object, e As Global.System.EventArgs) Handles ListBox1.SelectedIndexChanged
        If ListBox1.SelectedItem IsNot Nothing Then
            T2.Text = ListBox1.SelectedItem.ToString
            TestJump(T2.Text, True)
            Draw2(False)
        End If
    End Sub
    Private Sub Form1_Resize(sender As System.Object, e As Global.System.EventArgs) Handles MyBase.Resize
        PB.Width = Me.ClientSize.Width - 362
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
                    ReDim Preserve BlkType(0)
                    SpikeBlk(0).X = EPoint.X \ 80
                    SpikeBlk(0).Y = EPoint.Y \ 80
                    BlkType(0) = ComboBox7.SelectedIndex
                Else
                    ReDim Preserve SpikeBlk(UBound(SpikeBlk) + 1)
                    ReDim Preserve BlkType(UBound(BlkType) + 1)
                    SpikeBlk(UBound(SpikeBlk)).X = EPoint.X \ 80
                    SpikeBlk(UBound(SpikeBlk)).Y = EPoint.Y \ 80
                    BlkType(UBound(SpikeBlk)) = ComboBox7.SelectedIndex
                End If
                Select Case ComboBox7.SelectedIndex
                    Case 0 '刺
                        G.DrawImage(Magnifier(Image.FromFile(Application.StartupPath & "\img\T2.png"), 5), (EPoint.X \ 80) * 80, (EPoint.Y \ 80) * 80, 80, 80)
                    Case 1 '绿花
                        G.DrawImage(Magnifier(Image.FromFile(Application.StartupPath & "\img\T3.png"), 5), (EPoint.X \ 80) * 80, (EPoint.Y \ 80) * 80 - 40, 80, 120)
                    Case 2 '倒绿花
                        G.DrawImage(Magnifier(Image.FromFile(Application.StartupPath & "\img\T3.png"), 5), (EPoint.X \ 80) * 80, (EPoint.Y \ 80) * 80 + 120, 80, -120)
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

                'Case MouseButtons.Middle '设置摄像机，已过时
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
End Class
