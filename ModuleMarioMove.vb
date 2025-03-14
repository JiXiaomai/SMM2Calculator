Imports System.Net.Http.Headers

Friend Module ModuleMarioMove
    Public HitBlk As Boolean = False, HitBlkLoc As Single = 0
    Public Structure MarioFrameData
        Dim X As Single
        Dim Y As Single
        Dim Sx As Single
        Dim Sy As Single
        Dim Cmd As String
        Dim F As Integer
    End Structure
    Public FrameData(999) As MarioFrameData, FrameLoc As Integer
    Public Sub AddFrameData(x As Single, y As Single, spd As Single, spy As Single)
        With FrameData(FrameLoc)
            .X = x
            .Y = y
            .Sx = spd
            .Sy = spy
        End With
        FrameLoc += 1
    End Sub
    Public Function MAAir(ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        MAAir = ""
        For i = 1 To Frame
            Select Case Spy
                Case Is <= -3
                    Spy -= 0.34
                Case Is <= -0.12
                    Spy -= 0.31
                Case Is <= 0.3
                    Spy -= 0.08
                Case Is <= 1.5
                    Spy -= 0.34
                Case Is <= 2.5
                    Spy -= 0.25
                Case Else
                    Spy -= 0.06
            End Select
            If Spy < -4 Then Spy = -4 '最小速度-4
            Y += Spy
            X += Spd
            '落地检测
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If

            If Y <= Val(Form1.TY1.Text) AndAlso X <= Form1.TYW.Value - 3.1 Then
                '落地转换为跑步
                Y = Val(Form1.TY1.Text)
                Spy = 0
                IsJump = False
                'MAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                If i < Frame Then
                    '落地一帧延长？
                    X += Spd
                    'MAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                    AddFrameData(X, Y, Spd, Spy)
                    If Frame - i - 1 > 0 Then '还有剩余帧则计算地面状态
                        MAAir += MDuck(X, Spd, True, True, Frame - i - 1, Y, Spy, False)
                    End If
                    Exit Function
                End If
            Else
                'MAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If
        Next
    End Function
    Public Function SpinAir(ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        Dim SSpd As Single
        SpinAir = ""
        '固定11帧，未考虑落地
        For i = 1 To 11
            If Spy >= 0 Then
                '正常下落
                Select Case Spy
                    Case Is <= -3
                        Spy -= 0.34
                    Case Is <= -0.12
                        Spy -= 0.31
                    Case Is <= 0.3
                        Spy -= 0.08
                    Case Is <= 1.5
                        Spy -= 0.34
                    Case Is <= 2.5
                        Spy -= 0.25
                    Case Else
                        Spy -= 0.06
                End Select
            Else
                '速度小于0，获得反向15%加速度
                SSpd = -0.15 * Spy
                '加速度最小0.1
                If SSpd < 0.1 Then SSpd = 0.1
                '加速不能反向，速度最高变为0
                Spy += SSpd
                If Spy > 0 Then Spy = 0
            End If

            If Spy < -4 Then Spy = -4 '最小速度-4
            Y += Spy
            X += Spd
            'SpinAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
    End Function
    Public Function MAir(ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean, STP As Integer) As String
        Dim i As Integer
        Dim DY As Single
        MAir = ""
        For i = 1 To Frame
            X += Spd
            Select Case Spy
                Case Is <= -3
                    Spy -= 0.34
                Case Is <= -0.12
                    Spy -= 0.34
                Case Is <= 0.3
                    Spy -= 0.25
                Case Is <= 1.5
                    Spy -= 0.34
                Case Is <= 2.5
                    Spy -= 0.34
                Case Else
                    Spy -= 0.34
            End Select
            If Spy < -4 Then Spy = -4 '最小速度-4
            DY = Y
            Y += Spy
            '落地检测
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            If Y <= Val(Form1.TY1.Text) AndAlso X <= Form1.TYW.Value - 3.1 Then
                '落地转换为跑步
                Y = Val(Form1.TY1.Text)
                Spy = 0
                IsJump = False
                'MAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                If i < Frame Then '=================未测试完成===============
                    '落地一帧延长
                    X += Spd
                    'MAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                    AddFrameData(X, Y, Spd, Spy)
                    If Frame - i - 1 > 0 Then '还有剩余帧则计算地面状态
                        Select Case STP
                            Case 0 '站立
                                MAir += MDuck(X, Spd, True, True, Frame - i - 1, Y, Spy, False)
                            Case 1 '正蹲
                                MAir += MDuck(X, Spd, False, True, Frame - i - 1, Y, Spy, False)
                            Case -1 '反蹲
                                MAir += MDuck(X, Spd, False, False, Frame - i - 1, Y, Spy, False)
                        End Select
                    End If
                    Exit Function
                End If
            Else
                'MAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If

        Next
    End Function
    Public Function ZAir(ByRef X As Single, ByRef Spd As Single, FRAME As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        ZAir = ""
        For i = 1 To 4
            '滞空
            Select Case Spy
                Case Is <= -3
                    Spy -= 0.34
                Case Is <= -0.12
                    Spy -= 0.31
                Case Is <= 0.3
                    Spy -= 0.08
                Case Is <= 1.5
                    Spy -= 0.34
                Case Is <= 2.5
                    Spy -= 0.25
                Case Else
                    Spy -= 0.06
            End Select
            If Spy < -4 Then Spy = -4 '最小速度-4
            Y += Spy
            X += Spd
            'ZAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
        Spd = 0
        Spy = -6
        '未加坐莲前摇时间
        For i = 1 To FRAME
            Y += Spy
            X += Spd
            If Y <= 0 Then
                Y = 0
                Spy = 0
                'ZAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                Exit Function
            Else
                'ZAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If
        Next

    End Function
    Public Function ZAAir(ByRef X As Single, ByRef Spd As Single, FRAME As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        ZAAir = ""
        For i = 1 To 4
            '不滞空
            Select Case Spy
                Case Is <= -3
                    Spy -= 0.34
                Case Is <= -0.12
                    Spy -= 0.34
                Case Is <= 0.3
                    Spy -= 0.25
                Case Is <= 1.5
                    Spy -= 0.34
                Case Is <= 2.5
                    Spy -= 0.34
                Case Else
                    Spy -= 0.34
            End Select
            If Spy < -4 Then Spy = -4 '最小速度-4
            Y += Spy
            X += Spd

            'ZAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
        Spd = 0
        Spy = -6
        '未加坐莲前摇时间
        For i = 1 To FRAME
            Y += Spy
            X += Spd
            If Y <= 0 Then
                Y = 0
                Spy = 0
                'ZAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                Exit Function
            Else
                'ZAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If
        Next
    End Function
    Public Function LZAir(ByRef X As Single, ByRef Spd As Single, FRAME As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        LZAir = ""
        For i = 1 To 4
            '滞空
            Select Case Spy
                Case Is <= -0.12
                    Spy -= 0.062
                Case Is <= 0.3
                    Spy -= 0.016
                Case Is <= 1.5
                    Spy -= 0.068
                Case Is <= 2.5
                    Spy -= 0.05
                Case Else
                    Spy -= 0.012
            End Select
            If Spy < -2 Then Spy = -2 '最小速度-2
            Y += Spy
            X += Spd
            'LZAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
        Spd = 0
        Spy = -6
        '未加坐莲前摇时间
        For i = 1 To FRAME
            Y += Spy
            X += Spd
            If Y <= 0 Then
                Y = 0
                Spy = 0
                'LZAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                Exit Function
            Else
                'LZAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If
        Next
    End Function
    Public Function LZAAir(ByRef X As Single, ByRef Spd As Single, FRAME As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        LZAAir = ""
        For i = 1 To 4
            '不滞空
            Select Case Spy
                Case Is <= -0.12
                    Spy -= 0.068
                Case Is <= 0.3
                    Spy -= 0.05
                Case Is <= 1.5
                    Spy -= 0.068
                Case Is <= 2.5
                    Spy -= 0.068
                Case Else
                    Spy -= 0.068
            End Select
            If Spy < -2 Then Spy = -2 '最小速度-2
            Y += Spy
            X += Spd
            'LZAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
        Spd = 0
        Spy = -6
        '未加坐莲前摇时间
        For i = 1 To FRAME
            Y += Spy
            X += Spd
            If Y <= 0 Then
                Y = 0
                Spy = 0
                'LZAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                Exit Function
            Else
                'LZAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If
        Next
    End Function
    Public Function LAAir(ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        LAAir = ""
        If Spd > 1.5 Then Spd = 1.5
        If Spd < -1.5 Then Spd = -1.5
        For i = 1 To Frame
            '不滞空
            Select Case Spy
                Case Is <= -0.12
                    Spy -= 0.068
                Case Is <= 0.3
                    Spy -= 0.05
                Case Is <= 1.5
                    Spy -= 0.068
                Case Is <= 2.5
                    Spy -= 0.068
                Case Else
                    Spy -= 0.068
            End Select
            If Spy < -2 Then Spy = -2 '最小速度-2
            Y += Spy
            X += Spd
            '落地检测
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            If Y <= Val(Form1.TY1.Text) AndAlso X <= Form1.TYW.Value - 3.1 Then
                '落地转换为跑步
                Y = Val(Form1.TY1.Text)
                Spy = 0
                IsJump = False
                'LAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                LAAir += MDuck(X, Spd, True, True, Frame - i, Y, Spy, False)
                Exit Function
            Else
                'LAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If
        Next
    End Function
    Public Function LAir(ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        LAir = ""
        If Spd > 1.5 Then Spd = 1.5
        If Spd < -1.5 Then Spd = -1.5
        For i = 1 To Frame
            If Not IsJump And i = 1 Then '未起跳，第一帧初始化Y轴计算
                IsJump = True
                Select Case Math.Abs(Spd)
                    Case Is >= 1.5
                        Spy = 2.688
                    Case Is >= 0.7
                        Spy = 2.6656
                    Case Else
                        Spy = 2.5276
                End Select
            Else
                '滞空
                Select Case Spy
                    Case Is <= -0.12
                        Spy -= 0.062
                    Case Is <= 0.3
                        Spy -= 0.016
                    Case Is <= 1.5
                        Spy -= 0.068
                    Case Is <= 2.5
                        Spy -= 0.05
                    Case Else
                        Spy -= 0.012
                End Select
                If Spy < -2 Then Spy = -2 '最小速度-2
            End If
            Y += Spy
            X += Spd
            '落地检测
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            If Y <= Val(Form1.TY1.Text) AndAlso X <= Form1.TYW.Value - 3.1 Then
                '落地转换为跑步
                Y = Val(Form1.TY1.Text)
                Spy = 0
                IsJump = False
                'LAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                LAir += MDuck(X, Spd, True, True, Frame - i, Y, Spy, False)
                Exit Function
            Else
                'LAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If

        Next
    End Function
    Public Function LBAir(ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        LBAir = ""
        If Spd > 1.5 Then Spd = 1.5
        If Spd < -1.5 Then Spd = -1.5
        For i = 1 To Frame
            If Not IsJump And i = 1 Then '未起跳，第一帧初始化Y轴计算
                IsJump = True
                Select Case Math.Abs(Spd)
                    Case Is >= 1.5
                        Spy = 2.632
                    Case Is >= 0.7
                        Spy = 2.5976
                    Case Else
                        Spy = 2.4716
                End Select
            Else
                '滞空
                Select Case Spy
                    Case Is <= -0.12
                        Spy -= 0.062
                    Case Is <= 0.3
                        Spy -= 0.016
                    Case Is <= 1.5
                        Spy -= 0.068
                    Case Is <= 2.5
                        Spy -= 0.05
                    Case Else
                        Spy -= 0.012
                End Select
                If Spy < -2 Then Spy = -2 '最小速度-2
            End If
            Y += Spy
            X += Spd
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            'LBAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
    End Function
    Public Function IRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        IRun = ""
        '没加快跑刹车判断
        For i = 1 To Frame
            If Ori Then
                '向右
                If Spd < 0 Then
                    '从左向右
                    If Acc Then
                        Spd += 0.02
                    Else
                        Spd += 0.02
                    End If
                Else
                    If Acc Then
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.02
                            Case Is <= 1.5
                                Spd += 0.06
                            Case Is <= 2.25
                                Spd += 0.028
                            Case Else
                                Spd += 0.013
                        End Select
                        If Spd > 3 Then Spd = 3
                    Else
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.02
                            Case Else
                                Spd += 0.04
                        End Select
                        If Spd > 1.5 Then Spd = 1.5
                    End If
                End If
            Else
                '向左
                If Spd < 0 Then
                    '从右向左
                    If Acc Then
                        Spd += -0.02
                    Else
                        Spd += -0.02
                    End If
                Else
                    If Acc Then
                        Select Case Spd
                            Case Is >= -0.5
                                Spd += -0.02
                            Case Is >= -1.5
                                Spd += -0.06
                            Case Is >= -2.25
                                Spd += -0.028
                            Case Else
                                Spd += -0.013
                        End Select
                        If Spd < -3 Then Spd = -3
                    Else
                        Select Case Spd
                            Case Is >= -0.5
                                Spd += -0.02
                            Case Else
                                Spd += -0.04
                        End Select
                        If Spd < -1.5 Then Spd = -1.5
                    End If
                End If
            End If

            X += Spd
            'IRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
    End Function
    Public isOnAir As Boolean = False
    Public WallAirJumpFrame As Integer
    Public Function MRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        MRun = ""
        WallAirJumpFrame = 0
        '没加快跑刹车判断
        For i = 1 To Frame
            Spd -= DSpd
            If Ori Then
                '向右
                If Spd < 0 Then
                    '从左向右
                    If Acc Then
                        Spd += 0.1
                    Else
                        Spd += 0.1
                    End If
                Else
                    '从右向右
                    If Acc Then
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.1
                            Case Is <= 1.5
                                Spd += 0.06
                            Case Is <= 2.25
                                Spd += 0.029
                            Case Else
                                Spd += 0.035
                        End Select
                        If Spd > 3 Then Spd = 3
                    Else
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.1
                            Case Else
                                Spd += 0.03
                        End Select
                        If Spd > 1.5 Then Spd = 1.5
                    End If
                End If
            Else
                '向左
                If Spd > 0 Then
                    '从右向左
                    If Acc Then
                        Spd += -0.1
                    Else
                        Spd += -0.1
                    End If
                Else
                    '从左向左
                    If Acc Then
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.1
                            Case Is >= -1.5
                                Spd -= 0.06
                            Case Is >= -2.25
                                Spd -= 0.029
                            Case Else
                                Spd -= 0.035
                        End Select
                        If Spd < -3 Then Spd = -3
                    Else
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.1
                            Case Else
                                Spd -= 0.03
                        End Select
                        If Spd < -1.5 Then Spd = -1.5
                    End If
                End If
            End If
            Spd += DSpd
            X += Spd
            '跑出判断
            '前两帧-0.84
            '在3.1范围内可以跳起，按正常Y加速度计算
            If IsJump = False AndAlso X > Form1.TYW.Value - 3.1 Then
                If WallAirJumpFrame < 2 Then
                    Y -= 0.84
                    WallAirJumpFrame += 1
                Else
                    IsJump = True
                    Y -= 0.84
                    Spy = If(Acc, 0.31 - 0.84, 0.34 - 0.84)
                    MRun += MJump(X, Spd, Ori, Acc, Frame - i, Y, Spy, IsJump, False)
                    AddFrameData(X, Y, Spd, Spy)
                    Exit Function
                End If

            End If
            'MRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)



            '跑出判断
            '前两帧-0.84
            '在3.1范围内可以跳起，按正常Y加速度计算

            '未起跳 超出范围 剩余至少1帧
            If IsJump = False AndAlso X > Form1.TYW.Value - 3.1 AndAlso i < Frame Then
                '按地面计算X速度
                Spd -= DSpd
                If Ori Then
                    '向右
                    If Spd < 0 Then
                        '从左向右
                        If Acc Then
                            Spd += 0.1
                        Else
                            Spd += 0.1
                        End If
                    Else
                        '从右向右
                        If Acc Then
                            Select Case Spd
                                Case Is <= 0.5
                                    Spd += 0.1
                                Case Is <= 1.5
                                    Spd += 0.06
                                Case Is <= 2.25
                                    Spd += 0.029
                                Case Else
                                    Spd += 0.035
                            End Select
                            If Spd > 3 Then Spd = 3
                        Else
                            Select Case Spd
                                Case Is <= 0.5
                                    Spd += 0.1
                                Case Else
                                    Spd += 0.03
                            End Select
                            If Spd > 1.5 Then Spd = 1.5
                        End If
                    End If
                Else
                    '向左
                    If Spd > 0 Then
                        '从右向左
                        If Acc Then
                            Spd += -0.1
                        Else
                            Spd += -0.1
                        End If
                    Else
                        '从左向左
                        If Acc Then
                            Select Case Spd
                                Case Is >= -0.5
                                    Spd -= 0.1
                                Case Is >= -1.5
                                    Spd -= 0.06
                                Case Is >= -2.25
                                    Spd -= 0.029
                                Case Else
                                    Spd -= 0.035
                            End Select
                            If Spd < -3 Then Spd = -3
                        Else
                            Select Case Spd
                                Case Is >= -0.5
                                    Spd -= 0.1
                                Case Else
                                    Spd -= 0.03
                            End Select
                            If Spd < -1.5 Then Spd = -1.5
                        End If
                    End If
                End If
                Spd += DSpd
                X += Spd
                Y -= 0.84
                Spy = 0
                'MRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                i += 1
                If i < Frame Then
                    IsJump = True
                    Spy = If(Acc, 0.31 - 0.84, 0.34 - 0.84)
                    MRun += MJump(X, Spd, Ori, Acc, Frame - i, Y, Spy, IsJump, False)
                    Exit Function
                End If
            End If

        Next
    End Function
    Public Function MDSlopeRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef UPDOWN As Boolean) As String
        Dim i As Integer
        MDSlopeRun = ""
        '陡坡跑步
        For i = 1 To Frame
            Spd -= DSpd
            If UPDOWN Then
                '右坡/
                If Ori Then
                    '向右，上坡
                    If Spd < 0 Then
                        '从左向右，下坡减速
                        Spd += 0.021
                    Else
                        '从右向右，上坡加速
                        Spd += 0.108
                    End If
                    If Acc Then
                        If Spd > 1.5 Then Spd = 1.5
                    Else
                        If Spd > 0.75 Then Spd = 0.75
                    End If
                Else
                    '向左，下坡
                    If Spd > 0 Then
                        '从右向左，上坡减速
                        Spd -= 0.123
                    Else
                        '从左向左，下坡加速
                        Spd -= 0.077
                    End If
                    If Acc Then
                        If Spd < -4.32 Then Spd = -4.32
                    Else
                        If Spd < -2.16 Then Spd = -2.16
                    End If
                End If
            Else
                '左坡\
                If Ori Then
                    '向右，下坡
                    If Spd < 0 Then
                        '从左向右，上坡减速
                        Spd += 0.123
                    Else
                        '从右向右，下坡加速
                        Spd += 0.077
                    End If
                    If Acc Then
                        If Spd > 4.32 Then Spd = 4.32
                    Else
                        If Spd > 2.16 Then Spd = 2.16
                    End If
                Else
                    '向左，上坡
                    If Spd > 0 Then
                        '从右向左，下坡减速
                        Spd -= 0.021
                    Else
                        '从左向左，上坡加速
                        Spd -= 0.108
                    End If
                    If Acc Then
                        If Spd < -1.5 Then Spd = -1.5
                    Else
                        If Spd < -0.75 Then Spd = -0.75
                    End If
                End If
            End If

            Spd += DSpd
            '先计算速度再投影
            X += Spd / Math.Sqrt(2)
            'MDSlopeRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
    End Function
    Public Function MHSlopeRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef UPDOWN As Boolean) As String
        Dim i As Integer
        MHSlopeRun = ""
        '陡坡跑步
        For i = 1 To Frame
            Spd -= DSpd
            If UPDOWN Then
                '右坡/
                If Ori Then
                    '向右，上坡
                    If Spd < 0 Then
                        '从左向右，下坡减速
                        Spd += 0.028
                    Else
                        '从右向右，上坡加速
                        Spd += 0.04
                    End If
                    If Acc Then
                        If Spd > 2.0625 Then Spd = 2.0625
                    Else
                        If Spd > 1.03125 Then Spd = 1.03125
                    End If
                Else
                    '向左，下坡
                    If Spd > 0 Then
                        '从右向左，上坡减速
                        Spd -= 0.06
                    Else
                        '从左向左，下坡加速
                        Spd -= 0.055
                    End If
                    If Acc Then
                        If Spd < -3.75 Then Spd = -3.75
                    Else
                        If Spd < -1.875 Then Spd = -1.875
                    End If
                End If
            Else
                '左坡\
                If Ori Then
                    '向右，下坡
                    If Spd < 0 Then
                        '从左向右，上坡减速
                        Spd += 0.06
                    Else
                        '从右向右，下坡加速
                        Spd += 0.055
                    End If
                    If Acc Then
                        If Spd > 3.75 Then Spd = 3.75
                    Else
                        If Spd > 1.875 Then Spd = 1.875
                    End If
                Else
                    '向左，上坡
                    If Spd > 0 Then
                        '从右向左，下坡减速
                        Spd -= 0.028
                    Else
                        '从左向左，上坡加速
                        Spd -= 0.04
                    End If
                    If Acc Then
                        If Spd < -2.0625 Then Spd = -2.0625
                    Else
                        If Spd < -1.03125 Then Spd = -1.03125
                    End If
                End If
            End If

            Spd += DSpd
            '先计算速度再投影
            X += Spd * 2 / Math.Sqrt(5)
            'MHSlopeRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
    End Function
    Public Function MSRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        MSRun = ""
        '没加快跑刹车判断
        For i = 1 To Frame
            Spd -= DSpd
            If Ori Then
                '向右
                If Spd < 0 Then
                    '从左向右
                    If Acc Then
                        Spd += 0.1
                    Else
                        Spd += 0.1
                    End If
                Else
                    '从右向右
                    If Acc Then
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.13
                            Case Is <= 2
                                Spd += 0.078
                            Case Is <= 3.25
                                Spd += 0.039
                            Case Else
                                Spd += 0.035
                        End Select
                        If Spd > 4 Then Spd = 4
                    Else
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.13
                            Case Else
                                Spd += 0.04
                        End Select
                        If Spd > 2 Then Spd = 2
                    End If
                End If
            Else
                '向左
                If Spd > 0 Then
                    '从右向左
                    If Acc Then
                        Spd += -0.1
                    Else
                        Spd += -0.1
                    End If
                Else
                    '从左向左
                    If Acc Then
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.13
                            Case Is >= -2
                                Spd -= 0.078
                            Case Is >= -3.25
                                Spd -= 0.039
                            Case Else
                                Spd -= 0.035
                        End Select
                        If Spd < -4 Then Spd = -4
                    Else
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.13
                            Case Else
                                Spd -= 0.04
                        End Select
                        If Spd < -2 Then Spd = -2
                    End If
                End If
            End If
            Spd += DSpd
            X += Spd
            'MSRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
    End Function
    Public Function WtRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        WtRun = ""
        '水走
        For i = 1 To Frame
            Spd -= DSpd
            If Ori Then
                '向右
                If Spd < 0 Then
                    '从左向右
                    Spd += 0.05
                Else
                    '从右向右
                    Spd += 0.025
                    If Spd > 0.5625 Then Spd = 0.5625
                End If
            Else
                '向左
                If Spd > 0 Then
                    '从右向左
                    Spd += -0.05
                Else
                    '从左向左
                    Spd += -0.025
                    If Spd < -0.5625 Then Spd = -0.5625
                End If
            End If
            Spd += DSpd
            X += Spd
            'WtRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
    End Function
    Public Function WtIceRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        WtIceRun = ""
        '水冰走
        For i = 1 To Frame
            Spd -= DSpd
            If Ori Then
                '向右
                If Spd < 0 Then
                    '从左向右
                    Spd += 0.012
                Else
                    '从右向右
                    Spd += 0.012
                    If Spd > 0.5625 Then Spd = 0.5625
                End If
            Else
                '向左
                If Spd > 0 Then
                    '从右向左
                    Spd += -0.012
                Else
                    '从左向左
                    Spd += -0.012
                    If Spd < -0.5625 Then Spd = -0.5625
                End If
            End If
            Spd += DSpd
            X += Spd
            'WtIceRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
    End Function
    Public Function WtJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef AirJump As Boolean, ByRef IsJump As Boolean) As String
        Dim i As Integer
        '有怠速转换
        WtJump = ""

        For i = 1 To Frame
            Spd -= DSpd
            If i = 1 Then '第一帧初始化Y轴计算
                If Not IsJump Then
                    '未起跳，地面跳
                    Spy = 1.25
                    IsJump = True
                Else
                    '已起跳
                    If AirJump Then '水中跳
                        Select Case Spy '水跳加速度=1+当前加速度
                            Case -1.5
                                Spy += 1
                            Case Is >= 0
                                Spy += 1 - 0.043
                            Case Else
                                Spy += 1 - 0.027
                        End Select
                    Else '水中落体
                        Select Case Spy
                            Case Is > 1.8 '水跳满速1.8，超过则持续1帧
                                Spy = 1.8
                            Case Is >= 0
                                Spy -= 0.043
                            Case Else
                                Spy -= 0.027
                        End Select
                        If Spy < -1.5 Then Spy = -1.5 '最小速度
                    End If
                End If
            Else
                Select Case Spy
                    Case Is > 1.8 '水跳满速1.8，超过则持续1帧
                        Spy = 1.8
                    Case Is >= 0
                        Spy -= 0.043
                    Case Else
                        Spy -= 0.027
                End Select
                If Spy < -1.5 Then Spy = -1.5 '最小速度
            End If
            Y += Spy


            If Ori Then
                If Spd >= 0 Then
                    Spd += 0.025
                    If Spd > 1.125 Then Spd = 1.125
                Else
                    Spd += 0.03
                End If
            Else
                If Spd <= 0 Then
                    Spd += -0.025
                    If Spd < -1.125 Then Spd = -1.125
                Else
                    Spd += -0.03
                End If
            End If
            Spd += DSpd
            X += Spd
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            '水中未加落地检测
            'WtJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
    End Function
    Public Function WtAir(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef AirJump As Boolean, ByRef IsJump As Boolean) As String
        Dim i As Integer
        '有怠速转换
        WtAir = ""
        '水中自由落体减速
        For i = 1 To Frame

            If i = 1 Then '第一帧初始化Y轴计算
                If Not IsJump Then
                    '未起跳，地面跳
                    Spy = 1.25
                    IsJump = True
                Else
                    '已起跳
                    If AirJump Then '水中跳
                        Select Case Spy '水跳加速度=1+当前加速度
                            Case -1.5
                                Spy += 1
                            Case Is >= 0
                                Spy += 1 - 0.043
                            Case Else
                                Spy += 1 - 0.027
                        End Select
                    Else '水中落体
                        Select Case Spy
                            Case Is > 1.8 '水跳满速1.8，超过则持续1帧
                                Spy = 1.8
                            Case Is >= 0
                                Spy -= 0.043
                            Case Else
                                Spy -= 0.027
                        End Select
                        If Spy < -1.5 Then Spy = -1.5 '最小速度
                    End If
                End If
            Else
                Select Case Spy
                    Case Is > 1.8 '水跳满速1.8，超过则持续1帧
                        Spy = 1.8
                    Case Is >= 0
                        Spy -= 0.043
                    Case Else
                        Spy -= 0.027
                End Select
                If Spy < -1.5 Then Spy = -1.5 '最小速度
            End If
            Y += Spy

            Spd -= DSpd
            If Spd >= 0 Then
                Spd += -0.02
                If Spd < 0 Then Spd = 0
            Else
                Spd += 0.02
                If Spd > 0 Then Spd = 0
            End If
            Spd += DSpd
            X += Spd
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            '水中未加落地检测
            'WtAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
    End Function
    Public Function WtDuck(ByRef X As Single, ByRef Spd As Single, ST As Boolean, ORI As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        WtDuck = ""
        For i = 1 To Frame
            Spd -= DSpd
            If Spd > 0 Then
                Spd -= 0.04
                If Spd < 0 Then Spd = 0
            Else
                Spd += 0.04
                If Spd > 0 Then Spd = 0
            End If
            Spd += DSpd
            X += Spd
            'WtDuck += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
            If Spd = 0 Then Exit For
        Next
    End Function
    Public Function WtIceDuck(ByRef X As Single, ByRef Spd As Single, ST As Boolean, ORI As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        WtIceDuck = ""
        For i = 1 To Frame
            Spd -= DSpd
            If Spd > 0 Then
                Spd -= 0.01
                If Spd < 0 Then Spd = 0
            Else
                Spd += 0.01
                If Spd > 0 Then Spd = 0
            End If
            Spd += DSpd
            X += Spd
            'WtIceDuck += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
            If Spd = 0 Then Exit For
        Next
    End Function
    Public Function WRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        WRun = ""
        '没加快跑刹车判断
        For i = 1 To Frame
            Spd -= WSpd
            If WFrame < 10 Then
                WSpd += -0.1
                WFrame += 1
            End If

            If Ori Then
                '向右
                If Spd < 0 Then
                    '从左向右
                    If Acc Then
                        Spd += 0.1
                    Else
                        Spd += 0.1
                    End If
                Else
                    '从右向右
                    If Acc Then
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.1
                            Case Is <= 1.5
                                Spd += 0.06
                            Case Is <= 2.25
                                Spd += 0.029
                            Case Else
                                Spd += 0.035
                        End Select
                        If Spd > 3 Then Spd = 3
                    Else
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.1
                            Case Else
                                Spd += 0.03
                        End Select
                        If Spd > 1.5 Then Spd = 1.5
                    End If
                End If
            Else
                '向左
                If Spd > 0 Then
                    '从右向左
                    If Acc Then
                        Spd += -0.1
                    Else
                        Spd += -0.1
                    End If
                Else
                    '从左向左
                    If Acc Then
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.1
                            Case Is >= -1.5
                                Spd -= 0.06
                            Case Is >= -2.25
                                Spd -= 0.029
                            Case Else
                                Spd -= 0.035
                        End Select
                        If Spd < -3 Then Spd = -3
                    Else
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.1
                            Case Else
                                Spd -= 0.03
                        End Select
                        If Spd < -1.5 Then Spd = -1.5
                    End If
                End If
            End If
            Spd += WSpd
            X += Spd
            'WRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
    End Function
    Public Function MDuck(ByRef X As Single, ByRef Spd As Single, ST As Boolean, ORI As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        MDuck = ""
        For i = 1 To Frame
            Spd -= DSpd
            If Spd > 0 Then
                If ST Then
                    '自由停止
                    Select Case Spd
                        Case Is <= 1.5
                            Spd -= 0.05
                        Case Else
                            Spd -= 0.035
                    End Select
                    If Spd < 0 Then Spd = 0
                Else
                    '蹲滑减速
                    If ORI Then
                        '正向蹲滑
                        Select Case Spd
                            Case Is <= 1.5
                                Spd -= 0.06
                            Case Else
                                Spd -= 0.05
                        End Select
                        If Spd < 0 Then Spd = 0
                    Else
                        '反向蹲滑
                        Spd -= 0.07
                        If Spd < 0 Then Spd = 0
                    End If
                End If
            Else
                If ST Then
                    '自由停止
                    Select Case Spd
                        Case Is >= -1.5
                            Spd += 0.05
                        Case Else
                            Spd += 0.035
                    End Select
                    If Spd > 0 Then Spd = 0
                Else
                    '蹲滑减速
                    If ORI Then
                        '正向蹲滑
                        Select Case Spd
                            Case Is >= -1.5
                                Spd += 0.06
                            Case Else
                                Spd += 0.05
                        End Select
                        If Spd > 0 Then Spd = 0
                    Else
                        '反向蹲滑
                        Spd += 0.07
                        If Spd > 0 Then Spd = 0
                    End If
                End If
            End If
            Spd += DSpd
            X += Spd
            'MDuck += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
            If Spd = 0 Then Exit For

            '跑出判断
            '前两帧-0.84
            '在3.1范围内可以跳起，按正常Y加速度计算

            '未起跳 超出范围 剩余至少1帧
            If IsJump = False AndAlso X > Form1.TYW.Value - 3.1 AndAlso i < Frame Then
                '按地面计算X速度
                Spd -= DSpd
                If ORI Then
                    '向右
                    If Spd < 0 Then
                        '从左向右
                        Spd += 0.1
                    Else
                        '从右向右
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.1
                            Case Is <= 1.5
                                Spd += 0.06
                            Case Is <= 2.25
                                Spd += 0.029
                            Case Else
                                Spd += 0.035
                        End Select
                        If Spd > 3 Then Spd = 3
                    End If
                Else
                    '向左
                    If Spd > 0 Then
                        '从右向左
                        Spd += -0.1
                    Else
                        '从左向左
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.1
                            Case Is >= -1.5
                                Spd -= 0.06
                            Case Is >= -2.25
                                Spd -= 0.029
                            Case Else
                                Spd -= 0.035
                        End Select
                        If Spd < -3 Then Spd = -3
                    End If
                End If
                Spd += DSpd
                X += Spd
                Y -= 0.84
                Spy = 0
                'MDuck += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                i += 1
                If i < Frame Then
                    IsJump = True
                    Y -= 0.84
                    Spy = If(True, 0.31 - 0.84, 0.34 - 0.84)
                    MDuck += MJump(X, Spd, ORI, True, Frame - i, Y, Spy, IsJump, False)
                    AddFrameData(X, Y, Spd, Spy)
                    Exit Function
                End If
            End If

        Next

    End Function
    Public Function MSDuck(ByRef X As Single, ByRef Spd As Single, ST As Boolean, ORI As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        MSDuck = ""
        For i = 1 To Frame
            Spd -= DSpd
            If Spd > 0 Then
                If ST Then
                    '自由停止
                    Select Case Spd
                        Case Is <= 2
                            Spd -= 0.05
                        Case Else
                            Spd -= 0.035
                    End Select
                    If Spd < 0 Then Spd = 0
                Else
                    '蹲滑减速
                    If ORI Then
                        '正向蹲滑
                        Select Case Spd
                            Case Is <= 2
                                Spd -= 0.06
                            Case Else
                                Spd -= 0.05
                        End Select
                        If Spd < 0 Then Spd = 0
                    Else
                        '反向蹲滑
                        Spd -= 0.07
                        If Spd < 0 Then Spd = 0
                    End If
                End If
            Else
                If ST Then
                    '自由停止
                    Select Case Spd
                        Case Is >= -2
                            Spd += 0.05
                        Case Else
                            Spd += 0.035
                    End Select
                    If Spd > 0 Then Spd = 0
                Else
                    '蹲滑减速
                    If ORI Then
                        '正向蹲滑
                        Select Case Spd
                            Case Is >= -2
                                Spd += 0.06
                            Case Else
                                Spd += 0.05
                        End Select
                        If Spd > 0 Then Spd = 0
                    Else
                        '反向蹲滑
                        Spd += 0.07
                        If Spd > 0 Then Spd = 0
                    End If
                End If
            End If
            Spd += DSpd
            X += Spd
            'MSDuck += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)

            If Spd = 0 Then Exit For

            '跑出判断
            '前两帧-0.84
            '在3.1范围内可以跳起，按正常Y加速度计算

            '未起跳 超出范围 剩余至少1帧
            If IsJump = False AndAlso X > Form1.TYW.Value - 3.1 AndAlso i < Frame Then
                '按地面计算X速度
                Spd -= DSpd
                If ORI Then
                    '向右
                    If Spd < 0 Then
                        '从左向右
                        Spd += 0.1
                    Else
                        '从右向右
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.1
                            Case Is <= 1.5
                                Spd += 0.06
                            Case Is <= 2.25
                                Spd += 0.029
                            Case Else
                                Spd += 0.035
                        End Select
                        If Spd > 3 Then Spd = 3
                    End If
                Else
                    '向左
                    If Spd > 0 Then
                        '从右向左
                        Spd += -0.1
                    Else
                        '从左向左
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.1
                            Case Is >= -1.5
                                Spd -= 0.06
                            Case Is >= -2.25
                                Spd -= 0.029
                            Case Else
                                Spd -= 0.035
                        End Select
                        If Spd < -3 Then Spd = -3
                    End If
                End If
                Spd += DSpd
                X += Spd
                Y -= 0.84
                Spy = 0
                'MSDuck += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                i += 1
                If i < Frame Then
                    IsJump = True
                    Y -= 0.84
                    Spy = If(True, 0.31 - 0.84, 0.34 - 0.84)
                    MSDuck += MJump(X, Spd, ORI, True, Frame - i, Y, Spy, IsJump, False)
                    AddFrameData(X, Y, Spd, Spy)
                    Exit Function
                End If
            End If

        Next

    End Function
    Public Function MFireRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        MFireRun = ""
        '没加快跑刹车判断
        For i = 1 To Frame
            If Ori Then
                '向右
                If Spd < 0 Then
                    '从左向右
                    Spd += 0.03
                Else
                    '从右向右
                    Spd += 0.025
                End If
                If Spd > 1.5 Then Spd = 1.5
            Else
                '向左
                If Spd > 0 Then
                    '从右向左
                    Spd += -0.03
                Else
                    '从左向左
                    Spd += -0.025
                End If
                If Spd < -1.5 Then Spd = -1.5
            End If
            X += Spd
            'MFireRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
    End Function
    Public Function MWallJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean, IsBuffer As Boolean) As String
        Dim i As Integer
        '有怠速转换
        MWallJump = ""

        For i = 1 To Frame

            If Not IsJump And i = 1 Then '未起跳，第一帧初始化Y轴计算
                IsJump = True
                Select Case Math.Abs(Spd)
                    Case Is >= 2.8
                        Spy = If(IsBuffer, 3.588, 3.868)
                    Case Is >= 1.5
                        Spy = If(IsBuffer, 3.528, 3.808)
                    Case Is >= 0.7
                        Spy = If(IsBuffer, 3.468, 3.748)
                    Case Else
                        Spy = If(IsBuffer, 3.288, 3.568)
                End Select

            Else
                '滞空
                Select Case Spy
                    Case Is <= -3
                        Spy -= 0.34
                    Case Is <= -0.12
                        Spy -= 0.31
                    Case Is <= 0.3
                        Spy -= 0.08
                    Case Is <= 1.5
                        Spy -= 0.34
                    Case Is <= 2.5
                        Spy -= 0.25
                    Case Else
                        Spy -= 0.06
                End Select
                If Spy < -4 Then Spy = -4 '最小速度-4
            End If
            Y += Spy

            Spd -= DSpd
            Spd = If(Ori, -2.25, 2.25)
            Spd += DSpd
            X += Spd
            '落地检测
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            If Y <= Val(Form1.TY1.Text) AndAlso X <= Form1.TYW.Value - 3.1 Then
                '落地转换为跑步
                Y = Val(Form1.TY1.Text)
                Spy = 0
                IsJump = False
                'MWallJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                MWallJump += MRun(X, Spd, Ori, Acc, Frame - i, Y, Spy, False)
                Exit Function
            Else
                'MWallJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If
        Next
    End Function
    Public Function MFireDuck(ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single) As String
        Dim i As Integer
        MFireDuck = ""
        For i = 1 To Frame
            If Spd > 0 Then
                Spd -= 0.02
                If Spd < 0 Then Spd = 0
            Else
                Spd += 0.02
                If Spd > 0 Then Spd = 0
            End If
            X += Spd
            'MFireDuck += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
            If Spd = 0 Then Exit For
        Next

    End Function
    Public Function IDuck(ByRef X As Single, ByRef Spd As Single, ST As Boolean, ORI As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        IDuck = ""
        For i = 1 To Frame
            Spd -= DSpd
            If Spd > 0 Then
                If ST Then
                    '自由停止
                    Select Case Spd
                        Case Is <= 1.5
                            Spd -= 0.015
                        Case Else
                            Spd -= 0.013
                    End Select
                    If Spd < 0 Then Spd = 0
                Else
                    '蹲滑减速
                    If ORI Then
                        '正向蹲滑
                        Select Case Spd
                            Case Is <= 1.5
                                Spd -= 0.018
                            Case Else
                                Spd -= 0.015
                        End Select
                        If Spd < 0 Then Spd = 0
                    Else
                        '反向蹲滑
                        Spd -= 0.018
                        If Spd < 0 Then Spd = 0
                    End If
                End If
            Else
                If ST Then
                    '自由停止
                    Select Case Spd
                        Case Is >= -1.5
                            Spd += 0.015
                        Case Else
                            Spd += 0.013
                    End Select
                    If Spd > 0 Then Spd = 0
                Else
                    '蹲滑减速
                    If ORI Then
                        '正向蹲滑
                        Select Case Spd
                            Case Is >= -1.5
                                Spd += 0.018
                            Case Else
                                Spd += 0.015
                        End Select
                        If Spd > 0 Then Spd = 0
                    Else
                        '反向蹲滑
                        Spd += 0.018
                        If Spd > 0 Then Spd = 0
                    End If
                End If
            End If
            Spd += DSpd
            X += Spd
            'IDuck += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
            If Spd = 0 Then Exit For
        Next
    End Function
    Public Function LJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean, IsBuffer As Boolean) As String
        Dim i As Integer
        '有怠速转换
        LJump = ""
        For i = 1 To Frame
            Spd -= DSpd

            If Not IsJump And i = 1 Then '未起跳，第一帧初始化Y轴计算
                IsJump = True
                Select Case Math.Abs(Spd)
                    Case Is >= 1.5
                        Spy = If(IsBuffer, 2.632, 2.688)
                    Case Is >= 0.7
                        Spy = If(IsBuffer, 2.5976, 2.6536)
                    Case Else
                        Spy = If(IsBuffer, 2.4716, 2.5276)
                End Select
            Else
                '滞空
                Select Case Spy
                    Case Is <= -0.12
                        Spy -= 0.062
                    Case Is <= 0.3
                        Spy -= 0.016
                    Case Is <= 1.5
                        Spy -= 0.068
                    Case Is <= 2.5
                        Spy -= 0.05
                    Case Else
                        Spy -= 0.012
                End Select
                If Spy < -2 Then Spy = -2 '最小速度-2
            End If
            Y += Spy


            If Ori Then
                'R
                If Spd >= 0 Then
                    'RR
                    If Acc Then
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.08
                            Case Else
                                Spd += 0.06
                        End Select
                        If Spd > 1.5 Then Spd = 1.5
                    Else
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.08
                            Case Else
                                Spd += 0.03
                        End Select
                        If Spd > 1.5 Then Spd = 1.5
                    End If
                Else
                    'RL
                    Spd += 0.09
                    If Spd < -1.5 Then Spd = -1.5
                End If
            Else
                'L
                If Spd <= 0 Then
                    'LL
                    If Acc Then
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.08
                            Case Else
                                Spd -= 0.06
                        End Select
                        If Spd < -1.5 Then Spd = -1.5
                    Else
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.08
                            Case Else
                                Spd -= 0.03
                        End Select
                        If Spd < -1.5 Then Spd = -1.5
                    End If
                Else
                    'LR
                    Spd -= 0.09
                    If Spd > 1.5 Then Spd = 1.5
                End If
            End If
            Spd += DSpd
            X += Spd
            '落地检测
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            If Y <= Val(Form1.TY1.Text) AndAlso X <= Form1.TYW.Value - 3.1 Then
                '落地转换为跑步
                Y = Val(Form1.TY1.Text)
                Spy = 0
                IsJump = False
                'LJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                LJump += MRun(X, Spd, Ori, Acc, Frame - i, Y, Spy, False)
                Exit Function
            Else
                'LJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If
        Next
    End Function
    Public Function LAJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        '有怠速转换
        LAJump = ""
        For i = 1 To Frame
            Spd -= DSpd
            '不滞空
            Select Case Spy
                Case Is <= -0.12
                    Spy -= 0.068
                Case Is <= 0.3
                    Spy -= 0.05
                Case Is <= 1.5
                    Spy -= 0.068
                Case Is <= 2.5
                    Spy -= 0.068
                Case Else
                    Spy -= 0.068
            End Select
            If Spy < -2 Then Spy = -2 '最小速度-2
            Y += Spy


            If Ori Then
                'R
                If Spd >= 0 Then
                    'RR
                    If Acc Then
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.08
                            Case Else
                                Spd += 0.06
                        End Select
                        If Spd > 1.5 Then Spd = 1.5
                    Else
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.08
                            Case Else
                                Spd += 0.03
                        End Select
                        If Spd > 1.5 Then Spd = 1.5
                    End If
                Else
                    'RL
                    Spd += 0.09
                    If Spd < -1.5 Then Spd = -1.5
                End If
            Else
                'L
                If Spd <= 0 Then
                    'LL
                    If Acc Then
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.08
                            Case Else
                                Spd -= 0.06
                        End Select
                        If Spd < -1.5 Then Spd = -1.5
                    Else
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.08
                            Case Else
                                Spd -= 0.03
                        End Select
                        If Spd < -1.5 Then Spd = -1.5
                    End If
                Else
                    'LR
                    Spd -= 0.09
                    If Spd > 1.5 Then Spd = 1.5
                End If
            End If
            Spd += DSpd
            X += Spd
            '落地检测
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            If Y <= Val(Form1.TY1.Text) AndAlso X <= Form1.TYW.Value - 3.1 Then
                '落地转换为跑步
                Y = Val(Form1.TY1.Text)
                Spy = 0
                IsJump = False
                'LAJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                LAJump += MRun(X, Spd, Ori, Acc, Frame - i, Y, Spy, False)
                Exit Function
            Else
                'LAJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If
        Next
    End Function
    Public Function TJump(ByRef X As Single, ByRef Spd As Single, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer, DSPD As Single
        DSPD = Spd / 2
        Select Case Spd
            Case 0
                Spd = 0
            Case Is > 0
                Spd = 0.2
            Case Else
                Spd = -0.2
        End Select
        TJump = ""
        '弹簧跳前置8帧，未加Y速度
        For i = 1 To 8
            X += Spd
            'TJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
        Spd = DSPD
    End Function
    Public Function Jump(ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        '有怠速转换
        Jump = ""
        For i = 1 To Frame
            If Not IsJump And i = 1 Then '未起跳，第一帧初始化Y轴计算
                IsJump = True
                Select Case Math.Abs(Spd)
                    Case Is >= 2.8
                        Spy = 3.868
                    Case Is >= 1.5
                        Spy = 3.808
                    Case Is >= 0.7
                        Spy = 3.748
                    Case Else
                        Spy = 3.568
                End Select
            Else
                '滞空
                Select Case Spy
                    Case Is <= -3
                        Spy -= 0.34
                    Case Is <= -0.12
                        Spy -= 0.31
                    Case Is <= 0.3
                        Spy -= 0.08
                    Case Is <= 1.5
                        Spy -= 0.34
                    Case Is <= 2.5
                        Spy -= 0.25
                    Case Else
                        Spy -= 0.06
                End Select
                If Spy < -4 Then Spy = -4 '最小速度-4
            End If
            Y += Spy
            X += Spd
            '落地检测
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            If Y <= Val(Form1.TY1.Text) AndAlso X <= Form1.TYW.Value - 3.1 Then
                '落地转换为跑步
                Y = Val(Form1.TY1.Text)
                Spy = 0
                IsJump = False
                'Jump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                Exit Function
            Else
                'Jump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If

        Next
    End Function
    Public Function DonutJump(ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        '测试，只有X速度
        Spd = 0.5 + Spd * 0.6
        X += Spd
        AddFrameData(X, Y, Spd, Spy)
        DonutJump = ""
    End Function
    Public Function MXJump(ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        '有怠速转换
        MXJump = ""
        For i = 1 To Frame
            If Not IsJump And i = 1 Then '未起跳，第一帧初始化Y轴计算
                IsJump = True
                Spy = 3.168 '固定加速度
            Else
                '滞空
                Select Case Spy
                    Case Is <= -3
                        Spy -= 0.34
                    Case Is <= -0.12
                        Spy -= 0.31
                    Case Is <= 0.3
                        Spy -= 0.08
                    Case Is <= 1.5
                        Spy -= 0.34
                    Case Is <= 2.5
                        Spy -= 0.25
                    Case Else
                        Spy -= 0.06
                End Select
                If Spy < -4 Then Spy = -4 '最小速度-4
            End If
            Y += Spy
            X += Spd
            '落地检测
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            If Y <= Val(Form1.TY1.Text) AndAlso X <= Form1.TYW.Value - 3.1 Then
                '落地转换为跑步，未更新
                Y = Val(Form1.TY1.Text)
                Spy = 0
                IsJump = False
                AddFrameData(X, Y, Spd, Spy)

                Exit Function
            Else
                AddFrameData(X, Y, Spd, Spy)
            End If

        Next
    End Function
    Public Function BJump(ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        BJump = ""
        For i = 1 To Frame
            If Not IsJump And i = 1 Then '未起跳，第一帧初始化Y轴计算
                IsJump = True
                Select Case Math.Abs(Spd)
                    Case Is >= 2.8
                        Spy = 3.588
                    Case Is >= 1.5
                        Spy = 3.528
                    Case Is >= 0.7
                        Spy = 3.468
                    Case Else
                        Spy = 3.288
                End Select
            Else
                '滞空
                Select Case Spy
                    Case Is <= -3
                        Spy -= 0.34
                    Case Is <= -0.12
                        Spy -= 0.31
                    Case Is <= 0.3
                        Spy -= 0.08
                    Case Is <= 1.5
                        Spy -= 0.34
                    Case Is <= 2.5
                        Spy -= 0.25
                    Case Else
                        Spy -= 0.06
                End Select
                If Spy < -4 Then Spy = -4 '最小速度-4
            End If
            Y += Spy
            X += Spd
            '落地检测
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            If Y <= Val(Form1.TY1.Text) AndAlso X <= Form1.TYW.Value - 3.1 Then
                '落地转换为跑步
                Y = Val(Form1.TY1.Text)
                Spy = 0
                IsJump = False
                'BJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                Exit Function
            Else
                'BJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If
        Next
    End Function

    Public Function MSJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean, IsBuffer As Boolean) As String
        Dim i As Integer
        '有怠速转换
        MSJump = ""

        For i = 1 To Frame
            Spd -= DSpd
            If Not IsJump And i = 1 Then '未起跳，第一帧初始化Y轴计算
                IsJump = True
                Select Case Math.Abs(Spd)
                    Case Is >= 2.8
                        Spy = If(IsBuffer, 3.588, 3.868)
                    Case Is >= 1.5
                        Spy = If(IsBuffer, 3.528, 3.808)
                    Case Is >= 0.7
                        Spy = If(IsBuffer, 3.468, 3.748)
                    Case Else
                        Spy = If(IsBuffer, 3.288, 3.568)
                End Select

            Else
                '滞空
                Select Case Spy
                    Case Is <= -3
                        Spy -= 0.34
                    Case Is <= -0.12
                        Spy -= 0.31
                    Case Is <= 0.3
                        Spy -= 0.08
                    Case Is <= 1.5
                        Spy -= 0.34
                    Case Is <= 2.5
                        Spy -= 0.25
                    Case Else
                        Spy -= 0.06
                End Select
                If Spy < -4 Then Spy = -4 '最小速度-4
            End If
            Y += Spy


            If Ori Then
                If Spd >= 0 Then
                    If Acc Then
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.11
                            Case Is <= 2
                                Spd += 0.078
                            Case Is <= 3.25
                                Spd += 0.039
                            Case Else
                                Spd += 0.027
                        End Select
                        If Spd > 4 Then Spd = 4
                    Else
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.11
                            Case Else
                                Spd += 0.04
                        End Select
                        If Spd > 2 Then Spd = 2
                    End If
                Else
                    Spd += 0.12
                End If
            Else
                If Spd <= 0 Then
                    If Acc Then
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.11
                            Case Is >= -2
                                Spd -= 0.078
                            Case Is >= -3.25
                                Spd -= 0.039
                            Case Else
                                Spd -= 0.027
                        End Select
                        If Spd < -4 Then Spd = -4
                    Else
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.11
                            Case Else
                                Spd -= 0.04
                        End Select
                        If Spd < -2 Then Spd = -2
                    End If
                Else
                    Spd -= 0.12
                End If
            End If
            Spd += DSpd
            X += Spd
            '落地检测
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            If Y <= Val(Form1.TY1.Text) AndAlso X <= Form1.TYW.Value - 3.1 Then
                '落地转换为跑步
                Y = Val(Form1.TY1.Text)
                Spy = 0
                IsJump = False
                'MSJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                MSJump += MSRun(X, Spd, Ori, Acc, Frame - i, Y, Spy, False)
                Exit Function
            Else
                'MSJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If
        Next
    End Function

    Public Function MJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean, IsBuffer As Boolean) As String
        Dim i As Integer
        '有怠速转换
        MJump = ""

        For i = 1 To Frame
            Spd -= DSpd
            If Not IsJump And i = 1 Then '未起跳，第一帧初始化Y轴计算
                IsJump = True
                Select Case Math.Abs(Spd)
                    Case Is >= 2.8
                        Spy = If(IsBuffer, 3.588, 3.868)
                    Case Is >= 1.5
                        Spy = If(IsBuffer, 3.528, 3.808)
                    Case Is >= 0.7
                        Spy = If(IsBuffer, 3.468, 3.748)
                    Case Else
                        Spy = If(IsBuffer, 3.288, 3.568)
                End Select
            Else
                '滞空
                Select Case Spy
                    Case Is <= -3
                        Spy -= 0.34
                    Case Is <= -0.12
                        Spy -= 0.31
                    Case Is <= 0.3
                        Spy -= 0.08
                    Case Is <= 1.5
                        Spy -= 0.34
                    Case Is <= 2.5
                        Spy -= 0.25
                    Case Else
                        Spy -= 0.06
                End Select
                If Spy < -4 Then Spy = -4 '最小速度-4
            End If
            Y += Spy
            If Ori Then
                If Spd >= 0 Then
                    If Acc Then
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.08
                            Case Is <= 1.5
                                Spd += 0.06
                            Case Is <= 2.25
                                Spd += 0.029
                            Case Else
                                Spd += 0.021
                        End Select
                        If Spd > 3 Then Spd = 3
                    Else
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.08
                            Case Else
                                Spd += 0.03
                        End Select
                        If Spd > 1.5 Then Spd = 1.5
                    End If
                Else
                    Spd += 0.09
                End If
            Else
                If Spd <= 0 Then
                    If Acc Then
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.08
                            Case Is >= -1.5
                                Spd -= 0.06
                            Case Is >= -2.25
                                Spd -= 0.029
                            Case Else
                                Spd -= 0.021
                        End Select
                        If Spd < -3 Then Spd = -3
                    Else
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.08
                            Case Else
                                Spd -= 0.03
                        End Select
                        If Spd < -1.5 Then Spd = -1.5
                    End If
                Else
                    Spd -= 0.09
                End If
            End If
            Spd += DSpd
            X += Spd
            '碰撞检测 落地检测
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            If Y <= Val(Form1.TY1.Text) AndAlso X <= Form1.TYW.Value - 3.1 Then
                '落地转换为跑步
                Y = Val(Form1.TY1.Text)
                Spy = 0
                IsJump = False
                'MJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                MJump += MRun(X, Spd, Ori, Acc, Frame - i, Y, Spy, False)
                Exit Function
            Else
                'MJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If
        Next
    End Function

    Public Function MSpinJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean, IsBuffer As Boolean) As String
        Dim i As Integer
        '有怠速转换
        MSpinJump = ""

        For i = 1 To Frame
            Spd -= DSpd
            If Not IsJump And i = 1 Then '未起跳，第一帧初始化Y轴计算
                IsJump = True
                Spy = 3.168
            Else
                '滞空
                Select Case Spy
                    Case Is <= -3
                        Spy -= 0.34
                    Case Is <= -0.12
                        Spy -= 0.31
                    Case Is <= 0.3
                        Spy -= 0.08
                    Case Is <= 1.5
                        Spy -= 0.34
                    Case Is <= 2.5
                        Spy -= 0.25
                    Case Else
                        Spy -= 0.06
                End Select
                If Spy < -4 Then Spy = -4 '最小速度-4
            End If
            Y += Spy


            If Ori Then
                If Spd >= 0 Then
                    If Acc Then
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.08
                            Case Is <= 1.5
                                Spd += 0.06
                            Case Is <= 2.25
                                Spd += 0.029
                            Case Else
                                Spd += 0.021
                        End Select
                        If Spd > 3 Then Spd = 3
                    Else
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.08
                            Case Else
                                Spd += 0.03
                        End Select
                        If Spd > 1.5 Then Spd = 1.5
                    End If
                Else
                    Spd += 0.09
                End If
            Else
                If Spd <= 0 Then
                    If Acc Then
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.08
                            Case Is >= -1.5
                                Spd -= 0.06
                            Case Is >= -2.25
                                Spd -= 0.029
                            Case Else
                                Spd -= 0.021
                        End Select
                        If Spd < -3 Then Spd = -3
                    Else
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.08
                            Case Else
                                Spd -= 0.03
                        End Select
                        If Spd < -1.5 Then Spd = -1.5
                    End If
                Else
                    Spd -= 0.09
                End If
            End If
            Spd += DSpd
            X += Spd
            '落地检测
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            If Y <= Val(Form1.TY1.Text) AndAlso X <= Form1.TYW.Value - 3.1 Then
                '落地转换为跑步
                Y = Val(Form1.TY1.Text)
                Spy = 0
                IsJump = False
                'MSpinJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                MSpinJump += MRun(X, Spd, Ori, Acc, Frame - i, Y, Spy, False)
                Exit Function
            Else
                'MSpinJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If
        Next
    End Function

    Public Function SpinJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        Dim SSpd As Single
        '有怠速转换
        SpinJump = ""
        '默认11帧，未考虑落地
        Dim RF, LF As Integer
        RF = Math.Max(Frame, 1)
        RF = Math.Min(RF, 11)
        LF = 11 - RF
        For i = 1 To 11

            '旋转
            If Spy >= 0 Then
                '正常下落
                Select Case Spy
                    Case Is <= -3
                        Spy -= 0.34
                    Case Is <= -0.12
                        Spy -= 0.31
                    Case Is <= 0.3
                        Spy -= 0.08
                    Case Is <= 1.5
                        Spy -= 0.34
                    Case Is <= 2.5
                        Spy -= 0.25
                    Case Else
                        Spy -= 0.06
                End Select
            Else
                '速度小于0，获得反向15%加速度
                SSpd = -0.15 * Spy
                '加速度最小0.1
                If SSpd < 0.1 Then SSpd = 0.1
                '加速不能反向，速度最高变为0
                Spy += SSpd
                If Spy > 0 Then Spy = 0
            End If

            If Spy < -4 Then Spy = -4 '最小速度-4
            Y += Spy
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            Spd -= DSpd
            If Ori Then
                If Spd >= 0 Then
                    If Acc Then
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.08
                            Case Is <= 1.5
                                Spd += 0.06
                            Case Is <= 2.25
                                Spd += 0.029
                            Case Else
                                Spd += 0.021
                        End Select
                        If Spd > 3 Then Spd = 3
                    Else
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.08
                            Case Else
                                Spd += 0.03
                        End Select
                        If Spd > 1.5 Then Spd = 1.5
                    End If
                Else
                    Spd += 0.09
                End If
            Else
                If Spd <= 0 Then
                    If Acc Then
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.08
                            Case Is >= -1.5
                                Spd -= 0.06
                            Case Is >= -2.25
                                Spd -= 0.029
                            Case Else
                                Spd -= 0.021
                        End Select
                        If Spd < -3 Then Spd = -3
                    Else
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.08
                            Case Else
                                Spd -= 0.03
                        End Select
                        If Spd < -1.5 Then Spd = -1.5
                    End If
                Else
                    Spd -= 0.09
                End If
            End If

            Spd += DSpd
            X += Spd
            'SpinJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)

            If i = RF Then '反向
                Ori = Not Ori
            End If

        Next
    End Function
    Public Function MAJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        '有怠速转换
        MAJump = ""

        For i = 1 To Frame
            Spd -= DSpd
            '不滞空
            Select Case Spy
                Case Is <= -3
                    Spy -= 0.34
                Case Is <= -0.12
                    Spy -= 0.34
                Case Is <= 0.3
                    Spy -= 0.25
                Case Is <= 1.5
                    Spy -= 0.34
                Case Is <= 2.5
                    Spy -= 0.34
                Case Else
                    Spy -= 0.34
            End Select
            If Spy < -4 Then Spy = -4 '最小速度-4
            Y += Spy


            If Ori Then
                If Spd >= 0 Then
                    If Acc Then
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.08
                            Case Is <= 1.5
                                Spd += 0.06
                            Case Is <= 2.25
                                Spd += 0.029
                            Case Else
                                Spd += 0.021
                        End Select
                        If Spd > 3 Then Spd = 3
                    Else
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.08
                            Case Else
                                Spd += 0.03
                        End Select
                        If Spd > 1.5 Then Spd = 1.5
                    End If
                Else
                    Spd += 0.09
                End If
            Else
                If Spd <= 0 Then
                    If Acc Then
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.08
                            Case Is >= -1.5
                                Spd -= 0.06
                            Case Is >= -2.25
                                Spd -= 0.029
                            Case Else
                                Spd -= 0.021
                        End Select
                        If Spd < -3 Then Spd = -3
                    Else
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.08
                            Case Else
                                Spd -= 0.03
                        End Select
                        If Spd < -1.5 Then Spd = -1.5
                    End If
                Else
                    Spd -= 0.09
                End If
            End If
            Spd += DSpd
            X += Spd
            '落地检测
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            If Y <= Val(Form1.TY1.Text) AndAlso X <= Form1.TYW.Value - 3.1 Then
                '落地转换为跑步
                Y = Val(Form1.TY1.Text)
                Spy = 0
                IsJump = False
                'MAJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                MAJump += MRun(X, Spd, Ori, Acc, Frame - i, Y, Spy, False)
                Exit Function
            Else
                'MAJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If
        Next
    End Function

    Public Function MSAJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        '有怠速转换
        MSAJump = ""

        For i = 1 To Frame
            Spd -= DSpd
            '不滞空
            Select Case Spy
                Case Is <= -3
                    Spy -= 0.34
                Case Is <= -0.12
                    Spy -= 0.34
                Case Is <= 0.3
                    Spy -= 0.25
                Case Is <= 1.5
                    Spy -= 0.34
                Case Is <= 2.5
                    Spy -= 0.34
                Case Else
                    Spy -= 0.34
            End Select
            If Spy < -4 Then Spy = -4 '最小速度-4
            Y += Spy


            If Ori Then
                If Spd >= 0 Then
                    If Acc Then
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.11
                            Case Is <= 2
                                Spd += 0.078
                            Case Is <= 3.25
                                Spd += 0.039
                            Case Else
                                Spd += 0.027
                        End Select
                        If Spd > 4 Then Spd = 4
                    Else
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.11
                            Case Else
                                Spd += 0.04
                        End Select
                        If Spd > 2 Then Spd = 2
                    End If
                Else
                    Spd += 0.12
                End If
            Else
                If Spd <= 0 Then
                    If Acc Then
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.11
                            Case Is >= -2
                                Spd -= 0.078
                            Case Is >= -3.25
                                Spd -= 0.039
                            Case Else
                                Spd -= 0.027
                        End Select
                        If Spd < -4 Then Spd = -4
                    Else
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.11
                            Case Else
                                Spd -= 0.04
                        End Select
                        If Spd < -2 Then Spd = -2
                    End If
                Else
                    Spd -= 0.12
                End If
            End If
            Spd += DSpd
            X += Spd
            '落地检测
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            If Y <= Val(Form1.TY1.Text) AndAlso X <= Form1.TYW.Value - 3.1 Then
                '落地转换为跑步
                Y = Val(Form1.TY1.Text)
                Spy = 0
                IsJump = False
                'MSAJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
                MSAJump += MRun(X, Spd, Ori, Acc, Frame - i, Y, Spy, False)
                Exit Function
            Else
                'MSAJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                AddFrameData(X, Y, Spd, Spy)
            End If
        Next
    End Function

    Public WSpd As Single, WFrame As Integer
    Public DSpd As Single
    Public Function WAir(ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        WAir = ""
        For i = 1 To Frame
            Spd -= WSpd
            If WFrame < 10 Then
                WSpd += -0.1
                WFrame += 1
            End If
            Spd += WSpd
            X += Spd
            'WAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
    End Function

    Public Function WJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        WJump = ""
        For I = 1 To Frame
            Spd -= WSpd
            If WFrame < 10 Then
                WSpd += -0.1
                WFrame += 1
            End If
            If Ori Then
                If Spd >= 0 Then
                    If Acc Then
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.08
                            Case Is <= 1.5
                                Spd += 0.06
                            Case Is <= 2.25
                                Spd += 0.029
                            Case Else
                                Spd += 0.021
                        End Select
                        If Spd > 3 Then Spd = 3
                    Else
                        Select Case Spd
                            Case Is <= 0.5
                                Spd += 0.08
                            Case Else
                                Spd += 0.03
                        End Select
                        If Spd > 1.5 Then Spd = 1.5
                    End If
                Else
                    Spd += 0.09
                End If
            Else
                If Spd <= 0 Then
                    If Acc Then
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.08
                            Case Is >= -1.5
                                Spd -= 0.06
                            Case Is >= -2.25
                                Spd -= 0.029
                            Case Else
                                Spd -= 0.021
                        End Select
                        If Spd < -3 Then Spd = -3
                    Else
                        Select Case Spd
                            Case Is >= -0.5
                                Spd -= 0.08
                            Case Else
                                Spd -= 0.03
                        End Select
                        If Spd < -1.5 Then Spd = -1.5
                    End If
                Else
                    Spd -= 0.09
                End If
            End If
            Spd += WSpd
            X += Spd
            '测试
            If HitBlk AndAlso Y >= HitBlkLoc Then
                Y = HitBlkLoc
                Spy = 0
            End If
            '============
            'WJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            AddFrameData(X, Y, Spd, Spy)
        Next
    End Function

    Public Function Sng2Hex(s As Single) As String
        Dim a() As Byte
        a = BitConverter.GetBytes(s)
        Sng2Hex = Strings.Right("00" & Hex(a(3)), 2) & Strings.Right("00" & Hex(a(2)), 2) &
                  Strings.Right("00" & Hex(a(1)), 2) & Strings.Right("00" & Hex(a(0)), 2)
    End Function

    Public Function Hex2Bin(H As String, ByRef s() As Byte) As String
        Dim I As Long
        For I = 0 To 7
            Select Case Mid(H, I + 1, 1)
                Case "0"
                Case "1"
                    s((I * 4) + 3) = 1
                Case "2"
                    s((I * 4) + 2) = 1
                Case "3"
                    s((I * 4) + 3) = 1
                    s((I * 4) + 2) = 1
                Case "4"
                    s((I * 4) + 1) = 1
                Case "5"
                    s((I * 4) + 3) = 1
                    s((I * 4) + 1) = 1
                Case "6"
                    s((I * 4) + 2) = 1
                    s((I * 4) + 1) = 1
                Case "7"
                    s((I * 4) + 3) = 1
                    s((I * 4) + 2) = 1
                    s((I * 4) + 1) = 1
                Case "8"
                    s((I * 4) + 0) = 1
                Case "9"
                    s((I * 4) + 0) = 1
                    s((I * 4) + 3) = 1
                Case "A"
                    s((I * 4) + 0) = 1
                    s((I * 4) + 2) = 1
                Case "B"
                    s((I * 4) + 0) = 1
                    s((I * 4) + 3) = 1
                    s((I * 4) + 2) = 1
                Case "C"
                    s((I * 4) + 0) = 1
                    s((I * 4) + 1) = 1
                Case "D"
                    s((I * 4) + 0) = 1
                    s((I * 4) + 3) = 1
                    s((I * 4) + 1) = 1
                Case "E"
                    s((I * 4) + 0) = 1
                    s((I * 4) + 2) = 1
                    s((I * 4) + 1) = 1
                Case "F"
                    s((I * 4) + 0) = 1
                    s((I * 4) + 3) = 1
                    s((I * 4) + 2) = 1
                    s((I * 4) + 1) = 1
            End Select
        Next
        Hex2Bin = ""
        For I = 0 To 31
            Hex2Bin &= CStr(s(I))
        Next
    End Function

    Public Function Bin2Dbl(P() As Byte) As Double
        '0 S符号位 0+ 1-
        '1~8 E指数位
        '9-31 X小数位
        'F=(-1)^S*(1+X)*2^(E-127)
        Dim s As Long, E As Long, x As Double
        Dim I As Long
        For I = 1 To 31
            s += P(I)
        Next
        If s = 0 Then
            Bin2Dbl = IIf(P(0) = 0, 0, -1)
            Exit Function
        End If
        s = P(0)
        For I = 1 To 8
            E += P(I) * (2 ^ (8 - I))
        Next
        For I = 1 To 23
            x += P(I + 8) * (2 ^ (-I))
        Next
        Bin2Dbl = ((-1) ^ s) * (1 + x) * (2 ^ (E - 127))
    End Function

    Public Function Hex2Dbl(H As String) As Double
        '0 S符号位 0+ 1-
        '1~8 E指数位
        '9-31 X小数位
        'F=(-1)^S*(1+X)*2^(E-127)
        Dim P(31) As Byte
        Dim unused = Hex2Bin(H, P)

        Dim s As Long, E As Long, x As Double
        Dim I As Long
        For I = 1 To 31
            s += P(I)
        Next
        If s = 0 Then
            Hex2Dbl = IIf(P(0) = 0, 0, -1)
            Exit Function
        End If
        s = P(0)
        For I = 1 To 8
            E += P(I) * (2 ^ (8 - I))
        Next
        For I = 1 To 23
            x += P(I + 8) * (2 ^ (-I))
        Next
        Hex2Dbl = ((-1) ^ s) * (1 + x) * (2 ^ (E - 127))
    End Function
    Public CmdTrans(,) As String
    Public Function GetCnCmd(cmd As String, Cn As Boolean) As String
        For i As Integer = 0 To CmdTrans.GetUpperBound(0)
            If LCase(cmd) = LCase(CmdTrans(i, 1)) Then
                If Cn Then
                    Return CmdTrans(i, 0)
                Else
                    Return CmdTrans(i, 1)
                End If
            End If
        Next
        Return cmd
    End Function
    Public Sub MarioMove(Cmd As String, ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean)

        Select Case GetCnCmd(Cmd, True) '.Replace("D", "").Replace("+", "")
            Case "左风站立"
                WAir(X, Spd, Frame, Y, Spy, IsJump)
            Case "风右跑跳"    '加速右跳
                WJump(X, Spd, True, True, Frame, Y, Spy, IsJump)
            Case "风左跑跳"    '加速左跳
                WJump(X, Spd, False, True, Frame, Y, Spy, IsJump)
            Case "风右跳"    '右跳
                WJump(X, Spd, True, False, Frame, Y, Spy, IsJump)
            Case "风左跳"    '左跳
                WJump(X, Spd, False, False, Frame, Y, Spy, IsJump)
            Case "加速右跳"    '加速右跳
                MJump(X, Spd, True, True, Frame, Y, Spy, IsJump, False)
            Case "加速左跳"    '加速左跳
                MJump(X, Spd, False, True, Frame, Y, Spy, IsJump, False)
            Case "加速右"   '加速右跳
                MAJump(X, Spd, True, True, Frame, Y, Spy, IsJump)
            Case "加速左" '加速左跳
                MAJump(X, Spd, False, True, Frame, Y, Spy, IsJump)
            Case "右跳"    '右跳
                MJump(X, Spd, True, False, Frame, Y, Spy, IsJump, False)
            Case "左跳"   '左跳
                MJump(X, Spd, False, False, Frame, Y, Spy, IsJump, False)
            Case "右"   '右跳
                MAJump(X, Spd, True, False, Frame, Y, Spy, IsJump)
            Case "左"  '左跳
                MAJump(X, Spd, False, False, Frame, Y, Spy, IsJump)
            Case "左反墙"
                MWallJump(X, Spd, True, False, Frame, Y, Spy, IsJump, False)
            Case "右反墙"
                MWallJump(X, Spd, False, False, Frame, Y, Spy, IsJump, False)
            Case "加速右旋转"    '加速右跳
                SpinJump(X, Spd, True, True, Frame, Y, Spy, IsJump)
            Case "加速左旋转"   '加速左跳
                SpinJump(X, Spd, False, True, Frame, Y, Spy, IsJump)
            Case "右旋转"    '右跳
                SpinJump(X, Spd, True, False, Frame, Y, Spy, IsJump)
            Case "左旋转"    '左跳
                SpinJump(X, Spd, False, False, Frame, Y, Spy, IsJump)
            Case "旋转"    '匀速滞空跳
                SpinAir(X, Spd, 0, Y, Spy, IsJump)

            Case "加速右旋转跳"    '加速右跳
                MSpinJump(X, Spd, True, True, Frame, Y, Spy, IsJump, False)
            Case "加速左旋转跳"    '加速左跳
                MSpinJump(X, Spd, False, True, Frame, Y, Spy, IsJump, False)
            Case "右旋转跳"    '右跳
                MSpinJump(X, Spd, True, False, Frame, Y, Spy, IsJump, False)
            Case "左旋转跳"    '左跳
                MSpinJump(X, Spd, False, False, Frame, Y, Spy, IsJump, False)

            Case "滞空"    '匀速滞空跳
                MAAir(X, Spd, Frame, Y, Spy, IsJump)
            Case "落体"    '匀速落体跳
                MAir(X, Spd, Frame, Y, Spy, IsJump, 0)
            Case "落体站"    '匀速落体跳
                MAir(X, Spd, Frame, Y, Spy, IsJump, 0)
            Case "落体正"    '匀速落体跳
                MAir(X, Spd, Frame, Y, Spy, IsJump, 1)
            Case "落体反"    '匀速落体跳
                MAir(X, Spd, Frame, Y, Spy, IsJump, -1)

            Case "跳" '原地跳
                Jump(X, Spd, Frame, Y, Spy, IsJump)
            Case "甜甜圈跳" '原地跳
                DonutJump(X, Spd, Frame, Y, Spy, IsJump)
            Case "旋转跳" '原地跳
                MXJump(X, Spd, Frame, Y, Spy, IsJump)
            Case "缓冲跳"
                BJump(X, Spd, Frame, Y, Spy, IsJump)
            Case "弹簧跳" '弹簧跳
                TJump(X, Spd, Y, Spy, IsJump)
            Case "低重滞空"    '低重力匀速跳
                LAir(X, Spd, Frame, Y, Spy, IsJump)
            Case "低重落体" '低重力落体
                LAAir(X, Spd, Frame, Y, Spy, IsJump)
            Case "低重跳"    '低重力匀速跳
                LAir(X, Spd, Frame, Y, Spy, IsJump)
            Case "低重缓冲跳"    '低重力匀速跳
                LBAir(X, Spd, Frame, Y, Spy, IsJump)
            Case "风地右跑"    '风地面右跑
                WRun(X, Spd, True, True, Frame, Y, Spy, IsJump)
            Case "风地左跑"    '风地面左跑
                WRun(X, Spd, False, True, Frame, Y, Spy, IsJump)
            Case "风地右走"    '风地面右走
                WRun(X, Spd, True, False, Frame, Y, Spy, IsJump)
            Case "风地左走"    '风地面左走
                WRun(X, Spd, False, False, Frame, Y, Spy, IsJump)
            Case "右跑"    '地面右跑
                MRun(X, Spd, True, True, Frame, Y, Spy, IsJump)
            Case "左跑"    '地面左跑
                MRun(X, Spd, False, True, Frame, Y, Spy, IsJump)
            Case "右走"    '地面右走
                MRun(X, Spd, True, False, Frame, Y, Spy, IsJump)
            Case "左走"    '地面左走
                MRun(X, Spd, False, False, Frame, Y, Spy, IsJump)

            Case "右陡坡右跑"    '地面右跑
                MDSlopeRun(X, Spd, True, True, Frame, Y, Spy, True)
            Case "右陡坡左跑"    '地面左跑
                MDSlopeRun(X, Spd, False, True, Frame, Y, Spy, True)
            Case "右陡坡右走"    '地面右走
                MDSlopeRun(X, Spd, True, False, Frame, Y, Spy, True)
            Case "右陡坡左走"    '地面左走
                MDSlopeRun(X, Spd, False, False, Frame, Y, Spy, True)

            Case "左陡坡右跑"    '地面右跑
                MDSlopeRun(X, Spd, True, True, Frame, Y, Spy, False)
            Case "左陡坡左跑"    '地面左跑
                MDSlopeRun(X, Spd, False, True, Frame, Y, Spy, False)
            Case "左陡坡右走"    '地面右走
                MDSlopeRun(X, Spd, True, False, Frame, Y, Spy, False)
            Case "左陡坡左走"    '地面左走
                MDSlopeRun(X, Spd, False, False, Frame, Y, Spy, False)

            Case "右缓坡右跑"    '地面右跑
                MHSlopeRun(X, Spd, True, True, Frame, Y, Spy, True)
            Case "右缓坡左跑"    '地面左跑
                MHSlopeRun(X, Spd, False, True, Frame, Y, Spy, True)
            Case "右缓坡右走"    '地面右走
                MHSlopeRun(X, Spd, True, False, Frame, Y, Spy, True)
            Case "右缓坡左走"    '地面左走
                MHSlopeRun(X, Spd, False, False, Frame, Y, Spy, True)

            Case "左缓坡右跑"    '地面右跑
                MHSlopeRun(X, Spd, True, True, Frame, Y, Spy, False)
            Case "左缓坡左跑"    '地面左跑
                MHSlopeRun(X, Spd, False, True, Frame, Y, Spy, False)
            Case "左缓坡右走"    '地面右走
                MHSlopeRun(X, Spd, True, False, Frame, Y, Spy, False)
            Case "左缓坡左走"    '地面左走
                MHSlopeRun(X, Spd, False, False, Frame, Y, Spy, False)

            Case "岩浆右走"    '地面右走
                MFireRun(X, Spd, True, False, Frame, Y, Spy, IsJump)
            Case "岩浆左走"    '地面左走
                MFireRun(X, Spd, False, False, Frame, Y, Spy, IsJump)
            Case "岩浆站立"    '地面站立
                MFireDuck(X, Spd, Frame, Y, Spy)
            Case "岩浆站停"    '地面站立
                MFireDuck(X, Spd, 999, Y, Spy)
                    '鞋走路 USA大跳

            Case "站立"    '地面站立
                MDuck(X, Spd, True, True, Frame, Y, Spy, IsJump)
            Case "正蹲"    '地面正蹲
                MDuck(X, Spd, False, True, Frame, Y, Spy, IsJump)
            Case "反蹲"    '地面反蹲
                MDuck(X, Spd, False, False, Frame, Y, Spy, IsJump)
            Case "站停"    '地面站立
                MDuck(X, Spd, True, True, 999, Y, Spy, IsJump)
            Case "正停"    '地面正蹲
                MDuck(X, Spd, False, True, 999, Y, Spy, IsJump)
            Case "反停"    '地面反蹲
                MDuck(X, Spd, False, False, 999, Y, Spy, IsJump)
            Case "冰站立"    '地面站立
                IDuck(X, Spd, True, True, Frame, Y, Spy, IsJump)
            Case "冰正蹲"    '地面正蹲
                IDuck(X, Spd, False, True, Frame, Y, Spy, IsJump)
            Case "冰反蹲"    '地面反蹲
                IDuck(X, Spd, False, False, Frame, Y, Spy, IsJump)
            Case "低重右跳"    '低重力右跳
                LJump(X, Spd, True, False, Frame, Y, Spy, IsJump, False)
            Case "低重左跳"    '低重力左跳
                LJump(X, Spd, False, False, Frame, Y, Spy, IsJump, False)
            Case "低重右"    '低重力右跳
                LAJump(X, Spd, True, False, Frame, Y, Spy, IsJump)
            Case "低重左"    '低重力左跳
                LAJump(X, Spd, False, False, Frame, Y, Spy, IsJump)
            Case "低重加速右跳"    '低重力右跳
                LJump(X, Spd, True, True, Frame, Y, Spy, IsJump, False)
            Case "低重加速左跳"    '低重力左跳
                LJump(X, Spd, False, True, Frame, Y, Spy, IsJump, False)
            Case "低重加速右"    '低重力右跳
                LAJump(X, Spd, True, True, Frame, Y, Spy, IsJump)
            Case "低重加速左"    '低重力左跳
                LAJump(X, Spd, False, True, Frame, Y, Spy, IsJump)
            Case "坐莲" '坐莲
                ZAAir(X, Spd, Frame, Y, Spy, IsJump)
            Case "滞空坐莲" '坐莲
                ZAir(X, Spd, Frame, Y, Spy, IsJump)
            Case "低重坐莲" '坐莲
                LZAAir(X, Spd, Frame, Y, Spy, IsJump)
            Case "低重滞空坐莲" '坐莲
                LZAir(X, Spd, Frame, Y, Spy, IsJump)
            Case "冰右跑"    '冰面右跑
                IRun(X, Spd, True, True, Frame, Y, Spy, IsJump)
            Case "冰右走"    '冰面右跑
                IRun(X, Spd, True, False, Frame, Y, Spy, IsJump)
            Case "冰左跑"    '冰面右跑
                IRun(X, Spd, False, True, Frame, Y, Spy, IsJump)
            Case "冰左走"    '冰面右跑
                IRun(X, Spd, False, False, Frame, Y, Spy, IsJump)

            Case "加速右缓冲跳"    '加速右跳
                MJump(X, Spd, True, True, Frame, Y, Spy, IsJump, True)
            Case "加速左缓冲跳"    '加速左跳
                MJump(X, Spd, False, True, Frame, Y, Spy, IsJump, True)
            Case "右缓冲跳"    '右跳
                MJump(X, Spd, True, False, Frame, Y, Spy, IsJump, True)
            Case "左缓冲跳"    '左跳
                MJump(X, Spd, False, False, Frame, Y, Spy, IsJump, True)
            Case "低重加速右缓冲跳"    '低重力右跳
                LJump(X, Spd, True, True, Frame, Y, Spy, IsJump, True)
            Case "低重加速左缓冲跳"    '低重力左跳
                LJump(X, Spd, False, True, Frame, Y, Spy, IsJump, True)
            Case "低重右缓冲跳"    '低重力右跳
                LJump(X, Spd, True, False, Frame, Y, Spy, IsJump, True)
            Case "低重左缓冲跳"    '低重力左跳
                LJump(X, Spd, False, False, Frame, Y, Spy, IsJump, True)

            Case "水右跳"    '右跳
                WtJump(X, Spd, True, True, Frame, Y, Spy, True, IsJump)
            Case "水左跳"    '左跳
                WtJump(X, Spd, False, True, Frame, Y, Spy, True, IsJump)
            Case "水右"    '右
                WtJump(X, Spd, True, True, Frame, Y, Spy, False, IsJump)
            Case "水左"    '左
                WtJump(X, Spd, False, True, Frame, Y, Spy, False, IsJump)
            Case "水右走"    '地面右走
                WtRun(X, Spd, True, False, Frame, Y, Spy, IsJump)
            Case "水左走"    '地面左走
                WtRun(X, Spd, False, False, Frame, Y, Spy, IsJump)
            Case "水跳"
                WtAir(X, Spd, False, True, Frame, Y, Spy, True, IsJump)
            Case "水落"
                WtAir(X, Spd, False, True, Frame, Y, Spy, False, IsJump)
            Case "水站"    '地面站立
                WtDuck(X, Spd, True, True, Frame, Y, Spy, IsJump)
            Case "水冰右走"    '地面右走
                WtIceRun(X, Spd, True, False, Frame, Y, Spy, IsJump)
            Case "水冰左走"    '地面左走
                WtIceRun(X, Spd, False, False, Frame, Y, Spy, IsJump)
            Case "水冰站"    '地面站立
                WtIceDuck(X, Spd, True, True, Frame, Y, Spy, IsJump)

            Case "无敌加速右跳"    '加速右跳
                MSJump(X, Spd, True, True, Frame, Y, Spy, IsJump, False)
            Case "无敌加速左跳"    '加速左跳
                MSJump(X, Spd, False, True, Frame, Y, Spy, IsJump, False)
            Case "无敌加速右"    '加速右跳
                MSAJump(X, Spd, True, True, Frame, Y, Spy, IsJump)
            Case "无敌加速左"    '加速左跳
                MSAJump(X, Spd, False, True, Frame, Y, Spy, IsJump)
            Case "无敌右跳"    '右跳
                MSJump(X, Spd, True, False, Frame, Y, Spy, IsJump, False)
            Case "无敌左跳"    '左跳
                MSJump(X, Spd, False, False, Frame, Y, Spy, IsJump, False)
            Case "无敌右"    '右跳
                MSAJump(X, Spd, True, False, Frame, Y, Spy, IsJump)
            Case "无敌左"    '左跳
                MSAJump(X, Spd, False, False, Frame, Y, Spy, IsJump)
            Case "无敌右跑"    '地面右跑
                MSRun(X, Spd, True, True, Frame, Y, Spy, IsJump)
            Case "无敌左跑"    '地面左跑
                MSRun(X, Spd, False, True, Frame, Y, Spy, IsJump)
            Case "无敌右走"    '地面右走
                MSRun(X, Spd, True, False, Frame, Y, Spy, IsJump)
            Case "无敌左走"    '地面左走
                MSRun(X, Spd, False, False, Frame, Y, Spy, IsJump)
            Case "无敌站立"    '地面站立
                MSDuck(X, Spd, True, True, Frame, Y, Spy, IsJump)
            Case "无敌正蹲"    '地面正蹲
                MSDuck(X, Spd, False, True, Frame, Y, Spy, IsJump)
            Case "无敌反蹲"    '地面反蹲
                MSDuck(X, Spd, False, False, Frame, Y, Spy, IsJump)
            Case "无敌站停"    '地面站立
                MSDuck(X, Spd, True, True, 999, Y, Spy, IsJump)
            Case "无敌正停"    '地面正蹲
                MSDuck(X, Spd, False, True, 999, Y, Spy, IsJump)
            Case "无敌反停"    '地面反蹲
                MSDuck(X, Spd, False, False, 999, Y, Spy, IsJump)
        End Select
    End Sub
End Module
