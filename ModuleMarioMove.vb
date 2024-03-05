Friend Module ModuleMarioMove
    Public Function MAAir(ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        MAAir = ""
        For i = 1 To Frame
            Select Case Spy
                Case Is <= -3
                    Spy -= 0.34
                Case Is <= -0.15
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
            If Y <= 0 Then
                '落地转换为跑步
                Y = 0
                Spy = 0
                IsJump = False
                MAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                If i < Frame Then
                    '落地一帧延长
                    X += Spd
                    MAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                    If Frame - i - 1 > 0 Then '还有剩余帧则计算地面状态
                        MAAir += MDuck(X, Spd, True, True, Frame - i - 1, Y, Spy, False)
                    End If
                    Exit Function
                End If
            Else
                MAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
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
                    Case Is <= -0.15
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
            SpinAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
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
                Case Is <= -0.15
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
            If Y <= 0 Then
                '落地转换为跑步
                Y = 0
                Spy = 0
                IsJump = False
                MAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                If i < Frame Then '=================未测试完成===============
                    '落地一帧延长
                    X += Spd
                    MAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
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
                MAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
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
                Case Is <= -0.15
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
            ZAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
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
                ZAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                Exit Function
            Else
                ZAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
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
                Case Is <= -0.15
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

            ZAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
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
                ZAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                Exit Function
            Else
                ZAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            End If
        Next
    End Function
    Public Function LZAir(ByRef X As Single, ByRef Spd As Single, FRAME As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        LZAir = ""
        For i = 1 To 4
            '滞空
            Select Case Spy
                Case Is <= -0.15
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
            LZAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
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
                LZAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                Exit Function
            Else
                LZAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            End If
        Next
    End Function
    Public Function LZAAir(ByRef X As Single, ByRef Spd As Single, FRAME As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim i As Integer
        LZAAir = ""
        For i = 1 To 4
            '不滞空
            Select Case Spy
                Case Is <= -0.15
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
            LZAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
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
                LZAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                Exit Function
            Else
                LZAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
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
                Case Is <= -0.15
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
            If Y <= 0 Then
                '落地转换为跑步
                Y = 0
                Spy = 0
                IsJump = False
                LAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                LAAir += MDuck(X, Spd, True, True, Frame - i, Y, Spy, False)
                Exit Function
            Else
                LAAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
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
                    Case Is <= -0.15
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
            If Y <= 0 Then
                '落地转换为跑步
                Y = 0
                Spy = 0
                IsJump = False
                LAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                LAir += MDuck(X, Spd, True, True, Frame - i, Y, Spy, False)
                Exit Function
            Else
                LAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
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
                    Case Is <= -0.15
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
            LBAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
        Next
    End Function
    Public Function IRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        IRun = ""
        '没加快跑刹车判断
        For I = 1 To Frame
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
            IRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
        Next
    End Function
    Public Function MRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        MRun = ""
        '没加快跑刹车判断
        For I = 1 To Frame
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
            MRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
        Next
    End Function
    Public Function MDSlopeRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef UPDOWN As Boolean) As String
        Dim I As Integer
        MDSlopeRun = ""
        '陡坡跑步
        For I = 1 To Frame
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
            MDSlopeRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf

        Next
    End Function
    Public Function MHSlopeRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef UPDOWN As Boolean) As String
        Dim I As Integer
        MHSlopeRun = ""
        '陡坡跑步
        For I = 1 To Frame
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
            MHSlopeRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
        Next
    End Function
    Public Function MSRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        MSRun = ""
        '没加快跑刹车判断
        For I = 1 To Frame
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
            MSRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
        Next
    End Function
    Public Function WtRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        WtRun = ""
        '水走
        For I = 1 To Frame
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
            WtRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
        Next
    End Function
    Public Function WtIceRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        WtIceRun = ""
        '水冰走
        For I = 1 To Frame
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
            WtIceRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
        Next
    End Function
    Public Function WtJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef AirJump As Boolean, ByRef IsJump As Boolean) As String
        Dim I As Integer
        '有怠速转换
        WtJump = ""

        For I = 1 To Frame

            If I = 1 Then '第一帧初始化Y轴计算
                If Not IsJump Then
                    '未起跳，地面跳
                    Spy = 1.25
                    IsJump = True
                Else
                    '已起跳
                    If AirJump Then '水中跳
                        Spy += 1
                    Else '水中落体
                        Select Case Spy
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
                    Case Is >= 0
                        Spy -= 0.043
                    Case Else
                        Spy -= 0.027
                End Select
                If Spy < -1.5 Then Spy = -1.5 '最小速度
            End If
            Y += Spy

            Spd -= DSpd
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
            '水中未加落地检测
            'If Y <= 0 Then
            '    '落地转换为跑步
            '    Y = 0
            '    Spy = 0
            '    IsJump = False
            '    WtJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            '    WtJump += WtRun(X, Spd, Ori, Acc, Frame - I, Y, Spy, False)
            '    Exit Function
            'Else
            WtJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            'End If
        Next
    End Function
    Public Function WtAir(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef AirJump As Boolean, ByRef IsJump As Boolean) As String
        Dim I As Integer
        '有怠速转换
        WtAir = ""
        '水中自由落体减速
        For I = 1 To Frame

            If I = 1 Then '第一帧初始化Y轴计算
                If Not IsJump Then
                    '未起跳，地面跳
                    Spy = 1.25
                    IsJump = True
                Else
                    '已起跳
                    If AirJump Then '水中跳
                        Spy += 1
                    Else '水中落体
                        Select Case Spy
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
            '水中未加落地检测
            'If Y <= 0 Then
            '    '落地转换为跑步
            '    Y = 0
            '    Spy = 0
            '    IsJump = False
            '    WtJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            '    WtJump += WtRun(X, Spd, Ori, Acc, Frame - I, Y, Spy, False)
            '    Exit Function
            'Else
            WtAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            'End If
        Next
    End Function
    Public Function WtDuck(ByRef X As Single, ByRef Spd As Single, ST As Boolean, ORI As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        WtDuck = ""
        For I = 1 To Frame
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
            WtDuck += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            If Spd = 0 Then Exit For
        Next
    End Function
    Public Function WtIceDuck(ByRef X As Single, ByRef Spd As Single, ST As Boolean, ORI As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        WtIceDuck = ""
        For I = 1 To Frame
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
            WtIceDuck += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            If Spd = 0 Then Exit For
        Next
    End Function
    Public Function WRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        WRun = ""
        '没加快跑刹车判断
        For I = 1 To Frame
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
            WRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
        Next
    End Function
    Public Function MDuck(ByRef X As Single, ByRef Spd As Single, ST As Boolean, ORI As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        MDuck = ""
        For I = 1 To Frame
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
            MDuck += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            If Spd = 0 Then Exit For
        Next

    End Function
    Public Function MSDuck(ByRef X As Single, ByRef Spd As Single, ST As Boolean, ORI As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        MSDuck = ""
        For I = 1 To Frame
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
            MSDuck += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            If Spd = 0 Then Exit For
        Next

    End Function
    Public Function MFireRun(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        MFireRun = ""
        '没加快跑刹车判断
        For I = 1 To Frame
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
            MFireRun += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
        Next
    End Function
    Public Function MWallJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean, IsBuffer As Boolean) As String
        Dim I As Integer
        '有怠速转换
        MWallJump = ""

        For I = 1 To Frame

            If Not IsJump And I = 1 Then '未起跳，第一帧初始化Y轴计算
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
                    Case Is <= -0.15
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
            If Y <= 0 Then
                '落地转换为跑步
                Y = 0
                Spy = 0
                IsJump = False
                MWallJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                MWallJump += MRun(X, Spd, Ori, Acc, Frame - I, Y, Spy, False)
                Exit Function
            Else
                MWallJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            End If
        Next
    End Function
    Public Function MFireDuck(ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single) As String
        Dim I As Integer
        MFireDuck = ""
        For I = 1 To Frame
            If Spd > 0 Then
                Spd -= 0.02
                If Spd < 0 Then Spd = 0
            Else
                Spd += 0.02
                If Spd > 0 Then Spd = 0
            End If
            X += Spd
            MFireDuck += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            If Spd = 0 Then Exit For
        Next

    End Function
    Public Function IDuck(ByRef X As Single, ByRef Spd As Single, ST As Boolean, ORI As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        IDuck = ""
        For I = 1 To Frame
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
            IDuck += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            If Spd = 0 Then Exit For
        Next
    End Function
    Public Function LJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean, IsBuffer As Boolean) As String
        Dim I As Integer
        '有怠速转换
        LJump = ""
        For I = 1 To Frame

            If Not IsJump And I = 1 Then '未起跳，第一帧初始化Y轴计算
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
                    Case Is <= -0.15
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

            Spd -= DSpd
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
            If Y <= 0 Then
                '落地转换为跑步
                Y = 0
                Spy = 0
                IsJump = False
                LJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                LJump += MRun(X, Spd, Ori, Acc, Frame - I, Y, Spy, False)
                Exit Function
            Else
                LJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            End If
        Next
    End Function
    Public Function LAJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        '有怠速转换
        LAJump = ""
        For I = 1 To Frame
            '不滞空
            Select Case Spy
                Case Is <= -0.15
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

            Spd -= DSpd
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
            If Y <= 0 Then
                '落地转换为跑步
                Y = 0
                Spy = 0
                IsJump = False
                LAJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                LAJump += MRun(X, Spd, Ori, Acc, Frame - I, Y, Spy, False)
                Exit Function
            Else
                LAJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            End If
        Next
    End Function
    Public Function TJump(ByRef X As Single, ByRef Spd As Single, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer, DSPD As Single
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
        For I = 1 To 8
            X += Spd
            TJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
        Next
        Spd = DSPD
    End Function
    Public Function Jump(ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        '有怠速转换
        Jump = ""
        For I = 1 To Frame
            If Not IsJump And I = 1 Then '未起跳，第一帧初始化Y轴计算
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
                    Case Is <= -0.15
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
            If Y <= 0 Then
                '落地转换为跑步
                Y = 0
                Spy = 0
                IsJump = False
                Jump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                Exit Function
            Else
                Jump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            End If

        Next
    End Function
    Public Function BJump(ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        '有怠速转换
        BJump = ""
        For I = 1 To Frame
            If Not IsJump And I = 1 Then '未起跳，第一帧初始化Y轴计算
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
                    Case Is <= -0.15
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
            If Y <= 0 Then
                '落地转换为跑步
                Y = 0
                Spy = 0
                IsJump = False
                BJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                Exit Function
            Else
                BJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            End If
        Next
    End Function

    'Public Function AJump(ByRef X As Single, ByRef Spd As Single, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
    '    Dim I As Integer
    '    AJump = ""
    '    '有怠速转换
    '    For I = 1 To Frame
    '        Select Case Spy
    '            Case Is <= -3
    '                Spy -= 0.34
    '            Case Is <= -0.15
    '                Spy -= 0.34
    '            Case Is <= 0.3
    '                Spy -= 0.25
    '            Case Is <= 1.5
    '                Spy -= 0.34
    '            Case Is <= 2.5
    '                Spy -= 0.34
    '            Case Else
    '                Spy -= 0.34
    '        End Select
    '        If Spy < -4 Then Spy = -4 '最小速度-4
    '        Y += Spy
    '        X += Spd
    '        AJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
    '    Next
    'End Function

    Public Function MSJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean, IsBuffer As Boolean) As String
        Dim I As Integer
        '有怠速转换
        MSJump = ""

        For I = 1 To Frame

            If Not IsJump And I = 1 Then '未起跳，第一帧初始化Y轴计算
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
                    Case Is <= -0.15
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
            If Y <= 0 Then
                '落地转换为跑步
                Y = 0
                Spy = 0
                IsJump = False
                MSJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                MSJump += MSRun(X, Spd, Ori, Acc, Frame - I, Y, Spy, False)
                Exit Function
            Else
                MSJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            End If
        Next
    End Function

    Public Function MJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean, IsBuffer As Boolean) As String
        Dim I As Integer
        '有怠速转换
        MJump = ""

        For I = 1 To Frame

            If Not IsJump And I = 1 Then '未起跳，第一帧初始化Y轴计算
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
                    Case Is <= -0.15
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
            If Y <= 0 Then
                '落地转换为跑步
                Y = 0
                Spy = 0
                IsJump = False
                MJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                MJump += MRun(X, Spd, Ori, Acc, Frame - I, Y, Spy, False)
                Exit Function
            Else
                MJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            End If
        Next
    End Function

    Public Function MSpinJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean, IsBuffer As Boolean) As String
        Dim I As Integer
        '有怠速转换
        MSpinJump = ""

        For I = 1 To Frame

            If Not IsJump And I = 1 Then '未起跳，第一帧初始化Y轴计算
                IsJump = True
                Spy = 3.168
            Else
                '滞空
                Select Case Spy
                    Case Is <= -3
                        Spy -= 0.34
                    Case Is <= -0.15
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
            If Y <= 0 Then
                '落地转换为跑步
                Y = 0
                Spy = 0
                IsJump = False
                MSpinJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                MSpinJump += MRun(X, Spd, Ori, Acc, Frame - I, Y, Spy, False)
                Exit Function
            Else
                MSpinJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            End If
        Next
    End Function

    Public Function SpinJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        Dim SSpd As Single
        '有怠速转换
        SpinJump = ""
        '默认11帧，未考虑落地
        For I = 1 To 11
            '旋转
            If Spy >= 0 Then
                '正常下落
                Select Case Spy
                    Case Is <= -3
                        Spy -= 0.34
                    Case Is <= -0.15
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
            SpinJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
        Next
    End Function
    Public Function MAJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        '有怠速转换
        MAJump = ""

        For I = 1 To Frame
            '不滞空
            Select Case Spy
                Case Is <= -3
                    Spy -= 0.34
                Case Is <= -0.15
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
            '落地检测
            If Y <= 0 Then
                '落地转换为跑步
                Y = 0
                Spy = 0
                IsJump = False
                MAJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                MAJump += MRun(X, Spd, Ori, Acc, Frame - I, Y, Spy, False)
                Exit Function
            Else
                MAJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
            End If
        Next
    End Function

    Public Function MSAJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
        '有怠速转换
        MSAJump = ""

        For I = 1 To Frame
            '不滞空
            Select Case Spy
                Case Is <= -3
                    Spy -= 0.34
                Case Is <= -0.15
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

            Spd -= DSpd
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
            If Y <= 0 Then
                '落地转换为跑步
                Y = 0
                Spy = 0
                IsJump = False
                MSAJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
                MSAJump += MRun(X, Spd, Ori, Acc, Frame - I, Y, Spy, False)
                Exit Function
            Else
                MSAJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
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
            WAir += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
        Next
    End Function

    Public Function WJump(ByRef X As Single, ByRef Spd As Single, Ori As Boolean, Acc As Boolean, Frame As Integer, ByRef Y As Single, ByRef Spy As Single, ByRef IsJump As Boolean) As String
        Dim I As Integer
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
            WJump += X & vbTab & Y & vbTab & Spd & vbTab & Spy & vbTab & Hex2Dbl(Sng2Hex(X)) & vbTab & Hex2Dbl(Sng2Hex(Spd)) & vbTab & "'" & Sng2Hex(X) & vbTab & "'" & Sng2Hex(Spd) & vbCrLf
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
            s += p(I)
        Next
        If s = 0 Then
            Hex2Dbl = IIf(p(0) = 0, 0, -1)
            Exit Function
        End If
        s = p(0)
        For I = 1 To 8
            E += p(I) * (2 ^ (8 - I))
        Next
        For I = 1 To 23
            x += p(I + 8) * (2 ^ (-I))
        Next
        Hex2Dbl = ((-1) ^ s) * (1 + x) * (2 ^ (E - 127))
    End Function
End Module
