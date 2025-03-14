Module ModuleCmd
    Public pfc As System.Drawing.Text.PrivateFontCollection
    Public CmdBtn(99) As CmdCtrl
    Public Sub SetCmdText()
        Dim s As String = ""
        For i As Integer = 0 To CmdBtn.Length - 1
            If CmdBtn(i).Visible Then
                s &= CmdBtn(i).Label1.Text & CmdBtn(i).Label2.Text & " "
            Else
                Exit For
            End If
        Next
        'Debug.Print(CmdBtn.Length.ToString & "-" & s)
        Form1.T2.Text = s.TrimEnd
        Form1.ReDraw()
    End Sub
End Module
