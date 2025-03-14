Public Class CmdCtrl
    Public Sub New()

        ' 此调用是设计器所必需的。
        InitializeComponent()

        ' 在 InitializeComponent() 调用之后添加任何初始化。
        Dim F = New Font(pfc.Families(0), 9)
        For Each i As Control In Me.Controls
            i.Font = F
        Next
    End Sub

    Private Sub BtnP_Click(sender As Object, e As EventArgs) Handles BtnP.Click
        Label2.Text = (Val(Label2.Text) + 1).ToString
        SetCmdText()
    End Sub

    Private Sub BtnM_Click(sender As Object, e As EventArgs) Handles BtnM.Click
        If Val(Label2.Text) < 1 Then Exit Sub
        Label2.Text = (Val(Label2.Text) - 1).ToString
        SetCmdText()
    End Sub


End Class
