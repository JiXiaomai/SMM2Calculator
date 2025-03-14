<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class CmdCtrl
    Inherits System.Windows.Forms.UserControl

    'UserControl 重写释放以清理组件列表。
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Windows 窗体设计器所必需的
    Private components As System.ComponentModel.IContainer

    '注意: 以下过程是 Windows 窗体设计器所必需的
    '可以使用 Windows 窗体设计器修改它。  
    '不要使用代码编辑器修改它。
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Label1 = New Label()
        BtnP = New Button()
        BtnM = New Button()
        Label2 = New Label()
        SuspendLayout()
        ' 
        ' Label1
        ' 
        Label1.AutoSize = True
        Label1.Font = New Font("Microsoft YaHei UI", 9F)
        Label1.Location = New Point(2, 2)
        Label1.Name = "Label1"
        Label1.Size = New Size(35, 17)
        Label1.TabIndex = 0
        Label1.Text = "Cmd"
        ' 
        ' BtnP
        ' 
        BtnP.BackColor = Color.White
        BtnP.FlatStyle = FlatStyle.Flat
        BtnP.Font = New Font("Microsoft YaHei UI", 9F)
        BtnP.Location = New Point(140, 1)
        BtnP.Name = "BtnP"
        BtnP.Size = New Size(25, 23)
        BtnP.TabIndex = 1
        BtnP.Text = "﹢"
        BtnP.UseVisualStyleBackColor = False
        ' 
        ' BtnM
        ' 
        BtnM.BackColor = Color.White
        BtnM.FlatStyle = FlatStyle.Flat
        BtnM.Font = New Font("Microsoft YaHei UI", 9F, FontStyle.Regular, GraphicsUnit.Point, CByte(134))
        BtnM.Location = New Point(112, 1)
        BtnM.Name = "BtnM"
        BtnM.Size = New Size(25, 23)
        BtnM.TabIndex = 2
        BtnM.Text = "‐"
        BtnM.UseVisualStyleBackColor = False
        ' 
        ' Label2
        ' 
        Label2.AutoSize = True
        Label2.Font = New Font("Microsoft YaHei UI", 9F)
        Label2.Location = New Point(88, 2)
        Label2.Name = "Label2"
        Label2.Size = New Size(22, 17)
        Label2.TabIndex = 3
        Label2.Text = "99"
        ' 
        ' CmdCtrl
        ' 
        AutoScaleDimensions = New SizeF(7F, 17F)
        AutoScaleMode = AutoScaleMode.Font
        BorderStyle = BorderStyle.FixedSingle
        Controls.Add(Label2)
        Controls.Add(BtnM)
        Controls.Add(BtnP)
        Controls.Add(Label1)
        Name = "CmdCtrl"
        Size = New Size(168, 27)
        ResumeLayout(False)
        PerformLayout()
    End Sub

    Friend WithEvents Label1 As Label
    Friend WithEvents BtnP As Button
    Friend WithEvents BtnM As Button
    Friend WithEvents Label2 As Label
End Class
