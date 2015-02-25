Public Class OscMessageEventArgs
    Inherits EventArgs

    Dim _str As Object

    Public Sub New(ByVal data As OscMessage)
        _str = data
    End Sub

    Public Property Data As OscMessage
        Get
            Return _str
        End Get
        Set(ByVal value As OscMessage)
            _str = value
        End Set
    End Property
End Class
