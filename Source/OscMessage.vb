Public Class OscMessage
    Dim _address As String = ""
    Dim _args As Object() = Nothing

    Public Sub New(ByVal address As String, Optional ByVal args As Object() = Nothing)
        _address = address
        _args = args
    End Sub

    Public Property Address As String
        Get
            Return _address
        End Get
        Set(ByVal value As String)
            _address = value
        End Set
    End Property

    Public Property Args As Object()
        Get
            Return _args
        End Get
        Set(ByVal value As Object())
            _args = value
        End Set
    End Property
End Class
