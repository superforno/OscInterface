Imports System.Text

'Public Class OSCArgument

'    Dim _type As String
'    Dim _data As Byte()

'    Public Sub New(ByVal type As String, ByVal data As Byte())
'        _type = type
'        _data = data
'    End Sub



'End Class

Public Class OSCArguments

    Dim _list As New List(Of Array)

    Public Sub New()

    End Sub

    ' Add string to arguments. String is terminated and padded with null char
    Public Sub AddString(ByVal Str As String)
        Dim data As Byte() = PadString(Str)
        _list.Add({"s", data})
    End Sub

    ' Add int32 to arguments. Int32 is converted to big endian
    Public Sub AddInt32(ByVal Value As Integer)
        Dim bigend As Byte() = BitConverter.GetBytes(Value)
        Array.Reverse(bigend, 0, bigend.Length)
        _list.Add({"i", bigend})
    End Sub

    ' Add float to arguments. Float is converted to big endian
    Public Sub AddFloat(ByVal Value As Single)
        Dim bigend As Byte() = BitConverter.GetBytes(Value)
        Array.Reverse(bigend, 0, bigend.Length)
        _list.Add({"f", bigend})
    End Sub

    '   TO DO
    '   Implement blob as OSC standard blob
    '
    Public Sub AddBlob(ByVal Blob As Byte())
        _list.Add({"b", Blob})
    End Sub

    Public Function PadString(ByVal Str As String) As Byte()
        Dim data As Byte() = Encoding.ASCII.GetBytes(Str)
        ReDim Preserve data(data.Length)
        Dim len As Integer = data.Length
        Dim pad As Integer = len Mod 4
        If pad > 0 Then
            Dim realPad As Integer = 4 - pad
            Dim newLen As Integer = data.Length + realPad
            ReDim Preserve data(newLen - 1)
        End If
        Return data
    End Function

    ' Array of bytes containing all agruments, starting with args header
    Public ReadOnly Property ArgsBytes As Byte()
        Get
            Dim FullLenght As Integer = 0
            Dim FullData As Byte()
            Dim Header As String = ","

            For Each arg As Array In _list
                Dim data As Byte() = arg(1)
                Header += arg(0)
                FullLenght += data.Length
                ReDim Preserve FullData(FullLenght - 1)
                System.Buffer.BlockCopy(data, 0, FullData, (FullLenght - data.Length), data.Length)
            Next

            Dim headBytes As Byte() = PadString(Header)
            FullLenght += headBytes.Length
            ReDim Preserve headBytes(FullLenght - 1)
            System.Buffer.BlockCopy(FullData, 0, headBytes, headBytes.Length - FullData.Length, FullData.Length)

            Return headBytes
        End Get
    End Property

    Public ReadOnly Property Data As List(Of Byte())
        Get
            Dim _l As New List(Of Byte())
            For Each e In _list
                _l.Add(e(1))
            Next
            Return _l
        End Get
    End Property

    Public ReadOnly Property DataTypes As String
        Get
            Dim s As String = ""
            For Each e In _list
                s += e(0)
            Next
            Return s
        End Get
    End Property

    Public ReadOnly Property Lenght As Integer
        Get
            Return _list.Count
        End Get
    End Property

End Class
