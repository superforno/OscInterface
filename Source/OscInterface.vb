Imports System.Net.Sockets
Imports System.Text
Imports System.Net

Public Class OscInterface

#Region "OscInterface --> Local variables"

    Dim _client As UdpClient = Nothing
    Dim _destination As IPEndPoint = Nothing
    Dim _localPort As Integer = 0
    Dim _recvFrom As IPEndPoint = Nothing

#End Region

#Region "OscInterface --> Contructor - Destructor"

    Public Sub New(ByVal IP As String, ByVal Port As Integer, Optional ByVal LocalPort As Integer = 0)
        _localPort = LocalPort
        If _localPort = 0 Then
            _localPort = RandomNumber(50000, 60000)
        End If
        _client = New UdpClient(_localPort)
        _destination = New IPEndPoint(IPAddress.Parse(IP), Port)
        _recvFrom = New IPEndPoint(IPAddress.Parse(IP), _localPort)
    End Sub

    Protected Overrides Sub Finalize()
        _client.Close()
    End Sub

#End Region

#Region "OscInterface --> Private Functions"

    Private Function PadStr(ByVal str As String) As Byte()
        Dim data As Byte() = Encoding.ASCII.GetBytes(str)
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

    Private Function RandomNumber(ByVal MaxNumber As Integer, Optional ByVal MinNumber As Integer = 0) As Integer
        Dim r As New Random(System.DateTime.Now.Millisecond)
        If MinNumber > MaxNumber Then
            Dim t As Integer = MinNumber
            MinNumber = MaxNumber
            MaxNumber = t
        End If
        Return r.Next(MinNumber, MaxNumber)
    End Function

    Private Function Combine(ByVal ParamArray arrays As Byte()()) As Byte()
        Dim rv As Byte() = New Byte(arrays.Sum(Function(a) a.Length) - 1) {}
        Dim offset As Integer = 0
        For Each array As Byte() In arrays
            System.Buffer.BlockCopy(array, 0, rv, offset, array.Length)
            offset += array.Length
        Next
        Return rv
    End Function

#End Region

#Region "OscInterface --> Private Subs"

    Private Sub UDPRecv(ByVal ar As IAsyncResult)
        Try
            '' Next statement will throw when the socket was closed
            If _recvFrom Is Nothing Then
                _recvFrom = New IPEndPoint(IPAddress.Any, _localPort)
            End If
            Dim recvBytes As Byte() = _client.EndReceive(ar, _recvFrom)
            Dim recvMsg As String = Encoding.UTF8.GetString(recvBytes)

            '' PARSE DATA

            Dim msg As New EF.OSC.OscMessage("")

            Dim types As New List(Of Char)
            Dim finalArgs As Object() = Nothing

            Dim comma As Integer = Array.IndexOf(Of Byte)(recvBytes, Encoding.ASCII.GetBytes(",")(0))
            Dim _headpad As Integer = 0

            Dim _addr As Byte() = {&H0}
            Dim _head As Byte() = {&H0}
            Dim _data As Byte() = Nothing

            For i = 0 To comma - 1
                If recvBytes(i) = &H0 Then
                    msg.Address = Encoding.ASCII.GetString(_addr)
                    Exit For
                Else
                    ReDim Preserve _addr(_addr.Length)
                    _addr(i) = recvBytes(i)
                End If
            Next

            For i = comma + 1 To recvBytes.Length - 1
                If recvBytes(i) = &H0 Then
                    _headpad = i
                    Exit For
                Else
                    _head(0) = recvBytes(i)
                    types.Add(Encoding.ASCII.GetChars(_head)(0))
                End If
            Next

            Dim p As Integer = 4 - (_headpad Mod 4)
            If p < 4 Then
                _headpad += p
            End If

            If types.Count > 0 Then
                ReDim finalArgs(types.Count - 1)

                Dim argsdata(recvBytes.Length - _headpad - 1) As Byte
                System.Buffer.BlockCopy(recvBytes, _headpad, argsdata, 0, recvBytes.Length - _headpad)
                _headpad = 0

                For i = 0 To types.Count - 1
                    Select Case types(i)
                        Case "s"
                            Dim lastch As Integer = Array.IndexOf(argsdata, BitConverter.GetBytes(&H0)(0), _headpad)
                            Dim len As Integer = lastch - _headpad
                            ReDim _data(len - 1)
                            System.Buffer.BlockCopy(argsdata, _headpad, _data, 0, len)
                            finalArgs(i) = Encoding.ASCII.GetString(_data)
                            _headpad += len

                            Dim n As Integer = 4 - (_headpad Mod 4)
                            If n < 4 Then
                                _headpad += n
                            End If
                        Case "i"
                            ReDim _data(3)
                            System.Buffer.BlockCopy(argsdata, _headpad, _data, 0, 4)
                            Array.Reverse(_data)
                            finalArgs(i) = BitConverter.ToInt32(_data, 0)
                            _headpad += 4
                        Case "f"
                            ReDim _data(3)
                            System.Buffer.BlockCopy(argsdata, _headpad, _data, 0, 4)
                            Array.Reverse(_data)
                            finalArgs(i) = BitConverter.ToSingle(_data, 0)
                            _headpad += 4
                        Case "b"
                            ReDim _data(3)
                            System.Buffer.BlockCopy(argsdata, 0, _data, 0, 4)
                            Array.Reverse(_data)
                            Dim lenght As Integer = BitConverter.ToInt32(_data, 0)
                            ReDim _data(lenght - 1)
                            System.Buffer.BlockCopy(argsdata, 4, _data, 0, lenght)
                            finalArgs(i) = _data
                    End Select
                Next

                msg.Args = finalArgs

            End If
            '' END PARSING DATA

            RaiseEvent ReceivedMessage(Me, New OscMessageEventArgs(msg))

            _client.BeginReceive(AddressOf UDPRecv, Nothing)
        Catch ex As ObjectDisposedException
            '' Socket was closed, do nothing
        End Try
    End Sub

#End Region

#Region "OscInterface --> Public Subs"

    Public Sub SendMessage(ByVal Message As OscMessage)
        Dim argsdata As Byte() = Nothing
        Dim typedef As String = ","
        Dim typedata As Byte() = Nothing

        Dim datagram As Byte() = PadStr(Message.Address)

        If Message.Args Is Nothing Then
            typedata = PadStr(typedef)
            datagram = Combine({datagram, typedata})
        Else
            For Each arg In Message.Args
                If TypeOf arg Is String Then
                    typedef += "s"
                    Dim str As Byte() = PadStr(arg)
                    If argsdata Is Nothing Then
                        ReDim argsdata(str.Length - 1)
                        argsdata = str
                    Else
                        'ReDim Preserve argsdata(argsdata.Length + str.Length - 1)
                        argsdata = Combine({argsdata, str})
                    End If
                ElseIf TypeOf arg Is Integer Or TypeOf arg Is Single Then
                    If TypeOf arg Is Integer Then
                        typedef += "i"
                    Else
                        typedef += "f"
                    End If

                    Dim bigend As Byte() = BitConverter.GetBytes(arg)
                    Array.Reverse(bigend, 0, bigend.Length)
                    If argsdata Is Nothing Then
                        ReDim argsdata(bigend.Length + 1)
                        argsdata = bigend
                    Else
                        'ReDim Preserve argsdata(argsdata.Length + bigend.Length - 1)
                        argsdata = Combine({argsdata, bigend})
                    End If
                ElseIf TypeOf arg Is Byte() Then
                    typedef += "b"
                    If argsdata Is Nothing Then
                        ReDim argsdata(arg.Lenght - 1)
                        argsdata = arg
                    Else
                        ReDim Preserve argsdata(argsdata.Length + arg.Length - 1)
                        For i = 0 To arg.Lenght
                            argsdata(argsdata.Length + i) = arg(i)
                        Next
                    End If
                End If
            Next

            typedata = PadStr(typedef)
            datagram = Combine({datagram, typedata})

            Dim argsdt(argsdata.Length - 2) As Byte
            'Array.Copy(argsdata, 1, argsdt, 0, argsdata.Length - 1)
            'ReDim Preserve argsdata(argsdata.Length)
            datagram = Combine({datagram, argsdata})
        End If

        ' send OSC datagram
        _client.Send(datagram, datagram.Length, _destination)
    End Sub

    Public Sub ListenToMessage()
        _client.BeginReceive(AddressOf UDPRecv, Nothing)
    End Sub

#End Region

#Region "OscInterface --> Public Properties"

    Public ReadOnly Property LocalPort As Integer
        Get
            Return _localPort
        End Get
    End Property

#End Region

#Region "OscInterface --> Public Events"

    Public Event ReceivedMessage As EventHandler(Of OscMessageEventArgs)

#End Region

End Class