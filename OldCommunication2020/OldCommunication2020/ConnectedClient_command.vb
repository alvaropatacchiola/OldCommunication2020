Public Class ConnectedClient_command
    Public stack_messaggi As Queue
    Private mClient As System.Net.Sockets.TcpClient
    Private Const BYTES_TO_READ As Integer = 800
    Private readBuffer(BYTES_TO_READ) As Byte
    Private Const MESSAGE_DELIMITER As Char = ControlChars.Cr
    Public Event dataReceived(ByVal sender As ConnectedClient_command, ByVal message As String)
    Public Event stringReceived(ByVal sender As ConnectedClient_command, ByVal message As String)

    Sub New(ByVal client As System.Net.Sockets.TcpClient)
        stack_messaggi = New Queue()
        mClient = client
        mClient.GetStream.BeginRead(readBuffer, 0, BYTES_TO_READ, AddressOf doRead, Nothing) 'This will start reading from the stream between this server and the connected client.

    End Sub
    Private Sub doRead(ByVal ar As System.IAsyncResult)

        Dim totalRead As Integer
        Try
            totalRead = mClient.GetStream.EndRead(ar) 'Ends the reading and returns the number of bytes read.
        Catch ex As Exception
            'da gestire la rimozione dell'utente
            'mParentForm.removeClient(Me)
            close_connection()
            '    Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log.txt", True)
            '    writer.WriteLine("#" + Now.ToString + identificativo_client + "rimosso_read#")
            'End Using

            Exit Sub
            'The underlying socket have probably been closed OR an error has occured whilst trying to access it, either way, this is where you should remove close all eventuall connections
            'to this client and remove it from the list of connected clients.
        End Try

        If totalRead > 0 Then
            'the readBuffer array will contain everything read from the client.
            Dim receivedString As String = System.Text.Encoding.UTF8.GetString(readBuffer, 0, totalRead)
            RaiseEvent dataReceived(Me, receivedString)
        End If

        Try
            mClient.GetStream.BeginRead(readBuffer, 0, BYTES_TO_READ, AddressOf doRead, Nothing) 'Begin the reading again.
        Catch ex As Exception
            'The underlying socket have probably been closed OR an error has occured whilst trying to access it, either way, this is where you should remove close all eventuall connections
            'to this client and remove it from the list of connected clients.
        End Try
    End Sub
    Public Sub elabora_messaggio(ByVal message As String)
        If (InStr(message, "|end")) <> 0 Then
            Dim data() As String = message.Split("|"c)
            If data.Length > 2 Then
                Select Case data(1)
                    Case "DISCONNECT"
                        'SendMessage("Disconnected" + Chr(13) + Chr(10))
                        close_connection()
                    Case "CONNECT"
                        SendMessage("Connected" + Chr(13) + Chr(10))
                    Case Else ' comando da inviare "LOCALCOMMAND|subcommand;id;codice;valore|end"
                        Dim coppia_temp As New coppia
                        coppia_temp.messaggio = data(1)
                        coppia_temp.server_connesso = Me
                        condivisa.add_comandi(coppia_temp)
                End Select
            End If
        End If
    End Sub
    Public Sub SendMessage(ByVal msg As String)
        Dim sw As IO.StreamWriter
        Try
            SyncLock mClient.GetStream
                sw = New IO.StreamWriter(mClient.GetStream) 'Create a new streamwriter that will be writing directly to the networkstream.
                sw.Write(msg)
                sw.Flush()
            End SyncLock
        Catch ex As Exception
            close_connection()
            'MessageBox.Show(ex.ToString)
        End Try
        'As opposed to writing to a file, we DONT call close on the streamwriter, since we dont want to close the stream.
    End Sub
    Public Sub libera_coda()
        Dim message_temp As String
        Dim i As Integer
        For i = 0 To stack_messaggi.Count
            If stack_messaggi.Count > 0 Then
                message_temp = stack_messaggi.Dequeue
                elabora_messaggio(message_temp)
            Else
                Exit For
            End If

        Next

    End Sub


    Public Sub messageReceived(ByVal sender As ConnectedClient_command, ByVal message As String)
        If InStr(message, "LOCALCOMMAND") <> 0 Then
            stack_messaggi.Enqueue(message)
        End If
        libera_coda()


    End Sub
    Public Sub close_connection()
        mClient.Close()
    End Sub
End Class