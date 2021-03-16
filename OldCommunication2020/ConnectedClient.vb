Public Class ConnectedClient
    Public stack_messaggi As Queue
    Public ip_number As String
    Private mClient As System.Net.Sockets.TcpClient
    Private Const BYTES_TO_READ As Integer = 355
    Private mUsername As String
    Private mNumber As String
    Private mstatus_connessione As String
    Private midentificativo_connessione As String
    Private string_buffer As String
    Private string_buffer_db As String
    Private removable As Boolean
    Public assegnato_pc As Boolean
    Private mtime_connessione As DateTime
    Private readThread As System.Threading.Thread
    Private readBuffer(BYTES_TO_READ) As Byte
    Private Const MESSAGE_DELIMITER As Char = ControlChars.Cr
    Public Event dataReceived(ByVal sender As ConnectedClient, ByVal message As String)
    Public Event stringReceived(ByVal sender As ConnectedClient, ByVal message As String)
    Public ip_info As ip_list.ip_listDataTable
    Public keep_alive As Integer
    Public keep_alive_counter As Integer
    Public query As New query
    Public invio As Boolean
    Sub New(ByVal client As System.Net.Sockets.TcpClient)
        stack_messaggi = New Queue()
        mClient = client
        mClient.GetStream.BeginRead(readBuffer, 0, BYTES_TO_READ, AddressOf doRead, Nothing) 'This will start reading from the stream between this server and the connected client.
        keep_alive = 60
        keep_alive_counter = 0
        removable = False
        invio = False
    End Sub
    Public Property send_receive() As String
        Get
            Return string_buffer
        End Get
        Set(ByVal value As String)
            string_buffer = value
        End Set
    End Property
    Public Property send_receive_db() As String
        Get
            Return string_buffer_db
        End Get
        Set(ByVal value As String)
            string_buffer_db = value
        End Set
    End Property
    Public Property Username() As String
        Get
            Return mUsername
        End Get
        Set(ByVal value As String)
            mUsername = value
        End Set
    End Property
    Public Property Number() As String
        Get
            Return mNumber
        End Get
        Set(ByVal value As String)
            mNumber = value
        End Set
    End Property
    Public Property status_connessione() As String
        Get
            Return mstatus_connessione
        End Get
        Set(ByVal value As String)
            mstatus_connessione = value
        End Set
    End Property
    Public Property identificativo_client() As String
        Get
            Return midentificativo_connessione
        End Get
        Set(ByVal value As String)
            midentificativo_connessione = value
        End Set
    End Property

    Public Property remove() As Boolean
        Get
            Return removable
        End Get
        Set(ByVal value As Boolean)
            removable = value
        End Set
    End Property
    Public Property time_connessione() As DateTime
        Get
            Return mtime_connessione
        End Get
        Set(ByVal value As DateTime)
            mtime_connessione = value
        End Set
    End Property

    Public Sub close_connection()
        Me.removable = True
        Try
            Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log.txt", True)
                writer.WriteLine("#" + Now.ToString + identificativo_client + "rimosso_close#")
            End Using

            mClient.Close()

        Catch ex As Exception

        End Try
    End Sub
    Private Sub doRead(ByVal ar As System.IAsyncResult)

        Dim totalRead As Integer
        Try
            totalRead = mClient.GetStream.EndRead(ar) 'Ends the reading and returns the number of bytes read.

        Catch ex As Exception
            'da gestire la rimozione dell'utente
            'mParentForm.removeClient(Me)
            Me.removable = True
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
            'Dim receivedString As String = System.Text.Encoding.UTF8.GetString(readBuffer, 0, totalRead)
            Dim receivedString As String = ""
            For Each byt As Byte In readBuffer
                'If byt > 0 Then
                receivedString = receivedString + Chr(byt)
                'End If
            Next

            RaiseEvent dataReceived(Me, receivedString)
        End If

        Try
            mClient.GetStream.BeginRead(readBuffer, 0, BYTES_TO_READ, AddressOf doRead, Nothing) 'Begin the reading again.
        Catch ex As Exception
            Me.removable = True
            close_connection()
            'The underlying socket have probably been closed OR an error has occured whilst trying to access it, either way, this is where you should remove close all eventuall connections
            'to this client and remove it from the list of connected clients.
        End Try
    End Sub
    Public Sub SendMessage(ByVal msg As String)
        Dim sw As IO.StreamWriter
        Try
            SyncLock mClient.GetStream
                ' Dim message_send As String = msg + Chr(10)
                Dim message_send As String = msg
                If invio Then
                    message_send = message_send + Chr(13)
                End If
                If (InStr(midentificativo_connessione, "543415724") <> 0) Then ' modifica yagel id=0
                    If (Mid(message_send, 1, 2)) = "01" Then
                        message_send = "00" + Mid(message_send, 3, Len(message_send))
                    End If
                End If
                'If (InStr(midentificativo_connessione, "854219") <> 0) Then
                '    Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\854219.txt", True)
                '        writer.WriteLine("#" + Now.ToString + msg + "#")
                '    End Using
                'End If
                sw = New IO.StreamWriter(mClient.GetStream) 'Create a new streamwriter that will be writing directly to the networkstream.
                sw.Write(message_send)
                sw.Flush()
            End SyncLock
        Catch ex As Exception

            Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log.txt", True)
                writer.WriteLine("#" + Now.ToString + identificativo_client + "rimosso_send#")
            End Using

            Me.removable = True
            close_connection()
            'MessageBox.Show(ex.ToString)
        End Try
        'As opposed to writing to a file, we DONT call close on the streamwriter, since we dont want to close the stream.
    End Sub
    Public Sub update_ip(ByVal codice As String)
        Dim differenza_minuti As Long

        For Each dc In ip_info
            differenza_minuti = DateDiff(DateInterval.Minute, dc.data_creazione, Now)
        Next
        query.aggiorna_ip(ip_number, codice, differenza_minuti)
    End Sub
    Public Function ip_config(ByVal codice As String) As Integer
        Dim keep_alive_temp As Long
        ip_info = query.get_ip_config(codice)

        For Each dc In ip_info
            keep_alive_temp = dc.time_keep_alive
        Next
        If keep_alive_temp < 1 Or keep_alive_temp > 60 Then
            keep_alive_temp = 60
        End If
        Return keep_alive_temp
    End Function
    Private Function UnicodeStringToBytes(ByVal str As String) As Byte()

        Return System.Text.Encoding.UTF8.GetBytes(str)
    End Function
    Public Sub elabora_messaggio(ByVal message As String)
        Dim data() As String = message.Split("|"c)
        Dim indice As Integer
        'If InStr(message, "633820") <> 0 Then
        '    indice = 0
        'End If
        If data.Length > 2 Then
            If InStr(data(2), "log") = 0 And InStr(data(2), "@") <> 0 And InStr(data(2), "1Dend") = 0 And InStr(data(2), "aze") = 0 Then
                Try
                    Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log_mail.txt", True)
                        writer.WriteLine("#" + Now.ToString + message + "#")
                    End Using
                Catch ex As Exception

                End Try
                data(2) = Replace(data(2), "34mai", "")
                data(2) = Replace(data(2), "'34mai", "")
                Dim data_mail() As String = data(2).Split("'"c)
                Dim mail_code() As Byte
                Try
                    mail_code = UnicodeStringToBytes(data_mail(3))
                    data_mail(3) = ""
                    For Each mail_code_val In mail_code
                        If mail_code_val <> 13 Then
                            data_mail(3) = data_mail(3) + Chr(mail_code_val)
                        Else
                            Exit For
                        End If
                    Next
                Catch ex As Exception

                End Try

              
                Try
                    If data_mail.Length > 3 And Len(data_mail(1)) > 0 And data_mail(1) <> "" Then
                        'Try
                        '    DataGridView3.Rows.Add(Now.ToString, sender.Connected, data_mail(1), data_mail(1), "False", data_mail(3))
                        'Catch ex As Exception

                        'End Try
                        Try
                            'send_mail(data_mail(1), data_mail(3))
                            send_mail(data_mail(1), data_mail(3), data(1))
                            SendMessage("MAIO" + Chr(13) + Chr(10))
                        Catch ex As Exception

                        End Try
                    End If
                    If data_mail.Length > 3 And Len(data_mail(2)) > 0 And data_mail(2) <> "" Then
                        'Try
                        '    DataGridView3.Rows.Add(Now.ToString, sender.Connected, data_mail(2), data_mail(2), "False", data_mail(3))
                        'Catch ex As Exception

                        'End Try
                        Try
                            'send_mail(data_mail(2), data_mail(3))
                            send_mail(data_mail(2), data_mail(3), data(1))
                            SendMessage("MAIO" + Chr(13) + Chr(10))
                        Catch ex As Exception

                        End Try
                        Exit Sub

                    End If
                    Exit Sub
                Catch ex As Exception
                    Exit Sub
                End Try
            End If
        End If
        Select Case data(0)
            Case "/CONNECTM5" 'connessione strumento
                
                '----------------
                'elementi da non accettare
                Try
                    If data(1) = "905481" Or data(1) = "314371" Then
                        Me.status_connessione = "rifiuta#m5"
                        SendMessage("/Connected" + Chr(13) + Chr(10))
                        Exit Sub
                    End If
                Catch ex As Exception

                End Try
                Try
                    Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log.txt", True)
                        writer.WriteLine("#" + Now.ToString + message + "#")
                    End Using
                Catch ex As Exception

                End Try

                Try
                    'If data(1) = "464299" Then
                    '    Me.status_connessione = "rifiuta#m5"
                    '    SendMessage("/Connected" + Chr(13) + Chr(10))
                    'End If
                    If (InStr(data(2), "We") > 0) Or (InStr(data(2), "WFe") > 0) Or (InStr(data(2), "Ue") > 0) Or (InStr(data(2), "Le") > 0) Or (InStr(data(2), "Me") > 0) Or (InStr(data(2), "en") = 0) Then
                        invio = True
                    End If
                    If data(1) = "1234" Then
                        Me.status_connessione = "connesso#m5"
                        Me.identificativo_client = "222222"
                        SendMessage("/Connected" + Chr(13) + Chr(10))
                        'Me.close_connection()
                        'Me.status_connessione = "rifiuta#m5"
                        'SendMessage("/Connected" + Chr(13) + Chr(10))

                        Exit Sub
                    End If

                    If data(1).Length < 4 Then
                        Me.status_connessione = "connesso#m5"
                        Me.identificativo_client = ip_number
                        SendMessage("/Connected" + Chr(13) + Chr(10))
                        'Me.close_connection()
                        'Me.status_connessione = "rifiuta#m5"
                        'SendMessage("/Connected" + Chr(13) + Chr(10))

                        Exit Sub
                    End If
                    Me.status_connessione = "connesso#m5"
                    Me.identificativo_client = data(1)
                    SendMessage("/Connected" + Chr(13) + Chr(10))
                    'keep_alive = ip_config(data(1))
                    keep_alive = 100
                    'update_ip(data(1))
                Catch ex As Exception

                End Try
            Case "/DISCONNECTPC"

                If assegnato_pc Then
                    RaiseEvent stringReceived(Me, message)
                Else
                    close_connection()
                    assegnato_pc = False
                End If
            Case "/CONNECTPC" 'connession PC
                Try
                    Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log.txt", True)
                        writer.WriteLine("#" + Now.ToString + message + "#")
                    End Using
                Catch ex As Exception

                End Try

                Try
                    Dim codice_finale_pc As String = ""
                    For i = 1 To data(1).Length
                        If Asc(Mid(data(1), i, 1)) >= 43 And Asc(Mid(data(1), i, 1)) <= 57 Then
                            codice_finale_pc = codice_finale_pc + Mid(data(1), i, 1)
                        End If
                    Next
                    Me.status_connessione = "connesso#pc"
                    data(1) = codice_finale_pc
                    Me.identificativo_client = data(1)
                    SendMessage("/Connected" + Chr(13) + Chr(10))

                Catch ex As Exception

                End Try
            Case Else

                RaiseEvent stringReceived(Me, message)
        End Select
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

    Public Sub messageReceived(ByVal sender As ConnectedClient, ByVal message As String)
        Dim index_start As Integer = 0
        Dim index_start_pc As Integer = 0
        Dim index_stop As Integer = 0
        Dim message_received As String = message

        If InStr(midentificativo_connessione, "993816") <> 0 Then
            Try
                Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\993816.txt", True)
                    writer.WriteLine("#" + Now.ToString + message_received + "#")
                End Using
            Catch ex As Exception

            End Try
        End If

        If (InStr(midentificativo_connessione, "543415724") <> 0) Then ' modifica yagel id=0
            Dim split_value() As String = message_received.Split("|")
            Try
                If (Mid(split_value(2), 1, 2)) = "00" Then
                    message_received = split_value(0) + "|" + split_value(1) + "|" + "01" + Mid(split_value(2), 3, Len(split_value(2))) + "|end"
                End If
            Catch ex As Exception

            End Try

        End If

        index_start = InStr(message_received, "M5")
        index_start_pc = InStr(message_received, "PC")
        index_stop = InStr(message_received, "end")
        'If index_start < index_stop Or index_start_pc < index_stop Or InStr(message_received, "CONNECT") <> 0 Or InStr(message_received, "mai") <> 0 Then
        'Or InStr(message_received, "mai") <>
        Dim R As New System.Text.RegularExpressions.Regex("(\w+)@(\w+)\.(\w+)\'+(\w+)")
        Dim R1 As New System.Text.RegularExpressions.Regex("(\w+)@(\w+)\.(\w+)\.(\w+)\'+(\w+)")
        Dim R2 As New System.Text.RegularExpressions.Regex("([\w-+]+(?:\.[\w-+]+)*@(?:[\w-]+\.)+[a-zA-Z]{2,7})")
        Dim pluto As Boolean = False
        Dim paperino As Boolean = False
        Dim pippo As Boolean = False
        pluto = R.IsMatch(message_received)
        paperino = R1.IsMatch(message_received)
        pippo = R2.IsMatch(message_received)
        If InStr(message_received, "CONNECT") <> 0 Or InStr(message_received, "34mai") Or InStr(message_received, "34mai") <> 0 Or pluto Or paperino Or pippo Then
            stack_messaggi.Enqueue(message_received)
        End If
        libera_coda()


    End Sub
    'Public Sub send_mail(ByVal mailto As String, ByVal mailtext As String)
    '    Dim indice As Integer
    '    'For Each cc As coppia In buffer_mail
    '    Try
    '        'if cc.stato = False Then
    '        Dim client As New System.Net.Mail.SmtpClient("outmail.impresasemplice.it")
    '        Dim temp_str As String = ""
    '        Dim oMessage As New System.Net.Mail.MailMessage()
    '        client.UseDefaultCredentials = False
    '        client.Credentials = New Net.NetworkCredential("Admin267238@emec.191.it", "VPkBELQOMq")
    '        oMessage.From = New System.Net.Mail.MailAddress("ermes@ermes-server.com")
    '        oMessage.To.Add(New System.Net.Mail.MailAddress(mailto))
    '        'For i = 1 To Len(cc.mail_text) - 1
    '        '    If Asc(Mid(cc.mail_text, i, 1)) = 14 Then
    '        '        temp_str = temp_str + Chr(13) + Chr(10)
    '        '    Else
    '        '        temp_str = temp_str + Mid(cc.mail_text, i, 1)
    '        '    End If
    '        'Next
    '        'cc.mail_text = temp_str
    '        Dim messaggio_mail() As String = mailtext.Split("#"c)
    '        If messaggio_mail.Length > 1 Then
    '            oMessage.Body = messaggio_mail(0) + Chr(13) + Chr(10) + messaggio_mail(1)
    '            oMessage.Subject = Replace(messaggio_mail(0), Chr(13) + Chr(10), "")
    '        Else
    '            oMessage.Body = messaggio_mail(0) + Chr(13) + Chr(10)
    '        End If
    '        client.Send(oMessage)
    '        ' cc.stato = True
    '        'End If
    '    Catch
    '    End Try
    '    'Next

    'End Sub
    Public Sub send_mail(ByVal mailto As String, ByVal mailtext As String, ByVal codice_mail As String)
        Dim indice As Integer
        Dim impianto As String = ""
        Dim descrizione_impianto As String = ""
        Dim data_mail As mail.impianto_newDataTable
        codice_mail = codice_mail.Replace("M5", "")
        data_mail = query.valore_mail(codice_mail)
        For Each dc In data_mail
            impianto = dc.nome_impianto

        Next
        'For Each cc As coppia In buffer_mail
        Try
            'if cc.stato = False Then
            If InStr(mailto, "biano.bastos@accamargo") <> 0 Or InStr(mailto, "silva@nalco") <> 0 _
                Or InStr(mailto, "olo88.mr@gmail") <> 0 Or InStr(mailto, "tecnia@geoprojectes") <> 0 Then

                Return

            End If
            Dim client As New System.Net.Mail.SmtpClient("smtp-mail.outlook.com")
            Dim temp_str As String = ""
            Dim oMessage As New System.Net.Mail.MailMessage()
            client.UseDefaultCredentials = False
            client.Credentials = New Net.NetworkCredential("ermes@ermes-server.com", "Erm,2019-1")
            oMessage.From = New System.Net.Mail.MailAddress("ermes@ermes-server.com")
            oMessage.To.Add(New System.Net.Mail.MailAddress(mailto))
            'For i = 1 To Len(cc.mail_text) - 1
            '    If Asc(Mid(cc.mail_text, i, 1)) = 14 Then
            '        temp_str = temp_str + Chr(13) + Chr(10)
            '    Else
            '        temp_str = temp_str + Mid(cc.mail_text, i, 1)
            '    End If
            'Next
            'cc.mail_text = temp_str
            Dim messaggio_mail() As String = mailtext.Split("#"c)
            Dim stringa_split() As String
            Dim nessuno As Boolean
            Dim nessuno_tipo As Boolean = False
            Dim asterisco As Boolean = False
            Dim tipo_strumento As String = ""
            Dim lunghezza As Integer = 0
            Dim partenza As Integer = 0
            Dim i As Integer

            If messaggio_mail.Length > 1 Then
                nessuno_tipo = False
                messaggio_mail(1) = messaggio_mail(1).Replace(Chr(14), Chr(13))
                messaggio_mail(1) = messaggio_mail(1).Replace(Chr(10), Chr(13))
                If InStr(messaggio_mail(1), "*") Then
                    asterisco = True
                End If
                If InStr(messaggio_mail(0), "LD") <> 0 Then
                    nessuno_tipo = True
                    lunghezza = 2
                    partenza = 1
                    tipo_strumento = "LD"
                End If
                If InStr(messaggio_mail(0), "WD") <> 0 Then
                    nessuno_tipo = True
                    lunghezza = 2
                    partenza = 1
                    tipo_strumento = "WD"
                End If
                If InStr(messaggio_mail(0), "WH") <> 0 Then
                    nessuno_tipo = True
                    lunghezza = 2
                    partenza = 1
                    tipo_strumento = "WH"
                End If

                If InStr(messaggio_mail(0), "MTower") <> 0 Then
                    nessuno_tipo = True
                    lunghezza = 2
                    partenza = 1
                    tipo_strumento = "MTower"
                End If
                If InStr(messaggio_mail(0), "LOTUS") <> 0 Then
                    nessuno_tipo = True
                    lunghezza = 2
                    partenza = 1
                    tipo_strumento = "LOTUS"
                End If

                If nessuno_tipo = False Then
                    lunghezza = 1
                    partenza = 0
                    tipo_strumento = "MAX5"
                End If
                oMessage.Body = messaggio_mail(0) + Chr(13) + Chr(10) + messaggio_mail(1)
                If impianto = "" Then
                    oMessage.Subject = Replace(messaggio_mail(0), Chr(13) + Chr(10), "")
                Else
                    oMessage.Subject = Replace(messaggio_mail(0), Chr(13) + Chr(10), "") + "-" + impianto
                End If

                stringa_split = messaggio_mail(1).Split(Chr(13))
                Dim sub_str As String = ""
                For i = 0 To stringa_split.Length - lunghezza
                    nessuno = False
                    If tipo_strumento = "LOTUS" Then 'caso ld
                        If InStr(stringa_split(i), "ID") <> 0 Then
                            nessuno = True
                        End If
                        If InStr(stringa_split(i), "MAXIMUM") <> 0 Then
                            If (asterisco = False And nessuno = False) Or (InStr(stringa_split(i), "*") <> 0 And asterisco = True And nessuno = False) Then
                                sub_str = "<li><span class=""alarm"">" + stringa_split(i) + "*" + "</span></li>"
                                stringa_split(i) = sub_str
                            Else
                                sub_str = "<li>" + stringa_split(i) + "</li>"
                                stringa_split(i) = sub_str
                            End If
                            nessuno = True
                        End If
                        If InStr(stringa_split(i), "LEVEL") <> 0 Then
                            If (asterisco = False And nessuno = False) Or (InStr(stringa_split(i), "*") <> 0 And asterisco = True And nessuno = False) Then
                                sub_str = "<li><span class=""alarm"">" + stringa_split(i) + "*" + "</span></li>"
                                stringa_split(i) = sub_str
                            Else
                                sub_str = "<li>" + stringa_split(i) + "</li>"
                                stringa_split(i) = sub_str
                            End If
                            nessuno = True
                        End If
                        If InStr(stringa_split(i), "STORAGE") <> 0 Then
                            If (asterisco = False And nessuno = False) Or (InStr(stringa_split(i), "*") <> 0 And asterisco = True And nessuno = False) Then
                                sub_str = "<li><span class=""alarm"">" + stringa_split(i) + "*" + "</span></li>"
                                stringa_split(i) = sub_str
                            Else
                                sub_str = "<li>" + stringa_split(i) + "</li>"
                                stringa_split(i) = sub_str
                            End If
                            nessuno = True
                        End If
                        If InStr(stringa_split(i), "SYSTEM") <> 0 Then
                            If (asterisco = False And nessuno = False) Or (InStr(stringa_split(i), "*") <> 0 And asterisco = True And nessuno = False) Then
                                sub_str = "<li><span class=""alarm"">" + stringa_split(i) + "*" + "</span></li>"
                                stringa_split(i) = sub_str
                            Else
                                sub_str = "<li>" + stringa_split(i) + "</li>"
                                stringa_split(i) = sub_str
                            End If
                            nessuno = True
                        End If
                        If InStr(stringa_split(i), "STOP") <> 0 Then
                            If (asterisco = False And nessuno = False) Or (InStr(stringa_split(i), "*") <> 0 And asterisco = True And nessuno = False) Then
                                sub_str = "<li><span class=""alarm"">" + stringa_split(i) + "*" + "</span></li>"
                                stringa_split(i) = sub_str
                            Else
                                sub_str = "<li>" + stringa_split(i) + "</li>"
                                stringa_split(i) = sub_str
                            End If
                            nessuno = True
                        End If

                        If nessuno = False Then
                            If stringa_split(i).Length > 2 Then
                                sub_str = "<li>" + stringa_split(i) + "</li>"
                                stringa_split(i) = sub_str
                            Else
                                stringa_split(i) = ""
                            End If
                        End If


                    End If
                    If tipo_strumento = "LD" Or tipo_strumento = "WD" Or tipo_strumento = "WH" Then 'caso ld
                        If InStr(stringa_split(i), "ID") <> 0 Then
                            nessuno = True
                        End If
                        If InStr(stringa_split(i), "Flow") <> 0 Then
                            If (InStr(stringa_split(i), "No") <> 0 And asterisco = False And nessuno = False) Or (InStr(stringa_split(i), "*") <> 0 And asterisco = True And nessuno = False) Then
                                sub_str = "<li><span class=""alarm"">" + stringa_split(i) + "*" + "</span></li>"
                                stringa_split(i) = sub_str
                            Else
                                sub_str = "<li>" + stringa_split(i) + "</li>"
                                stringa_split(i) = sub_str
                            End If
                            nessuno = True
                        End If
                        If InStr(stringa_split(i), "Lev") <> 0 Then

                            If (InStr(stringa_split(i), "Lo") <> 0 And asterisco = False And nessuno = False) Or (InStr(stringa_split(i), "*") <> 0 And asterisco = True And nessuno = False) Then
                                sub_str = "<li><span class=""alarm"">" + stringa_split(i) + "*" + "</span></li>"
                                stringa_split(i) = sub_str
                            Else
                                sub_str = "<li>" + stringa_split(i) + "</li>"
                                stringa_split(i) = sub_str
                            End If
                            nessuno = True
                        End If
                        If InStr(stringa_split(i), "Min/Max") <> 0 Then
                            If (InStr(stringa_split(i), "Yes") <> 0 And asterisco = False And nessuno = False) Or (InStr(stringa_split(i), "*") <> 0 And asterisco = True And nessuno = False) Then
                                sub_str = "<li><span class=""alarm"">" + stringa_split(i) + "*" + "</span></li>"
                                stringa_split(i) = sub_str
                            Else
                                sub_str = "<li>" + stringa_split(i) + "</li>"
                                stringa_split(i) = sub_str
                            End If
                            nessuno = True
                        End If
                        If InStr(stringa_split(i), "Dos") <> 0 Then
                            If (InStr(stringa_split(i), "Yes") <> 0 And asterisco = False And nessuno = False) Or (InStr(stringa_split(i), "*") <> 0 And asterisco = True And nessuno = False) Then
                                sub_str = "<li><span class=""alarm"">" + stringa_split(i) + "*" + "</span></li>"
                                stringa_split(i) = sub_str
                            Else
                                sub_str = "<li>" + stringa_split(i) + "</li>"
                                stringa_split(i) = sub_str
                            End If
                            nessuno = True
                        End If
                        If InStr(stringa_split(i), "ON") <> 0 Then
                            If InStr(stringa_split(i), "Yes") <> 0 Then
                                sub_str = "<li><span class=""pump"">" + stringa_split(i) + "</span></li>"
                                stringa_split(i) = sub_str
                            Else
                                sub_str = "<li>" + stringa_split(i) + "</li>"
                                stringa_split(i) = sub_str
                            End If
                            nessuno = True
                        End If
                        If InStr(stringa_split(i), "OFF") <> 0 Then
                            nessuno = True
                            sub_str = "<li>" + stringa_split(i) + "</li>"
                            stringa_split(i) = sub_str
                        End If
                        If InStr(stringa_split(i), "Dis") <> 0 Then
                            nessuno = True
                            sub_str = "<li>" + stringa_split(i) + "</li>"
                            stringa_split(i) = sub_str
                        End If
                        If nessuno = False Then
                            If stringa_split(i).Length > 2 Then
                                sub_str = "<li>" + stringa_split(i) + "</li>"
                                stringa_split(i) = sub_str
                            Else
                                stringa_split(i) = ""
                            End If
                        End If
                    End If
                    If tipo_strumento = "MTower" Then 'caso Mtower
                        If InStr(stringa_split(i), "ID") <> 0 Then
                            nessuno = True
                        End If
                        If InStr(stringa_split(i), "Status") <> 0 Then
                            nessuno = True
                        End If

                        If InStr(stringa_split(i), "Flow") <> 0 Then
                            If (InStr(stringa_split(i), "No") <> 0 And asterisco = False And nessuno = False) Or (InStr(stringa_split(i), "*") <> 0 And asterisco = True And nessuno = False) Then
                                sub_str = "<li><span class=""alarm"">" + stringa_split(i) + "*" + "</span></li>"
                                stringa_split(i) = sub_str
                            Else
                                sub_str = "<li>" + stringa_split(i) + "</li>"
                                stringa_split(i) = sub_str
                            End If
                            nessuno = True
                        End If
                        If (InStr(stringa_split(i), "Lo") <> 0 And asterisco = False And nessuno = False) Or (InStr(stringa_split(i), "*") <> 0 And asterisco = True And nessuno = False) Then
                            nessuno = True
                            sub_str = "<li><span class=""alarm"">" + stringa_split(i) + "*" + "</span></li>"
                            stringa_split(i) = sub_str
                        End If
                        If (InStr(stringa_split(i), "On") <> 0 And asterisco = False And nessuno = False) Or (InStr(stringa_split(i), "*") <> 0 And asterisco = True And nessuno = False) Then
                            nessuno = True
                            sub_str = "<li><span class=""alarm"">" + stringa_split(i) + "*" + "</span></li>"
                            stringa_split(i) = sub_str
                        End If
                        If nessuno = False Then
                            If stringa_split(i).Length > 2 Then
                                sub_str = "<li>" + stringa_split(i) + "</li>"
                                stringa_split(i) = sub_str
                            Else
                                stringa_split(i) = ""
                            End If
                        End If
                    End If
                    If tipo_strumento = "MAX5" Then 'caso Mtower
                        If InStr(stringa_split(i), "Lv") <> 0 Or InStr(stringa_split(i), "In") <> 0 Or InStr(stringa_split(i), "Aa") <> 0 Or InStr(stringa_split(i), "Ab") <> 0 Or InStr(stringa_split(i), "Ad") <> 0 Or InStr(stringa_split(i), "Ar") <> 0 Or InStr(stringa_split(i), "Flow") <> 0 Or asterisco = True Then
                            If (asterisco = False And nessuno = False) Or (InStr(stringa_split(i), "*") <> 0 And asterisco = True And nessuno = False) Then
                                sub_str = "<li><span class=""alarm"">" + stringa_split(i) + "*" + "</span></li>"
                                stringa_split(i) = sub_str
                            Else
                                If stringa_split(i).Length > 2 Then
                                    sub_str = "<li>" + stringa_split(i) + "</li>"
                                    stringa_split(i) = sub_str
                                Else
                                    stringa_split(i) = ""
                                End If
                            End If
                            nessuno = True
                        End If
                        If nessuno = False Then
                            If stringa_split(i).Length > 2 Then
                                sub_str = "<li>" + stringa_split(i) + "</li>"
                                stringa_split(i) = sub_str
                            Else
                                stringa_split(i) = ""
                            End If
                        End If

                    End If
                Next
                oMessage.Body = "<!DOCTYPE html PUBLIC ""-//W3C//DTD XHTML 1.0 Transitional//EN"" ""http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"">" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "<html xmlns=""http://www.w3.org/1999/xhtml"">" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "<head>" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "<meta http-equiv=""Content-Type"" content=""text/html; charset=UTF-8""/>" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "<title>Documento senza titolo</title>" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "<style type=""text/css"">" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "body{" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "font-family: ""Helvetica Neue"", Helvetica, Arial, sans-serif;" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "font-size: 12px;" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "line-height: 20px;" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "	}" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + ".widget{" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "border: 1px solid #CCC;" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "}" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "div{" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "	display:block;" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "	padding:5px;" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "	}" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "li {" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "	font-size:14px;" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "	}" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + ".alarm{" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "	font-weight:bold;" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "	color:red;" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "	}" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + ".pump{" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "	font-weight:bold;" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "	color:green;" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "	}" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "</style>" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "</head>" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + " <body>" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + " <div class=""widget"">" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "<div style=""background-image:url(http://www.ermes-server.com/mail_alarm/logo.gif); padding: 20px 0 20px 100px; color:#495d62;""><div style=""width:175px;height:23px;background-repeat:no-repeat;border-style:none;"" ><a href=""http://www.ermes-server.com/""><img src=""http://www.ermes-server.com/mail_alarm/server.png""/></a></div></div>"
                If impianto = "" Then
                    oMessage.Body = oMessage.Body + "<div style=""padding-top:20px;margin-bottom:-20px;""><h3>ALARM</h3>  </div>"
                    'oMessage.Body = oMessage.Body + " <div style=""background-image:url(http://www.ermes-server.com/mail_alarm/logo.gif); padding: 20px 0 20px 100px; color:#495d62;""><h3>ALARM</h3></div>" + Chr(13) + Chr(10)
                Else
                    oMessage.Body = oMessage.Body + "<div style=""padding-top:20px;margin-bottom:-20px;""><h3>ALARM - " + impianto + "</h3>  </div>"
                    'oMessage.Body = oMessage.Body + " <div style=""background-image:url(http://www.ermes-server.com/mail_alarm/logo.gif); padding: 20px 0 20px 100px; color:#495d62;""><h3>ALARM - " + impianto + "</h3></div>" + Chr(13) + Chr(10)
                End If
                oMessage.Body = oMessage.Body + "<div style=""background-color:#495d62; margin-top:4px;padding:20px; color:#ffffff; width:100px;position:relative;float:left;height:37px;""><h3>" + tipo_strumento + "</h3></div>" + Chr(13) + Chr(10)
                If tipo_strumento = "MAX5" Then 'caso Mtower
                    oMessage.Body = oMessage.Body + "<div style=""background-image:url(http://www.ermes-server.com/mail_alarm/bg.gif); padding: 23px 0 15px 160px;color:#495d62; ""><h3>" + messaggio_mail(0) + "</h3></div>" + Chr(13) + Chr(10)
                Else
                    oMessage.Body = oMessage.Body + "<div style=""background-image:url(http://www.ermes-server.com/mail_alarm/bg.gif); padding: 23px 0 15px 160px;color:#495d62; ""><h3>" + stringa_split(0) + "</h3></div>" + Chr(13) + Chr(10)
                End If
                oMessage.Body = oMessage.Body + "<div style="""">" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "<ul>" + Chr(13) + Chr(10)
                For i = partenza To stringa_split.Length - 1
                    If stringa_split(i) <> "" Then
                        oMessage.Body = oMessage.Body + stringa_split(i) + Chr(13) + Chr(10)
                    End If
                Next
                oMessage.Body = oMessage.Body + "</ul>" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "</div>" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "</div>" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "</body>" + Chr(13) + Chr(10)
                oMessage.Body = oMessage.Body + "</html>" + Chr(13) + Chr(10)
                oMessage.BodyEncoding = System.Text.Encoding.UTF8
                oMessage.IsBodyHtml = True
            Else
                If impianto = "" Then
                    oMessage.Subject = "Alarm"
                Else
                    oMessage.Subject = "Alarm" + "-" + impianto
                End If
                oMessage.Body = messaggio_mail(0) + Chr(13) + Chr(10)
            End If
            client.EnableSsl = True
            client.Send(oMessage)
            ' cc.stato = True
            'End If
        Catch
        End Try
        'Next

    End Sub
End Class