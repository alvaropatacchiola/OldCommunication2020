Imports System.Threading
Imports System.Net.Sockets
Imports System.Net

Imports System.Globalization

Public Class OldCommunication
    Public Shared stack_connessioni As Queue
    Public Shared stack_anomalie As Queue
    Public Shared stack_ip As Queue
    Friend WithEvents Timer1 As System.Timers.Timer
    Friend WithEvents Timer2 As System.Timers.Timer
    Friend WithEvents Timer_fiera As System.Timers.Timer
    Private Shared listener As System.Net.Sockets.TcpListener
    Private Shared listener1 As System.Net.Sockets.TcpListener
    Private Const BYTES_TO_READ As Integer = 255
    Private readBuffer(BYTES_TO_READ) As Byte

    Private Shared listenThread As System.Threading.Thread
    Private Shared listenThread1 As System.Threading.Thread
    Private Shared sendThread As System.Threading.Thread
    Private Shared sendThread1 As System.Threading.Thread
    Private trd As Thread
    Private trd1 As Thread
    Private trd2 As Thread

    Private Shared lista_strumenti As New List(Of strumento)
    Public query As New query

    Protected Overrides Sub OnStart(ByVal args() As String)

        ' Inserire qui il codice necessario per avviare il proprio servizio. Il metodo deve effettuare
        ' le impostazioni necessarie per il funzionamento del servizio.
        create_connection()
        sendTelegram("1187580764:AAFA8pdGwgS5aEGQvK8_2eZkkAv_OfRt0Mc", "Avvio Servizio Old Communication")
    End Sub

    Protected Overrides Sub OnStop()
        ' Inserire qui il codice delle procedure di chiusura necessarie per arrestare il proprio servizio.
        closeConnection()
    End Sub

    Public Sub closeConnection()
        Try
            For Each strumento_temp In lista_strumenti
                strumento_temp.puntatore_strumento.close_connection()
                strumento_temp.puntatore_strumento.remove = True
            Next
        Catch ex As Exception

        End Try
        sendTelegram("1187580764:AAFA8pdGwgS5aEGQvK8_2eZkkAv_OfRt0Mc", "Stop Servizio Old Communication")
    End Sub
    Public Function sendTelegram(ByVal idTelegram As String, ByVal textToSend As String) As Boolean

        Dim response As String = ""
        Dim id As String = ""
        ServicePointManager.Expect100Continue = True
        ServicePointManager.SecurityProtocol = SecurityProtocolType.Tls Or SecurityProtocolType.Ssl3 Or 768 Or 3072

        Using wb As WebClient = New WebClient()
            '            response = wb.DownloadString("https://api.telegram.org/bot" + idTelegram + "/getUpdates")
            response = wb.DownloadString("https://api.telegram.org/bot" + idTelegram + "/getUpdates")

        End Using
        'response = "[{""ID"":1,""Officer"":""John Doe"",""Type"":""Moving Violation"",""Amount"":[{""Account"":100,""Price"":342},{""Account"":200,""Price"":60},{""Acctount"":300,""Price"":10}]},{""ID"":2,""Officer"":""Jane Doe"",""Type"":""Non-moving Violation"",""Amount"":[{""Account"":700,""Price"":155},{""Account"":300,""Price"":20},{""Account"":200,""Price"":120},{""Account"":500,""Price"":50}]}]"
        If (response.IndexOf("""id"":") > 0) Then
            id = Mid(response, response.IndexOf("""id"":") + 6, response.Length)
            id = Mid(id, 1, id.IndexOf(","))
            Using wb1 As WebClient = New WebClient()
                response = wb1.DownloadString("https://api.telegram.org/bot" + idTelegram + "/sendMessage?chat_id=283487656&text=" + textToSend)
                Return True
            End Using
        End If
        Return False

    End Function
    Public Function create_connection() As Boolean
        'query.create_db_connection()



        stack_connessioni = New Queue()
        stack_anomalie = New Queue()
        stack_ip = New Queue()
        ' Dim address As IPAddress = IPAddress.Parse("95.110.157.172") 'server 2020
        Dim address As IPAddress = IPAddress.Parse("95.110.157.234") 'server centurio
        condivisa.crea_coda()

        Try
            If listener Is Nothing Then

                listener = New System.Net.Sockets.TcpListener(System.Net.IPAddress.Any, "2020") 'The TcpListener will listen for incoming connections at port 43001        
                'listener = New System.Net.Sockets.TcpListener(address, "80") 'The TcpListener will listen for incoming connections at port 43001        
                listener.Start() 'Start listening.
                listenThread = New System.Threading.Thread(AddressOf doListen) 'This thread will run the doListen method        
                listenThread.IsBackground = True 'Since we dont want this thread to keep on running after the application closes, we set isBackground to true.        
                listenThread.Start() 'Start executing doListen on the worker thread.    
                sendThread = New System.Threading.Thread(AddressOf doSend) 'This thread will run the doListen method        
                sendThread.IsBackground = True 'Since we dont want this thread to keep on running after the application closes, we set isBackground to true.        
            End If
            If listener1 Is Nothing Then
                listener1 = New System.Net.Sockets.TcpListener(System.Net.IPAddress.Any, "2019") 'The TcpListener will listen for incoming connections at port 43001        
                'listener1 = New System.Net.Sockets.TcpListener(address, "2018") 'The TcpListener will listen for incoming connections at port 43001        
                listener1.Start() 'Start listening.        
                listenThread1 = New System.Threading.Thread(AddressOf doListen1) 'This thread will run the doListen method        
                listenThread1.Start() 'Start executing doListen on the worker thread.    
                sendThread1 = New System.Threading.Thread(AddressOf doSend_command) 'This thread will run the doListen method        
                sendThread1.IsBackground = True 'Since we dont want this thread to keep on running after the application closes, we set isBackground to true.        
            End If

        Catch ex As Exception
            stack_anomalie.Enqueue(Now.ToString + "|Polling porta 2020|" + ex.ToString)
            'Label1.Text = Now.ToString + "|Polling porta 2020|" + ex.ToString
            Return False
        End Try
        'Timer_fiera = New System.Timers.Timer
        'Timer_fiera.Interval = 300000
        'Timer_fiera.Start()

        'Timer1 = New System.Timers.Timer
        'Timer1.Interval = 300000
        'Timer1.Enabled = True
        'Timer1.Start()
        trd = New Thread(AddressOf ThreadTask1)
        trd.Start()

        trd1 = New Thread(AddressOf ThreadTask2)
        trd1.Start()

        trd2 = New Thread(AddressOf ThreadTask3)
        trd2.Start()

        'Try
        '    Dim service As ServiceReference1.Service1SoapClient = New ServiceReference1.Service1SoapClient
        '    Dim result As Boolean
        '    result = service.client_connect()
        'Catch ex As Exception
        '    stack_anomalie.Enqueue(Now.ToString + "|Client Connection Local Web service|" + ex.ToString)
        'End Try
        Return True
        'txtUserName.Enabled = False
    End Function
    Private Sub doListen1()
        ' Dim test As String
        Dim incomingClient As System.Net.Sockets.TcpClient
        Do
            Try
                incomingClient = listener1.AcceptTcpClient 'Accept the incoming connection. This is a blocking method so execution will halt here until someone tries to connect.            

                Dim connClient As New ConnectedClient_command(incomingClient) 'Create a new instance of ConnectedClient (check its constructor to see whats happening now).
                AddHandler connClient.dataReceived, AddressOf connClient.messageReceived
                'clients.Add(connClient) 'Adds the connected client to the list of connected clients.

            Catch ex As Exception

            End Try
        Loop
    End Sub
    Public Function check_ip(ByVal ip_temp As String) As Boolean
        Dim righe As Integer
        Try
            righe = query.get_ip(ip_temp).Count
        Catch ex As Exception
            righe = 0
        End Try
        If righe = 0 Then
            Return True
        Else
            Return False
        End If
    End Function
    Private Sub doListen()
        Dim ip_connect As String
        Dim incomingClient As System.Net.Sockets.TcpClient
        Do
            Try
                'If listener.Pending Then
                'Thread.Sleep(100)  ' Sleep 100 msec.
                incomingClient = listener.AcceptTcpClient 'Accept the incoming connection. This is a blocking method so execution will halt here until someone tries to connect.            

                ip_connect = IPAddress.Parse(CType(incomingClient.Client.RemoteEndPoint, IPEndPoint).Address.ToString()).ToString
                'stack_ip.Enqueue(ip_connect)
                'If check_ip(stack_ip.Dequeue) Then
                Dim connClient As New ConnectedClient(incomingClient) 'Create a new instance of ConnectedClient (check its constructor to see whats happening now).
                AddHandler connClient.dataReceived, AddressOf connClient.messageReceived
                'clients.Add(connClient) 'Adds the connected client to the list of connected clients.
                connClient.time_connessione = Now
                connClient.ip_number = ip_connect
                'connClient.ip_info = query.get_ip_config(ip_connect)
                connClient.status_connessione = "coda" ' coda analisi connessione
                stack_connessioni.Enqueue(connClient)
                'End If
                'decode_coda()
                ' End If
            Catch ex As Exception

            End Try
        Loop
    End Sub
    Private Sub doSend()

    End Sub
    Private Sub doSend_command()

    End Sub
    Public Sub aggiorna_lista()
        'Label1.Text = ""
        'For Each strumento_temp In lista_strumenti
        '    Dim stringa As String = ""
        '    stringa = strumento_temp.identificativo + "," + strumento_temp.sequnza_interrogazione.ToString + vbCrLf

        '    Label1.Text = Label1.Text + stringa

        'Next
    End Sub
    Private Sub ThreadTask3()
        While True
            'trd1.Suspend()


            Dim lista_strumenti_temp As New List(Of strumento)(lista_strumenti)
            ' Dim lista_strumenti_temp = lista_strumenti.Select(Function(innerList) innerList.ToList).ToList
            'lista_strumenti_temp.Items.Add(lista_strumenti.Clone())


            'lista_strumenti_temp = lista_strumenti
            ' Try
            For Each strumento_temp In lista_strumenti_temp
                Try
                    If strumento_temp.puntatore_strumento.remove = False Then
                        'If strumento_temp.busy_strumento_invio Then ' strumento occupato per invio
                        '    Return False ' invio impossibile poichè lo strumento è occupato
                        'Else
                        strumento_temp.update_database()
                        Thread.Sleep(500)
                        'faccio partire un timer di attesa risposta per liberare poi lo strumento

                        ' End If

                    End If
                Catch ex As Exception

                End Try
            Next
            'Catch ex As Exception
            'trd1.Resume()

            'End Try
            Thread.Sleep(350000)
        End While

    End Sub

    'Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Elapsed
    Private Sub ThreadTask2()
        While True
            free_memory()

            Try
                If stack_connessioni.Count > 0 Then
                    Dim stack_connessi_coda As ConnectedClient
                    stack_connessi_coda = stack_connessioni.Dequeue
                    If stack_connessi_coda.status_connessione = "connesso#m5" Then
                        Dim temp_strumento As strumento = check_strumento(stack_connessi_coda.identificativo_client, False)

                        Dim creazione_strumento As strumento = New strumento ' se non è presente nella lista degli strumenti creo una nuova struttura strumento con il puntatore al client stryumento
                        creazione_strumento.identificativo = stack_connessi_coda.identificativo_client
                        creazione_strumento.puntatore_strumento = stack_connessi_coda

                        If temp_strumento Is Nothing Then
                            lista_strumenti.Add(creazione_strumento)
                        Else
                            lista_strumenti.Item(lista_strumenti.IndexOf(temp_strumento)) = creazione_strumento
                        End If
                    Else
                        If stack_connessi_coda.status_connessione = "connesso#pc" Then
                            Dim temp_pc As strumento = check_strumento(stack_connessi_coda.identificativo_client, True)
                            If temp_pc Is Nothing Then
                                stack_connessi_coda.assegnato_pc = False
                                stack_connessi_coda.SendMessage("NoConnected") 'nessuno strumento connesso
                                stack_connessi_coda.close_connection()
                            Else
                                stack_connessi_coda.SendMessage(temp_pc.set_pc_puntatore(stack_connessi_coda))
                                stack_connessi_coda.assegnato_pc = True
                                'restituisce al pc connected se la connessione è nel limite delle 4 altrimenti errore
                            End If
                        Else
                            If stack_connessi_coda.status_connessione = "rifiuta#m5" Then
                                stack_connessi_coda.close_connection()
                            Else
                                Dim data_corrente As Long
                                data_corrente = DateDiff(DateInterval.Second, stack_connessi_coda.time_connessione, Now)
                                If data_corrente < 60 Then
                                    stack_connessioni.Enqueue(stack_connessi_coda)
                                Else
                                    stack_connessi_coda.close_connection()
                                End If
                            End If
                        End If
                    End If
                End If
            Catch ex As Exception

            End Try
            If stack_connessioni.Count > 20 Then
                Thread.Sleep(1)
            Else
                Thread.Sleep(200)
            End If

        End While

    End Sub
    Private Sub ThreadTask1()

        'Timer1.Stop()

        While True


            If condivisa.coda_comandi.Count > 0 Then
                Dim messaggio_coppia As coppia
                messaggio_coppia = condivisa.remove_comandi
                Dim data() As String = messaggio_coppia.messaggio.Split(";")
                Dim risultato As Boolean
                If data.Length = 8 Then
                    Select Case data(0)
                        Case "READSETPOINT"
                            Dim risultato_str As String = ""
                            risultato_str = read_set_point(data(1), data(2))
                            'risultato = True
                            messaggio_coppia.server_connesso.SendMessage("LOCALCOMMAND|" + risultato_str + "|end")

                        Case "WRITESETPOINT"
                            Try
                                Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log_send_setpoint.txt", True)
                                    writer.WriteLine("#" + Now.ToString + "#" + data(1) + "#" + data(2) + "#" + data(3) + "#")
                                End Using
                            Catch ex As Exception

                            End Try
                            risultato = write_set_point(data(1), data(2), data(3))
                            'risultato = True
                            messaggio_coppia.server_connesso.SendMessage("LOCALCOMMAND|" + risultato.ToString + "|end")
                        Case "WRITEBIOCIDE"
                            risultato = write_set_point_biocide1(data(1), data(2), data(3), data(4), data(5), data(6), data(7))
                            messaggio_coppia.server_connesso.SendMessage("LOCALCOMMAND|" + risultato.ToString + "|end")

                        Case "WAITSETPOINT"
                            Dim coppia(2) As Boolean
                            coppia = wait_response_set_point(data(1))
                            'coppia(0) = False
                            'coppia(1) = True
                            'If coppia(0) = True Then
                            '    re_write_set_point(data(1))
                            'End If
                            messaggio_coppia.server_connesso.SendMessage("LOCALCOMMAND|" + coppia(0).ToString + ";" + coppia(1).ToString + "|end")
                        Case "FLOWTIME"
                            Dim coppia(2) As Long
                            coppia = get_flow_time(data(1), data(2))
                            messaggio_coppia.server_connesso.SendMessage("LOCALCOMMAND|" + coppia(0).ToString + ";" + coppia(1).ToString + "|end")
                        Case "GETALL"
                            Dim lista_strumenti_temp As New List(Of strumento)
                            Dim strumentiConnessi As String = ""
                            lista_strumenti_temp = lista_strumenti


                            For Each strumento_temp In lista_strumenti_temp
                                Try
                                    If strumento_temp.puntatore_strumento.remove = False Then
                                        strumentiConnessi = strumentiConnessi + strumento_temp.identificativo + ";"
                                        'If (strumento_temp.identificativo = codice) Then
                                        'If strumento_temp.busy_strumento_invio Then ' strumento occupato per invio
                                        '    Return False ' invio impossibile poichè lo strumento è occupato
                                        'Else

                                        ' End If

                                    End If
                                Catch ex As Exception

                                End Try
                            Next
                            messaggio_coppia.server_connesso.SendMessage("LOCALCOMMAND|" + strumentiConnessi + "|end")
                        Case "REALTIME"
                            Try
                                Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log_refresh.txt", True)
                                    writer.WriteLine("#" + Now.ToString + "#" + data(1) + "#" + data(2) + "#" + "#")
                                End Using
                            Catch ex As Exception

                            End Try

                            risultato = read_real_time(data(1), data(2))
                            messaggio_coppia.server_connesso.SendMessage("LOCALCOMMAND|" + risultato.ToString + "|end")


                    End Select

                End If
            End If
            Try
                If listenThread.IsAlive = False Then
                    'Timer1.Start()
                End If
            Catch ex As Exception

            End Try
            Thread.Sleep(10)
        End While
        'Timer1.Start()
    End Sub
    Public Function wait_response_set_point(ByVal codice As String) As Boolean()
        Dim coppia(2) As Boolean
        Dim lista_strumenti_temp As New List(Of strumento)
        lista_strumenti_temp = lista_strumenti

        For Each strumento_temp In lista_strumenti_temp
            Try

                If (strumento_temp.identificativo = codice) And strumento_temp.puntatore_strumento.remove = False Then
                    coppia(0) = strumento_temp.busy_strumento_invio
                    coppia(1) = strumento_temp.wait_response_invio
                    Return coppia
                End If
            Catch ex As Exception

            End Try
        Next
        coppia(0) = False
        coppia(1) = False
        Return coppia
    End Function
    Public Sub free_memory()
        Try
            For Each strumento_temp In lista_strumenti
                Try
                    If strumento_temp.puntatore_strumento.remove = True Then
                        lista_strumenti.Remove(strumento_temp)
                        Exit Sub
                    End If
                Catch ex As Exception
                    lista_strumenti.Remove(strumento_temp)
                End Try
            Next
        Catch ex As Exception
        End Try
    End Sub
    Public Function re_write_set_point(ByVal codice As String) As Boolean
        Dim lista_strumenti_temp As New List(Of strumento)
        lista_strumenti_temp = lista_strumenti

        For Each strumento_temp In lista_strumenti_temp
            Try
                If (strumento_temp.identificativo = codice) And strumento_temp.puntatore_strumento.remove = False Then
                    'If (strumento_temp.identificativo = codice) Then
                    'If strumento_temp.busy_strumento_invio Then ' strumento occupato per invio
                    '    Return False ' invio impossibile poichè lo strumento è occupato
                    'Else
                    write_set_point(codice, strumento_temp.id_invio, strumento_temp.new_setpoint)
                    'faccio partire un timer di attesa risposta per liberare poi lo strumento
                    Return True 'setpoint inviato correttamente
                    ' End If

                End If
            Catch ex As Exception

            End Try
        Next
    End Function
    Public Function read_set_point(ByVal codice As String, ByVal id As String) As String
        Dim cliente_result As String = ""
        Dim lista_strumenti_temp As New List(Of strumento)
        lista_strumenti_temp = lista_strumenti

        Try
            For Each strumento_temp In lista_strumenti_temp
                Try
                    If (strumento_temp.identificativo = codice) And strumento_temp.puntatore_strumento.remove = False Then
                        'If strumento_temp.busy_strumento_invio Then ' strumento occupato per invio
                        '    Return False ' invio impossibile poichè lo strumento è occupato
                        'Else
                        cliente_result = strumento_temp.insieme_strumenti(Val(id)).time_connessione + "!"
                        cliente_result = cliente_result + codice + "!"
                        cliente_result = cliente_result + id + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).tipo_strumento + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).nome_strumento + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value1 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value2 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value3 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value4 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value5 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value6 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value7 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value8 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value9 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value10 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value11 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value12 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value13 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value14 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value15 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value16 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value17 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value18 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value19 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value20 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value21 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value22 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value23 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value24 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value25 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value26 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value27 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value28 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).value29 + "!"
                        cliente_result = cliente_result + strumento_temp.insieme_strumenti(Val(id)).time_no_flow.ToString + "!"

                        'faccio partire un timer di attesa risposta per liberare poi lo strumento
                        Return cliente_result 'setpoint inviato correttamente
                        ' End If

                    End If
                Catch ex As Exception

                End Try
            Next

        Catch ex As Exception

        End Try
        Return cliente_result
    End Function
    Public Function write_set_point(ByVal codice As String, ByVal id As String, ByVal new_setpoint As String) As Boolean
        Dim cliente_result As String = ""
        Dim lista_strumenti_temp As New List(Of strumento)
        lista_strumenti_temp = lista_strumenti

        Try
            For Each strumento_temp In lista_strumenti_temp
                Try
                    If (strumento_temp.identificativo = codice) And strumento_temp.puntatore_strumento.remove = False Then
                        'If (strumento_temp.identificativo = codice) Then
                        'If strumento_temp.busy_strumento_invio Then ' strumento occupato per invio
                        '    Return False ' invio impossibile poichè lo strumento è occupato
                        'Else
                        strumento_temp.Timer3.Enabled = True
                        strumento_temp.Timer3.Start()
                        strumento_temp.busy_strumento_invio = True ' occupo lo strumento
                        strumento_temp.wait_response_invio = False
                        strumento_temp.lock_reading()
                        strumento_temp.puntatore_strumento.SendMessage(new_setpoint)
                        strumento_temp.new_setpoint = new_setpoint
                        strumento_temp.id_invio = id
                        'faccio partire un timer di attesa risposta per liberare poi lo strumento
                        Return True 'setpoint inviato correttamente
                        ' End If

                    End If
                Catch ex As Exception

                End Try
            Next

        Catch ex As Exception

        End Try
    End Function

    Public Function write_set_point_biocide1(ByVal codice As String, ByVal id As String, ByVal new_setpoint1 As String, ByVal new_setpoint2 As String, ByVal new_setpoint3 As String, ByVal new_setpoint4 As String, ByVal new_setpoint5 As String) As Boolean
        Dim cliente_result As String = ""
        Dim result_send As Boolean = False
        Dim i As Integer
        Dim lista_strumenti_temp As New List(Of strumento)
        lista_strumenti_temp = lista_strumenti

        Try
            For Each strumento_temp In lista_strumenti_temp
                Try

                    If (InStr(strumento_temp.identificativo, codice) <> 0) And strumento_temp.puntatore_strumento.remove = False Then
                        ' If (InStr(strumento_temp.identificativo, codice) <> 0) Then

                        For i = 1 To 5
                            Select Case i
                                Case 1
                                    result_send = send_setpoint_biocide(strumento_temp, id, new_setpoint1)
                                Case 2
                                    result_send = send_setpoint_biocide(strumento_temp, id, new_setpoint2)
                                Case 3
                                    result_send = send_setpoint_biocide(strumento_temp, id, new_setpoint3)
                                Case 4
                                    result_send = send_setpoint_biocide(strumento_temp, id, new_setpoint4)
                                Case 5
                                    result_send = send_setpoint_biocide(strumento_temp, id, new_setpoint5)
                            End Select
                            If result_send = False Then
                                Return False
                            End If
                            Dim risultato(2) As Boolean
                            risultato = wait_response_set_point(codice)
                            While risultato(0) = True
                                risultato = wait_response_set_point(codice)
                            End While
                        Next

                        Return True
                    End If
                Catch ex As Exception

                End Try
            Next

        Catch ex As Exception

        End Try
    End Function
    Public Function send_setpoint_biocide(ByVal strumento_temp As strumento, ByVal id As String, ByVal new_setpoint As String) As Boolean
        Dim i As Integer = 0
        For i = 0 To 3
            If strumento_temp.busy_strumento_invio Then ' strumento occupato per invio
                'Return False ' invio impossibile poichè lo strumento è occupato
            Else
                strumento_temp.Timer3.Enabled = True
                strumento_temp.Timer3.Start()
                strumento_temp.busy_strumento_invio = True ' occupo lo strumento
                strumento_temp.wait_response_invio = False
                strumento_temp.lock_reading()
                strumento_temp.puntatore_strumento.SendMessage(new_setpoint)
                strumento_temp.id_invio = id

                'faccio partire un timer di attesa risposta per liberare poi lo strumento
                Return True 'setpoint inviato correttamente
            End If
        Next
        Return False
    End Function
    Private Function check_strumento(ByVal identificativo_search As String, ByVal pc As Boolean) As strumento
        Dim strumento_temp As strumento = Nothing

        For Each ss In lista_strumenti
            If ss.identificativo = identificativo_search Then 'elemto gia allocato
                If pc Then
                    Try
                        If ss.puntatore_strumento.remove Then
                            Return strumento_temp
                        End If
                    Catch ex As Exception
                        Return strumento_temp
                    End Try
                End If
                strumento_temp = ss
                Exit For
            End If
        Next
        Return strumento_temp
    End Function
    Public Function get_flow_time(ByVal codice As String, ByVal id As String) As Long()
        Dim cliente_result As String = ""
        Dim risultato(2) As Long
        risultato(0) = 0
        risultato(1) = 99
        Dim lista_strumenti_temp As New List(Of strumento)
        lista_strumenti_temp = lista_strumenti

        Try
            For Each strumento_temp In lista_strumenti_temp
                If (InStr(strumento_temp.identificativo, codice) <> 0) Then

                    Dim i As Integer = Val(id)
                    risultato(0) = DateDiff(DateInterval.Minute, strumento_temp.insieme_strumenti(i).start_data_flusso, strumento_temp.insieme_strumenti(i).current_data_flusso)
                    risultato(1) = DateDiff(DateInterval.Minute, strumento_temp.insieme_strumenti(i).time_connessione, Now)
                    Return risultato
                End If
            Next

        Catch ex As Exception
            Return risultato
        End Try
        Return risultato
    End Function
    Public Function read_real_time(ByVal codice As String, ByVal id_485 As String) As Boolean
        Dim cliente_result As String = ""
        Dim lista_strumenti_temp As New List(Of strumento)
        lista_strumenti_temp = lista_strumenti


        Try
            For Each strumento_temp In lista_strumenti_temp
                Try


                    '                    If (strumento_temp.identificativo = codice) And strumento_temp.puntatore_strumento.remove = False Then
                    If (strumento_temp.identificativo = codice) Then
                        If strumento_temp.nuovo_strumento_connesso Then
                            Return True
                        End If
                        strumento_temp.read_realtime(Val(id_485), False)
                        strumento_temp.save_read_realtime(Val(id_485))
                        Return False
                    End If
                Catch ex As Exception

                End Try
            Next

        Catch ex As Exception

        End Try
        Return True
    End Function
End Class
