Imports System.Threading
Imports System.IO
Imports System.Data.SqlClient
Public Class strumento
    Public stack_messaggi As Queue
    Private midentificativo As String
    Private mstrumento_tx As String
    Private mstrumento_rx As String
    Private mpuntatore_strumento As ConnectedClient
    Private puntatore_pc1 As pc
    Private puntatore_pc2 As pc
    Private puntatore_pc3 As pc
    Private puntatore_pc4 As pc
    Private puntatore_pc_scrittura_setpoint As pc
    Public insieme_strumenti(31) As light ' array di connessioni su 485
    Public numberid_485 As Integer ' max id presente
    Private maxid_485 As Integer ' max id check
    Private setpoint_modify As Boolean 'stato per indicare se uno strumento è occupato o meno per la modifica set_point
    Friend WithEvents Timer1 As System.Timers.Timer
    Friend WithEvents Timer2 As System.Timers.Timer
    Friend WithEvents Timer3 As System.Timers.Timer
    Friend WithEvents Timer4 As System.Timers.Timer
    Friend WithEvents Timer5 As System.Timers.Timer
    'Friend WithEvents Timer6 As System.Timers.Timer
    Friend WithEvents Timer_refresh As System.Timers.Timer

    Public nuovo_strumento_connesso As Boolean
    Public sequnza_interrogazione As Integer
    Public numero_interrogazione As Integer
    Public tipo_strumento_sequenza As String
    Public sequenza_timer As Boolean
    'web service invio set poit da web variabili
    Public busy_strumento_invio As Boolean
    Public wait_response_invio As Boolean
    Public id_invio As String
    Public reading_sp As Boolean
    Public reading_sp_bio1 As Boolean
    Public reading_sp_bio2 As Boolean
    'web service lettuera log
    Public busy_log As Boolean
    Public log_completed As Boolean
    Public log_fail As Integer
    Public first_log As Boolean
    Public blocco_pc As Boolean
    Public counter_next_log As Integer
    Public message_file_check As String
    Public id_485_log As Integer
    Private buffer_sp1 As String
    Private buffer_sp2 As String
    Private buffer_sp3 As String
    Private buffer_sp4 As String
    Private buffer_sp5 As String
    Private decode_log_ptr As decode_log
    Public real_time_bool As Boolean
    Public query As New query
    Public counter_refresh_old As Integer
    Public check_new_instruments As Boolean = False
    Public counter_double_log As Integer
    Public contatore_nuovi As Integer
    Dim priority_setpoint As Boolean = False
    Public new_setpoint As String = ""

    Sub New()

        init_strumento()

    End Sub
    Public Sub init_strumento()
        Dim i As Integer
        For i = 1 To 30
            insieme_strumenti(i) = Nothing
        Next
        query = New query
        counter_double_log = 0
        maxid_485 = 1
        busy_strumento_invio = False
        wait_response_invio = False
        priority_setpoint = False
        reading_sp = False
        reading_sp_bio1 = False
        reading_sp_bio2 = False
        puntatore_pc1 = New pc()
        puntatore_pc2 = New pc()
        puntatore_pc3 = New pc()
        puntatore_pc4 = New pc()
        decode_log_ptr = New decode_log()
        puntatore_pc_scrittura_setpoint = New pc()
        puntatore_pc_scrittura_setpoint.scrittura_sp = False
        'puntatore_pc_scrittura_setpoint = Nothing
        Timer1 = New System.Timers.Timer
        Timer1.Interval = 2000 ' timer che parte alla ricerca delola configurazione dello strumento
        Timer1.Enabled = False
        Timer2 = New System.Timers.Timer
        Timer2.Interval = 2000 ' timer che a runtime interroga e mantiene aggiornato lo stato degli stumenti
        Timer_refresh = New System.Timers.Timer
        Timer_refresh.Interval = 300000
        Timer_refresh.Enabled = False
        'Timer2.Interval = 60000 ' timer che a runtime interroga e mantiene aggiornato lo stato degli stumenti
        Timer2.Enabled = False
        Timer3 = New System.Timers.Timer
        Timer3.Interval = 10000 ' che gestisce i tempi di risposta al cambiamento set point 20 secondi risposta
        'Timer3.Enabled = False


        Timer4 = New System.Timers.Timer
        Timer5 = New System.Timers.Timer
        'Timer6 = New System.Timers.Timer

        Timer4.Interval = 1800000 ' che gestisce i tempi di risposta al cambiamento set point 20 secondi risposta
        'Timer4.Interval = 60000
        'Timer4.Interval = 400000 ' che gestisce i tempi di risposta al cambiamento set point 20 secondi risposta

        Timer4.Enabled = False
        'Timer4.Interval = 180000 ' che gestisce i tempi di risposta al cambiamento set point 20 secondi risposta
        'Timer6.Interval = 60000 ' che gestisce i tempi di risposta al cambiamento set point 20 secondi risposta
        'Timer6.Enabled = False
        'Timer6.Interval = 60000 ' che gestisce i tempi di risposta al cambiamento set point 20 secondi risposta
        Timer2.Stop()
        'Timer3.Stop()
        Timer4.Stop()
        Timer5.Stop()
        '        Timer6.Stop()
        Timer1.Stop()

        Timer1.Enabled = True
        Timer1.Start()
        real_time_bool = False
        stack_messaggi = New Queue()
        counter_refresh_old = 0
        contatore_nuovi = 0
        new_setpoint = ""
    End Sub

    Public Property identificativo() As String
        Get
            Return midentificativo
        End Get
        Set(ByVal value As String)
            midentificativo = value
          
            If Not Directory.Exists("c:\log\" + midentificativo) Then
                Directory.CreateDirectory("c:\log\" + midentificativo)
            End If
        End Set
    End Property
    Public Property tx_strumento() As String
        Get
            Return mstrumento_tx
        End Get
        Set(ByVal value As String)
            mstrumento_tx = value
        End Set
    End Property
    Public Property rx_strumento() As String
        Get
            Return mstrumento_rx
        End Get
        Set(ByVal value As String)
            mstrumento_rx = value
        End Set
    End Property
    Public Property puntatore_strumento() As ConnectedClient
        Get
            Return mpuntatore_strumento
        End Get
        Set(ByVal value As ConnectedClient)
            Try
                mpuntatore_strumento = value
                nuovo_strumento_connesso = True
                sequnza_interrogazione = 0
                numero_interrogazione = 0
                tipo_strumento_sequenza = ""
                value.remove = False
                Timer1.Interval = 1000 ' timer che parte alla ricerca delola configurazione dello strumento
                Timer1.Enabled = True
                Timer1.Start()
                AddHandler value.dataReceived, AddressOf Me.messageStumentoReceived
            Catch ex As Exception
                query.registra_anomalia("Procedura puntatore_strumento", midentificativo, numberid_485.ToString)
            End Try

        End Set
    End Property
    Private Function decode_option_flow(ByVal old_string As String, ByVal new_string As String, ByVal end_string As String) As String
        Dim forma_stringa As String = ""
        Dim forma_stringa_old() As String = old_string.Split("&")
        Try
            forma_stringa = forma_stringa_old(0) + "&"
            Dim timer_value_extract() As String = forma_stringa_old(1).Split("#")
            Dim password As String = Mid(timer_value_extract(2), 36, 4)
            forma_stringa = forma_stringa + timer_value_extract(0) + "#"
            forma_stringa = forma_stringa + "ph" + Mid(new_string, 4, 5) + "#" + timer_value_extract(2) + "#" + timer_value_extract(3) + "#" + timer_value_extract(4) + "#" + timer_value_extract(5)
            Return forma_stringa
        Catch ex As Exception
            Return old_string
        End Try
        Return old_string
    End Function
    Private Function decode_option_password(ByVal old_string As String, ByVal new_string As String, ByVal end_string As String) As String
        Dim forma_stringa As String = ""
        Dim forma_stringa_old() As String = old_string.Split("&")
        Try
            forma_stringa = forma_stringa_old(0) + "&"
            Dim timer_value_extract() As String = forma_stringa_old(1).Split("#")
            Dim password As String = Mid(new_string, 4, 4)
            forma_stringa = forma_stringa + timer_value_extract(0) + "#" + timer_value_extract(1) + "#"
            forma_stringa = forma_stringa + "ph" + Mid(timer_value_extract(2), 2, 34) + password + "#" + timer_value_extract(3) + "#" + timer_value_extract(4) + "#" + timer_value_extract(5)
            Return forma_stringa
        Catch ex As Exception
            Return old_string
        End Try
        Return old_string
    End Function
    Private Function decode_option_clock(ByVal old_string As String, ByVal new_string As String, ByVal end_string As String) As String
        Dim forma_stringa As String = ""
        Dim forma_stringa_old() As String = old_string.Split("&")
        Try
            forma_stringa = forma_stringa_old(0) + "&"
            Dim timer_value_extract() As String = forma_stringa_old(1).Split("#")
            Dim temperature_value As String = Mid(timer_value_extract(0), 15, 5)
            forma_stringa = forma_stringa + "ph" + Mid(new_string, 4, 4) + Mid(new_string, 10, 2) + Mid(new_string, 12, 1) + Mid(new_string, 17, 1) + Mid(new_string, 13, 4) + temperature_value + "#" + timer_value_extract(1) + "#" + timer_value_extract(2) + "#" + timer_value_extract(3) + "#" + timer_value_extract(4) + "#" + timer_value_extract(5)
            Return forma_stringa
        Catch ex As Exception
            Return old_string
        End Try
        Return old_string
    End Function

    Private Function decode_option(ByVal old_string As String, ByVal new_string As String, ByVal end_string As String) As String
        Dim forma_stringa As String = ""
        Dim forma_stringa_old() As String = old_string.Split("&")
        Try
            forma_stringa = forma_stringa_old(0) + "&"
            Dim timer_value_extract() As String = forma_stringa_old(1).Split("#")
            Dim password As String = Mid(timer_value_extract(2), 36, 4)
            forma_stringa = forma_stringa + timer_value_extract(0) + "#" + timer_value_extract(1) + "#"
            forma_stringa = forma_stringa + "ph" + Mid(new_string, 4, 34) + password + "#" + timer_value_extract(3) + "#" + timer_value_extract(4) + "#" + "ph" + Mid(new_string, 8, 1) + "b5end"
            Return forma_stringa
        Catch ex As Exception
            Return old_string
        End Try
        Return old_string
    End Function

    Private Function decode_timer2_1(ByVal old_string As String, ByVal new_string As String, ByVal end_string As String) As String
        Dim forma_stringa As String = ""
        Dim forma_stringa_old() As String = old_string.Split("&")
        Try
            forma_stringa = forma_stringa_old(0) + "&"
            Dim timer_value_extract() As String = forma_stringa_old(1).Split("#")
            forma_stringa = forma_stringa + timer_value_extract(0) + "#" + timer_value_extract(1) + "#" + timer_value_extract(2) + "#"
            forma_stringa = forma_stringa + "ph" + Mid(new_string, 4, 40) + "#" + timer_value_extract(4) + "#" + timer_value_extract(5)
            Return forma_stringa
        Catch ex As Exception
            Return old_string
        End Try
        Return old_string
    End Function
    Private Function decode_timer3_4_5(ByVal old_string As String, ByVal new_string As String, ByVal end_string As String) As String
        Dim forma_stringa As String = ""
        Dim forma_stringa_old() As String = old_string.Split("&")
        Try
            forma_stringa = forma_stringa_old(0) + "&"
            Dim timer_value_extract() As String = forma_stringa_old(1).Split("#")
            forma_stringa = forma_stringa + timer_value_extract(0) + "#" + timer_value_extract(1) + "#" + timer_value_extract(2) + "#" + timer_value_extract(3) + "#"
            forma_stringa = forma_stringa + "ph" + Mid(new_string, 4, 51) + "#" + timer_value_extract(5)
            Return forma_stringa
        Catch ex As Exception
            Return old_string
        End Try
        Return old_string
    End Function

    Private Function decode_setpoint_max5(ByVal old_string As String, ByVal new_string As String, ByVal end_string As String) As String
        'Dim inizio_stringa() As String = old_string.Split("p")
        Dim forma_stringa As String = ""
        Dim forma_stringa_old() As String = old_string.Split("&")
        Try
            forma_stringa = forma_stringa_old(0) + "&ph"
            forma_stringa = forma_stringa + Mid(new_string, 3, 9) + Mid(forma_stringa_old(1), 12, 8) + "#ph"
            forma_stringa = forma_stringa + Mid(new_string, 12, 20) + "#ph"
            forma_stringa = forma_stringa + Mid(new_string, 32, 20) + "#ph"

            forma_stringa = forma_stringa + Mid(new_string, 52, 8) + "0" + Mid(new_string, 60, 3) + "0" + Mid(new_string, 63, 6) + "#ph"
            forma_stringa = forma_stringa + Mid(new_string, 69, 8) + "0" + Mid(new_string, 77, 3) + "0" + Mid(new_string, 80, 6) + "#ph"
            forma_stringa = forma_stringa + Mid(new_string, 86, 8) + "0" + Mid(new_string, 94, 3) + "0" + Mid(new_string, 97, 6) + "#ph"
            forma_stringa = forma_stringa + Mid(new_string, 103, 8) + "#ph"
            forma_stringa = forma_stringa + Mid(new_string, 111, 8) + "#ph"
            forma_stringa = forma_stringa + Mid(new_string, 119, 3) + "#ph"
            forma_stringa = forma_stringa + Mid(new_string, 122, 3) + "#ph"
            forma_stringa = forma_stringa + Mid(new_string, 125, 9) + end_string + "end"
            Return forma_stringa
        Catch ex As Exception
            Return old_string
        End Try
        Return old_string
    End Function
    Private Sub aggiorna_db_strumenti(ByVal puntatore_temp As light, ByVal id_485_temp As String, ByVal codice_temp As String)
        Dim righe_aggiornate As Integer = 0
        'Dim con As New SqlConnection
        'Dim cmd As New SqlCommand
        'con.ConnectionString = "Data Source=LOCALHOST;Initial Catalog=max_5;Integrated Security=True;"
        'con.Open()
        'cmd.Connection = con
        'Dim numero_righe_cancellate As Integer = 0
        'For i = 1 To numberid_485
        '    numero_righe_cancellate = 0
        '    cmd.CommandText = "UPDATE    strumenti" + _
        '                "SET data_aggiornamento =" + puntatore_temp.time_connessione + ", tipo_strumento = '" + puntatore_temp.tipo_strumento + "', nome = '" + puntatore_temp.nome_strumento + "', value1 = '" + puntatore_temp.value1 + "', value2 = '" + puntatore_temp.value2 + "', value3 = '" + puntatore_temp.value3 + "'," _
        '              + "value4 = '" + puntatore_temp.value4 + "', value5 = '" + puntatore_temp.value5 + "', value6 = '" + puntatore_temp.value6 + "', value7 = '" + puntatore_temp.value7 + "', value8 = '" + puntatore_temp.value8 + "', value9 = '" + puntatore_temp.value9 + "', value10 = '" + puntatore_temp.value10 + "', value11 = '" + puntatore_temp.value11 + "'," _
        '              + "value12 = '" + puntatore_temp.value12 + "', value13 = '" + puntatore_temp.value13 + "', value14 ='" + puntatore_temp.value14 + "', value15 ='" + puntatore_temp.value15 + "', value16 = '" + puntatore_temp.value16 + "', value17 = '" + puntatore_temp.value17 + "', value18 = '" + puntatore_temp.value18 + "'," _
        '              + "value19 = '" + puntatore_temp.value19 + "', value20 = '" + puntatore_temp.value20 + "', value21 = '" + puntatore_temp.value21 + "', value22 = '" + puntatore_temp.value22 + "', value23 = '" + puntatore_temp.value23 + "', value24 = '" + puntatore_temp.value24 + "', value25 = '" + puntatore_temp.value1 + "'," _
        '              + "value26 = '" + puntatore_temp.value26 + "', value27 = '" + puntatore_temp.value27 + "', value28 = '" + puntatore_temp.value28 + "', value29 = '" + puntatore_temp.value29 + "', time_no_flow =" + puntatore_temp.time_no_flow + ", csv_enable = " + puntatore_temp.mail_csv_setting + "," _
        '              + "numero_log =" + puntatore_temp.counter_log
        '    'cmd.CommandTimeout = 60
        '    numero_righe_cancellate = cmd.ExecuteNonQuery()
        '    If numero_righe_cancellate = 0 Then
        '        righe_aggiornate = query.aggiorna_strumento(tipo_strumento_sequenza, puntatore_temp, id_485_temp, codice_temp)
        '    End If

        '    'aggiorna_db_strumenti(insieme_strumenti(i), Format(i, "00"), identificativo)
        '    'Thread.Sleep(500)

        'Next


        righe_aggiornate = query.aggiorna_strumento(tipo_strumento_sequenza, puntatore_temp, id_485_temp, codice_temp)


       

    End Sub
    Public Sub elabora_messaggio(ByVal message_val As String)
        Dim i As Integer
        Dim query_result As Boolean
        Dim messaggio_errore As String = ""
        Dim temp_id As Integer = -1
        Dim index_start As Integer = 0
        Dim index_stop As Integer = 0
        Dim index_temp_id As Integer = 0
        Dim message_temp As String = message_val
        'For i = 0 To 30
        '    If InStr(message_temp, Format(i, "00") + "&") <> 0 Then
        '        temp_id = i
        '        Exit For
        '    End If
        'Next

        index_temp_id = InStr(message_temp, "&")
        If index_temp_id > 2 Then
            temp_id = Val(Mid(message_temp, index_temp_id - 2, index_temp_id - 1))
        End If

        If temp_id = -1 Or temp_id = 0 Then
            Exit Sub
        End If
        index_start = InStr(message_temp, "M5")
        index_stop = InStr(message_temp, "|end")
        If index_start = 0 Then
            index_start = 1
        End If
        If index_start > index_stop Then
            Exit Sub
        End If

        message_temp = Mid(message_temp, index_start, index_stop)
        message_temp = message_temp + "|end"
        If InStr(message_temp, "513583") <> 0 Then
            Try
                Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\513583.txt", True)
                    writer.WriteLine("#" + Now.ToString + message_temp + "#" + Format(temp_id, "00"))
                End Using
            Catch ex As Exception

            End Try
        End If


        'Try
        '    Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log_id.txt", True)
        '        writer.WriteLine("#" + Now.ToString + sequnza_interrogazione.ToString + " " + identificativo + " receive#")
        '    End Using
        'Catch ex As Exception


        'End Try

        Try
            If InStr(message_temp, "okem") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then

                check_new_instruments = True
                contatore_nuovi = 0


                real_time_bool = False
                If insieme_strumenti(temp_id) Is Nothing Then
                    insieme_strumenti(temp_id) = New light
                End If
                Dim nessuno As Boolean = False 'variabile per gestire le vechie versione di max 5 dove non si specificava il tipo strumento
                messaggio_errore = "fase 1"
                insieme_strumenti(temp_id).tipo_strumento = message_temp

                numberid_485 = temp_id

                ' query_result = update_database(temp_id, 0)
                insieme_strumenti(temp_id).time_connessione = Now
                If InStr(message_temp, "#max5#") <> 0 Then 'risposta max 5
                    messaggio_errore = messaggio_errore + "|fase 2|max5"
                    nessuno = True
                    If reading_sp Then
                        reading_sp = False
                        sequnza_interrogazione = 5
                    Else
                        If sequenza_timer Then
                            sequnza_interrogazione = 1
                            sequenza_timer = False
                        End If
                    End If
                    numero_interrogazione = 0
                    tipo_strumento_sequenza = "max5"
                End If
                If InStr(message_temp, "#LDTower#") <> 0 Then 'risposta Tower
                    nessuno = True
                    If reading_sp Then
                        reading_sp = False
                        If reading_sp_bio1 Or reading_sp_bio2 Then
                            If reading_sp_bio1 Then
                                sequnza_interrogazione = 24
                            Else
                                sequnza_interrogazione = 29
                            End If
                            reading_sp_bio1 = False
                            reading_sp_bio2 = False
                        Else
                            sequnza_interrogazione = 22
                        End If
                    Else

                        If sequenza_timer Then
                            sequnza_interrogazione = 10
                            sequenza_timer = False
                        End If
                    End If
                    numero_interrogazione = 0
                    tipo_strumento_sequenza = "LDtower"

                End If
                If InStr(message_temp, "#Tower#") <> 0 Or InStr(message_temp, "#Boiler#") <> 0 Then 'risposta Tower
                    messaggio_errore = messaggio_errore + "|fase 2|Tower"
                    nessuno = True
                    If reading_sp Then
                        reading_sp = False
                        If reading_sp_bio1 Or reading_sp_bio2 Then
                            If reading_sp_bio1 Then
                                sequnza_interrogazione = 24
                            Else
                                sequnza_interrogazione = 29
                            End If
                            reading_sp_bio1 = False
                            reading_sp_bio2 = False
                        Else
                            sequnza_interrogazione = 22
                        End If
                    Else

                        If sequenza_timer Then
                            sequnza_interrogazione = 10
                            sequenza_timer = False
                        End If
                    End If
                    numero_interrogazione = 0
                    tipo_strumento_sequenza = "tower"
                End If
                If InStr(message_temp, "#LD#") <> 0 Or InStr(message_temp, "#LDDT#") <> 0 Then 'risposta LD
                    messaggio_errore = messaggio_errore + "|fase 2|LD"
                    nessuno = True
                    If reading_sp Then
                        reading_sp = False
                        sequnza_interrogazione = 47
                    Else

                        If sequenza_timer Then
                            sequnza_interrogazione = 40
                            sequenza_timer = False
                        End If
                    End If
                    numero_interrogazione = 0
                    tipo_strumento_sequenza = "ld"
                End If
                If InStr(message_temp, "#LD4#") <> 0 Then 'risposta LD
                    messaggio_errore = messaggio_errore + "|fase 2|LD4"
                    nessuno = True
                    If reading_sp Then
                        reading_sp = False
                        sequnza_interrogazione = 47
                    Else

                        If sequenza_timer Then
                            sequnza_interrogazione = 40
                            sequenza_timer = False
                        End If
                    End If
                    numero_interrogazione = 0
                    tipo_strumento_sequenza = "ld"
                End If

                If InStr(message_temp, "#LDS#") <> 0 Then 'risposta LDS
                    messaggio_errore = messaggio_errore + "|fase 2|LDS"
                    nessuno = True
                    If reading_sp Then
                        reading_sp = False
                        sequnza_interrogazione = 47
                    Else
                        If sequenza_timer Then
                            sequnza_interrogazione = 40
                            sequenza_timer = False
                        End If
                    End If
                    numero_interrogazione = 0
                    tipo_strumento_sequenza = "lds"
                End If
                If InStr(message_temp, "#WD#") <> 0 Then 'risposta WD
                    messaggio_errore = messaggio_errore + "|fase 2|WD"
                    nessuno = True
                    If reading_sp Then
                        reading_sp = False
                        sequnza_interrogazione = 47
                    Else
                        If sequenza_timer Then
                            sequnza_interrogazione = 40
                            sequenza_timer = False
                        End If
                    End If
                    numero_interrogazione = 0
                    tipo_strumento_sequenza = "wd"
                End If
                If InStr(message_temp, "#WH#") <> 0 Then 'risposta WD
                    messaggio_errore = messaggio_errore + "|fase 2|WD"
                    nessuno = True
                    If reading_sp Then
                        reading_sp = False
                        sequnza_interrogazione = 47
                    Else
                        If sequenza_timer Then
                            sequnza_interrogazione = 40
                            sequenza_timer = False
                        End If
                    End If
                    numero_interrogazione = 0
                    tipo_strumento_sequenza = "wh"
                End If
                If InStr(message_temp, "#LTB#") <> 0 Then 'risposta Lotus B
                    messaggio_errore = messaggio_errore + "|fase 2|WD"
                    nessuno = True
                    If reading_sp Then
                        reading_sp = False
                        sequnza_interrogazione = 47
                    Else
                        If sequenza_timer Then
                            sequnza_interrogazione = 40
                            sequenza_timer = False
                        End If
                    End If
                    numero_interrogazione = 0
                    tipo_strumento_sequenza = "ltb"
                End If
                If InStr(message_temp, "#LTA#") <> 0 Then 'risposta Lotus B
                    messaggio_errore = messaggio_errore + "|fase 2|WD"
                    nessuno = True
                    If reading_sp Then
                        reading_sp = False
                        sequnza_interrogazione = 47
                    Else
                        If sequenza_timer Then
                            sequnza_interrogazione = 40
                            sequenza_timer = False
                        End If
                    End If
                    numero_interrogazione = 0
                    tipo_strumento_sequenza = "ltaud"
                End If
                If InStr(message_temp, "#LTU#") <> 0 Then 'risposta Lotus B
                    messaggio_errore = messaggio_errore + "|fase 2|WD"
                    nessuno = True
                    If reading_sp Then
                        reading_sp = False
                        sequnza_interrogazione = 47
                    Else
                        If sequenza_timer Then
                            sequnza_interrogazione = 40
                            sequenza_timer = False
                        End If
                    End If
                    numero_interrogazione = 0
                    tipo_strumento_sequenza = "ltaud"
                End If
                If InStr(message_temp, "#LTD#") <> 0 Then 'risposta Lotus B
                    messaggio_errore = messaggio_errore + "|fase 2|WD"
                    nessuno = True
                    If reading_sp Then
                        reading_sp = False
                        sequnza_interrogazione = 47
                    Else
                        If sequenza_timer Then
                            sequnza_interrogazione = 40
                            sequenza_timer = False
                        End If
                    End If
                    numero_interrogazione = 0
                    tipo_strumento_sequenza = "ltaud"
                End If

                If InStr(message_temp, "#LDMA#") <> 0 Then 'risposta Lotus B
                    messaggio_errore = messaggio_errore + "|fase 2|WD"
                    nessuno = True
                    If reading_sp Then
                        reading_sp = False
                        sequnza_interrogazione = 47
                    Else
                        If sequenza_timer Then
                            sequnza_interrogazione = 40
                            sequenza_timer = False
                        End If
                    End If
                    numero_interrogazione = 0
                    tipo_strumento_sequenza = "ldma"
                End If
                If InStr(message_temp, "#LDLG#") <> 0 Then 'risposta Lotus B
                    messaggio_errore = messaggio_errore + "|fase 2|WD"
                    nessuno = True
                    If reading_sp Then
                        reading_sp = False
                        sequnza_interrogazione = 47
                    Else
                        If sequenza_timer Then
                            sequnza_interrogazione = 40
                            sequenza_timer = False
                        End If
                    End If
                    numero_interrogazione = 0
                    tipo_strumento_sequenza = "ldlg"
                End If

                If nessuno = False Then
                    messaggio_errore = messaggio_errore + "|fase 2|Nessuno"
                    If sequenza_timer Then
                        sequnza_interrogazione = 1
                        sequenza_timer = False
                    End If
                    numero_interrogazione = 0
                End If
                Exit Sub
            End If
        Catch ex As Exception
            query.registra_anomalia("Inizializzazione", midentificativo, temp_id.ToString)
        End Try


        'controllo ricezione di un eventuale id ancora non istanziato
        Try
            If insieme_strumenti(temp_id) Is Nothing Then
                Exit Sub
            End If

        Catch ex As Exception
            query.registra_anomalia("ID non istanziato", midentificativo, temp_id.ToString)
            Exit Sub
        End Try
        '
        If InStr(message_temp, "changeend") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            insieme_strumenti(temp_id).value_change = message_temp
        End If
        'max5--------------------------------------------------------------------------
        If InStr(message_temp, "bcend") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                insieme_strumenti(temp_id).value1 = message_temp
                ' If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint1 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint1 = False
                End If

                '  End If
                insieme_strumenti(temp_id).time_connessione = Now
                'patch per itaca strumento 495641
                If identificativo = "495641" Then
                    insieme_strumenti(temp_id).factor_divide_ch4 = insieme_strumenti(temp_id).factor_divide_ch3
                End If
                If sequenza_timer And tipo_strumento_sequenza = "max5" Then
                    sequnza_interrogazione = 2
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception
                query.registra_anomalia("bc errore", midentificativo, temp_id.ToString)
            End Try
        End If
        If InStr(message_temp, "abend") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                insieme_strumenti(temp_id).value2 = message_temp
                ' If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint2 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint2 = False
                End If

                ' End If
                insieme_strumenti(temp_id).value2_1 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                If sequenza_timer And tipo_strumento_sequenza = "max5" And real_time_bool = False Then
                    sequnza_interrogazione = 3
                    sequenza_timer = False
                End If
                real_time_bool = False
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception
                query.registra_anomalia("ab errore", midentificativo, temp_id.ToString)
            End Try
        End If
        If InStr(message_temp, "baend") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                insieme_strumenti(temp_id).value3 = message_temp
                ' If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint3 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint3 = False
                End If

                ' End If

                insieme_strumenti(temp_id).time_connessione = Now
                If sequenza_timer And tipo_strumento_sequenza = "max5" Then
                    sequnza_interrogazione = 4
                    sequenza_timer = False
                End If
                'temp_id = temp_id + 1 ' passo a controllare id successivo
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception
                query.registra_anomalia("ba errore", midentificativo, temp_id.ToString)
            End Try
        End If
        If InStr(message_temp, "b5end") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                insieme_strumenti(temp_id).value4 = message_temp
                ' If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint4 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint4 = False
                End If

                '  End If

                insieme_strumenti(temp_id).time_connessione = Now
                If sequenza_timer And tipo_strumento_sequenza = "max5" Then
                    ' If nuovo_strumento_connesso Then
                    sequnza_interrogazione = 5
                    'Else
                    '    sequnza_interrogazione = 0
                    '    maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                    'End If
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception
                query.registra_anomalia("b5 errore", midentificativo, temp_id.ToString)
            End Try
        End If
        If InStr(message_temp, "1Aend") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                insieme_strumenti(temp_id).value5 = message_temp
                'If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint5 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint5 = False
                End If

                'End If

                insieme_strumenti(temp_id).time_connessione = Now
                If sequenza_timer And tipo_strumento_sequenza = "max5" Then
                    sequnza_interrogazione = 6
                    sequenza_timer = False
                End If
                'temp_id = temp_id + 1 ' passo a controllare id successivo
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception
                query.registra_anomalia("1A errore", midentificativo, temp_id.ToString)

            End Try
        End If
        If InStr(message_temp, "2Aend") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                insieme_strumenti(temp_id).value6 = message_temp
                ' If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint6 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint6 = False
                End If

                ' End If

                insieme_strumenti(temp_id).value6_1 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                If sequenza_timer And tipo_strumento_sequenza = "max5" Then
                    sequnza_interrogazione = 7
                    sequenza_timer = False
                End If
                'temp_id = temp_id + 1 ' passo a controllare id successivo
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception
                query.registra_anomalia("2A errore", midentificativo, temp_id.ToString)
            End Try
        End If
        If InStr(message_temp, "3Aend") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                insieme_strumenti(temp_id).value7 = message_temp
                ' If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint7 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint7 = False
                End If

                'End If

                insieme_strumenti(temp_id).time_connessione = Now
                If sequenza_timer And tipo_strumento_sequenza = "max5" Then
                    sequnza_interrogazione = 8
                    sequenza_timer = False
                End If
                'temp_id = temp_id + 1 ' passo a controllare id successivo
                numero_interrogazione = 0

                Exit Sub
            Catch ex As Exception
                query.registra_anomalia("3A errore", midentificativo, temp_id.ToString)
            End Try
        End If
        If InStr(message_temp, "4Aend") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                insieme_strumenti(temp_id).value8 = message_temp
                ' If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint8 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint8 = False
                End If

                'End If

                insieme_strumenti(temp_id).time_connessione = Now
                If sequenza_timer And tipo_strumento_sequenza = "max5" Then
                    sequnza_interrogazione = 9
                    sequenza_timer = False
                End If
                'temp_id = temp_id + 1 ' passo a controllare id successivo
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception
                query.registra_anomalia("4A errore", midentificativo, temp_id.ToString)
            End Try
        End If
        If InStr(message_temp, "5Aend") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                insieme_strumenti(temp_id).value9 = message_temp
                ' If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint9 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint9 = False
                End If

                ' End If

                insieme_strumenti(temp_id).time_connessione = Now
                If sequenza_timer And tipo_strumento_sequenza = "max5" Then
                    If insieme_strumenti(temp_id).new_version Then
                        sequnza_interrogazione = 100
                    Else
                        'aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        sequnza_interrogazione = 0
                        maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                        sequenza_timer = False
                    End If
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception
                query.registra_anomalia("5A errore", midentificativo, temp_id.ToString)
            End Try
        End If
        If InStr(message_temp, "1Bend") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                insieme_strumenti(temp_id).value10 = message_temp
                ' If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint10 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint10 = False
                End If

                '  End If

                insieme_strumenti(temp_id).time_connessione = Now
                If sequenza_timer And tipo_strumento_sequenza = "max5" Then
                    sequnza_interrogazione = 101
                    sequenza_timer = False
                End If
                'temp_id = temp_id + 1 ' passo a controllare id successivo
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception
                query.registra_anomalia("1B errore", midentificativo, temp_id.ToString)
            End Try
        End If
        If InStr(message_temp, "2Bend") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                insieme_strumenti(temp_id).value11 = message_temp
                ' If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint11 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint11 = False
                End If

                '  End If

                insieme_strumenti(temp_id).time_connessione = Now
                If sequenza_timer And tipo_strumento_sequenza = "max5" Then
                    sequnza_interrogazione = 102
                    sequenza_timer = False
                End If
                'temp_id = temp_id + 1 ' passo a controllare id successivo
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception
                query.registra_anomalia("2B errore", midentificativo, temp_id.ToString)
            End Try
        End If
        If InStr(message_temp, "3Bend") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                insieme_strumenti(temp_id).value12 = message_temp
                'If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint12 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint12 = False
                End If

                ' End If

                insieme_strumenti(temp_id).time_connessione = Now
                If sequenza_timer And tipo_strumento_sequenza = "max5" Then
                    sequnza_interrogazione = 103
                    sequenza_timer = False
                End If
                'temp_id = temp_id + 1 ' passo a controllare id successivo
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception
                query.registra_anomalia("3B errore", midentificativo, temp_id.ToString)
            End Try
        End If
        If InStr(message_temp, "4Bend") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                insieme_strumenti(temp_id).value13 = message_temp
                ' If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint13 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint13 = False
                End If

                ' End If

                insieme_strumenti(temp_id).time_connessione = Now
                If sequenza_timer And tipo_strumento_sequenza = "max5" Then
                    sequnza_interrogazione = 104
                    sequenza_timer = False
                End If
                'temp_id = temp_id + 1 ' passo a controllare id successivo
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception
                query.registra_anomalia("4B errore", midentificativo, temp_id.ToString)
            End Try
        End If
        If InStr(message_temp, "5Bend") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                insieme_strumenti(temp_id).value14 = message_temp
                '   If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint14 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint14 = False
                End If

                '     End If
                '
                insieme_strumenti(temp_id).time_connessione = Now
                If sequenza_timer And tipo_strumento_sequenza = "max5" Then
                    sequnza_interrogazione = 105
                    sequenza_timer = False
                End If
                'temp_id = temp_id + 1 ' passo a controllare id successivo
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception
                query.registra_anomalia("5B errore", midentificativo, temp_id.ToString)
            End Try
        End If
        If InStr(message_temp, "1Cend") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                insieme_strumenti(temp_id).value15 = message_temp
                '   If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint15 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint15 = False
                End If

                '  End If

                insieme_strumenti(temp_id).time_connessione = Now
                If sequenza_timer And tipo_strumento_sequenza = "max5" Then
                    sequnza_interrogazione = 106
                    sequenza_timer = False
                End If
                'temp_id = temp_id + 1 ' passo a controllare id successivo
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception
                query.registra_anomalia("1C errore", midentificativo, temp_id.ToString)
            End Try
        End If
        If InStr(message_temp, "1Dend") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                insieme_strumenti(temp_id).value16 = message_temp
                '  If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint16 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint16 = False
                End If

                '    End If

                insieme_strumenti(temp_id).time_connessione = Now
                If sequenza_timer And tipo_strumento_sequenza = "max5" Then
                    sequnza_interrogazione = 107
                    sequenza_timer = False
                End If
                'temp_id = temp_id + 1 ' passo a controllare id successivo
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception
                query.registra_anomalia("1D errore", midentificativo, temp_id.ToString)
            End Try
        End If
        If InStr(message_temp, "b6end") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                insieme_strumenti(temp_id).value17 = message_temp
                '  If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint17 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint17 = False
                End If

                ' End If

                insieme_strumenti(temp_id).time_connessione = Now
                If sequenza_timer And tipo_strumento_sequenza = "max5" Then
                    sequnza_interrogazione = 0
                    maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                    sequenza_timer = False
                End If
                'temp_id = temp_id + 1 ' passo a controllare id successivo
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception
                query.registra_anomalia("b6 errore", midentificativo, temp_id.ToString)
            End Try
        End If

        'risposta cambiamento log
        If (InStr(message_temp, "logsetr") = 0) And InStr(message_temp, "logser") = 0 And (InStr(message_temp, "logstwok") = 0) And (InStr(message_temp, "logsewok") = 0) And (InStr(message_temp, "log") <> 0 Or InStr(message_temp, "empty") <> 0) And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                Try

                    blocco_pc = True
                    puntatore_pc_scrittura_setpoint.scrittura_sp = False
                    puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
                Catch ex As Exception

                End Try
                Exit Sub
            End If
            log_fail = 0
            If (InStr(message_temp, "empty") <> 0) Then
                log_completed = True
                busy_log = True
            End If
            Dim old_message_file As String = ""
            Dim message_file As String = ""
            old_message_file = message_temp + Chr(255)
            Dim j As Integer = InStr(message_temp, "log")
            Dim k As Integer
            If j > 0 Then
                For k = j + 3 To 300
                    If Mid(old_message_file, k, 1) = Chr(255) Then
                        Exit For
                    Else

                        message_file = message_file + Format(Asc(Mid(old_message_file, k, 1)), "00") + "?"
                    End If
                Next
            End If
            message_file = Now.ToString + Mid(old_message_file, 1, j + 3) + message_file + Chr(125) + "?" + "#"
            If (identificativo = "292593") Then
                Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\" + identificativo + ".txt", True)
                    writer.WriteLine(message_file)
                    writer.Close()
                End Using
            End If
            'If Not File.Exists("c:\log\" + identificativo + "\" + Format(temp_id, "00") + "_temp") Then
            '    'Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log\" + identificativo + "\" + Format(temp_id, "00") + "_" + insieme_strumenti(temp_id).tipo_strumento, True)
            '    '    writer.Close()
            '    'End Using
            '    Using writer_temp As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log\" + identificativo + "\" + Format(temp_id, "00") + "_temp", False)
            '        writer_temp.WriteLine("000")
            '        writer_temp.WriteLine("true")
            '        writer_temp.Close()
            '    End Using
            'End If
            'Dim sr As System.IO.StreamReader = New System.IO.StreamReader("c:\log\" + identificativo + "\" + Format(temp_id, "00") + "_temp")
            'Dim riga_file As String = ""
            'Dim limite_lettura As Integer = 0
            'If sr.Peek() > 0 Then
            '    riga_file = sr.ReadLine()
            'End If


            'If InStr(riga_file, "log") <> 0 Or counter_next_log > 300 Then
            If counter_next_log > 600 Then
                'Dim data_log() As String = riga_file.Split("log")
                Try
                    '   If counter_next_log > 300 Or InStr(message_file, data_log(1)) <> 0 Then
                    If counter_next_log > 600 Then
                        log_completed = True
                        busy_log = True
                    End If
                Catch ex As Exception
                    log_completed = True
                    busy_log = True
                End Try
            End If
            'sr.Close()
            Dim temp_log_completed As Boolean = False
            Select Case insieme_strumenti(temp_id).tipo_strumento
                Case "max5"
                    temp_log_completed = decode_log_ptr.decode_log_max5(identificativo, temp_id, message_file, insieme_strumenti(temp_id).factor_divide_ch1, insieme_strumenti(temp_id).factor_divide_ch2, insieme_strumenti(temp_id).factor_divide_ch3, insieme_strumenti(temp_id).factor_divide_ch4, insieme_strumenti(temp_id).factor_divide_ch5, insieme_strumenti(temp_id).yagel_version,
                                                   insieme_strumenti(temp_id).full_scale_ch1, insieme_strumenti(temp_id).full_scale_ch2, insieme_strumenti(temp_id).full_scale_ch3, insieme_strumenti(temp_id).full_scale_ch4, insieme_strumenti(temp_id).full_scale_ch5,
                                                   insieme_strumenti(temp_id).value_ch1, insieme_strumenti(temp_id).value_ch2, insieme_strumenti(temp_id).value_ch3, insieme_strumenti(temp_id).value_ch4, insieme_strumenti(temp_id).value_ch5, insieme_strumenti(temp_id).number_version_total,
                                                   insieme_strumenti(temp_id).ma1_enable, insieme_strumenti(temp_id).ma2_enable, insieme_strumenti(temp_id).ma3_enable, insieme_strumenti(temp_id).ma4_enable, insieme_strumenti(temp_id).ma5_enable)
                Case "LDtower"
                    temp_log_completed = decode_log_ptr.decode_log_tower(identificativo, temp_id, message_file, insieme_strumenti(temp_id).factor_divide_ch1, insieme_strumenti(temp_id).factor_divide_ch2, insieme_strumenti(temp_id).factor_divide_ch3,
                                                    insieme_strumenti(temp_id).full_scale_ch1, insieme_strumenti(temp_id).full_scale_ch2, insieme_strumenti(temp_id).full_scale_ch3,
                                                    insieme_strumenti(temp_id).value_ch1, insieme_strumenti(temp_id).value_ch2, insieme_strumenti(temp_id).value_ch3, insieme_strumenti(temp_id).tower_induttiva, insieme_strumenti(temp_id).type_tower)

                Case "Tower"
                    temp_log_completed = decode_log_ptr.decode_log_tower(identificativo, temp_id, message_file, insieme_strumenti(temp_id).factor_divide_ch1, insieme_strumenti(temp_id).factor_divide_ch2, insieme_strumenti(temp_id).factor_divide_ch3,
                                                    insieme_strumenti(temp_id).full_scale_ch1, insieme_strumenti(temp_id).full_scale_ch2, insieme_strumenti(temp_id).full_scale_ch3,
                                                    insieme_strumenti(temp_id).value_ch1, insieme_strumenti(temp_id).value_ch2, insieme_strumenti(temp_id).value_ch3, insieme_strumenti(temp_id).tower_induttiva, insieme_strumenti(temp_id).type_tower)
                Case "LD"
                    temp_log_completed = decode_log_ptr.decode_log_ld(identificativo, temp_id, message_file, insieme_strumenti(temp_id).factor_divide_ch1, insieme_strumenti(temp_id).factor_divide_ch2, insieme_strumenti(temp_id).factor_divide_ch3,
                                                 insieme_strumenti(temp_id).full_scale_ch1, insieme_strumenti(temp_id).full_scale_ch2, insieme_strumenti(temp_id).full_scale_ch3,
                                                 insieme_strumenti(temp_id).value_ch1, insieme_strumenti(temp_id).value_ch2, insieme_strumenti(temp_id).value_ch3, insieme_strumenti(temp_id).format_temperatura, insieme_strumenti(temp_id).factor_divide_ch4)
                Case "LD4"
                    temp_log_completed = decode_log_ptr.decode_log_ld4(identificativo, temp_id, message_file, insieme_strumenti(temp_id).factor_divide_ch1, insieme_strumenti(temp_id).factor_divide_ch2, insieme_strumenti(temp_id).factor_divide_ch3, insieme_strumenti(temp_id).factor_divide_ch4,
                                                 insieme_strumenti(temp_id).full_scale_ch1, insieme_strumenti(temp_id).full_scale_ch2, insieme_strumenti(temp_id).full_scale_ch3, insieme_strumenti(temp_id).full_scale_ch4,
                                                 insieme_strumenti(temp_id).value_ch1, insieme_strumenti(temp_id).value_ch2, insieme_strumenti(temp_id).value_ch3, insieme_strumenti(temp_id).value_ch4, insieme_strumenti(temp_id).format_temperatura)

                Case "LDS"
                    temp_log_completed = decode_log_ptr.decode_log_ld(identificativo, temp_id, message_file, insieme_strumenti(temp_id).factor_divide_ch1, insieme_strumenti(temp_id).factor_divide_ch1, insieme_strumenti(temp_id).factor_divide_ch1,
                                                 insieme_strumenti(temp_id).full_scale_ch1, insieme_strumenti(temp_id).full_scale_ch1, insieme_strumenti(temp_id).full_scale_ch1,
                                                 insieme_strumenti(temp_id).value_ch1, insieme_strumenti(temp_id).value_ch1, insieme_strumenti(temp_id).value_ch1, insieme_strumenti(temp_id).format_temperatura, 0)
                Case "WD"
                    temp_log_completed = decode_log_ptr.decode_log_wd(identificativo, temp_id, message_file, insieme_strumenti(temp_id).factor_divide_ch1, insieme_strumenti(temp_id).factor_divide_ch2, insieme_strumenti(temp_id).factor_divide_ch3,
                                                 insieme_strumenti(temp_id).full_scale_ch1, insieme_strumenti(temp_id).full_scale_ch2, insieme_strumenti(temp_id).full_scale_ch3,
                                                 insieme_strumenti(temp_id).value_ch1, insieme_strumenti(temp_id).value_ch2, insieme_strumenti(temp_id).value_ch3)
                Case "WH"
                    temp_log_completed = decode_log_ptr.decode_log_wh(identificativo, temp_id, message_file, insieme_strumenti(temp_id).factor_divide_ch1, insieme_strumenti(temp_id).factor_divide_ch2, insieme_strumenti(temp_id).factor_divide_ch3,
                                                 insieme_strumenti(temp_id).full_scale_ch1, insieme_strumenti(temp_id).full_scale_ch2, insieme_strumenti(temp_id).full_scale_ch3,
                                                 insieme_strumenti(temp_id).value_ch1, insieme_strumenti(temp_id).value_ch2, insieme_strumenti(temp_id).value_ch3)
                Case "LTB"
                    temp_log_completed = decode_log_ptr.decode_log_ltb(identificativo, temp_id, message_file, insieme_strumenti(temp_id).factor_divide_ch1, insieme_strumenti(temp_id).factor_divide_ch2,
                                                 insieme_strumenti(temp_id).full_scale_ch1, insieme_strumenti(temp_id).full_scale_ch2,
                                                 insieme_strumenti(temp_id).value_ch1, insieme_strumenti(temp_id).value_ch2)
                Case "LTA"
                    temp_log_completed = decode_log_ptr.decode_log_lta_ltu_ltd(identificativo, temp_id, message_file, insieme_strumenti(temp_id).factor_divide_ch1, insieme_strumenti(temp_id).factor_divide_ch2,
                                                 insieme_strumenti(temp_id).full_scale_ch1, insieme_strumenti(temp_id).full_scale_ch2,
                                                 insieme_strumenti(temp_id).value_ch1, insieme_strumenti(temp_id).value_ch2)
                Case "LTU"
                    temp_log_completed = decode_log_ptr.decode_log_lta_ltu_ltd(identificativo, temp_id, message_file, insieme_strumenti(temp_id).factor_divide_ch1, insieme_strumenti(temp_id).factor_divide_ch2,
                                                 insieme_strumenti(temp_id).full_scale_ch1, insieme_strumenti(temp_id).full_scale_ch2,
                                                 insieme_strumenti(temp_id).value_ch1, insieme_strumenti(temp_id).value_ch2)
                Case "LTD"
                    temp_log_completed = decode_log_ptr.decode_log_lta_ltu_ltd(identificativo, temp_id, message_file, insieme_strumenti(temp_id).factor_divide_ch1, insieme_strumenti(temp_id).factor_divide_ch2,
                                                 insieme_strumenti(temp_id).full_scale_ch1, insieme_strumenti(temp_id).full_scale_ch2,
                                                 insieme_strumenti(temp_id).value_ch1, insieme_strumenti(temp_id).value_ch2)

            End Select
            If Not temp_log_completed Then
                counter_double_log = counter_double_log + 1
                If counter_double_log < 5 Then
                    temp_log_completed = True
                End If
            End If

            busy_log = True
            If Not temp_log_completed Then
                counter_double_log = 0
                log_completed = True

                '                busy_log = True
            End If
            'If log_completed And busy_log Then
            '    Select Case insieme_strumenti(temp_id).tipo_strumento
            '        Case "max5"
            '            query.log_max5_remove(identificativo, temp_id)
            '        Case "Tower"
            '            query.log_tower_remove(identificativo, temp_id)
            '        Case "LD"
            '            query.log_ld_remove(identificativo, temp_id)
            '        Case "LD4"
            '            query.log_ld4_remove(identificativo, temp_id)
            '        Case "LDS"
            '            query.log_ld_remove(identificativo, temp_id)
            '        Case "WD"
            '            query.log_wd_remove(identificativo, temp_id)
            '    End Select
            'End If

            'If Not log_completed Then
            '    Try
            '        'Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log\" + identificativo + "\" + Format(temp_id, "00") + "_" + insieme_strumenti(temp_id).tipo_strumento, True)
            '        '    writer.WriteLine(message_file)
            '        '    writer.Close()
            '        'End Using
            '        If (counter_next_log = 1) Then
            '            message_file_check = message_file
            '        End If

            '    Catch ex As Exception
            '        query.registra_anomalia("LOG1", midentificativo, temp_id.ToString)

            '    End Try
            '    busy_log = True
            'Else
            '    If InStr(message_file_check, "log") <> 0 Then
            '        Using writer_temp As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log\" + identificativo + "\" + Format(temp_id, "00") + "_temp", False)
            '            writer_temp.WriteLine(message_file_check)
            '            writer_temp.WriteLine("false")
            '            writer_temp.Close()
            '        End Using
            '    End If
            'End If
            Exit Sub
        End If

        If InStr(message_temp, "az") <> 0 And InStr(message_temp, "smsmar") = 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                Try
                    puntatore_pc_scrittura_setpoint.scrittura_sp = False
                    puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
                Catch ex As Exception
                    query.registra_anomalia("LOG2", midentificativo, temp_id.ToString)

                End Try
                Exit Sub
            End If
            Dim data() As String = message_temp.Split("&"c)
            Dim index_log_val As Integer
            index_log_val = Val(Mid(data(1), 1, 4))
            If (index_log_val = counter_next_log + 1) Then
                Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "au")
                Exit Sub
            End If
            If (index_log_val > counter_next_log) Then
                log_completed = True
                busy_log = True
            End If
            If (index_log_val <= counter_next_log) Then
                counter_next_log = index_log_val
                busy_log = True
            End If
            'If log_completed And busy_log Then
            '    Select Case insieme_strumenti(temp_id).tipo_strumento
            '        Case "max5"
            '            query.log_max5_remove(identificativo, temp_id)
            '        Case "Tower"
            '            query.log_tower_remove(identificativo, temp_id)
            '        Case "LD"
            '            query.log_ld_remove(identificativo, temp_id)
            '        Case "LD4"
            '            query.log_ld4_remove(identificativo, temp_id)
            '        Case "LDS"
            '            query.log_ld_remove(identificativo, temp_id)
            '        Case "WD"
            '            query.log_wd_remove(identificativo, temp_id)
            '    End Select
            'End If
            Exit Sub
        End If

        'risposta cambiamento set point strumento max 5
        If InStr(message_temp, "6Aend") <> 0 Then 'risposta positiva canale 1
            wait_response_invio = True
            insieme_strumenti(temp_id).mvalue5_bool = False
            insieme_strumenti(temp_id).update_setpoint5 = True
            check_bool_value5(temp_id)
            priority_setpoint = True
            'insieme_strumenti(temp_id).mvalue10_bool = False
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'insieme_strumenti(temp_id).value5 = ""
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
            'Try
            '    Dim data_temp() As String = buffer_sp1.Split("A")
            '    If data_temp.Length > 0 Then
            '        Dim id_temp As Integer = Val(Mid(data_temp(0), 1, 2))
            '        insieme_strumenti(id_temp).value5 = decode_setpoint_max5(insieme_strumenti(id_temp).value5, data_temp(1), "1A")
            '    End If
            'Catch ex As Exception

            'End Try
            'puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message)

        End If
        If InStr(message_temp, "7Aend") <> 0 Then 'risposta positiva canale 2
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "2A")
            insieme_strumenti(temp_id).mvalue6_bool = False
            check_bool_value6(temp_id)
            insieme_strumenti(temp_id).update_setpoint6 = True
            priority_setpoint = True
            'insieme_strumenti(temp_id).mvalue11_bool = False

            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
            'Try
            '    Dim data_temp() As String = buffer_sp1.Split("A")
            '    If data_temp.Length > 0 Then
            '        Dim id_temp As Integer = Val(Mid(data_temp(0), 1, 2))
            '        insieme_strumenti(id_temp).value6 = decode_setpoint_max5(insieme_strumenti(id_temp).value6, data_temp(1), "2A")
            '    End If
            'Catch ex As Exception

            'End Try
            'puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message)
        End If
        If InStr(message_temp, "8Aend") <> 0 Then 'risposta positiva canale 3
            insieme_strumenti(temp_id).mvalue7_bool = False
            check_bool_value7(temp_id)
            insieme_strumenti(temp_id).update_setpoint7 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "3A")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
            'Try
            '    Dim data_temp() As String = buffer_sp1.Split("A")
            '    If data_temp.Length > 0 Then
            '        Dim id_temp As Integer = Val(Mid(data_temp(0), 1, 2))
            '        insieme_strumenti(id_temp).value7 = decode_setpoint_max5(insieme_strumenti(id_temp).value7, data_temp(1), "3A")
            '    End If
            'Catch ex As Exception

            'End Try
            'puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message)
        End If
        If InStr(message_temp, "9Aend") <> 0 Then 'risposta positiva canale 4
            insieme_strumenti(temp_id).mvalue8_bool = False
            check_bool_value8(temp_id)
            insieme_strumenti(temp_id).update_setpoint8 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "4A")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
            'Try
            '    Dim data_temp() As String = buffer_sp1.Split("A")
            '    If data_temp.Length > 0 Then
            '        Dim id_temp As Integer = Val(Mid(data_temp(0), 1, 2))
            '        insieme_strumenti(id_temp).value8 = decode_setpoint_max5(insieme_strumenti(id_temp).value8, data_temp(1), "4A")
            '    End If
            'Catch ex As Exception

            'End Try
            'puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message)
        End If
        If InStr(message_temp, "AAend") <> 0 Then 'risposta positiva canale 5
            insieme_strumenti(temp_id).mvalue9_bool = False
            check_bool_value9(temp_id)
            insieme_strumenti(temp_id).update_setpoint9 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "5A")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
            'Try
            '    Dim data_temp() As String = buffer_sp1.Split("A")
            '    If data_temp.Length > 0 Then
            '        Dim id_temp As Integer = Val(Mid(data_temp(0), 1, 2))
            '        insieme_strumenti(id_temp).value9 = decode_setpoint_max5(insieme_strumenti(id_temp).value9, data_temp(1), "5A")
            '    End If
            'Catch ex As Exception

            'End Try
            'puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message)
        End If
        If InStr(message_temp, "6Bend") <> 0 Then 'nomi del primo canale
            insieme_strumenti(temp_id).mvalue10_bool = False
            check_bool_value10(temp_id)
            insieme_strumenti(temp_id).update_setpoint10 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "1B")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "7Bend") <> 0 Then 'nomi del secondo canale
            insieme_strumenti(temp_id).mvalue11_bool = False
            check_bool_value11(temp_id)
            insieme_strumenti(temp_id).update_setpoint11 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "2B")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "8Bend") <> 0 Then 'nomi del terzo canale
            insieme_strumenti(temp_id).mvalue12_bool = False
            check_bool_value12(temp_id)
            insieme_strumenti(temp_id).update_setpoint12 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "3B")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "9Bend") <> 0 Then 'nomi del quarto canale
            insieme_strumenti(temp_id).mvalue13_bool = False
            check_bool_value13(temp_id)
            insieme_strumenti(temp_id).update_setpoint13 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "4B")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "ABend") <> 0 Then 'nomi del quinto canale
            insieme_strumenti(temp_id).mvalue14_bool = False
            check_bool_value14(temp_id)
            insieme_strumenti(temp_id).update_setpoint14 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "5B")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "2Dend") <> 0 Then 'mail sms
            insieme_strumenti(temp_id).mvalue16_bool = False
            check_bool_value16(temp_id)
            insieme_strumenti(temp_id).update_setpoint16 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "1D")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "2Fend") <> 0 Then 'reset  totalizzatore
            insieme_strumenti(temp_id).mvalue2_bool = False
            check_bool_value2(temp_id)
            insieme_strumenti(temp_id).update_setpoint2 = True
            'priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "ab")

            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "2Gend") <> 0 Then 'manual
            'insieme_strumenti(temp_id).mvalue2_bool = False
            'priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "ab")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "2Hend") <> 0 Then 'remote_calib
            insieme_strumenti(temp_id).mvalue2_bool = False
            check_bool_value2(temp_id)
            insieme_strumenti(temp_id).update_setpoint2 = True
            'priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "ab")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "2Iend") <> 0 Then 'remote_calib
            wait_response_invio = True

            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "2Lend") <> 0 Then 'remote_calib
            insieme_strumenti(temp_id).mvalue5_bool = False
            check_bool_value5(temp_id)
            insieme_strumenti(temp_id).update_setpoint5 = True
            'priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If


        If InStr(message_temp, "errorend") <> 0 Then 'risposta negativa trasmissione canale

            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "b4end") <> 0 Then 'risposta positiva canale per la modifica del timer 2 e timer 1
            insieme_strumenti(temp_id).mvalue4_bool = False
            priority_setpoint = True
            insieme_strumenti(temp_id).mvalue17_bool = False
            check_bool_value17(temp_id)
            insieme_strumenti(temp_id).update_setpoint4 = True
            insieme_strumenti(temp_id).update_setpoint17 = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "b5")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
            'Try
            '    Dim data_temp() As String = buffer_sp1.Split("b")
            '    If data_temp.Length > 0 Then
            '        Dim id_temp As Integer = Val(Mid(data_temp(0), 1, 2))
            '        insieme_strumenti(id_temp).value4 = decode_timer2_1(insieme_strumenti(id_temp).value4, data_temp(1), "b4")
            '    End If
            'Catch ex As Exception

            'End Try
            'puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message)
        End If
        If InStr(message_temp, "b6end") <> 0 Then 'risposta positiva canale per la modifica del timer 2 e timer 1
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "1A")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
            'Try
            '    Dim data_temp() As String = buffer_sp1.Split("b")
            '    If data_temp.Length > 0 Then
            '        Dim id_temp As Integer = Val(Mid(data_temp(0), 1, 2))
            '        insieme_strumenti(id_temp).value4 = decode_timer2_1(insieme_strumenti(id_temp).value4, data_temp(1), "b4")
            '    End If
            'Catch ex As Exception

            'End Try
            'puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message)
        End If
        If InStr(message_temp, "bkend") <> 0 Then 'risposta positiva canale per la modifica del timer 3 e timer 4 timer 5
            insieme_strumenti(temp_id).mvalue4_bool = False
            insieme_strumenti(temp_id).mvalue17_bool = False
            check_bool_value17(temp_id)
            insieme_strumenti(temp_id).update_setpoint4 = True
            insieme_strumenti(temp_id).update_setpoint17 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "b5")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub

            'Try
            '    Dim data_temp() As String = buffer_sp1.Split("b")
            '    If data_temp.Length > 0 Then
            '        Dim id_temp As Integer = Val(Mid(data_temp(0), 1, 2))
            '        insieme_strumenti(id_temp).value4 = decode_timer3_4_5(insieme_strumenti(id_temp).value4, data_temp(1), "bk")
            '    End If
            'Catch ex As Exception

            'End Try
            'puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message)
        End If
        If InStr(message_temp, "b1end") <> 0 Then 'risposta positiva canale per la modifica del timer 3 e timer 4 timer 5
            'priority_setpoint = True
            insieme_strumenti(temp_id).mvalue4_bool = False
            check_bool_value4(temp_id)
            insieme_strumenti(temp_id).update_setpoint4 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "b5")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
            'Try
            '    Dim data_temp() As String = buffer_sp1.Split("b")
            '    If data_temp.Length > 0 Then
            '        Dim id_temp As Integer = Val(Mid(data_temp(0), 1, 2))
            '        insieme_strumenti(id_temp).value4 = decode_option(insieme_strumenti(id_temp).value4, data_temp(1), "b1")
            '    End If
            'Catch ex As Exception

            'End Try
            'puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message)
        End If
        If InStr(message_temp, "b8end") <> 0 Then 'risposta positiva canale per la modifica del timer 3 e timer 4 timer 5
            'priority_setpoint = True
            insieme_strumenti(temp_id).mvalue4_bool = False
            check_bool_value4(temp_id)
            insieme_strumenti(temp_id).update_setpoint4 = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "b5")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
            'Try
            '    Dim data_temp() As String = buffer_sp1.Split("b")
            '    If data_temp.Length > 0 Then
            '        Dim id_temp As Integer = Val(Mid(data_temp(0), 1, 2))
            '        insieme_strumenti(id_temp).value4 = decode_option_flow(insieme_strumenti(id_temp).value4, data_temp(1), "b8")
            '    End If
            'Catch ex As Exception

            'End Try
            'puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message)
        End If
        If InStr(message_temp, "b2end") <> 0 Then 'risposta positiva canale per la modifica del password
            'Try
            '    Dim data_temp() As String = buffer_sp1.Split("b")
            '    If data_temp.Length > 0 Then
            '        Dim id_temp As Integer = Val(Mid(data_temp(0), 1, 2))
            '        insieme_strumenti(id_temp).value4 = decode_option_password(insieme_strumenti(id_temp).value4, data_temp(1), "b2")
            '    End If
            'Catch ex As Exception

            'End Try
            'puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message)
            'priority_setpoint = True
            insieme_strumenti(temp_id).mvalue4_bool = False
            check_bool_value4(temp_id)
            insieme_strumenti(temp_id).update_setpoint4 = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "b5")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "b3end") <> 0 Then 'risposta positiva canale per la modifica dell'orologio
            'Try
            '    Dim data_temp() As String = buffer_sp1.Split("b")
            '    If data_temp.Length > 0 Then
            '        Dim id_temp As Integer = Val(Mid(data_temp(0), 1, 2))
            '        insieme_strumenti(id_temp).value4 = decode_option_clock(insieme_strumenti(id_temp).value4, data_temp(1), "b3")
            '    End If
            'Catch ex As Exception

            'End Try
            'puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message)
            insieme_strumenti(temp_id).mvalue4_bool = False
            check_bool_value4(temp_id)
            insieme_strumenti(temp_id).update_setpoint4 = True
            'priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(Format(temp_id, "00") + "b5")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If


        'tower--------------------------------------------------------------------------
        'tower  modifica set point
        If (InStr(message_temp, "inhibwMT0ok") <> 0 Or InStr(message_temp, "inhibwMT1ok") <> 0 Or InStr(message_temp, "inhibwMT2ok") <> 0 Or InStr(message_temp, "inhibwMT3ok") <> 0 Or InStr(message_temp, "inhibwMT4ok") <> 0 Or InStr(message_temp, "inhibwMT5ok") <> 0) And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue14_bool = False
            check_bool_value14(temp_id)
            insieme_strumenti(temp_id).update_setpoint14 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            'Me.mpuntatore_strumento.SendMessage(id_invio + "inhibr")
            ' Me.mpuntatore_strumento.SendMessage(id_invio+ "bi1w4r")
            Exit Sub
        End If
        If InStr(message_temp, "bleedwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue13_bool = False
            check_bool_value13(temp_id)
            insieme_strumenti(temp_id).update_setpoint13 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            'Me.mpuntatore_strumento.SendMessage(id_invio + "bleedr")
            Exit Sub
        End If
        If InStr(message_temp, "flowmwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue25_bool = False
            check_bool_value25(temp_id)
            insieme_strumenti(temp_id).update_setpoint25 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            'Me.mpuntatore_strumento.SendMessage(id_invio + "flowstr")
            Exit Sub
        End If
        If InStr(message_temp, "logsewok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue12_bool = False
            check_bool_value12(temp_id)
            insieme_strumenti(temp_id).update_setpoint12 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            '(Me.mpuntatore_strumento.SendMessage(id_invio + "logser"))
            Exit Sub
        End If
        If InStr(message_temp, "optiowok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue7_bool = False
            check_bool_value7(temp_id)
            insieme_strumenti(temp_id).update_setpoint7 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            'Me.mpuntatore_strumento.SendMessage(id_invio + "optior")
            Exit Sub
        End If
        If InStr(message_temp, "setp2wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue27_bool = False
            insieme_strumenti(temp_id).mvalue7_bool = False
            check_bool_value27(temp_id)
            insieme_strumenti(temp_id).update_setpoint27 = True
            insieme_strumenti(temp_id).update_setpoint7 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            'Me.mpuntatore_strumento.SendMessage(id_invio + "setp2r")
            Exit Sub
        End If
        If InStr(message_temp, "setp3wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue28_bool = False
            insieme_strumenti(temp_id).mvalue7_bool = False

            check_bool_value28(temp_id)
            insieme_strumenti(temp_id).update_setpoint28 = True
            insieme_strumenti(temp_id).update_setpoint7 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            'Me.mpuntatore_strumento.SendMessage(id_invio + "setp3r")
            Exit Sub
        End If

        If InStr(message_temp, "passcwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then

            insieme_strumenti(temp_id).mvalue11_bool = False
            check_bool_value11(temp_id)
            insieme_strumenti(temp_id).update_setpoint11 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            'Me.mpuntatore_strumento.SendMessage(id_invio + "alarmr")
            Exit Sub
        End If
        If InStr(message_temp, "unitswok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue1_bool = False
            check_bool_value1(temp_id)
            insieme_strumenti(temp_id).update_setpoint1 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            'Me.mpuntatore_strumento.SendMessage(id_invio + "alarmr")
            Exit Sub
        End If

        If InStr(message_temp, "alarmwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue26_bool = False
            check_bool_value26(temp_id)
            insieme_strumenti(temp_id).update_setpoint26 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            'Me.mpuntatore_strumento.SendMessage(id_invio + "alarmr")
            Exit Sub
        End If
        If InStr(message_temp, "boilewok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue27_bool = False
            check_bool_value27(temp_id)
            insieme_strumenti(temp_id).update_setpoint27 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            'Me.mpuntatore_strumento.SendMessage(id_invio + "alarmr")
            Exit Sub
        End If

        If InStr(message_temp, "bioc1wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            'priority_setpoint = True
            wait_response_invio = True
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            'Me.mpuntatore_strumento.SendMessage(id_invio+ "bioc1r")
            Exit Sub
        End If
        If InStr(message_temp, "bi1w1wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            wait_response_invio = True
            'priority_setpoint = True
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            ' Me.mpuntatore_strumento.SendMessage(id_invio+ "bi1w1r")
            Exit Sub
        End If
        If InStr(message_temp, "bi1w2wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            wait_response_invio = True
            'priority_setpoint = True
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            'Me.mpuntatore_strumento.SendMessage(id_invio+ "bi1w2r")
            Exit Sub
        End If
        If InStr(message_temp, "bi1w3wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            wait_response_invio = True
            'priority_setpoint = True
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            ' Me.mpuntatore_strumento.SendMessage(id_invio+ "bi1w3r")
            Exit Sub
        End If
        If InStr(message_temp, "bi1w4wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 And insieme_strumenti(temp_id).tipo_strumento = "Tower" Then
            wait_response_invio = True
            priority_setpoint = True
            If busy_strumento_invio Then
                reading_sp = True
                'reading_sp_bio1 = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            insieme_strumenti(temp_id).mvalue15_bool = False
            insieme_strumenti(temp_id).mvalue16_bool = False
            insieme_strumenti(temp_id).mvalue17_bool = False
            insieme_strumenti(temp_id).mvalue18_bool = False
            insieme_strumenti(temp_id).mvalue19_bool = False
            insieme_strumenti(temp_id).update_setpoint15 = True
            insieme_strumenti(temp_id).update_setpoint16 = True
            insieme_strumenti(temp_id).update_setpoint17 = True
            insieme_strumenti(temp_id).update_setpoint18 = True
            insieme_strumenti(temp_id).update_setpoint19 = True
            'Me.mpuntatore_strumento.SendMessage(id_invio+ "bioc1r")
            ' Me.mpuntatore_strumento.SendMessage(id_invio+ "bi1w4r")
            Exit Sub
        End If
        If InStr(message_temp, "bi1w4wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 And insieme_strumenti(temp_id).tipo_strumento = "LDtower" Then
            wait_response_invio = True
            'priority_setpoint = True
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            ' Me.mpuntatore_strumento.SendMessage(id_invio+ "bi1w3r")
            Exit Sub
        End If
        If InStr(message_temp, "bi1w5wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 And insieme_strumenti(temp_id).tipo_strumento = "LDtower" Then
            wait_response_invio = True
            'priority_setpoint = True
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            ' Me.mpuntatore_strumento.SendMessage(id_invio+ "bi1w3r")
            Exit Sub
        End If
        If InStr(message_temp, "bi1w6wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 And insieme_strumenti(temp_id).tipo_strumento = "LDtower" Then
            wait_response_invio = True
            'priority_setpoint = True
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            ' Me.mpuntatore_strumento.SendMessage(id_invio+ "bi1w3r")
            Exit Sub
        End If
        If InStr(message_temp, "bi1w7wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 And insieme_strumenti(temp_id).tipo_strumento = "LDtower" Then
            wait_response_invio = True
            priority_setpoint = True
            If busy_strumento_invio Then
                reading_sp = True
                'reading_sp_bio1 = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            insieme_strumenti(temp_id).mvalue15_bool = False
            insieme_strumenti(temp_id).mvalue16_bool = False
            insieme_strumenti(temp_id).mvalue17_bool = False
            insieme_strumenti(temp_id).mvalue18_bool = False
            insieme_strumenti(temp_id).mvalue19_bool = False
            insieme_strumenti(temp_id).mvalue27_bool = False
            insieme_strumenti(temp_id).mvalue28_bool = False
            insieme_strumenti(temp_id).mvalue29_bool = False
            insieme_strumenti(temp_id).update_setpoint15 = True
            insieme_strumenti(temp_id).update_setpoint16 = True
            insieme_strumenti(temp_id).update_setpoint17 = True
            insieme_strumenti(temp_id).update_setpoint18 = True
            insieme_strumenti(temp_id).update_setpoint19 = True
            insieme_strumenti(temp_id).update_setpoint27 = True
            insieme_strumenti(temp_id).update_setpoint28 = True
            insieme_strumenti(temp_id).update_setpoint29 = True
            'Me.mpuntatore_strumento.SendMessage(id_invio+ "bioc1r")
            ' Me.mpuntatore_strumento.SendMessage(id_invio+ "bi1w4r")
            Exit Sub
        End If

        If InStr(message_temp, "bioc2wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            wait_response_invio = True
            ' priority_setpoint = True
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            'Me.mpuntatore_strumento.SendMessage(id_invio+ "bioc1r")
            Exit Sub
        End If
        If InStr(message_temp, "bi2w1wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            wait_response_invio = True
            'priority_setpoint = True
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            ' Me.mpuntatore_strumento.SendMessage(id_invio+ "bi1w1r")
            Exit Sub
        End If
        If InStr(message_temp, "bi2w2wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            wait_response_invio = True
            'priority_setpoint = True
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            'Me.mpuntatore_strumento.SendMessage(id_invio+ "bi1w2r")
            Exit Sub
        End If
        If InStr(message_temp, "bi2w3wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            wait_response_invio = True
            'priority_setpoint = True
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            ' Me.mpuntatore_strumento.SendMessage(id_invio+ "bi1w3r")
            Exit Sub
        End If
        If InStr(message_temp, "bi2w4wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 And insieme_strumenti(temp_id).tipo_strumento = "Tower" Then
            insieme_strumenti(temp_id).mvalue20_bool = False
            insieme_strumenti(temp_id).mvalue21_bool = False
            insieme_strumenti(temp_id).mvalue22_bool = False
            insieme_strumenti(temp_id).mvalue23_bool = False
            insieme_strumenti(temp_id).mvalue24_bool = False
            priority_setpoint = True
            insieme_strumenti(temp_id).update_setpoint20 = True
            insieme_strumenti(temp_id).update_setpoint21 = True
            insieme_strumenti(temp_id).update_setpoint22 = True
            insieme_strumenti(temp_id).update_setpoint23 = True
            insieme_strumenti(temp_id).update_setpoint24 = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                'reading_sp_bio2 = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            'Me.mpuntatore_strumento.SendMessage(id_invio+ "bioc1r")
            ' Me.mpuntatore_strumento.SendMessage(id_invio+ "bi1w4r")
            Exit Sub
        End If
        If InStr(message_temp, "bi2w4wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 And insieme_strumenti(temp_id).tipo_strumento = "LDtower" Then
            wait_response_invio = True
            'priority_setpoint = True
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            ' Me.mpuntatore_strumento.SendMessage(id_invio+ "bi1w3r")
            Exit Sub
        End If
        If InStr(message_temp, "bi2w5wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 And insieme_strumenti(temp_id).tipo_strumento = "LDtower" Then
            wait_response_invio = True
            'priority_setpoint = True
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            ' Me.mpuntatore_strumento.SendMessage(id_invio+ "bi1w3r")
            Exit Sub
        End If
        If InStr(message_temp, "bi2w6wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 And insieme_strumenti(temp_id).tipo_strumento = "LDtower" Then
            wait_response_invio = True
            'priority_setpoint = True
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            ' Me.mpuntatore_strumento.SendMessage(id_invio+ "bi1w3r")
            Exit Sub
        End If
        If InStr(message_temp, "bi2w7wok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 And insieme_strumenti(temp_id).tipo_strumento = "LDtower" Then
            insieme_strumenti(temp_id).mvalue20_bool = False
            insieme_strumenti(temp_id).mvalue21_bool = False
            insieme_strumenti(temp_id).mvalue22_bool = False
            insieme_strumenti(temp_id).mvalue23_bool = False
            insieme_strumenti(temp_id).mvalue24_bool = False
            insieme_strumenti(temp_id).mvalue30_bool = False
            insieme_strumenti(temp_id).mvalue31_bool = False
            insieme_strumenti(temp_id).mvalue32_bool = False
            priority_setpoint = True
            insieme_strumenti(temp_id).update_setpoint20 = True
            insieme_strumenti(temp_id).update_setpoint21 = True
            insieme_strumenti(temp_id).update_setpoint22 = True
            insieme_strumenti(temp_id).update_setpoint23 = True
            insieme_strumenti(temp_id).update_setpoint24 = True
            insieme_strumenti(temp_id).update_setpoint30 = True
            insieme_strumenti(temp_id).update_setpoint31 = True
            insieme_strumenti(temp_id).update_setpoint32 = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                'reading_sp_bio2 = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            ' Me.mpuntatore_strumento.SendMessage(id_invio+ "bi1w3r")
            Exit Sub
        End If
        If InStr(message_temp, "calcdwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue33_bool = False
            check_bool_value33(temp_id)
            insieme_strumenti(temp_id).update_setpoint33 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            'Me.mpuntatore_strumento.SendMessage(id_invio + "alarmr")
            Exit Sub
        End If

        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "unitsr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value1 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint1 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint1 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 11
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try

        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "valuer") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value2 = message_temp
                    '  If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint2 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint2 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).value2_1 = message_temp
                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 12
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "algenr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value3 = message_temp
                    ' If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint3 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint3 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 13
                    sequenza_timer = False
                End If
                numero_interrogazione = 0

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "config") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                'If InStr(message_temp, "537401") <> 0 Then
                '    Try
                '        Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\537401.txt", True)
                '            writer.WriteLine("#" + Now.ToString + message_temp + "#" + Format(temp_id, "00"))
                '        End Using
                '    Catch ex As Exception

                '    End Try
                'End If
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value4 = message_temp
                    '    If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint4 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint4 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then

                    sequnza_interrogazione = 14
                    sequenza_timer = False
                End If

                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "clockr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value5 = message_temp
                    ' If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint5 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint5 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 15
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "stbior") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value6 = message_temp
                    '  If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint6 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint6 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 16
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "optior") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value7 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint7 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint7 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 17
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "tota1r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value8 = message_temp
                    ' If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint8 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint8 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 18
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
            Catch ex As Exception
                Exit Sub
            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "tota2r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value9 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint9 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint9 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 19
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "stoutr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value10 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint10 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint10 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 20
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If

        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "passcr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value11 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint11 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint11 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 21
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "logser") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value12 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint12 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint12 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 22
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "bleedr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value13 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint13 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint13 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 23
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "inhibr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value14 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint14 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint14 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 24
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "bioc1r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value15 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint15 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint15 = False
                    End If

                    ' If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 25
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "bi1w1r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value16 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint16 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint16 = False
                    End If

                    ' End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 26
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "bi1w2r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value17 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint17 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint17 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 27
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "bi1w3r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value18 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint18 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint18 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 28
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "bi1w4r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value19 = message_temp
                    ' If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint19 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint19 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 29
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "bioc2r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value20 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint20 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint20 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 30
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If

        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "bi2w1r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value21 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint21 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint21 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 31
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "bi2w2r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value22 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint22 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint22 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 32
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "bi2w3r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value23 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint23 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint23 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 33
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "bi2w4r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value24 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint24 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint24 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 34
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "flowmr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value25 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint25 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint25 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "tower" Or tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 35
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "alarmr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value26 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint26 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint26 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And tipo_strumento_sequenza = "tower" Then
                    If insieme_strumenti(temp_id).type_tower = 1 Then
                        If insieme_strumenti(temp_id).mail_csv_setting Then
                            sequnza_interrogazione = 38
                            sequenza_timer = False
                        Else
                            If insieme_strumenti(temp_id).mBoiler Then
                                sequnza_interrogazione = 39
                                sequenza_timer = False

                            Else
                                sequnza_interrogazione = 0
                                maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                                sequenza_timer = False
                            End If
                        End If
                    End If
                    If insieme_strumenti(temp_id).type_tower > 1 Then
                        sequnza_interrogazione = 36
                        sequenza_timer = False
                    End If
                End If
                If sequenza_timer And tipo_strumento_sequenza = "LDtower" Then
                    sequnza_interrogazione = 71
                    sequenza_timer = False

                End If


                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If

        '---LD Tower
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "bi1w5r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value27 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint27 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint27 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 72
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "bi1w6r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value28 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint28 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint28 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 73
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "bi1w7r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value29 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint29 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint29 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 74
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "bi2w5r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value30 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint30 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint30 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 75
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "bi2w6r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value31 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint31 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint31 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 76
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If

        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "bi2w7r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value32 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint32 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint32 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 77
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "calcdr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value33 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint33 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint33 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 78
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "servir") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value34 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint34 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint34 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "LDtower") Then
                    sequnza_interrogazione = 0
                    maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        '---end LD Tower
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "setp2r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value27 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint27 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint27 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And tipo_strumento_sequenza = "tower" Then
                    If insieme_strumenti(temp_id).type_tower = 2 Then
                        If insieme_strumenti(temp_id).mail_csv_setting Then
                            sequnza_interrogazione = 38
                            sequenza_timer = False
                        Else
                            sequnza_interrogazione = 0
                            maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                            sequenza_timer = False
                        End If
                    End If
                    If insieme_strumenti(temp_id).type_tower > 2 Then
                        sequnza_interrogazione = 37
                        sequenza_timer = False
                    End If
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If

        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "setp3r") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value28 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint28 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint28 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And tipo_strumento_sequenza = "tower" Then
                    If insieme_strumenti(temp_id).mail_csv_setting Then
                        sequnza_interrogazione = 38
                        sequenza_timer = False
                    Else

                        sequnza_interrogazione = 0
                        maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                        sequenza_timer = False
                    End If
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If        'tower--------------------------------------------------------------------------
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "emailr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value29 = message_temp
                    ' If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint29 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint29 = False
                    End If

                    'End If

                    query.aggiorna_csv(insieme_strumenti(temp_id).tipo_strumento, insieme_strumenti(temp_id).mail_csv_setting, insieme_strumenti(temp_id).value4, insieme_strumenti(temp_id).value12, insieme_strumenti(temp_id).value29, midentificativo, temp_id)
                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And tipo_strumento_sequenza = "tower" Then

                    sequnza_interrogazione = 0
                    maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                    sequenza_timer = False

                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If InStr(message_temp, "MT") <> 0 And InStr(message_temp, "boiler") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value27 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint27 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint27 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And tipo_strumento_sequenza = "tower" Then

                    sequnza_interrogazione = 0
                    maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                    sequenza_timer = False

                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If        'ld--------------------------------------------------------------------------

        'lds--------------------------------------------------------------------------
        'ld e lds modifica set point
        If InStr(message_temp, "compswok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue12_bool = False
            check_bool_value12(temp_id)
            insieme_strumenti(temp_id).update_setpoint12 = True
            priority_setpoint = True
            wait_response_invio = True
            'busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If

            'Me.mpuntatore_strumento.SendMessage(id_invio + "compphr")
            Exit Sub
        End If

        If InStr(message_temp, "clockwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            If insieme_strumenti(temp_id).tipo_strumento = "Tower" Or insieme_strumenti(temp_id).tipo_strumento = "LDtower" Then
                insieme_strumenti(temp_id).mvalue5_bool = False
                check_bool_value5(temp_id)
                insieme_strumenti(temp_id).update_setpoint5 = True
            Else
                insieme_strumenti(temp_id).mvalue13_bool = False
                check_bool_value13(temp_id)
                insieme_strumenti(temp_id).update_setpoint13 = True
            End If

            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            'Me.mpuntatore_strumento.SendMessage(id_invio + "clocksr")
            Exit Sub
        End If

        If InStr(message_temp, "setpnwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue7_bool = False
            check_bool_value7(temp_id)
            insieme_strumenti(temp_id).update_setpoint7 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            'Me.mpuntatore_strumento.SendMessage(id_invio + "setpntr")
            Exit Sub
        End If
        If InStr(message_temp, "alldowok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue9_bool = False
            check_bool_value9(temp_id)
            insieme_strumenti(temp_id).update_setpoint9 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            'Me.mpuntatore_strumento.SendMessage(id_invio + "alldosr")
            Exit Sub
        End If

        If InStr(message_temp, "maoutwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue14_bool = False
            check_bool_value14(temp_id)
            insieme_strumenti(temp_id).update_setpoint14 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            'Me.mpuntatore_strumento.SendMessage(id_invio + "maoutsr")
            Exit Sub
        End If
        If InStr(message_temp, "allprwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue10_bool = False
            check_bool_value10(temp_id)
            insieme_strumenti(temp_id).update_setpoint10 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            'Me.mpuntatore_strumento.SendMessage(id_invio + "allprbr")
            Exit Sub
        End If
        If InStr(message_temp, "minmxwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue16_bool = False
            check_bool_value16(temp_id)
            insieme_strumenti(temp_id).update_setpoint16 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            'Me.mpuntatore_strumento.SendMessage(id_invio + "minmaxr")
            Exit Sub
        End If
        If InStr(message_temp, "flowswok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue11_bool = False
            check_bool_value11(temp_id)
            insieme_strumenti(temp_id).update_setpoint11 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Me.mpuntatore_strumento.SendMessage(id_invio + "flowstr")
            Exit Sub
        End If

        If InStr(message_temp, "paramwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue8_bool = False
            check_bool_value8(temp_id)
            insieme_strumenti(temp_id).update_setpoint8 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            'Me.mpuntatore_strumento.SendMessage(id_invio + "paramtr")
            Exit Sub
        End If
        If InStr(message_temp, "logstwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue15_bool = False
            check_bool_value15(temp_id)
            insieme_strumenti(temp_id).update_setpoint15 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            'Me.mpuntatore_strumento.SendMessage(id_invio + "logsetr")
            Exit Sub
        End If
        If InStr(message_temp, "setskwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue14_bool = False
            check_bool_value14(temp_id)
            insieme_strumenti(temp_id).update_setpoint14 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            'Me.mpuntatore_strumento.SendMessage(id_invio + "setstkr")
            Exit Sub
        End If
        If InStr(message_temp, "diginwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue16_bool = False
            check_bool_value16(temp_id)
            insieme_strumenti(temp_id).update_setpoint16 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(id_invio + "diginpr")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "resalwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue3_bool = False
            check_bool_value3(temp_id)
            insieme_strumenti(temp_id).update_setpoint3 = True
            'priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(id_invio + "diginpr")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "fastcwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue2_bool = False
            check_bool_value2(temp_id)
            insieme_strumenti(temp_id).update_setpoint2 = True
            'priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(id_invio + "diginpr")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "labelwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue18_bool = False
            check_bool_value18(temp_id)
            insieme_strumenti(temp_id).update_setpoint18 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(id_invio + "diginpr")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "maismwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue19_bool = False
            insieme_strumenti(temp_id).mvalue18_bool = False
            check_bool_value18(temp_id)
            check_bool_value19(temp_id)
            insieme_strumenti(temp_id).update_setpoint19 = True
            insieme_strumenti(temp_id).update_setpoint18 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(id_invio + "diginpr")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "manuawok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue5_bool = False
            check_bool_value5(temp_id)
            insieme_strumenti(temp_id).update_setpoint5 = True
            'priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(id_invio + "diginpr")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "wmwtewok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue20_bool = False
            check_bool_value20(temp_id)
            insieme_strumenti(temp_id).update_setpoint20 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(id_invio + "diginpr")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "flowawok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue21_bool = False
            check_bool_value21(temp_id)
            insieme_strumenti(temp_id).update_setpoint21 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(id_invio + "diginpr")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        '---------------wh
        If InStr(message_temp, "autstwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue20_bool = False
            check_bool_value20(temp_id)
            insieme_strumenti(temp_id).update_setpoint20 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(id_invio + "diginpr")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "alertwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue21_bool = False
            check_bool_value21(temp_id)
            insieme_strumenti(temp_id).update_setpoint21 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(id_invio + "diginpr")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "timepwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue22_bool = False
            check_bool_value22(temp_id)
            insieme_strumenti(temp_id).update_setpoint22 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(id_invio + "diginpr")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        '---------------lotus
        If InStr(message_temp, "wtmetwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue20_bool = False
            check_bool_value20(temp_id)
            insieme_strumenti(temp_id).update_setpoint20 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(id_invio + "diginpr")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "rescywok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue21_bool = False
            check_bool_value21(temp_id)
            insieme_strumenti(temp_id).update_setpoint21 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(id_invio + "diginpr")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If

        '---------------------ldma
        If (InStr(message_temp, "setpn1wok") <> 0 Or InStr(message_temp, "setpn2wok") <> 0 _
            Or InStr(message_temp, "setpn3wok") <> 0 Or InStr(message_temp, "setpn4wok") <> 0 _
            Or InStr(message_temp, "setpn5wok") <> 0 Or InStr(message_temp, "setpn6wok") <> 0 Or InStr(message_temp, "setpn7wok") <> 0 _
            Or InStr(message_temp, "setpn8wok") <> 0 Or InStr(message_temp, "setpn9wok") <> 0 Or InStr(message_temp, "setpnAwok") <> 0 _
            Or InStr(message_temp, "setpnBwok") <> 0) And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue7_bool = False
            check_bool_value7(temp_id)
            insieme_strumenti(temp_id).update_setpoint7 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            'Me.mpuntatore_strumento.SendMessage(id_invio + "setpntr")
            Exit Sub
        End If
        If InStr(message_temp, "namelwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mtipo_strumento_bool = False
            'insieme_strumenti(temp_id).update_setpoint = True
            'priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(id_invio + "diginpr")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If


        '---------------lds
        If InStr(message_temp, "selfcwok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue20_bool = False
            check_bool_value20(temp_id)
            insieme_strumenti(temp_id).update_setpoint20 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(id_invio + "diginpr")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "circowok") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue21_bool = False
            check_bool_value21(temp_id)
            insieme_strumenti(temp_id).update_setpoint21 = True
            priority_setpoint = True
            wait_response_invio = True
            If busy_strumento_invio Then
                reading_sp = True
                free_reading()
            End If
            busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(id_invio + "diginpr")
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
            End If
            Exit Sub
        End If
        If InStr(message_temp, "ermokrend") <> 0 And InStr(message_temp, id_invio + "&") <> 0 Then

            'wait_response_invio = True
            'busy_strumento_invio = False
            'Me.mpuntatore_strumento.SendMessage(id_invio+ "diginpr")
            Try
                If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                    puntatore_pc_scrittura_setpoint.scrittura_sp = False
                    puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
                End If
            Catch ex As Exception

            End Try
            Exit Sub
        End If

        'end ld e lds modifica setpoint
        If (InStr(message_temp, "LDMA") <> 0) And InStr(message_temp, "ildma") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue_log_ldma_bool = True
            insieme_strumenti(temp_id).reading_log = True
            If InStr(message_temp, "#vuoto#") <> 0 Then
                Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "sldma")
                insieme_strumenti(temp_id).reading_log = False
            Else
                Dim data() As String = message_temp.Split("|")
                Dim result_query As Boolean = True
                Try
                    Dim data1() As String = data(2).Split("&")
                    If data1.Length > 0 Then
                        Dim data2() As String = data1(1).Split("#")
                        Dim data3() As String = data2(1).Split(",")
                        Dim data_temp As DateTime = Date.Parse(data3(0))

                        result_query = query.log_ldma(identificativo, temp_id, data_temp, Val(data3(1)), Val(data3(2)), Val(data3(3)), Val(data3(4)), Val(data3(5)), insieme_strumenti(temp_id).list_10_log, insieme_strumenti(temp_id).first_log, insieme_strumenti(temp_id).counter_log)
                        insieme_strumenti(temp_id).first_log = False
                    End If
                Catch ex As Exception

                End Try
                If result_query Then
                    Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "nldma")
                Else
                    Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "sldma")
                    insieme_strumenti(temp_id).reading_log = False
                End If

            End If
        End If
        If (InStr(message_temp, "LDLG") <> 0) And InStr(message_temp, "ildlg") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            insieme_strumenti(temp_id).mvalue_log_ldma_bool = True
            insieme_strumenti(temp_id).reading_log = True
            If InStr(message_temp, "#vuoto#") <> 0 Then
                Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "sldlg")
                insieme_strumenti(temp_id).reading_log = False
            Else
                Dim data() As String = message_temp.Split("|")
                Dim result_query As Boolean = True
                Try
                    Dim data1() As String = data(2).Split("&")
                    If data1.Length > 0 Then
                        Dim data2() As String = data1(1).Split("#")
                        Dim data3() As String = data2(1).Split(",")
                        Dim data_temp As DateTime = Date.Parse(data3(0))

                        result_query = query.log_ldlg(identificativo, temp_id, data_temp, Val(data3(1)), Val(data3(2)), Val(data3(3)), Val(data3(4)), Val(data3(5)), Val(data3(6)), Val(data3(7)), Val(data3(8)), Val(data3(9)), Val(data3(10)), Val(data3(11)), Val(data3(12)), insieme_strumenti(temp_id).list_10_log, insieme_strumenti(temp_id).first_log, insieme_strumenti(temp_id).counter_log)
                        insieme_strumenti(temp_id).first_log = False
                    End If
                Catch ex As Exception

                End Try
                If result_query Then
                    Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "nldlg")
                Else
                    Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "sldlg")
                    insieme_strumenti(temp_id).reading_log = False
                End If

            End If
        End If
        If (InStr(message_temp, "LD") <> 0 Or InStr(message_temp, "WD") <> 0 Or InStr(message_temp, "WH") <> 0 Or InStr(message_temp, "LTB") <> 0 Or InStr(message_temp, "LDMA") <> 0 Or InStr(message_temp, "LDLG") <> 0 _
            Or InStr(message_temp, "LTA") <> 0 Or InStr(message_temp, "LTD") <> 0 Or InStr(message_temp, "LTU") <> 0 _
            ) And InStr(message_temp, "config") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then


                insieme_strumenti(temp_id).value1 = message_temp
                'If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint1 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint1 = False
                End If

                'End If

                insieme_strumenti(temp_id).time_connessione = Now
            End If
            Try
                If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd" Or tipo_strumento_sequenza = "wh" Or _
                                       tipo_strumento_sequenza = "ltb" Or tipo_strumento_sequenza = "ltaud" Or tipo_strumento_sequenza = "ldma" Or tipo_strumento_sequenza = "ldlg") Then
                    If insieme_strumenti(temp_id).new_version = False Then
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                    End If

                    sequnza_interrogazione = 41
                    sequenza_timer = False
                End If
            Catch ex As Exception

            End Try
            numero_interrogazione = 0
            Exit Sub
        End If
        If (InStr(message_temp, "LD") <> 0 Or InStr(message_temp, "WD") <> 0 Or InStr(message_temp, "WH") <> 0 Or InStr(message_temp, "LTB") <> 0 _
            Or InStr(message_temp, "LTA") <> 0 Or InStr(message_temp, "LTD") <> 0 Or InStr(message_temp, "LTU") <> 0 _
            Or InStr(message_temp, "LDMA") <> 0 Or InStr(message_temp, "LDLG") <> 0) And InStr(message_temp, "valuer") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    If InStr(message_temp, "WH") <> 0 Then
                        insieme_strumenti(temp_id).value2 = message_temp
                    End If
                    insieme_strumenti(temp_id).value2 = message_temp
                    insieme_strumenti(temp_id).value2_1 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint2 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint2 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now

                End If
                If sequenza_timer And real_time_bool = False And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" _
                                                                  Or tipo_strumento_sequenza = "wd" Or tipo_strumento_sequenza = "wh" _
                                                                  Or tipo_strumento_sequenza = "ltb" Or tipo_strumento_sequenza = "ltaud" Or tipo_strumento_sequenza = "ldma" Or tipo_strumento_sequenza = "ldlg") Then
                    If insieme_strumenti(temp_id).new_version = False Then
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                    End If

                    sequnza_interrogazione = 42
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception

            End Try

        End If
        If (InStr(message_temp, "LD") <> 0 Or InStr(message_temp, "WD") <> 0 Or InStr(message_temp, "WH") <> 0 Or InStr(message_temp, "LTB") <> 0 _
            Or InStr(message_temp, "LTA") <> 0 Or InStr(message_temp, "LTD") <> 0 Or InStr(message_temp, "LTU") <> 0 _
            Or InStr(message_temp, "LDMA") <> 0 Or InStr(message_temp, "LDLG") <> 0) And InStr(message_temp, "allrmr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then 'And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd") Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value3 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint3 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint3 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And real_time_bool = False And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" _
                                                                  Or tipo_strumento_sequenza = "wd" Or tipo_strumento_sequenza = "wh" Or _
                                                                  tipo_strumento_sequenza = "ltb" Or tipo_strumento_sequenza = "ltaud" Or tipo_strumento_sequenza = "ldma" Or tipo_strumento_sequenza = "ldlg") Then
                    If tipo_strumento_sequenza = "ldma" Or tipo_strumento_sequenza = "ldlg" Then
                        sequnza_interrogazione = 46
                    Else
                        If insieme_strumenti(temp_id).new_version = False Then
                            Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                        End If

                        sequnza_interrogazione = 43
                    End If
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception

            End Try
        End If
        If (InStr(message_temp, "LD") <> 0 Or InStr(message_temp, "WD") <> 0 Or InStr(message_temp, "WH") <> 0 Or InStr(message_temp, "LTB") <> 0 _
            Or InStr(message_temp, "LTA") <> 0 Or InStr(message_temp, "LTD") <> 0 Or InStr(message_temp, "LTU") <> 0 _
            ) And InStr(message_temp, "calibzr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then 'And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd") Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value4 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint4 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint4 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd" Or tipo_strumento_sequenza = "wh" Or tipo_strumento_sequenza = "ltb") Then
                    If insieme_strumenti(temp_id).new_version = False Then
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                    End If

                    sequnza_interrogazione = 44
                    sequenza_timer = False
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "ltaud") Then
                    sequnza_interrogazione = 45
                    sequenza_timer = False

                End If
                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception
            End Try
        End If
        If (InStr(message_temp, "LD") <> 0 Or InStr(message_temp, "WD") <> 0 Or InStr(message_temp, "WH") <> 0 Or InStr(message_temp, "LTB") <> 0) And (InStr(message_temp, "LD") <> 0 Or InStr(message_temp, "WD") <> 0 Or InStr(message_temp, "WH") <> 0 Or InStr(message_temp, "LTB") <> 0) And InStr(message_temp, "outptr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then 'And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd") Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value5 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                ' If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint5 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint5 = False
                End If

                'If

            End If
            If sequenza_timer And real_time_bool = False And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd" Or tipo_strumento_sequenza = "ltb") Then
                If insieme_strumenti(temp_id).new_version = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                End If

                sequnza_interrogazione = 45
                sequenza_timer = False
            End If
            If sequenza_timer And real_time_bool = False And (tipo_strumento_sequenza = "wh") Then
                If insieme_strumenti(temp_id).new_version = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                End If

                sequnza_interrogazione = 46
                sequenza_timer = False
            End If
            numero_interrogazione = 0
            Exit Sub
        End If
        If (InStr(message_temp, "LD") <> 0 Or InStr(message_temp, "WD") <> 0 Or InStr(message_temp, "WH") <> 0 _
            Or InStr(message_temp, "LTA") <> 0 Or InStr(message_temp, "LTD") <> 0 Or InStr(message_temp, "LTU") <> 0 _
            ) And InStr(message_temp, "clockr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then 'And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd") Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value6 = message_temp
                insieme_strumenti(temp_id).value6_1 = message_temp
                'If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint6 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint6 = False
                End If

                'End If

                insieme_strumenti(temp_id).time_connessione = Now
            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" _
                                   Or tipo_strumento_sequenza = "wd" Or tipo_strumento_sequenza = "wh" Or tipo_strumento_sequenza = "ltb" _
                                   Or tipo_strumento_sequenza = "ltaud") Then
                If insieme_strumenti(temp_id).new_version = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                End If

                sequnza_interrogazione = 46
                sequenza_timer = False
            End If
            numero_interrogazione = 0
            Exit Sub
        End If
        If (InStr(message_temp, "LD") <> 0 Or InStr(message_temp, "WD") <> 0 Or InStr(message_temp, "WH") <> 0 Or InStr(message_temp, "LTB") <> 0 _
            Or InStr(message_temp, "LTA") <> 0 Or InStr(message_temp, "LTD") <> 0 Or InStr(message_temp, "LTU") <> 0 _
            Or InStr(message_temp, "LDMA") <> 0 Or InStr(message_temp, "LDLG") <> 0) And InStr(message_temp, "setpntr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then 'And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd") Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value7 = message_temp
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint7 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint7 = False
                    End If

                    'End If

                    insieme_strumenti(temp_id).time_connessione = Now
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd" _
                                       Or tipo_strumento_sequenza = "wh" Or tipo_strumento_sequenza = "ltb" _
                                       Or tipo_strumento_sequenza = "ltaud" Or tipo_strumento_sequenza = "ldma" Or tipo_strumento_sequenza = "ldlg") Then
                    If insieme_strumenti(temp_id).new_version = False Then
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                    End If

                    sequnza_interrogazione = 47
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
            '            'numero_interrogazione = 0
        End If
        If (InStr(message_temp, "LD") <> 0 Or InStr(message_temp, "WD") <> 0 Or InStr(message_temp, "WH") Or InStr(message_temp, "LTB") <> 0 _
            Or InStr(message_temp, "LTA") <> 0 Or InStr(message_temp, "LTD") <> 0 Or InStr(message_temp, "LTU") <> 0 _
            Or InStr(message_temp, "LDMA") <> 0 Or InStr(message_temp, "LDLG") <> 0) And InStr(message_temp, "paramtr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then 'And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd") Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value8 = message_temp
                    insieme_strumenti(temp_id).time_connessione = Now
                    ' If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint8 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint8 = False
                    End If

                    'End If

                End If
                If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd" Or tipo_strumento_sequenza = "wh") Then
                    If insieme_strumenti(temp_id).new_version = False Then
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                    End If

                    sequnza_interrogazione = 48
                    sequenza_timer = False
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "ltb" Or tipo_strumento_sequenza = "ltaud") Then
                    If insieme_strumenti(temp_id).new_version = False Then
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                    End If

                    sequnza_interrogazione = 50
                    sequenza_timer = False
                End If
                If sequenza_timer And (tipo_strumento_sequenza = "ldma" Or tipo_strumento_sequenza = "ldlg") Then
                    'Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                    sequnza_interrogazione = 52
                    sequenza_timer = False
                End If

                numero_interrogazione = 0
                Exit Sub
            Catch ex As Exception
                Exit Sub
            End Try
        End If
        If (InStr(message_temp, "LD") <> 0 Or InStr(message_temp, "WD") <> 0 Or InStr(message_temp, "WH") <> 0) And InStr(message_temp, "alldosr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value9 = message_temp
                    insieme_strumenti(temp_id).time_connessione = Now
                    ' If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint9 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint9 = False
                    End If

                    'End If

                End If
                If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd" Or tipo_strumento_sequenza = "wh") Then
                    If insieme_strumenti(temp_id).new_version = False Then
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                    End If

                    sequnza_interrogazione = 49
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If (InStr(message_temp, "LD") <> 0 Or InStr(message_temp, "WD") <> 0 Or InStr(message_temp, "WH") <> 0) And InStr(message_temp, "allprbr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value10 = message_temp
                    insieme_strumenti(temp_id).time_connessione = Now
                    ' If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint10 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint10 = False
                    End If

                    'End If

                End If
                If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd" Or tipo_strumento_sequenza = "wh") Then
                    If insieme_strumenti(temp_id).new_version = False Then
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                    End If

                    sequnza_interrogazione = 50
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If (InStr(message_temp, "LD") <> 0 Or InStr(message_temp, "WD") <> 0 Or InStr(message_temp, "WH") <> 0 Or InStr(message_temp, "LTB") <> 0 _
            Or InStr(message_temp, "LTA") <> 0 Or InStr(message_temp, "LTD") <> 0 Or InStr(message_temp, "LTU") <> 0 _
            ) And InStr(message_temp, "flowstr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value11 = message_temp
                    insieme_strumenti(temp_id).time_connessione = Now
                    ' If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint11 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint11 = False
                    End If

                    'End If

                End If
                If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "wd" Or tipo_strumento_sequenza = "wh") Then
                    If insieme_strumenti(temp_id).new_version = False Then
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                        sequnza_interrogazione = 52
                    Else
                        sequnza_interrogazione = 51
                    End If


                    sequenza_timer = False
                Else
                    If sequenza_timer And (tipo_strumento_sequenza = "lds") Then
                        If insieme_strumenti(temp_id).new_version = False Then
                            Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                        End If
                        sequnza_interrogazione = 52
                        sequenza_timer = False
                    End If
                    If sequenza_timer And (tipo_strumento_sequenza = "ltb" Or tipo_strumento_sequenza = "ltaud") Then
                        sequnza_interrogazione = 53
                        sequenza_timer = False
                    End If

                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If (InStr(message_temp, "LD") <> 0 Or InStr(message_temp, "WD") <> 0 Or InStr(message_temp, "WH") <> 0) And InStr(message_temp, "compphr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            Try
                If InStr(message_temp, "BUSY") = 0 Then
                    insieme_strumenti(temp_id).value12 = message_temp
                    insieme_strumenti(temp_id).time_connessione = Now
                    'If insieme_strumenti(temp_id).mvalue_update Then
                    If insieme_strumenti(temp_id).update_setpoint12 = True Then
                        aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                        insieme_strumenti(temp_id).update_setpoint12 = False
                    End If

                    'End If

                End If
                If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "wd" Or tipo_strumento_sequenza = "wh") Then
                    If insieme_strumenti(temp_id).new_version = False Then
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                    End If

                    sequnza_interrogazione = 52
                    sequenza_timer = False
                End If
                numero_interrogazione = 0
                Exit Sub

            Catch ex As Exception

            End Try
        End If
        If (InStr(message_temp, "LD") <> 0 Or InStr(message_temp, "WD") <> 0 Or InStr(message_temp, "WH") <> 0 Or InStr(message_temp, "LDMA") <> 0 Or InStr(message_temp, "LDLG") <> 0) And InStr(message_temp, "clocksr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value13 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                ' If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint13 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint13 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd" Or tipo_strumento_sequenza = "wh" Or tipo_strumento_sequenza = "ldma" Or tipo_strumento_sequenza = "ldlg") Then
                ' sequnza_interrogazione = 0
                If insieme_strumenti(temp_id).new_version = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                End If

                sequnza_interrogazione = 53
                sequenza_timer = False
            End If
            numero_interrogazione = 0
            Exit Sub
        End If
        If (InStr(message_temp, "LD") <> 0 Or InStr(message_temp, "WD") <> 0 Or InStr(message_temp, "WH") <> 0 Or InStr(message_temp, "LTB") <> 0 _
            Or InStr(message_temp, "LTA") <> 0 Or InStr(message_temp, "LTD") <> 0 Or InStr(message_temp, "LTU") <> 0 _
            Or InStr(message_temp, "LDMA") <> 0 Or InStr(message_temp, "LDLG") <> 0) And InStr(message_temp, "logsetr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value15 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                'If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint15 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint15 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "ltaud" Or tipo_strumento_sequenza = "wd" Or tipo_strumento_sequenza = "wh" Or tipo_strumento_sequenza = "ltb" Or tipo_strumento_sequenza = "ldma" Or tipo_strumento_sequenza = "ldlg") Then
                Select Case tipo_strumento_sequenza
                    Case "lds"
                        sequnza_interrogazione = 54
                    Case "ld"
                        sequnza_interrogazione = 54
                    Case "wd"
                        sequnza_interrogazione = 56
                    Case "wh"
                        sequnza_interrogazione = 57
                    Case "ltaud"
                        'sequnza_interrogazione = 54 bypass uscita in corrente
                        sequnza_interrogazione = 58

                    Case "ltb"
                        'sequnza_interrogazione = 54 bypass uscita in corrente
                        sequnza_interrogazione = 58
                    Case "ldma"
                        'sequnza_interrogazione = 54 bypass uscita in corrente
                        sequnza_interrogazione = 58
                    Case "ldlg"
                        'sequnza_interrogazione = 54 bypass uscita in corrente
                        sequnza_interrogazione = 58


                    Case Else
                        If insieme_strumenti(temp_id).new_version = False Then
                            Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                        End If

                        sequnza_interrogazione = 0
                        maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                End Select
                sequenza_timer = False
            End If
            numero_interrogazione = 0
            Exit Sub
        End If
        If (InStr(message_temp, "WD") <> 0) And InStr(message_temp, "setstkr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value14 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                'If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint14 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint14 = False
                End If

                'End If

            End If
            If sequenza_timer And tipo_strumento_sequenza = "wd" Then
                If insieme_strumenti(temp_id).new_version = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                End If

                sequnza_interrogazione = 57
                sequenza_timer = False
            End If
            numero_interrogazione = 0
            Exit Sub
        End If
        If (InStr(message_temp, "WD") <> 0 Or InStr(message_temp, "WH") <> 0) And InStr(message_temp, "diginpr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value16 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                ' If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint16 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint16 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "wd" Or tipo_strumento_sequenza = "wh") Then

                If insieme_strumenti(temp_id).new_version = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                End If

                If insieme_strumenti(temp_id).new_version Then
                    sequnza_interrogazione = 58
                Else
                    sequnza_interrogazione = 0
                    maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                    sequenza_timer = False
                End If
            End If
            numero_interrogazione = 0
            Exit Sub
        End If

        If (InStr(message_temp, "LD") <> 0 Or InStr(message_temp, "LTB") <> 0) And InStr(message_temp, "maoutsr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value14 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                'If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint14 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint14 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds") Then
                sequnza_interrogazione = 55
                sequenza_timer = False
            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ltb") Then
                sequnza_interrogazione = 58
                sequenza_timer = False
            End If

            numero_interrogazione = 0
            Exit Sub
        End If

        If (InStr(message_temp, "LD") <> 0) And InStr(message_temp, "minmaxr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value16 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                'If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint16 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint16 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds") Then
                Try
                    If insieme_strumenti(temp_id).new_version = False Then
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ermokr")
                    End If

                    If insieme_strumenti(temp_id).new_version Then
                        sequnza_interrogazione = 58
                    Else
                        sequnza_interrogazione = 0
                        maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                        sequenza_timer = False
                    End If
                Catch ex As Exception

                End Try
            End If
            numero_interrogazione = 0
            Exit Sub
        End If

        If ((InStr(message_temp, "LD") <> 0) Or (InStr(message_temp, "WD") <> 0) Or InStr(message_temp, "WH") <> 0 Or InStr(message_temp, "LTB") <> 0 _
            Or InStr(message_temp, "LTA") <> 0 Or InStr(message_temp, "LTD") <> 0 Or InStr(message_temp, "LTU") <> 0 _
            Or InStr(message_temp, "LDMA") <> 0 Or InStr(message_temp, "LDLG") <> 0) And InStr(message_temp, "servicr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value17 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                'If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint17 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint17 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or _
                                   tipo_strumento_sequenza = "wd" Or tipo_strumento_sequenza = "wh" _
                                   Or tipo_strumento_sequenza = "ltb" Or tipo_strumento_sequenza = "ltaud" Or tipo_strumento_sequenza = "ldma" Or tipo_strumento_sequenza = "ldlg") Then
                sequnza_interrogazione = 59
                sequenza_timer = False
            End If
            numero_interrogazione = 0
            Exit Sub
        End If

        If ((InStr(message_temp, "LD") <> 0) Or (InStr(message_temp, "WD") <> 0) Or InStr(message_temp, "WH") <> 0 _
            Or InStr(message_temp, "LTA") <> 0 Or InStr(message_temp, "LTD") <> 0 Or InStr(message_temp, "LTU") <> 0 _
            Or InStr(message_temp, "LTB") <> 0) And InStr(message_temp, "labelr") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value18 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                'If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint18 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint18 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" _
                                   Or tipo_strumento_sequenza = "wd" Or tipo_strumento_sequenza = "wh" _
                                   Or tipo_strumento_sequenza = "ltb" Or tipo_strumento_sequenza = "ltaud") Then
                sequnza_interrogazione = 60
                sequenza_timer = False
            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ldma" Or tipo_strumento_sequenza = "ldlg") Then
                sequnza_interrogazione = 0
                maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                sequenza_timer = False
            End If

            numero_interrogazione = 0
            Exit Sub
        End If

        'If (InStr(message_temp, "LD") <> 0) And InStr(message_temp, "smsmar") <> 0 And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
        If ((InStr(message_temp, "LD") <> 0) Or (InStr(message_temp, "WD") <> 0) Or InStr(message_temp, "WH") <> 0 _
            Or InStr(message_temp, "LTA") <> 0 Or InStr(message_temp, "LTD") <> 0 Or InStr(message_temp, "LTU") <> 0 _
            Or InStr(message_temp, "LTB") <> 0) And (InStr(message_temp, "maismwend") <> 0 Or InStr(message_temp, "smsmar") <> 0) And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value19 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                'If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint19 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint19 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" _
                                   Or tipo_strumento_sequenza = "wd" Or tipo_strumento_sequenza = "wh" _
                                   Or tipo_strumento_sequenza = "ltb" Or tipo_strumento_sequenza = "ltaud") Then
                Select Case insieme_strumenti(temp_id).tipo_strumento
                    Case "LD4"
                        sequnza_interrogazione = 61
                        sequenza_timer = False
                    Case "WH"
                        sequnza_interrogazione = 65
                        sequenza_timer = False
                    Case "LTA"
                        sequnza_interrogazione = 68
                        sequenza_timer = False

                    Case "LTD"
                        sequnza_interrogazione = 68
                        sequenza_timer = False

                    Case "LTU"
                        sequnza_interrogazione = 68
                        sequenza_timer = False

                    Case "LTB"
                        sequnza_interrogazione = 68
                        sequenza_timer = False

                    Case "LDS"
                        sequnza_interrogazione = 63
                        sequenza_timer = False

                    Case Else
                        sequnza_interrogazione = 0
                        maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                        sequenza_timer = False
                End Select
            End If
            numero_interrogazione = 0
            Exit Sub
        End If
        If ((InStr(message_temp, "LD") <> 0)) And (InStr(message_temp, "wmeterr") <> 0) And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value20 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                'If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint20 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint20 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd") Then
                sequnza_interrogazione = 62
                sequenza_timer = False
            End If
            numero_interrogazione = 0
            Exit Sub
        End If
        If ((InStr(message_temp, "LD") <> 0)) And (InStr(message_temp, "flowalr") <> 0) And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value21 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                'If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint21 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint21 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd") Then
                sequnza_interrogazione = 0
                maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                sequenza_timer = False
            End If
            numero_interrogazione = 0
            Exit Sub
        End If
        '-----------------------------------------------LTB
        If ((InStr(message_temp, "LTB") <> 0) _
            Or InStr(message_temp, "LTA") <> 0 Or InStr(message_temp, "LTD") <> 0 Or InStr(message_temp, "LTU") <> 0
            ) And (InStr(message_temp, "wtmetw") <> 0) And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value20 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                ' If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint20 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint20 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ltb" Or tipo_strumento_sequenza = "ltaud") Then
                Select Case tipo_strumento_sequenza
                    Case "ltb"
                        Dim str_version As String = insieme_strumenti(temp_id).get_str_version(insieme_strumenti(temp_id).nome_strumento)
                        If Val(str_version) >= 117 Then
                            sequnza_interrogazione = 69
                            sequenza_timer = False
                        Else
                            sequnza_interrogazione = 0
                            maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                            sequenza_timer = False
                        End If
                    Case "ltaud"
                        sequnza_interrogazione = 70
                        sequenza_timer = False
                End Select
            End If
            numero_interrogazione = 0
            Exit Sub
        End If



        If ((InStr(message_temp, "LTB") <> 0)) And (InStr(message_temp, "nciclirend") <> 0) And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value21 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                If InStr(message_temp, "014310") <> 0 Then
                    Try
                        Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\014310.txt", True)
                            writer.WriteLine("#" + Now.ToString + message_temp + "#" + Format(temp_id, "00"))
                        End Using
                    Catch ex As Exception

                    End Try
                End If
                '  If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint21 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint21 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ltb") Then
                sequnza_interrogazione = 0
                maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                sequenza_timer = False
            End If
            numero_interrogazione = 0
            Exit Sub
        End If
        If ((InStr(message_temp, "LTA") <> 0) Or (InStr(message_temp, "LTU") <> 0) Or (InStr(message_temp, "LTD") <> 0)) And (InStr(message_temp, "allenarend") <> 0) And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value21 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                ' If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint21 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint21 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ltaud") Then
                Dim str_version As String = insieme_strumenti(temp_id).get_str_version(insieme_strumenti(temp_id).nome_strumento)
                If Val(str_version) >= 218 Then
                    sequnza_interrogazione = 69
                    sequenza_timer = False
                Else
                    sequnza_interrogazione = 0
                    maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                    sequenza_timer = False
                End If
            End If
            numero_interrogazione = 0
            Exit Sub
        End If
        If (InStr(message_temp, "LTA") <> 0 Or InStr(message_temp, "LTD") <> 0 Or InStr(message_temp, "LTU") <> 0) And (InStr(message_temp, "nciclirend") <> 0) And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value22 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                If InStr(message_temp, "014310") <> 0 Then
                    Try
                        Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\014310.txt", True)
                            writer.WriteLine("#" + Now.ToString + message_temp + "#" + Format(temp_id, "00"))
                        End Using
                    Catch ex As Exception

                    End Try
                End If
                '  If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint22 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint22 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ltaud") Then
                sequnza_interrogazione = 0
                maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                sequenza_timer = False
            End If
            numero_interrogazione = 0
            Exit Sub
        End If

        '-----------------------------------------------WH
        If ((InStr(message_temp, "WH") <> 0)) And (InStr(message_temp, "autsetr") <> 0) And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value20 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                'If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint20 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint20 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "wh") Then
                sequnza_interrogazione = 66
                sequenza_timer = False
            End If
            numero_interrogazione = 0
            Exit Sub
        End If
        If ((InStr(message_temp, "WH") <> 0)) And (InStr(message_temp, "allertr") <> 0) And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value21 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                ' If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint21 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint21 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "wh") Then
                sequnza_interrogazione = 67
                sequenza_timer = False
            End If
            numero_interrogazione = 0
            Exit Sub
        End If
        If ((InStr(message_temp, "WH") <> 0)) And (InStr(message_temp, "timerpr") <> 0) And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value22 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                'If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint22 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint22 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "wh") Then
                sequnza_interrogazione = 0
                maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                sequenza_timer = False
            End If
            numero_interrogazione = 0
            Exit Sub
        End If

        '------------------lds nuova versione
        If ((InStr(message_temp, "LD") <> 0)) And (InStr(message_temp, "selfcr") <> 0) And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value20 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                'If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint20 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint20 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd") Then
                sequnza_interrogazione = 64
                sequenza_timer = False
            End If
            numero_interrogazione = 0
            Exit Sub
        End If
        If ((InStr(message_temp, "LD") <> 0)) And (InStr(message_temp, "circor") <> 0) And InStr(message_temp, Format(temp_id, "00") + "&") <> 0 Then
            If InStr(message_temp, "BUSY") = 0 Then
                insieme_strumenti(temp_id).value21 = message_temp
                insieme_strumenti(temp_id).time_connessione = Now
                'If insieme_strumenti(temp_id).mvalue_update Then
                If insieme_strumenti(temp_id).update_setpoint21 = True Then
                    aggiorna_db_strumenti(insieme_strumenti(temp_id), Format(temp_id, "00"), identificativo)
                    insieme_strumenti(temp_id).update_setpoint21 = False
                End If

                'End If

            End If
            If sequenza_timer And (tipo_strumento_sequenza = "ld" Or tipo_strumento_sequenza = "lds" Or tipo_strumento_sequenza = "wd") Then
                sequnza_interrogazione = 0
                maxid_485 = maxid_485 + 1 ' passo a controllare id successivo
                sequenza_timer = False
            End If
            numero_interrogazione = 0
            Exit Sub
        End If



        Try
            If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                puntatore_pc_scrittura_setpoint.scrittura_sp = False
                puntatore_pc_scrittura_setpoint.puntatore_pc.SendMessage(message_temp)
                Exit Sub
            End If
        Catch ex As Exception
            Exit Sub
        End Try
        ' End If

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
    Public Sub messageStumentoReceived(ByVal sender As ConnectedClient, ByVal message As String)
        'If nuovo_strumento_connesso Then
        Dim i As Integer
        Dim query_result As Boolean
        Dim messaggio_errore As String = ""
        Dim temp_id As Integer = -1
        Dim message_received As String = message
        If (InStr(identificativo, "543415724") <> 0) Then ' modifica yagel id=0
            Dim split_value() As String = message_received.Split("|")
            Try
                If (Mid(split_value(2), 1, 2)) = "00" Then
                    message_received = split_value(0) + "|" + split_value(1) + "|" + "01" + Mid(split_value(2), 3, Len(split_value(2))) + "|end"
                End If
            Catch ex As Exception

            End Try

        End If

        If InStr(message_received, "end") <> 0 Then
            stack_messaggi.Enqueue(message_received)
        End If

        libera_coda()


    End Sub
    Public Function risposta_pc(ByVal sender As ConnectedClient, ByVal message As String, ByVal current_pc As pc) As Boolean
        Dim data() As String = message.Split("|")
        Dim id_cliente As Integer
        If data.Length > 3 Then
            id_cliente = Val(Mid(data(2), 1, 2))
            If id_cliente < 1 Then
                Return False
            End If
            If InStr(data(2), "emzc") <> 0 Then
                Try
                    sender.SendMessage(insieme_strumenti(id_cliente).nome_strumento)
                Catch ex As Exception
                    sender.SendMessage("null")
                End Try

                Return True
            End If
            If InStr(data(2), "bc") <> 0 Then
                Try
                    If insieme_strumenti(id_cliente).value1 = "" Then
                        Me.mpuntatore_strumento.SendMessage(data(2))
                    Else
                        sender.SendMessage(insieme_strumenti(id_cliente).value1)
                    End If
                Catch ex As Exception

                End Try
                Return True
            End If
            If InStr(data(2), "ab") <> 0 Then
                Try
                    If insieme_strumenti(id_cliente).value2_1 = "" Then
                        Me.mpuntatore_strumento.SendMessage(data(2))
                    Else
                        sender.SendMessage(insieme_strumenti(id_cliente).value2_1)
                        'insieme_strumenti(id_cliente).value2_1 = ""
                    End If
                Catch ex As Exception

                End Try
                Return True
            End If
            If InStr(data(2), "ba") <> 0 Then
                Try
                    If insieme_strumenti(id_cliente).value3 = "" Then
                        Me.mpuntatore_strumento.SendMessage(data(2))
                    Else
                        sender.SendMessage(insieme_strumenti(id_cliente).value3)
                    End If
                Catch ex As Exception

                End Try
                Return True
            End If
            If InStr(data(2), "b5") <> 0 Then
                Try
                    If insieme_strumenti(id_cliente).value4 = "" Then
                        Me.mpuntatore_strumento.SendMessage(data(2))
                    Else
                        sender.SendMessage(insieme_strumenti(id_cliente).value4)
                    End If
                Catch ex As Exception

                End Try
                Return True
            End If
            '-------------------------------------------------------------------
            'lettura valore setpoint max 5
            If InStr(data(2), "1A") <> 0 Then
                Try
                    If insieme_strumenti(id_cliente).value5 = "" Then
                        Me.mpuntatore_strumento.SendMessage(data(2))
                        'puntatore_pc_scrittura_setpoint = puntatore_pc1
                    Else
                        sender.SendMessage(insieme_strumenti(id_cliente).value5)
                    End If
                Catch ex As Exception

                End Try
                Return True
            End If
            If InStr(data(2), "2A") <> 0 Then
                Try
                    If insieme_strumenti(id_cliente).value6 = "" Then
                        Me.mpuntatore_strumento.SendMessage(data(2))
                        ' puntatore_pc_scrittura_setpoint = puntatore_pc1
                    Else
                        sender.SendMessage(insieme_strumenti(id_cliente).value6)
                    End If
                Catch ex As Exception

                End Try
                Return True
            End If
            If InStr(data(2), "3A") <> 0 Then
                Try
                    If insieme_strumenti(id_cliente).value7 = "" Then
                        Me.mpuntatore_strumento.SendMessage(data(2))
                        'puntatore_pc_scrittura_setpoint = puntatore_pc1
                    Else
                        sender.SendMessage(insieme_strumenti(id_cliente).value7)
                    End If
                Catch ex As Exception

                End Try
                Return True
            End If
            If InStr(data(2), "4A") <> 0 Then
                Try
                    If insieme_strumenti(id_cliente).value8 = "" Then
                        Me.mpuntatore_strumento.SendMessage(data(2))
                        ' puntatore_pc_scrittura_setpoint = puntatore_pc1
                    Else
                        sender.SendMessage(insieme_strumenti(id_cliente).value8)
                    End If
                Catch ex As Exception

                End Try
                Return True
            End If
            If InStr(data(2), "5A") <> 0 Then
                Try
                    If insieme_strumenti(id_cliente).value9 = "" Then
                        Me.mpuntatore_strumento.SendMessage(data(2))
                        ' puntatore_pc_scrittura_setpoint = puntatore_pc1
                    Else
                        sender.SendMessage(insieme_strumenti(id_cliente).value9)
                    End If
                Catch ex As Exception

                End Try
                Return True
            End If

            '-------------------------------------------------------------------
            'scrittura nuovi set point 
            'If InStr(data(2), "6A") <> 0 And InStr(data(2), "end") <> 0 Then
            '    buffer_sp1 = data(2)
            '    Me.mpuntatore_strumento.SendMessage(data(2))
            '    puntatore_pc_scrittura_setpoint = puntatore_pc1
            '    'sender.SendMessage(insieme_strumenti(id_cliente).value5)
            'End If
            'If InStr(data(2), "7A") <> 0 And InStr(data(2), "end") <> 0 Then
            '    buffer_sp1 = data(2)
            '    Me.mpuntatore_strumento.SendMessage(data(2))
            '    puntatore_pc_scrittura_setpoint = puntatore_pc1
            'End If
            'If InStr(data(2), "8A") <> 0 And InStr(data(2), "end") <> 0 Then
            '    buffer_sp1 = data(2)
            '    Me.mpuntatore_strumento.SendMessage(data(2))
            '    puntatore_pc_scrittura_setpoint = puntatore_pc1
            'End If
            'If InStr(data(2), "9A") <> 0 And InStr(data(2), "end") <> 0 Then
            '    buffer_sp1 = data(2)
            '    Me.mpuntatore_strumento.SendMessage(data(2))
            '    puntatore_pc_scrittura_setpoint = puntatore_pc1
            'End If
            'If InStr(data(2), "AA") <> 0 And InStr(data(2), "end") <> 0 Then
            '    buffer_sp1 = data(2)
            '    Me.mpuntatore_strumento.SendMessage(data(2))
            '    puntatore_pc_scrittura_setpoint = puntatore_pc1
            'End If

            ''-------------------------------------------------------------------
            ''-------------------------------------------------------------------
            ''scrittura nuovi valori option per timer 1 timer 2 timer 3 timer 4 timer 5
            'If InStr(data(2), "b4") <> 0 And InStr(data(2), "end") <> 0 Then
            '    ' timer 2 e timer 1
            '    buffer_sp1 = data(2)
            '    Me.mpuntatore_strumento.SendMessage(data(2))
            '    puntatore_pc_scrittura_setpoint = puntatore_pc1
            'End If
            'If InStr(data(2), "bk") <> 0 And InStr(data(2), "end") <> 0 Then
            '    ' timer 3 e timer 4 timer 5
            '    buffer_sp1 = data(2)
            '    Me.mpuntatore_strumento.SendMessage(data(2))
            '    puntatore_pc_scrittura_setpoint = puntatore_pc1
            'End If
            'If InStr(data(2), "b1") <> 0 And InStr(data(3), "end") <> 0 Then
            '    ' VALORE DELLE OPTION
            '    buffer_sp1 = data(2)
            '    Me.mpuntatore_strumento.SendMessage(data(2))
            '    puntatore_pc_scrittura_setpoint = puntatore_pc1
            'End If
            'If InStr(data(2), "b2") <> 0 And InStr(data(3), "end") <> 0 Then
            '    ' VALORE DELLE password
            '    buffer_sp1 = data(2)
            '    Me.mpuntatore_strumento.SendMessage(data(2))
            '    puntatore_pc_scrittura_setpoint = puntatore_pc1
            'End If
            'If InStr(data(2), "b8") <> 0 And InStr(data(3), "end") <> 0 Then
            '    ' VALORE DEL flusso
            '    buffer_sp1 = data(2)
            '    Me.mpuntatore_strumento.SendMessage(data(2))
            '    puntatore_pc_scrittura_setpoint = puntatore_pc1
            'End If
            'If InStr(data(2), "b3") <> 0 And InStr(data(3), "end") <> 0 Then
            '    ' VALORE DEL clock
            '    buffer_sp1 = data(2)
            '    Me.mpuntatore_strumento.SendMessage(data(2))
            '    puntatore_pc_scrittura_setpoint = puntatore_pc1
            'End If

            '-------------------------------------------------------------------
            If insieme_strumenti(id_cliente).tipo_strumento = "Tower" Then

                If InStr(data(2), "unitsr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value1 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value1)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value1)
                    Return True
                End If
                If InStr(data(2), "valuer") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value2 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value2)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value2)
                    Return True
                End If
                If InStr(data(2), "algenr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value3 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value3)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value3)
                    Return True
                End If
                If InStr(data(2), "config") <> 0 Then
                    If insieme_strumenti(id_cliente).value4 = "" Then
                        Me.mpuntatore_strumento.SendMessage(data(2))
                        'puntatore_pc_scrittura_setpoint = puntatore_pc1
                    Else
                        sender.SendMessage(insieme_strumenti(id_cliente).value4)
                    End If

                    'sender.SendMessage(insieme_strumenti(id_cliente).value4)
                    Return True
                End If
                If InStr(data(2), "clockr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value5 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value5)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value5)
                    Return True
                End If
                If InStr(data(2), "stbior") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value6 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value6)
                        End If
                    Catch ex As Exception

                    End Try

                    'sender.SendMessage(insieme_strumenti(id_cliente).value6)
                    Return True
                End If
                If InStr(data(2), "optior") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value7 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value7)
                        End If
                    Catch ex As Exception

                    End Try

                    'sender.SendMessage(insieme_strumenti(id_cliente).value7)
                    Return True
                End If
                If InStr(data(2), "tota1r") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value8 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value8)
                        End If
                    Catch ex As Exception

                    End Try

                    'sender.SendMessage(insieme_strumenti(id_cliente).value8)
                    Return True
                End If
                If InStr(data(2), "tota2r") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value9 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value9)
                        End If
                    Catch ex As Exception

                    End Try

                    'sender.SendMessage(insieme_strumenti(id_cliente).value9)
                    Return True
                End If
                If InStr(data(2), "stoutr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value10 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value10)
                        End If
                    Catch ex As Exception

                    End Try
                    '                    sender.SendMessage(insieme_strumenti(id_cliente).value10)
                    Return True
                End If
                If InStr(data(2), "passcr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value11 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value11)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "logser") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value12 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value12)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "bleedr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value13 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value13)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "inhibr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value14 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value14)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "bioc1r") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value15 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value15)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "bi1w1r") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value16 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value16)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "bi1w2r") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value17 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value17)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "bi1w3r") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value18 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value18)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "bi1w4r") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value19 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value19)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "bioc2r") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value20 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value20)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "bi2w1r") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value21 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value21)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "bi2w2r") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value22 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value22)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "bi2w3r") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value23 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value23)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "bi2w4r") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value24 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value24)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "flowmr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value25 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value25)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "alarmr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value26 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value26)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "setp2r") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value27 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value27)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "setp3r") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value28 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value28)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If

            End If
            If insieme_strumenti(id_cliente).tipo_strumento = "LD" Or insieme_strumenti(id_cliente).tipo_strumento = "LD4" Or insieme_strumenti(id_cliente).tipo_strumento = "LDS" Or insieme_strumenti(id_cliente).tipo_strumento = "WD" Then
                If InStr(data(2), "config") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value1 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value1)
                        End If
                    Catch ex As Exception

                    End Try
                    '                    sender.SendMessage(insieme_strumenti(id_cliente).value1)
                    Return True
                End If
                If InStr(data(2), "valuer") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value2_1 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value2_1)
                            'insieme_strumenti(id_cliente).value2_1 = ""
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value2)
                    Return True
                End If
                If InStr(data(2), "allrmr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value3 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value3)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value3)
                    Return True
                End If
                If InStr(data(2), "calibzr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value4 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value4)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value4)
                    Return True
                End If
                If InStr(data(2), "outptr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value5 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value5)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value5)
                    Return True
                End If
                If InStr(data(2), "clockr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value6_1 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value6_1)
                            ' insieme_strumenti(id_cliente).value6_1 = ""
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value6)
                    Return True
                End If
                If InStr(data(2), "setpntr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value7 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value7)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value7)
                    Return True
                End If
                If InStr(data(2), "paramtr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value8 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value8)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value8)
                    Return True
                End If
                If InStr(data(2), "alldosr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value9 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value9)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value9)
                    Return True
                End If
                If InStr(data(2), "allprbr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value10 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value10)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value10)
                    Return True
                End If
                If InStr(data(2), "flowstr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value11 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value11)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value11)
                    Return True
                End If
                If InStr(data(2), "compphr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value12 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value12)
                        End If
                    Catch ex As Exception

                    End Try

                    'sender.SendMessage(insieme_strumenti(id_cliente).value12)
                    Return True
                End If
                If InStr(data(2), "clocksr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value13 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value13)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value13)
                    Return True
                End If
                If InStr(data(2), "maoutsr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value14 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value14)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value14)
                    Return True
                End If
                If InStr(data(2), "logsetr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value15 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value15)
                        End If
                    Catch ex As Exception

                    End Try
                    'sender.SendMessage(insieme_strumenti(id_cliente).value15)
                    Return True
                End If
                If InStr(data(2), "minmaxr") <> 0 Then
                    Try
                        If insieme_strumenti(id_cliente).value16 = "" Then
                            Me.mpuntatore_strumento.SendMessage(data(2))
                            'puntatore_pc_scrittura_setpoint = puntatore_pc1
                        Else
                            sender.SendMessage(insieme_strumenti(id_cliente).value16)
                        End If
                    Catch ex As Exception

                    End Try

                    'sender.SendMessage(insieme_strumenti(id_cliente).value16)
                    Return True
                End If
                If InStr(data(2), "okermr") <> 0 Then
                    If puntatore_pc_scrittura_setpoint.scrittura_sp Then
                        sender.SendMessage("34gpd&" + Format(id_cliente, "00") + "LD#1#okermrend")
                    Else
                        sender.SendMessage("34gpd&" + Format(id_cliente, "00") + "LD#0#okermrend")
                    End If
                    Return True

                End If
            End If
            'scrittura di un setpoint
            Try
                Me.mpuntatore_strumento.SendMessage(data(2))
                puntatore_pc_scrittura_setpoint = current_pc
                puntatore_pc_scrittura_setpoint.scrittura_sp = True
                Return True

            Catch ex As Exception
                Return False
            End Try
        End If
        Return False
    End Function
    Public Sub messagepc1Received(ByVal sender As ConnectedClient, ByVal message As String)
        Dim risultato_invio As Boolean = True

        puntatore_pc1.reset_contatore()
        risultato_invio = risposta_pc(sender, message, puntatore_pc1)

        If InStr(message, "/DISCONNECTPC") Or risultato_invio = False Then
            puntatore_pc1.puntatore_pc.close_connection()
            puntatore_pc1.puntatore_pc = Nothing
            puntatore_pc1.connected = False
            Timer4.Enabled = True
            Timer4.Start()
        End If
        Dim test As String = message
    End Sub
    Public Sub messagepc2Received(ByVal sender As ConnectedClient, ByVal message As String)
        Dim risultato_invio As Boolean = True
        Try
            Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log.txt", True)
                writer.WriteLine("#" + Now.ToString + message + "#")
            End Using
        Catch ex As Exception

        End Try
        puntatore_pc2.reset_contatore()
        risultato_invio = risposta_pc(sender, message, puntatore_pc2)

        If InStr(message, "/DISCONNECTPC") Or risultato_invio = False Then
            puntatore_pc2.puntatore_pc.close_connection()
            puntatore_pc2.puntatore_pc = Nothing
            puntatore_pc2.connected = False
        End If

    End Sub
    Public Sub messagepc3Received(ByVal sender As ConnectedClient, ByVal message As String)
        Dim risultato_invio As Boolean = True
        puntatore_pc3.reset_contatore()
        risultato_invio = risposta_pc(sender, message, puntatore_pc3)

        If InStr(message, "/DISCONNECTPC") Or risultato_invio = False Then
            puntatore_pc3.puntatore_pc.close_connection()
            puntatore_pc3.puntatore_pc = Nothing
            puntatore_pc3.connected = False
        End If

    End Sub
    Public Sub messagepc4Received(ByVal sender As ConnectedClient, ByVal message As String)
        Dim risultato_invio As Boolean = True
        puntatore_pc4.reset_contatore()
        risultato_invio = risposta_pc(sender, message, puntatore_pc4)

        If InStr(message, "/DISCONNECTPC") Or risultato_invio = False Then
            puntatore_pc4.puntatore_pc.close_connection()
            puntatore_pc4.puntatore_pc = Nothing
            puntatore_pc4.connected = False
        End If
    End Sub

    Public Function set_pc_puntatore(ByVal client_pc As ConnectedClient) As String
        'gestione della multiutenza pc 4 connessioni contemporanee quindi 4 pc connessi
        If puntatore_pc1.connected = False Then
            Timer4.Enabled = False
            Timer4.Stop()
            Timer5.Enabled = False
            Timer5.Stop()
            puntatore_pc1.puntatore_pc = client_pc
            puntatore_pc1.connected = True
            puntatore_pc1.puntatore_pc.assegnato_pc = False
            AddHandler client_pc.dataReceived, AddressOf Me.messagepc1Received
            Return "/Connected"
        End If
        If puntatore_pc2.connected = False Then
            puntatore_pc2.puntatore_pc = client_pc
            puntatore_pc2.connected = True
            puntatore_pc2.puntatore_pc.assegnato_pc = False
            AddHandler client_pc.dataReceived, AddressOf Me.messagepc2Received
            Return "/Connected"
        End If
        If puntatore_pc3.connected = False Then
            puntatore_pc3.puntatore_pc = client_pc
            puntatore_pc3.connected = True
            puntatore_pc3.puntatore_pc.assegnato_pc = False
            AddHandler client_pc.dataReceived, AddressOf Me.messagepc3Received
            Return "/Connected"
        End If
        If puntatore_pc4.connected = False Then
            puntatore_pc4.puntatore_pc = client_pc
            puntatore_pc4.connected = True
            puntatore_pc4.puntatore_pc.assegnato_pc = False
            AddHandler client_pc.dataReceived, AddressOf Me.messagepc4Received
            Return "/Connected"
        End If
        Return "/UserLimits"
    End Function

    'Public Function update_database(ByVal id_485_temp As Integer, ByVal numero_query As Integer) As Boolean
    '    Dim righe_aggiornate1 As Integer = 0
    '    Dim righe_aggiornate2 As Integer = 0
    '    Try


    '        '    Select Case numero_query
    '        '        Case 0
    '        '            If insieme_strumenti(id_485_temp).mtipo_strumento_bool Then
    '        '                query.aggiorna_strumento_nome(insieme_strumenti(id_485_temp).nome_strumento, insieme_strumenti(id_485_temp).tipo_strumento, midentificativo, id_485_temp)
    '        '            End If
    '        '        Case 1
    '        '            If insieme_strumenti(id_485_temp).mvalue1_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value1(insieme_strumenti(id_485_temp).value1, midentificativo, id_485_temp)
    '        '            End If
    '        '        Case 2
    '        '            If insieme_strumenti(id_485_temp).mvalue2_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value2(insieme_strumenti(id_485_temp).value2, midentificativo, id_485_temp, insieme_strumenti(id_485_temp).time_no_flow)

    '        '            End If
    '        '        Case 3
    '        '            If insieme_strumenti(id_485_temp).mvalue3_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value3(insieme_strumenti(id_485_temp).value3, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 4
    '        '            If insieme_strumenti(id_485_temp).mvalue4_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value4(insieme_strumenti(id_485_temp).value4, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 5
    '        '            If insieme_strumenti(id_485_temp).mvalue5_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value5(insieme_strumenti(id_485_temp).value5, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 6
    '        '            If insieme_strumenti(id_485_temp).mvalue6_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value6(insieme_strumenti(id_485_temp).value6, midentificativo, id_485_temp)
    '        '            End If
    '        '        Case 7
    '        '            If insieme_strumenti(id_485_temp).mvalue7_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value7(insieme_strumenti(id_485_temp).value7, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 8
    '        '            If insieme_strumenti(id_485_temp).mvalue8_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value8(insieme_strumenti(id_485_temp).value8, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 9
    '        '            If insieme_strumenti(id_485_temp).mvalue9_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value9(insieme_strumenti(id_485_temp).value9, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 10
    '        '            If insieme_strumenti(id_485_temp).mvalue10_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value10(insieme_strumenti(id_485_temp).value10, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 11
    '        '            If insieme_strumenti(id_485_temp).mvalue11_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value11(insieme_strumenti(id_485_temp).value11, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 12
    '        '            If insieme_strumenti(id_485_temp).mvalue12_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value12(insieme_strumenti(id_485_temp).value12, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 13
    '        '            If insieme_strumenti(id_485_temp).mvalue13_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value13(insieme_strumenti(id_485_temp).value13, midentificativo, id_485_temp)
    '        '            End If
    '        '        Case 14
    '        '            If insieme_strumenti(id_485_temp).mvalue14_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value14(insieme_strumenti(id_485_temp).value14, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 15
    '        '            If insieme_strumenti(id_485_temp).mvalue15_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value15(insieme_strumenti(id_485_temp).value15, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 16
    '        '            If insieme_strumenti(id_485_temp).mvalue16_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value16(insieme_strumenti(id_485_temp).value16, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 17
    '        '            If insieme_strumenti(id_485_temp).mvalue17_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value17(insieme_strumenti(id_485_temp).value17, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 18
    '        '            If insieme_strumenti(id_485_temp).mvalue18_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value18(insieme_strumenti(id_485_temp).value18, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 19
    '        '            If insieme_strumenti(id_485_temp).mvalue19_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value19(insieme_strumenti(id_485_temp).value19, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 20
    '        '            If insieme_strumenti(id_485_temp).mvalue20_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value20(insieme_strumenti(id_485_temp).value20, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 21
    '        '            If insieme_strumenti(id_485_temp).mvalue21_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value21(insieme_strumenti(id_485_temp).value21, midentificativo, id_485_temp)
    '        '            End If
    '        '        Case 22
    '        '            If insieme_strumenti(id_485_temp).mvalue22_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value22(insieme_strumenti(id_485_temp).value22, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 23
    '        '            If insieme_strumenti(id_485_temp).mvalue23_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value23(insieme_strumenti(id_485_temp).value23, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 24
    '        '            If insieme_strumenti(id_485_temp).mvalue24_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value24(insieme_strumenti(id_485_temp).value24, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 25
    '        '            If insieme_strumenti(id_485_temp).mvalue25_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value25(insieme_strumenti(id_485_temp).value25, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 26
    '        '            If insieme_strumenti(id_485_temp).mvalue26_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value26(insieme_strumenti(id_485_temp).value26, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 27
    '        '            If insieme_strumenti(id_485_temp).mvalue27_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value27(insieme_strumenti(id_485_temp).value27, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 28
    '        '            If insieme_strumenti(id_485_temp).mvalue28_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value28(insieme_strumenti(id_485_temp).value28, midentificativo, id_485_temp)

    '        '            End If
    '        '        Case 29
    '        '            If insieme_strumenti(id_485_temp).mvalue29_bool Then
    '        '                righe_aggiornate1 = query.aggiorna_strumento_value29(insieme_strumenti(id_485_temp).value29, midentificativo, id_485_temp)

    '        '            End If

    '        '    End Select


    '    Catch ex As Exception

    '    End Try

    '    'If righe_aggiornate1 = 0 Then
    '    '    Try
    '    '        Select Case numero_query
    '    '            Case 0
    '    '            Case 1
    '    '                insieme_strumenti(id_485_temp).mvalue1_bool = False
    '    '            Case 2
    '    '                insieme_strumenti(id_485_temp).mvalue2_bool = False
    '    '            Case 3
    '    '                insieme_strumenti(id_485_temp).mvalue3_bool = False
    '    '            Case 4
    '    '                insieme_strumenti(id_485_temp).mvalue4_bool = False
    '    '            Case 5
    '    '                insieme_strumenti(id_485_temp).mvalue5_bool = False
    '    '            Case 6
    '    ''                insieme_strumenti(id_485_temp).mvalue6_bool = False
    '    ''            Case 7
    '    ''                insieme_strumenti(id_485_temp).mvalue7_bool = False
    '    ''            Case 8
    '    ''                insieme_strumenti(id_485_temp).mvalue8_bool = False
    '    ''            Case 9
    '    ''                insieme_strumenti(id_485_temp).mvalue9_bool = False
    '    ''            Case 10
    '    ''                insieme_strumenti(id_485_temp).mvalue10_bool = False
    '    ''            Case 11
    '    ''                insieme_strumenti(id_485_temp).mvalue11_bool = False
    '    ''            Case 12
    '    ''                insieme_strumenti(id_485_temp).mvalue12_bool = False
    '    ''            Case 13
    '    ''                insieme_strumenti(id_485_temp).mvalue13_bool = False
    '    ''            Case 14
    '    ''                insieme_strumenti(id_485_temp).mvalue14_bool = False
    '    ''            Case 15
    '    ''                insieme_strumenti(id_485_temp).mvalue15_bool = False
    '    ''            Case 16
    '    ''                insieme_strumenti(id_485_temp).mvalue16_bool = False
    '    ''            Case 17
    '    ''                insieme_strumenti(id_485_temp).mvalue17_bool = False
    '    ''            Case 18
    '    ''                insieme_strumenti(id_485_temp).mvalue18_bool = False
    '    ''            Case 19
    '    ''                insieme_strumenti(id_485_temp).mvalue19_bool = False
    '    ''            Case 20
    '    ''                insieme_strumenti(id_485_temp).mvalue20_bool = False
    '    ''            Case 21
    '    ''                insieme_strumenti(id_485_temp).mvalue21_bool = False
    '    ''            Case 22
    '    ''                insieme_strumenti(id_485_temp).mvalue22_bool = False
    '    ''            Case 23
    '    ''                insieme_strumenti(id_485_temp).mvalue23_bool = False
    '    ''            Case 24
    '    ''                insieme_strumenti(id_485_temp).mvalue24_bool = False
    '    ''            Case 25
    '    ''                insieme_strumenti(id_485_temp).mvalue25_bool = False
    '    ''            Case 26
    '    ''                insieme_strumenti(id_485_temp).mvalue26_bool = False
    '    ''            Case 27
    '    ''                insieme_strumenti(id_485_temp).mvalue27_bool = False
    '    ''            Case 28
    '    ''                insieme_strumenti(id_485_temp).mvalue28_bool = False
    '    ''            Case 29
    '    ''                insieme_strumenti(id_485_temp).mvalue29_bool = False

    '    ''        End Select

    '    ''    Catch ex As Exception

    '    ''    End Try


    '    'Return False
    '    'Else
    '    'Return True
    '    'End If

    'End Function


    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Elapsed
        Timer1.Enabled = False
        Timer1.Stop()
        'Try
        '    Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log_id.txt", True)
        '        writer.WriteLine("#" + Now.ToString + sequnza_interrogazione.ToString + " " + identificativo + " timer1 send#")
        '    End Using
        'Catch ex As Exception

        'End Try
        Try
            If mpuntatore_strumento.remove Then
                mpuntatore_strumento.close_connection()
                mpuntatore_strumento = Nothing
            End If

        Catch ex As Exception
            Exit Sub
        End Try
        Try
            Timer1.Interval = 1500
            If nuovo_strumento_connesso Then ' se nuovo strumento connesso avvio una sequenza di interrogazione
                sequenza_timer = True
                Select Case sequnza_interrogazione
                    Case 0 ' richiesta informazione 01emzc
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "emzc82")
                        ' numero_interrogazione = numero_interrogazione + 1
                        'max5--------------------------------------------------------------------------
                    Case 1 ' richiesta informazione bc(valori di calibrazione e informazione canali)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bc")
                    Case 2 ' richiesta informazione ab(valori runtime e allarmi)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ab")
                    Case 3 ' richiesta informazione ba(di orologio flusso e temperatura timer)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "ba")
                    Case 4 ' richiesta informazione bc(valori di impostazione generale max 5 option)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "b5")
                    Case 5 ' richiesta informazione 1A(set point canale 1)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "1A")
                    Case 6 ' richiesta informazione 2A(set point canale 2)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "2A")
                    Case 7 ' richiesta informazione 3A(set point canale 3)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "3A")
                    Case 8 ' richiesta informazione 4A(set point canale 4)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "4A")
                    Case 9 ' richiesta informazione 5A(set point canale 5)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "5A")
                    Case 100 '1B
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "1B")
                    Case 101 '2B
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "2B")
                    Case 102 '3B
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "3B")
                    Case 103 '4B
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "4B")
                    Case 104 '5B
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "5B")
                    Case 105 '1C
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "1C")
                    Case 106 '1D
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "1D")
                    Case 107 'b6
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "b6")

                        'tower--------------------------------------------------------------------------
                    Case 10 ' richiesta informazione unitstr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "unitsr")
                    Case 11 ' richiesta informazione valuer(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "valuer")
                    Case 12 ' richiesta informazione algenr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "algenr")
                    Case 13 ' richiesta informazione config(config)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "config")
                    Case 14 ' richiesta informazione clockr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "clockr")
                    Case 15 ' richiesta informazione stbior(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "stbior")
                    Case 16 ' richiesta informazione optior(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "optior")
                    Case 17 ' richiesta informazione tota1r(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "tota1r")
                    Case 18 ' richiesta informazione tota2r(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "tota2r")
                    Case 19 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "stoutr")
                    Case 20 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "passcr")
                    Case 21 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "logser")
                    Case 22 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bleedr")
                    Case 23 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "inhibr")
                    Case 24 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bioc1r")
                    Case 25 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bi1w1r")
                    Case 26 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bi1w2r")
                    Case 27 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bi1w3r")
                    Case 28 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bi1w4r")

                    Case 29 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bioc2r")
                    Case 30 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bi2w1r")
                    Case 31 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bi2w2r")
                    Case 32 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bi2w3r")
                    Case 33 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bi2w4r")
                    Case 34 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "flowmr")
                    Case 35 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "alarmr")
                    Case 36 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "setp2r")
                    Case 37 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "setp3r")
                    Case 38 ' richiesta informazione stoutr(tower)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "emailr")

                    Case 39 ' richiesta informazione boiler
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "boiler")

                        'ld--------------------------------------------------------------------------
                        'lds--------------------------------------------------------------------------
                        'WD--------------------------------------------------------------------------
                    Case 40 ' richiesta informazione unitstr(ld)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "config")
                    Case 41 ' richiesta informazione valuer(ld)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "valuer")
                    Case 42 ' richiesta informazione algenr(ld)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "allrmr")
                    Case 43 ' richiesta informazione config(ld)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "calibzr")
                    Case 44 ' richiesta informazione outptr(ld)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "outptr")
                    Case 45 ' richiesta informazione clockr(ld)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "clockr")
                    Case 46 ' richiesta informazione clockr(config)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "setpntr")
                    Case 47 ' richiesta informazione clockr(config)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "paramtr")
                    Case 48 ' richiesta informazione clockr(config)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "alldosr")
                    Case 49 ' richiesta informazione clockr(config)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "allprbr")
                    Case 50 ' richiesta informazione clockr(config)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "flowstr")
                    Case 51 ' richiesta informazione clockr(config)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "compphr")
                    Case 52 ' richiesta informazione clockr(config)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "clocksr")
                    Case 53 ' richiesta informazione clockr(config)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "logsetr")
                    Case 54 ' richiesta informazione clockr(config)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "maoutsr")
                    Case 55 ' richiesta informazione clockr(config)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "minmaxr")
                    Case 56 ' richiesta informazione strokes(config)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "setstkr")
                    Case 57 ' richiesta informazione strokes(config)
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "diginpr")

                        'nuova versione
                    Case 58 ' service
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "servicr")
                    Case 59 ' service
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "labelr")
                    Case 60 ' service
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "smsmar")

                    Case 61 ' service
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "wmeterr")
                    Case 62 ' service
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "flowalr")
                    Case 63 ' service
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "selfcr")
                    Case 64 ' service
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "circor")

                        'wh
                    Case 65 '
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "autsetr")
                    Case 66 '
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "allertr")
                    Case 67 '
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "timerpr")
                        'ltb
                    Case 68 '
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "wtmeter")
                    Case 69 'ltb 117
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "nciclir")
                    Case 70 'ltaud
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "allenar")
                    '------------------------------------------------------------------------
                    '----LD TOWER----------------------------
                    Case 71
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bi1w5r")
                    Case 72
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bi1w6r")
                    Case 73
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bi1w7r")
                    Case 74
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bi2w5r")
                    Case 75
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bi2w6r")
                    Case 76
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "bi2w7r")
                    Case 77
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "calcdr")
                    Case 78
                        Me.mpuntatore_strumento.SendMessage(Format(maxid_485, "00") + "servir")

                End Select
                numero_interrogazione = numero_interrogazione + 1

                If numero_interrogazione > 3 Then
                    If sequnza_interrogazione = 0 Then
                        Try
                            Dim righe_aggiornate As Integer = 0
                            'Dim adapter1 As statusTableAdapters.statusTableAdapter = New statusTableAdapters.statusTableAdapter
                            'righe_aggiornate = adapter1.UpdateQuery(get_strumenti_valore, "Connected", Now, identificativo)
                            'If righe_aggiornate = 0 Then
                            ' adapter1.InsertQuery(Now, identificativo, get_strumenti_valore, "Connected", Now)
                            'End If
                        Catch ex As Exception

                        End Try
                        If maxid_485 > 1 Then
                            maxid_485 = maxid_485 - 1
                            ' numberid_485 = maxid_485
                        End If
                        sequenza_timer = False
                        nuovo_strumento_connesso = False
                        sequnza_interrogazione = 0
                        numero_interrogazione = 0
                        maxid_485 = 1
                        Timer1.Enabled = False
                        Timer1.Stop()
                        Timer2.Enabled = True
                        Timer_refresh.Enabled = True
                        If busy_strumento_invio Then
                        Else
                            Timer2.Start()
                        End If
                        Timer4.Enabled = True
                        Timer4.Start()
                        'Timer6.Start()
                        Exit Sub
                    Else
                        maxid_485 = maxid_485 + 1
                        sequnza_interrogazione = 0
                        numero_interrogazione = 0
                    End If
                End If
            End If
            Timer1.Enabled = True
            Timer1.Start()
        Catch ex As Exception

        End Try

    End Sub
    'Private Sub Timer6_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer6.Elapsed
    '    Try
    '        sequenza_timer = False
    '        Me.mpuntatore_strumento.SendMessage("emzc82")
    '    Catch ex As Exception

    '    End Try
    'End Sub
    Public Sub save_read_realtime(ByVal indice_485_local As Integer)
        Select Case insieme_strumenti(indice_485_local).tipo_strumento
            Case "max5"
                insieme_strumenti(indice_485_local).update_setpoint2 = True
                insieme_strumenti(indice_485_local).update_setpoint3 = True
                insieme_strumenti(indice_485_local).update_setpoint4 = True
            Case "LD"
                insieme_strumenti(indice_485_local).update_setpoint2 = True
                insieme_strumenti(indice_485_local).update_setpoint3 = True
                insieme_strumenti(indice_485_local).update_setpoint5 = True
                insieme_strumenti(indice_485_local).update_setpoint6 = True
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).update_setpoint17 = True
                End If
            Case "LD4"
                insieme_strumenti(indice_485_local).update_setpoint2 = True
                insieme_strumenti(indice_485_local).update_setpoint3 = True
                insieme_strumenti(indice_485_local).update_setpoint5 = True
                insieme_strumenti(indice_485_local).update_setpoint6 = True
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).update_setpoint17 = True
                End If

            Case "LDS"
                insieme_strumenti(indice_485_local).update_setpoint2 = True
                insieme_strumenti(indice_485_local).update_setpoint3 = True
                insieme_strumenti(indice_485_local).update_setpoint5 = True
                insieme_strumenti(indice_485_local).update_setpoint6 = True
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).update_setpoint17 = True
                End If

            Case "WD"
                insieme_strumenti(indice_485_local).update_setpoint2 = True
                insieme_strumenti(indice_485_local).update_setpoint3 = True
                insieme_strumenti(indice_485_local).update_setpoint5 = True
                insieme_strumenti(indice_485_local).update_setpoint6 = True
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).update_setpoint17 = True
                End If
            Case "WH"
                insieme_strumenti(indice_485_local).update_setpoint2 = True
                insieme_strumenti(indice_485_local).update_setpoint3 = True
                insieme_strumenti(indice_485_local).update_setpoint5 = True
                insieme_strumenti(indice_485_local).update_setpoint16 = True
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).update_setpoint17 = True
                End If
            Case "LTA"
                insieme_strumenti(indice_485_local).update_setpoint2 = True
                insieme_strumenti(indice_485_local).update_setpoint3 = True
                insieme_strumenti(indice_485_local).update_setpoint6 = True
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).update_setpoint17 = True
                End If
            Case "LTD"
                insieme_strumenti(indice_485_local).update_setpoint2 = True
                insieme_strumenti(indice_485_local).update_setpoint3 = True
                insieme_strumenti(indice_485_local).update_setpoint6 = True
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).update_setpoint17 = True
                End If

            Case "LTU"
                insieme_strumenti(indice_485_local).update_setpoint2 = True
                insieme_strumenti(indice_485_local).update_setpoint3 = True
                insieme_strumenti(indice_485_local).update_setpoint6 = True
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).update_setpoint17 = True
                End If

            Case "LTB"
                insieme_strumenti(indice_485_local).update_setpoint2 = True
                insieme_strumenti(indice_485_local).update_setpoint3 = True
                insieme_strumenti(indice_485_local).update_setpoint5 = True
                insieme_strumenti(indice_485_local).update_setpoint6 = True
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).update_setpoint17 = True
                End If


            Case "Tower"
                insieme_strumenti(indice_485_local).update_setpoint2 = True
                insieme_strumenti(indice_485_local).update_setpoint3 = True
                insieme_strumenti(indice_485_local).update_setpoint5 = True
                insieme_strumenti(indice_485_local).update_setpoint6 = True
                insieme_strumenti(indice_485_local).update_setpoint8 = True
                insieme_strumenti(indice_485_local).update_setpoint9 = True
                insieme_strumenti(indice_485_local).update_setpoint10 = True
            Case "LDtower"
                insieme_strumenti(indice_485_local).update_setpoint2 = True
                insieme_strumenti(indice_485_local).update_setpoint3 = True
                insieme_strumenti(indice_485_local).update_setpoint5 = True
                insieme_strumenti(indice_485_local).update_setpoint6 = True
                insieme_strumenti(indice_485_local).update_setpoint8 = True
                insieme_strumenti(indice_485_local).update_setpoint9 = True
                insieme_strumenti(indice_485_local).update_setpoint10 = True


            Case "LDMA"
                insieme_strumenti(indice_485_local).update_setpoint2 = True
                insieme_strumenti(indice_485_local).update_setpoint3 = True
                insieme_strumenti(indice_485_local).update_setpoint13 = True
                insieme_strumenti(indice_485_local).update_setpoint17 = True
            Case "LDLG"
                insieme_strumenti(indice_485_local).update_setpoint2 = True
                insieme_strumenti(indice_485_local).update_setpoint3 = True
                insieme_strumenti(indice_485_local).update_setpoint13 = True
                insieme_strumenti(indice_485_local).update_setpoint17 = True

        End Select
    End Sub
    Public Sub read_realtime(ByVal indice_485_local As Integer, ByVal log_ldma As Boolean)
        Select Case insieme_strumenti(indice_485_local).tipo_strumento
            Case "max5"
                insieme_strumenti(indice_485_local).mvalue2_bool = False
                insieme_strumenti(indice_485_local).mvalue3_bool = False
                insieme_strumenti(indice_485_local).mvalue4_bool = False

                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).mvalue15_bool = False
                    insieme_strumenti(indice_485_local).mvalue_change_bool = False
                Else
                    insieme_strumenti(indice_485_local).mvalue_change_bool = True
                End If
            Case "LD"
                insieme_strumenti(indice_485_local).mvalue2_bool = False
                insieme_strumenti(indice_485_local).mvalue3_bool = False
                insieme_strumenti(indice_485_local).mvalue5_bool = False
                insieme_strumenti(indice_485_local).mvalue6_bool = False
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).mvalue_change_bool = False
                    insieme_strumenti(indice_485_local).mvalue17_bool = False
                Else
                    insieme_strumenti(indice_485_local).mvalue_change_bool = True
                End If
            Case "LD4"
                insieme_strumenti(indice_485_local).mvalue2_bool = False
                insieme_strumenti(indice_485_local).mvalue3_bool = False
                insieme_strumenti(indice_485_local).mvalue5_bool = False
                insieme_strumenti(indice_485_local).mvalue6_bool = False
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).mvalue_change_bool = False
                    insieme_strumenti(indice_485_local).mvalue17_bool = False
                Else
                    insieme_strumenti(indice_485_local).mvalue_change_bool = True
                End If

            Case "LDS"
                insieme_strumenti(indice_485_local).mvalue2_bool = False
                insieme_strumenti(indice_485_local).mvalue3_bool = False
                insieme_strumenti(indice_485_local).mvalue5_bool = False
                insieme_strumenti(indice_485_local).mvalue6_bool = False
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).mvalue_change_bool = False
                    insieme_strumenti(indice_485_local).mvalue17_bool = False
                Else
                    insieme_strumenti(indice_485_local).mvalue_change_bool = True
                End If

            Case "WD"
                insieme_strumenti(indice_485_local).mvalue2_bool = False
                insieme_strumenti(indice_485_local).mvalue3_bool = False
                insieme_strumenti(indice_485_local).mvalue5_bool = False
                insieme_strumenti(indice_485_local).mvalue6_bool = False
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).mvalue_change_bool = False
                    insieme_strumenti(indice_485_local).mvalue17_bool = False
                Else
                    insieme_strumenti(indice_485_local).mvalue_change_bool = True
                End If
            Case "WH"
                insieme_strumenti(indice_485_local).mvalue2_bool = False
                insieme_strumenti(indice_485_local).mvalue3_bool = False
                insieme_strumenti(indice_485_local).mvalue5_bool = False
                insieme_strumenti(indice_485_local).mvalue16_bool = False
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).mvalue_change_bool = False
                    insieme_strumenti(indice_485_local).mvalue17_bool = False
                Else
                    insieme_strumenti(indice_485_local).mvalue_change_bool = True
                End If
            Case "LTA"
                insieme_strumenti(indice_485_local).mvalue2_bool = False
                insieme_strumenti(indice_485_local).mvalue3_bool = False
                insieme_strumenti(indice_485_local).mvalue6_bool = False
                insieme_strumenti(indice_485_local).mvalue22_bool = False
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).mvalue_change_bool = False
                    insieme_strumenti(indice_485_local).mvalue17_bool = False
                Else
                    insieme_strumenti(indice_485_local).mvalue_change_bool = True
                End If
            Case "LTD"
                insieme_strumenti(indice_485_local).mvalue2_bool = False
                insieme_strumenti(indice_485_local).mvalue3_bool = False
                insieme_strumenti(indice_485_local).mvalue6_bool = False
                insieme_strumenti(indice_485_local).mvalue22_bool = False
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).mvalue_change_bool = False
                    insieme_strumenti(indice_485_local).mvalue17_bool = False
                Else
                    insieme_strumenti(indice_485_local).mvalue_change_bool = True
                End If
            Case "LTU"
                insieme_strumenti(indice_485_local).mvalue2_bool = False
                insieme_strumenti(indice_485_local).mvalue3_bool = False
                insieme_strumenti(indice_485_local).mvalue6_bool = False
                insieme_strumenti(indice_485_local).mvalue22_bool = False
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).mvalue_change_bool = False
                    insieme_strumenti(indice_485_local).mvalue17_bool = False
                Else
                    insieme_strumenti(indice_485_local).mvalue_change_bool = True
                End If

            Case "LTB"
                insieme_strumenti(indice_485_local).mvalue2_bool = False
                insieme_strumenti(indice_485_local).mvalue3_bool = False
                insieme_strumenti(indice_485_local).mvalue5_bool = False
                insieme_strumenti(indice_485_local).mvalue6_bool = False
                insieme_strumenti(indice_485_local).mvalue21_bool = False
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).mvalue_change_bool = False
                    insieme_strumenti(indice_485_local).mvalue17_bool = False
                Else
                    insieme_strumenti(indice_485_local).mvalue_change_bool = True
                End If

            Case "Tower"
                insieme_strumenti(indice_485_local).mvalue2_bool = False
                insieme_strumenti(indice_485_local).mvalue3_bool = False
                insieme_strumenti(indice_485_local).mvalue5_bool = False
                insieme_strumenti(indice_485_local).mvalue6_bool = False
                insieme_strumenti(indice_485_local).mvalue8_bool = False
                insieme_strumenti(indice_485_local).mvalue9_bool = False
                insieme_strumenti(indice_485_local).mvalue10_bool = False
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).mvalue_change_bool = False
                Else
                    insieme_strumenti(indice_485_local).mvalue_change_bool = True
                End If
            Case "LDtower"
                insieme_strumenti(indice_485_local).mvalue2_bool = False
                insieme_strumenti(indice_485_local).mvalue3_bool = False
                insieme_strumenti(indice_485_local).mvalue5_bool = False
                insieme_strumenti(indice_485_local).mvalue6_bool = False
                insieme_strumenti(indice_485_local).mvalue8_bool = False
                insieme_strumenti(indice_485_local).mvalue9_bool = False
                insieme_strumenti(indice_485_local).mvalue10_bool = False
                insieme_strumenti(indice_485_local).mvalue34_bool = False
                If insieme_strumenti(indice_485_local).change_enabled Then
                    insieme_strumenti(indice_485_local).mvalue_change_bool = False
                Else
                    insieme_strumenti(indice_485_local).mvalue_change_bool = True
                End If

            Case "LDMA"
                If log_ldma Then
                    insieme_strumenti(indice_485_local).mvalue_log_ldma_bool = False
                    insieme_strumenti(indice_485_local).reading_log = True
                    insieme_strumenti(indice_485_local).first_log = True
                    insieme_strumenti(indice_485_local).list_10_log.Clear()
                    insieme_strumenti(indice_485_local).counter_log = 0
                Else
                    If insieme_strumenti(indice_485_local).reading_log = False Then
                        insieme_strumenti(indice_485_local).mvalue2_bool = False
                        insieme_strumenti(indice_485_local).mvalue3_bool = False
                        insieme_strumenti(indice_485_local).mvalue13_bool = False
                        insieme_strumenti(indice_485_local).mvalue17_bool = False

                        If insieme_strumenti(indice_485_local).change_enabled Then
                            insieme_strumenti(indice_485_local).mvalue_change_bool = False
                        Else
                            insieme_strumenti(indice_485_local).mvalue_change_bool = True
                        End If
                    Else
                        If insieme_strumenti(indice_485_local).mvalue_log_ldma_bool = False Then
                            insieme_strumenti(indice_485_local).mvalue_log_ldma_bool = True

                        End If
                    End If
                End If
            Case "LDLG"
                If log_ldma Then
                    insieme_strumenti(indice_485_local).mvalue_log_ldma_bool = False
                    insieme_strumenti(indice_485_local).reading_log = True
                    insieme_strumenti(indice_485_local).first_log = True
                    insieme_strumenti(indice_485_local).list_10_log.Clear()
                    insieme_strumenti(indice_485_local).counter_log = 0
                Else
                    If insieme_strumenti(indice_485_local).reading_log = False Then
                        insieme_strumenti(indice_485_local).mvalue2_bool = False
                        insieme_strumenti(indice_485_local).mvalue3_bool = False
                        insieme_strumenti(indice_485_local).mvalue13_bool = False
                        insieme_strumenti(indice_485_local).mvalue17_bool = False

                        If insieme_strumenti(indice_485_local).change_enabled Then
                            insieme_strumenti(indice_485_local).mvalue_change_bool = False
                        Else
                            insieme_strumenti(indice_485_local).mvalue_change_bool = True
                        End If
                    Else
                        If insieme_strumenti(indice_485_local).mvalue_log_ldma_bool = False Then
                            insieme_strumenti(indice_485_local).mvalue_log_ldma_bool = True

                        End If
                    End If
                End If
        End Select
    End Sub
    Private Sub Timer_refresh_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer_refresh.Elapsed
        Timer_refresh.Stop()
        Dim i As Integer = 0
        Dim old_refresh As Boolean = False
        Dim righe_tanella As strumenti_db.strumentiDataTable
        'righe_tanella = query.get_strumenti_all(midentificativo)
        'If Not righe_tanella Is Nothing Then
        '    For Each dc In righe_tanella
        '        If Not insieme_strumenti(Val(dc.id_485)) Is Nothing Then
        '            Try
        '                If dc.nome.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mtipo_strumento_bool = False
        '                End If

        '                If dc.value1.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue1_bool = False
        '                End If
        '                If dc.value2.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue2_bool = False
        '                End If
        '                If dc.value3.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue3_bool = False
        '                End If
        '                If dc.value4.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue4_bool = False
        '                End If
        '                If dc.value5.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue5_bool = False
        '                End If
        '                If dc.value6.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue6_bool = False
        '                End If
        '                If dc.value7.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue7_bool = False
        '                End If
        '                If dc.value8.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue8_bool = False
        '                End If
        '                If dc.value9.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue9_bool = False
        '                End If
        '                If dc.value10.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue10_bool = False
        '                End If
        '                If dc.value11.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue11_bool = False
        '                End If
        '                If dc.value12.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue12_bool = False
        '                End If
        '                If dc.value13.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue13_bool = False
        '                End If
        '                If dc.value14.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue14_bool = False
        '                End If
        '                If dc.value15.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue15_bool = False
        '                End If
        '                If dc.value16.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue16_bool = False
        '                End If
        '                If dc.value17.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue17_bool = False
        '                End If
        '                If dc.value18.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue18_bool = False
        '                End If
        '                If dc.value19.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue19_bool = False
        '                End If
        '                If dc.value20.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue20_bool = False
        '                End If
        '                If dc.value21.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue21_bool = False
        '                End If
        '                If dc.value22.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue22_bool = False
        '                End If
        '                If dc.value23.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue23_bool = False
        '                End If
        '                If dc.value24.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue24_bool = False
        '                End If
        '                If dc.value25.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue25_bool = False
        '                End If
        '                If dc.value26.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue26_bool = False
        '                End If
        '                If dc.value27.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue27_bool = False
        '                End If
        '                If dc.value28.Length < 5 Then
        '                    insieme_strumenti(Val(dc.id_485)).mvalue28_bool = False
        '                End If
        '            Catch ex As Exception

        '            End Try
        '        End If
        '    Next
        'End If
        'Dim pippo As String

        counter_refresh_old = counter_refresh_old + 1
        If counter_refresh_old > 10 Then
            counter_refresh_old = 0
            For i = 1 To numberid_485

            Next
            old_refresh = True
        End If
        For i = 1 To numberid_485 + 1
            Try
                If midentificativo = "999666" Then
                    Dim pippo As String = ""
                    pippo = "ciao"
                End If
                If insieme_strumenti(i) Is Nothing Then
                    insieme_strumenti(i) = New light
                End If
                If insieme_strumenti(i).nome_strumento.Length < 5 Then
                    insieme_strumenti(i).mtipo_strumento_bool = False
                End If
                If i = numberid_485 + 1 Then
                    check_new_instruments = True
                    contatore_nuovi = 0
                    Exit For
                End If
                If insieme_strumenti(i).value1.Length < 5 Then
                    insieme_strumenti(i).mvalue1_bool = False
                End If
                If insieme_strumenti(i).value2.Length < 5 Then
                    insieme_strumenti(i).mvalue2_bool = False
                End If
                If insieme_strumenti(i).value3.Length < 5 Then
                    insieme_strumenti(i).mvalue3_bool = False
                End If
                If insieme_strumenti(i).value4.Length < 5 Then
                    insieme_strumenti(i).mvalue4_bool = False
                End If
                If insieme_strumenti(i).value5.Length < 5 Then
                    insieme_strumenti(i).mvalue5_bool = False
                End If
                If insieme_strumenti(i).value6.Length < 5 Then
                    insieme_strumenti(i).mvalue6_bool = False
                End If
                If insieme_strumenti(i).value7.Length < 5 Then
                    insieme_strumenti(i).mvalue7_bool = False
                End If
                If insieme_strumenti(i).value8.Length < 5 Then
                    insieme_strumenti(i).mvalue8_bool = False
                End If
                If insieme_strumenti(i).value9.Length < 5 Then
                    insieme_strumenti(i).mvalue9_bool = False
                End If
                If insieme_strumenti(i).value10.Length < 5 Then
                    insieme_strumenti(i).mvalue10_bool = False
                End If
                If insieme_strumenti(i).value11.Length < 5 Then
                    insieme_strumenti(i).mvalue11_bool = False
                End If
                If insieme_strumenti(i).value12.Length < 5 Then
                    insieme_strumenti(i).mvalue12_bool = False
                End If
                If insieme_strumenti(i).value13.Length < 5 Then
                    insieme_strumenti(i).mvalue13_bool = False
                End If
                If insieme_strumenti(i).value14.Length < 5 Then
                    insieme_strumenti(i).mvalue14_bool = False
                End If
                If insieme_strumenti(i).value15.Length < 5 Then
                    insieme_strumenti(i).mvalue15_bool = False
                End If
                If insieme_strumenti(i).value16.Length < 5 Then
                    insieme_strumenti(i).mvalue16_bool = False
                End If
                If insieme_strumenti(i).value17.Length < 5 Then
                    insieme_strumenti(i).mvalue17_bool = False
                End If
                If insieme_strumenti(i).value18.Length < 5 Then
                    insieme_strumenti(i).mvalue18_bool = False
                End If
                If insieme_strumenti(i).value19.Length < 5 Then
                    insieme_strumenti(i).mvalue19_bool = False
                End If
                If insieme_strumenti(i).value20.Length < 5 Then
                    insieme_strumenti(i).mvalue20_bool = False
                End If
                If insieme_strumenti(i).value21.Length < 5 Then
                    insieme_strumenti(i).mvalue21_bool = False
                End If
                If insieme_strumenti(i).value22.Length < 5 Then
                    insieme_strumenti(i).mvalue22_bool = False
                End If
                If insieme_strumenti(i).value23.Length < 5 Then
                    insieme_strumenti(i).mvalue23_bool = False
                End If
                If insieme_strumenti(i).value24.Length < 5 Then
                    insieme_strumenti(i).mvalue24_bool = False
                End If
                If insieme_strumenti(i).value25.Length < 5 Then
                    insieme_strumenti(i).mvalue25_bool = False
                End If
                If insieme_strumenti(i).value26.Length < 5 Then
                    insieme_strumenti(i).mvalue26_bool = False
                End If
                If insieme_strumenti(i).value27.Length < 5 Then
                    insieme_strumenti(i).mvalue27_bool = False
                End If
                If insieme_strumenti(i).value28.Length < 5 Then
                    insieme_strumenti(i).mvalue28_bool = False
                End If
                If insieme_strumenti(i).value29.Length < 5 Then
                    insieme_strumenti(i).mvalue29_bool = False
                End If
                If insieme_strumenti(i).value30.Length < 5 Then
                    insieme_strumenti(i).mvalue30_bool = False
                End If
                If insieme_strumenti(i).value31.Length < 5 Then
                    insieme_strumenti(i).mvalue31_bool = False
                End If
                If insieme_strumenti(i).value32.Length < 5 Then
                    insieme_strumenti(i).mvalue32_bool = False
                End If

            Catch ex As Exception
                'anomalia
            End Try
            If old_refresh And insieme_strumenti(i).change_enabled = False Then
                insieme_strumenti(i).read_all_value()
            Else
                read_realtime(i, old_refresh)
            End If
            'If i <= numberid_485 Then
            '    aggiorna_db_strumenti(insieme_strumenti(i), Format(i, "00"), identificativo)

            'End If

        Next

        Timer_refresh.Start()

    End Sub
    Public Sub update_database()
        Dim i As Integer = 0
        For i = 1 To numberid_485
            aggiorna_db_strumenti(insieme_strumenti(i), Format(i, "00"), identificativo)
        Next

    End Sub

    Private Sub Timer2_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer2.Elapsed
        Dim i As Integer = 1
        Dim keep_alive_bool As Boolean = True
        Dim numberid_485_temp As Integer = numberid_485
        Timer2.Enabled = False
        Timer2.Stop()
        'Try
        '    Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log_id.txt", True)
        '        writer.WriteLine("#" + Now.ToString + sequnza_interrogazione.ToString + " " + identificativo + " timer 2 send#")
        '    End Using
        'Catch ex As Exception

        'End Try
        Try
            If mpuntatore_strumento.remove Then
                mpuntatore_strumento.close_connection()
                mpuntatore_strumento = Nothing
                nuovo_strumento_connesso = False
                sequnza_interrogazione = 0
                numero_interrogazione = 0
                maxid_485 = 1
                'Timer2.Interval = 120000
                'Timer2.Enabled = True
                'Timer2.Start()
                Exit Sub
            End If
            If midentificativo = "797255" Then
                sequenza_timer = True
            End If
            Timer2.Interval = 1500

            If check_new_instruments Then
                Me.mpuntatore_strumento.SendMessage(Format(numberid_485_temp + 1, "00") + "emzc82")
                keep_alive_bool = False
                Timer2.Enabled = True
                contatore_nuovi = contatore_nuovi + 1
                If contatore_nuovi > 4 Then
                    check_new_instruments = False
                    contatore_nuovi = 0
                End If

                Exit Sub
            End If

            For i = 1 To numberid_485_temp
                If Not insieme_strumenti(i).mtipo_strumento_bool Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "emzc82")
                    keep_alive_bool = False
                    Exit For
                End If

                If check_bool_value1(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value2(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value3(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value4(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value5(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value6(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value7(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value8(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value9(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value10(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value11(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value12(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value13(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value14(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value15(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value16(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value17(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value18(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value19(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value20(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value21(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value22(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value23(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value24(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value25(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value26(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value27(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value28(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value29(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value30(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value31(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value32(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value33(i, keep_alive_bool) Then
                    Exit For
                End If
                If check_bool_value34(i, keep_alive_bool) Then
                    Exit For
                End If

                If check_bool_log_ldma(i, keep_alive_bool) Then
                    Exit For
                End If


                If Not insieme_strumenti(i).mvalue_change_bool And insieme_strumenti(i).new_version Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "change")
                    keep_alive_bool = False
                    Exit For
                End If

            Next
            If keep_alive_bool Then
                Me.puntatore_strumento.keep_alive_counter = Me.puntatore_strumento.keep_alive_counter + 10
                If Me.puntatore_strumento.keep_alive_counter > Me.puntatore_strumento.keep_alive Then
                    Me.mpuntatore_strumento.SendMessage("emzc82")
                    Me.puntatore_strumento.keep_alive_counter = 0
                End If
            End If
            'sequenza_timer = True

            'numero_interrogazione = numero_interrogazione + 1
            'If numero_interrogazione > 3 Then
            '    If sequnza_interrogazione = 0 Then
            '        Try
            '            Dim righe_aggiornate As Integer = 0
            '            Dim adapter1 As statusTableAdapters.statusTableAdapter = New statusTableAdapters.statusTableAdapter
            '            righe_aggiornate = adapter1.UpdateQuery(get_strumenti_valore, "Connected", Now, identificativo)
            '            If righe_aggiornate = 0 Then
            '                adapter1.InsertQuery(Now, identificativo, get_strumenti_valore, "Connected", Now)
            '            End If
            '        Catch ex As Exception

            '        End Try
            '        If maxid_485 > 1 Then
            '            maxid_485 = maxid_485 - 1
            '            numberid_485 = maxid_485
            '        End If
            '        sequenza_timer = False
            '        nuovo_strumento_connesso = False
            '        sequnza_interrogazione = 0
            '        numero_interrogazione = 0
            '        maxid_485 = 1
            '        'Timer2.Interval = 180000
            '        Timer2.Interval = 300000
            '        Timer2.Enabled = True
            '        If busy_strumento_invio Then
            '        Else
            '            Timer2.Start()
            '        End If
            '        Exit Sub
            '    Else
            '        maxid_485 = maxid_485 + 1
            '        sequnza_interrogazione = 0
            '        numero_interrogazione = 0
            '    End If

            'End If
            Timer2.Enabled = True
            'If busy_strumento_invio Then
            'Else
            Timer2.Start()
            'End If

        Catch ex As Exception
            sequenza_timer = False
            nuovo_strumento_connesso = False
            sequnza_interrogazione = 0
            numero_interrogazione = 0
            maxid_485 = 1
            'Timer2.Interval = 180000
            'Timer2.Interval = 300000
            Timer2.Enabled = True
            If busy_strumento_invio Then
            Else
                Timer2.Start()
            End If
            Exit Sub
        End Try
    End Sub
    Public Function get_strumenti_valore() As String
        Dim cliente_result As String = ""
        Dim i As Integer = 1
        For i = 1 To 30
            Try
                cliente_result = cliente_result + "?" + identificativo + "?" + insieme_strumenti(i).tipo_strumento + "?" + insieme_strumenti(i).nome_strumento + "?"
                cliente_result = cliente_result + insieme_strumenti(i).time_connessione + "?" + insieme_strumenti(i).value1 + "?"
                cliente_result = cliente_result + insieme_strumenti(i).value2 + "?" + insieme_strumenti(i).value3 + "?"
                cliente_result = cliente_result + insieme_strumenti(i).value4 + "?" + insieme_strumenti(i).value5 + "?"
                cliente_result = cliente_result + insieme_strumenti(i).value6 + "?" + insieme_strumenti(i).value7 + "?"
                cliente_result = cliente_result + insieme_strumenti(i).value8 + "?" + insieme_strumenti(i).value9 + "?" + insieme_strumenti(i).value10 + "?" + insieme_strumenti(i).value11 + "?" + insieme_strumenti(i).value12 + "?" + insieme_strumenti(i).value13 + "?" + insieme_strumenti(i).value14 + "?" + insieme_strumenti(i).value15 + "?" + insieme_strumenti(i).value16 + "?"
                cliente_result = cliente_result + insieme_strumenti(i).value17 + "?" + insieme_strumenti(i).value18 + "?" + insieme_strumenti(i).value19 + "?" + insieme_strumenti(i).value20 + "?" + insieme_strumenti(i).value21 + "?" + insieme_strumenti(i).value22 + "?" + insieme_strumenti(i).value23 + "?" + insieme_strumenti(i).value24 + "?" + insieme_strumenti(i).value25 + "?" + insieme_strumenti(i).value26 + "?" + insieme_strumenti(i).value27 + "|"
            Catch
                Exit For
            End Try

        Next
        Return cliente_result
    End Function
    Private Sub Timer3_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer3.Elapsed
        ' busy_strumento_invio = False ' risposta falsa dallo strumento e libero lo strumento
        ' wait_response_invio = False
        free_reading()
        Timer3.Enabled = False
        Timer3.Stop()
    End Sub
    Public Sub free_reading()
        Timer2.Interval = 1000
        sequenza_timer = False
        nuovo_strumento_connesso = False
        sequnza_interrogazione = 0
        numero_interrogazione = 0
        maxid_485 = 1
        Timer2.Start()
    End Sub
    Public Sub lock_reading()
        Timer5.Stop()
        Timer4.Stop()
        'Timer4.Interval = 1800000
        Timer4.Interval = 1800000
        Timer4.Start()
        Timer2.Stop()
    End Sub

    Private Sub Timer4_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer4.Elapsed
        If puntatore_pc1.connected Or puntatore_pc2.connected Or puntatore_pc3.connected Or puntatore_pc4.connected Or identificativo = "420066" Then
            Timer4.Stop()
            Timer4.Enabled = True
            Timer4.Start()
            Exit Sub
        End If
        'If sequenza_timer Then
        '    Timer4.Interval = 60000
        '    Exit Sub
        'End If
        'Timer1.Stop()
        'Timer2.Stop()
        Timer4.Enabled = False
        Timer4.Stop()
        first_log = True
        busy_log = False
        log_completed = False
        blocco_pc = False
        'Dim i As Integer

        'For i = 1 To numberid_485
    
        id_485_log = 1

        log_fail = 0
        Timer5.Interval = 4000
        Timer5.Enabled = True
        Timer5.Start()

        'Try

        '    If insieme_strumenti(i).enabled_log Then
        '        counter_next_log = 0
        '        message_file_check = ""

        '        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "ar")
        '        busy_log = False
        '        log_completed = False
        '        counter_next_log = 1
        '        Do
        '            Thread.Sleep(3000)
        '            If log_completed Then
        '                Exit Do
        '            End If
        '            If busy_log Then ' log restituito nei tempi di thread
        '                Me.mpuntatore_strumento.SendMessage(Format(i, "00") + Format(counter_next_log, "0000") + "u")
        '                counter_next_log = counter_next_log + 1
        '            Else ' log non  restituito nei tempi di thread
        '                Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "az")
        '            End If
        '            busy_log = False

        '        Loop Until (log_completed = True)

        '    End If
        'Catch ex As Exception

        'End Try
        '' Next

        ''Timer1.Start()
        ''Timer2.Start()
        'Timer4.Interval = 1200000 ' dopo la prima lettura l'intervallo di lettura è 30 minuti
        'Timer4.Start()
    End Sub
    Private Sub Timer5_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer5.Elapsed
        Timer5.Enabled = False
        Timer5.Stop()
        If numberid_485 > 0 And id_485_log <= numberid_485 And blocco_pc = False Then
            Try
                If insieme_strumenti(id_485_log).enabled_log Then
                    If log_completed Or first_log Then
                        If first_log Then
                            first_log = False
                        Else
                            first_log = True
                            id_485_log = id_485_log + 1
                            Timer5.Enabled = True
                            Timer5.Start()
                            Exit Sub
                        End If
                        counter_next_log = 0
                        message_file_check = ""
                        Try
                            Me.mpuntatore_strumento.SendMessage(Format(id_485_log, "00") + "ar")
                        Catch ex As Exception
                            'Timer4.Interval = 1800000
                            Timer4.Interval = 1800000
                            Timer4.Enabled = True
                            Timer4.Start()
                            Exit Sub
                        End Try

                        busy_log = False
                        log_completed = False
                        counter_next_log = 1
                        Timer5.Enabled = True
                        Timer5.Start()
                    Else
                        If log_completed Then
                            first_log = True
                            Timer5.Enabled = True
                            Timer5.Start()

                            id_485_log = id_485_log + 1
                            Exit Sub
                        End If
                        Try
                            If busy_log Then ' log restituito nei tempi di thread
                                Me.mpuntatore_strumento.SendMessage(Format(id_485_log, "00") + Format(counter_next_log, "0000") + "u")
                                counter_next_log = counter_next_log + 1
                                'log_fail = 0
                            Else ' log non  restituito nei tempi di thread
                                Me.mpuntatore_strumento.SendMessage(Format(id_485_log, "00") + "az")
                                log_fail = log_fail + 1
                                If log_fail > 5 Then
                                    log_completed = True
                                    first_log = True
                                    Timer5.Enabled = True
                                    Timer5.Start()
                                    id_485_log = id_485_log + 1
                                    Exit Sub
                                End If
                            End If
                            Timer5.Enabled = True
                            Timer5.Start()
                            busy_log = False
                        Catch ex As Exception
                            'Timer4.Interval = 1800000
                            Timer4.Interval = 1800000
                            Timer4.Enabled = True
                            Timer4.Start()
                            Exit Sub
                        End Try

                    End If
                Else
                    first_log = True
                    id_485_log = id_485_log + 1
                    Timer5.Enabled = True
                    Timer5.Start()
                End If

            Catch ex As Exception
                'Timer4.Interval = 1800000
                Timer4.Interval = 1800000
                Timer4.Enabled = True
                Timer4.Start()
            End Try
        Else
            'Timer4.Interval = 1800000
            Timer4.Interval = 1800000
            Timer4.Enabled = True
            Timer4.Start()
        End If
    End Sub


    Private Function check_bool_value1(ByVal indice As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(indice).mvalue1_bool Then
            Select Case insieme_strumenti(indice).tipo_strumento
                Case "max5"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(indice, "00") + "bc")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "Tower"
                    Me.mpuntatore_strumento.SendMessage(Format(indice, "00") + "unitsr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(indice, "00") + "unitsr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True

                Case "LD"
                    Me.mpuntatore_strumento.SendMessage(Format(indice, "00") + "config")
                    keep_alive_bool = False
                    Return True
                Case "LD4"
                    Me.mpuntatore_strumento.SendMessage(Format(indice, "00") + "config")
                    keep_alive_bool = False
                    Return True

                Case "LDS"
                    Me.mpuntatore_strumento.SendMessage(Format(indice, "00") + "config")
                    keep_alive_bool = False
                    Return True
                Case "WH"
                    Me.mpuntatore_strumento.SendMessage(Format(indice, "00") + "config")
                    keep_alive_bool = False
                    Return True
                Case "WD"
                    Me.mpuntatore_strumento.SendMessage(Format(indice, "00") + "config")
                    keep_alive_bool = False
                    Return True
                Case "LTA"
                    Me.mpuntatore_strumento.SendMessage(Format(indice, "00") + "config")
                    keep_alive_bool = False
                    Return True

                Case "LTU"
                    Me.mpuntatore_strumento.SendMessage(Format(indice, "00") + "config")
                    keep_alive_bool = False
                    Return True

                Case "LTD"
                    Me.mpuntatore_strumento.SendMessage(Format(indice, "00") + "config")
                    keep_alive_bool = False
                    Return True

                Case "LTB"
                    Me.mpuntatore_strumento.SendMessage(Format(indice, "00") + "config")
                    keep_alive_bool = False
                    Return True
                Case "LDMA"
                    Me.mpuntatore_strumento.SendMessage(Format(indice, "00") + "config")
                    keep_alive_bool = False
                    Return True
                Case "LDLG"
                    Me.mpuntatore_strumento.SendMessage(Format(indice, "00") + "config")
                    keep_alive_bool = False
                    Return True

            End Select
        End If
        Return False
    End Function
    Private Function check_bool_log_ldma(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue_log_ldma_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "LDMA"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "ildma")
                    keep_alive_bool = False
                    Return True
                Case "LDLG"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "ildlg")
                    keep_alive_bool = False
                    Return True

            End Select

            Return False
        End If
    End Function
    Private Function check_bool_value2(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue2_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "ab")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "Tower"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "valuer")
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "valuer")
                    keep_alive_bool = False
                    Return True

                    'End If
                Case "LD"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "valuer")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "LD4"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "valuer")
                    keep_alive_bool = False
                    Return True
                    'End If

                Case "LDS"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "valuer")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "WH"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "valuer")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "WD"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "valuer")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "LTA"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "valuer")
                    keep_alive_bool = False
                    Return True

                Case "LTU"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "valuer")
                    keep_alive_bool = False
                    Return True

                Case "LTD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "valuer")
                    keep_alive_bool = False
                    Return True

                Case "LTB"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "valuer")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "LDMA"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "valuer")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "LDLG"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "valuer")
                    keep_alive_bool = False
                    Return True
                    'End If

            End Select
        End If
        Return False
    End Function
    Private Function check_bool_value3(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue3_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "ba")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "Tower"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "algenr")
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "algenr")
                    keep_alive_bool = False
                    Return True

                    'End If
                Case "LD"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allrmr")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "LD4"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allrmr")
                    keep_alive_bool = False
                    Return True
                    'End If

                Case "LDS"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allrmr")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "WH"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allrmr")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "WD"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allrmr")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "LTA"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allrmr")
                    keep_alive_bool = False
                    Return True

                Case "LTU"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allrmr")
                    keep_alive_bool = False
                    Return True

                Case "LTD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allrmr")
                    keep_alive_bool = False
                    Return True

                Case "LTB"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allrmr")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "LDMA"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allrmr")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "LDLG"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allrmr")
                    keep_alive_bool = False
                    Return True
                    'End If

            End Select
        End If
        Return False
    End Function
    Private Function check_bool_value4(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue4_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "b5")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "Tower"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "config")
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "config")
                    keep_alive_bool = False
                    Return True

                    'End If
                Case "LD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "calibzr")
                    keep_alive_bool = False
                    Return True
                Case "LD4"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "calibzr")
                    keep_alive_bool = False
                    Return True
                Case "LDS"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "calibzr")
                    keep_alive_bool = False
                    Return True
                Case "WH"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "calibzr")
                    keep_alive_bool = False
                    Return True
                Case "WD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "calibzr")
                    keep_alive_bool = False
                    Return True
                Case "LTA"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "calibzr")
                    keep_alive_bool = False
                    Return True

                Case "LTU"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "calibzr")
                    keep_alive_bool = False
                    Return True

                Case "LTD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "calibzr")
                    keep_alive_bool = False
                    Return True

                Case "LTB"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "calibzr")
                    keep_alive_bool = False
                    Return True

            End Select
            Return False
        End If
    End Function
    Private Function check_bool_value5(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue5_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "1A")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "Tower"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "clockr")
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "clockr")
                    keep_alive_bool = False
                    Return True

                    'End If
                Case "LD"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "outptr")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "LD4"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "outptr")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "LDS"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "outptr")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "WH"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "outptr")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "WD"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "outptr")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "LTB"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "outptr")
                    keep_alive_bool = False
                    Return True
                    'End If

            End Select
        End If
        Return False
    End Function
    Private Function check_bool_value6(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue6_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "2A")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "Tower"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "stbior")
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "stbior")
                    keep_alive_bool = False
                    Return True

                    'End If
                Case "LD"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "clockr")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "LD4"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "clockr")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "LDS"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "clockr")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "WD"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "clockr")
                    keep_alive_bool = False
                    Return True
                    'End If
                Case "LTA"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "clockr")
                    keep_alive_bool = False
                    Return True

                Case "LTU"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "clockr")
                    keep_alive_bool = False
                    Return True

                Case "LTD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "clockr")
                    keep_alive_bool = False
                    Return True

                Case "LTB"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "clockr")
                    keep_alive_bool = False
                    Return True
                    'End If
            End Select
        End If
        Return False
    End Function
    Private Function check_bool_value7(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue7_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "3A")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "Tower"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "optior")
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "optior")
                    keep_alive_bool = False
                    Return True

                Case "LD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "setpntr")
                    keep_alive_bool = False
                    Return True
                Case "LD4"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "setpntr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True
                Case "LDS"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "setpntr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True
                Case "WH"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "setpntr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True
                Case "WD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "setpntr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True
                Case "LTA"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "setpntr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

                Case "LTU"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "setpntr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

                Case "LTD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "setpntr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

                Case "LTB"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "setpntr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True
                Case "LDMA"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "setpntr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True
                Case "LDLG"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "setpntr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

            End Select
        End If
        Return False
    End Function
    Private Function check_bool_value8(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue8_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "4A")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "Tower"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "tota1r")
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "tota1r")
                    keep_alive_bool = False
                    Return True

                    'End If
                Case "LD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "paramtr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True
                Case "LD4"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "paramtr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True

                Case "LDS"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "paramtr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "WH"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "paramtr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "WD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "paramtr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "LTA"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "paramtr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True

                Case "LTU"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "paramtr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True

                Case "LTD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "paramtr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True

                Case "LTB"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "paramtr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "LDMA"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "paramtr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "LDLG"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "paramtr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True

            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value9(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue9_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "5A")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "Tower"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "tota2r")
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "tota2r")
                    keep_alive_bool = False
                    Return True

                    'End If
                Case "LD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "alldosr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "LD4"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "alldosr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True

                Case "LDS"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "alldosr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "WH"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "alldosr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "WD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "alldosr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value10(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue10_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                    If insieme_strumenti(i).new_version Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "1B")
                        keep_alive_bool = False
                        priority_setpoint = False
                        Return True
                    End If
                Case "Tower"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "stoutr")
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    'If priority_setpoint = False Then
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "stoutr")
                    keep_alive_bool = False
                    Return True

                    ' End If
                Case "LD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allprbr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "LD4"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allprbr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "LDS"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allprbr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "WH"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allprbr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "WD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allprbr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value11(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue11_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                    If insieme_strumenti(i).new_version Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "2B")
                        keep_alive_bool = False
                        priority_setpoint = False
                        Return True
                    End If
                Case "Tower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "passcr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "passcr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True
                Case "LD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "flowstr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "LD4"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "flowstr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True

                Case "LDS"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "flowstr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "WH"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "flowstr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "WD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "flowstr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "LTA"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "flowstr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True

                Case "LTU"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "flowstr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True

                Case "LTD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "flowstr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True

                Case "LTB"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "flowstr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True

            End Select
        End If
        Return False
    End Function
    Private Function check_bool_value12(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue12_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                    If insieme_strumenti(i).new_version Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "3B")
                        keep_alive_bool = False
                        priority_setpoint = False
                        Return True
                    End If
                Case "Tower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "logser")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "logser")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

                Case "LD"
                    If insieme_strumenti(i).disable_comp_ph = False Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "compphr")
                        keep_alive_bool = False
                        priority_setpoint = False
                        Return True
                    End If
                Case "LD4"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "compphr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "LDS"
                    'Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "compphr")
                    'Exit For
                Case "WH"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "compphr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "WD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "compphr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value13(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue13_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                    If insieme_strumenti(i).new_version Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "4B")
                        keep_alive_bool = False
                        priority_setpoint = False
                        Return True
                    End If
                Case "Tower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bleedr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bleedr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

                Case "LD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "clocksr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "LD4"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "clocksr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "LDS"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "clocksr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "WH"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "clocksr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "WD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "clocksr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "LDMA"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "clocksr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True
                Case "LDLG"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "clocksr")
                    keep_alive_bool = False
                    priority_setpoint = False
                    Return True

            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value14(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue14_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                    If insieme_strumenti(i).new_version Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "5B")
                        keep_alive_bool = False
                        priority_setpoint = False
                        Return True
                    End If
                Case "Tower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "inhibr")
                    keep_alive_bool = False

                    priority_setpoint = False
                    Return True
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "inhibr")
                    keep_alive_bool = False

                    priority_setpoint = False
                    Return True

                Case "LD"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "maoutsr")
                    keep_alive_bool = False
                    Return True
                Case "LD4"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "maoutsr")
                    keep_alive_bool = False
                    Return True

                Case "LDS"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "maoutsr")
                    keep_alive_bool = False
                    Return True
                    'Case "LTB"
                    '    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "maoutsr")
                    '    keep_alive_bool = False
                    '    Exit For

                Case "WD"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "setstkr")
                    keep_alive_bool = False
                    Return True
            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value15(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue15_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                    If insieme_strumenti(i).new_version Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "1C")
                        keep_alive_bool = False
                        priority_setpoint = False
                        Return True
                    End If
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bioc1r")
                    keep_alive_bool = False
                    Return True

                Case "Tower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bioc1r")
                    keep_alive_bool = False
                    Return True
                Case "LD4"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "logsetr")
                    keep_alive_bool = False
                    Return True

                Case "LD"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "logsetr")
                    keep_alive_bool = False
                    Return True
                Case "LDS"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "logsetr")
                    keep_alive_bool = False
                    Return True
                Case "WH"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "logsetr")
                    keep_alive_bool = False
                    Return True
                Case "LTA"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "logsetr")
                    keep_alive_bool = False
                    Return True

                Case "LTU"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "logsetr")
                    keep_alive_bool = False
                    Return True

                Case "LTD"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "logsetr")
                    keep_alive_bool = False
                    Return True

                Case "LTB"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "logsetr")
                    keep_alive_bool = False
                    Return True

                Case "WD"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "logsetr")
                    keep_alive_bool = False
                    Return True
                Case "LDMA"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "logsetr")
                    keep_alive_bool = False
                    Return True
                Case "LDLG"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "logsetr")
                    keep_alive_bool = False
                    Return True

            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value16(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue16_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                    If insieme_strumenti(i).new_version Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "1D")
                        keep_alive_bool = False
                        priority_setpoint = False
                        Return True
                    End If
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi1w1r")
                    keep_alive_bool = False
                    Return True

                Case "Tower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi1w1r")
                    keep_alive_bool = False
                    Return True
                Case "LD4"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "minmaxr")
                    keep_alive_bool = False
                    Return True

                Case "LD"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "minmaxr")
                    keep_alive_bool = False
                    Return True
                Case "LDS"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "minmaxr")
                    keep_alive_bool = False
                    Return True
                Case "WH"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "diginpr")
                    keep_alive_bool = False
                    Return True

                Case "WD"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "diginpr")
                    keep_alive_bool = False
                    Return True
            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value17(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue17_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                    If insieme_strumenti(i).new_version Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "b6")
                        keep_alive_bool = False
                        priority_setpoint = False
                        Return True
                    End If
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi1w2r")
                    keep_alive_bool = False
                    Return True

                Case "Tower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi1w2r")
                    keep_alive_bool = False
                    Return True
                Case "LD4"
                    If insieme_strumenti(i).new_version Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "servicr")
                        keep_alive_bool = False
                        Return True
                    End If

                Case "LD"
                    If insieme_strumenti(i).new_version Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "servicr")
                        keep_alive_bool = False
                        Return True
                    End If
                Case "LDS"
                    If insieme_strumenti(i).new_version Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "servicr")
                        keep_alive_bool = False
                        Return True
                    End If
                Case "LTA"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "servicr")
                    keep_alive_bool = False
                    Return True

                Case "LTU"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "servicr")
                    keep_alive_bool = False
                    Return True

                Case "LTD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "servicr")
                    keep_alive_bool = False
                    Return True

                Case "LTB"
                    If insieme_strumenti(i).new_version Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "servicr")
                        keep_alive_bool = False
                        Return True
                    End If
                Case "WH"
                    If insieme_strumenti(i).new_version Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "servicr")
                        keep_alive_bool = False
                        Return True
                    End If
                Case "WD"
                    If insieme_strumenti(i).new_version Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "servicr")
                        keep_alive_bool = False
                        Return True
                    End If
                Case "LDMA"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "servicr")
                    keep_alive_bool = False
                    Return True
                Case "LDLG"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "servicr")
                    keep_alive_bool = False
                    Return True

                    'Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "setstkr")
                    'Exit For
            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value18(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue18_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                Case "Tower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi1w3r")
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi1w3r")
                    keep_alive_bool = False
                    Return True

                Case "LD4"
                    If insieme_strumenti(i).new_version Then
                        priority_setpoint = False
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "labelr")
                        keep_alive_bool = False
                        Return True
                    End If

                Case "LD"
                    If insieme_strumenti(i).new_version Then
                        priority_setpoint = False
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "labelr")
                        keep_alive_bool = False
                        Return True
                    End If

                    'Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "diginpr")
                    'Exit For
                Case "LDS"
                    If insieme_strumenti(i).new_version Then
                        priority_setpoint = False
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "labelr")
                        keep_alive_bool = False
                        Return True
                    End If
                    'Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "diginpr")
                    'Exit For
                Case "WH"
                    If insieme_strumenti(i).new_version Then
                        priority_setpoint = False
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "labelr")
                        keep_alive_bool = False
                        Return True
                    End If
                Case "WD"
                    If insieme_strumenti(i).new_version Then
                        priority_setpoint = False
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "labelr")
                        keep_alive_bool = False
                        Return True
                    End If
                Case "LTA"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "labelr")
                    keep_alive_bool = False
                    Return True

                Case "LTU"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "labelr")
                    keep_alive_bool = False
                    Return True

                Case "LTD"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "labelr")
                    keep_alive_bool = False
                    Return True

                Case "LTB"
                    If insieme_strumenti(i).new_version Then
                        priority_setpoint = False
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "labelr")
                        keep_alive_bool = False
                        Return True
                    End If
                Case "LDMA"
                    If insieme_strumenti(i).new_version Then
                        priority_setpoint = False
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "labelr")
                        keep_alive_bool = False
                        Return True
                    End If
                Case "LDLG"
                    If insieme_strumenti(i).new_version Then
                        priority_setpoint = False
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "labelr")
                        keep_alive_bool = False
                        Return True
                    End If

            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value19(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue19_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                Case "Tower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi1w4r")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi1w4r")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

                Case "LD"
                    If insieme_strumenti(i).new_version Then
                        priority_setpoint = False
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "smsmar")

                        keep_alive_bool = False
                        Return True
                    End If
                Case "LD4"
                    If insieme_strumenti(i).new_version Then
                        priority_setpoint = False
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "smsmar")

                        keep_alive_bool = False
                        Return True
                    End If

                Case "LDS"
                    If insieme_strumenti(i).new_version Then
                        priority_setpoint = False
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "smsmar")

                        keep_alive_bool = False
                        Return True
                    End If
                Case "WH"
                    If insieme_strumenti(i).new_version Then
                        priority_setpoint = False
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "smsmar")
                        keep_alive_bool = False
                        Return True
                    End If
                Case "LTA"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "smsmar")
                    keep_alive_bool = False
                    Return True

                Case "LTU"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "smsmar")
                    keep_alive_bool = False
                    Return True

                Case "LTD"
                    priority_setpoint = False
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "smsmar")
                    keep_alive_bool = False
                    Return True

                Case "LTB"
                    If insieme_strumenti(i).new_version Then
                        priority_setpoint = False
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "smsmar")
                        keep_alive_bool = False
                        Return True
                    End If

                Case "WD"
                    If insieme_strumenti(i).new_version Then
                        priority_setpoint = False
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "smsmar")
                        keep_alive_bool = False
                        Return True
                    End If
            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value20(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue20_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                Case "Tower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bioc2r")
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bioc2r")
                    keep_alive_bool = False
                    Return True

                Case "LD"
                Case "LD4"
                    If insieme_strumenti(i).new_version Then
                        priority_setpoint = False
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "wmeterr")
                        keep_alive_bool = False
                        Return True
                    End If
                Case "LDS"
                    If insieme_strumenti(i).new_version Then
                        priority_setpoint = False
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "selfcr")
                        keep_alive_bool = False
                        Return True
                    End If
                Case "LTA"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "wtmeter")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

                Case "LTU"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "wtmeter")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

                Case "LTD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "wtmeter")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

                Case "LTB"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "wtmeter")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

                Case "WH"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "autsetr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value21(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue21_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                Case "Tower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi2w1r")
                    keep_alive_bool = False
                    Return True

                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi2w1r")
                    keep_alive_bool = False
                    Return True
                Case "LD4"
                    If insieme_strumenti(i).new_version Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "flowalr")
                        priority_setpoint = False
                        keep_alive_bool = False
                        Return True
                    End If

                Case "LD"
                Case "LDS"
                    If insieme_strumenti(i).new_version Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "circor")
                        priority_setpoint = False
                        keep_alive_bool = False
                        Return True
                    End If
                Case "WD"
                Case "WH"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allertr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True
                Case "LTA"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allenar")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

                Case "LTU"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allenar")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

                Case "LTD"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "allenar")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

                Case "LTB"
                    If Val(insieme_strumenti(i).get_str_version(insieme_strumenti(i).nome_strumento)) >= 117 Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "nciclir")
                        priority_setpoint = False
                        keep_alive_bool = False
                        Return True
                    End If
            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value22(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue22_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                Case "Tower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi2w2r")
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi2w2r")
                    keep_alive_bool = False
                    Return True

                Case "LD"
                Case "LDS"
                Case "WD"
                Case "WH"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "timerpr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True
                Case "LTA"
                    If Val(insieme_strumenti(i).get_str_version(insieme_strumenti(i).nome_strumento)) >= 218 Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "nciclir")
                        priority_setpoint = False
                        keep_alive_bool = False
                        Return True
                    End If

                Case "LTU"
                    If Val(insieme_strumenti(i).get_str_version(insieme_strumenti(i).nome_strumento)) >= 218 Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "nciclir")
                        priority_setpoint = False
                        keep_alive_bool = False
                        Return True
                    End If

                Case "LTD"
                    If Val(insieme_strumenti(i).get_str_version(insieme_strumenti(i).nome_strumento)) >= 218 Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "nciclir")
                        priority_setpoint = False
                        keep_alive_bool = False
                        Return True
                    End If

            End Select
        End If
        Return False
    End Function
    Private Function check_bool_value23(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue23_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                Case "Tower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi2w3r")
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi2w3r")
                    keep_alive_bool = False
                    Return True

                Case "LD"
                Case "LDS"
                Case "WD"
            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value24(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue24_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                Case "Tower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi2w4r")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi2w4r")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

                Case "LD"
                Case "LDS"
                Case "WD"
            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value25(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue25_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                Case "Tower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "flowmr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "flowmr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

                Case "LD"
                Case "LDS"
                Case "WD"
            End Select
        End If
        Return False
    End Function
    Private Function check_bool_value26(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue26_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                Case "Tower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "alarmr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "alarmr")
                    priority_setpoint = False
                    keep_alive_bool = False
                    Return True

                Case "LD"
                Case "LDS"
                Case "WD"
            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value27(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue27_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                Case "Tower"
                    If insieme_strumenti(i).mBoiler Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "boiler")
                        priority_setpoint = False
                        keep_alive_bool = False
                        Return True
                    End If
                    If insieme_strumenti(i).type_tower > 1 Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "setp2r")
                        priority_setpoint = False
                        keep_alive_bool = False
                        Return True
                    End If
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi1w5r")
                    keep_alive_bool = False
                    Return True

                Case "LD"
                Case "LDS"
                Case "WD"
            End Select
        End If
        Return False
    End Function
    Private Function check_bool_value28(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue28_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                Case "Tower"
                    If insieme_strumenti(i).type_tower > 2 Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "setp3r")
                        priority_setpoint = False
                        keep_alive_bool = False
                        Return True
                    End If
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi1w6r")
                    keep_alive_bool = False
                    Return True

                Case "LD"
                Case "LDS"
                Case "WD"
            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value29(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue29_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                Case "Tower"
                    If insieme_strumenti(i).mail_csv_setting Then
                        Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "emailr")
                        keep_alive_bool = False
                        Return True
                    End If
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi1w7r")
                    keep_alive_bool = False
                    Return True

                Case "LD"
                Case "LDS"
                Case "WD"
            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value30(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue30_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi2w5r")
                    keep_alive_bool = False
                    Return True

                Case "LD"
                Case "LDS"
                Case "WD"
            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value31(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue31_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi2w6r")
                    keep_alive_bool = False
                    Return True

                Case "LD"
                Case "LDS"
                Case "WD"
            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value32(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue32_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "bi2w7r")
                    keep_alive_bool = False
                    Return True

                Case "LD"
                Case "LDS"
                Case "WD"
            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value33(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue33_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "calcdr")
                    keep_alive_bool = False
                    Return True

                Case "LD"
                Case "LDS"
                Case "WD"
            End Select

        End If
        Return False
    End Function
    Private Function check_bool_value34(ByVal i As Integer, Optional ByRef keep_alive_bool As Boolean = False) As Boolean
        If Not insieme_strumenti(i).mvalue34_bool Then
            Select Case insieme_strumenti(i).tipo_strumento
                Case "max5"
                Case "LDtower"
                    Me.mpuntatore_strumento.SendMessage(Format(i, "00") + "servir")
                    keep_alive_bool = False
                    Return True
                Case "LD"
                Case "LDS"
                Case "WD"
            End Select

        End If
        Return False
    End Function

End Class