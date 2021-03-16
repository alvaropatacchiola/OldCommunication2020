Imports System.Data.SqlClient
Public Class query
    'Public Shared objConn As New SqlConnection("Data Source=LOCALHOST;Initial Catalog=max_5;Integrated Security=True;MultipleActiveResultSets=True")
    Public anomalia As anomaliaTableAdapters.anomaliaTableAdapter = New anomaliaTableAdapters.anomaliaTableAdapter
    Public strimenti_db As strumenti_dbTableAdapters.strumentiTableAdapter = New strumenti_dbTableAdapters.strumentiTableAdapter
    Public strumenti_csv As strumenti_dbTableAdapters.enable_csvTableAdapter = New strumenti_dbTableAdapters.enable_csvTableAdapter

    Public ip_puntatore As ip_listTableAdapters.ip_listTableAdapter = New ip_listTableAdapters.ip_listTableAdapter
    Public mail_puntatore As mailTableAdapters.impianto_newTableAdapter = New mailTableAdapters.impianto_newTableAdapter
    Public log_max5_puntatore As log_dbTableAdapters.logTableAdapter = New log_dbTableAdapters.logTableAdapter
    Public log_max5_puntatore_tampone As log_dbTableAdapters.log_tamponeTableAdapter = New log_dbTableAdapters.log_tamponeTableAdapter
    Public log_ld_puntatore As log_dbTableAdapters.ld_logTableAdapter = New log_dbTableAdapters.ld_logTableAdapter
    Public log_ld_puntatore_tampone As log_dbTableAdapters.ld_log_tamponeTableAdapter = New log_dbTableAdapters.ld_log_tamponeTableAdapter
    Public log_ld4_puntatore As log_dbTableAdapters.ld4_logTableAdapter = New log_dbTableAdapters.ld4_logTableAdapter

    Public log_wd_puntatore As log_dbTableAdapters.wd_logTableAdapter = New log_dbTableAdapters.wd_logTableAdapter
    Public log_wh_puntatore As log_dbTableAdapters.wh_logTableAdapter = New log_dbTableAdapters.wh_logTableAdapter
    Public log_ltb_puntatore As log_dbTableAdapters.ltb_logTableAdapter = New log_dbTableAdapters.ltb_logTableAdapter
    Public log_lta_ltu_ltd_puntatore As log_dbTableAdapters.log_lta_ltu_ltdTableAdapter = New log_dbTableAdapters.log_lta_ltu_ltdTableAdapter
    Public log_ldma_puntatore As log_dbTableAdapters.log_ldmaTableAdapter = New log_dbTableAdapters.log_ldmaTableAdapter
    Public log_ldlg_puntatore As log_dbTableAdapters.log_ldlgTableAdapter = New log_dbTableAdapters.log_ldlgTableAdapter

    Public log_wd_puntatore_tampone As log_dbTableAdapters.wd_log_tamponeTableAdapter = New log_dbTableAdapters.wd_log_tamponeTableAdapter
    Public log_tower_puntatore As log_dbTableAdapters.log_towerTableAdapter = New log_dbTableAdapters.log_towerTableAdapter
    Public log_tower_puntatore_tampone As log_dbTableAdapters.log_tower_tamponeTableAdapter = New log_dbTableAdapters.log_tower_tamponeTableAdapter
    Public Function create_db_connection()
        'If objConn.State = ConnectionState.Closed Then
        '    objConn.Open()

        'End If
    End Function
    Public Function close_db_connection()
        'If objConn.State = ConnectionState.Open Then
        '    objConn.Close()
        'End If
    End Function
    'Public Shared Function check_log_table(ByVal codice_str As String) As Boolean
    '    Dim restrictions(3) As String
    '    restrictions(2) = codice_str
    '    Dim dbTbl As DataTable = objConn.GetSchema("Tables", restrictions)
    '    If dbTbl.Rows.Count = 0 Then
    '        'Table does not exist
    '        Return False
    '    Else
    '        'Table exists
    '        Return True
    '    End If

    'End Function


    Public Function registra_anomalia(ByVal testo As String, ByVal identificativo As String, ByVal id_485 As String)
        Try
            'anomalia.Connection = objConn
            anomalia.InsertQuery(Now, testo, identificativo + "|" + id_485, "")
        Catch ex As Exception

        End Try
    End Function
    Public Function aggiorna_strumento(ByVal tipo_strumento_sequenza As String, ByVal puntatore_temp As light, ByVal id_485_temp As String, ByVal codice_temp As String) As Integer
        Dim righe_aggiornate As Integer = 0
        Dim i As Integer = 0
        Dim exit_query As Boolean = True


        For i = 0 To 3
            exit_query = True
            Try
                righe_aggiornate = strimenti_db.UpdateQuery(puntatore_temp.time_connessione, puntatore_temp.tipo_strumento, puntatore_temp.nome_strumento, puntatore_temp.value1, puntatore_temp.value2, puntatore_temp.value3, puntatore_temp.value4, puntatore_temp.value5,
                                                puntatore_temp.value6, puntatore_temp.value7, puntatore_temp.value8, puntatore_temp.value9, puntatore_temp.value10, puntatore_temp.value11, puntatore_temp.value12,
                                                puntatore_temp.value13, puntatore_temp.value14, puntatore_temp.value15, puntatore_temp.value16, puntatore_temp.value17, puntatore_temp.value18, puntatore_temp.value19 _
                                                , puntatore_temp.value20, puntatore_temp.value21, puntatore_temp.value22, puntatore_temp.value23, puntatore_temp.value24, puntatore_temp.value25, puntatore_temp.value26, puntatore_temp.value27 _
                                                , puntatore_temp.value28, puntatore_temp.value29, puntatore_temp.value30, puntatore_temp.value31, puntatore_temp.value32, puntatore_temp.value33, puntatore_temp.value34, puntatore_temp.time_no_flow, puntatore_temp.mail_csv_setting, puntatore_temp.counter_log, codice_temp, id_485_temp)
                If righe_aggiornate = 0 Then
                    strimenti_db.InsertQuery(Now, codice_temp, id_485_temp, puntatore_temp.nome_strumento, "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 0, 0, 0, 0, 0, puntatore_temp.tipo_strumento, "", "", "", "")
                End If


            Catch ex As Exception

                'Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\query_errot.txt", True)
                '    writer.WriteLine("#" + Now.ToString + codice_temp + "#" + id_485_temp)
                'End Using

                exit_query = False
            End Try
            If exit_query Then
                Exit For
            End If
        Next
        'strimenti_db.Connection = objConn


        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_nome(ByVal value_nome As String, ByVal value_tipo As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        Dim pippo As Integer = 0
        'strimenti_db.Connection = objConn
        'righe_aggiornate = strimenti_db.get_strumenti(identificativo, Format(id_485_temp, "00")).Count
        righe_aggiornate = strimenti_db.Update_nome(Now, value_nome, value_tipo, identificativo, Format(id_485_temp, "00"))
        If righe_aggiornate = 0 Then
            strimenti_db.InsertQuery(Now, identificativo, Format(id_485_temp, "00"), value_nome, "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 0, 0, 0, 0, 0, value_tipo, "", "", "", "")
            If value_nome = "max5" Then
                pippo = 1
            End If
            'Else
            '    righe_aggiornate = strimenti_db.Update_nome(Now, value_nome, value_tipo, identificativo, Format(id_485_temp, "00"))
            '    If value_nome = "max5" Then
            '        pippo = 1
            '    End If

        End If
        Return righe_aggiornate
    End Function
    Public Function aggiorna_csv(ByVal tipo_strumento As String, ByVal enable_csv As Integer, ByVal config As String, ByVal log_config As String, ByVal mail As String, ByVal codice As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        righe_aggiornate = strumenti_csv.UpdateQuery(tipo_strumento, enable_csv, config, log_config, mail, codice, Format(id_485_temp, "00"))
        If righe_aggiornate = 0 Then
            strumenti_csv.InsertQuery(codice, Format(id_485_temp, "00"), tipo_strumento, enable_csv, config, log_config, mail)
        End If

    End Function
    Public Function aggiorna_strumento_value1(ByVal value_1 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value1(Now, value_1, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value2(ByVal value_2 As String, ByVal identificativo As String, ByVal id_485_temp As Integer, ByVal time_no_flow As Long) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value2(Now, value_2, time_no_flow, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value3(ByVal value_3 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value3(Now, value_3, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value4(ByVal value_4 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value4(Now, value_4, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value5(ByVal value_5 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value5(Now, value_5, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value6(ByVal value_6 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        righe_aggiornate = strimenti_db.Update_value6(Now, value_6, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value7(ByVal value_7 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value7(Now, value_7, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value8(ByVal value_8 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value8(Now, value_8, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value9(ByVal value_9 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        righe_aggiornate = strimenti_db.Update_value9(Now, value_9, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value10(ByVal value_10 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value10(Now, value_10, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value11(ByVal value_11 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value11(Now, value_11, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value12(ByVal value_12 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value12(Now, value_12, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value13(ByVal value_13 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value13(Now, value_13, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value14(ByVal value_14 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        righe_aggiornate = strimenti_db.Update_value14(Now, value_14, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value15(ByVal value_15 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value15(Now, value_15, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value16(ByVal value_16 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value16(Now, value_16, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value17(ByVal value_17 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        righe_aggiornate = strimenti_db.Update_value17(Now, value_17, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value18(ByVal value_18 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value18(Now, value_18, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value19(ByVal value_19 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value19(Now, value_19, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value20(ByVal value_20 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value20(Now, value_20, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value21(ByVal value_21 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value21(Now, value_21, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value22(ByVal value_22 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value22(Now, value_22, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value23(ByVal value_23 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value23(Now, value_23, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value24(ByVal value_24 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value24(Now, value_24, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value25(ByVal value_25 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value25(Now, value_25, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value26(ByVal value_26 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value26(Now, value_26, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value27(ByVal value_27 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value27(Now, value_27, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value28(ByVal value_28 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value28(Now, value_28, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function aggiorna_strumento_value29(ByVal value_29 As String, ByVal identificativo As String, ByVal id_485_temp As Integer) As Integer
        Dim righe_aggiornate As Integer = 0
        'strimenti_db.Connection = objConn
        righe_aggiornate = strimenti_db.Update_value29(Now, value_29, identificativo, Format(id_485_temp, "00"))
        Return righe_aggiornate
    End Function
    Public Function get_strumenti_all(ByVal identificativo As String) As appoggio_service.strumenti_db.strumentiDataTable
        'strimenti_db.Connection = objConn
        Try
            Return strimenti_db.get_strumenti_all(identificativo)
        Catch ex As Exception
            Return Nothing
        End Try
        Return Nothing
    End Function
    Public Function aggiorna_ip(ByVal ip_number As String, ByVal codice As String, ByVal differrenza_minuti As Long)
        Dim numero_righe As Integer = 0
        Try
            'ip_puntatore.Connection = objConn
            numero_righe = ip_puntatore.UpdateQuery(ip_number, Now.ToString, Now, differrenza_minuti, codice)
            If numero_righe = 0 Then
                ip_puntatore.InsertQuery(ip_number, codice, "Active", Now, Now.ToString, 0, 1000)
            End If
        Catch ex As Exception

        End Try
    End Function
    Public Function get_ip(ByVal ip_number As String) As appoggio_service.ip_list.ip_listDataTable
        'ip_puntatore.Connection = objConn
        Return ip_puntatore.GetData(ip_number)
    End Function
    Public Function get_ip_config(ByVal codice As String) As appoggio_service.ip_list.ip_listDataTable
        'ip_puntatore.Connection = objConn
        Return ip_puntatore.GetData_config(codice)
    End Function

    Public Function valore_mail(ByVal codice_mail As String) As appoggio_service.mail.impianto_newDataTable
        'mail_puntatore.Connection = objConn
        Return mail_puntatore.GetData(codice_mail)
    End Function
    Public Function log_max5(ByVal identificativo As String, ByVal id_strumento As String, ByVal data As Date, _
                                    ByVal valore_ch1 As Single, ByVal valore_ch2 As Single, ByVal valore_ch3 As Single, ByVal valore_ch4 As Single, ByVal valore_ch5 As Single, _
                                    ByVal aa1_val As String, ByVal ab1_val As String, ByVal ad1_val As String, ByVal ar1_val As String, _
                                    ByVal aa2_val As String, ByVal ab2_val As String, ByVal ad2_val As String, ByVal ar2_val As String, _
                                    ByVal aa3_val As String, ByVal ab3_val As String, ByVal ad3_val As String, ByVal ar3_val As String, _
                                    ByVal aa4_val As String, ByVal ab4_val As String, ByVal ad4_val As String, ByVal ar4_val As String, _
                                    ByVal aa5_val As String, ByVal ab5_val As String, ByVal ad5_val As String, ByVal ar5_val As String, _
                                    ByVal valore_flow As String, ByVal valore_wm As Single, ByVal valore_temp As Single, ByVal val_fml As Single, _
                                    ByVal da1 As Boolean, ByVal db1 As Boolean, ByVal pa1 As Integer, ByVal pb1 As Integer, ByVal ma1 As Integer, _
                                    ByVal da2 As Boolean, ByVal db2 As Boolean, ByVal pa2 As Integer, ByVal pb2 As Integer, ByVal ma2 As Integer, _
                                    ByVal da3 As Boolean, ByVal db3 As Boolean, ByVal pa3 As Integer, ByVal pb3 As Integer, ByVal ma3 As Integer, _
                                    ByVal da4 As Boolean, ByVal db4 As Boolean, ByVal pa4 As Integer, ByVal pb4 As Integer, ByVal ma4 As Integer, _
                                    ByVal da5 As Boolean, ByVal db5 As Boolean, ByVal pa5 As Integer, ByVal pb5 As Integer, ByVal ma5 As Integer, ByVal stby As Boolean) As Boolean
        Dim data_table As log_db.logDataTable
        Dim data_table1 As log_db.log_tamponeDataTable
        Dim count_row As Integer
        'log_max5_puntatore.Connection = objConn
        'data_table = log_max5_puntatore.GetData(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_ch3, valore_ch4, valore_ch5, aa1_val, ab1_val, ad1_val, ar1_val, aa2_val, ab2_val, ad2_val, ar2_val, aa3_val, ab3_val, ad3_val, ar3_val, aa4_val, ab4_val, ad4_val, ar4_val, aa5_val, ab5_val, ad5_val, ar5_val, valore_flow, valore_wm, valore_temp, val_fml)
        data_table = log_max5_puntatore.GetData(identificativo, id_strumento, data, aa1_val, ab1_val, ad1_val, ar1_val, aa2_val, ab2_val, ad2_val, ar2_val, aa3_val, ab3_val, ad3_val, ar3_val, aa4_val, ab4_val, ad4_val, ar4_val, aa5_val, ab5_val, ad5_val, ar5_val, valore_flow, valore_wm, valore_temp, val_fml)
        count_row = data_table.Count
        If count_row = 0 Then
            log_max5_puntatore.InsertQuery(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_ch3, valore_ch4, valore_ch5, aa1_val, ab1_val, ad1_val, ar1_val, aa2_val, ab2_val, ad2_val, ar2_val, aa3_val, ab3_val, ad3_val, ar3_val, aa4_val, ab4_val, ad4_val, ar4_val, aa5_val, ab5_val, ad5_val, ar5_val, valore_flow, valore_wm, valore_temp, val_fml, da1, db1, pa1, pb1, ma1, da2, db2, pa2, pb2, ma2, da3, db3, pa3, pb3, ma3, da4, db4, pa4, pb4, ma4, da5, db5, pa5, pb5, ma5, stby)

            Return True
        Else
            'log_max5_remove(identificativo, id_strumento)
            'data_table1 = log_max5_puntatore_tampone.GetData(identificativo, id_strumento)
            'For Each dc In data_table1
            '    log_max5_puntatore.InsertQuery(identificativo, id_strumento, dc.data, dc.valore1, dc.valore2, dc.valore3, dc.valore4, dc.valore5, dc.Aa1, dc.Ab1, dc.Ad1, dc.Ar1, dc.Aa2, dc.Ab2, dc.Ad2, dc.Ar2, dc.Aa3, dc.Ab3, dc.Ad3, dc.Ar3, dc.Aa4, dc.Ab4, dc.Ad4, dc.Ar4, dc.Aa5, dc.Ab5, dc.Ad5, dc.Ar5, dc.Flow, dc.wm, dc.temperatura, dc.fml)
            'Next
            'log_max5_puntatore_tampone.DeleteQuery(identificativo, id_strumento)
            Return False
            'count_row = data_table1.Count
            'If count_row = 0 Then
            '    Return True
            'Else
            '    Dim codice_user As New System.Guid()
            '    For Each dc In data_table
            '        codice_user = dc.id
            '        Exit For
            '    Next
            '    log_max5_puntatore_tampone.UpdateQuery(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_ch3, valore_ch4, valore_ch5, aa1_val, ab1_val, ad1_val, ar1_val, aa2_val, ab2_val, ad2_val, ar2_val, aa3_val, ab3_val, ad3_val, ar3_val, aa4_val, ab4_val, ad4_val, ar4_val, aa5_val, ab5_val, ad5_val, ar5_val, valore_flow, valore_wm, valore_temp, val_fml, codice_user)
            '    Return False
            'End If

        End If
        Return True
    End Function

    Public Sub log_max5_remove(ByVal identificativo As String, ByVal id_strumento As String)
        Dim data_table1 As log_db.log_tamponeDataTable
        Try
            data_table1 = log_max5_puntatore_tampone.GetData(identificativo, id_strumento)
            For Each dc In data_table1
                'log_max5_puntatore.InsertQuery(identificativo, id_strumento, dc.data, dc.valore1, dc.valore2, dc.valore3, dc.valore4, dc.valore5, dc.Aa1, dc.Ab1, dc.Ad1, dc.Ar1, dc.Aa2, dc.Ab2, dc.Ad2, dc.Ar2, dc.Aa3, dc.Ab3, dc.Ad3, dc.Ar3, dc.Aa4, dc.Ab4, dc.Ad4, dc.Ar4, dc.Aa5, dc.Ab5, dc.Ad5, dc.Ar5, dc.Flow, dc.wm, dc.temperatura, dc.fml)
            Next
            log_max5_puntatore_tampone.DeleteQuery(identificativo, id_strumento)

        Catch ex As Exception

        End Try

    End Sub

    Public Function log_ld(ByVal identificativo As String, ByVal id_strumento As String, ByVal data As Date,
                                    ByVal valore_ch1 As Single, ByVal valore_ch2 As Single, ByVal valore_ch3 As Single,
                                    ByVal valore_flow As String,
                                    ByVal allarmes1_val As String, ByVal allarmes2_val As String, ByVal allarmed1_val As String,
                                    ByVal allarmed2_val As String, ByVal allarmep1_val As String, ByVal allarmep2_val As String, ByVal livello1_val As String,
                                    ByVal livello2_val As String,
                                    ByVal temperatura_val As Single, ByVal livello3_val As String, ByVal stby As Boolean,
                                    ByVal m3h As Single, ByVal tot_iput As Single, ByVal Stato_pulse1_ch1_val As Integer, ByVal Stato_pulse2_ch1_val As Integer, ByVal Stato_pulse_ch2_val As Integer, ByVal Stato_rele_ch1_val As Integer, ByVal Stato_rele_ch2_val As Integer,
                           ByVal valore_ch4 As Single, ByVal allarmes3_val As Boolean, ByVal allarmes4_val As Boolean) As Boolean

        Dim data_table As log_db.ld_logDataTable
        Dim data_table1 As log_db.ld_log_tamponeDataTable
        Dim count_row As Integer
        'log_ld_puntatore.Connection = objConn
        data_table = log_ld_puntatore.GetData(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_ch3, valore_flow, allarmes1_val, allarmes2_val, allarmed1_val, allarmed2_val, allarmep1_val, allarmep2_val, livello1_val, livello2_val, temperatura_val, livello3_val)
        count_row = data_table.Count
        If identificativo = "498699" Then
            Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log_498699.txt", True)
                writer.WriteLine("#" + count_row.ToString + "#" + data.ToString + "#")
                writer.Close()
            End Using
        End If

        If count_row = 0 Then
            count_row = log_ld_puntatore.InsertQuery(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_ch3, valore_flow, allarmes1_val, allarmes2_val, allarmed1_val, allarmed2_val, allarmep1_val, allarmep2_val, livello1_val, livello2_val, temperatura_val, livello3_val, m3h, tot_iput, stby, Stato_pulse1_ch1_val, Stato_pulse2_ch1_val, Stato_pulse_ch2_val, Stato_rele_ch1_val, Stato_rele_ch2_val, valore_ch4, allarmes3_val, allarmes4_val)

            Return True
        Else

            'log_ld_remove(identificativo, id_strumento)
            Return False

        End If
        Return True
    End Function
    Public Function log_ldma(ByVal identificativo As String, ByVal id_strumento As String, ByVal data As Date, _
                                ByVal valore_ch1 As Single, ByVal valore_ch2 As Single, ByVal valore_ch3 As Single, _
                                ByVal valore_ch4 As Single, ByVal valore_ch5 As Single, _
                                ByRef lettura_top10 As List(Of String), ByVal first As Boolean, ByRef counter_log As Integer) As Boolean

        Dim data_table As log_db.log_ldmaDataTable
        Dim count_row As Integer = 0
        Dim count_row_ins As Integer = 0
        'log_ld_puntatore.Connection = objConn
        If first Then
            data_table = log_ldma_puntatore.GetData(identificativo, id_strumento)

            For Each dc In data_table
                lettura_top10.Add(dc.data.Day.ToString + dc.data.Month.ToString + dc.data.Year.ToString + dc.data.Hour.ToString + dc.data.Minute.ToString + _
                dc.livello1.ToString + dc.livello2.ToString + dc.livello3.ToString + dc.livello4.ToString + dc.livello5.ToString)

            Next

        End If
        Dim stringa_confronto As String = data.Day.ToString + data.Month.ToString + data.Year.ToString + data.Hour.ToString + data.Minute.ToString + _
        valore_ch1.ToString + valore_ch2.ToString + valore_ch3.ToString + valore_ch4.ToString + valore_ch5.ToString

        For Each valore In lettura_top10
            If InStr(stringa_confronto, valore) <> 0 Then
                count_row = 1
                counter_log += 1
                Exit For
            End If
        Next



        If count_row = 0 Then

            count_row_ins = log_ldma_puntatore.InsertQuery(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_ch3, valore_ch4, valore_ch5)
            If count_row_ins > 0 Then
                lettura_top10.Add(stringa_confronto)
            End If
            Return True
        Else
            If counter_log > 8 Then
                Return False
            End If
            'log_ld_remove(identificativo, id_strumento)


        End If
        Return True
    End Function
    Public Function log_ldlg(ByVal identificativo As String, ByVal id_strumento As String, ByVal data As Date, _
                            ByVal valore_ch1 As Single, ByVal valore_ch2 As Single, ByVal valore_ch3 As Single, _
                            ByVal valore_ch4 As Single, ByVal valore_ch5 As Single, _
                            ByVal valore_ch1_1 As Single, ByVal valore_ch2_1 As Single, ByVal valore_ch3_1 As Single, _
                            ByVal valore_ch4_1 As Single, ByVal valore_ch5_1 As Single, _
                            ByVal valore_ch1_2 As Single, ByVal valore_ch2_2 As Single, _
                            ByRef lettura_top10 As List(Of String), ByVal first As Boolean, ByRef counter_log As Integer) As Boolean

        Dim data_table As log_db.log_ldlgDataTable
        Dim count_row As Integer = 0
        Dim count_row_ins As Integer = 0
        'log_ld_puntatore.Connection = objConn
        If first Then
            data_table = log_ldlg_puntatore.GetData(identificativo, id_strumento)

            For Each dc In data_table
                lettura_top10.Add(dc.data.Day.ToString + dc.data.Month.ToString + dc.data.Year.ToString + dc.data.Hour.ToString + dc.data.Minute.ToString + _
                dc.counter1.ToString + dc.counter2.ToString + dc.counter3.ToString + dc.counter4.ToString + dc.counter5.ToString + _
                dc.counter1_1.ToString + dc.counter2_1.ToString + dc.counter3_1.ToString + dc.counter4_1.ToString + dc.counter5_1.ToString + _
                dc.counter1_2.ToString + dc.counter2_2.ToString)

            Next

        End If
        'Dim stringa_confronto As String = data.Day.ToString + data.Month.ToString + data.Year.ToString + data.Hour.ToString + data.Minute.ToString + _
        'valore_ch1.ToString + valore_ch2.ToString + valore_ch3.ToString + valore_ch4.ToString + valore_ch5.ToString + _
        'valore_ch1_1.ToString + valore_ch2_1.ToString + valore_ch3_1.ToString + valore_ch4_1.ToString + valore_ch5_1.ToString + _
        'valore_ch1_2.ToString + valore_ch2_2.ToString
        Dim stringa_confronto As String = data.Day.ToString + data.Month.ToString + data.Year.ToString + data.Hour.ToString + data.Minute.ToString


        For Each valore In lettura_top10
            If InStr(stringa_confronto, valore) <> 0 Then
                count_row = 1
                counter_log += 1
                Exit For
            End If
        Next



        If count_row = 0 Then

            count_row_ins = log_ldlg_puntatore.InsertQuery(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_ch3, valore_ch4, valore_ch5, _
                                                           valore_ch1_1, valore_ch2_1, valore_ch3_1, valore_ch4_1, valore_ch5_1, _
                                                           valore_ch1_2, valore_ch2_2)
            If count_row_ins > 0 Then
                lettura_top10.Add(stringa_confronto)
            End If
            Return True
        Else
            If counter_log > 8 Then
                Return False
            End If
            'log_ld_remove(identificativo, id_strumento)


        End If
        Return True
    End Function
    Public Sub log_ld_remove(ByVal identificativo As String, ByVal id_strumento As String)
        Try
            'Dim data_table1 As log_db.ld_log_tamponeDataTable
            'data_table1 = log_ld_puntatore_tampone.GetData(identificativo, id_strumento)
            'For Each dc In data_table1
            '    log_ld_puntatore.InsertQuery(identificativo, id_strumento, dc.data, dc.valore1, dc.valore2, dc.valore3, dc.flusso, dc.feed_limit_ph, dc.feed_limit_cl, dc.dos_alarm_cl, dc.dos_alarm_ph, dc.probe_fail_ph, dc.probe_fail_cl, dc.livello1, dc.livello2, dc.temperatura, dc.livello3)
            'Next
            'log_ld_puntatore_tampone.DeleteQuery(identificativo, id_strumento)

        Catch ex As Exception

        End Try
    End Sub
    Public Function log_ld4(ByVal identificativo As String, ByVal id_strumento As String, ByVal data As Date, _
                                    ByVal valore_ch1 As Single, ByVal valore_ch2 As Single, ByVal valore_ch3 As Single, ByVal valore_ch4 As Single, _
                                    ByVal valore_flow As Boolean, _
                                    ByVal livello_ch1 As Boolean, ByVal livello_ch2 As Boolean, ByVal allarmed1_val As Boolean, _
                                    ByVal allarmed2_val As Boolean, ByVal allarmep1_val As Boolean, ByVal allarmep2_val As Boolean, ByVal allarmes1_val As Boolean, _
                                    ByVal allarmes2_val As Boolean, ByVal allarmes3_val As Boolean, ByVal allarmes4_val As Boolean, _
                                    ByVal livellostby_val As Boolean, _
                                    ByVal temperature_val As Single, ByVal flowmeter_val As Single, ByVal totalizer_val As Single, ByVal flow_meter_low_val As Boolean) As Boolean

        Dim data_table As log_db.ld4_logDataTable
        Dim count_row As Integer
        'log_ld_puntatore.Connection = objConn
        data_table = log_ld4_puntatore.GetData(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_ch3, valore_ch4, valore_flow, livello_ch1, livello_ch2, allarmed1_val, allarmed2_val, allarmep1_val, allarmep2_val, 0, 0, allarmes1_val, allarmes2_val, allarmes3_val, allarmes4_val, livellostby_val, temperature_val, flowmeter_val, totalizer_val, flow_meter_low_val)
        count_row = data_table.Count
        If count_row = 0 Then
            log_ld4_puntatore.InsertQuery(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_ch3, valore_ch4, valore_flow, livello_ch1, livello_ch2, allarmed1_val, allarmed2_val, allarmep1_val, allarmep2_val, 0, 0, allarmes1_val, allarmes2_val, allarmes3_val, allarmes4_val, livellostby_val, temperature_val, flowmeter_val, totalizer_val, flow_meter_low_val)

            Return True
        Else

            'log_ld4_remove(identificativo, id_strumento)
            Return False

        End If

    End Function
    
    Public Function log_wd(ByVal identificativo As String, ByVal id_strumento As String, ByVal data As Date, _
                                ByVal valore_ch1 As Single, ByVal valore_ch2 As Single, _
                                ByVal valore_flow As String, _
                                ByVal allarmed2_val As String, _
                                ByVal allarmed1_val As String, ByVal allarmep1_val As String, ByVal allarmep2_val As String, ByVal livello1_val As String, _
                                ByVal livello2_val As String, ByVal rele_ch1 As Integer, ByVal rele_ch2 As Integer) As Boolean

        Dim data_table As log_db.wd_logDataTable
        Dim data_table1 As log_db.wd_log_tamponeDataTable
        Dim count_row As Integer
        'log_wd_puntatore.Connection = objConn
        data_table = log_wd_puntatore.GetData(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_flow, allarmed2_val, allarmed1_val, allarmep1_val, allarmep2_val, livello1_val, livello2_val, rele_ch1, rele_ch2)
        count_row = data_table.Count
        If count_row = 0 Then
            log_wd_puntatore.InsertQuery(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_flow, allarmed2_val, allarmed1_val, allarmep1_val, allarmep2_val, livello1_val, livello2_val, rele_ch1, rele_ch2)

            Return True
        Else
            'log_wd_remove(identificativo, id_strumento)
            Return False

        End If
        Return True
    End Function


    Public Function log_lta_ltu_ltd(ByVal identificativo As String, ByVal id_strumento As String, ByVal data As Date,
                                ByVal Flow_Acid_L As Boolean, ByVal Flow_Self_Water_L As Boolean,
                                ByVal Flow_Chlorite_L As Boolean,
                                ByVal Lev_Acid_L As Boolean, ByVal Lev_Chlorite_L As Boolean, ByVal Lev_Water_L As Boolean, ByVal Sefl_Acid_L As Boolean,
                                ByVal Sefl_Chlorite_L As Boolean, ByVal TimeOut_L As Boolean, ByVal Flow_Water_dil_L As Boolean, ByVal Service_F_L As Boolean,
                                ByVal Analog_L As Boolean, ByVal Tank_Empty_L As Boolean, ByVal Level_SW As Boolean, ByVal BypassB As Boolean,
                                                                ByVal Lim_Dioxide As Boolean, ByVal Lev_Alflow As Boolean, ByVal Overf As Boolean,
                                    ByVal flow1 As Single, ByVal flow2 As Single, ByVal lettura As Single, ByVal naso As Single, ByVal temperatura As Single, ByVal levSoglia As Boolean,
                                    ByVal totAcido As Single, ByVal totCloro As Single, ByVal totAcqua As Single,
                                    ByVal totAcidoDay As Single, ByVal totCloroDay As Single, ByVal totAcquaDay As Single, ByVal setpoint As Single) As Boolean

        Dim data_table As log_db.log_lta_ltu_ltdDataTable

        Dim count_row As Integer
        'log_wd_puntatore.Connection = objConn
        data_table = log_lta_ltu_ltd_puntatore.GetData(identificativo, id_strumento, data, Flow_Acid_L, Flow_Self_Water_L, Flow_Chlorite_L, Lev_Acid_L, Lev_Chlorite_L, Lev_Water_L,
                                                       Sefl_Acid_L, Sefl_Chlorite_L, TimeOut_L, Flow_Water_dil_L, Service_F_L, Analog_L, Tank_Empty_L, Level_SW, BypassB,
                                                       Lim_Dioxide, Lev_Alflow, Overf)
        count_row = data_table.Count
        If count_row = 0 Then
            log_lta_ltu_ltd_puntatore.InsertQuery(identificativo, id_strumento, data, Flow_Acid_L, Flow_Self_Water_L, Flow_Chlorite_L, Lev_Acid_L, Lev_Chlorite_L, Lev_Water_L,
                                                       Sefl_Acid_L, Sefl_Chlorite_L, TimeOut_L, Flow_Water_dil_L, Service_F_L, Analog_L, Tank_Empty_L, Level_SW, BypassB, Lim_Dioxide,
                                                        Lev_Alflow, Overf, flow1, flow2, lettura, naso, temperatura, levSoglia, totAcido, totCloro, totAcqua,
                                                  totAcidoDay, totCloroDay, totAcquaDay, setpoint)

            Return True
        Else
            'log_wd_remove(identificativo, id_strumento)
            Return False

        End If
        Return True
    End Function

    Public Function log_ltb(ByVal identificativo As String, ByVal id_strumento As String, ByVal data As Date, _
                                ByVal valore_ch1 As Single, ByVal valore_ch2 As Single, ByVal valore_temperatura As Single, _
                                ByVal flusso As Boolean, _
                                ByVal lev_hcl As Boolean, _
                                ByVal lev_naclo2 As Boolean, ByVal lev_k6 As Boolean, ByVal temp_max As Boolean, ByVal stop_l As Boolean, _
                                                                ByVal lev_errata As Boolean) As Boolean

        Dim data_table As log_db.ltb_logDataTable

        Dim count_row As Integer
        'log_wd_puntatore.Connection = objConn
        data_table = log_ltb_puntatore.GetData(data, valore_ch1, valore_ch2, valore_temperatura, flusso, lev_hcl, lev_naclo2, lev_k6, temp_max, stop_l, lev_errata, identificativo, id_strumento)
        count_row = data_table.Count
        If count_row = 0 Then
            log_ltb_puntatore.InsertQuery(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_temperatura, flusso, lev_hcl, lev_naclo2, lev_k6, temp_max, stop_l, lev_errata)

            Return True
        Else
            'log_wd_remove(identificativo, id_strumento)
            Return False

        End If
        Return True
    End Function

    Public Function log_wh(ByVal identificativo As String, ByVal id_strumento As String, ByVal data As Date, _
                                ByVal valore_ch1 As Single, ByVal valore_ch2 As Single, _
                                ByVal valore_flow As Boolean, _
                                ByVal allarmed2_val As Boolean, _
                                ByVal allarmed1_val As Boolean, ByVal allarmep1_val As Boolean, ByVal allarmep2_val As Boolean, ByVal livello1_val As Boolean, _
                                ByVal livello2_val As Boolean, ByVal timer_filtro As Boolean, ByVal timer_sonda_ph As Boolean, ByVal timer_sonda_mv As Boolean, _
                                ByVal timer_pagamento As Boolean, ByVal timer_manutenzione As Boolean, ByVal stato_pulse1_ch1 As Integer, ByVal stato_pulse2_ch1 As Integer, _
                                ByVal stato_pulse_ch2 As Integer, ByVal stato_rele_ch1 As Boolean, ByVal stato_rele_ch2 As Boolean, _
                                ByVal stby As Boolean) As Boolean

        Dim data_table As log_db.wh_logDataTable

        Dim count_row As Integer
        'log_wd_puntatore.Connection = objConn
        data_table = log_wh_puntatore.GetData(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_flow, allarmed2_val, allarmed1_val, allarmep1_val, allarmep2_val, livello1_val, livello2_val, timer_filtro _
                                              , timer_sonda_ph, timer_sonda_mv, timer_pagamento, timer_manutenzione, stato_pulse1_ch1, _
                                              stato_pulse2_ch1, stato_pulse_ch2, stato_rele_ch1, stato_rele_ch2, stby)
        count_row = data_table.Count
        If count_row = 0 Then
            log_wh_puntatore.InsertQuery(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_flow, allarmed2_val, allarmed1_val, allarmep1_val, allarmep2_val, livello1_val, livello2_val, _
                                         timer_filtro, timer_sonda_ph, timer_sonda_mv, timer_pagamento, timer_manutenzione, _
                                         stato_pulse1_ch1, stato_pulse2_ch1, stato_pulse_ch2, stato_rele_ch1, stato_rele_ch2, stby)
            Return True
        Else
            'log_wd_remove(identificativo, id_strumento)
            Return False

        End If
        Return True
    End Function

    Public Sub log_wd_remove(ByVal identificativo As String, ByVal id_strumento As String)
        Try
            Dim data_table1 As log_db.wd_log_tamponeDataTable
            data_table1 = log_wd_puntatore_tampone.GetData(identificativo, id_strumento)
            For Each dc In data_table1
                'log_wd_puntatore.InsertQuery(identificativo, id_strumento, dc.data, dc.valore1, dc.valore2, dc.flusso, dc.dos_alarm_cl, dc.dos_alarm_ph, dc.probe_fail_ph, dc.probe_fail_cl, dc.livello1, dc.livello2)
            Next
            log_wd_puntatore_tampone.DeleteQuery(identificativo, id_strumento)

        Catch ex As Exception

        End Try

    End Sub
    Public Function log_tower(ByVal identificativo As String, ByVal id_strumento As String, ByVal data As Date, _
                            ByVal valore_ch1 As Single, ByVal valore_ch2 As Single, ByVal valore_ch3 As Single, _
                            ByVal temperatura_val As Single, _
                            ByVal cd_high_val As String, ByVal cd_low_val As String, ByVal bleed_timeout_val As String, _
                            ByVal flow_val As String, ByVal inhib_lev_val As String, ByVal prebio1_lev_val As String, ByVal prebio2_lev_val As String, _
                            ByVal bio1_lev_val As String, ByVal bio2_lev_val As String, ByVal ch2_high_val As String, ByVal ch2_low_val As String, _
                            ByVal ch2_level_val As String, ByVal ch3_high_val As String, ByVal ch3_low_val As String, ByVal ch3_level_val As String, _
                            ByVal tot_input_val As String, ByVal tot_bleed_val As String, ByVal out_inibitore_digital_val As String, ByVal out_inibitore_prop_val As String, ByVal out_bleed_val As String, ByVal out_prebiocide1_val As String, _
                                            ByVal out_prebiocide2_val As String, ByVal out_biocide1_val As String, ByVal out_biocide2_val As String, ByVal ch2_prop_val As String, ByVal ch2_dig_val As String, ByVal ch3_prop_val As String, ByVal ch3_dig_val As String) As Boolean

        Dim data_table As log_db.log_towerDataTable
        Dim data_table1 As log_db.log_tower_tamponeDataTable
        Dim count_row As Integer = 0
        'log_tower_puntatore.Connection = objConn
        'data_table = log_tower_puntatore.GetData(identificativo, id_strumento, data, _
        '                                     cd_high_val, cd_low_val, bleed_timeout_val, flow_val, inhib_lev_val, _
        '                                    prebio1_lev_val, prebio2_lev_val, bio1_lev_val, bio2_lev_val, ch2_high_val, ch2_low_val, ch2_level_val, ch3_high_val, ch3_low_val, _
        '                                    ch3_level_val)
        'count_row = data_table.Count
        For Each dc1 In log_tower_puntatore.GetData(identificativo, id_strumento, data, _
                                             cd_high_val, cd_low_val, bleed_timeout_val, flow_val, inhib_lev_val, _
                                            prebio1_lev_val, prebio2_lev_val, bio1_lev_val, bio2_lev_val, ch2_high_val, ch2_low_val, ch2_level_val, ch3_high_val, ch3_low_val, _
                                            ch3_level_val)
            count_row = dc1.Expr1
            Exit For
        Next
        If count_row = 0 Then
            log_tower_puntatore.InsertQuery(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_ch3, _
                                            temperatura_val, cd_high_val, cd_low_val, bleed_timeout_val, flow_val, inhib_lev_val, _
                                            prebio1_lev_val, prebio2_lev_val, bio1_lev_val, bio2_lev_val, ch2_high_val, ch2_low_val, ch2_level_val, ch3_high_val, ch3_low_val, _
                                            ch3_level_val, tot_input_val, tot_bleed_val, _
                                            out_inibitore_digital_val, out_inibitore_prop_val, out_bleed_val, out_prebiocide1_val, _
                                            out_prebiocide2_val, out_biocide1_val, out_biocide2_val, ch2_prop_val, ch2_dig_val, ch3_prop_val, ch3_dig_val)

            Return True
        Else
            'log_tower_remove(identificativo, id_strumento)
            Return False
        End If
        Return True
    End Function
    Public Sub log_tower_remove(ByVal identificativo As String, ByVal id_strumento As String)
        Try
            'Dim data_table1 As log_db.log_tower_tamponeDataTable
            'data_table1 = log_tower_puntatore_tampone.GetData(identificativo, id_strumento)
            'For Each dc In data_table1
            '    log_tower_puntatore.InsertQuery(identificativo, id_strumento, dc.data, dc.valore1, dc.valore2, dc.valore3, dc.temperatura, dc.cd_high, dc.cd_low, dc.bleed_timeout, dc.flow, dc.level_inhibitor, dc.level_prebiocide1, dc.level_prebiocide2, dc.level_biocide1, dc.level_biocide2, dc.ch2_high, dc.ch2_low, dc.ch2_level, dc.ch3_high, dc.ch3_low, dc.ch3_level, dc.tot_input, dc.tot_bleed)
            'Next
            'log_tower_puntatore_tampone.DeleteQuery(identificativo, id_strumento)

        Catch ex As Exception

        End Try

    End Sub
End Class