Public Class light
    Private mtipo_strumento As String
    Public mtipo_strumento_bool As Boolean
    Private mnome_strumento As String
    Private mvalue1 As String
    Public mvalue1_bool As Boolean
    Private mvalue2 As String
    Private mvalue2_1 As String
    Public mvalue2_bool As Boolean
    Private mvalue3 As String
    Public mvalue3_bool As Boolean
    Private mvalue4 As String
    Public mvalue4_bool As Boolean
    Private mvalue5 As String
    Public mvalue5_bool As Boolean
    Private mvalue6 As String
    Public mvalue6_bool As Boolean
    Private mvalue6_1 As String
    Private mvalue7 As String
    Public mvalue7_bool As Boolean
    Private mvalue8 As String
    Public mvalue8_bool As Boolean
    Private mvalue9 As String
    Public mvalue9_bool As Boolean
    Private mvalue10 As String
    Public mvalue10_bool As Boolean
    Private mvalue11 As String
    Public mvalue11_bool As Boolean
    Private mvalue12 As String
    Public mvalue12_bool As Boolean
    Private mvalue13 As String
    Public mvalue13_bool As Boolean
    Private mvalue14 As String
    Public mvalue14_bool As Boolean
    Private mvalue15 As String
    Public mvalue15_bool As Boolean
    Private mvalue16 As String
    Public mvalue16_bool As Boolean
    Private mvalue17 As String
    Public mvalue17_bool As Boolean
    Private mvalue18 As String
    Public mvalue18_bool As Boolean
    Private mvalue19 As String
    Public mvalue19_bool As Boolean
    Private mvalue20 As String
    Public mvalue20_bool As Boolean
    Private mvalue21 As String
    Public mvalue21_bool As Boolean
    Private mvalue22 As String
    Public mvalue22_bool As Boolean
    Private mvalue23 As String
    Public mvalue23_bool As Boolean
    Private mvalue24 As String
    Public mvalue24_bool As Boolean
    Private mvalue25 As String
    Public mvalue25_bool As Boolean
    Private mvalue26 As String
    Public mvalue26_bool As Boolean
    Private mvalue27 As String
    Public mvalue27_bool As Boolean
    Private mvalue28 As String
    Public mvalue28_bool As Boolean
    Private mvalue29 As String
    Public mvalue29_bool As Boolean
    Private mvalue30 As String
    Public mvalue30_bool As Boolean
    Private mvalue31 As String
    Public mvalue31_bool As Boolean
    Private mvalue32 As String
    Public mvalue32_bool As Boolean

    Private mvalue33 As String
    Public mvalue33_bool As Boolean

    Private mvalue34 As String
    Public mvalue34_bool As Boolean


    Public update_setpoint1 As Boolean
    Public update_setpoint2 As Boolean
    Public update_setpoint3 As Boolean
    Public update_setpoint4 As Boolean
    Public update_setpoint5 As Boolean
    Public update_setpoint6 As Boolean
    Public update_setpoint7 As Boolean
    Public update_setpoint8 As Boolean
    Public update_setpoint9 As Boolean
    Public update_setpoint10 As Boolean
    Public update_setpoint11 As Boolean
    Public update_setpoint12 As Boolean
    Public update_setpoint13 As Boolean
    Public update_setpoint14 As Boolean
    Public update_setpoint15 As Boolean
    Public update_setpoint16 As Boolean
    Public update_setpoint17 As Boolean
    Public update_setpoint18 As Boolean
    Public update_setpoint19 As Boolean
    Public update_setpoint20 As Boolean
    Public update_setpoint21 As Boolean
    Public update_setpoint22 As Boolean
    Public update_setpoint23 As Boolean
    Public update_setpoint24 As Boolean
    Public update_setpoint25 As Boolean
    Public update_setpoint26 As Boolean
    Public update_setpoint27 As Boolean
    Public update_setpoint28 As Boolean
    Public update_setpoint29 As Boolean
    Public update_setpoint30 As Boolean
    Public update_setpoint31 As Boolean
    Public update_setpoint32 As Boolean
    Public update_setpoint33 As Boolean
    Public update_setpoint34 As Boolean

    Public mvalue_log_ldma_bool As Boolean
    Public reading_log As Boolean

    Private mvalue_change As String
    Public mvalue_change_bool As Boolean

    Private time_strumento As DateTime
    Public factor_divide_ch1 As Integer
    Public factor_divide_ch2 As Integer
    Public factor_divide_ch3 As Integer
    Public factor_divide_ch4 As Integer
    Public factor_divide_ch5 As Integer
    Public full_scale_ch1 As Integer
    Public full_scale_ch2 As Integer
    Public full_scale_ch3 As Integer
    Public full_scale_ch4 As Integer
    Public full_scale_ch5 As Integer
    Public value_ch1 As Single
    Public value_ch2 As Single
    Public value_ch3 As Single
    Public value_ch4 As Single
    Public value_ch5 As Single

    Public ma1_enable As Boolean
    Public ma2_enable As Boolean
    Public ma3_enable As Boolean
    Public ma4_enable As Boolean
    Public ma5_enable As Boolean


    Private menabled_log As Boolean
    Public id_strumento As String
    Public allarme_flusso As Boolean
    Public start_data_flusso As Date
    Public current_data_flusso As Date
    Public time_no_flow As Long
    Public type_tower As Integer
    Public yagel_version As Boolean
    Public new_version As Boolean
    Public mail_csv_setting As Boolean
    Public number_version_total As Integer
    Public tower_induttiva As Boolean
    Public disable_comp_ph As Boolean
    Public format_temperatura As String = ""

    Public towerVersione As Integer = 0

    Public list_10_log As List(Of String)
    Public first_log As Boolean
    Public counter_log As Integer

    Public change_enabled As Boolean



    Public mBoiler As Boolean
    Sub New()
        mtipo_strumento = ""
        mnome_strumento = ""
        towerVersione = 0
        mtipo_strumento_bool = False
        update_setpoint1 = update_setpoint2 = update_setpoint3 = update_setpoint4 = update_setpoint5 = update_setpoint6 = update_setpoint7 = update_setpoint8 = update_setpoint9 = update_setpoint10 = False
        update_setpoint11 = update_setpoint12 = update_setpoint13 = update_setpoint14 = update_setpoint15 = update_setpoint16 = update_setpoint17 = update_setpoint18 = update_setpoint19 = update_setpoint20 = False
        update_setpoint21 = update_setpoint22 = update_setpoint23 = update_setpoint24 = update_setpoint25 = update_setpoint26 = update_setpoint27 = update_setpoint28 = update_setpoint29 = update_setpoint30 = update_setpoint31 = update_setpoint32 = update_setpoint33 = update_setpoint34 = False
        mvalue1 = ""
        mvalue1_bool = False
        mvalue2 = ""
        mvalue2_bool = False
        mvalue3 = ""
        mvalue3_bool = False
        mvalue4 = ""
        mvalue4_bool = False
        mvalue5 = ""
        mvalue5_bool = False
        mvalue6 = ""
        mvalue6_bool = False
        mvalue7 = ""
        mvalue7_bool = False
        mvalue8 = ""
        mvalue8_bool = False
        mvalue9 = ""
        mvalue9_bool = False
        mvalue10 = ""
        mvalue10_bool = False
        mvalue11 = ""
        mvalue11_bool = False
        mvalue12 = ""
        mvalue12_bool = False
        mvalue13 = ""
        mvalue13_bool = False
        mvalue14 = ""
        mvalue14_bool = False
        mvalue15 = ""
        mvalue15_bool = False
        mvalue16 = ""
        mvalue16_bool = False
        mvalue17 = ""
        mvalue17_bool = False
        mvalue18 = ""
        mvalue18_bool = False
        mvalue19 = ""
        mvalue19_bool = False
        mvalue20 = ""
        mvalue20_bool = False
        mvalue21 = ""
        mvalue21_bool = False
        mvalue22 = ""
        mvalue22_bool = False
        mvalue23 = ""
        mvalue23_bool = False
        mvalue24 = ""
        mvalue24_bool = False
        mvalue25 = ""
        mvalue25_bool = False
        mvalue26 = ""
        mvalue26_bool = False
        mvalue27 = ""
        mvalue27_bool = False
        mvalue28 = ""
        mvalue28_bool = False
        mvalue29 = ""
        mvalue29_bool = False
        mvalue30 = ""
        mvalue30_bool = False
        mvalue31 = ""
        mvalue31_bool = False
        mvalue32 = ""
        mvalue32_bool = False

        mvalue33 = ""
        mvalue33_bool = False
        mvalue34 = ""
        mvalue34_bool = False

        mvalue_change = ""
        mvalue_change_bool = False
        factor_divide_ch1 = 0
        factor_divide_ch2 = 0
        factor_divide_ch3 = 0
        factor_divide_ch4 = 0
        factor_divide_ch5 = 0
        full_scale_ch1 = 0
        full_scale_ch2 = 0
        full_scale_ch3 = 0
        full_scale_ch4 = 0
        full_scale_ch5 = 0
        id_strumento = ""
        type_tower = 0
        yagel_version = False
        new_version = False
        change_enabled = False

        mvalue_log_ldma_bool = True
        reading_log = False

        mail_csv_setting = False

        format_temperatura = "0"

        time_no_flow = 0

        value_ch1 = 0
        value_ch2 = 0
        value_ch3 = 0
        value_ch4 = 0
        value_ch5 = 0
        tower_induttiva = False
        number_version_total = 0
        ma1_enable = False
        ma2_enable = False
        ma3_enable = False
        ma4_enable = False
        ma5_enable = False
        disable_comp_ph = False
        first_log = True
        counter_log = 0
        list_10_log = New List(Of String)
        mBoiler = False
    End Sub
    Public Property enabled_log() As Boolean
        Get
            Return menabled_log
        End Get
        Set(ByVal value As Boolean)
            menabled_log = value
        End Set
    End Property

    Public Property tipo_strumento() As String
        'proprieta che contiene le info iniziali dello strumento
        '01emzc82#versione#etc
        Get
            Return mnome_strumento
        End Get
        Set(ByVal value As String)
            'If InStr(value, "log") <> 0 Then
            '    Exit Property
            'End If
            Dim nessuno = True
            If InStr(value, "#max5#") <> 0 Then 'risposta max 5
                Dim str_version As String = get_str_version(value)
                If InStr(str_version, "Y") <> 0 Then
                    yagel_version = True
                End If
                Dim str_version_split() As String = str_version.Split(".")
                Dim number_version As Integer
                Try
                    number_version = Val(str_version_split(0) + str_version_split(1))
                    number_version_total = Val(str_version_split(0) + str_version_split(1) + str_version_split(2))
                Catch ex As Exception
                    number_version = 0
                End Try
                If number_version > 29 Then
                    new_version = True
                    change_enabled = True
                Else
                    new_version = False
                    change_enabled = False
                End If
                nessuno = False
                mnome_strumento = "max5"
            End If
            If InStr(value, "#Boiler#") <> 0 Then 'risposta Tower
                Dim str_version As String = get_str_version(value)
                If InStr(str_version, "305") Or InStr(str_version, "306") Or InStr(str_version, "307") Or
                    InStr(str_version, "283") Or InStr(str_version, "284") Or InStr(str_version, "285") Then

                    new_version = True
                    change_enabled = True
                End If
                nessuno = False
                mnome_strumento = "Tower"
                mBoiler = True
            End If
            If InStr(value, "#LDTower#") <> 0 Then 'risposta Tower
                new_version = True
                change_enabled = True
                nessuno = False
                mnome_strumento = "LDtower"
                mail_csv_setting = True
            End If
            If InStr(value, "#Tower#") <> 0 Then 'risposta Tower
                Dim str_version As String = get_str_version(value)
                If Val(str_version) >= 283 And Val(str_version) < 300 Then
                    new_version = True
                    change_enabled = True

                End If
                If Val(str_version) >= 300 And Val(str_version) < 400 Then
                    new_version = True
                    change_enabled = True

                End If

                If Val(str_version) >= 285 And Val(str_version) < 300 Then
                    new_version = True
                    change_enabled = True
                    mail_csv_setting = True
                End If
                If Val(str_version) >= 307 And Val(str_version) < 400 Then
                    new_version = True
                    change_enabled = True
                    mail_csv_setting = True
                End If
                If Val(str_version) >= 700 Then
                    new_version = True
                    change_enabled = True
                End If
                If Val(str_version) >= 704 And Val(str_version) < 800 Then
                    mail_csv_setting = True
                    towerVersione = Val(str_version)
                End If
                If Val(str_version) >= 804 Then
                    mail_csv_setting = True
                    towerVersione = Val(str_version)
                End If

                'If InStr(str_version, "305") Or InStr(str_version, "306") Or InStr(str_version, "307") Or _
                '    InStr(str_version, "283") Or InStr(str_version, "284") Or InStr(str_version, "285") Then

                'End If
                'If InStr(str_version, "308") Or _
                '    InStr(str_version, "286") Or _
                '    InStr(str_version, "309") Or _
                '    InStr(str_version, "287") Then

                '    new_version = True
                '    change_enabled = True

                '    mail_csv_setting = True
                'End If

                nessuno = False
                mnome_strumento = "Tower"
            End If
            If InStr(value, "#LD#") <> 0 Or InStr(value, "#LDDT#") <> 0 Then 'risposta LD
                Dim str_version As String = get_str_version(value)
                If Val(str_version) >= 420 Then
                    new_version = True
                    change_enabled = True
                End If
                nessuno = False
                mnome_strumento = "LD"
            End If
            If InStr(value, "#LD4#") <> 0 Then 'risposta LD
                Dim str_version As String = get_str_version(value)
                If Val(str_version) >= 420 Then
                    new_version = True
                    change_enabled = True
                End If
                nessuno = False
                mnome_strumento = "LD4"
            End If
            If InStr(value, "#LDS#") <> 0 Then 'risposta LDS
                Dim str_version As String = get_str_version(value)
                If Val(str_version) >= 420 Then
                    new_version = True
                    change_enabled = True
                End If
                nessuno = False
                mnome_strumento = "LDS"
            End If
            If InStr(value, "#WD#") <> 0 Then 'risposta WD
                Dim str_version As String = get_str_version(value)
                If Val(str_version) >= 140 Then
                    new_version = True
                    change_enabled = True
                End If
                nessuno = False
                mnome_strumento = "WD"
            End If
            If InStr(value, "#WH#") <> 0 Then 'risposta WD
                Dim str_version As String = get_str_version(value)
                If Val(str_version) >= 140 Then
                    new_version = True
                    change_enabled = True
                End If
                nessuno = False
                mnome_strumento = "WH"
            End If
            If InStr(value, "#LTB#") <> 0 Then
                Dim str_version As String = get_str_version(value)
                If Val(str_version) >= 115 Then
                    new_version = True
                    change_enabled = True
                End If
                nessuno = False
                mnome_strumento = "LTB"
            End If
            If InStr(value, "#LTA#") <> 0 Then
                Dim str_version As String = get_str_version(value)
                new_version = True
                change_enabled = True
                nessuno = False
                mnome_strumento = "LTA"
            End If
            If InStr(value, "#LTU#") <> 0 Then
                Dim str_version As String = get_str_version(value)
                new_version = True
                change_enabled = True
                nessuno = False
                mnome_strumento = "LTU"
            End If
            If InStr(value, "#LTD#") <> 0 Then
                Dim str_version As String = get_str_version(value)
                new_version = True
                change_enabled = True
                nessuno = False
                mnome_strumento = "LTD"
            End If

            If InStr(value, "#LDMA#") <> 0 Then 'risposta WD
                'Dim str_version As String = get_str_version(value)
                'If Val(str_version) >= 100 Then
                new_version = True
                change_enabled = True
                'End If
                nessuno = False
                mnome_strumento = "LDMA"
            End If
            If InStr(value, "#LDLG#") <> 0 Then 'risposta WD
                'Dim str_version As String = get_str_version(value)
                'If Val(str_version) >= 100 Then
                new_version = True
                change_enabled = True
                'End If
                nessuno = False
                mnome_strumento = "LDLG"
            End If

            If nessuno Then
                mnome_strumento = "max5"
            End If
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                If nessuno Then
                    data(2) = "01&##max5#2.2.3#okem"
                End If
                value = data(2)
            End If
            Try
                id_strumento = get_str_id(data(2))
                mtipo_strumento = value
                mtipo_strumento_bool = True
            Catch ex As Exception

            End Try
        End Set
    End Property
    Public Function get_str_version(ByVal input As String) As String
        Dim id_value() As String = input.Split("&")
        If id_value.Length > 1 Then
            Dim id_name() As String = id_value(1).Split("#")
            If id_name.Length > 1 Then
                Try
                    Return id_name(3)
                Catch ex As Exception
                    Return ""
                End Try
            End If
        End If
        Return ""
    End Function
    Public Property nome_strumento() As String
        'proprieta che contiene le info iniziali dello strumento
        '01emzc82#versione#etc
        Get
            Return mtipo_strumento
        End Get
        Set(ByVal value As String)
            If InStr(value, "log") <> 0 Then
                Exit Property
            End If
            mtipo_strumento = value
        End Set
    End Property
    Public Property value1() As String
        'proprieta che contine(bc per max 5, unitsr per tower config per LD e LDS)
        Get
            Return mvalue1
        End Get
        Set(ByVal value As String)
            If InStr(value, "log") <> 0 Then
                Exit Property
            End If
            Dim data() As String = value.Split("|")

            Select Case mnome_strumento
                Case "max5"
                    If data.Length > 2 Then
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                enable_scale(data1(1))
                            End If
                        Catch ex As Exception

                        End Try
                    End If

            End Select
            Try
                If InStr(data(2), "end") <> 0 Then
                    value = data(2)
                    If value = mvalue1 Then

                    End If
                    mvalue1 = value
                    mvalue1_bool = True
                End If
            Catch ex As Exception

            End Try
        End Set
    End Property
    Public Property value2() As String
        'proprieta che contine(ab per max 5, valuer per tower valuer per LD e LDS
        Get
            Return mvalue2
        End Get
        Set(ByVal value As String)
            If InStr(value, "log") <> 0 Then
                Exit Property
            End If
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                If InStr(data(2), "end") <> 0 Then
                    value = data(2)
                Else
                    Exit Property
                End If

                Select Case mnome_strumento
                    Case "max5"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                value_ch1 = Val(Mid(data_temp(0), 3, 4)) / factor_divide_ch1
                                value_ch2 = Val(Mid(data_temp(1), 3, 4)) / factor_divide_ch2
                                value_ch3 = Val(Mid(data_temp(2), 3, 4)) / factor_divide_ch3
                                value_ch4 = Val(Mid(data_temp(3), 3, 4)) / factor_divide_ch4
                                value_ch5 = Val(Mid(data_temp(4), 3, 4)) / factor_divide_ch5
                                If Val(Mid(data_temp(10), 3, 2)) >= 1 Then 'allarme di flusso
                                    If allarme_flusso = False Then
                                        time_no_flow = 1
                                        start_data_flusso = Now
                                        current_data_flusso = Now
                                        allarme_flusso = True
                                    Else
                                        current_data_flusso = Now
                                        time_no_flow = DateDiff(DateInterval.Minute, start_data_flusso, current_data_flusso)
                                    End If
                                Else
                                    time_no_flow = 0
                                    allarme_flusso = False
                                End If
                            End If
                        Catch ex As Exception

                        End Try
                    Case "LD"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                value_ch1 = Val(Mid(data_temp(1), 1, 4)) / factor_divide_ch1
                                value_ch2 = Val(Mid(data_temp(2), 1, 4)) / factor_divide_ch2
                                If data_temp.Length > 5 Then ' terzo canale
                                    value_ch3 = Val(Mid(data_temp(5), 1, 4)) / factor_divide_ch3
                                    format_temperatura = data_temp(4)
                                Else
                                    format_temperatura = data_temp(4)
                                    value_ch3 = 0
                                End If
                            End If
                        Catch ex As Exception

                        End Try
                    Case "LD4"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                value_ch1 = Val(Mid(data_temp(1), 1, 4)) / factor_divide_ch1
                                value_ch2 = Val(Mid(data_temp(2), 1, 4)) / factor_divide_ch2
                                value_ch3 = Val(Mid(data_temp(3), 1, 4)) / factor_divide_ch3
                                value_ch4 = Val(Mid(data_temp(4), 1, 4)) / factor_divide_ch4
                            End If
                        Catch ex As Exception

                        End Try
                    Case "LDS"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                value_ch1 = Val(Mid(data_temp(1), 1, 4)) / factor_divide_ch1
                            End If
                        Catch ex As Exception

                        End Try
                    Case "WD"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                value_ch1 = Val(Mid(data_temp(1), 1, 4)) / factor_divide_ch1
                                value_ch2 = Val(Mid(data_temp(2), 1, 4)) / factor_divide_ch2
                            End If
                        Catch ex As Exception

                        End Try
                    Case "WH"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                value_ch1 = Val(Mid(data_temp(1), 1, 4)) / factor_divide_ch1
                                value_ch2 = Val(Mid(data_temp(2), 1, 4)) / factor_divide_ch2
                            End If
                        Catch ex As Exception

                        End Try
                    Case "LTA"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                value_ch1 = Val(Mid(data_temp(1), 1, 4)) / factor_divide_ch1
                                value_ch2 = Val(Mid(data_temp(2), 1, 4)) / factor_divide_ch2
                            End If
                        Catch ex As Exception

                        End Try

                    Case "LTD"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                value_ch1 = Val(Mid(data_temp(1), 1, 4)) / factor_divide_ch1
                                value_ch2 = Val(Mid(data_temp(2), 1, 4)) / factor_divide_ch2
                            End If
                        Catch ex As Exception

                        End Try

                    Case "LTU"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                value_ch1 = Val(Mid(data_temp(1), 1, 4)) / factor_divide_ch1
                                value_ch2 = Val(Mid(data_temp(2), 1, 4)) / factor_divide_ch2
                            End If
                        Catch ex As Exception

                        End Try

                    Case "LTB"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                value_ch1 = Val(Mid(data_temp(1), 1, 4)) / factor_divide_ch1
                                value_ch2 = Val(Mid(data_temp(2), 1, 4)) / factor_divide_ch2
                            End If
                        Catch ex As Exception

                        End Try
                    Case "LDtower"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                value_ch1 = Val(Mid(data_temp(0), 3, 4)) / factor_divide_ch1
                                'If tower_induttiva Then
                                '    value_ch1 = value_ch1 * 10
                                'End If
                            End If
                        Catch ex As Exception

                        End Try
                    Case "Tower"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                Select Case type_tower
                                    Case 0 '1 canale
                                        value_ch1 = Val(Mid(data_temp(0), 3, 4)) / factor_divide_ch1
                                        If tower_induttiva Then
                                            value_ch1 = value_ch1 * 10
                                        End If
                                    Case 1 '2 canali
                                        value_ch1 = Val(Mid(data_temp(0), 3, 4)) / factor_divide_ch1
                                        If tower_induttiva Then
                                            value_ch1 = value_ch1 * 10
                                        End If
                                        value_ch2 = Val(Mid(data_temp(0), 11, 4)) / factor_divide_ch2
                                    Case 2 '3 canali
                                        value_ch1 = Val(Mid(data_temp(0), 3, 4)) / factor_divide_ch1
                                        If tower_induttiva Then
                                            value_ch1 = value_ch1 * 10
                                        End If
                                        value_ch2 = Val(Mid(data_temp(0), 11, 4)) / factor_divide_ch2
                                        value_ch3 = Val(Mid(data_temp(0), 15, 4)) / factor_divide_ch3
                                End Select
                                value_ch1 = Val(Mid(data_temp(1), 1, 4)) / factor_divide_ch1
                                value_ch2 = Val(Mid(data_temp(2), 1, 4)) / factor_divide_ch2
                            End If
                        Catch ex As Exception

                        End Try

                End Select
                mvalue2_bool = True

                'If value = mvalue2 Then
                'mvalue_update = False
                'End If

                mvalue2 = value
            End If
        End Set
    End Property
    Public Property value2_1() As String
        'proprieta che contine(ab per max 5, valuer per tower valuer per LD e LDS
        Get
            Return mvalue2_1
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                If InStr(data(2), "end") <> 0 Then
                    value = data(2)
                Else
                    Exit Property
                End If

            End If

            'If value = mvalue2_1 Then
            'mvalue_update = False
            'End If

            mvalue2_1 = value
        End Set
    End Property

    Public Property value3() As String
        'proprieta che contine(ba per max 5, algenr per tower allrnr per LD e LDS
        Get
            Return mvalue3
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                If InStr(data(2), "end") <> 0 Then
                    value = data(2)
                Else
                    Exit Property
                End If

                Select Case mnome_strumento
                    Case "LD"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                If Val(Mid(data_temp(4), 1, 1)) >= 1 Then 'allarme di flusso
                                    If allarme_flusso = False Then
                                        time_no_flow = 1
                                        start_data_flusso = Now
                                        current_data_flusso = Now
                                        allarme_flusso = True
                                    Else
                                        current_data_flusso = Now
                                        time_no_flow = DateDiff(DateInterval.Minute, start_data_flusso, current_data_flusso)
                                    End If
                                Else
                                    time_no_flow = 0
                                    allarme_flusso = False
                                End If
                            End If
                        Catch ex As Exception

                        End Try
                    Case "LD4"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                If Val(Mid(data_temp(4), 1, 1)) >= 1 Then 'allarme di flusso
                                    If allarme_flusso = False Then
                                        time_no_flow = 1
                                        start_data_flusso = Now
                                        current_data_flusso = Now
                                        allarme_flusso = True
                                    Else
                                        current_data_flusso = Now
                                        time_no_flow = DateDiff(DateInterval.Minute, start_data_flusso, current_data_flusso)
                                    End If
                                Else
                                    time_no_flow = 0
                                    allarme_flusso = False
                                End If
                            End If
                        Catch ex As Exception

                        End Try
                    Case "LDS"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                If Val(Mid(data_temp(2), 1, 1)) >= 1 Then 'allarme di flusso
                                    If allarme_flusso = False Then
                                        time_no_flow = 1
                                        start_data_flusso = Now
                                        current_data_flusso = Now
                                        allarme_flusso = True
                                    Else
                                        current_data_flusso = Now
                                        time_no_flow = DateDiff(DateInterval.Minute, start_data_flusso, current_data_flusso)
                                    End If
                                Else
                                    time_no_flow = 0
                                    allarme_flusso = False
                                End If
                            End If
                        Catch ex As Exception

                        End Try
                    Case "WD"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                If Val(Mid(data_temp(3), 1, 1)) >= 1 Then 'allarme di flusso
                                    If allarme_flusso = False Then
                                        time_no_flow = 1
                                        start_data_flusso = Now
                                        current_data_flusso = Now
                                        allarme_flusso = True
                                    Else
                                        current_data_flusso = Now
                                        time_no_flow = DateDiff(DateInterval.Minute, start_data_flusso, current_data_flusso)
                                    End If
                                Else
                                    time_no_flow = 0
                                    allarme_flusso = False
                                End If
                            End If
                        Catch ex As Exception

                        End Try
                    Case "LTU"
                    Case "LTD"
                    Case "LTA"
                    Case "LTB"
                    Case "WH"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                If Val(Mid(data_temp(3), 1, 1)) >= 1 Then 'allarme di flusso
                                    If allarme_flusso = False Then
                                        time_no_flow = 1
                                        start_data_flusso = Now
                                        current_data_flusso = Now
                                        allarme_flusso = True
                                    Else
                                        current_data_flusso = Now
                                        time_no_flow = DateDiff(DateInterval.Minute, start_data_flusso, current_data_flusso)
                                    End If
                                Else
                                    time_no_flow = 0
                                    allarme_flusso = False
                                End If
                            End If
                        Catch ex As Exception

                        End Try
                    Case "LDtower"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                If Val(Mid(data_temp(0), 4, 1)) >= 1 Then 'allarme di flusso
                                    If allarme_flusso = False Then
                                        time_no_flow = 1
                                        start_data_flusso = Now
                                        current_data_flusso = Now
                                        allarme_flusso = True
                                    Else
                                        current_data_flusso = Now
                                        time_no_flow = DateDiff(DateInterval.Minute, start_data_flusso, current_data_flusso)
                                    End If
                                Else
                                    time_no_flow = 0
                                    allarme_flusso = False
                                End If
                            End If
                        Catch ex As Exception

                        End Try
                    Case "Tower"
                        Dim data1() As String = data(2).Split("&")
                        Try
                            If data1.Length > 0 Then
                                Dim data_temp() As String = data1(1).Split("#")
                                If Val(Mid(data_temp(0), 4, 1)) >= 1 Then 'allarme di flusso
                                    If allarme_flusso = False Then
                                        time_no_flow = 1
                                        start_data_flusso = Now
                                        current_data_flusso = Now
                                        allarme_flusso = True
                                    Else
                                        current_data_flusso = Now
                                        time_no_flow = DateDiff(DateInterval.Minute, start_data_flusso, current_data_flusso)
                                    End If
                                Else
                                    time_no_flow = 0
                                    allarme_flusso = False
                                End If
                            End If
                        Catch ex As Exception

                        End Try

                End Select
                If data(1) = "747353" Then
                    mvalue3_bool = True
                End If
                mvalue3_bool = True

                'If value = mvalue3 Then
                '    mvalue_update = False
                'End If

                mvalue3 = value
            End If
        End Set
    End Property
    Public Property value4() As String
        'proprieta che contine( b5 per max 5 config per Tower clibrz per LD e LDS)
        Get
            Return mvalue4
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                If InStr(data(2), "end") <> 0 Then
                Else
                    Exit Property
                End If
                Select Case mnome_strumento
                    Case "LD"
                        If data.Length > 2 Then
                            Dim data1() As String = data(2).Split("&")
                            Try
                                If data1.Length > 0 Then
                                    enable_scale_ld(data1(1))
                                End If
                            Catch ex As Exception

                            End Try
                        End If
                    Case "LD4"
                        If data.Length > 2 Then
                            Dim data1() As String = data(2).Split("&")
                            Try
                                If data1.Length > 0 Then
                                    enable_scale_ld4(data1(1))
                                End If
                            Catch ex As Exception

                            End Try
                        End If

                    Case "LDS"
                        If data.Length > 2 Then
                            Dim data1() As String = data(2).Split("&")
                            Try
                                If data1.Length > 0 Then
                                    enable_scale_lds(data1(1))
                                End If
                            Catch ex As Exception

                            End Try
                        End If
                    Case "WD"
                        If data.Length > 2 Then
                            Dim data1() As String = data(2).Split("&")
                            Try
                                If data1.Length > 0 Then
                                    enable_scale_ld(data1(1))
                                End If
                            Catch ex As Exception

                            End Try
                        End If
                    Case "WH"
                        If data.Length > 2 Then
                            Dim data1() As String = data(2).Split("&")
                            Try
                                If data1.Length > 0 Then
                                    enable_scale_ld(data1(1))
                                End If
                            Catch ex As Exception

                            End Try
                        End If
                    Case "LTU"
                        If data.Length > 2 Then
                            Dim data1() As String = data(2).Split("&")
                            Try
                                If data1.Length > 0 Then
                                    enable_scale_ld(data1(1))
                                End If
                            Catch ex As Exception

                            End Try
                        End If

                    Case "LTD"
                        If data.Length > 2 Then
                            Dim data1() As String = data(2).Split("&")
                            Try
                                If data1.Length > 0 Then
                                    enable_scale_ld(data1(1))
                                End If
                            Catch ex As Exception

                            End Try
                        End If
                    Case "LTA"
                        If data.Length > 2 Then
                            Dim data1() As String = data(2).Split("&")
                            Try
                                If data1.Length > 0 Then
                                    enable_scale_ld(data1(1))
                                End If
                            Catch ex As Exception

                            End Try
                        End If

                    Case "LTB"
                        If data.Length > 2 Then
                            Dim data1() As String = data(2).Split("&")
                            Try
                                If data1.Length > 0 Then
                                    enable_scale_ld(data1(1))
                                End If
                            Catch ex As Exception

                            End Try
                        End If
                    Case "LDtower"
                        If data.Length > 2 Then
                            Dim data1() As String = data(2).Split("&")
                            Try
                                If data1.Length > 0 Then
                                    type_tower = enable_scale_tower(data1(1))
                                    If type_tower = 0 Then
                                        Exit Property
                                    End If
                                    Dim data_temp() As String = data1(1).Split("#")
                                    If Mid(data_temp(0), data_temp(0).Length, 1) = "1" Then
                                        factor_divide_ch1 = 1
                                        full_scale_ch1 = 5000
                                    Else
                                        factor_divide_ch1 = 1
                                        full_scale_ch1 = 500
                                    End If
                                    Dim pippo As String = data_temp(0)
                                End If
                            Catch ex As Exception

                            End Try
                        End If

                    Case "Tower"
                        If data.Length > 2 Then
                            Dim data1() As String = data(2).Split("&")
                            Try
                                If data1.Length > 0 Then
                                    type_tower = enable_scale_tower(data1(1))
                                    If type_tower = 0 Then
                                        Exit Property
                                    End If
                                    Dim data_temp() As String = data1(1).Split("#")
                                    If Mid(data_temp(0), data_temp(0).Length, 1) = "1" Then
                                        tower_induttiva = True
                                    Else
                                        tower_induttiva = False
                                    End If
                                    Dim pippo As String = data_temp(0)
                                    If (InStr(data_temp(0), "MT01") Or InStr(data_temp(0), "M01")) And (InStr(data_temp(1), "MT02") Or InStr(data_temp(1), "M02")) And (InStr(data_temp(2), "MT03") Or InStr(data_temp(2), "M03")) And (InStr(data_temp(3), "MT05") Or InStr(data_temp(3), "M05")) Then 'Cd pH rH
                                        If (towerVersione >= 704) And (towerVersione < 800) Then
                                        Else
                                            If (towerVersione >= 804) And (towerVersione < 900) Then
                                            Else
                                                mail_csv_setting = False
                                            End If
                                        End If
                                    End If
                                    If (InStr(data_temp(0), "MT01") Or InStr(data_temp(0), "M01")) And (InStr(data_temp(1), "MT02") Or InStr(data_temp(1), "M02")) And (InStr(data_temp(2), "MT04") Or InStr(data_temp(2), "M04")) And (InStr(data_temp(3), "MT05") Or InStr(data_temp(3), "M05")) Then 'Cd pH Cl
                                        If (towerVersione >= 704) And (towerVersione < 800) Then
                                        Else
                                            If (towerVersione >= 804) And (towerVersione < 900) Then
                                            Else
                                                mail_csv_setting = False
                                            End If
                                        End If

                                    End If
                                    If (InStr(data_temp(0), "MT01") Or InStr(data_temp(0), "M01")) And (InStr(data_temp(1), "MT03") Or InStr(data_temp(1), "M03")) And (InStr(data_temp(2), "MT04") Or InStr(data_temp(2), "M04")) And (InStr(data_temp(3), "MT05") Or InStr(data_temp(3), "M05")) Then 'Cd rH Cl
                                        If (towerVersione >= 704) And (towerVersione < 800) Then
                                        Else
                                            If (towerVersione >= 804) And (towerVersione < 900) Then
                                            Else
                                                mail_csv_setting = False
                                            End If
                                        End If

                                    End If

                                    If (InStr(data_temp(0), "MT01") Or InStr(data_temp(0), "M01")) And (InStr(data_temp(1), "MT07") Or InStr(data_temp(1), "M07")) And (InStr(data_temp(2), "MT04") Or InStr(data_temp(2), "M04")) And (InStr(data_temp(3), "MT05") Or InStr(data_temp(3), "M05")) Then 'Cd trc Cl
                                        If (towerVersione >= 704) And (towerVersione < 800) Then
                                        Else
                                            If (towerVersione >= 804) And (towerVersione < 900) Then
                                            Else
                                                mail_csv_setting = False
                                            End If
                                        End If

                                    End If
                                    If (InStr(data_temp(0), "MT01") Or InStr(data_temp(0), "M01")) And (InStr(data_temp(1), "MT07") Or InStr(data_temp(1), "M07")) And (InStr(data_temp(2), "MT03") Or InStr(data_temp(2), "M03")) And (InStr(data_temp(3), "MT05") Or InStr(data_temp(3), "M05")) Then 'Cd trc rh
                                        If (towerVersione >= 704) And (towerVersione < 800) Then
                                        Else
                                            If (towerVersione >= 804) And (towerVersione < 900) Then
                                            Else
                                                mail_csv_setting = False
                                            End If
                                        End If
                                    End If

                                End If
                            Catch ex As Exception

                            End Try
                        End If
                    Case "max5" ' check enable o disable log
                        If data.Length > 2 Then
                            Dim data1() As String = data(2).Split("&")
                            Try
                                If data1.Length > 0 Then
                                    Dim data_temp() As String = data1(1).Split("#")
                                    If Mid(data_temp(2), 13, 1) = "1" Then 'nabled
                                        menabled_log = True
                                    Else
                                        menabled_log = False
                                    End If
                                End If
                            Catch ex As Exception

                            End Try
                        End If
                End Select
                value = data(2)

                'If value = mvalue4 Then
                '    mvalue_update = False
                'End If

                mvalue4_bool = True
                mvalue4 = value

            End If
        End Set
    End Property
    Public Property value5() As String
        'proprieta che contine( 1A per max 5 clockr per Tower outptr per LD e LDS)
        Get
            Return mvalue5
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                If InStr(data(2), "end") <> 0 Then
                Else
                    Exit Property
                End If

                value = data(2)

                'If value = mvalue5 Then
                '    mvalue_update = False
                'End If

                mvalue5_bool = True
                mvalue5 = value
                Try
                    Select Case mnome_strumento
                        Case "max5"
                            ma1_enable = enable_ma(get_split_str(data(2))(0))
                    End Select

                Catch ex As Exception

                End Try

            End If
        End Set
    End Property
    Public Property value6() As String
        'proprieta che contine(2A per max 5 stbior per Tower clockr per LD e LDS)
        Get
            Return mvalue6
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                If InStr(data(2), "end") <> 0 Then
                Else
                    Exit Property
                End If
                value = data(2)

                'If value = mvalue6 Then
                '    mvalue_update = False
                'End If

                mvalue6_bool = True
                mvalue6 = value
                Try
                    Select Case mnome_strumento
                        Case "max5"
                            ma2_enable = enable_ma(get_split_str(data(2))(0))
                    End Select
                Catch ex As Exception

                End Try


            End If
        End Set
    End Property
    Public Property value6_1() As String
        'proprieta che contine(2A per max 5 stbior per Tower clockr per LD e LDS)
        Get
            Return mvalue6_1
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                If InStr(data(2), "end") <> 0 Then
                Else
                    Exit Property
                End If
                value = data(2)

                'If value = mvalue6_1 Then
                '    mvalue_update = False
                'End If

                mvalue6_1 = value
            End If

        End Set
    End Property

    Public Property value7() As String
        'proprieta che contine(3A per max 5 optior per Tower setpntr per LD e LDS)
        Get
            Return mvalue7
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                If InStr(data(2), "end") <> 0 Then
                Else
                    Exit Property
                End If
                value = data(2)

                'If value = mvalue7 Then
                '    mvalue_update = False
                'End If

                mvalue7_bool = True
                mvalue7 = value
                Try
                    Select Case mnome_strumento
                        Case "max5"
                            ma3_enable = enable_ma(get_split_str(data(2))(0))
                    End Select
                Catch ex As Exception

                End Try

            End If
        End Set
    End Property
    Public Property value8() As String
        'proprieta che contine(4A per max 5 tota1r per Tower paramtr per LD e LDS)
        Get
            Return mvalue8
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue8 Then
                '    mvalue_update = False
                'End If

                mvalue8_bool = True
                mvalue8 = value
                Try
                    Select mnome_strumento
                        Case "max5"
                            ma4_enable = enable_ma(get_split_str(data(2))(0))
                    End Select
                Catch ex As Exception

                End Try

            End If
        End Set
    End Property
    Public Property value9() As String
        'proprieta che contine(5A per max 5 tota2r per Tower alldosr per LD e LDS)
        Get
            Return mvalue9
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue9 Then
                '    mvalue_update = False
                'End If

                mvalue9_bool = True
                mvalue9 = value
                Try
                    Select mnome_strumento
                        Case "max5"
                            ma5_enable = enable_ma(get_split_str(data(2))(0))
                    End Select
                Catch ex As Exception

                End Try

            End If
        End Set
    End Property
    Public Function enable_ma(ByVal output_str As String) As Boolean
        If Mid(output_str, 7, 1) <> "0" Then ' ma
            Return True
        Else
            Return False
        End If
    End Function
    Public Shared Function get_split_str(ByVal output_str) As String()
        Dim data() As String = output_str.Split("&"c)
        Dim data1() As String
        If data.Length() > 0 Then
            data1 = data(1).Split("#")
            Return data1
        End If
        Return Nothing
    End Function
    Public Property value10() As String
        'proprieta che contine( stoutr per Tower allprbr per LD e LDS)
        Get
            Return mvalue10
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue10 Then
                '    mvalue_update = False
                'End If

                mvalue10_bool = True
                mvalue10 = value
            End If
        End Set
    End Property
    Public Property value11() As String
        'proprieta che contine( stoutr per Tower flowstr per LD e LDS)
        Get
            Return mvalue11
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue11 Then
                '    mvalue_update = False
                'End If

                mvalue11_bool = True
                mvalue11 = value
            End If
        End Set
    End Property
    Public Property value12() As String
        'proprieta che contine( stoutr per Tower  compphr  per LD e LDS)
        Get
            Return mvalue12
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)
                If mnome_strumento = "Tower" Or mnome_strumento = "LDtower" Then
                    Try
                        Dim data1() As String = value.Split("&")
                        If data1.Length > 0 Then
                            Dim data_setpntr() As String = data1(1).Split("#"c)
                            'If data_setpntr.Length > 3 Then
                            If Mid(data_setpntr(0), 3, 1) = "0" Then 'disable
                                menabled_log = False
                            Else
                                menabled_log = True
                                'End If
                            End If
                        End If
                    Catch ex As Exception

                    End Try
                End If
                mvalue12_bool = True

                'If value = mvalue12 Then
                '    mvalue_update = False
                'End If

                mvalue12 = value
            End If
        End Set
    End Property
    Public Property value13() As String
        'proprieta che contine( stoutr per Tower  clocksr per LD e LDS)
        Get
            Return mvalue13
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue13 Then
                '    mvalue_update = False
                'End If

                mvalue13_bool = True
                mvalue13 = value
            End If
        End Set
    End Property
    Public Property value14() As String
        'proprieta che contine( stoutr per Tower  maoutsr per LD e LDS)
        Get
            Return mvalue14
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue14 Then
                '    mvalue_update = False
                'End If

                mvalue14_bool = True
                mvalue14 = value
            End If
        End Set
    End Property
    Public Property value15() As String
        'proprieta che contine( stoutr per Tower  logsetr per LD e LDS)
        Get
            Return mvalue15
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)
                If mnome_strumento = "LDMA" Or mnome_strumento = "LDLG" Then
                    menabled_log = False
                End If
                If mnome_strumento = "LD" Or mnome_strumento = "LDS" Or mnome_strumento = "WD" Or mnome_strumento = "WH" Or mnome_strumento = "LD4" Or mnome_strumento = "LTB" _
 Or mnome_strumento = "LTA" Or mnome_strumento = "LTU" Or mnome_strumento = "LTD" Then
                    Try
                        Dim data1() As String = value.Split("&")
                        If data1.Length > 0 Then
                            Dim data_setpntr() As String = data1(1).Split("#"c)
                            If data_setpntr.Length > 3 Then
                                If Mid(data_setpntr(1), 1, 1) = "0" Then 'disable
                                    menabled_log = False
                                Else
                                    menabled_log = True
                                End If
                            End If
                        End If
                    Catch ex As Exception

                    End Try
                End If

                'If value = mvalue15 Then
                '    mvalue_update = False
                'End If

                mvalue15_bool = True
                mvalue15 = value
            End If
        End Set
    End Property
    Public Property value16() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue16
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue16 Then
                '    mvalue_update = False
                'End If

                mvalue16_bool = True
                mvalue16 = value
            End If
        End Set
    End Property
    Public Property value17() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue17
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue17 Then
                '    mvalue_update = False
                'End If

                mvalue17_bool = True
                mvalue17 = value
            End If
        End Set
    End Property
    Public Property value18() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue18
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue18 Then
                '    mvalue_update = False
                'End If

                mvalue18_bool = True
                mvalue18 = value
            End If
        End Set
    End Property
    Public Property value19() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue19
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue19 Then
                '    mvalue_update = False
                'End If

                mvalue19_bool = True
                mvalue19 = value
            End If
        End Set
    End Property
    Public Property value20() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue20
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue20 Then
                '    mvalue_update = False
                'End If

                mvalue20_bool = True
                mvalue20 = value

            End If
        End Set
    End Property
    Public Property value21() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue21
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue21 Then
                '    mvalue_update = False
                'End If

                mvalue21_bool = True
                mvalue21 = value
            End If
        End Set
    End Property
    Public Property value22() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue22
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue22 Then
                '    mvalue_update = False
                'End If

                mvalue22_bool = True
                mvalue22 = value

            End If
        End Set
    End Property
    Public Property value23() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue23
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue23 Then
                '    mvalue_update = False
                'End If

                mvalue23_bool = True
                mvalue23 = value
            End If
        End Set
    End Property
    Public Property value24() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue24
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue24 Then
                '    mvalue_update = False
                'End If

                mvalue24_bool = True
                mvalue24 = value
            End If
        End Set
    End Property
    Public Property value25() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue25
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue25 Then
                '    mvalue_update = False
                'End If

                mvalue25_bool = True
                mvalue25 = value
            End If
        End Set
    End Property
    Public Property value26() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue26
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue26 Then
                '    mvalue_update = False
                'End If

                mvalue26_bool = True
                mvalue26 = value
            End If
        End Set
    End Property
    Public Property value27() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue27
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue27 Then
                '    mvalue_update = False
                'End If

                mvalue27_bool = True
                mvalue27 = value
            End If
        End Set
    End Property
    Public Property value28() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue28
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue28 Then
                '    mvalue_update = False
                'End If

                mvalue28_bool = True
                mvalue28 = value
            End If


        End Set
    End Property
    Public Property value29() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue29
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue29 Then
                '    mvalue_update = False
                'End If

                mvalue29_bool = True
                mvalue29 = value
            End If


        End Set
    End Property
    Public Property value30() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue30
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue29 Then
                '    mvalue_update = False
                'End If

                mvalue30_bool = True
                mvalue30 = value
            End If


        End Set
    End Property
    Public Property value31() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue31
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue29 Then
                '    mvalue_update = False
                'End If

                mvalue31_bool = True
                mvalue31 = value
            End If


        End Set
    End Property

    Public Property value32() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue32
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue29 Then
                '    mvalue_update = False
                'End If

                mvalue32_bool = True
                mvalue32 = value
            End If


        End Set
    End Property
    Public Property value33() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue33
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue29 Then
                '    mvalue_update = False
                'End If

                mvalue33_bool = True
                mvalue33 = value
            End If


        End Set
    End Property
    Public Property value34() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue34
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)

                'If value = mvalue29 Then
                '    mvalue_update = False
                'End If

                mvalue34_bool = True
                mvalue34 = value
            End If


        End Set
    End Property

    Public Property value_change() As String
        'proprieta che contine( stoutr per Tower  minmaxr per LD e LDS)
        Get
            Return mvalue_change
        End Get
        Set(ByVal value As String)
            Dim data() As String = value.Split("|")
            If data.Length > 2 Then
                value = data(2)
                mvalue_change_bool = True
                mvalue_change = value
                If InStr(value, "yes") <> 0 Then
                    read_all_value()

                    If mail_csv_setting Then
                        mvalue29_bool = False
                    End If
                End If
            End If
        End Set
    End Property
    Public Sub read_all_value()
        mvalue1_bool = False
        mvalue2_bool = False
        mvalue3_bool = False
        mvalue4_bool = False
        mvalue5_bool = False
        mvalue6_bool = False
        mvalue7_bool = False
        mvalue8_bool = False
        mvalue9_bool = False
        mvalue10_bool = False
        mvalue11_bool = False
        mvalue12_bool = False
        mvalue13_bool = False
        mvalue14_bool = False
        mvalue15_bool = False
        mvalue16_bool = False
        mvalue17_bool = False
        mvalue18_bool = False
        mvalue19_bool = False
        mvalue20_bool = False
        mvalue21_bool = False
        mvalue22_bool = False
        mvalue23_bool = False
        mvalue24_bool = False
        mvalue25_bool = False
        mvalue26_bool = False
        mvalue27_bool = False
        mvalue28_bool = False
        mvalue29_bool = False
        mvalue30_bool = False
        mvalue31_bool = False
        mvalue32_bool = False
        mvalue33_bool = False
        mvalue34_bool = False
    End Sub

    Public Property time_connessione() As DateTime
        'ultimo aggiornamento
        Get
            Return time_strumento
        End Get
        Set(ByVal value As DateTime)
            time_strumento = value
        End Set
    End Property
    Private Function get_str_id(ByVal input As String) As String
        Try
            Dim id_value() As String = input.Split("&")
            Dim id_name() As String = id_value(0).Split("d")
            Dim id_name_str As String = ""
            If id_name.Count = 1 Then
                id_name_str = id_name(0)
            End If
            If id_name.Count > 1 Then
                id_name_str = id_name(1)
            End If
            Return id_name_str

        Catch ex As Exception

        End Try

    End Function
    Private Function enable_scale_tower(ByVal input As String) As Integer
        Dim data() As String = input.Split("#")
        Dim canali_tower As Integer = 0
        If data.Length > 0 Then
            Try
                If (InStr(data(0), "MT01") Or InStr(data(0), "M01")) And (InStr(data(1), "MT06") Or InStr(data(1), "M06")) And (InStr(data(2), "MT06") Or InStr(data(2), "M06")) And (InStr(data(3), "MT05") Or InStr(data(3), "M05")) Then ' Cd 
                    factor_divide_ch1 = 1
                    canali_tower = 1
                    If tower_induttiva Then
                        full_scale_ch1 = 30000
                    Else
                        full_scale_ch1 = 3000
                    End If


                End If
                If (InStr(data(0), "MT01") Or InStr(data(0), "M01")) And (InStr(data(1), "MT02") Or InStr(data(1), "M02")) And (InStr(data(2), "MT06") Or InStr(data(2), "M06")) And (InStr(data(3), "MT05") Or InStr(data(3), "M05")) Then 'Cd pH
                    factor_divide_ch1 = 1
                    If tower_induttiva Then
                        full_scale_ch1 = 30000
                    Else
                        full_scale_ch1 = 3000
                    End If

                    factor_divide_ch2 = 100
                    full_scale_ch2 = 14
                    canali_tower = 2
                End If
                If (InStr(data(0), "MT01") Or InStr(data(0), "M01")) And (InStr(data(1), "MT03") Or InStr(data(1), "M03")) And (InStr(data(2), "MT06") Or InStr(data(2), "M06")) And (InStr(data(3), "MT05") Or InStr(data(3), "M05")) Then 'Cd rH
                    factor_divide_ch1 = 1
                    If tower_induttiva Then
                        full_scale_ch1 = 30000
                    Else
                        full_scale_ch1 = 3000
                    End If

                    factor_divide_ch2 = 1
                    full_scale_ch2 = 1000
                    canali_tower = 2
                End If

                If (InStr(data(0), "MT01") Or InStr(data(0), "M01")) And (InStr(data(1), "MT04") Or InStr(data(1), "M04")) And (InStr(data(2), "MT06") Or InStr(data(2), "M06")) And (InStr(data(3), "MT05") Or InStr(data(3), "M05")) Then 'Cd Cl
                    factor_divide_ch1 = 1
                    If tower_induttiva Then
                        full_scale_ch1 = 30000
                    Else
                        full_scale_ch1 = 3000
                    End If

                    factor_divide_ch2 = get_sonda_tower(Val(Right(data(1), 2)), full_scale_ch2)
                    canali_tower = 2
                End If
                If (InStr(data(0), "MT01") Or InStr(data(0), "M01")) And (InStr(data(1), "MT02") Or InStr(data(1), "M02")) And (InStr(data(2), "MT03") Or InStr(data(2), "M03")) And (InStr(data(3), "MT05") Or InStr(data(3), "M05")) Then 'Cd pH rH
                    factor_divide_ch1 = 1
                    If tower_induttiva Then
                        full_scale_ch1 = 30000
                    Else
                        full_scale_ch1 = 3000
                    End If

                    factor_divide_ch2 = 100
                    full_scale_ch2 = 14

                    factor_divide_ch3 = 1
                    full_scale_ch3 = 1000
                    canali_tower = 3
                End If
                If (InStr(data(0), "MT01") Or InStr(data(0), "M01")) And (InStr(data(1), "MT03") Or InStr(data(1), "M03")) And (InStr(data(2), "MT04") Or InStr(data(2), "M04")) And (InStr(data(3), "MT05") Or InStr(data(3), "M05")) Then 'Cd rH cl
                    factor_divide_ch1 = 1
                    If tower_induttiva Then
                        full_scale_ch1 = 30000
                    Else
                        full_scale_ch1 = 3000
                    End If

                    factor_divide_ch2 = 1
                    full_scale_ch2 = 1000

                    factor_divide_ch3 = get_sonda_tower(Val(Right(data(2), 2)), full_scale_ch3)
                    canali_tower = 3
                End If

                If (InStr(data(0), "MT01") Or InStr(data(0), "M01")) And (InStr(data(1), "MT02") Or InStr(data(1), "M02")) And (InStr(data(2), "MT04") Or InStr(data(2), "M04")) And (InStr(data(3), "MT05") Or InStr(data(3), "M05")) Then 'Cd pH Cl
                    factor_divide_ch1 = 1
                    If tower_induttiva Then
                        full_scale_ch1 = 30000
                    Else
                        full_scale_ch1 = 3000
                    End If

                    factor_divide_ch2 = 100
                    full_scale_ch2 = 14
                    factor_divide_ch3 = get_sonda_tower(Val(Right(data(2), 2)), full_scale_ch3)
                    canali_tower = 3
                End If
                If (InStr(data(0), "MT01") Or InStr(data(0), "M01")) And (InStr(data(1), "MT07") Or InStr(data(1), "M07")) And (InStr(data(2), "MT03") Or InStr(data(2), "M03")) And (InStr(data(3), "MT05") Or InStr(data(3), "M05")) Then 'Cd trc rh
                    factor_divide_ch1 = 1
                    If tower_induttiva Then
                        full_scale_ch1 = 30000
                    Else
                        full_scale_ch1 = 3000
                    End If

                    factor_divide_ch2 = 10
                    full_scale_ch2 = 999

                    factor_divide_ch3 = 1
                    full_scale_ch3 = 1000

                    canali_tower = 3

                End If
                If (InStr(data(0), "MT01") Or InStr(data(0), "M01")) And (InStr(data(1), "MT07") Or InStr(data(1), "M07")) And (InStr(data(2), "MT04") Or InStr(data(2), "M04")) And (InStr(data(3), "MT05") Or InStr(data(3), "M05")) Then 'Cd trc Cl
                    factor_divide_ch1 = 1
                    If tower_induttiva Then
                        full_scale_ch1 = 30000
                    Else
                        full_scale_ch1 = 3000
                    End If

                    factor_divide_ch2 = 10
                    full_scale_ch2 = 999
                    factor_divide_ch3 = get_sonda_tower(Val(Right(data(2), 2)), full_scale_ch3)
                    canali_tower = 3
                End If

            Catch ex As Exception
                canali_tower = 0
            End Try
            Return canali_tower
        End If
    End Function
    Private Function get_sonda_tower(ByVal tipo_sonda As Integer, ByRef full_scale As Integer) As Integer
        Select Case tipo_sonda
            Case 0
                'alvaro per log
                full_scale = 2
                Return 1000

            Case 1
                'alvaro per log
                full_scale = 5
                Return 1000
                '      gauge.NumericIndicators(stringa_canale).Value = valuer / 1000
            Case 2
                full_scale = 20
                Return 100
                '       gauge.NumericIndicators(stringa_canale).Value = valuer / 100
            Case 3
                full_scale = 200
                Return 10
                '        gauge.NumericIndicators(stringa_canale).Value = valuer / 10
            Case 4
                full_scale = 2
                Return 1000
                '         gauge.NumericIndicators(stringa_canale).Value = valuer / 1000
            Case 5
                full_scale = 20
                Return 100
                '          gauge.NumericIndicators(stringa_canale).Value = valuer / 100
            Case 6
                full_scale = 2
                Return 1000
                '           gauge.NumericIndicators(stringa_canale).Value = valuer / 1000
            Case 7
                full_scale = 10
                Return 100
                '            gauge.NumericIndicators(stringa_canale).Value = valuer / 100
            Case 8
                full_scale = 10
                Return 100
                '             gauge.NumericIndicators(stringa_canale).Value = valuer / 100
            Case 9
                full_scale = 10
                Return 100
                '              gauge.NumericIndicators(stringa_canale).Value = valuer / 100
            Case 10
                full_scale = 10
                Return 100
                '               gauge.NumericIndicators(stringa_canale).Value = valuer / 100

            Case 11
                full_scale = 10
                Return 100
                '                gauge.NumericIndicators(stringa_canale).Value = valuer / 100
            Case 12
                full_scale = 200
                Return 10
                '                gauge.NumericIndicators(stringa_canale).Value = valuer / 10
            Case 13
                full_scale = 20
                Return 100
                '                gauge.NumericIndicators(stringa_canale).Value = valuer / 10
            Case 14
                full_scale = 5
                Return 1000
        End Select
    End Function
    Private Sub enable_scale_ld(ByVal input As String)
        Dim data() As String = input.Split("#")
        Try
            If data.Length > 0 Then
                get_scale_ld(Mid(data(1), 1, 2), factor_divide_ch1, full_scale_ch1)
                get_scale_ld(Mid(data(2), 1, 2), factor_divide_ch2, full_scale_ch2)
                If data.Length > 4 Then ' ld tre canali
                    get_scale_ld(data(3), factor_divide_ch3, full_scale_ch3)
                End If
                If data.Length > 5 Then ' ld 4 canali
                    get_scale_ld(data(4), factor_divide_ch4, full_scale_ch4)
                End If
            End If
        Catch ex As Exception

        End Try

    End Sub
    Private Sub enable_scale_ld4(ByVal input As String)
        Dim data() As String = input.Split("#")
        If data.Length > 0 Then
            get_scale_ld(Mid(data(1), 1, 2), factor_divide_ch1, full_scale_ch1)
            get_scale_ld(Mid(data(2), 1, 2), factor_divide_ch2, full_scale_ch2)
            get_scale_ld(Mid(data(3), 1, 2), factor_divide_ch3, full_scale_ch3)
            get_scale_ld(Mid(data(4), 1, 2), factor_divide_ch4, full_scale_ch4)

        End If
    End Sub

    Private Sub enable_scale_lds(ByVal input As String)
        Dim data() As String = input.Split("#")
        If data.Length > 0 Then
            ' per scala lds 
            If (data(1).Length > 2) Then
                get_scale_ld(Mid(data(1), 1, 3), factor_divide_ch1, full_scale_ch1)
                factor_divide_ch2 = factor_divide_ch1
            Else
                get_scale_ld(Mid(data(1), 1, 2), factor_divide_ch1, full_scale_ch1)
                factor_divide_ch2 = factor_divide_ch1
            End If
        End If
    End Sub
    Public Function get_scale_ld(ByVal value_1 As String, ByRef factor_divide As Integer, ByRef full_scale As Integer) As Integer
        Select Case value_1
            Case "00"
                factor_divide = 1000
                full_scale = 2
            Case "01"
                factor_divide = 1000
                full_scale = 5
            Case "02"
                factor_divide = 100
                full_scale = 20
            Case "03"
                factor_divide = 10
                full_scale = 200
            Case "04"
                factor_divide = 1000
                full_scale = 2
            Case "05"
                factor_divide = 100
                full_scale = 20
            Case "06"
                factor_divide = 1000
                full_scale = 2
            Case "07"
                factor_divide = 100
                full_scale = 10
            Case "08"
                factor_divide = 100
                full_scale = 20
            Case "09"
                factor_divide = 100
                full_scale = 10
            Case "10"
                factor_divide = 1000
                full_scale = 2
            Case "110"
                factor_divide = 1000
                full_scale = 2

            Case "11"
                factor_divide = 100
                full_scale = 20
            Case "12"
                factor_divide = 10
                full_scale = 200
            Case "13"
                factor_divide = 1
                full_scale = 2000
            Case "14"
                factor_divide = 1000
                full_scale = 1
            Case "15"
                factor_divide = 100
                full_scale = 10
            Case "16"
                factor_divide = 10
                full_scale = 200
            Case "17"
                factor_divide = 1
                full_scale = 2000
            Case "18"
                factor_divide = 100
                full_scale = 60
            Case "19"
                factor_divide = 100
                full_scale = 10
            Case "20"
                factor_divide = 100
                full_scale = 10
            Case "21"
                factor_divide = 100
                full_scale = 10
            Case "22" ' modifica mV
                factor_divide = 1
                full_scale = 1000
            Case "23" ' modifica per gestione pH
                factor_divide = 100
                full_scale = 14
            Case "24" ' uS
                factor_divide = 10
                full_scale = 300
            Case "25" ' uS
                factor_divide = 1
                full_scale = 3000
            Case "26" ' mS
                factor_divide = 100
                full_scale = 30
            Case "27" ' mS
                factor_divide = 10
                full_scale = 300
            Case "28" ' LDO x.xxx
                factor_divide = 1000
                full_scale = 9
            Case "29" ' LDO xx.xx
                factor_divide = 100
                full_scale = 99
            Case "30" ' LDO xxx.x
                factor_divide = 10
                full_scale = 999
            Case "31" ' LDO xxxx
                factor_divide = 1
                full_scale = 9999
            Case "32" ' NTU vecchia xx.xx
                factor_divide = 100
                full_scale = 30
            Case "33" ' NTU nuova x.xxx
                factor_divide = 1000
                full_scale = 9
            Case "34" ' NTU nuova xx.xx
                factor_divide = 100
                full_scale = 99
            Case "35" ' NTU nuova xxx.x
                factor_divide = 10
                full_scale = 999
            Case "36" ' NTU vecchia xxxx
                factor_divide = 1
                full_scale = 9999

            Case "37" '
                factor_divide = 100
                full_scale = 5

            Case "38" '
                factor_divide = 100
                full_scale = 5
            Case "39" '
                full_scale = 99
                factor_divide = 100
            Case "40" '
                full_scale = 99
                factor_divide = 100
            Case "41" '
                full_scale = 30
                factor_divide = 100
            Case "42" '
                full_scale = 9
                factor_divide = 1000
            Case "43" '
                full_scale = 9
                factor_divide = 1000
            Case "44" '
                full_scale = 20
                factor_divide = 100
            Case "45" '
                full_scale = 200
                factor_divide = 10
            Case "46" '
                full_scale = 200
                factor_divide = 10

            Case "47" ' mS
                full_scale = 10
                factor_divide = 100
            Case "48" ' mS
                full_scale = 999
                factor_divide = 10
            Case "49" ' mS
                full_scale = 999
                factor_divide = 10
            Case "50" ' ppm
                full_scale = 9999
                factor_divide = 1

            Case "51"
                full_scale = 10
                factor_divide = 100

            Case "53"
                full_scale = 10
                factor_divide = 100
            Case "54"
                full_scale = 10
                factor_divide = 100
            Case "55"
                disable_comp_ph = True
                full_scale = 3
                factor_divide = 100
            Case "56"
                full_scale = 1000
                factor_divide = 1
            Case "57"
                factor_divide = 100
                full_scale = 5
            Case "58"
                factor_divide = 100
                full_scale = 1

            Case "59"
                factor_divide = 100
                full_scale = 20
            Case "60"
                factor_divide = 100
                full_scale = 20
            Case "61"
                factor_divide = 1000
                full_scale = 2
            Case "62"
                factor_divide = 100
                full_scale = 20
            Case "63"
                factor_divide = 1000
                full_scale = 2
            Case "64"
                factor_divide = 100
                full_scale = 50

            Case "65"
                factor_divide = 1000
                full_scale = 2
            Case "66"
                factor_divide = 1000
                full_scale = 5
            Case "67"
                factor_divide = 100
                full_scale = 10
            Case "68"
                factor_divide = 100
                full_scale = 20
            Case "69"
                factor_divide = 100
                full_scale = 50
            Case "70"
                factor_divide = 10
                full_scale = 100
            Case "71"
                factor_divide = 10
                full_scale = 200
            Case "72"
                factor_divide = 10
                full_scale = 500
            Case "73"
                factor_divide = 1
                full_scale = 1000
            Case "74"
                factor_divide = 100
                full_scale = 50

            Case "78"
                factor_divide = 100
                full_scale = 5

            Case "79"
                factor_divide = 100
                full_scale = 5
            Case "88"
                factor_divide = 100
                full_scale = 5
            Case "89"
                factor_divide = 100
                full_scale = 5
            Case "90"
                factor_divide = 100
                full_scale = 5


            Case "92"
                factor_divide = 100
                full_scale = 40
            Case "93"
                factor_divide = 10
                full_scale = 400
            Case "94"
                factor_divide = 1
                full_scale = 4000

            Case "100"
                factor_divide = 100
                full_scale = 40
            Case "101"
                factor_divide = 100
                full_scale = 40
            Case "102"
                factor_divide = 10
                full_scale = 400
            Case "103"
                factor_divide = 1
                full_scale = 4000
            Case "104"
                factor_divide = 100
                full_scale = 20

            Case "106"
                factor_divide = 100
                full_scale = 3

        End Select
    End Function

    Private Sub enable_scale(ByVal input As String)
        Dim data() As String = input.Split("#")
        Dim code_channel As Integer = 0
        For i = 1 To 5
            code_channel = Val(Mid(data(i - 1), 3, 1))
            Select Case i
                Case 1
                    set_scale(code_channel, factor_divide_ch1, data(i - 1), full_scale_ch1, i)
                Case 2
                    set_scale(code_channel, factor_divide_ch2, data(i - 1), full_scale_ch2, i)
                Case 3
                    set_scale(code_channel, factor_divide_ch3, data(i - 1), full_scale_ch3, i)
                Case 4
                    set_scale(code_channel, factor_divide_ch4, data(i - 1), full_scale_ch4, i)
                Case 5
                    If code_channel = 7 Then
                        factor_divide_ch5 = factor_divide_ch3
                        full_scale_ch5 = full_scale_ch3
                        set_scale(code_channel, factor_divide_ch5, data(i - 1), full_scale_ch5, i)
                    Else
                        set_scale(code_channel, factor_divide_ch5, data(i - 1), full_scale_ch5, i)
                    End If
            End Select
        Next
    End Sub
    Public Sub set_scale(ByVal code_channel As Integer, ByRef factor_divide As Integer, ByVal input_str As String, ByRef full_scale As Integer, ByVal numeroCanale As Integer)
        Select Case code_channel
            Case 0
                factor_divide = 100
                full_scale = 3

            Case 1
                factor_divide = 100
                full_scale = 14
            Case 2
                factor_divide = 1
                full_scale = 1000
            Case 3
                Dim indice As Integer = InStr(input_str, "E")
                factor_divide = get_scale(Mid(input_str, indice - 3, 1))
                full_scale = Val(Mid(input_str, indice - 7, 4))
                'factor_divide = get_scale(Mid(input_str, 16, 1))
            Case 4
                If (numeroCanale = 1) Then
                    factor_divide = get_scale(Mid(input_str, Len(input_str) - 1, 1))
                    full_scale = Val(Mid(input_str, Len(input_str) - 5, 4))

                Else
                    factor_divide = get_scale(Mid(input_str, Len(input_str), 1))
                    full_scale = Val(Mid(input_str, Len(input_str) - 4, 4))

                End If

            Case 5
                factor_divide = get_scale(Mid(input_str, Len(input_str) - 7, 1))
                full_scale = Val(Mid(input_str, Len(input_str) - 11, 4))
            Case 6
                factor_divide = 1
                full_scale = 0
            Case 7
                factor_divide = factor_divide
                full_scale = full_scale
            Case 8
                factor_divide = get_scale(Mid(input_str, 8, 1))
                full_scale = Val(Mid(input_str, 4, 4))

            Case 9 'dosim acquedotto
                factor_divide = get_scale(Mid(input_str, 16, 1))
                full_scale = Val(Mid(input_str, 12, 4))
        End Select

    End Sub
    Function get_scale(ByVal value_1 As String) As Integer
        Select Case value_1
            Case "0", "4"
                Return 1
            Case "1"
                Return 1000
            Case "2"
                Return 100
            Case "3"
                Return 10
        End Select
    End Function

End Class
