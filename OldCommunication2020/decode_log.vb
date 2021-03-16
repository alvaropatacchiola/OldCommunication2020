
Public Class decode_log
    Public query As New query
    Public Function return_lettura(ByVal divisore As Integer, ByVal value As Double) As Double
        Select Case divisore
            Case 1
                Return value
            Case 1000
                'Return Mid(stringa, 1, 1) & "." & Mid(stringa, 2, 3)
                value = value / 1000
                Return value
            Case 100
                'Return Mid(stringa, 1, 2) & "." & Mid(stringa, 3, 2)
                value = value / 100
                Return value
            Case 10
                'Return Mid(stringa, 1, 3) & "." & Mid(stringa, 4, 1)
                value = value / 10
                Return value
        End Select
        Return 0
    End Function
    Public Function return_lettura_lotus(ByVal divisore As Integer) As Integer
        Select Case divisore
            Case 1
                Return 1000
            Case 2
                'Return Mid(stringa, 1, 1) & "." & Mid(stringa, 2, 3)

                Return 100
            Case 3
                'Return Mid(stringa, 1, 2) & "." & Mid(stringa, 3, 2)
                Return 10
            Case 4
                'Return Mid(stringa, 1, 3) & "." & Mid(stringa, 4, 1)

                Return 1
        End Select
        Return 0
    End Function

    Public Function filtra_valore(ByVal valore As Single, ByVal full_scale As Integer) As Boolean
        If full_scale > 0 Then
            If valore >= full_scale Then
                Return False
            End If
            If valore = 0 Then
                Return False
            End If
            Return True
        Else
            Return False
        End If
    End Function
    Public Function decode_log_max5(ByVal identificativo As String, ByVal id_strumento As String, ByVal string_parse As String, ByVal divide_ch1 As Integer, ByVal divide_ch2 As Integer, ByVal divide_ch3 As Integer, ByVal divide_ch4 As Integer, ByVal divide_ch5 As Integer, ByVal yagel_version As Boolean, ByVal full_1 As Integer, ByVal full_2 As Integer, ByVal full_3 As Integer, ByVal full_4 As Integer, ByVal full_5 As Integer, _
                               ByVal ch1_value As Single, ByVal ch2_value As Single, ByVal ch3_value As Single, ByVal ch4_value As Single, ByVal ch5_value As Single, ByVal numero_versione As Integer, _
                               ByVal ma1_enable As Boolean, ByVal ma2_enable As Boolean, ByVal ma3_enable As Boolean, ByVal ma4_enable As Boolean, ByVal ma5_enable As Boolean) As Boolean
        Try
            Dim fermati As Boolean
            If identificativo = "638388" Or identificativo = "797255" Then
                fermati = True
            End If

            Dim split_log_first() As String = string_parse.Split("&")
            If split_log_first.Length > 0 Then
                Dim split_log_second() As String = split_log_first(1).Split("g")
                If split_log_second.Length > 0 Then
                    Dim file_str As String
                    Dim split_log() As String = split_log_second(1).Split("?")
                    Dim matrice(100, 1) As String
                    file_str = split_log(0)
                    Dim i As Integer
                    Dim j As Integer
                    For i = 0 To split_log.Length - 2
                        matrice(j, 0) = split_log(i)
                        matrice(j, 1) = split_log(i + 1)
                        If matrice(j, 0) = "126" Then
                            matrice(j, 0) = "13"
                        End If
                        If matrice(j, 1) = "126" Then
                            matrice(j, 1) = "13"
                        End If
                        If matrice(j, 0) = "127" Then
                            matrice(j, 0) = "38"
                        End If
                        If matrice(j, 1) = "127" Then
                            matrice(j, 1) = "38"
                        End If
                        j = j + 1
                        i = i + 1
                        If i > split_log.Length - 1 Or j > 99 Then
                            Exit For
                        End If
                    Next
                    Dim data_str As String = ""
                    Dim condition As String = ""
                    Dim insert As String = ""
                    Dim ch1_val As Boolean = False
                    Dim ch2_val As Boolean = False
                    Dim ch3_val As Boolean = False
                    Dim ch4_val As Boolean = False
                    Dim ch5_val As Boolean = False
                    Dim temp_val As Boolean = False
                    Dim wm_val As Boolean = False
                    Dim aa1 As Boolean = False
                    Dim aa2 As Boolean = False
                    Dim aa3 As Boolean = False
                    Dim aa4 As Boolean = False
                    Dim aa5 As Boolean = False
                    Dim ab1 As Boolean = False
                    Dim ab2 As Boolean = False
                    Dim ab3 As Boolean = False
                    Dim ab4 As Boolean = False
                    Dim ab5 As Boolean = False
                    Dim ad1 As Boolean = False
                    Dim ad2 As Boolean = False
                    Dim ad3 As Boolean = False
                    Dim ad4 As Boolean = False
                    Dim ad5 As Boolean = False
                    Dim ar1 As Boolean = False
                    Dim ar2 As Boolean = False
                    Dim ar3 As Boolean = False
                    Dim ar4 As Boolean = False
                    Dim ar5 As Boolean = False

                    Dim da1 As Boolean = False
                    Dim db1 As Boolean = False
                    Dim pa1 As Boolean = False
                    Dim pa1_val As Integer = 0
                    Dim pb1 As Boolean = False
                    Dim pb1_val As Integer = 0
                    Dim ma1 As Boolean = False
                    Dim ma1_val As Integer = 0

                    Dim da2 As Boolean = False
                    Dim db2 As Boolean = False
                    Dim pa2 As Boolean = False
                    Dim pa2_val As Integer = 0
                    Dim pb2 As Boolean = False
                    Dim pb2_val As Integer = 0
                    Dim ma2 As Boolean = False
                    Dim ma2_val As Integer = 0

                    Dim da3 As Boolean = False
                    Dim db3 As Boolean = False
                    Dim pa3 As Boolean = False
                    Dim pa3_val As Integer = 0
                    Dim pb3 As Boolean = False
                    Dim pb3_val As Integer = 0
                    Dim ma3 As Boolean = False
                    Dim ma3_val As Integer = 0

                    Dim da4 As Boolean = False
                    Dim db4 As Boolean = False
                    Dim pa4 As Boolean = False
                    Dim pa4_val As Integer = 0
                    Dim pb4 As Boolean = False
                    Dim pb4_val As Integer = 0
                    Dim ma4 As Boolean = False
                    Dim ma4_val As Integer = 0

                    Dim da5 As Boolean = False
                    Dim db5 As Boolean = False
                    Dim pa5 As Boolean = False
                    Dim pa5_val As Integer = 0
                    Dim pb5 As Boolean = False
                    Dim pb5_val As Integer = 0
                    Dim ma5 As Boolean = False
                    Dim ma5_val As Integer = 0

                    Dim flow_val As Boolean = False
                    Dim stby_val As Boolean = False
                    Dim fml_val As Boolean = False

                    Dim valore_ch1 As Single = 0
                    Dim valore_ch2 As Single = 0
                    Dim valore_ch3 As Single = 0
                    Dim valore_ch4 As Single = 0
                    Dim valore_ch5 As Single = 0
                    Dim valore_temp As Single = 0
                    Dim valore_wm As Single = 0

                    Dim aa1_val As String = ""
                    Dim aa2_val As String = ""
                    Dim aa3_val As String = ""
                    Dim aa4_val As String = ""
                    Dim aa5_val As String = ""
                    Dim ab1_val As String = ""
                    Dim ab2_val As String = ""
                    Dim ab3_val As String = ""
                    Dim ab4_val As String = ""
                    Dim ab5_val As String = ""
                    Dim ad1_val As String = ""
                    Dim ad2_val As String = ""
                    Dim ad3_val As String = ""
                    Dim ad4_val As String = ""
                    Dim ad5_val As String = ""
                    Dim ar1_val As String = ""
                    Dim ar2_val As String = ""
                    Dim ar3_val As String = ""
                    Dim ar4_val As String = ""
                    Dim ar5_val As String = ""
                    Dim val_fml As String = ""
                    Dim data As Date
                    Dim valore_flow As String = ""
                    Dim insert_flow As String = ""
                    Dim query_select As String = ""
                    Dim query_insert As String = ""
                    Try
                        If identificativo = "317110" Then
                            val_fml = ""
                        End If
                        data = New Date(2000 + matrice(2, 1), matrice(1, 1), matrice(0, 1), matrice(3, 1), matrice(4, 1), 0)
                        For i = 5 To 99
                            'mese giorno e anno
                            Select Case Val(matrice(i, 0))
                                Case 6 To 8
                                    If ch1_val = False Then
                                        valore_ch1 = return_lettura(divide_ch1, Val(matrice(i, 1) * 100) + Val(matrice(i + 2, 1)))
                                        If Not filtra_valore(valore_ch1, full_1) Then
                                            valore_ch1 = ch1_value
                                        End If
                                        i = i + 2
                                        ch1_val = True
                                    End If
                                Case 9 To 11
                                    If ch2_val = False Then
                                        If identificativo = "638382" Then
                                            ch2_val = False
                                        End If
                                        valore_ch2 = return_lettura(divide_ch2, Val(matrice(i, 1) * 100) + Val(matrice(i + 2, 1)))
                                        If Not filtra_valore(valore_ch2, full_2) Then
                                            valore_ch2 = ch2_value
                                        End If
                                        i = i + 2
                                        ch2_val = True
                                    End If
                                Case 12 To 14
                                    If ch3_val = False Then
                                        valore_ch3 = return_lettura(divide_ch3, Val(matrice(i, 1) * 100) + Val(matrice(i + 2, 1)))
                                        If Not filtra_valore(valore_ch3, full_3) Then
                                            valore_ch3 = ch3_value
                                        End If
                                        i = i + 2
                                        ch3_val = True
                                    End If
                                Case 15 To 17
                                    If ch4_val = False Then
                                        valore_ch4 = return_lettura(divide_ch4, Val(matrice(i, 1) * 100) + Val(matrice(i + 2, 1)))
                                        If Not filtra_valore(valore_ch4, full_4) Then
                                            valore_ch4 = ch4_value
                                        End If
                                        i = i + 2
                                        ch4_val = True
                                    End If
                                Case 18 To 20
                                    If ch5_val = False Then
                                        valore_ch5 = return_lettura(divide_ch5, Val(matrice(i, 1) * 100) + Val(matrice(i + 2, 1)))
                                        'If Not filtra_valore(valore_ch5, full_5) Then
                                        'valore_ch5 = ch5_value
                                        'End If
                                        i = i + 2
                                        ch5_val = True
                                    End If
                                Case 26 'Aa1
                                    If Val(matrice(i, 1)) = 1 Then
                                        aa1 = True
                                    Else
                                        aa1 = False
                                    End If
                                Case 27 'Ab1
                                    If Val(matrice(i, 1)) = 1 Then
                                        ab1 = True
                                    Else
                                        ab1 = False
                                    End If
                                Case 28 'Ad1
                                    If Val(matrice(i, 1)) = 1 Then
                                        ad1 = True
                                    Else
                                        ad1 = False
                                    End If

                                Case 29 'Ar1
                                    If Val(matrice(i, 1)) = 1 Then
                                        ar1 = True
                                    Else
                                        ar1 = False
                                    End If

                                Case 35 'Aa2
                                    If Val(matrice(i, 1)) = 1 Then
                                        aa2 = True
                                    Else
                                        aa2 = False
                                    End If

                                Case 36 'Ab2
                                    If Val(matrice(i, 1)) = 1 Then
                                        ab2 = True
                                    Else
                                        ab2 = False
                                    End If

                                Case 37 'Ad2
                                    If Val(matrice(i, 1)) = 1 Then
                                        ad2 = True
                                    Else
                                        ad2 = False
                                    End If

                                Case 38 'Ar2
                                    If Val(matrice(i, 1)) = 1 Then
                                        ar2 = True
                                    Else
                                        ar2 = False
                                    End If

                                Case 44 'Aa3
                                    If Val(matrice(i, 1)) = 1 Then
                                        aa3 = True
                                    Else
                                        aa3 = False
                                    End If

                                Case 45 'Ab3
                                    If Val(matrice(i, 1)) = 1 Then
                                        ab3 = True
                                    Else
                                        ab3 = False
                                    End If

                                Case 46 'Ad3
                                    If Val(matrice(i, 1)) = 1 Then
                                        ad3 = True
                                    Else
                                        ad3 = False
                                    End If

                                Case 47 'Ar3
                                    If Val(matrice(i, 1)) = 1 Then
                                        ar3 = True
                                    Else
                                        ar3 = False
                                    End If

                                Case 53 'Aa4
                                    If Val(matrice(i, 1)) = 1 Then
                                        aa4 = True
                                    Else
                                        aa4 = False
                                    End If

                                Case 54 'Ab4
                                    If Val(matrice(i, 1)) = 1 Then
                                        ab4 = True
                                    Else
                                        ab4 = False
                                    End If

                                Case 55 'Ad4
                                    If Val(matrice(i, 1)) = 1 Then
                                        ad4 = True
                                    Else
                                        ad4 = False
                                    End If

                                Case 56 'Ar4
                                    If Val(matrice(i, 1)) = 1 Then
                                        ar4 = True
                                    Else
                                        ar4 = False
                                    End If

                                Case 62 'Aa5
                                    If Val(matrice(i, 1)) = 1 Then
                                        aa5 = True
                                    Else
                                        aa5 = False
                                    End If

                                Case 63 'Ab5
                                    If Val(matrice(i, 1)) = 1 Then
                                        ab5 = True
                                    Else
                                        ab5 = False
                                    End If

                                Case 64 'Ad5
                                    If Val(matrice(i, 1)) = 1 Then
                                        ad5 = True
                                    Else
                                        ad5 = False
                                    End If

                                Case 65 'Ar5
                                    If Val(matrice(i, 1)) = 1 Then
                                        ar5 = True
                                    Else
                                        ar5 = False
                                    End If
                                Case 21
                                    If Val(matrice(i, 1)) = 1 Then
                                        da1 = True
                                    Else
                                        da1 = False
                                    End If
                                Case 22
                                    If Val(matrice(i, 1)) = 1 Then
                                        db1 = True
                                    Else
                                        db1 = False
                                    End If
                                Case 23
                                    pa1_val = Val(matrice(i, 1))
                                    pa1 = True
                                Case 24
                                    pb1_val = Val(matrice(i, 1))
                                    pb1 = True
                                Case 25
                                    ma1_val = Val(matrice(i, 1))
                                    ma1 = True


                                Case 30
                                    If Val(matrice(i, 1)) = 1 Then
                                        da2 = True
                                    Else
                                        da2 = False
                                    End If

                                Case 31
                                    If Val(matrice(i, 1)) = 1 Then
                                        db2 = True
                                    Else
                                        db2 = False
                                    End If
                                Case 32
                                    pa2_val = Val(matrice(i, 1))
                                    pa2 = True
                                Case 33
                                    If numero_versione < 303 And ma2_enable Then
                                        ma2_val = Val(matrice(i, 1))
                                        ma2 = True
                                    Else
                                        pb2_val = Val(matrice(i, 1))
                                        pb2 = True
                                    End If
                                Case 34
                                    ma2_val = Val(matrice(i, 1))
                                    ma2 = True


                                Case 39
                                    If Val(matrice(i, 1)) = 1 Then
                                        da3 = True
                                    Else
                                        da3 = False
                                    End If

                                Case 40
                                    If Val(matrice(i, 1)) = 1 Then
                                        db3 = True
                                    Else
                                        db3 = False
                                    End If
                                Case 41
                                    If numero_versione < 303 And ma3_enable Then
                                        ma3_val = Val(matrice(i, 1))
                                        ma3 = True
                                    Else
                                        pa3_val = Val(matrice(i, 1))
                                        pa3 = True
                                    End If
                                Case 42
                                    pb3_val = Val(matrice(i, 1))
                                    pb3 = True
                                Case 43
                                    ma3_val = Val(matrice(i, 1))
                                    ma3 = True

                                Case 48
                                    If Val(matrice(i, 1)) = 1 Then
                                        da4 = True
                                    Else
                                        da4 = False
                                    End If
                                Case 49
                                    If numero_versione < 303 And ma4_enable Then
                                        ma4_val = Val(matrice(i, 1))
                                        ma4 = True

                                    Else
                                        If Val(matrice(i, 1)) = 1 Then
                                            db4 = True
                                        Else
                                            db4 = False
                                        End If
                                    End If
                                Case 50
                                    pa4_val = Val(matrice(i, 1))
                                    pa4 = True
                                Case 51
                                    pb4_val = Val(matrice(i, 1))
                                    pb4 = True
                                Case 52
                                    ma4_val = Val(matrice(i, 1))
                                    ma4 = True

                                Case 57
                                    If numero_versione < 303 And ma5_enable Then
                                        ma5_val = Val(matrice(i, 1))
                                        ma5 = True
                                    Else
                                        If Val(matrice(i, 1)) = 1 Then
                                            da5 = True
                                        Else
                                            da5 = False
                                        End If
                                    End If
                                Case 58
                                    If Val(matrice(i, 1)) = 1 Then
                                        db5 = True
                                    Else
                                        db5 = False
                                    End If
                                Case 59
                                    pa5_val = Val(matrice(i, 1))
                                    pa5 = True
                                Case 60
                                    pb5_val = Val(matrice(i, 1))
                                    pb5 = True
                                Case 61
                                    ma5_val = Val(matrice(i, 1))
                                    ma5 = True
                                Case 81 To 82
                                    valore_temp = ((matrice(i, 1) * 10) + matrice(i + 1, 1)) / 10
                                    i = i + 1
                                    temp_val = True
                                Case 83 To 84
                                    wm_val = True
                                    If yagel_version Then
                                        valore_wm = (((matrice(i, 1) * 100) + (matrice(i + 1, 1)))) / 10
                                    Else
                                        valore_wm = (((matrice(i, 1) * 100) + (matrice(i + 1, 1))))
                                    End If
                                    Exit For
                                Case 76
                                    If Val(matrice(i, 1)) = 1 Then
                                        valore_flow = "0"
                                    Else
                                        valore_flow = "1"
                                    End If
                                    flow_val = True
                                Case 77
                                    If Val(matrice(i, 1)) = 1 Then
                                        stby_val = True
                                    End If


                                    'Exit For
                            End Select

                        Next
                        If ch1_val = False Then
                            valore_ch1 = 0

                        End If
                        If ch2_val = False Then
                            valore_ch2 = 0
                        End If
                        If ch3_val = False Then
                            valore_ch3 = 0
                        End If
                        If ch4_val = False Then
                            valore_ch4 = 0
                        End If
                        If ch5_val = False Then
                            valore_ch5 = 0
                        End If
                        If temp_val = False Then
                            valore_temp = 0
                        End If
                        If wm_val = False Then
                            valore_wm = 0
                        End If
                        If fml_val = False Then
                            val_fml = "0"
                        End If

                        If aa1 Then
                            aa1_val = "1"
                        Else
                            aa1_val = "0"
                        End If
                        If ab1 Then
                            ab1_val = "1"
                        Else
                            ab1_val = "0"
                        End If
                        If ad1 Then
                            ad1_val = "1"
                        Else
                            ad1_val = "0"
                        End If
                        If ar1 Then
                            ar1_val = "1"
                        Else
                            ar1_val = "0"
                        End If

                        If aa2 Then
                            aa2_val = "1"
                        Else
                            aa2_val = "0"
                        End If
                        If ab2 Then
                            ab2_val = "1"
                        Else
                            ab2_val = "0"
                        End If
                        If ad2 Then
                            ad2_val = "1"
                        Else
                            ad2_val = "0"
                        End If
                        If ar2 Then
                            ar2_val = "1"
                        Else
                            ar2_val = "0"
                        End If

                        If aa3 Then
                            aa3_val = "1"
                        Else
                            aa3_val = "0"
                        End If
                        If ab3 Then
                            ab3_val = "1"
                        Else
                            ab3_val = "0"
                        End If
                        If ad3 Then
                            ad3_val = "1"
                        Else
                            ad3_val = "0"
                        End If
                        If ar3 Then
                            ar3_val = "1"
                        Else
                            ar3_val = "0"
                        End If
                        If aa4 Then
                            aa4_val = "1"
                        Else
                            aa4_val = "0"
                        End If
                        If ab4 Then
                            ab4_val = "1"
                        Else
                            ab4_val = "0"
                        End If
                        If ad4 Then
                            ad4_val = "1"
                        Else
                            ad4_val = "0"
                        End If
                        If ar4 Then
                            ar4_val = "1"
                        Else
                            ar4_val = "0"
                        End If
                        If aa5 Then
                            aa5_val = "1"
                        Else
                            aa5_val = "0"
                        End If
                        If ab5 Then
                            ab5_val = "1"
                        Else
                            ab5_val = "0"
                        End If
                        If ad5 Then
                            ad5_val = "1"
                        Else
                            ad5_val = "0"
                        End If
                        If ar5 Then
                            ar5_val = "1"
                        Else
                            ar5_val = "0"
                        End If
                        If flow_val = False Then
                            valore_flow = "0"
                        End If

                        Return query.log_max5(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_ch3, valore_ch4, valore_ch5, aa1_val, ab1_val, ad1_val, ar1_val, aa2_val, ab2_val, ad2_val, ar2_val, aa3_val, ab3_val, ad3_val, ar3_val, aa4_val, ab4_val, ad4_val, ar4_val, aa5_val, ab5_val, ad5_val, ar5_val, valore_flow, valore_wm, valore_temp, val_fml, _
                                              da1, db1, pa1_val, pb1_val, ma1_val, da2, db2, pa2_val, pb2_val, ma2_val, da3, db3, pa3_val, pb3_val, ma3_val, da4, db4, pa4_val, pb4_val, ma4_val, da5, db5, pa5_val, pb5_val, ma5_val, stby_val)
                    Catch ex As Exception

                    End Try
                End If

            End If

        Catch ex As Exception

        End Try
    End Function
    Public Function return_lettura_ld(ByVal divisore As Integer, ByVal value As Double) As Double
        Select Case divisore
            Case 4
                Return value
            Case 1
                'Return Mid(stringa, 1, 1) & "." & Mid(stringa, 2, 3)
                value = value / 1000
                Return value
            Case 2
                'Return Mid(stringa, 1, 2) & "." & Mid(stringa, 3, 2)
                value = value / 100
                Return value
            Case 3
                'Return Mid(stringa, 1, 3) & "." & Mid(stringa, 4, 1)
                value = value / 10
                Return value
        End Select
        Return 0
    End Function
    Public Function decode_log_ld(ByVal identificativo As String, ByVal id_strumento As String, ByVal string_parse As String, ByVal divide_ch1 As Integer, ByVal divide_ch2 As Integer, ByVal divide_ch3 As Integer, ByVal full_1 As Integer, ByVal full_2 As Integer, ByVal full_3 As Integer,
                             ByVal ch1_value As Single, ByVal ch2_value As Single, ByVal ch3_value As Single, ByVal temperature_format As String, ByVal divide_ch4 As Integer) As Boolean
        Try

            Dim split_log_first() As String = string_parse.Split("&")
            Dim m31 As Boolean = False
            Dim m32 As Boolean = False
            Dim m33 As Boolean = False
            Dim m34 As Boolean = False
            Dim m35 As Boolean = False
            Dim m3 As Boolean = False

            Dim tot1 As Boolean = False
            Dim tot2 As Boolean = False
            Dim tot3 As Boolean = False
            Dim tot4 As Boolean = False
            Dim tot5 As Boolean = False
            Dim tot As Boolean = False
            Dim correctLog As Boolean = False

            If split_log_first.Length > 0 Then
                Dim split_log_second() As String = split_log_first(1).Split("g")
                If split_log_second.Length > 0 Then
                    Dim file_str As String
                    Dim split_log() As String = split_log_second(1).Split("?")
                    Dim matrice(100, 1) As String
                    file_str = split_log(0)
                    Dim i As Integer
                    Dim j As Integer
                    For i = 0 To split_log.Length - 2
                        matrice(j, 0) = split_log(i)
                        matrice(j, 1) = split_log(i + 1)
                        If Val(matrice(j, 0)) = 0 And Val(matrice(j, 1)) = 125 Then
                            correctLog = True
                        End If
                        If matrice(j, 0) = "126" Then
                            matrice(j, 0) = "13"
                        End If
                        If matrice(j, 1) = "126" Then
                            matrice(j, 1) = "13"
                        End If
                        If matrice(j, 0) = "127" Then
                            matrice(j, 0) = "38"
                        End If
                        If matrice(j, 1) = "127" Then
                            matrice(j, 1) = "38"
                        End If

                        j = j + 1
                        i = i + 1
                        If i > split_log.Length - 1 Or j > 99 Then
                            Exit For
                        End If
                    Next

                    Dim data_str As String = ""
                    Dim condition As String = ""
                    Dim insert As String = ""
                    Dim ch1_val As Boolean = False
                    Dim ch2_val As Boolean = False
                    Dim ch3_val As Boolean = False
                    Dim ch4_val As Boolean = False

                    Dim level_ch1 As Boolean = False
                    Dim level_ch2 As Boolean = False
                    Dim level_ch3 As Boolean = False
                    Dim allarme_d1 As Boolean = False
                    Dim allarme_d2 As Boolean = False
                    Dim allarme_p1 As Boolean = False
                    Dim allarme_p2 As Boolean = False
                    Dim allarme_s1 As Boolean = False
                    Dim allarme_s2 As Boolean = False
                    Dim allarme_s3 As Boolean = False
                    Dim allarme_s4 As Boolean = False
                    Dim flow_val As Boolean = False
                    Dim livello1 As Boolean = False
                    Dim livello2 As Boolean = False
                    Dim livello3 As Boolean = False
                    Dim temperatura As Boolean = False
                    Dim stby As Boolean = False

                    Dim valore_ch1 As Single = 0
                    Dim valore_ch2 As Single = 0
                    Dim valore_ch3 As Single = 0
                    Dim valore_ch4 As Single = 0

                    Dim level1_val As String = ""
                    Dim level2_val As String = ""
                    Dim level3_val As String = ""
                    Dim allarmed1_val As String = ""
                    Dim allarmed2_val As String = ""
                    Dim allarmep1_val As String = ""
                    Dim allarmep2_val As String = ""
                    Dim allarmes1_val As String = ""
                    Dim allarmes2_val As String = ""
                    Dim livello1_val As String = ""
                    Dim livello2_val As String = ""
                    Dim livello3_val As String = ""
                    Dim temperatura_val As Single = 0
                    Dim tot_iput As Single = 0
                    Dim m3h As Single = 0

                    Dim data As Date
                    Dim valore_flow As String = ""
                    Dim insert_flow As String = ""
                    Dim query_select As String = ""
                    Dim query_insert As String = ""
                    Dim debug_dosim As String = ""

                    Dim Stato_pulse1_ch1 As Boolean = False '28
                    Dim Stato_pulse1_ch1_val As Integer = 0 '28
                    Dim Stato_pulse2_ch1 As Boolean = False '29
                    Dim Stato_pulse2_ch1_val As Integer = 0 '29
                    Dim Stato_pulse_ch2 As Boolean = False '30
                    Dim Stato_pulse_ch2_val As Integer = 0 '30
                    Dim Stato_rele_ch1 As Boolean = False '31
                    Dim Stato_rele_ch1_val As Integer = 0 '31
                    Dim Stato_rele_ch2 As Boolean = False '32
                    Dim Stato_rele_ch2_val As Integer = 0 '32

                    '              Set_point_pulse1_ch1 = 23,Set_point_pulse2_ch1=24,Set_point_pulse_ch2=25,Set_point_rele_ch1=26,
                    'Set_point_rele_ch2 = 27,Stato_pulse1_ch1=28,Stato_pulse2_ch1=29,Stato_pulse_ch2=30,Stato_rele_ch1=31,Stato_rele_ch2=32,
                    Try
                        If identificativo = "234978" Then
                            valore_flow = ""
                        End If

                        If identificativo = "681919" Or identificativo = "132028" Then
                            For i = 0 To 99
                                debug_dosim = debug_dosim + Val(matrice(i, 0)).ToString + "," + Val(matrice(i, 1)).ToString + ";"
                            Next
                            Try
                                Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log_" + identificativo + ".txt", True)
                                    If Not correctLog Then
                                        writer.WriteLine("Errato#" + debug_dosim + "#")
                                    Else
                                        writer.WriteLine("#" + debug_dosim + "#")
                                    End If

                                    writer.Close()
                                End Using
                            Catch ex As Exception

                            End Try
                        End If
                        If Not correctLog Then
                            Return True

                        End If
                        data = New Date(2000 + matrice(2, 1), matrice(1, 1), matrice(0, 1), matrice(3, 1), matrice(4, 1), 0)
                        For i = 5 To 99
                            'mese giorno e anno
                            Select Case Val(matrice(i, 0))
                                Case 13 To 14
                                    If ch1_val = False And i < 10 Then
                                        valore_ch1 = return_lettura(divide_ch1, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        If Not filtra_valore(valore_ch1, full_1) Then
                                            valore_ch1 = ch1_value
                                        End If
                                        i = i + 1
                                        ch1_val = True
                                    End If

                                Case 15 To 16
                                    If ch2_val = False Then
                                        If (Val(matrice(i, 1))) > 128 Then 'per redox negativo
                                            Dim redox_negativo As Integer = (Val(matrice(i, 1))) - 256
                                            redox_negativo = (redox_negativo * 100) - Val(matrice(i + 1, 1))
                                            valore_ch2 = return_lettura(divide_ch2, redox_negativo)
                                        Else
                                            valore_ch2 = return_lettura(divide_ch2, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        End If

                                        'If Not filtra_valore(valore_ch2, full_2) Then
                                        'valore_ch2 = ch2_value
                                        'End If
                                        i = i + 1
                                        ch2_val = True
                                    End If
                                Case 38 To 39
                                    If ch3_val = False Then
                                        valore_ch3 = return_lettura(divide_ch3, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        If Not filtra_valore(valore_ch3, full_3) Then
                                            valore_ch3 = ch3_value
                                        End If
                                        i = i + 1
                                        ch3_val = True
                                    End If
                                Case 52 To 53 ' per ld 3
                                    If ch3_val = False Then
                                        valore_ch2 = valore_ch2 * 10 ' moltiplico il valore delk secondo canale per 10, legato al numero di cifre dell LD3 che sono 3 sul display
                                        valore_ch3 = return_lettura(divide_ch3, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        'If Not filtra_valore(valore_ch3, full_3) Then
                                        '    valore_ch3 = ch3_value
                                        'End If
                                        i = i + 1
                                        ch3_val = True
                                    End If
                                Case 54 To 55 ' per ld 3
                                    If ch4_val = False Then
                                        valore_ch4 = return_lettura(divide_ch4, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        'If Not filtra_valore(valore_ch3, full_3) Then
                                        '    valore_ch3 = ch3_value
                                        'End If
                                        i = i + 1
                                        ch4_val = True
                                    End If
                                Case 56
                                    If Val(matrice(i, 1)) = 1 Then
                                        allarme_s3 = True
                                    End If
                                Case 57
                                    If Val(matrice(i, 1)) = 1 Then
                                        allarme_s4 = True
                                    End If

                                Case 19 'Allarme d1
                                    allarme_d1 = True
                                Case 20 'Allarme d2
                                    allarme_d2 = True
                                Case 21 'A probe
                                    allarme_p1 = True
                                Case 22 'A probe 2
                                    allarme_p2 = True
                                Case 35 'A allarme soglia 1
                                    allarme_s1 = True
                                Case 36 'allarme soglia 2
                                    allarme_s2 = True
                                    'Exit For
                                Case 28
                                    Stato_pulse1_ch1 = True
                                    Stato_pulse1_ch1_val = Val(matrice(i, 1))
                                Case 29
                                    Stato_pulse2_ch1 = True
                                    Stato_pulse2_ch1_val = Val(matrice(i, 1))
                                Case 30
                                    Stato_pulse_ch2 = True
                                    Stato_pulse_ch2_val = Val(matrice(i, 1))
                                Case 31
                                    Stato_rele_ch1 = True
                                    Stato_rele_ch1_val = Val(matrice(i, 1))
                                Case 32
                                    Stato_rele_ch2 = True
                                    Stato_rele_ch2_val = Val(matrice(i, 1))
                                Case 8
                                    flow_val = True
                                Case 9
                                    If Val(matrice(i, 1)) = 1 Then
                                        livello1 = True
                                    End If
                                Case 10
                                    If Val(matrice(i, 1)) = 1 Then
                                        livello2 = True
                                    End If

                                Case 11
                                    If Val(matrice(i, 1)) = 1 Then
                                        livello3 = True
                                    End If
                                Case 17 To 18
                                    'If temperatura = "1" Then '°F
                                    '    condition_temperatura = condition_temperatura + " And (Temperatura  ='" + Format(matrice(i, 1), "00") + Format(matrice(i + 1, 1), "0") + "')"
                                    '    insert_temperatura = insert_temperatura + ",'" + Format(matrice(i, 1), "00") + Format(matrice(i + 1, 1), "0") + "'"
                                    'Else '°C
                                    If temperature_format = "1" Then ' formato americano
                                        temperatura_val = ((matrice(i, 1) * 10) + matrice(i + 1, 1))
                                    Else
                                        temperatura_val = ((matrice(i, 1) * 10) + matrice(i + 1, 1)) / 10
                                    End If

                                    'End If
                                    i = i + 1
                                    temperatura = True
                                Case 41
                                    If Val(matrice(i, 1)) = 1 Then
                                        stby = True
                                    End If
                                Case 42 To 46 'm3/h
                                    If Val(matrice(i, 0)) = 42 Then
                                        m31 = True
                                    End If
                                    If Val(matrice(i + 1, 0)) = 43 Then
                                        m32 = True
                                    End If
                                    If Val(matrice(i + 2, 0)) = 44 Then
                                        m33 = True
                                    End If
                                    If Val(matrice(i + 3, 0)) = 45 Then
                                        m34 = True
                                    End If
                                    If Val(matrice(i + 4, 0)) = 46 Then
                                        m35 = True
                                    End If
                                    If m31 And m32 And m33 And m34 And m35 Then

                                    End If
                                    If m3 = False Then
                                        m3h = Val(matrice(i, 1)) * 1000000000
                                        m3h = m3h + Val(matrice(i + 1, 1)) * 10000000
                                        m3h = m3h + Val(matrice(i + 2, 1)) * 100000
                                        m3h = m3h + Val(matrice(i + 3, 1)) * 1000
                                        m3h = m3h + Val(matrice(i + 4, 1)) * 10
                                        m3h = m3h / 100000

                                    End If
                                    If m31 And m32 And m33 And m34 And m35 And m3 = False Then
                                        m3 = True
                                        i = i + 4
                                    End If


                                Case 47 To 51 'totalizer
                                    If Val(matrice(i, 0)) = 47 Then
                                        tot1 = True
                                    End If
                                    If Val(matrice(i + 1, 0)) = 48 Then
                                        tot2 = True
                                    End If
                                    If Val(matrice(i + 2, 0)) = 49 Then
                                        tot3 = True
                                    End If
                                    If Val(matrice(i + 3, 0)) = 50 Then
                                        tot4 = True
                                    End If
                                    If Val(matrice(i + 4, 0)) = 51 Then
                                        tot5 = True
                                    End If
                                    If tot = False Then
                                        tot_iput = Val(matrice(i, 1)) * 100000000
                                        tot_iput = tot_iput + Val(matrice(i + 1, 1)) * 1000000
                                        tot_iput = tot_iput + Val(matrice(i + 2, 1)) * 10000
                                        tot_iput = tot_iput + Val(matrice(i + 3, 1)) * 100
                                        tot_iput = tot_iput + Val(matrice(i + 4, 1)) * 1
                                    End If
                                    If tot1 And tot2 And tot3 And tot4 And tot5 And tot = False Then
                                        i = i + 4
                                        tot = True
                                    End If

                            End Select

                        Next
                        If m31 Or m32 Or m33 Or m34 Or m35 Then
                            If m31 And m32 And m33 And m34 And m35 Then
                            Else
                                Return True
                            End If
                        End If
                        If tot1 Or tot2 Or tot3 Or tot4 Or tot5 Then
                            If tot1 And tot2 And tot3 And tot4 And tot5 Then
                            Else
                                Return True
                            End If
                        End If

                        If ch1_val = False And ch2_val = False And ch3_val = False Then
                            Return True
                        End If
                        If ch1_val = False Then
                            valore_ch1 = 0

                        End If
                        If ch2_val = False Then
                            valore_ch2 = 0
                        End If
                        If ch3_val = False Then
                            valore_ch3 = 0
                        End If
                        If ch4_val = False Then
                            valore_ch4 = 0
                        End If

                        If allarme_d1 Then
                            allarmed1_val = "1"
                        Else
                            allarmed1_val = "0"
                        End If
                        If allarme_d2 Then
                            allarmed2_val = "1"
                        Else
                            allarmed2_val = "0"
                        End If
                        If allarme_p1 Then
                            allarmep1_val = "1"
                        Else
                            allarmep1_val = "0"
                        End If
                        If allarme_p2 Then
                            allarmep2_val = "1"
                        Else
                            allarmep2_val = "0"
                        End If

                        If allarme_s1 Then
                            allarmes1_val = "1"
                        Else
                            allarmes1_val = "0"
                        End If
                        If allarme_s2 Then
                            allarmes2_val = "1"
                        Else
                            allarmes2_val = "0"
                        End If

                        If flow_val = False Then
                            valore_flow = "0"
                        Else
                            valore_flow = "1"
                        End If
                        If livello1 = False Then
                            livello1_val = "0"
                        Else
                            livello1_val = "1"
                        End If
                        If livello2 = False Then
                            livello2_val = "0"
                        Else
                            livello2_val = "1"
                        End If
                        If livello3 = False Then
                            livello3_val = "0"
                        Else
                            livello3_val = "1"
                        End If
                        If temperatura = False Then
                            temperatura_val = 0

                        End If


                        Return query.log_ld(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_ch3, valore_flow, allarmes1_val, allarmes2_val, allarmed1_val, allarmed2_val, allarmep1_val, allarmep2_val, livello1_val, livello2_val, temperatura_val, livello3_val, stby, m3h, tot_iput, Stato_pulse1_ch1_val, Stato_pulse2_ch1_val, Stato_pulse_ch2_val, Stato_rele_ch1_val, Stato_rele_ch2_val, valore_ch4, allarme_s3, allarme_s4)
                    Catch ex As Exception

                    End Try
                End If

            End If
        Catch ex As Exception

        End Try
    End Function
    Public Function decode_log_ld4(ByVal identificativo As String, ByVal id_strumento As String, ByVal string_parse As String, ByVal divide_ch1 As Integer, ByVal divide_ch2 As Integer, ByVal divide_ch3 As Integer, ByVal divide_ch4 As Integer, ByVal full_1 As Integer, ByVal full_2 As Integer, ByVal full_3 As Integer, ByVal full_4 As Integer, _
                         ByVal ch1_value As Single, ByVal ch2_value As Single, ByVal ch3_value As Single, ByVal ch4_value As Single, ByVal temperature_format As String) As Boolean
        Try

            Dim split_log_first() As String = string_parse.Split("&")
            If split_log_first.Length > 0 Then
                Dim split_log_second() As String = split_log_first(1).Split("g")
                If split_log_second.Length > 0 Then
                    Dim file_str As String
                    Dim split_log() As String = split_log_second(1).Split("?")
                    Dim matrice(100, 1) As String
                    file_str = split_log(0)
                    Dim i As Integer
                    Dim j As Integer
                    For i = 0 To split_log.Length - 2
                        matrice(j, 0) = split_log(i)
                        matrice(j, 1) = split_log(i + 1)
                        If matrice(j, 0) = "126" Then
                            matrice(j, 0) = "13"
                        End If
                        If matrice(j, 1) = "126" Then
                            matrice(j, 1) = "13"
                        End If
                        If matrice(j, 0) = "127" Then
                            matrice(j, 0) = "38"
                        End If
                        If matrice(j, 1) = "127" Then
                            matrice(j, 1) = "38"
                        End If

                        j = j + 1
                        i = i + 1
                        If i > split_log.Length - 1 Or j > 99 Then
                            Exit For
                        End If
                    Next
                    Dim data_str As String = ""
                    Dim condition As String = ""
                    Dim insert As String = ""
                    Dim ch1_val As Boolean = False
                    Dim ch2_val As Boolean = False
                    Dim ch3_val As Boolean = False
                    Dim ch4_val As Boolean = False

                    Dim level_ch1 As Boolean = False
                    Dim level_ch2 As Boolean = False
                    Dim level_ch3 As Boolean = False
                    Dim allarme_d1 As Boolean = False
                    Dim allarme_d2 As Boolean = False
                    Dim allarme_p1 As Boolean = False
                    Dim allarme_p2 As Boolean = False
                    Dim allarme_s1 As Boolean = False
                    Dim allarme_s2 As Boolean = False
                    Dim allarme_s3 As Boolean = False
                    Dim allarme_s4 As Boolean = False

                    Dim flow_val As Boolean = False
                    Dim livello1 As Boolean = False
                    Dim livello2 As Boolean = False
                    Dim flow_meter_low As Boolean = False
                    Dim livello3 As Boolean = False
                    Dim temperatura As Boolean = False

                    Dim valore_ch1 As Single = 0
                    Dim valore_ch2 As Single = 0
                    Dim valore_ch3 As Single = 0
                    Dim valore_ch4 As Single = 0
                    Dim m3h As Single = 0

                    Dim level1_val As String = ""
                    Dim level2_val As String = ""
                    Dim level3_val As String = ""
                    Dim allarmed1_val As String = ""
                    Dim allarmed2_val As String = ""
                    Dim allarmep1_val As String = ""
                    Dim allarmep2_val As String = ""
                    Dim allarmes1_val As String = ""
                    Dim allarmes2_val As String = ""
                    Dim livello1_val As String = ""
                    Dim livello2_val As String = ""
                    Dim livello3_val As String = ""
                    Dim temperatura_val As Single = 0

                    Dim data As Date
                    Dim valore_flow As String = ""
                    Dim insert_flow As String = ""
                    Dim query_select As String = ""
                    Dim query_insert As String = ""
                    Try
                        data = New Date(2000 + matrice(2, 1), matrice(1, 1), matrice(0, 1), matrice(3, 1), matrice(4, 1), 0)
                        For i = 5 To 99
                            'mese giorno e anno
                            Select Case Val(matrice(i, 0))
                                Case 12 To 13
                                    If ch1_val = False And i < 10 Then
                                        valore_ch1 = return_lettura(divide_ch1, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        If Not filtra_valore(valore_ch1, full_1) Then
                                            valore_ch1 = ch1_value
                                        End If
                                        i = i + 1
                                        ch1_val = True
                                    End If

                                Case 14 To 15
                                    If ch2_val = False Then
                                        valore_ch2 = return_lettura(divide_ch2, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        If Not filtra_valore(valore_ch2, full_2) Then
                                            valore_ch2 = ch2_value
                                        End If
                                        i = i + 1
                                        ch2_val = True
                                    End If

                                Case 16 To 17
                                    If ch3_val = False Then
                                        valore_ch3 = return_lettura(divide_ch3, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        If Not filtra_valore(valore_ch3, full_3) Then
                                            valore_ch3 = ch3_value
                                        End If
                                        i = i + 1
                                        ch3_val = True
                                    End If
                                Case 18 To 19
                                    If ch4_val = False Then
                                        valore_ch4 = return_lettura(divide_ch4, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        If Not filtra_valore(valore_ch4, full_4) Then
                                            valore_ch4 = ch4_value
                                        End If
                                        i = i + 1
                                        ch4_val = True
                                    End If
                                Case 23 'Allarme d1
                                    allarme_d1 = True
                                Case 24 'Allarme d2
                                    allarme_d2 = True
                                Case 25 'A probe
                                    allarme_p1 = True
                                Case 26 'A probe 2
                                    allarme_p2 = True

                                Case 29 'A allarme soglia 1
                                    allarme_s1 = True
                                Case 30 'allarme soglia 2
                                    allarme_s2 = True
                                Case 31 'allarme soglia 3
                                    allarme_s3 = True
                                Case 32 'allarme soglia 4
                                    allarme_s4 = True

                                Case 9

                                    flow_val = True
                                Case 10
                                    If Val(matrice(i, 1)) = 1 Then
                                        livello1 = True
                                    End If
                                Case 11
                                    If Val(matrice(i, 1)) = 1 Then
                                        livello2 = True
                                    End If
                                Case 38 ' flow_meter_low
                                    If Val(matrice(i, 1)) = 1 Then
                                        flow_meter_low = True
                                    End If

                                Case 39 To 43
                                    m3h = (matrice(i, 1) * 10000) + (matrice(i + 1, 1) * 1000) + (matrice(i + 2, 1) * 100) + (matrice(i + 3, 1) * 10) + (matrice(i + 4, 1) * 1)
                                    m3h = m3h / 100
                                    Exit For
                                Case 20 To 21
                                    'If temperatura = "1" Then '°F
                                    '    condition_temperatura = condition_temperatura + " And (Temperatura  ='" + Format(matrice(i, 1), "00") + Format(matrice(i + 1, 1), "0") + "')"
                                    '    insert_temperatura = insert_temperatura + ",'" + Format(matrice(i, 1), "00") + Format(matrice(i + 1, 1), "0") + "'"
                                    'Else '°C
                                    If temperature_format = "1" Then ' formato americano
                                        temperatura_val = ((matrice(i, 1) * 10) + matrice(i + 1, 1))
                                    Else
                                        temperatura_val = ((matrice(i, 1) * 10) + matrice(i + 1, 1)) / 10
                                    End If

                                    'End If
                                    i = i + 1
                                    temperatura = True

                            End Select

                        Next
                        If ch1_val = False Then
                            valore_ch1 = 0

                        End If
                        If ch2_val = False Then
                            valore_ch2 = 0
                        End If
                        If ch3_val = False Then
                            valore_ch3 = 0
                        End If
                        If allarme_d1 Then
                            allarmed1_val = "1"
                        Else
                            allarmed1_val = "0"
                        End If
                        If allarme_d2 Then
                            allarmed2_val = "1"
                        Else
                            allarmed2_val = "0"
                        End If
                        If allarme_p1 Then
                            allarmep1_val = "1"
                        Else
                            allarmep1_val = "0"
                        End If
                        If allarme_p2 Then
                            allarmep2_val = "1"
                        Else
                            allarmep2_val = "0"
                        End If

                        If allarme_s1 Then
                            allarmes1_val = "1"
                        Else
                            allarmes1_val = "0"
                        End If
                        If allarme_s2 Then
                            allarmes2_val = "1"
                        Else
                            allarmes2_val = "0"
                        End If

                        If flow_val = False Then
                            valore_flow = "0"
                        Else
                            valore_flow = "1"
                        End If
                        If livello1 = False Then
                            livello1_val = "0"
                        Else
                            livello1_val = "1"
                        End If
                        If livello2 = False Then
                            livello2_val = "0"
                        Else
                            livello2_val = "1"
                        End If
                        If livello3 = False Then
                            livello3_val = "0"
                        Else
                            livello3_val = "1"
                        End If
                        If temperatura = False Then
                            temperatura_val = 0

                        End If

                        Return query.log_ld4(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_ch3, valore_ch4, valore_flow, level_ch1, level_ch2, allarme_d1, allarme_d1, allarme_p1, allarme_p1, allarme_s1, allarme_s2, allarme_s3, allarme_s4, False, temperatura_val, m3h, 0, flow_meter_low)
                    Catch ex As Exception

                    End Try
                End If

            End If
        Catch ex As Exception

        End Try
    End Function
    Public Function decode_log_wd(ByVal identificativo As String, ByVal id_strumento As String, ByVal string_parse As String, ByVal divide_ch1 As Integer, ByVal divide_ch2 As Integer, ByVal divide_ch3 As Integer, ByVal full_1 As Integer, ByVal full_2 As Integer, ByVal full_3 As Integer, _
                             ByVal ch1_value As Single, ByVal ch2_value As Single, ByVal ch3_value As Single) As Boolean
        Try
            Dim split_log_first() As String = string_parse.Split("&")
            If split_log_first.Length > 0 Then
                Dim split_log_second() As String = split_log_first(1).Split("g")
                If split_log_second.Length > 0 Then
                    Dim file_str As String
                    Dim split_log() As String = split_log_second(1).Split("?")
                    Dim matrice(100, 1) As String
                    file_str = split_log(0)
                    Dim i As Integer
                    Dim j As Integer
                    For i = 0 To split_log.Length - 2
                        matrice(j, 0) = split_log(i)
                        matrice(j, 1) = split_log(i + 1)
                        If matrice(j, 0) = "126" Then
                            matrice(j, 0) = "13"
                        End If
                        If matrice(j, 1) = "126" Then
                            matrice(j, 1) = "13"
                        End If
                        If matrice(j, 0) = "127" Then
                            matrice(j, 0) = "38"
                        End If
                        If matrice(j, 1) = "127" Then
                            matrice(j, 1) = "38"
                        End If

                        j = j + 1
                        i = i + 1
                        If i > split_log.Length - 1 Or j > 99 Then
                            Exit For
                        End If
                    Next
                    Dim data_str As String = ""
                    Dim condition As String = ""
                    Dim insert As String = ""
                    Dim ch1_val As Boolean = False
                    Dim ch2_val As Boolean = False
                    Dim ch3_val As Boolean = False

                    Dim level_ch1 As Boolean = False
                    Dim level_ch2 As Boolean = False
                    Dim level_ch3 As Boolean = False
                    Dim allarme_d1 As Boolean = False
                    Dim allarme_d2 As Boolean = False
                    Dim allarme_p1 As Boolean = False
                    Dim allarme_p2 As Boolean = False
                    Dim allarme_s1 As Boolean = False
                    Dim allarme_s2 As Boolean = False
                    Dim flow_val As Boolean = False
                    Dim livello1 As Boolean = False
                    Dim livello2 As Boolean = False
                    Dim livello3 As Boolean = False

                    Dim valore_ch1 As Single = 0
                    Dim valore_ch2 As Single = 0
                    Dim valore_ch3 As Single = 0

                    Dim rele_ch1 As Integer = 0
                    Dim rele_ch2 As Integer = 0

                    Dim level1_val As String = ""
                    Dim level2_val As String = ""
                    Dim level3_val As String = ""
                    Dim allarmed1_val As String = ""
                    Dim allarmed2_val As String = ""
                    Dim allarmep1_val As String = ""
                    Dim allarmep2_val As String = ""
                    Dim allarmes1_val As String = ""
                    Dim allarmes2_val As String = ""
                    Dim livello1_val As String = ""
                    Dim livello2_val As String = ""
                    Dim livello3_val As String = ""
                    Dim data As Date
                    Dim valore_flow As String = ""
                    Dim insert_flow As String = ""
                    Dim query_select As String = ""
                    Dim query_insert As String = ""
                    Dim debug_dosim As String = ""
                    Try
                        'If identificativo = "671140" Then
                        '    For i = 0 To 99
                        '        debug_dosim = debug_dosim + Val(matrice(i, 0)).ToString + "," + Val(matrice(i, 1)).ToString + ";"
                        '    Next
                        '    Try
                        '        Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log_671140.txt", True)
                        '            writer.WriteLine("#" + debug_dosim + "#")
                        '            writer.Close()
                        '        End Using
                        '    Catch ex As Exception

                        '    End Try
                        'End If
                        data = New Date(2000 + matrice(2, 1), matrice(1, 1), matrice(0, 1), matrice(3, 1), matrice(4, 1), 0)
                        For i = 5 To 99
                            'mese giorno e anno
                            Select Case Val(matrice(i, 0))
                                Case 13 To 14
                                    If ch1_val = False And i < 10 Then
                                        valore_ch1 = return_lettura(divide_ch1, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        If Not filtra_valore(valore_ch1, full_1) Then
                                            valore_ch1 = ch1_value
                                        End If
                                        i = i + 1
                                        ch1_val = True
                                    End If
                                Case 15 To 16
                                    If ch2_val = False Then
                                        valore_ch2 = return_lettura(divide_ch2, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        If Not filtra_valore(valore_ch2, full_2) Then
                                            valore_ch2 = ch2_value
                                        End If
                                        i = i + 1
                                        ch2_val = True
                                    End If
                                Case 19 'Allarme d1
                                    allarme_d1 = True
                                Case 20 'Allarme d2
                                    allarme_d2 = True
                                Case 21 'A probe
                                    allarme_p1 = True
                                Case 22 'A probe 2
                                    allarme_p2 = True
                                    'Exit For
                                Case 31 'A probe 2
                                    rele_ch1 = Val(matrice(i, 1))
                                Case 32 'A probe 2
                                    rele_ch2 = Val(matrice(i, 1))
                                    Exit For

                                Case 8
                                    flow_val = True
                                Case 9
                                    If Val(matrice(i, 1)) = 1 Then
                                        livello1 = True
                                    End If

                                Case 10
                                    If Val(matrice(i, 1)) = 1 Then
                                        livello1 = True
                                    End If
                                Case 11
                                    If Val(matrice(i, 1)) = 1 Then
                                        livello2 = True
                                    End If
                            End Select

                        Next
                        If ch1_val = False Then
                            valore_ch1 = 0

                        End If
                        If ch2_val = False Then
                            valore_ch2 = 0
                        End If
                        If ch3_val = False Then
                            valore_ch3 = 0
                        End If
                        If allarme_d1 Then
                            allarmed1_val = "1"
                        Else
                            allarmed1_val = "0"
                        End If
                        If allarme_d2 Then
                            allarmed2_val = "1"
                        Else
                            allarmed2_val = "0"
                        End If
                        If allarme_p1 Then
                            allarmep1_val = "1"
                        Else
                            allarmep1_val = "0"
                        End If
                        If allarme_p2 Then
                            allarmep2_val = "1"
                        Else
                            allarmep2_val = "0"
                        End If

                        If flow_val = False Then
                            valore_flow = "0"
                        Else
                            valore_flow = "1"
                        End If
                        If livello1 = False Then
                            livello1_val = "0"
                        Else
                            livello1_val = "1"
                        End If
                        If livello2 = False Then
                            livello2_val = "0"
                        Else
                            livello2_val = "1"
                        End If
                        Return query.log_wd(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_flow, allarmed2_val, allarmed1_val, allarmep1_val, allarmep2_val, livello1_val, livello2_val, rele_ch1, rele_ch2)
                    Catch ex As Exception

                    End Try
                End If

            End If

        Catch ex As Exception

        End Try
    End Function
    Public Function decode_log_ltb(ByVal identificativo As String, ByVal id_strumento As String, ByVal string_parse As String, ByVal divide_ch1 As Integer, ByVal divide_ch2 As Integer, ByVal full_1 As Integer, ByVal full_2 As Integer, _
                        ByVal ch1_value As Single, ByVal ch2_value As Single) As Boolean
        Try
            Dim split_log_first() As String = string_parse.Split("&")
            If split_log_first.Length > 0 Then
                Dim split_log_second() As String = split_log_first(1).Split("g")
                If split_log_second.Length > 0 Then
                    Dim file_str As String
                    Dim split_log() As String = split_log_second(1).Split("?")
                    Dim matrice(100, 1) As String
                    file_str = split_log(0)
                    Dim i As Integer
                    Dim j As Integer
                    For i = 0 To split_log.Length - 2
                        matrice(j, 0) = split_log(i)
                        matrice(j, 1) = split_log(i + 1)
                        If matrice(j, 0) = "126" Then
                            matrice(j, 0) = "13"
                        End If
                        If matrice(j, 1) = "126" Then
                            matrice(j, 1) = "13"
                        End If
                        If matrice(j, 0) = "127" Then
                            matrice(j, 0) = "38"
                        End If
                        If matrice(j, 1) = "127" Then
                            matrice(j, 1) = "38"
                        End If

                        j = j + 1
                        i = i + 1
                        If i > split_log.Length - 1 Or j > 99 Then
                            Exit For
                        End If
                    Next
                    Dim data_str As String = ""
                    Dim condition As String = ""
                    Dim temperature_format As Integer = 0
                    Dim insert As String = ""
                    Dim ch1_val As Boolean = False
                    Dim ch2_val As Boolean = False

                    Dim temperatura_val As Boolean = False


                    Dim flusso As Boolean = False
                    Dim lev_hcl As Boolean = False

                    Dim lev_naclo2 As Boolean = False
                    Dim lev_k6 As Boolean = False
                    Dim temp_max As Boolean = False
                    Dim stop_l As Boolean = False

                    Dim lev_errata As Boolean = False


                    Dim valore_ch1 As Single = 0
                    Dim valore_ch2 As Single = 0
                    Dim valore_temperatura As Single = 0
                    Dim data As Date
                    Dim query_select As String = ""
                    Dim query_insert As String = ""
                    Dim totAcqua As Single = 0
                    Dim totAcqua_b As Boolean = False

                    Try
                        data = New Date(2000 + matrice(2, 1), matrice(1, 1), matrice(0, 1), matrice(3, 1), matrice(4, 1), 0)
                        For i = 5 To 99
                            'mese giorno e anno
                            Select Case Val(matrice(i, 0))
                                Case 9 To 10
                                    If ch1_val = False And i < 10 Then
                                        valore_ch1 = return_lettura(divide_ch1, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        If Not filtra_valore(valore_ch1, full_1) Then
                                            valore_ch1 = ch1_value
                                        End If
                                        i = i + 1
                                        ch1_val = True
                                    End If
                                Case 11 To 12
                                    If ch2_val = False Then
                                        valore_ch2 = return_lettura(divide_ch2, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        If Not filtra_valore(valore_ch2, full_2) Then
                                            valore_ch2 = ch2_value
                                        End If
                                        i = i + 1
                                        ch2_val = True
                                    End If
                                Case 8
                                    temperature_format = Val(matrice(i, 1))
                                Case 13 To 14
                                    'If temperatura = "1" Then '°F
                                    '    condition_temperatura = condition_temperatura + " And (Temperatura  ='" + Format(matrice(i, 1), "00") + Format(matrice(i + 1, 1), "0") + "')"
                                    '    insert_temperatura = insert_temperatura + ",'" + Format(matrice(i, 1), "00") + Format(matrice(i + 1, 1), "0") + "'"
                                    'Else '°C
                                    If temperatura_val = False Then
                                        If temperature_format <> 2 Then ' formato americano
                                            valore_temperatura = ((matrice(i, 1) * 10) + matrice(i + 1, 1)) / 10

                                        Else
                                            valore_temperatura = ((matrice(i, 1) * 10) + matrice(i + 1, 1))
                                        End If

                                        'End If
                                        i = i + 1
                                    End If
                                    temperatura_val = True

                                Case 15 'flusso
                                    If Val(matrice(i, 1)) = 1 Then
                                        flusso = True
                                    End If
                                Case 16 'Allarme d2
                                    If Val(matrice(i, 1)) = 1 Then
                                        lev_hcl = True
                                    End If

                                Case 17 'A probe
                                    If Val(matrice(i, 1)) = 1 Then
                                        lev_naclo2 = True
                                    End If
                                Case 18 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        lev_k6 = True
                                    End If
                                Case 19 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        temp_max = True
                                    End If
                                Case 20 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        stop_l = True
                                    End If
                                Case 21 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        lev_errata = True
                                    End If

                                Case 25 To 28
                                    totAcqua = (Val(matrice(i, 1)) * 100000 + Val(matrice(i + 1, 1)) * 1000 + Val(matrice(i + 2, 1)) * 10 + Val(matrice(i + 3, 1)) * 1) / 10
                                    i = i + 3
                                    totAcqua_b = True

                            End Select

                        Next
                        If ch1_val = False Then
                            valore_ch1 = 0

                        End If
                        If ch2_val = False Then
                            valore_ch2 = 0
                        End If
                        If temperatura_val = False Then
                            valore_temperatura = 0
                        End If
                        Return query.log_ltb(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_temperatura, flusso, lev_hcl,
                                             lev_naclo2, lev_k6, temp_max, stop_l, lev_errata, totAcqua)

                        'Return query.log_wd(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_flow, allarmed2_val, allarmed1_val, allarmep1_val, allarmep2_val, livello1_val, livello2_val)

                    Catch ex As Exception

                    End Try
                End If

            End If

        Catch ex As Exception

        End Try
    End Function
    Public Function decode_log_lta_ltu_ltd(ByVal identificativo As String, ByVal id_strumento As String, ByVal string_parse As String, ByVal divide_ch1 As Integer, ByVal divide_ch2 As Integer, ByVal full_1 As Integer, ByVal full_2 As Integer, _
                    ByVal ch1_value As Single, ByVal ch2_value As Single) As Boolean
        Dim correctLog As Boolean = False

        Try
            Dim split_log_first() As String = string_parse.Split("&")
            If split_log_first.Length > 0 Then
                Dim split_log_second() As String = split_log_first(1).Split("g")
                If split_log_second.Length > 0 Then
                    Dim file_str As String
                    Dim split_log() As String = split_log_second(1).Split("?")
                    Dim matrice(100, 1) As String
                    file_str = split_log(0)
                    Dim i As Integer
                    Dim j As Integer
                    For i = 0 To split_log.Length - 2
                        matrice(j, 0) = split_log(i)
                        matrice(j, 1) = split_log(i + 1)
                        If Val(matrice(j, 0)) = 0 And Val(matrice(j, 1)) = 125 Then
                            correctLog = True
                        End If

                        If matrice(j, 0) = "126" Then
                            matrice(j, 0) = "13"
                        End If
                        If matrice(j, 1) = "126" Then
                            matrice(j, 1) = "13"
                        End If
                        If matrice(j, 0) = "127" Then
                            matrice(j, 0) = "38"
                        End If
                        If matrice(j, 1) = "127" Then
                            matrice(j, 1) = "38"
                        End If

                        j = j + 1
                        i = i + 1
                        If i > split_log.Length - 1 Or j > 99 Then
                            Exit For
                        End If
                    Next
                    Dim data_str As String = ""
                    Dim debug_dosim As String = ""
                    Dim condition As String = ""
                    Dim temperature_format As Integer = 0
                    Dim insert As String = ""


                    Dim Flow_Acid_L As Boolean = False
                    Dim Flow_Self_Water_L As Boolean = False

                    Dim Flow_Chlorite_L As Boolean = False
                    Dim Lev_Acid_L As Boolean = False
                    Dim Lev_Chlorite_L As Boolean = False
                    Dim Lev_Water_L As Boolean = False
                    Dim Sefl_Acid_L As Boolean = False

                    Dim Sefl_Chlorite_L As Boolean = False
                    Dim TimeOut_L As Boolean = False
                    Dim Flow_Water_dil_L As Boolean = False
                    Dim Service_F_L As Boolean = False

                    Dim Analog_L As Boolean = False
                    Dim Tank_Empty_L As Boolean = False
                    Dim Level_SW As Boolean = False
                    Dim BypassB As Boolean = False
                    Dim Lim_Dioxide As Boolean = False
                    Dim Lev_Alflow As Boolean = False
                    Dim Overf As Boolean = False
                    Dim LevSoglia As Boolean = False

                    Dim flow1 As Single = 0
                    Dim flow1_b As Boolean = False
                    Dim flow2 As Single = 0
                    Dim flow2_b As Boolean = False

                    Dim totAcido As Single = 0
                    Dim totAcido_b As Boolean = False

                    Dim totCloro As Single = 0
                    Dim totCloro_b As Boolean = False

                    Dim totAcqua As Single = 0
                    Dim totAcqua_b As Boolean = False

                    Dim totAcidoDay As Single = 0
                    Dim totAcidoDay_b As Boolean = False

                    Dim totCloroDay As Single = 0
                    Dim totCloroDay_b As Boolean = False

                    Dim totAcquaDay As Single = 0
                    Dim totAcquaDay_b As Boolean = False

                    Dim setpoint As Single = 0
                    Dim setpoint_b As Boolean = False

                    Dim lettura As Single = 0
                    Dim lettura_b As Boolean = False
                    Dim naso As Single = 0
                    Dim naso_b As Boolean = False
                    Dim temperatura As Single = 0
                    Dim temperatura_b As Boolean = False

                    Dim data As Date
                    Dim query_select As String = ""
                    Dim query_insert As String = ""

                    If identificativo = "884884" Then
                        For i = 0 To 99
                            debug_dosim = debug_dosim + Val(matrice(i, 0)).ToString + "," + Val(matrice(i, 1)).ToString + ";"
                        Next
                        Try
                            Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log_884884.txt", True)

                                writer.WriteLine("#" + debug_dosim + "#")

                                writer.Close()
                            End Using
                        Catch ex As Exception

                        End Try
                    End If

                    If Not correctLog Then
                        Return True

                    End If

                    Try
                        data = New Date(2000 + matrice(2, 1), matrice(1, 1), matrice(0, 1), matrice(3, 1), matrice(4, 1), 0)
                        For i = 5 To 99
                            'mese giorno e anno
                            Select Case Val(matrice(i, 0))
                                Case 8 'flusso
                                    If Val(matrice(i, 1)) = 1 Then
                                        Flow_Acid_L = True
                                    End If
                                Case 9 'Allarme d2
                                    If Val(matrice(i, 1)) = 1 Then
                                        Flow_Self_Water_L = True
                                    End If

                                Case 10 'A probe
                                    If Val(matrice(i, 1)) = 1 Then
                                        Flow_Chlorite_L = True
                                    End If
                                Case 11 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        Lev_Acid_L = True
                                    End If
                                Case 12 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        Lev_Chlorite_L = True
                                    End If
                                Case 13 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        Lev_Water_L = True
                                    End If
                                Case 14 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        Sefl_Acid_L = True
                                    End If
                                Case 15 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        Sefl_Chlorite_L = True
                                    End If
                                Case 16 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        TimeOut_L = True
                                    End If
                                Case 17 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        Flow_Water_dil_L = True
                                    End If
                                Case 18 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        Service_F_L = True
                                    End If
                                Case 19 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        Analog_L = True
                                    End If
                                Case 20 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        Tank_Empty_L = True
                                    End If
                                Case 21 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        Level_SW = True
                                    End If
                                Case 22 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        BypassB = True
                                    End If
                                Case 23 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        Lim_Dioxide = True
                                    End If
                                Case 24 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        Lev_Alflow = True
                                    End If
                                Case 25 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        Overf = True
                                    End If
                                Case 47 'lev soglia
                                    If Val(matrice(i, 1)) = 1 Then
                                        LevSoglia = True
                                    End If
                                Case 26 To 28
                                    totAcido = (Val(matrice(i, 1)) * 1000 + Val(matrice(i + 1, 1)) * 100 + Val(matrice(i + 2, 1))) / 100
                                    i = i + 2
                                    totAcido_b = True
                                Case 29 To 31
                                    totCloro = (Val(matrice(i, 1)) * 1000 + Val(matrice(i + 1, 1)) * 100 + Val(matrice(i + 2, 1))) / 100
                                    i = i + 2
                                    totCloro_b = True

                                Case 32 To 34
                                    totAcqua = (Val(matrice(i, 1)) * 1000 + Val(matrice(i + 1, 1)) * 10 + Val(matrice(i + 2, 1)) * 1) / 10
                                    i = i + 2
                                    totAcqua_b = True

                                Case 48 To 50
                                    totAcidoDay = (Val(matrice(i, 1)) * 1000 + Val(matrice(i + 1, 1)) * 100 + Val(matrice(i + 2, 1))) / 100
                                    i = i + 2
                                    totAcidoDay_b = True
                                Case 51 To 53
                                    totCloroDay = (Val(matrice(i, 1)) * 1000 + Val(matrice(i + 1, 1)) * 100 + Val(matrice(i + 2, 1))) / 100
                                    i = i + 2
                                    totCloroDay_b = True
                                Case 54 To 56
                                    totAcquaDay = (Val(matrice(i, 1)) * 1000 + Val(matrice(i + 1, 1)) * 10 + Val(matrice(i + 2, 1)) * 1) / 10
                                    i = i + 2
                                    totAcquaDay_b = True

                                Case 57 To 59
                                    setpoint = (Val(matrice(i + 1, 1)) * 100 + Val(matrice(i + 2, 1)) * 1)
                                    setpoint = setpoint / return_lettura_lotus(Val(matrice(i, 1)))
                                    i = i + 2
                                    setpoint_b = True


                                Case 35 To 37
                                    flow1 = (Val(matrice(i, 1)) * 1000 + Val(matrice(i + 1, 1)) * 10 + Val(matrice(i + 2, 1)) * 1) / 10
                                    i = i + 2
                                    flow1_b = True

                                Case 40 To 41
                                    lettura = return_lettura(divide_ch1, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                    i = i + 1
                                    lettura_b = True
                                Case 42 To 43
                                    naso = return_lettura(divide_ch2, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                    i = i + 1
                                    naso_b = True
                                Case 45 To 46
                                    temperatura = (Val(matrice(i, 1) * 10) + Val(matrice(i + 1, 1))) / 10
                                    i = i + 1
                                    temperatura_b = True


                            End Select

                        Next
                        If Not flow1_b Then
                            flow1 = 0
                        End If
                        If Not flow2_b Then
                            flow2 = 0
                        End If
                        If Not lettura_b Then
                            lettura = 0
                        End If
                        If Not naso_b Then
                            naso = 0
                        End If
                        If Not temperatura_b Then
                            temperatura = 0
                        End If
                        If Not totAcido_b Then
                            totAcido = 0
                        End If
                        If Not totCloro_b Then
                            totCloro = 0
                        End If
                        If Not totAcqua_b Then
                            totAcqua = 0
                        End If
                        If Not totAcidoDay_b Then
                            totAcidoDay = 0
                        End If
                        If Not totCloroDay_b Then
                            totCloroDay = 0
                        End If
                        If Not totAcquaDay_b Then
                            totAcquaDay = 0
                        End If
                        If Not setpoint_b Then
                            setpoint = 0
                        End If

                        Return query.log_lta_ltu_ltd(identificativo, id_strumento, data, Flow_Acid_L, Flow_Self_Water_L, Flow_Chlorite_L, Lev_Acid_L, Lev_Chlorite_L,
                                             Lev_Water_L, Sefl_Acid_L, Sefl_Chlorite_L, TimeOut_L, Flow_Water_dil_L, Service_F_L, Analog_L, Tank_Empty_L, Level_SW,
                                             BypassB, Lim_Dioxide, Lev_Alflow, Overf, flow1, flow2, lettura, naso, temperatura, LevSoglia,
                                                     totAcido, totCloro, totAcqua,
                                                     totAcidoDay, totCloroDay, totAcquaDay, setpoint)

                        'Return query.log_wd(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_flow, allarmed2_val, allarmed1_val, allarmep1_val, allarmep2_val, livello1_val, livello2_val)

                    Catch ex As Exception

                    End Try
                End If

            End If

        Catch ex As Exception

        End Try
    End Function
    Public Function decode_log_wh(ByVal identificativo As String, ByVal id_strumento As String, ByVal string_parse As String, ByVal divide_ch1 As Integer, ByVal divide_ch2 As Integer, ByVal divide_ch3 As Integer, ByVal full_1 As Integer, ByVal full_2 As Integer, ByVal full_3 As Integer, _
                            ByVal ch1_value As Single, ByVal ch2_value As Single, ByVal ch3_value As Single) As Boolean
        Try
            Dim split_log_first() As String = string_parse.Split("&")
            If split_log_first.Length > 0 Then
                Dim split_log_second() As String = split_log_first(1).Split("g")
                If split_log_second.Length > 0 Then
                    Dim file_str As String
                    Dim split_log() As String = split_log_second(1).Split("?")
                    Dim matrice(100, 1) As String
                    file_str = split_log(0)
                    Dim i As Integer
                    Dim j As Integer
                    For i = 0 To split_log.Length - 2
                        matrice(j, 0) = split_log(i)
                        matrice(j, 1) = split_log(i + 1)
                        If matrice(j, 0) = "126" Then
                            matrice(j, 0) = "13"
                        End If
                        If matrice(j, 1) = "126" Then
                            matrice(j, 1) = "13"
                        End If
                        If matrice(j, 0) = "127" Then
                            matrice(j, 0) = "38"
                        End If
                        If matrice(j, 1) = "127" Then
                            matrice(j, 1) = "38"
                        End If

                        j = j + 1
                        i = i + 1
                        If i > split_log.Length - 1 Or j > 99 Then
                            Exit For
                        End If
                    Next
                    Dim data_str As String = ""
                    Dim condition As String = ""
                    Dim insert As String = ""
                    Dim ch1_val As Boolean = False
                    Dim ch2_val As Boolean = False
                    Dim ch3_val As Boolean = False

                    Dim level_ch1 As Boolean = False
                    Dim level_ch2 As Boolean = False

                    Dim allarme_d1 As Boolean = False
                    Dim allarme_d2 As Boolean = False
                    Dim allarme_p1 As Boolean = False
                    Dim allarme_p2 As Boolean = False

                    Dim allarme_timer_filtro As Boolean = False
                    Dim allarme_timer_sondaPH As Boolean = False
                    Dim allarme_timer_sondaMV As Boolean = False
                    Dim allarme_timer_pagamento As Boolean = False
                    Dim allarme_timer_manutenzione As Boolean = False
                    Dim stato_rele_ch1 As Boolean = False
                    Dim stato_rele_ch2 As Boolean = False
                    Dim stato_stby As Boolean = False

                    Dim flow_val As Boolean = False
                    Dim livello1 As Boolean = False
                    Dim livello2 As Boolean = False
                    Dim livello3 As Boolean = False

                    Dim valore_ch1 As Single = 0
                    Dim valore_ch2 As Single = 0
                    Dim valore_ch3 As Single = 0
                    Dim pulse1_ch1 As Integer
                    Dim pulse2_ch1 As Integer
                    Dim pulse_ch2 As Integer
                    Dim data As Date
                    Dim query_select As String = ""
                    Dim query_insert As String = ""
                    Try
                        data = New Date(2000 + matrice(2, 1), matrice(1, 1), matrice(0, 1), matrice(3, 1), matrice(4, 1), 0)
                        For i = 5 To 99
                            'mese giorno e anno
                            Select Case Val(matrice(i, 0))
                                Case 13 To 14
                                    If ch1_val = False And i < 10 Then
                                        valore_ch1 = return_lettura(divide_ch1, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        If Not filtra_valore(valore_ch1, full_1) Then
                                            valore_ch1 = ch1_value
                                        End If
                                        i = i + 1
                                        ch1_val = True
                                    End If
                                Case 15 To 16
                                    If ch2_val = False Then
                                        valore_ch2 = return_lettura(divide_ch2, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        If Not filtra_valore(valore_ch2, full_2) Then
                                            valore_ch2 = ch2_value
                                        End If
                                        i = i + 1
                                        ch2_val = True
                                    End If
                                Case 19 'Allarme d1
                                    allarme_d1 = True
                                Case 20 'Allarme d2
                                    allarme_d2 = True
                                Case 21 'A probe
                                    allarme_p1 = True
                                Case 22 'A probe 2
                                    allarme_p2 = True
                                Case 23 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        allarme_timer_filtro = True
                                    End If
                                Case 24 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        allarme_timer_sondaPH = True
                                    End If
                                Case 25 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        allarme_timer_sondaMV = True
                                    End If
                                Case 26 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        allarme_timer_pagamento = True
                                    End If
                                Case 27 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        allarme_timer_manutenzione = True
                                    End If
                                Case 28 'A probe 2
                                    pulse1_ch1 = Val(matrice(i, 1))
                                Case 29 'A probe 2
                                    pulse2_ch1 = Val(matrice(i, 1))
                                Case 30 'A probe 2
                                    pulse_ch2 = Val(matrice(i, 1))
                                Case 31 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        stato_rele_ch1 = True
                                    End If
                                Case 32 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        stato_rele_ch2 = True
                                    End If
                                Case 40 'A probe 2
                                    If Val(matrice(i, 1)) = 1 Then
                                        stato_stby = True
                                    End If



                                Case 8
                                    flow_val = True
                                Case 10
                                    If Val(matrice(i, 1)) = 1 Then
                                        livello1 = True
                                    End If
                                Case 11
                                    If Val(matrice(i, 1)) = 1 Then
                                        livello2 = True
                                    End If
                            End Select

                        Next
                        If ch1_val = False Then
                            valore_ch1 = 0

                        End If
                        If ch2_val = False Then
                            valore_ch2 = 0
                        End If
                        If ch3_val = False Then
                            valore_ch3 = 0
                        End If
                        Return query.log_wh(identificativo, id_strumento, data, valore_ch1, valore_ch2, flow_val, allarme_d2, allarme_d1, allarme_p1, allarme_p2, livello1, livello2, allarme_timer_filtro, allarme_timer_sondaPH, allarme_timer_sondaMV, allarme_timer_pagamento, allarme_timer_manutenzione, pulse1_ch1, pulse2_ch1, pulse_ch2, stato_rele_ch1, stato_rele_ch2, stato_stby)

                        'Return query.log_wd(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_flow, allarmed2_val, allarmed1_val, allarmep1_val, allarmep2_val, livello1_val, livello2_val)

                    Catch ex As Exception

                    End Try
                End If

            End If

        Catch ex As Exception

        End Try
    End Function
    Public Function decode_log_tower(ByVal identificativo As String, ByVal id_strumento As String, ByVal string_parse As String, ByVal divide_ch1 As Integer, ByVal divide_ch2 As Integer, ByVal divide_ch3 As Integer, ByVal full_1 As Integer, ByVal full_2 As Integer, ByVal full_3 As Integer,
                                ByVal ch1_value As Single, ByVal ch2_value As Single, ByVal ch3_value As Single, ByVal tower_induttiva As Boolean, ByVal typeTower As Integer) As Boolean

        Try

            Dim split_log_first() As String = string_parse.Split("&")
            If split_log_first.Length > 0 Then
                Dim split_log_second() As String = split_log_first(1).Split("g")
                If split_log_second.Length > 0 Then
                    Dim file_str As String
                    Dim split_log() As String = split_log_second(1).Split("?")
                    Dim matrice(100, 1) As String
                    file_str = split_log(0)
                    Dim i As Integer
                    Dim j As Integer
                    For i = 0 To split_log.Length - 2
                        matrice(j, 0) = split_log(i)
                        matrice(j, 1) = split_log(i + 1)
                        If matrice(j, 0) = "126" Then
                            matrice(j, 0) = "13"
                        End If
                        If matrice(j, 1) = "126" Then
                            matrice(j, 1) = "13"
                        End If
                        If matrice(j, 0) = "127" Then
                            matrice(j, 0) = "38"
                        End If
                        If matrice(j, 1) = "127" Then
                            matrice(j, 1) = "38"
                        End If

                        j = j + 1
                        i = i + 1
                        If i > split_log.Length - 1 Or j > 99 Then
                            Exit For
                        End If
                    Next
                    Dim data_str As String = ""
                    Dim condition As String = ""
                    Dim insert As String = ""
                    Dim ch1_val As Boolean = False
                    Dim ch2_val As Boolean = False
                    Dim ch3_val As Boolean = False

                    Dim cd_high As Boolean = False
                    Dim cd_low As Boolean = False
                    Dim bleed_timeout As Boolean = False
                    Dim flow As Boolean = False
                    Dim inhib_lev As Boolean = False
                    Dim prebio1_lev As Boolean = False
                    Dim prebio2_lev As Boolean = False
                    Dim bio1_lev As Boolean = False
                    Dim bio2_lev As Boolean = False

                    Dim inhibitor_out As Boolean = False
                    Dim inhibitor_prop As Boolean = False
                    Dim bleed_out As Boolean = False
                    Dim prebio1_out As Boolean = False
                    Dim prebio2_out As Boolean = False
                    Dim bio1_out As Boolean = False
                    Dim bio2_out As Boolean = False
                    Dim ch2_dig As Boolean = False
                    Dim ch2_prop As Boolean = False


                    Dim ch2_high As Boolean = False
                    Dim ch2_low As Boolean = False
                    Dim ch2_level As Boolean = False
                    Dim ch3_high As Boolean = False
                    Dim ch3_low As Boolean = False
                    Dim ch3_level As Boolean = False

                    Dim tot_input As Boolean = False
                    Dim tot_bleed As Boolean = False

                    Dim temperatura As Boolean = False

                    Dim valore_ch1 As Single = 0
                    Dim valore_ch2 As Single = 0
                    Dim valore_ch3 As Single = 0

                    Dim cd_high_val As String = ""
                    Dim cd_low_val As String = ""
                    Dim bleed_timeout_val As String = ""
                    Dim flow_val As String = ""
                    Dim inhib_lev_val As String = ""
                    Dim prebio1_lev_val As String = ""
                    Dim prebio2_lev_val As String = ""
                    Dim bio1_lev_val As String = ""
                    Dim bio2_lev_val As String = ""
                    Dim ch2_high_val As String = ""
                    Dim ch2_low_val As String = ""
                    Dim ch2_level_val As String = ""
                    Dim ch3_high_val As String = ""
                    Dim ch3_low_val As String = ""
                    Dim ch3_level_val As String = ""

                    Dim inhibitor_out_val As String = ""
                    Dim inhibitor_prop_val As String = ""
                    Dim bleed_out_val As String = ""
                    Dim prebio1_out_val As String = ""
                    Dim prebio2_out_val As String = ""
                    Dim bio1_out_val As String = ""
                    Dim bio2_out_val As String = ""
                    Dim ch2_dig_val As String = ""
                    Dim ch2_prop_val As String = ""


                    Dim tot_input_val As String = ""
                    Dim tot_bleed_val As String = ""


                    Dim temperatura_val As Single = 0

                    Dim data As Date
                    Dim valore_flow As String = ""
                    Dim insert_flow As String = ""
                    Dim query_select As String = ""
                    Dim query_insert As String = ""

                    Dim power_on As Boolean = False

                    Dim debug_dosim As String = ""
                    Try
                        'If (matrice(1, 1) = 11) Then
                        '    Exit Function
                        'End If
                        If identificativo = "234978" Then
                            valore_flow = ""
                        End If

                        If identificativo = "470046" Then
                            For i = 0 To 99
                                debug_dosim = debug_dosim + Val(matrice(i, 0)).ToString + "," + Val(matrice(i, 1)).ToString + ";"
                            Next
                            Try
                                Using writer As System.IO.StreamWriter = New System.IO.StreamWriter("c:\log_tower_470046.txt", True)
                                    writer.WriteLine("#" + debug_dosim + "#")
                                    writer.Close()
                                End Using
                            Catch ex As Exception

                            End Try
                        End If
                        data = New Date(2000 + matrice(2, 1), matrice(1, 1), matrice(0, 1), matrice(3, 1), matrice(4, 1), 0)
                        For i = 5 To 99
                            'mese giorno e anno
                            Select Case Val(matrice(i, 0))
                                Case 6 To 7
                                    If ch1_val = False Then
                                        valore_ch1 = return_lettura(divide_ch1, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        'If valore_ch1 > full_1 Then
                                        '    valore_ch1 = full_1
                                        'End If
                                        If tower_induttiva Then
                                            valore_ch1 = valore_ch1 * 10
                                        End If
                                        'If Not filtra_valore(valore_ch1, full_1) Then
                                        '    valore_ch1 = ch1_value
                                        'End If
                                        i = i + 1
                                        ch1_val = True
                                    End If

                                Case 38 To 39
                                    If ch2_val = False Then
                                        valore_ch2 = return_lettura(divide_ch2, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        'If valore_ch2 > full_2 Then
                                        '    valore_ch2 = full_2
                                        'End If

                                        'If Not filtra_valore(valore_ch2, full_2) Then
                                        '    valore_ch2 = ch2_value
                                        'End If
                                        i = i + 1
                                        ch2_val = True
                                    End If
                                Case 46 To 47
                                    If ch3_val = False Then
                                        valore_ch3 = return_lettura(divide_ch3, Val(matrice(i, 1) * 100) + Val(matrice(i + 1, 1)))
                                        If valore_ch3 > full_3 Then
                                            valore_ch3 = full_3
                                        End If

                                        'If Not filtra_valore(valore_ch3, full_3) Then
                                        '    valore_ch3 = ch3_value
                                        'End If
                                        i = i + 1
                                        ch3_val = True
                                    End If

                                Case 11
                                    cd_high = True
                                Case 12
                                    cd_low = True
                                Case 13
                                    If Val(matrice(i, 1)) <> 124 Then
                                        bleed_timeout = True
                                    End If
                                Case 14
                                    flow = True
                                Case 15
                                    inhib_lev = True
                                Case 16
                                    prebio1_lev = True
                                Case 17
                                    prebio2_lev = True
                                Case 18
                                    bio1_lev = True
                                Case 19
                                    bio2_lev = True

                                Case 20
                                    inhibitor_out = True
                                    inhibitor_out_val = Format(Val(matrice(i, 1)), "0")
                                Case 21
                                    inhibitor_prop = True
                                    inhibitor_prop_val = Format(Val(matrice(i, 1)), "0")
                                Case 22
                                    bleed_out = True
                                    bleed_out_val = Format(Val(matrice(i, 1)), "0")
                                Case 24
                                    prebio1_out = True
                                    prebio1_out_val = Format(Val(matrice(i, 1)), "0")
                                Case 25
                                    prebio2_out = True
                                    prebio2_out_val = Format(Val(matrice(i, 1)), "0")
                                Case 26
                                    bio1_out = True
                                    bio1_out_val = Format(Val(matrice(i, 1)), "0")
                                Case 27
                                    bio2_out = True
                                    bio2_out_val = Format(Val(matrice(i, 1)), "0")
                                Case 43
                                    ch2_dig = True
                                    ch2_dig_val = Format(Val(matrice(i, 1)), "0")
                                Case 44
                                    ch2_prop = True
                                    ch2_prop_val = Format(Val(matrice(i, 1)), "0")

                                Case 40
                                    'cd_high = True
                                    ch2_high = True
                                Case 41
                                    'cd_low = True
                                    ch2_low = True
                                Case 42
                                    ch2_level = True
                                Case 48
                                    'cd_high = True
                                    ch3_high = True
                                Case 49
                                    'cd_low = True
                                    ch3_low = True
                                Case 50
                                    ch3_level = True
                                Case 28 To 32
                                    tot_input = True
                                    tot_input_val = Format(Val(matrice(i, 1)), "00") + Format(Val(matrice(i + 1, 1)), "00") + Format(Val(matrice(i + 2, 1)), "00") + Format(Val(matrice(i + 3, 1)), "00") + Format(Val(matrice(i + 4, 1)), "00")
                                    i = i + 4
                                Case 33 To 37
                                    If tot_bleed = False Then
                                        tot_bleed = True
                                        tot_bleed_val = Format(Val(matrice(i, 1)), "00") + Format(Val(matrice(i + 1, 1)), "00") + Format(Val(matrice(i + 2, 1)), "00") + Format(Val(matrice(i + 3, 1)), "00") + Format(Val(matrice(i + 4, 1)), "00")
                                        i = i + 4
                                    End If

                                Case 88
                                    power_on = Val(matrice(i, 1))

                                Case 8 To 9
                                    'If temperatura = "1" Then '°F
                                    '    condition_temperatura = condition_temperatura + " And (Temperatura  ='" + Format(matrice(i, 1), "00") + Format(matrice(i + 1, 1), "0") + "')"
                                    '    insert_temperatura = insert_temperatura + ",'" + Format(matrice(i, 1), "00") + Format(matrice(i + 1, 1), "0") + "'"
                                    'Else '°C
                                    temperatura_val = ((matrice(i, 1) * 100) + matrice(i + 1, 1)) / 10
                                    'End If
                                    i = i + 1
                                    temperatura = True

                            End Select

                        Next
                        Select Case typeTower
                            Case 1
                                If ch1_val = False Then
                                    Return True
                                End If
                            Case 2
                                If ch1_val = False Or ch2_val = False Then
                                    Return True
                                End If
                            Case 3
                                If ch1_val = False Or ch2_val = False Or ch3_val = False Then
                                    Return True
                                End If

                        End Select

                        If ch1_val = False Then
                            valore_ch1 = 0

                        End If
                        If ch2_val = False Then
                            valore_ch2 = 0
                        End If
                        If ch3_val = False Then
                            valore_ch3 = 0
                        End If
                        If cd_high Then
                            cd_high_val = "1"
                        Else
                            cd_high_val = "0"
                        End If
                        If cd_low Then
                            cd_low_val = "1"
                        Else
                            cd_low_val = "0"
                        End If
                        If bleed_timeout Then
                            bleed_timeout_val = "1"
                        Else
                            bleed_timeout_val = "0"
                        End If
                        If flow Then
                            flow_val = "1"
                        Else
                            flow_val = "0"
                        End If

                        If inhib_lev Then
                            inhib_lev_val = "1"
                        Else
                            inhib_lev_val = "0"
                        End If
                        If prebio1_lev Then
                            prebio1_lev_val = "1"
                        Else
                            prebio1_lev_val = "0"
                        End If

                        If prebio2_lev Then
                            prebio2_lev_val = "1"
                        Else
                            prebio2_lev_val = "0"
                        End If
                        If bio1_lev Then
                            bio1_lev_val = "1"
                        Else
                            bio1_lev_val = "0"
                        End If
                        If bio2_lev Then
                            bio2_lev_val = "1"
                        Else
                            bio2_lev_val = "0"
                        End If
                        If ch2_high Then
                            ch2_high_val = "1"
                        Else
                            ch2_high_val = "0"
                        End If

                        If ch2_low Then
                            ch2_low_val = "1"
                        Else
                            ch2_low_val = "0"
                        End If

                        If ch2_level Then
                            ch2_level_val = "1"
                        Else
                            ch2_level_val = "0"
                        End If

                        If ch3_high Then
                            ch3_high_val = "1"
                        Else
                            ch3_high_val = "0"
                        End If

                        If ch3_low Then
                            ch3_low_val = "1"
                        Else
                            ch3_low_val = "0"
                        End If

                        If ch3_level Then
                            ch3_level_val = "1"
                        Else
                            ch3_level_val = "0"
                        End If
                        If tot_input = False Then
                            tot_input_val = "0000000000"
                        End If
                        If tot_bleed = False Then
                            tot_bleed_val = "0000000000"
                        End If
                        If temperatura = False Then
                            temperatura_val = 0

                        End If
                        If inhibitor_out = False Then
                            inhibitor_out_val = "0"
                        End If
                        If inhibitor_prop = False Then
                            inhibitor_prop_val = "0"
                        End If
                        If bleed_out = False Then
                            bleed_out_val = "0"
                        End If
                        If prebio1_out = False Then
                            prebio1_out_val = "0"
                        End If
                        If prebio2_out = False Then
                            prebio2_out_val = "0"
                        End If
                        If bio1_out = False Then
                            bio1_out_val = "0"
                        End If
                        If bio2_out = False Then
                            bio2_out_val = "0"
                        End If
                        If ch2_dig = False Then
                            ch2_dig_val = "0"
                        End If
                        If ch2_prop = False Then
                            ch2_prop_val = "0"
                        End If


                        Return query.log_tower(identificativo, id_strumento, data, valore_ch1, valore_ch2, valore_ch3, temperatura_val, cd_high_val, cd_low_val, bleed_timeout_val, flow_val, inhib_lev_val, prebio1_lev_val, prebio2_lev_val, bio1_lev_val, bio2_lev_val, ch2_high_val, ch2_low_val, ch2_level_val, ch3_high_val, ch3_low_val, ch3_level_val, tot_input_val, tot_bleed_val,
                                               inhibitor_out_val, inhibitor_prop_val, bleed_out_val, prebio1_out_val, prebio2_out_val, bio1_out_val, bio2_out_val, ch2_prop_val, ch2_dig_val, "0", "0", power_on)
                    Catch ex As Exception
                        Return True
                    End Try
                End If

            End If
        Catch ex As Exception
            Return True
        End Try
        Return True
    End Function
End Class