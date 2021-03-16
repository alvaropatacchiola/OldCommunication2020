Public Class pc
    Private mstringa_tx As String
    Private mstringa_rx As String
    Private mstringa_rx_attesa As String
    Private mconnected As Boolean
    Private mscrittura_setpoint As Boolean
    Private mpuntatore_pc As ConnectedClient
    Friend WithEvents Timer1 As System.Timers.Timer
    Public timer_generale1 As System.Timers.Timer
    Public timer_generale2 As System.Timers.Timer
    'Private mlight_send_iph As String
    Private time_pc As DateTime
    Sub New()
        mconnected = False
        mstringa_tx = ""
        mstringa_rx = ""
        mstringa_rx_attesa = ""
        puntatore_pc = Nothing
        Timer1 = New System.Timers.Timer
        Timer1.Interval = 60000 ' timer che parte alla ricerca delola configurazione dello strumento
    End Sub
    Public Property pc_tx() As String
        'stringa di richiesta inviata dal pc
        Get
            Return mstringa_tx
        End Get
        Set(ByVal value As String)
            mstringa_tx = value
        End Set
    End Property
    Public Property pc_rx() As String
        'stringa di risposta dello strumento da inviare al pc
        Get
            Return mstringa_rx
        End Get
        Set(ByVal value As String)
            mstringa_rx = value
        End Set
    End Property
    Public Property pc_rx_attesa() As String
        'stringa dirisposta che si attende il pc
        Get
            Return mstringa_rx_attesa
        End Get
        Set(ByVal value As String)
            mstringa_rx_attesa = value
        End Set
    End Property
    Public Property connected() As Boolean
        'stringa dirisposta che si attende il pc
        Get
            Return mconnected
        End Get
        Set(ByVal value As Boolean)
            mconnected = value
        End Set
    End Property
    Public Property scrittura_sp() As Boolean
        'stringa dirisposta che si attende il pc
        Get
            Return mscrittura_setpoint
        End Get
        Set(ByVal value As Boolean)
            mscrittura_setpoint = value
        End Set
    End Property
    Public Property puntatore_pc() As ConnectedClient
        'stringa dirisposta che si attende il pc
        Get
            Return mpuntatore_pc
        End Get
        Set(ByVal value As ConnectedClient)
            mpuntatore_pc = value
        End Set
    End Property
    'Public Property light_send_iph() As String
    '    'proprieta che contiene info light per iphone
    '    Get
    '        Return mlight_send_iph
    '    End Get
    '    Set(ByVal value As String)
    '        mlight_send_iph = value
    '    End Set
    'End Property
    Public Property time_connessione() As DateTime
        'ora di connessione del pc
        Get
            Return time_pc
        End Get
        Set(ByVal value As DateTime)
            time_pc = value
        End Set
    End Property
    Public Sub reset_contatore()
        Timer1.Stop()
        Timer1.Start()
    End Sub
    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Elapsed
        If connected Then
            Try
                timer_generale1.Start()
                timer_generale2.Start()
            Catch ex As Exception

            End Try
            puntatore_pc.close_connection()
            puntatore_pc = Nothing
            connected = False
        End If
        Timer1.Stop()
    End Sub
End Class