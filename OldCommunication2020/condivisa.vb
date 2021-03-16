Public Class condivisa
    Public Shared coda_comandi As Queue
    Public Shared Sub crea_coda()
        coda_comandi = New Queue()
    End Sub

    Public Shared Sub add_comandi(ByVal messaggio As coppia)
        coda_comandi.Enqueue(messaggio)
    End Sub
    Public Shared Function remove_comandi() As coppia
        Return coda_comandi.Dequeue()
    End Function

End Class