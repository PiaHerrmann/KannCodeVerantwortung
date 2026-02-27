#Requires AutoHotkey v2
#SingleInstance Force

; =====================================================
; NOTFALL-ABBRUCH
; =====================================================
Esc::ExitApp

; =====================================================
; STARTMELDUNG
; =====================================================
TrayTip "KI Automation", "Skript läuft…  ESC = Abbruch", 3

; =====================================================
; FRAGEN LADEN
; =====================================================
FragenText := FileRead("RLPFragenPersona.txt", "UTF-8")
FragenArray := StrSplit(FragenText, "`n")

for index, Frage in FragenArray {
    Frage := Trim(Frage)
    if (Frage = "")
        continue

    ; ==========================
    ; CHATGPT → GOOGLE DOC
    ; ==========================
    AntwortGPT := AskKI("ChatGPT", Frage)
    CreateDoc(Frage, AntwortGPT, "ChatGPT")

    ; ==========================
    ; GEMINI → GOOGLE DOC
    ; ==========================
    AntwortGemini := AskKI("Gemini", Frage)
    CreateDoc(Frage, AntwortGemini, "Gemini")
}

TrayTip "KI Automation", "Fertig – alle Fragen verarbeitet.", 5
ExitApp


; =====================================================
; FUNKTIONEN
; =====================================================


ActivateBrowser() {
    WinActivate "ahk_exe chrome.exe"
    Sleep 300
}

AskKI(kiName, frage) {
    ActivateBrowser()

    ; Tab aktivieren: STRG+1 = ChatGPT / STRG+2 = Gemini
    if (kiName = "ChatGPT")
        Send "^1"
    else if (kiName = "Gemini")
        Send "^2"
   

    A_Clipboard := frage

    ; === FOKUS TAB SETZEN ===

        CoordMode "Mouse","Window"
        Click 600,650      ;
        Sleep 300


    Send "^+o"        ; Neuer Chat 
    Sleep 5000
    Send "^v{Enter}"
    Sleep 5000
    Send "^{Enter}"
    Sleep 15000   ; Wartezeit für KI-Antwort

    MsgBox(
        kiName . ": Bitte jetzt die Antwort der KI kopieren.`n`n" .
        "Danach OK drücken.",
        "Manuelles Kopieren",
        64
    )

    ClipWait(60)   ; wartet bis etwas in der Zwischenablage steht

    return A_Clipboard   ; Inhalt zurückgeben als Textantwort
}

CreateDoc(frage, antwort, ki) {
    ActivateBrowser()
    Send "^t" ; neuer Tab
    Sleep 200
    Send "https://docs.google.com/document/create?usp=drive_web&folder=1jNfJOmu9GCn1sbBFirbShYz6jJWoCcjW{Enter}" ; Ordner im OneDrive
    Sleep 8000
            
    timestamp := FormatTime("", "dd.MM.yyyy HH:mm:ss")
    A_Clipboard :=
        "Zeitpunkt: " . timestamp . "`nFrage: " . frage .
        "`n`nAntwort (" . ki . "):`n" . antwort
    
    Send "^v"
    Sleep 800
}