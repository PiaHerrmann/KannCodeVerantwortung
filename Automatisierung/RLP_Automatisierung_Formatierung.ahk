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
    AntwortGPT_Clip := AskKI("ChatGPT", Frage)
    CreateDoc(Frage, AntwortGPT_Clip, "ChatGPT")

    ; ==========================
    ; GEMINI → GOOGLE DOC
    ; ==========================
    AntwortGemini_Clip := AskKI("Gemini", Frage)
    CreateDoc(Frage, AntwortGemini_Clip, "Gemini")
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

    ; Tab aktivieren
    if (kiName = "ChatGPT")
        Send "^1"
    else if (kiName = "Gemini")
        Send "^2"

    ; Frage einfügen
    A_Clipboard := frage
    Sleep 100

    ; Neuer Chat
    Send "^+o"
    Sleep 5000
    Send "^v{Enter}"
    Sleep 5000
    Send "^{Enter}"
    Sleep 15000   ; Wartezeit auf KI-Antwort

    MsgBox(
        kiName . ": Bitte jetzt die Antwort der KI mit STRG+C kopieren.`n`n" .
        "👉 Die Formatierung bleibt erhalten!",
        "Manuelles Kopieren",
        64
    )

    ClipWait 60
    return ClipboardAll()   ; 🔥 komplette Zwischenablage inkl. HTML
}

CreateDoc(frage, antwortClip, ki) {
    ActivateBrowser()
    Send "^3"           ; Google Docs Tab
    Sleep 500

    ; Neues Dokument
    Send "^t"
    Sleep 200
    Send "https://docs.google.com/document/create?usp=drive_web&folder=1jNfJOmu9GCn1sbBFirbShYz6jJWoCcjW{Enter}"
    Sleep 5000

    timestamp := FormatTime("", "dd.MM.yyyy HH:mm:ss")

    ; ==========================
    ; KOPF (PLAIN TEXT)
    ; ==========================
    KopfText :=
        "Zeitpunkt: " . timestamp . "`n" .
        "Frage: " . frage . "`n`n" .
        "Antwort (" . ki . "):`n"

    OldClip := ClipboardAll()   ; Sicherung

    A_Clipboard := KopfText
    Sleep 100
    Send "^v"
    Sleep 300

    ; ==========================
    ; ANTWORT (MIT FORMATIERUNG)
    ; ==========================
    A_Clipboard := antwortClip
    Sleep 100
    Send "^v"
    Sleep 300

    ; Clipboard wiederherstellen
    A_Clipboard := OldClip
}
