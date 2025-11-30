
# Projektstatus-Snapshot: TSF-Scheduler (v1.0 Stable)

**Datum:** 23. November 2025
**Kontext:** Teil der TSF (Tiny Smalltalk Framework) Suite.

## 1. Zielsetzung & Kernphilosophie
Ein leichtgewichtiges, robustes und thread-sicheres Framework zur Ausführung von asynchronen Hintergrundaufgaben und periodischen Jobs in Smalltalk. Der Fokus liegt auf Stabilität, sauberer Architektur ("Smalltalk Way") und einfacher Bedienbarkeit für Entwickler.

## 2. Architektur-Überblick
Das System basiert auf einer strikten Trennung von Verantwortlichkeiten (Separation of Concerns) nach dem Producer-Consumer-Muster, realisiert durch Singleton-Instanzen. Thread-Sicherheit wird durch den gezielten Einsatz von `Mutex` für Datenzugriffe und `Semaphore` für Signalisierung gewährleistet.

### Die Hauptkomponenten

#### A. `TsfTask` (Die Arbeitseinheit)
Kapselt *was* getan werden soll und in welchem *Zustand* es sich befindet.
* **Dualer Ausführungsmodus:** Unterstützt sowohl Ad-hoc-Blöcke (`setAction:`) für Skripte als auch sauberes Subclassing durch Überschreiben von `executeAction` (Command Pattern).
* **Zustandsmanagement:** Verwaltet Status (`#pending`, `#running`, `#finished`, `#failed`, `#cancelled`, `#skipped`).
* **Lifecycle-Kontrolle:** Tasks können "von innen heraus" vor der Ausführung pausiert (`pause`) oder abgebrochen (`cancel`) werden.
* **Fehlerbehandlung:** Fängt Fehler in `execute` ab, speichert sie (`caughtError`), ruft individuelle `onFailure:`-Callbacks auf und informiert den globalen Handler des Schedulers.

#### B. `TsfScheduler` (Der Arbeiter / The Worker)
Ein Singleton, das nichts von Zeit weiß, sondern nur Arbeit sieht.
* **Funktion:** Arbeitet eine Queue von `TsfTask`-Objekten sequenziell in einem eigenen Hintergrundprozess ab.
* **Thread-Sicherheit:** Schützt seine `taskQueue` mit einem `Mutex`.
* **Effizienz:** Nutzt ein `workSignal` (Semaphore), um CPU-schonend zu schlafen, wenn die Queue leer ist.
* **Graceful Shutdown:** Beendet den Worker-Prozess kooperativ via Signal, statt ihn hart zu terminieren.

#### C. `TsfCron` (Der Zeitgeber / The Timer)
Ein Singleton, das die Zeit verwaltet und den Scheduler füttert.
* **Funktion:** Verwaltet periodische Tasks in einer Priority Queue (nach `nextRun` sortiert).
* **Effizienz:** Nutzt "Interruptible Wait". Der Prozess schläft exakt bis zur Fälligkeit des nächsten Tasks, kann aber durch ein `wakeUpSignal` (z.B. beim Hinzufügen eines neuen Tasks) sofort geweckt werden.

## 3. Kritische Design-Entscheidungen & Features

### "Fixed Delay" Scheduling
Um ein "Stapeln" von Tasks bei langer Ausführungsdauer zu verhindern, plant sich ein periodischer Task erst *nach* Abschluss seiner `execute`-Methode selbst neu ein. Die Pause zwischen den Ausführungen ist garantiert.

### Idempotenz (`ensureTaskNamed:...`)
Die zentrale API für Konfigurationsskripte. Erlaubt das mehrfache Ausführen von Setup-Code, ohne Tasks zu duplizieren. Existierende Tasks werden anhand ihres Namens gefunden und aktualisiert.

### Graceful Shutdown Kette
Ein `stop` auf Systemebene wird sauber propagiert:
1.  `TsfCron` stoppt (keine neuen Tasks werden eingeplant).
2.  `TsfScheduler` stoppt (arbeitet aktuellen Task zu Ende und beendet sich dann).
3.  `Mutex`-Sperren werden dabei nicht gefährdet.

## 4. Status der Qualitätssicherung (Testing)

Das Framework verfügt über eine umfassende Suite aus Unit- und Integrationstests (`TSF-Scheduler-Tests`).

* **Unit Tests:** Prüfen die Logik von Task-Zuständen, Error-Handling und Cron-Sortierung isoliert.
* **Integration Tests:** Prüfen das Zusammenspiel von Cron und Scheduler mit echten Delays.
* **Gelöste Concurrency-Herausforderung:** Ein kritischer Race Condition im Test `testSubclassingIntegration` (Test prüfte Status `#finished`, bevor der Scheduler-Thread ihn setzen konnte) wurde durch ein minimales `(Delay forMilliseconds: 1) wait` im Test nach dem Semaphore-Signal gelöst.

**Aktueller Stand:** Alle Tests sind **grün** (stabil).
