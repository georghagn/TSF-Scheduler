
|<sub>🇬🇧 [English translation →](README.en.md)</sub>|
|----:|
|    |

|[![Pharo Version](https://img.shields.io/badge/Pharo-12.0%2B-blue.svg)](https://pharo.org)|[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](./LICENSE) [![Dependencies](https://img.shields.io/badge/dependencies-zero-brightgreen.svg)](#)|
|----|----|
|![TSF-Scheduler Logo](logo-scheduler.png)| ***TSF-Scheduler***<br>Ein robustes, threadsicheres und ressourcenschonendes Framework zur Aufgabenplanung für Smalltalk. Teil der **TSF (Tiny Smalltalk Framework)**-Suite|

<sup>***TSF*** steht für ***Tiny Smalltalk Framework*** — eine Sammlung von minimalistischen Tools für robuste Anwendungen.</sup>


## Überblick

`TSF-Scheduler` bietet einen leistungsstarken Mechanismus zur Verarbeitung asynchroner Hintergrundprozesse und periodischer Aufgaben. Es wurde mit Blick auf Stabilität und einfache Bedienbarkeit "Smalltalk-Way"entwickelt, Es wird klar unterschieden zwischen  **scheduling logic** (Timer) und **execution logic** (Worker).

## Hauptmerkmale

* **Thread-Safe:** Zum Schutz der Warteschlangen-Queue und für atomare Operationen wird der Mechanismus *`Mutex`* verwendet.
* **Architecture:** Trennung der Zuständigkeiten zwischen *`TsfCron`* (Timer) und *`TsfScheduler`* (Worker).
* **Smart Scheduling:** Um das "Stapeln" von Task in der Ausführungs-Queue zu verhindern, wird eine **Fixed Delay** Strategie für periodische Tasks verwendet. Ein Task wird erst *nach* seiner Beendigung wieder in den Scheduler geschoben.
* **Idempotency:** Sicheres updating der Task-Konfiguration zur Laufzeit ohne die Jobs zu duplizieren.
* **Lifecycle Management:** Unterstützt Lifecycle Stati für *`pause`*, *`resume`*, und *`cancel`* von laufenden Tasks.
* **Graceful Shutdown:** Kooperatives Thread-Termination stellt sicher, daß Resourcen in keinem inkonsistenten Zustand verbeliben.

## Architectur

- **TsfScheduler:** Ein Singleton Worker, welcher die Task-Queue verarbeitet. Er hat keine Ahnung von Zeit, nur von Arbeit. Er verarbeitet Tasks sequentiell in einem Background-Prozess.
- **TsfCron:** Ein Singleton Ttimer, welcher eine priorisierte Queue von periodischen Tasks verwaltet. Er wacht nur auf, wenn ein Task an der Reihe ist, oder wenn ein neuer Task hinzugefügt wird. (Interruptible Wait), dies stellt sicher, dass die CPU im Idle Modus zu 0% belastet wird.
- **TsfTask:** Die atomare Arbeitseinheit des TSF-Schedulers. Er kapselt **was** getan werden muss (die Logik) und **in welchem Zustand** sich die Arbeit befindet (State Management).

## Installation

```smalltalk
Metacello new
    baseline: 'TsfScheduler';
    repository: 'github://georghagn/TSF-Scheduler:main';
    load.
```

## Usage the Scheduler

### 1. Starting the Scheduler System

*TSF Scheduler* besteht aus Scheduler (führt Tasks aus) und Cron (verwaltet Scheduling-Zeitpunkte).

```smalltalk
TsfScheduler current start.
TsfCron current start.
```

### 2. Scheduler One-Off Tasks (Fire and Forget)

Für einfache "background operations"" können Smalltalk-Blöcke direct mit dem Scheduler benutzt werden

```smalltalk
TsfScheduler current scheduleBlock: [ 
    (Delay forSeconds: 2) wait.
    Transcript show: 'Background job finished!'; cr.
].
```

### 3. Scheduler Lifecycle Control

Bereits gestartete Tasks können sogar nach ihrem Start noch kontrolliert werden.

```smalltalk
| task |
task := TsfCron current findTaskByName: 'System Cleanup'.

task pause.   "Stops execution, but keeps the timer ticking"
task resume.  "Resumes execution"
task cancel.  "Permanently stops and removes the task"
```

### 4. Scheduler Error Handling

Aufgaben fangen ihre eigenen Fehler ab, um einen Absturz des Worker-Threads zu verhindern. Sie können für jede Aufgabe einen eigenen Fehlerbehandler oder einen globalen Fehlerbehandler definieren.

```smalltalk
TsfScheduler current globalErrorHandler: [ :task :error |
    Transcript 
		show: 'Critical failure in ';
		showCr: task name.
].
```

## Using TsfTask: Die generische Arbeitseinheit

Das "Dual Mode" Konzept. Die Klasse unterstützt zwei Arten der Nutzung, je nach Komplexität der Aufgabe:

### 1. Task Scripting Mode (Ad-Hoc via Blöcke)

Ideal für schnelle Wartungsaufgaben, One-Liners oder Konfigurationen via Workspace. Hierbei wird ein `BlockClosure` (oder ein `MessageSend`) direkt in den Task injiziert.

**Vorteil:** Kein Overhead durch neue Klassen.
**Nutzung 1:** Via `TsfCron >> #ensureTaskNamed:frequency:action:`

```smalltalk
"Beispiel: Ein Task, der alle 5 Minuten den Speicher bereinigt"
TsfCron current 
    ensureTaskNamed: 'GarbageCollector' 
    frequency: 5 minutes 
    action: [ Smalltalk garbageCollect ].
```

oder alternativ: 

**Nutzung 2:** Via `TsfTask >> #named:do:`

```smalltalk
task := TsfTask named: 'GarbageCollector'
    do: [ Smalltalk garbageCollect ].
```

### Task 2. OOP Mode (Robuster Smalltalk Way)

Ideal für komplexe Logik, die eigenen State benötigt, testbar sein muss oder Abhängigkeiten hat. 

**Vorteil:** Saubere Kapselung, Wiederverwendbarkeit und bessere Testbarkeit.
**Nutzung:**

```smalltalk
| task |

"Definition"
Object subclass: #MyDatabaseExport
    ...

!MyDatabaseExport methodsFor: 'actions'!
runAction
    "Hier die komplexe Logik implementieren"
    Database exportTo: 'backup.sql'.
    ^ 'Export successful'
! !

"Registrierung"
task := TsfTask 
			named: 'NightlyDBExport' 
			receiver: MyDatabaseExport 
			selector: #runAction 
			frequency: 24 hours.
			
task execute.
```


## 3. Task Lifecycle & Zustandsautomat

Ein Task durchläuft einen definierten Lebenszyklus, der vom `TsfScheduler` gesteuert wird.

  * `#pending`: Task wartet auf Ausführung (Initialzustand).
  * `#running`: Task wird aktuell vom Worker-Thread ausgeführt.
  * `#finished`: Erfolgreich beendet.
  * `#failed`: Ein Fehler ist aufgetreten (wird in `caughtError` gespeichert).
  * `#cancelled`: Task wurde manuell abgebrochen (wird nicht neu eingeplant).
  * `#skipped`: Task war pausiert (`isPaused`), Ausführung wurde übersprungen, aber Timer läuft weiter.

### Task Error Handling

Fehler innerhalb der Ausführung werden **nicht** ignoriert, sondern gefangen:

1.  Der Status wechselt auf `#failed`.
2.  Die Exception wird in `caughtError` gespeichert.
3.  Der `onFailure:` Block des Tasks wird ausgeführt (falls gesetzt).
4.  Der globale Error-Handler des Schedulers wird informiert.


```smalltalk
task onFailure: [ :err | Transcript show: 'Oje: ', err description ].
```


## Entwicklungsprozess & Credits

Ein besonderer Dank gilt meinem KI-Sparringspartner für die intensiven und wertvollen Diskussionen während der Entwurfsphase. Die Fähigkeit der KI, verschiedene Architekturansätze (wie Polling-Loops vs. Priority Queues) schnell zu skizzieren und Vor- und Nachteile abzuwägen, hat die Entwicklung von `tsf-scheduler` erheblich beschleunigt und die Robustheit des Endergebnisses verbessert.


## License

Dieses Projekt steht unter der Apache 2.0 Lizenz. Siehe LICENSE.


## Kontakt

Bei Fragen oder Interesse an diesem Projekt erreichen Sie mich unter:   
📧 *georghagn [at] tiny-frameworks.io*

<sup>*(Bitte keine Anfragen an die privaten GitHub-Account-Adressen)*</sup>




