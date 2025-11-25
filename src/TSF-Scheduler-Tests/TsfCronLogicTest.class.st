"
Ich lege meinen Focus auf die Logik.

Ich teste die Priority Queue und die Idempotenz, ohne den Timer-Thread wirklich starten zu müssen. Ich greife dafür (nur im Test!) auf die interne Variable tasks zu.

"
Class {
	#name : 'TsfCronLogicTest',
	#superclass : 'TsfTestCase',
	#category : 'TSF-Scheduler-Tests-Task',
	#package : 'TSF-Scheduler-Tests',
	#tag : 'Task'
}

{ #category : 'tests' }
TsfCronLogicTest >> testIdempotencyEnsureTask [
	"Prüft, ob ensureTaskNamed: existierende Tasks updatet statt dupliziert"
	| cron taskA taskB internalTasks |

	cron := TsfCron current.

	"1. Erster Aufruf"
	taskA := cron ensureTaskNamed: 'TestTask' frequency: 5 minutes action: [ 1 ].

	"2. Zweiter Aufruf mit anderem Parameter"
	taskB := cron ensureTaskNamed: 'TestTask' frequency: 10 minutes action: [ 2 ].

	internalTasks := cron instVarNamed: 'tasks'.

	self assert: (internalTasks size = 1) description: 'Es darf nur einen Task geben'.
	self assert: (taskA == taskB) description: 'Es muss das identische Objekt sein'.
	self assert: (taskA frequency = 10 minutes) description: 'Frequenz muss geupdatet sein'.
	
]

{ #category : 'tests' }
TsfCronLogicTest >> testPrioritySorting [
	"Prüft, ob die Tasks korrekt nach nextRun sortiert werden"
	| cron task1 task2 task3 internalTasks |

	cron := TsfCron current.

	task1 := TsfTask new nextRun: (DateAndTime now + 10 minutes).
	task2 := TsfTask new nextRun: (DateAndTime now + 1 minute).
	task3 := TsfTask new nextRun: (DateAndTime now + 5 minutes).

	cron addPeriodicTask: task1.
	cron addPeriodicTask: task2.
	cron addPeriodicTask: task3.

	"Zugriff auf die private Variable 'tasks' für den Test"
	internalTasks := cron instVarNamed: 'tasks'.

	self assert: internalTasks size equals: 3.
	self assert: (internalTasks first = task2) description: 'Task in 1 Minute muss erster sein'.
	self assert: internalTasks second equals: task3.
	self assert: internalTasks third equals: task1.

]

{ #category : 'tests' }
TsfCronLogicTest >> testSubclassingIntegration [
	"Testet, ob eigene Task-Klassen korrekt vom Scheduler ausgeführt werden"
	| finishedSem task |

	finishedSem := Semaphore new.

	TsfScheduler current start.

	"HIER wird die Mock-Klasse benutzt:"
	task := TsfMockTask new.
	task name: 'Subclass Test'.
	
	"Wir geben ihr das Semaphore, damit sie Bescheid geben kann, wenn sie fertig ist"
	task setSemaphore: finishedSem.

	"Ab in den Scheduler damit"
	TsfScheduler current scheduleTask: task.

	"DIE ÄNDERUNG: Wir nutzen ein normales wait."
	"Da wir wissen (laut Transcript), dass der Task läuft und signalisiert,"
	"wird dieser Aufruf sofort zurückkehren, sobald der Task fertig ist."
	"Wenn der Test hier für immer hängen bleibt, wissen wir, dass das Signal nie ankam."
	
	Transcript show: 'Test wartet jetzt (blockierend)...'; cr.
	finishedSem wait.
	Transcript show: 'Test ist aufgewacht (Signal erhalten!)'; cr.

	"Wir geben die CPU kurz ab, damit der Scheduler-Prozess"
	"die Zeile 'state := #finished' ausführen kann."
	(Delay forMilliseconds: 1) wait.
	
    "Wenn wir hier ankommen, war das wait erfolgreich."
    self assert: task isFinished.


	
]
