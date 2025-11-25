Class {
	#name : 'TsfIntegrationTest',
	#superclass : 'TsfTestCase',
	#category : 'TSF-Scheduler-Tests-Scheduler',
	#package : 'TSF-Scheduler-Tests',
	#tag : 'Scheduler'
}

{ #category : 'tests' }
TsfIntegrationTest >> testFixedDelayStackingPrevention [
	"Testet, dass sich langsame Tasks nicht stapeln (Fixed Delay)"
	| runCount task |

	runCount := 0.

	TsfScheduler current start.
	TsfCron current start.

	task := TsfTask new.
	task frequency: 200 milliSeconds.
	task setAction: [ 
		"Die Ausführung dauert länger als die Frequenz!"
		(Delay forMilliseconds: 400) wait.
		runCount := runCount + 1.
	].
    
	TsfCron current addPeriodicTask: task.
    
	"Wir warten 1 Sekunde."
	"Bei 'Fixed Rate' (falsch) würde er versuchen, 5 Mal zu laufen (alle 200ms)."
	"Bei 'Fixed Delay' (richtig) läuft er: Start(0ms)->Ende(400ms) -> Pause(200ms) -> Start(600ms)->Ende(1000ms)."
	"Er sollte also in 1 Sekunde nur ca. 2 Mal fertig werden."
	(Delay forSeconds: 1) wait.
    
	task cancel.
    
	self assert: runCount <= 2 description: 'Tasks haben sich gestapelt! Sollten max 2 Runs sein.'.
	self assert: runCount >= 1 description: 'Task lief gar nicht.'.
	
]

{ #category : 'tests' }
TsfIntegrationTest >> testFullSystemPeriodicExecution [
	"Testet das Zusammenspiel von Cron und Scheduler mit echten Delays"
	"Test ist sehr diffiziel. Das Ergebnis ist von vielen Faktoren wie VM-Version, Betriebssystem, 
	CPU-Geschwindigkeit, .. Test ist auch unter meiner Konfiguration manchmal erfolgreich und manchmal nicht"
	| runSemaphore task |

	runSemaphore := Semaphore new.

	"1. System starten"
	TsfScheduler current start.
	TsfCron current start.

	"2. Task erstellen, der das Semaphore signalisiert"
	task := TsfTask new.
	task name: 'Integration Test Task'.
	task frequency: 1 milliSeconds. "Sehr schnell für den Test"
	task setAction: [ runSemaphore signal ].

	"3. Einplanen"
	TsfCron current addPeriodicTask: task.

	"4. Warten und Zählen"
	"Wir warten darauf, dass das Semaphore 3 Mal signalisiert wird."
	self assert: (runSemaphore waitTimeoutSeconds: 1). "Run 1"
	self assert: (runSemaphore waitTimeoutSeconds: 1). "Run 2"
	self assert: (runSemaphore waitTimeoutSeconds: 1). "Run 3"
    
	task cancel.
	"Kurz warten, damit Cancellation durchgreift, bevor teardown kommt"
	(Delay forMilliseconds: 150) wait.

]

{ #category : 'tests' }
TsfIntegrationTest >> testSubclassingIntegration [
	"Testet, ob eigene Task-Klassen korrekt vom Scheduler ausgeführt werden"
	| finishedSem task |

	finishedSem := Semaphore new.

	TsfScheduler current start.

	task := TsfMockTask new.
	task name: 'Subclass Test'.
	task setSemaphore: finishedSem.

	"Direkt in den Scheduler (ohne Cron für diesen Test)"
	TsfScheduler current scheduleTask: task.

	"Warten bis der Task 'signal' sendet (max 1 Sekunde)"
	(Delay forMilliseconds: 1) wait.
	finishedSem waitTimeoutSeconds: 1.
	
	self assert: task isFinished.


]
