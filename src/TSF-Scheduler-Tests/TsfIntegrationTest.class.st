"
**Test 1: The *Heartbeat* (Happy Path & Rescheduling)** `testPeriodicExecutionFlow`
- This test checks the entire chain: Cron wakes up the scheduler, the scheduler executes the task, and the task reschedules itself via Cron.
- Szenario: A task should run every 10ms. We are waiting for 3 executions.

**Test 2: The *Kamikaze* (Error Resilience)** `testErrorResilience`
- This test is almost even more important. It proves that a crashing task does not kill the worker thread.
- Scenario: A task throws an error. The scheduler must continue running, and the task should be marked as #failed.

**DEBUGGING / UI**

I have no UI for TsfCron yet. If you want see what happens:
- Execute *TsfCron current report* in PlayGround. You will see at once the state in Transcript.
"
Class {
	#name : 'TsfIntegrationTest',
	#superclass : 'TsfTestCase',
	#category : 'TSF-Scheduler-Tests-Scheduler',
	#package : 'TSF-Scheduler-Tests',
	#tag : 'Scheduler'
}

{ #category : 'tests' }
TsfIntegrationTest >> testErrorResilience [
    | task waitSem cron scheduler caughtError |
    
    waitSem := Semaphore new.
    cron := TsfCron current.
    scheduler := TsfScheduler current.
    cron stop. scheduler stop.

    "1. Kamikaze Task erstellen"
    task := TsfTask new
        name: 'IntegrationTest-Kamikaze';
        frequency: 100 milliSeconds;
        action: [ 1 / 0 ]; "Provozierter ZeroDivide Fehler"
        onFailure: [ :err | 
            caughtError := err.
            waitSem signal. "Fehler wurde abgefangen!"
        ].

    [
        scheduler start.
        cron start.
        cron addPeriodicTask: task.

        "2. Warten, bis der Fehler passiert ist"
        waitSem wait.

        "3. Assertions"
        self assert: task state equals: #failed.
        self assert: (caughtError isKindOf: ZeroDivide).
        
        "4. DER WICHTIGSTE CHECK: Lebt der Scheduler noch?"
        self assert: scheduler isRunning.
        
        "Optional: Kann er noch arbeiten? Einen 'Heile-Welt' Task hinterherschicken"
        "..."
        
    ] ensure: [
        cron stop. scheduler stop.
        cron removeTask: task.
    ].
]

{ #category : 'tests' }
TsfIntegrationTest >> testFixedDelayStackingPrevention [
	| runCount task |

	runCount := 0.

	TsfScheduler current start.
	TsfCron current start.

	task := TsfTask new.
	task frequency: 200 milliSeconds.
	task setAction: [ 
		"The execution takes longer than the frequency!"
		(Delay forMilliseconds: 400) wait.
		runCount := runCount + 1.
	].
    
	TsfCron current addPeriodicTask: task.
    
	"We wait 1 second."
	"With 'Fixed Rate' (incorrect), it would attempt to run 5 times (every 200ms)."
	"With 'Fixed Delay' (correct), it runs: Start (0ms) -> End (400ms) -> Pause (200ms) -> Start (600ms) -> End (1000ms)."
	"So it should only complete approximately 2 cycles in 1 second."
	(Delay forSeconds: 1) wait.
    
	task cancel.
    
	self assert: runCount <= 2 description: 'The number of tasks has piled up! There should be a maximum of 2 runs.'.
	self assert: runCount >= 1 description: 'The task did not run at all.'.
	
]

{ #category : 'tests' }
TsfIntegrationTest >> testPeriodicExecutionFlow [
    | task executionCount waitSem cron scheduler |
    
    executionCount := 0.
    waitSem := Semaphore new.
    
    "Setup: Wir holen die Singletons (oder besser: neue Instanzen für den Test)"
    cron := TsfCron current.
    scheduler := TsfScheduler current.
    
    "Aufräumen sicherstellen, falls vorher was lief"
    cron stop. scheduler stop.
    
    "1. Task definieren: Zählt hoch und signalisiert"
    task := TsfTask new
        name: 'IntegrationTest-Heartbeat';
        frequency: 10 milliSeconds;
        action: [ 
            executionCount := executionCount + 1.
            waitSem signal. "Ich bin fertig!"
        ].

    [
        "2. System starten"
        scheduler start.
        cron start.

        "3. Task einplanen"
        cron addPeriodicTask: task.
        
        "4. Synchronisation: Wir warten auf 3 Signale (3 Ausführungen)"
        waitSem wait. "1. Run fertig"
        self assert: executionCount equals: 1.
        
        waitSem wait. "2. Run fertig"
        self assert: executionCount equals: 2.
        
        waitSem wait. "3. Run fertig"
        self assert: executionCount equals: 3.
        

        "Prüfen, ob der Task wirklich neu eingeplant wurde"
        (Delay forMilliseconds: 400) wait.
        self assert: task state equals: #finished. "Zustand nach Ausführung"
        self assert: (cron findTaskByName: task name) notNil.
        
    ] ensure: [
        "5. Teardown: Immer alles stoppen, auch wenn der Test rot ist"
        cron stop.
        scheduler stop.
        cron removeTask: task.
    ].
]

{ #category : 'tests' }
TsfIntegrationTest >> testSubclassingIntegration [
	| finishedSem task |

	finishedSem := Semaphore new.

	TsfScheduler current start.

	task := TsfMockTask new.
	task name: 'Subclass Test'.
	task setSemaphore: finishedSem.

	"Directly into the scheduler (without cron for this test)"
	TsfScheduler current scheduleTask: task.

	"Wait until the task sends 'signal' (max 1 second)"
	(Delay forMilliseconds: 1) wait.
	finishedSem waitTimeoutSeconds: 1.
	
	self assert: task isFinished.

]
