"
**Basisklasse für alle Tests**

Da ich mit Singletons (TsfScheduler current, TsfCron current) arbeite, ist das Wichtigste, dass jeder Test mit einem sauberen System startet. Sonst beeinflusst ein Test den nächsten.
"
Class {
	#name : 'TsfTestCase',
	#superclass : 'TestCase',
	#category : 'TSF-Scheduler-Tests-Scheduler',
	#package : 'TSF-Scheduler-Tests',
	#tag : 'Scheduler'
}

{ #category : 'running' }
TsfTestCase >> setUp [

	super setUp.

	"Vor jedem Test sicherstellen, dass alles gestoppt und zurückgesetzt ist."
	TsfCron reset.
	TsfScheduler reset.

	"Wir geben den Systemen kurz Zeit, wirklich runterzufahren"
	(Delay forMilliseconds: 10) wait.

]

{ #category : 'running' }
TsfTestCase >> tearDown [
"Nach dem Test auch wieder aufräumen, falls der Test etwas gestartet hat."

	TsfCron reset.
	TsfScheduler reset.
	super tearDown.
	
]
