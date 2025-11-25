Class {
	#name : 'TsfSystem',
	#superclass : 'Object',
	#category : 'TSF-System',
	#package : 'TSF-System'
}

{ #category : 'accessing' }
TsfSystem class >> start [

	TsfScheduler start. "Erst der Worker..."
	TsfCron start.      "...dann der Zeitgeber"
	
	Transcript show: 'TSF System läuft.'; cr.

]

{ #category : 'accessing' }
TsfSystem class >> stop [

	TsfCron stop.       "Erst den Zeitgeber stoppen (damit keine neuen Tasks kommen)"
	TsfScheduler stop.  "Dann den Worker stoppen"
	
	Transcript show: 'TSF System gestoppt.'; cr.
]
