Class {
	#name : 'BaselineOfTsfScheduler',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfTsfScheduler',
	#package : 'BaselineOfTsfScheduler'
}

{ #category : 'baselines' }
BaselineOfTsfScheduler >> baseline: spec [
    <baseline>

    spec for: #common do: [
		spec package: 'TSF-Scheduler'.
		spec package: 'TSF-Scheduler-Tests' with: [ spec requires: #('TSF-Scheduler') ].
    ].

    spec group: 'core' with: #( 'TSF-Scheduler' ).
    spec group: 'tests' with: #( 'TSF-Scheduler-Tests' ).
    spec group: 'default' with: #( 'core' 'tests').

]
