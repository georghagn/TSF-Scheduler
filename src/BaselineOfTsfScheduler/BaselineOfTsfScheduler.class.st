Class {
    #name : 'BaselineOfTsfScheduler',
    #superclass : 'BaselineOf',
    #category : 'BaselineOfTsfScheduler'
}

{ #category : 'baselines' }
BaselineOfTsfScheduler >> baseline: spec [
    <baseline>

    spec for: #common do: [
        spec repository: 'github://georghagn/TSF-Scheduler:main/src'.

        self definePackages: spec.
        self defineGroups: spec.
    ].
]

{ #category : 'packages' }
BaselineOfTsfScheduler >> definePackages: spec [
    spec package: 'TSF-Scheduler'.
    spec package: 'TSF-Scheduler-Tests' with: [
        spec requires: #( 'TSF-Scheduler' )
    ].
]

{ #category : 'groups' }
BaselineOfTsfScheduler >> defineGroups: spec [
    spec group: 'core' with: #( 'TSF-Scheduler' ).
    spec group: 'tests' with: #( 'TSF-Scheduler-Tests' ).
    spec group: 'default' with: #( 'core' ).
]

