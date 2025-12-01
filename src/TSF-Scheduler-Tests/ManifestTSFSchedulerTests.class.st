"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestTSFSchedulerTests',
	#superclass : 'PackageManifest',
	#category : 'TSF-Scheduler-Tests-Manifest',
	#package : 'TSF-Scheduler-Tests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestTSFSchedulerTests class >> ruleTempsReadBeforeWrittenRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#TsfIntegrationTest #testErrorResilience #false)) #'2025-12-01T12:13:31.685491+01:00') )
]
