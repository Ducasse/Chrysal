Class {
	#name : #BaselineOfChrysal,
	#superclass : #BaselineOf,
	#category : #BaselineOfChrysal
}

{ #category : #baselines }
BaselineOfChrysal >> baseline: spec [
	<baseline>
	
	spec
		baseline: 'Containers-PropertyEnvironment' 
		with: [ spec repository: 'github://Ducasse/Containers-PropertyEnvironment' ].
		
	spec 
		for: #pharo 
		do: [	spec 
					package: 'Containers-PropertyEnvironment';
					package: 'Chrysal' with: [ spec requires: #('Containers-PropertyEnvironment') ] ]
]
