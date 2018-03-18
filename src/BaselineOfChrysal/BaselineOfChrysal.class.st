"
Now introduce Chrysal-Runtime package containing a simple class whose other configurations will be subclass of. 
"
Class {
	#name : #BaselineOfChrysal,
	#superclass : #BaselineOf,
	#category : #BaselineOfChrysal
}

{ #category : #baselines }
BaselineOfChrysal >> baseline: spec [
	<baseline>
	spec
		baseline: 'ContainersPropertyEnvironment'
		with: [ spec repository: 'github://Ducasse/Containers-PropertyEnvironment' ].
	spec
		for: #pharo
		do: [ 
			spec
				package: 'Chrysal-Runtime'.
			spec
				package: 'Chrysal-Test'
				with: [ spec requires: #('Chrysal-Runtime' 'Chrysal') ].
			spec
				package: 'Chrysal'
				with: [ spec requires: #('ContainersPropertyEnvironment' 'Chrysal-Runtime') ] ]
]
