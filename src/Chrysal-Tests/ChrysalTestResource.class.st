Class {
	#name : #ChrysalTestResource,
	#superclass : #TestResource,
	#category : #'Chrysal-Tests-Tests'
}

{ #category : #running }
ChrysalTestResource >> setUp [
	
	Smalltalk globals at: #ConfigurationForXX ifAbsent: [  
		ChrysalConfigurationBuilder new
			configurationRootClass: ConfigurationRootForXX;
			defineConfigurationClassNamed: #ConfigurationForXX packagedIn: 'Chrysal-Tests';
			withDescriptionItems: ConfigurationDescriptionForXX itemDescriptionForXX ].
	^ Smalltalk globals at: #ConfigurationForXX
]

{ #category : #running }
ChrysalTestResource >> tearDown [

	| forXXClass |
	forXXClass := Smalltalk globals at: #ConfigurationForXX ifAbsent: [ ^ self ].
	forXXClass removeFromSystem
]
