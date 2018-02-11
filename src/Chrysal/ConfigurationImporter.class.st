Class {
	#name : #ConfigurationImporter,
	#superclass : #Object,
	#instVars : [
		'configuration'
	],
	#category : #Chrysal
}

{ #category : #initialization }
ConfigurationImporter >> initialize [
	super initialize.
	configuration := ChrysalConfiguration new importer: self
]
