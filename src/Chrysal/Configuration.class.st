Class {
	#name : #Configuration,
	#superclass : #Object,
	#instVars : [
		'importer'
	],
	#category : #Chrysal
}

{ #category : #'instance creation' }
Configuration class >> newFromStream: aStream [
	| stonDictionary |
	self halt.
	stonDictionary := (STON fromStream: aStream).

]

{ #category : #accessing }
Configuration >> importer: anImporter [

	importer := anImporter
]
