Class {
	#name : #Configuration,
	#superclass : #Object,
	#category : #Chrysal
}

{ #category : #'instance creation' }
Configuration class >> newFromStream: aStream [
	| stonDictionary |
	self halt.
	stonDictionary := (STON fromStream: aStream).

]
